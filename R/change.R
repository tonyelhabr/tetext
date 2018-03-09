
#' Compute change
#'
#' @description Compute change in token usage over time.
#' @details None.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param token bare for NSE; character for SE. Name of column in \code{data} corresponding to token.
#' @param timebin bare for NSE; character for SE. Name of column in \code{data} specifying temporal period
#' to use to compute change.
#' @param top_pct numeric. Number between 0 and 1. Useful primarily to limit the number of models
#' that need to be computed and to reduce 'nose' regarding what is deemed significant.
#' @param only_signif logical. Whether or not to return rows with a significant p-value.
#' @param signif_cutoff numeric. Number between 0 and 1. Value to use as 'maximum' threshhld for significance.
#' Only used if \code{only_signif = TRUE}.
#' @param return_models logical. Whether to return just the models. This is probably the preferred
#' option when calling \code{compute_change_at()} directly.
#' @param return_data logical. Whether to 'bytime' data which is used as
#' the \code{data} parameter in \code{stats::glm()} for creating models.
#' Needed when using \code{visualize_change_at()}.
#' @param return_both logical. Set to \code{TRUE} when \code{visualize_change_at()}.
#' @return data.frame.
#' @rdname compute_change
#' @export
#' @seealso \url{https://www.tidytextmining.com/twitter.html#changes-in-token-use}
compute_change_at <-
  function(data = NULL,
           token = NULL,
           timebin = NULL,
           bin = TRUE,
           timefloor = NULL,
           top_pct = 0.25,
           only_signif = FALSE,
           signif_cutoff = 0.1,
           return_models = TRUE,
           return_data = FALSE,
           return_both = FALSE) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(timebin), is.character(timebin))

    n <-
      time_total <-
      token_total <-
      time_floor <- models <- term <- p.value <- adjusted_p_value <- NULL

    token_quo <- rlang::sym(token)
    timebin_quo <- rlang::sym(timebin)

    if(bin) {
      stopifnot(!is.null(timefloor), is.character(timefloor))
      data <-
        data %>%
        dplyr::mutate(time_floor = lubridate::floor_date(!!timebin_quo, unit = timefloor))
    } else {
      data <-
        data %>% dplyr::mutate(time_floor = !!timebin_quo)
    }

    data_bytime <-
      data %>%
      dplyr::group_by(time_floor, !!token_quo) %>%
      dplyr::summarise(n = n()) %>%
      # dplyr::summarise(n = sum(!is.na(time_floor))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(time_floor) %>%
      dplyr::mutate(time_total = sum(n)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!token_quo) %>%
      dplyr::mutate(token_total = sum(n)) %>%
      dplyr::ungroup() %>%
      filter_num_top_at("token_total", (1 - top_pct), 1, 0) %>%
      dplyr::arrange(dplyr::desc(token_total))

    # NOTE: This supplies a formula to `stats::glm()` in an atypical fashion:
    # "... as a two-column matrix with the columns giving the numbers of successes and failures".
    data_bytime_models <-
      data_bytime %>%
      tidyr::nest(-!!token_quo) %>%
      dplyr::mutate(models =
                      purrr::map(data, ~ stats::glm(
                        cbind(n, time_total) ~ time_floor, ., family = "binomial"
                      )))

    data_bytime_models <-
      data_bytime_models %>%
      tidyr::unnest(purrr::map(models, broom::tidy)) %>%
      dplyr::filter(term == "time_floor") %>%
      dplyr::mutate(adjusted_p_value = stats::p.adjust(p.value)) %>%
      dplyr::arrange(adjusted_p_value)

    if(only_signif) {
      signif_cutoff <- validate_range(x = signif_cutoff, max = 1, min = 0)
      data_bytime_models <-
        data_bytime_models %>%
        dplyr::filter(adjusted_p_value <= signif_cutoff)
    }

    if(return_both | (return_data & return_models)) {
      out <- list(data = data_bytime, models = data_bytime_models)
    } else if (return_data) {
      out <- data_bytime
    } else if (return_models) {
      out <- data_bytime_models
    }
    out
  }

#' @rdname compute_change
#' @export
compute_change <-
  function(...,
           token,
           timebin,
           timefloor) {
    stopifnot(!missing(token))
    stopifnot(!missing(timebin))
    timebin <- rlang::quo_text(rlang::enquo(timebin))
    token <- rlang::quo_text(rlang::enquo(token))
    if (missing(timefloor)) {
      timefloor <- NULL
    }
    compute_change_at(...,
                      token = token,
                      timebin = timebin,
                      timefloor = timefloor)
}

#' Visualize change
#'
#' @description Visualize the words that have changed the most across \code{timebin}
#' @details Calls \code{compute_change()} internally. Works similarly to \code{visualize_corrs}.
#' (Two data sets are returned from the list that is returned by the \code{compute_} function.)
#' @param ... dots. Parameters passed to \code{compute_change_at()}.
#' @param add_labels logical. Whether or not to add labels to the lines.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @inheritParams compute_change
#' @return gg.
#' @rdname visualize_change
#' @export
visualize_change_at <-
  function(...,
           token = NULL,
           num_top = 5,
           color = token,
           add_labels = FALSE,
           scale_manual_base = default_scale_manual(values = rep("grey50", num_top)),
           scale_manual_params = list(),
           labs_base = default_labs(),
           labs_params = list(title = "Tokens with Most Significant Change in Frequency",
                              caption = paste0("Statistical significance is determined by a logistical model\n",
                                               "estimating token appearance in a given time period.")),
           theme_base = default_theme(panel.grid.major.x = ggplot2::element_blank()),
           theme_params = list(legend.position = ifelse(add_labels, "none", "bottom"))) {

    stopifnot(!is.null(token), is.character(token))

    data_list <-
      compute_change_at(
        ...,
        token = token,
        return_both = TRUE
      )
    data <- data_list$data
    data_models <- data_list$models

    temp <- adjusted_p_value <- NULL

    data_models_top <-
      data_models %>%
      dplyr::mutate(temp = 1 - adjusted_p_value) %>%
      filter_num_top_at("temp", num_top) %>%
      dplyr::select(-temp)

    if (is.null(color)) {
      data_models_top <- data_models_top %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
    # NOTE: DON'T NEED THIS!
    # data_models_top <- wrangle_color_col(data_models_top color)

    time_total <- time_floor <- pct <- label <- n <- NULL


    token_quo <- rlang::sym(token)
    # Create label column even if `add_labels = FALSE`.

    data_proc <-
      data %>%
      dplyr::inner_join(data_models_top, by = token) %>%
      dplyr::mutate(pct = n / time_total) %>%
      dplyr::mutate(label = dplyr::if_else(time_floor == max(time_floor), !!token_quo, NA_character_))

    viz <-
      data_proc %>%
      ggplot2::ggplot(ggplot2::aes_string(x = "time_floor", y = "pct", color = color)) +
      generate_scale_manual(scale_manual_base, scale_manual_params, type = "color") +
      ggplot2::geom_line(size = 1.5) +
      ggplot2::scale_y_continuous(labels = scales::percent_format())

    if(add_labels) {
      viz <-
        viz +
        ggrepel::geom_label_repel(ggplot2::aes(label = label), nudge_x = 1, na.rm = TRUE)
    }

    viz <-
      viz +
      generate_labs(labs_base, labs_params) +
      generate_theme(theme_base, theme_params)
}

#' @rdname visualize_change
#' @export
visualize_change <-
  function(...,
           token,
           timebin,
           timefloor,
           color) {
    stopifnot(!missing(token))
    stopifnot(!missing(timebin))
    token <- rlang::quo_text(rlang::enquo(token))
    timebin <- rlang::quo_text(rlang::enquo(timebin))
    if (missing(color)) {
      # color <- NULL
      color <- token
    } else {
      color <- rlang::quo_text(rlang::enquo(color))
    }
    if (missing(timefloor)) {
      timefloor <- NULL
    }
    visualize_change_at(
      ...,
      token = token,
      timebin = timebin,
      timefloor = timefloor,
      color = color
    )
  }

