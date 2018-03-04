
#' Compute change
#'
#' @description Compute change in n-gram usage over time.
#' @details None.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param word character. Name of column in \code{data} corresponding to n-gram.
#' @param timebin character. Name of column in \code{data} specifying temporal period
#' to use to compute change.
#' @param bin logical. Whether or not to call \code{lubridate::floor_date()} to truncate \code{timebin}.
#' @param timefloor character. Name of column passed directly to \code{unit} parameter of
#' \code{lubridate::floor_date()} if \code{bin = FALSE}.
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
#' @seealso \url{https://www.tidytextmining.com/twitter.html#changes-in-word-use}
compute_change_at <-
  function(data = NULL,
           word = "word",
           timebin = NULL,
           bin = TRUE,
           timefloor = NULL,
           top_pct = 0.25,
           only_signif = FALSE,
           signif_cutoff = 0.1,
           return_models = TRUE,
           return_data = FALSE,
           return_both = FALSE) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    if (is.null(timebin))
      stop("`timebin` cannot be NULL.", call. = FALSE)

    n <-
      time_total <-
      word_total <-
      time_floor <- models <- term <- p.value <- adjusted_p_value <- NULL

    word_quo <- rlang::sym(word)
    timebin_quo <- rlang::sym(timebin)

    if(bin) {
      if (is.null(timefloor))
        stop("`timefloor` cannot be NULL.", call. = FALSE)
      data <-
        data %>%
        dplyr::mutate(time_floor = lubridate::floor_date(!!timebin_quo, unit = timefloor))
    } else {
      data <-
        data %>% dplyr::mutate(time_floor = !!timebin_quo)
    }

    # browser()
    data_bytime <-
      data %>%
      dplyr::group_by(time_floor, !!word_quo) %>%
      dplyr::summarise(n = n()) %>%
      # dplyr::summarise(n = sum(!is.na(time_floor))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(time_floor) %>%
      dplyr::mutate(time_total = sum(n)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!word_quo) %>%
      dplyr::mutate(word_total = sum(n)) %>%
      dplyr::ungroup() %>%
      filter_num_top_at("word_total", top_pct, 1, 0) %>%
      # dplyr::filter(word_total >= stats::quantile(word_total, top_pct)) %>%
      dplyr::arrange(dplyr::desc(word_total))

    # NOTE: This supplies a formula to `stats::sglm()` in an atypical fashion:
    # "... as a two-column matrix with the columns giving the numbers of successes and failures".
    data_bytime_models <-
      data_bytime %>%
      tidyr::nest(-!!word_quo) %>%
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
compute_change <- compute_change_at

#' Visualize change
#'
#' @description Visualize the words that have changed the most across \code{timebin}
#' @details Calls \code{compute_change_at()} internally. Works similarly to \code{visualize_corrs}.
#' (Two data sets are returned from the list that is returned by the \code{compute_} function.)
#' @param ... dots. Parameters passed to \code{compute_change_at()}.
#' @param add_labels logical. Whether or not to add labels to the lines.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @return gg.
#' @rdname visualize_change
#' @export
visualize_change_at <-
  function(...,
           num_top = 5,
           color = NULL,
           color_value = rep("grey50", num_top),
           add_labels = FALSE,
           lab_title = "Words with Most Statistically Significant Change in Frequency",
           lab_subtitle = NULL,
           lab_caption = paste0("Statistical Significance is determined by a logistical model\n",
                                "estimating word appearance in a given time period."),
           lab_x = NULL,
           lab_y = NULL,
           theme_base = theme_tetext()) {

    data_list <-
      compute_change_at(
        ...,
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
      # filter_num_top_at("adjusted_p_value", num_top)

    if (is.null(color)) {
      data_models_top <- data_models_top %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
    data_models_top <- wrangle_color_col(data_models_top, color)

    time_total <- time_floor <- word <- pct <- label <- n <- NULL

    # Create label column even if `add_labels = FALSE`.
    data_proc <-
      data %>%
      dplyr::inner_join(data_models_top, by = c("word")) %>%
      dplyr::mutate(pct = n / time_total) %>%
      dplyr::mutate(label = dplyr::if_else(time_floor == max(time_floor), word, NA_character_))

    viz <-
      data_proc %>%
      ggplot2::ggplot(ggplot2::aes(x = time_floor, y = pct, color = word)) +
      ggplot2::scale_color_manual(values = color_value) +
      ggplot2::geom_line(size = 1.5) +
      ggplot2::scale_y_continuous(labels = scales::percent_format())

    if(add_labels) {
      viz <-
        viz +
        ggrepel::geom_label_repel(ggplot2::aes(label = label), nudge_x = 1, na.rm = TRUE)
    }


    viz_labs <-
      ggplot2::labs(
        x = lab_x,
        y = lab_y,
        title = lab_title,
        subtitle = lab_subtitle,
        caption = lab_caption
      )
    viz_theme <-
      theme_base +
      ggplot2::theme(
        legend.position = ifelse(add_labels, "none", "bottom")
      )
    viz <-
      viz +
      viz_labs +
      viz_theme
    viz
}

#' @rdname visualize_change
#' @export
visualize_change <- visualize_change_at

