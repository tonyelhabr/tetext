
#' Compute change
#'
#' @description Compute change in word usage over time.
#' @details None.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param timebin character. Name of column in \code{data} specifying temporal period
#' to use to compute change.
#' @param word character. Name of column in \code{data} corresponding to n-gram.
#' @param timefloor character. Passed directly to \code{lubridate::floor_date()} as\code{unit} parameter.
#' @param top_pct numeric. Number between 0 and 1. Default is provided.
#' @param return_models logical. Whether to return just the models. This is probably the preferred
#' option when calling \code{compute_change_at()} directly. Default is provided.
#' @param return_data logical. Whether to 'bytime' data which is used as
#' the \code{data} parameter in \code{stats::glm()} for creating models. Default is provided.
#' Needed when using \code{visualize_change_at()}.
#' @param return_both logical. Set to \code{TRUE} when \code{visualize_change_at()}. Default is provided.
#' @return data.frame.
#' @rdname compute_change
#' @export
#' @seealso \url{https://www.tidytextmining.com/twitter.html#changes-in-word-use}
compute_change_at <-
  function(data = NULL,
           timebin = NULL,
           word = "word",
           timefloor = NULL,
           top_pct = 0.05,
           return_models = TRUE,
           return_data = FALSE,
           return_both = FALSE) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    if (is.null(timebin))
      stop("`timebin` cannot be NULL.", call. = FALSE)
    if (is.null(timefloor))
      stop("`ctimefloor` cannot be NULL.", call. = FALSE)
     if ((top_pct < 0) |
        (top_pct > 1))
      stop("`top_pct` is not valid.", call. = FALSE)

    n <-
      time_total <-
      word_total <-
      time_floor <- models <- term <- p.value <- adjusted_p_value <- NULL

    word_quo <- rlang::sym(word)
    timebin_quo <- rlang::sym(timebin)

    data_bytime <-
      data %>%
      dplyr::mutate(time_floor = lubridate::floor_date(!!timebin_quo, unit = timefloor)) %>%
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
      dplyr::filter(word_total >= stats::quantile(word_total, 1 - top_pct)) %>%
      dplyr::arrange(dplyr::desc(word_total))

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
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @return gg.
#' @rdname visualize_change
#' @export
visualize_change_at <-
  function(...,
           num_top = 5,
           color = NULL,
           color_value = rep("grey80", num_top),
           lab_title = "Largest Changes in Word Frequency",
           lab_subtitle = NULL,
           lab_x = NULL,
           lab_y = NULL,
           theme_base = temisc::theme_te_a()) {

    data_list <-
      compute_change_at(
        ...,
        return_both = TRUE
      )
    data <- data_list$data
    data_models <- data_list$models

    data_models_top <-
      data_models %>%
      dplyr::top_n(num_top, -adjusted_p_value)

    if (is.null(color)) {
      data_models_top$color_value <- "dummy"
      color <- "color_value"
    }
    data_models_top <- wrangle_color_col(data_models_top, color)

    adjusted_p_value <- time_total <- time_floor <- word <- pct <- label <- n <- NULL

    viz <-
      data %>%
      dplyr::inner_join(data_models_top, by = c("word")) %>%
      dplyr::mutate(pct = n / time_total) %>%
      dplyr::mutate(label = dplyr::if_else(time_floor == max(time_floor), word, NA_character_)) %>%
      ggplot2::ggplot(ggplot2::aes(x = time_floor, y = pct, color = word)) +
      ggplot2::scale_color_manual(values = color_value) +
      ggplot2::geom_line(size = 1.5) +
      ggrepel::geom_label_repel(ggplot2::aes(label = label), nudge_x = 1, na.rm = TRUE) +
      ggplot2::scale_y_continuous(labels = scales::percent_format())

    viz_labs <-
      ggplot2::labs(
        x = lab_x,
        y = lab_y,
        title = lab_title,
        subtitle = lab_subtitle
      )
    viz_theme <-
      theme_base +
      ggplot2::theme(
        legend.position = "none"
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

