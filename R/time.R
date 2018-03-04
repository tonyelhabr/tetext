

#' Visualize by time period.
#'
#' @description Visualize time-based data over time.
#' @details 'multi' version is for facetting.
#' @param data data.frame.
#' @param timebin character. Name of column in \code{data} to use for time axis.
#' Probably something like 'yyyy', 'mm', etc.
#' @param geom character. 'bar' or 'hist'. 'bar' is probably best for everything except Date objects.
#' @param add_alpha logical. Whether or not to use \code{ggplot2::scale_alpha()} based on count.
#' @param alpha_range numeric (vector). Direct parameter passed to \code{range} parameter
#' of \code{ggplot2::scale_alpha()}.
#' @param color character. Name of column in \code{data} to use for color basis.
#' Set to \code{NULL} by default although not actually required in order to simplify internal code.
#' @param color_value character (vector). Hex value(s) of color. May be a vector.
#' Should not be a function.
#' @param lab_title,lab_subtitle,lab_caption,lab_x,lab_y character. It is recommended NOT
#' to use these parameters directly. (Instead, it is recommended that they are modified
#' 'after' the function is called.) Noneheless, they are provided as inputs and assigned
#' reasonable defaults (e.g. 'Count Over Time')
#' where appropriate. (Otherwise, they are given the value of NULL.)
#' @param theme_base \code{ggplot2} theme (e.g. as \code{ggplot2::theme_minimal()}.)
#' A custom \code{tetext} thems is supplied as a default.
#' @return gg
#' @rdname visualize_time
#' @export
#' @seealso \url{https://juliasilge.com/blog/ten-thousand-tweets/}.
visualize_time_at <-
  function(data = NULL,
           timebin = NULL,
           geom = c("bar", "hist"),
           add_alpha = FALSE,
           alpha_range = c(0.25, 1),
           color = NULL,
           color_value = "grey50",
           lab_x = NULL,
           lab_y = NULL,
           lab_title = "Count Over Time",
           lab_subtitle = NULL,
           lab_caption = NULL,
           theme_base = theme_tetext()) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)
    if (is_nothing(timebin))
      stop("`timebin` must not be NULL.", call. = FALSE)

    geom <- match.arg(geom)

    if((class(data$timebin) %in% c("Date", "POSIXct")) & (geom != "hist")) {
      message("It is recommended to set `geom = hist` for Date-time objects.")
    }

    if (is_nothing(color)) {
      data <- data %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }

    # if(length(color_value) > 1) {
    #   color_value <- color_value[1]
    #   message("Only using the first color in the specified `color_value`.")
    # }

    data <- wrangle_color_col(data, color)

    # browser()
    viz <-
      ggplot2::ggplot(data = data, ggplot2::aes_string(x = timebin))

    if (geom == "bar") {
      if (!add_alpha) {
        viz <-
          viz +
          ggplot2::geom_bar(
            ggplot2::aes_string(
              y = "..count..",
              fill = color
            )
          )
      } else {
        viz <-
          viz +
          ggplot2::geom_bar(ggplot2::aes_string(
            y = "..count..",
            alpha = "..count..",
            fill = color
          )
          ) +
          ggplot2::scale_alpha(range = alpha_range)
      }
    } else if (geom == "hist") {
      viz <-
        viz +
        ggplot2::geom_histogram(
          ggplot2::aes_string(
            y = "..count..",
            fill = color
          ),
          bins = 30
        )
    }
    viz <-
      viz +
      ggplot2::scale_fill_manual(values = color_value)

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
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "none")

    viz <-
      viz +
      viz_labs +
      viz_theme
    viz
  }

#' @rdname visualize_time
#' @export
visualize_time <- visualize_time_at

#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param ... dots. Parameters to pass directly to \code{visualize_time_at()}.
#' @param multi character. Name of column in \code{data} to use for facetting.
#' @param facet_ncol,facet_nrow numeric. Direct parameters to analogous \code{ggplot2::facet_wrap()} parameters.
#' @param facet_scales,facet_strip_position character. Direct parameters to analogous \code{ggplot2::facet_wrap()} parameters.
#' @rdname visualize_time
#' @export
visualize_time_multi_at <-
  function(data = NULL,
           ...,
           theme_base = theme_tetext_facet(),
           multi = NULL,
           facet_scales = "free",
           facet_ncol = 3,
           facet_nrow = NULL,
           facet_strip_position = "top") {
    if (is.null(multi))
      stop("`multi` cannot be NULL.", call. = FALSE)
    # data <- wrangle_multi_col(data, multi)
    viz <-
      visualize_time_at(data = data, ..., theme_base = theme_base)

    viz <-
      viz +
      ggplot2::facet_wrap(
        stats::as.formula(paste0("~ ", multi)),
        scales = facet_scales,
        ncol = facet_ncol,
        nrow = facet_nrow,
        strip.position = facet_strip_position
      )
    viz
  }

#' @rdname visualize_time
#' @export
visualize_time_multi <- visualize_time_multi_at

#' Visualize over multiple time periods
#'
#' @description Visualize time-based data over time.
#' @details Calls \code{visualize_time_at()}.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param data data.frame (single).
#' @param timebins character (vector).
#' @param geoms character (vector).
#' @param add_alphas logical (vector).
#' @param colors character (vector).
#' @param color_values character (vector).
#' @param labs_title character (vector).
#' @param labs_subtitle character (vector).
#' @param labs_x character (vector).
#' @param labs_y character (vector).
#' @param arrange logical. Whether or not to plot in an arranged format.
#' @param show logical. Whether or not to show plots.
#' @return ggs (or gtable, if \code{arrange = TRUE})
#' @rdname visualize_time_batch
#' @export
visualize_time_batch_at <-
  function(data = NULL,
           timebins = c("timestamp", "yyyy", "mm", "wd", "hh"),
           geoms = c("hist", "bar", "bar", "bar", "bar"),
           add_alphas = c(FALSE, rep(TRUE, length(timebins) - 1)),
           # alpha_ranges = rep(c(0.25, 1), length(timebins)),
           colors = rep(NA, length(timebins)),
           color_values = rep("grey50", length(timebins)),
           labs_x = rep("", length(timebins)),
           labs_y = rep("", length(timebins)),
           labs_title = rep("Count Over Time", length(timebins)),
           labs_subtitle = c("", "By Year", "By Month", "By Day of Week", "By Hour"),
           # lab_caption = rep("", length(timebins)),
           # themes_base = rep(theme_tetext(), length(timebins)),
           arrange = FALSE,
           show = FALSE) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)

    num_timebins <- length(timebins)
    if (!(
      num_timebins == length(geoms) &
      num_timebins == length(labs_subtitle) &
      num_timebins == length(color_values)
    )) {
      stop("List elements must have same length.", call. = FALSE)
    }

    data_proc <- data %>% tidyr::nest()
    # This is temisc::repeat_df().
    num_rep <- length(timebins)
    data_rep <- data_proc[rep(seq_len(nrow(data_proc)), num_rep),]
    viz_time_params <-
      list(
        data = data_rep$data,
        timebin = timebins,
        geom = geoms,
        add_alpha = add_alphas,
        # alpha_range = alpha_ranges,
        color = colors,
        color_value = color_values,
        lab_title = labs_title,
        lab_subtitle = labs_subtitle,
        lab_x = labs_x,
        lab_y = labs_y #,
        # theme_base = themes_base
      )

    if (!missing(colors)) {
      if (length(colors != num_timebins))
        stop("List elements must have same length.", call. = FALSE)
      viz_time_params$color <- colors
    }

    viz_time_list <- purrr::pmap(viz_time_params, visualize_time_at)
    if (arrange) {
      num_viz <- length(viz_time_list)
      num_cols <- floor(sqrt(num_viz))
      # out <- do.call(gridExtra::grid.arrange, c(viz_time_list, ncol = num_cols)
      out <-
        do.call(gridExtra::arrangeGrob, c(viz_time_list, ncol = num_cols))
      # out <-
      #   gridExtra::arrangeGrob(
      #     viz_time_list[[1]],
      #     viz_time_list[[2]],
      #     viz_time_list[[3]],
      #     viz_time_list[[4]],
      #     viz_time_list[[5]],
      #     ncol = 2,
      #     nrow = 3,
      #     layout_matrix = rbind(c(1, 1), c(2, 2), c(3, 3)
      #   )
      if (show) {
        gridExtra::grid.arrange(out)
      }
    } else {
      out <- viz_time_list
      if (show) {
        print(out)
      }
    }
    invisible(out)
  }

#' @rdname visualize_time_batch
#' @export
visualize_time_batch <- visualize_time_batch_at

#' Visualize hourly data
#'
#' @description Visualize time-based data aggregated over hours.
#' @details This function is 'unique' because it calls \code{ggplot2::geom_violin()}.
#' @inheritParams visualize_time
#' @inheritParams visualize_time
#' @inheritParams visualize_time_multi
#' @param multi character. Name of column in \code{data} to use one of main axes, not for facetting.
#' @param timebin character. Name of column in \code{data} to use for time. Probably likely something like 'hh' or 'hour'.
#' @rdname visualize_hh_multi
#' @export
visualize_hh_multi_at <-
  function(data = NULL,
           timebin = NULL,
           multi = NULL,
           color = multi,
           color_value = "grey50",
           lab_x = NULL,
           lab_y = NULL,
           lab_title = "Count Over Time",
           lab_subtitle = "By Time of Day",
           lab_caption = NULL,
           theme_base = theme_tetext()) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)
    if (is_nothing(timebin))
      stop("`timebin` must not be NULL.", call. = FALSE)
    if (is.null(multi))
      stop("`multi` cannot be NULL.", call. = FALSE)

    if (is_nothing(color)) {
      data <- data %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"

    }
    # data <- wrangle_color_col(data, color)

    viz <-
      data %>%
      ggplot2::ggplot(ggplot2::aes_string(x = multi, y = timebin, fill = color)) +
      ggplot2::scale_y_continuous(
        limits = c(1, 24),
        breaks = c(6, 12, 18),
        labels = c("6am", "Noon", "6pm")
      ) +
      ggplot2::scale_fill_manual(values = color_value) +
      ggplot2::geom_violin(size = 0) +
      ggplot2::geom_hline(yintercept = seq(3, 24, by = 3),
                          color = "grey50",
                          size = 0.1) +
      ggplot2::coord_flip()

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
      ggplot2::theme(panel.grid = ggplot2::element_blank())

    viz <-
      viz +
      viz_labs +
      viz_theme
    viz
  }

#' @rdname visualize_hh_multi
#' @export
visualize_hh_multi <- visualize_hh_multi_at

