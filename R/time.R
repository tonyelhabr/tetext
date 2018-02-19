
#' Visualize by time period.
#'
#' @description Visualize time-based data over time.
#' @details 'multi' version is for facetting.
#' @param data data.frame.
#' @param colname_timebin character. Name of column in \code{data} to use for x-axis.
#' Probably something like 'yyyy', 'mm', etc.
#' @param geom character. 'bar' or 'hist'. 'bar' is probably best for everything except Date objects.
#' @param add_alpha logical. Whether or not to use \code{ggplot2::scale_alpha()} based on count.
#' @param alpha_range numeric (vector). Direct parameter passed to \code{range} parameter
#' of \code{ggplot2::scale_alpha()}. Default is provided.
#' @param colname_color character. Name of column in \code{data} to use for color basis.
#' Set to \code{NULL} by default although not actually required in order to simplify internal code.
#' @param color chracter. Hex value of color. Default is provided.
#' @param lab_title character. Default is provided.
#' @param lab_subtitle character. Probably something like 'By Year', 'By Month', etc.
#' Set to \code{NULL} by default although not actually required in order to simplify internal code.
#' @param lab_x character. Default is provided.
#' @param lab_y character. Default is provided.
#' @param theme_base \code{ggplot2} theme, such as \code{ggplot2::theme_minimal()}. Default is provided.
#' @return gg
#' @export
#' @importFrom temisc theme_te_a
#' @importFrom ggplot2 labs element_blank theme ggplot aes_string geom_bar scale_alpha geom_histogram scale_fill_manual
#' @seealso \url{https://juliasilge.com/blog/ten-thousand-tweets/}.
visualize_time <-
  function(data = NULL,
           colname_timebin = NULL,
           geom = c("bar", "hist"),
           add_alpha = FALSE,
           alpha_range = c(0.25, 1),
           colname_color = NULL,
           color = "grey50",
           lab_title = "Count Over Time",
           lab_subtitle = NULL,
           lab_x = NULL,
           lab_y = NULL,
           theme_base = temisc::theme_te_a()) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)
    if (is_nothing(colname_timebin))
      stop("`colname_timebin` must not be NULL.", call. = FALSE)

    geom <- match.arg(geom)

    if (is_nothing(colname_color)) {
      data$color <- "dummy"
      colname_color <- "color"

    }
    data <- wrangle_color_col(data, colname_color)


    viz <-
      ggplot2::ggplot(data = data, ggplot2::aes_string(x = colname_timebin))
    if (geom == "bar") {
      if (!add_alpha) {
        viz <-
          viz +
          ggplot2::geom_bar(ggplot2::aes_string(y = "..count..", fill = colname_color))
      } else {
        viz <-
          viz +
          ggplot2::geom_bar(ggplot2::aes_string(
            y = "..count..",
            alpha = "..count..",
            fill = colname_color
          )) +
          ggplot2::scale_alpha(range = alpha_range)
      }
    } else if (geom == "hist") {
      viz <-
        viz +
        ggplot2::geom_histogram(ggplot2::aes_string(y = "..count..", fill = colname_color),
                                bins = 30)
    }
    viz <-
      viz +
      ggplot2::scale_fill_manual(values = color)

    viz_labs <-
      ggplot2::labs(
        x = lab_x,
        y = lab_y,
        title = lab_title,
        subtitle = lab_subtitle
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

#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param ... dots. Parameters to pass to \code{visualize_time()}.
#' @param colname_multi character. Name of column in \code{data} to use for facetting.
#' @rdname visualize_time
#' @export
#' @importFrom ggplot2 facet_wrap
#' @importFrom temisc theme_te_a
visualize_time_multi <-
  function(...,
           theme_base = temisc::theme_te_a_facet(),
           colname_multi = NULL) {
    if (is.null(colname_multi))
      stop("`colname_multi` cannot be NULL.", call. = FALSE)
    viz <- visualize_time(..., theme_base = theme_base)

    viz <-
      viz +
      ggplot2::facet_wrap(paste0("~ ", colname_multi), scales = "free")
    viz
  }

#' Visualize over multiple time periods
#'
#' @description Visualize time-based data over time.
#' @details Calls \code{visualize_time()}.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param data data.frame (single).
#' @param colnames_timebin character (vector).
#' @param geoms character (vector).
#' @param add_alphas logical (vector).
#' @param colnames_color character (vector).
#' @param colors character (vector).
#' @param labs_title character (vector).
#' @param labs_subtitle character (vector).
#' @param labs_x character (vector).
#' @param labs_y character (vector).
#' @param arrange logical. Whether or not to plot in an arranged format.
#' @param show logical. Whether or not to show plots.
#' @return ggs (or gtable, if \code{arrange = TRUE})
#' @export
#' @importFrom purrr pmap
#' @importFrom gridExtra arrangeGrob grid.arrange
visualize_time_batched <-
  function(data = NULL,
           colnames_timebin = c("timestamp", "yyyy", "mm", "wd", "hh"),
           geoms = c("hist", "bar", "bar", "bar", "bar"),
           add_alphas = c(FALSE, rep(TRUE, length(colnames_timebin) - 1)),
           # alpha_ranges = rep(c(0.25, 1), length(colnames_timebin)),
           colnames_color = rep(NA, length(colnames_timebin)),
           colors = rep("grey50", length(colnames_timebin)),
           labs_title = rep("Count Over Time", length(colnames_timebin)),
           labs_subtitle = c("", "By Year", "By Month", "By Day of Week", "By Hour"),
           labs_x = rep("", length(colnames_timebin)),
           labs_y = rep("", length(colnames_timebin)),
           # themes_base = rep(temisc::theme_te_a(), length(colnames_timebin)),
           arrange = FALSE,
           show = FALSE) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)

    num_colnames_timebin <- length(colnames_timebin)
    if (!(
      num_colnames_timebin == length(geoms) &
      num_colnames_timebin == length(labs_subtitle) &
      num_colnames_timebin == length(colors)
    )) {
      stop("List elements must have same length.", call. = FALSE)
    }

    data_proc <- data %>% tidyr::nest()
    # This is temisc::repeat_df().
    num_rep <- length(colnames_timebin)
    data_rep <- data_proc[rep(seq_len(nrow(data_proc)), num_rep), ]
    viz_time_params <-
      list(
        data = data_rep$data,
        colname_timebin = colnames_timebin,
        geom = geoms,
        add_alpha = add_alphas,
        # alpha_range = alpha_ranges,
        colname_color = colnames_color,
        color = colors,
        lab_title = labs_title,
        lab_subtitle = labs_subtitle,
        lab_x = labs_x,
        lab_y = labs_y #,
        # theme_base = themes_base
      )

    if (!missing(colnames_color)) {
      if (length(colnames_color != num_colnames_timebin))
        stop("List elements must have same length.", call. = FALSE)
      viz_time_params$colname_color <- colnames_color
    }

    viz_time_list <- purrr::pmap(viz_time_params, visualize_time)
    if(arrange) {
      num_viz <- length(viz_time_list)
      num_cols <- floor(sqrt(num_viz))
      # out <- do.call(gridExtra::grid.arrange, c(viz_time_list, ncol = num_cols)
      out <- do.call(gridExtra::arrangeGrob, c(viz_time_list, ncol = num_cols))
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
      if(show) {
        gridExtra::grid.arrange(out)
      }
    } else {
      out <- viz_time_list
      if(show) {
        print(out)
      }
    }
    invisible(out)
  }
