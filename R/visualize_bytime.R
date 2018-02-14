


#' Visualize by time period.
#'
#' @description Visualize time-based data over time.
#' @details Even though \code{x_char} and \code{lab_subtitle} are not required, they
#' are sent to \code{NULL} by default in order to simplify the internal code.
#' @param data data.frame.
#' @param x_char character. Probably something like 'timestamp', 'yyyy', 'mm', etc.
#' @param geom character. "bar" or "hist". "bar" is probably best for everything except Date objects.
#' @param color_char character. Name of column in data.frame to use for color basis.
#' @param color chracter. Hex value of color. Default is provided.
#' @param lab_subtitle character. Probably something like 'By Year', 'By Month', etc.
#' @return ggplot
#' @export
#' @importFrom temisc theme_te_b_facet
visualize_bytime <-
  function(data,
           x_char = NULL,
           geom = c("bar", "hist"),
           color_char = NULL,
           color = "grey50",
           lab_subtitle = NULL) {
    if (is.null(x_char)) stop("`x_char` must not be NULL.", call. = FALSE)
    if (missing(color_char) || is.null(color_char)) {
      data$fill <- "dummy"
      color_char <- "fill"
      fill_cnt <- 1
    } else {
      # color_quo <- rlang::sym(color_char)
      # fill_cnt <- length(dplyr::pull(dplyr::distinct(data, !!color_quo)))
      fill_cnt <- length(unique(data[, color_char]))
    }
    geom <- match.arg(geom)
    viz_labs <-
      ggplot2::labs(
        x = NULL,
        y = NULL,
        title = "Count Over Time",
        subtitle = lab_subtitle
      )
    viz_theme <-
      temisc::theme_te_b_facet() +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "none")

    viz <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_char))
    if (geom == "bar") {
      viz <-
        viz +
        ggplot2::geom_bar(ggplot2::aes_string(y = "..count..", fill = color_char))
    } else if (geom == "hist") {
      viz <-
        viz +
        ggplot2::geom_histogram(ggplot2::aes_string(y = "..count..", fill = color_char),
                                bins = 30)
    }

    viz <-
      viz +
      ggplot2::scale_fill_manual(values = color) +
      viz_labs +
      viz_theme

    if (fill_cnt > 1) {
      viz <-
        viz +
        ggplot2::facet_wrap(stats::as.formula(paste0("~", color_char)), scales = "free")
    }

    viz
  }


#' Visualize over multiple time periods
#'
#'
#' @description Visualize time-based data over time.
#' @details Calls \code{visualize_bytime()}.
#' @param data data.frame (single).
#' @param x_chars character (vector).
#' @param geoms character (vector).
#' @param labs_subtitle character (vector).
#' @param colors character (vector).
#' @param color_chars character (vector).
#' @return ggplots
visualize_bytime_batched <- function(data = NULL,
                                     x_chars = c("timestamp", "yyyy", "mm", "wd", "hh"),
                                     geoms = c("hist", "bar", "bar", "bar", "bar"),
                                     labs_subtitle = c("", "By Year", "By Month", "By Day of Week", "By Hour"),
                                     colors = rep("grey50", 5),
                                     color_chars) {
  if (is.null(data)) {
    stop("`data` must not be NULL.", call. = FALSE)
  }

  num_x_chars <- length(x_chars)
  if (!(num_x_chars == (length(geoms) & num_x_chars == length(labs_subtitle) & num_x_chars == length(colors))))
    stop("List elements must have same length.", call. = FALSE)

  data_rep <- tidyr::nest(data)
  # This is temisc::repeat_df().
  num_rep <- length(x_chars)
  data_rep <- data[rep(seq_len(nrow(data)), num_rep), ]
  viz_bytime_params <-
    list(
      data = data_rep$data,
      x_char = x_chars,
      geom = geoms,
      color = colors,
      lab_subtitle = labs_subtitle
    )

  if (!missing(color_chars)) {
    if (length(color_chars != num_x_chars)) stop("List elements must have same length.", call. = FALSE)
    viz_bytime_params$color_char <- color_chars
  }

  helper_func <-
    function(data,
             x_char,
             geom,
             lab_subtitle,
             color,
             color_char = NULL) {
      visualize_bytime(
        data = data,
        x_char = x_char,
        geom = geom,
        lab_subtitle = lab_subtitle,
        color_char = color_char,
        color = color
      )
    }

  viz_bytime_list <- purrr::pmap(viz_bytime_params, helper_func)

}
