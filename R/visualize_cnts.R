
#' Visualize counts
#'
#' @description Visualize n-gram counts.
#' @details 'multi' version is for facetting.
#' @inheritParams visualize_time
#' @param colname_x character. Name of column in \code{data} to use for x-axis.
#' Probably something like 'word' or 'bigram'.
#' @param colname_y character.
#' @param num_top numeric. Default: 10.
#' @return gg
#' @export
#' @importFrom rlang sym !! :=
#' @importFrom dplyr count filter row_number desc mutate
#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot aes_string scale_color_manual coord_flip labs theme element_blank
#' @importFrom ggalt geom_lollipop
#' @importFrom temisc theme_te_b
#' @seealso \url{https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_R/2016-08-09-trump-data.Rmd}.
visualize_cnts <- function(data = NULL,
                           colname_x = "word",
                           colname_y = "n",
                           num_top = 10,
                           colname_color = NULL,
                           color = "grey50",
                           lab_title = "Count of Words",
                           lab_subtitle = NULL,
                           theme_base = temisc::theme_te_b()) {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)

  colname_x_quo <- rlang::sym(colname_x)
  if (is.null(colname_color)) {
    data$color <- "dummy"
    colname_color <- "color"
  }

  data_proc <- dplyr::count(data, !!colname_x_quo, sort = TRUE)
  data_proc <- dplyr::filter(data_proc, dplyr::row_number(dplyr::desc(n)) <= num_top)
  data_proc <- dplyr::mutate(data_proc, !!colname_x_quo := stats::reorder(!!colname_x_quo, n))
  viz_labs <-
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = lab_title,
      subtitle = lab_subtitle
    )
  viz_theme <-
    # temisc::theme_te_b() +
    theme_base +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank()
    )
  viz <-
    ggplot2::ggplot(
      data = data_proc,
      ggplot2::aes_string(x = colname_x, y = colname_y, color = colname_color)) +
    ggalt::geom_lollipop(size = 2, point.size = 4) +
    ggplot2::scale_color_manual(values = color) +
    ggplot2::coord_flip()

  viz <-
    viz +
    viz_labs +
    viz_theme
  viz
}

#' @inheritParams visualize_cnts
#' @param colname_multi character. Name of column to use as 'facetting' variable. Not used
#' if not specified.
#' @rdname visualize_cnts
#' @export
#' @importFrom stringr str_to_title
#' @importFrom drlib reorder_within scale_x_reordered
visualize_cnts_multi <- function(data = NULL,
                                 colname_x = "word",
                                 colname_y = "n",
                                 num_top = 10,
                                 colname_color = NULL,
                                 color = "grey50",
                                 lab_title = "Count of Words",
                                 lab_subtitle = paste0("By ", stringr::str_to_title(colname_multi)),
                                 theme_base = temisc::theme_te_b_facet(),
                                 colname_multi) {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)

  colname_x_quo <- rlang::sym(colname_x)
  colname_y_quo <- rlang::sym(colname_y)
  colname_multi_quo <- rlang::sym(colname_multi)

  cnt <- word <- NULL
  if (is.null(colname_color)) {
    data$color <- "dummy"
    colname_color <- "color"
  }

  colname_multi_quo <- rlang::sym(colname_multi)

  data_proc <- dplyr::count(data, !!colname_multi_quo, !!colname_x_quo, sort = TRUE)
  data_proc <- dplyr::group_by(data_proc, !!colname_multi_quo)
  data_proc <- dplyr::filter(data_proc, dplyr::row_number(dplyr::desc(n)) <= num_top)
  # NOTE: Not sure if `drlib::reorder_within works` with tidyeval!
  data_proc <-
    dplyr::mutate(data_proc,
                  !!colname_x_quo := drlib::reorder_within(!!colname_x_quo,
                                                           !!colname_y_quo,
                                                           !!colname_multi_quo))
  data_proc <- dplyr::ungroup(data_proc)

  viz_labs <-
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = lab_title,
      subtitle = lab_subtitle
    )
  viz_theme <-
    # temisc::theme_te_b() +
    theme_base +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank()
    )
  viz <-
    ggplot2::ggplot(
      data = data_proc,
      ggplot2::aes_string(x = colname_x, y = colname_y, color = colname_color)) +
    ggalt::geom_lollipop(size = 2, point.size = 4) +
    ggplot2::scale_color_manual(values = color) +
    ggplot2::facet_wrap(stats::as.formula(paste0("~", colname_multi)), scales = "free") +
    ggplot2::coord_flip()

  viz <-
    viz +
    viz_labs +
    viz_theme

  viz
}
