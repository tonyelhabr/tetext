
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
#' @inheritFrom rlang sym !! :=
#' @inheritFrom dplyr %>% count filter row_number desc mutate
#' @inheritFrom ggplot2 ggplot aes_string scale_color_manual coord_flip labs theme element_blank
#' @inheritFrom ggalt geom_lollipop
#' @inehritFrom temisc theme_te_b
visualize_cnts <- function(data = NULL,
                           colname_x = "word",
                           colname_y = "n",
                           num_top = 10,
                           colname_color = NULL,
                           color = "grey50") {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)

  x_quo <- rlang::sym(colname_x)
  if (is.null(colname_color)) {
    data$color <- "dummy"
    colname_color <- "color"
  }
  data_proc <-
    data %>%
    dplyr::count(!!x_quo, sort = TRUE) %>%
    dplyr::filter(dplyr::row_number(dplyr::desc(n)) <= num_top) %>%
    dplyr::mutate(!!x_quo := reorder(!!x_quo, n))
  viz_labs <-
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Count of Words"
    )
  viz_theme <-
    temisc::theme_te_b() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank()
    )
  viz <-
    data_proc %>%
    ggplot2::ggplot(ggplot2::aes_string(x = colname_x, y = colname_y, color = colname_color)) +
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
#' @importFrom drlib reorder_within scale_x_reordered
visualize_cnts_multi <- function(data = NULL,
                                 colname_x = NULL,
                                 num_top = 10,
                                 colname_color,
                                 color = "grey50",
                                 colname_multi) {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)
  if (is.null(colname_x))
    stop("`colname_x` cannot be NULL.", call. = FALSE)

  x_quo <- rlang::sym(colname_x)
  fill_quo <- rlang::sym(colname_color)

  facet_quo <- rlang::sym(colname_multi)
  data_proc <-
    data %>%
    dplyr::count(!!facet_quo, !!x_quo, sort = TRUE) %>%
    dplyr::group_by(!!facet_quo) %>%
    dplyr::filter(dplyr::row_number(dplyr::desc(n)) <= num_top) %>%
    # dplyr::mutate(!!x_quo := drlib::reorder_within(!!x_quo, n, !!facet_quo)) %>%
    dplyr::mutate(!!x_quo := drlib::reorder_within(word, n, name)) %>%
    dplyr::ungroup()
  viz <-
    data_proc %>%
    ggplot2::ggplot(ggplot2::aes_string(x = colname_x, y = "n")) +
    ggalt::geom_lollipop(size = 2, point.size = 4)

  viz +
    ggalt::geom_lollipop(ggplot2::aes_string(color = colname_color),
                         size = 2,
                         point.size = 4) +
    ggplot2::scale_color_manual(values = color) +
    drlib::scale_x_reordered() +
    ggplot2::facet_wrap(stats::as.formula(paste0("~", colname_multi)), scales = "free") +
    temisc::theme_te_b_facet() +
    ggplot2::labs(subtitle = paste0("By ", stringr::str_to_title(colname_multi)))
  viz <-
    viz +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::labs(title = "Count of Words") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  viz
}
