
#' Visualize counts
#'
#' @description Visualize n-gram counts.
#' @details 'multi' version is for facetting. 'y' value is calculated internally
#' with \code{dplyr::count{)}.
#' @inheritParams visualize_time
#' @param colname_word character. Name of column in \code{data} to use for x-axis.
#' Probably something like 'word' or 'bigram'. Default is provided.
#' @param num_top numeric. Default is provided.
#' @return gg
#' @export
#' @importFrom dplyr count filter row_number desc mutate
#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot aes_string scale_color_manual coord_flip labs theme element_blank
#' @importFrom ggalt geom_lollipop
#' @importFrom temisc theme_te_a
#' @seealso \url{https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_R/2016-08-09-trump-data.Rmd}.
visualize_cnts <- function(data = NULL,
                           colname_word = "word",
                           num_top = 10,
                           colname_color = NULL,
                           color = "grey50",
                           lab_title = "Count of Words",
                           lab_subtitle = NULL,
                           theme_base = temisc::theme_te_a()) {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)

  colname_word_quo <- rlang::sym(colname_word)
  if (is.null(colname_color)) {
    data$color <- "dummy"
    colname_color <- "color"
    data <- coerce_col_to_factor(data, colname_color)
  }

  data_viz <-
    data %>%
    dplyr::count(!!colname_word_quo) %>%
    dplyr::filter(dplyr::row_number(dplyr::desc(n)) <= num_top) %>%
    dplyr::mutate(!!colname_word_quo := stats::reorder(!!colname_word_quo, n))
  viz_labs <-
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = lab_title,
      subtitle = lab_subtitle
    )
  viz_theme <-
    theme_base +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank()
    )
  viz <-
    ggplot2::ggplot(
      data = data_viz,
      ggplot2::aes_string(x = colname_word, y = "n", color = colname_color)) +
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
#' @return gg
#' @export
#' @importFrom stringr str_to_title
#' @importFrom temisc theme_te_a_facet
#' @importFrom drlib reorder_within scale_x_reordered
visualize_cnts_multi <- function(data = NULL,
                                 colname_word = "word",
                                 num_top = 10,
                                 colname_color = NULL,
                                 color = "grey50",
                                 lab_title = "Count of Words",
                                 lab_subtitle = paste0("By ", stringr::str_to_title(colname_multi)),
                                 theme_base = temisc::theme_te_a_facet(),
                                 colname_multi) {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)

  colname_word_quo <- rlang::sym(colname_word)
  colname_multi_quo <- rlang::sym(colname_multi)

  if (is.null(colname_color)) {
    data$color <- "dummy"
    colname_color <- "color"
    data <- coerce_col_to_factor(data, colname_color)
  }

  colname_multi_quo <- rlang::sym(colname_multi)

  data <- coerce_col_to_factor(data, colname_multi)

  data_viz <-
    data %>%
    dplyr::count(!!colname_multi_quo, !!colname_word_quo) %>%
    dplyr::group_by(!!colname_multi_quo) %>%
    dplyr::filter(dplyr::row_number(dplyr::desc(n)) <= num_top) %>%
    dplyr::mutate(!!colname_word_quo :=
                    drlib::reorder_within(!!colname_word_quo,
                                          n,
                                          !!colname_multi_quo)) %>%
    # dplyr::mutate(word = drlib::reorder_within(word, n, yyyy)) %>%
    dplyr::ungroup()

  viz_labs <-
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = lab_title,
      subtitle = lab_subtitle
    )
  viz_theme <-
    theme_base +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank()
    )
  viz <-
    ggplot2::ggplot(
      data = data_viz,
      ggplot2::aes_string(x = colname_word, y = "n", color = colname_color)) +
    ggalt::geom_lollipop(size = 2, point.size = 4) +
    drlib::scale_x_reordered() +
    ggplot2::scale_color_manual(values = color) +
    ggplot2::facet_wrap(stats::as.formula(paste0("~", colname_multi)), scales = "free") +
    ggplot2::coord_flip()

  viz <-
    viz +
    viz_labs +
    viz_theme

  viz
}



#' Visualize counts
#'
#' @description Visualize n-gram counts with a word cloud.
#' @details 'multi' version is for creating multiple plots simulatenously
#' (probably using a \code{purrr} 'map' function).
#' Fairly original function.
#' @param colname_word character. Name of column in \code{data} to use for words. Default is provided.
#' @param num_top numeric. Number of words to show.
#' @param random_order logical. Passed directly to \code{random.order} parameter
#' in \code{wordcloud::wordcloud()}
#' @inheritParams visualize_time
#' @return plot
#' @export
#' @importFrom dplyr count pull
#' @importFrom wordcloud wordcloud
visualize_cnts_wordcloud <-
  function(data = NULL,
           colname_word = "word",
           color = "grey50",
           num_top = 50,
           random_order = FALSE) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)
    colname_word_quo <- rlang::sym(colname_word)
    data_viz <- data %>% dplyr::count(!!colname_word_quo)

    words <- dplyr::pull(data_viz, !!colname_word_quo)
    freqs <- dplyr::pull(data_viz, n)
    viz <-
      wordcloud::wordcloud(
        word = words,
        freq = freqs,
        colors = color,
        max.words = num_top,
        random.order = random_order
      )
    viz
  }

#' @inheritParams visualize_cnts_wordcloud
#' @param ... dots. Parameters to pass to \code{visualize_cnts_wordcloud()}.
#' @param colname_multi character. Name of column in \code{data} to use to create
#' a separate wordcloud.
#' @param value_multi charater or numeric. Value by which to filter the column
#' specified by \code{colname_multi}.
#' @rdname visualize_cnts_wordcloud
#' @export
#' @importFrom dplyr filter
visualize_cnts_wordcloud_multi <-
  function(data = NULL, ..., colname_multi = NULL, value_multi = NULL) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)

    if (is.null(colname_multi))
      stop("`colname_multi` must not be NULL.", call. = FALSE)
    if (is.null(value_multi))
      stop("`value_multi` must not be NULL.", call. = FALSE)

    colname_multi_quo <- rlang::sym(colname_multi)
    data_proc <- dplyr::filter(data, !!colname_multi_quo == value_multi)
    viz <- visualize_cnts_wordcloud(data = data_proc, ...)
    viz
  }




