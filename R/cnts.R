
#' Visualize counts
#'
#' @description Visualize n-gram counts.
#' @details 'multi' version is for facetting. 'y' value is calculated internally
#' with \code{dplyr::count()}.
#' @inheritParams visualize_time_at
#' @param word character. Name of column in \code{data} to use for words.
#' Probably something like 'word' or 'bigram'. Default is provided.
#' @param num_top numeric. Number of words to show. Default is provided.
#' @return gg.
#' @rdname visualize_cnts_at
#' @export
#' @importFrom dplyr count filter row_number desc mutate
#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot aes_string scale_color_manual coord_flip labs theme element_blank
#' @importFrom ggalt geom_lollipop
#' @importFrom temisc theme_te_a
#' @seealso \url{https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_R/2016-08-09-trump-data.Rmd}.
visualize_cnts_at <- function(data = NULL,
                           word = "word",
                           num_top = 10,
                           color = NULL,
                           color_value = "grey80",
                           lab_title = "Count of Words",
                           lab_subtitle = NULL,
                           lab_x = NULL,
                           lab_y = NULL,
                           theme_base = temisc::theme_te_a()) {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)

  word_quo <- rlang::sym(word)

  data_viz <-
    data %>%
    dplyr::count(!!word_quo) %>%
    dplyr::filter(dplyr::row_number(dplyr::desc(n)) <= num_top) %>%
    dplyr::mutate(!!word_quo := stats::reorder(!!word_quo, n))


  if (is.null(color)) {
    data_viz$color_value <- "dummy"
    color <- "color_value"
  }
  data_viz <- wrangle_color_col(data_viz, color)

  viz <-
    ggplot2::ggplot(
      data = data_viz,
      ggplot2::aes_string(x = word, y = "n", color_value = color)) +
    ggalt::geom_lollipop(size = 2, point.size = 4) +
    ggplot2::scale_color_manual(values = color_value) +
    ggplot2::coord_flip()

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
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank()
    )

  viz <-
    viz +
    viz_labs +
    viz_theme
  viz
}

#' @details Unfortunately, it seems difficult to combine this with \code{visualize_cnts_at()}
#' @inheritParams visualize_time_at
#' @inheritParams visualize_cnts_at
#' @param multi character. Name of column to use as 'facetting' variable. Not used
#' if not specified.
#' @rdname visualize_cnts_at
#' @return gg.
#' @export
#' @importFrom stringr str_to_title
#' @importFrom temisc theme_te_a_facet
#' @importFrom drlib reorder_within scale_x_reordered
visualize_cnts_multi_at <- function(data = NULL,
                                 word = "word",
                                 num_top = 10,
                                 color = NULL,
                                 color_value = "grey80",
                                 lab_title = "Count of Words",
                                 lab_subtitle = paste0("By ", stringr::str_to_title(multi)),
                                 lab_x = NULL,
                                 lab_y = NULL,
                                 theme_base = temisc::theme_te_a_facet(),
                                 multi) {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)

  word_quo <- rlang::sym(word)
  multi_quo <- rlang::sym(multi)
  data <- wrangle_multi_col(data, multi)

  data_viz <-
    data %>%
    dplyr::count(!!multi_quo, !!word_quo) %>%
    dplyr::group_by(!!multi_quo) %>%
    dplyr::filter(dplyr::row_number(dplyr::desc(n)) <= num_top) %>%
    dplyr::mutate(!!word_quo :=
                    drlib::reorder_within(!!word_quo,
                                          n,
                                          !!multi_quo)) %>%
    # dplyr::mutate(word = drlib::reorder_within(word, n, yyyy)) %>%
    dplyr::ungroup()


  if (is.null(color)) {
    data_viz$color_value <- "dummy"
    color <- "color_value"
  }
  data_viz <- wrangle_color_col(data_viz, color)

  viz <-
    ggplot2::ggplot(
      data = data_viz,
      ggplot2::aes_string(x = word, y = "n", color_value = color)) +
    ggalt::geom_lollipop(size = 2, point.size = 4) +
    drlib::scale_x_reordered() +
    ggplot2::scale_color_manual(values = color_value) +
    ggplot2::facet_wrap(stats::as.formula(paste0("~", multi)), scales = "free") +
    ggplot2::coord_flip()

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
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank()
    )

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
#' @inheritParams visualize_time_at
#' @inheritParams visualize_cnts_at
#' @param word character. Name of column in \code{data} to use for words. Default is provided.
#' @param num_top numeric. Number of words to show.
#' @param random_order logical. Passed directly to \code{random.order} parameter
#' in \code{wordcloud::wordcloud()}
#' @inheritParams visualize_time_at
#' @return plot.
#' @rdname visualize_cnts_wordcloud_at
#' @export
#' @importFrom dplyr count pull
#' @importFrom wordcloud wordcloud
visualize_cnts_wordcloud_at <-
  function(data = NULL,
           word = "word",
           color_value = "grey80",
           num_top = 50,
           random_order = FALSE) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)
    word_quo <- rlang::sym(word)
    data_viz <- data %>% dplyr::count(!!word_quo)

    words <- dplyr::pull(data_viz, !!word_quo)
    freqs <- dplyr::pull(data_viz, n)
    viz <-
      wordcloud::wordcloud(
        word = words,
        freq = freqs,
        color_values = color_value,
        max.words = num_top,
        random.order = random_order
      )
    viz
  }

#' @inheritParams visualize_time_at
#' @inheritParams visualize_cnts_at
#' @inheritParams visualize_cnts_wordcloud_at
#' @param ... dots. Parameters to pass to \code{visualize_cnts_wordcloud_at()}.
#' @param multi character. Name of column in \code{data} to use to create
#' a separate wordcloud.
#' @param value_multi charater or numeric. Value by which to filter the column
#' specified by \code{multi}.
#' @return plots.
#' @rdname visualize_cnts_wordcloud_at
#' @export
#' @importFrom dplyr filter
visualize_cnts_wordcloud_multi_at <-
  function(data = NULL, ..., multi = NULL, value_multi = NULL) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)

    if (is.null(multi))
      stop("`multi` must not be NULL.", call. = FALSE)
    if (is.null(value_multi))
      stop("`value_multi` must not be NULL.", call. = FALSE)

    multi_quo <- rlang::sym(multi)
    data_proc <- dplyr::filter(data, !!multi_quo == value_multi)
    viz <- visualize_cnts_wordcloud_at(data = data_proc, ...)
    viz
  }




