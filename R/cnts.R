
#' Visualize counts
#'
#' @description Visualize n-gram counts.
#' @details 'multi' version is for facetting. 'y' value is calculated internally
#' with \code{dplyr::count()}.
#' @inheritParams visualize_time
#' @param word character. Name of column in \code{data} to use for words.
#' Probably something like 'word' or 'bigram'.
#' @param num_top numeric. Number of words to show. If between 0 and 1, then assumed to be a percentage.
#' @return gg.
#' @rdname visualize_cnts
#' @export
#' @seealso \url{https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_R/2016-08-09-trump-data.Rmd}.
visualize_cnts_at <- function(data = NULL,
                           word = "word",
                           num_top = 10,
                           color = NULL,
                           color_value = "grey50",
                           lab_title = "Count of Words",
                           lab_subtitle = NULL,
                           lab_x = NULL,
                           lab_y = NULL,
                           theme_base = theme_tetext()) {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)

  word_quo <- rlang::sym(word)

  n <- rank <- NULL

  data_viz <-
    data %>%
    dplyr::count(!!word_quo) %>%
    filter_num_top_at("n", num_top) %>%
    dplyr::mutate(!!word_quo := stats::reorder(!!word_quo, n))


  if (is.null(color)) {
    data_viz <- data_viz %>% dplyr::mutate(`.dummy` = "dummy")
    color <- ".dummy"
  }
  data_viz <- wrangle_color_col(data_viz, color)

  viz <-
    ggplot2::ggplot(
      data = data_viz,
      ggplot2::aes_string(x = word, y = "n", fill = color)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = color_value) +
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

#' @rdname visualize_cnts
#' @export
visualize_cnts <- visualize_cnts_at

#' @details Unfortunately, it seems difficult to combine this with \code{visualize_cnts_at()}
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param multi character. Name of column to use as 'facetting' variable. Not used
#' if not specified.
#' @return gg.
#' @rdname visualize_cnts
#' @export
visualize_cnts_multi_at <- function(data = NULL,
                                 word = "word",
                                 num_top = 10,
                                 color = NULL,
                                 color_value = "grey50",
                                 lab_title = "Count of Words",
                                 lab_subtitle = paste0("By ", stringr::str_to_title(multi)),
                                 lab_x = NULL,
                                 lab_y = NULL,
                                 theme_base = theme_tetext_facet(),
                                 multi) {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)

  word_quo <- rlang::sym(word)
  multi_quo <- rlang::sym(multi)
  data <- wrangle_multi_col(data, multi)

  n <- rank <- NULL

  data_viz <-
    data %>%
    dplyr::count(!!multi_quo, !!word_quo) %>%
    dplyr::group_by(!!multi_quo) %>%
    filter_num_top_at("n", num_top) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      !!word_quo := reorder_within(!!word_quo, n, !!multi_quo)
    )

  if (is.null(color)) {
    data_viz <- data_viz %>% dplyr::mutate(`.dummy` = "dummy")
    color <- ".dummy"
  }
  data_viz <- wrangle_color_col(data_viz, color)

  viz <-
    ggplot2::ggplot(
      data = data_viz,
      ggplot2::aes_string(x = word, y = "n", fill = color)) +
    ggplot2::geom_col() +
    scale_x_reordered() +
    ggplot2::scale_fill_manual(values = color_value) +
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

#' @rdname visualize_cnts
#' @export
visualize_cnts_multi <- visualize_cnts_multi_at

#' Visualize counts
#'
#' @description Visualize n-gram counts with a word cloud.
#' @details 'multi' version is for creating multiple plots simulatenously
#' (probably using a \code{purrr} 'map' function).
#' Fairly original function.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param word character. Name of column in \code{data} to use for words.
#' @param num_top numeric. Number of words to show.
#' @param random_order logical. Passed directly to \code{random.order} parameter
#' in \code{wordcloud::wordcloud()}
#' @inheritParams visualize_time
#' @return plot.
#' @rdname visualize_cnts_wordcloud
#' @export
visualize_cnts_wordcloud_at <-
  function(data = NULL,
           word = "word",
           color_value = "grey50",
           num_top = 50,
           random_order = FALSE) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)
    word_quo <- rlang::sym(word)
    data_viz <- data %>% dplyr::count(!!word_quo)

    n <- NULL

    if(num_top < 1) {
      num_top <- nrow(data) * (1 - invert_pct(num_top))
    }

    words <- data_viz %>% dplyr::pull(!!word_quo)
    freqs <- data_viz %>% dplyr::pull(n)
    # browser()
    viz <-
      wordcloud::wordcloud(
        word = words,
        freq = freqs,
        colors = color_value,
        max.words = num_top,
        random.order = random_order
      )
    viz
  }

#' @rdname visualize_cnts_wordcloud
#' @export
visualize_cnts_wordcloud <- visualize_cnts_wordcloud_at

#' @inheritParams visualize_time_at
#' @inheritParams visualize_cnts_at
#' @inheritParams visualize_cnts_wordcloud_at
#' @param ... dots. Parameters to pass to \code{visualize_cnts_wordcloud_at()}.
#' @param multi character. Name of column in \code{data} to use to create
#' a separate wordcloud.
#' @param all logical. Whether or not to create a wordcloud for each value of \code{multi}.
#' @param value_multi charater or numeric. Value by which to filter the column
#' specified by \code{multi}. Ignored if \code{all = TRUE}.
#' @param num_par_row,num_par_col numeric. Values to pass to \code{mfrow} in \code{par}.
#' Defaults are provided internally based on value of \code{all} and number of \code{multi} values.
#' @return plots.
#' @rdname visualize_cnts_wordcloud
#' @export
visualize_cnts_wordcloud_multi_at <-
  function(data = NULL, ..., multi = NULL, all = FALSE, value_multi = NULL, num_par_row, num_par_col) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)
    if (is.null(multi))
      stop("`multi` must not be NULL.", call. = FALSE)

    multi_quo <- rlang::sym(multi)
    if(!all) {
      if (is.null(value_multi))
        stop("`value_multi` must not be NULL.", call. = FALSE)

      data_proc <- data %>% dplyr::filter(!!multi_quo == value_multi)
      # if(missing(num_par_row)) {
      #   num_par_row <- 1
      # }
      # if(missing(num_par_col)) {
      #   num_par_col <- 1
      # }
      # graphics::par(mfrow = c(num_par_row, num_par_col))
      visualize_cnts_wordcloud_at(data = data_proc, ...)
    } else {
      message("`all = TRUE` is not currently implemented.")
      # multis <- data %>% pull_distinctly_at(multi)
      #
      # if(missing(num_par_row)) {
      #   num_par_row <- ceiling(length(multis) / 3)
      # }
      # if(missing(num_par_col)) {
      #   num_par_col <- min(length(multis), 3)
      # }
      # graphics::par(mfrow = c(num_par_row, num_par_col))
      # purrr::map(
      #   ~visualize_cnts_wordcloud_multi_at(
      #     data = unigrams,
      #     ...
      #   )
      # )
    }
    # graphics::par(mfrow = c(1, 1))
  }

#' @rdname visualize_cnts_wordcloud
#' @export
visualize_cnts_wordcloud_multi <- visualize_cnts_wordcloud_multi_at


