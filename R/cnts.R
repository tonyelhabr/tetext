

#' Visualize counts
#'
#' @description Visualize token counts.
#' @details \code{facet} version is for facetting. 'y' value is calculated internally
#' with \code{dplyr::count()}.
#' @inheritParams visualize_time
#' @param token bare for NSE; character for SE. Name of column in \code{data} to use as unit for counting.
#' Probably something like 'word' or 'bigram'.
#' @param num_top numeric. Number of words to show. If between 0 and 1, then assumed to be a percentage.
#' @return gg.
#' @rdname visualize_cnts
#' @export
#' @seealso \url{https://github.com/dgrtwo/dgrtwo.github.com/blob/master/_R/2016-08-09-trump-data.Rmd}.
visualize_cnts_at <-
  function(data = NULL,
           token = NULL,
           color = NULL,
           num_top = 10,
           scale_manual_params = scale_manual_tetext(),
           labs_base = labs_tetext(),
           labs_params = list(title = "Count of Words"),
           theme_base = theme_tetext_dy(),
           theme_params = list(legend.position = "none")) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))

    token_quo <- rlang::sym(token)

    n <- NULL

    data_proc <-
      data %>%
      dplyr::count(!!token_quo) %>%
      filter_num_top_at("n", num_top) %>%
      dplyr::mutate(!!token_quo := stats::reorder(!!token_quo, n))


    if (is.null(color)) {
      data_proc <- data_proc %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
    # NOTE: DON'T NEED THIS!
    # data_proc <- wrangle_color_col(data_proc, color)

    viz <-
      ggplot2::ggplot(data = data_proc,
                      ggplot2::aes_string(x = token, y = "n", fill = color)) +
      ggplot2::geom_col() +
      do_call_scale_manual(scale_manual_params, type = "fill")

    viz <-
      viz +
      labs_base + do_call_labs(labs_params) +
      theme_base + do_call_theme(theme_params)

    viz <- viz + coord_flip()
  }

#' @rdname visualize_cnts
#' @export
visualize_cnts <-
  function(..., token, color) {
    stopifnot(!missing(token))
    token <- rlang::quo_text(rlang::enquo(token))
    if (missing(color)) {
      color <- NULL
    } else {
      color <- rlang::quo_text(rlang::enquo(color))
    }
    visualize_cnts_at(..., token = token, color = color)
  }

#' @details Unfortunately, it seems difficult to combine this with \code{visualize_cnts_at()}
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param facet bare for NSE; character for SE. Name of column to use as 'facetting' variable. Not used
#' if not specified.
#' @return gg.
#' @rdname visualize_cnts
#' @export
visualize_cnts_facet_at <-
  function(data = NULL,
           token = NULL,
           color = NULL,
           facet = NULL,
           num_top = 10,
           scale_manual_params = scale_manual_tetext(),
           labs_base = labs_tetext(),
           labs_params = list(title = "Count Over Time"),
           theme_base = theme_tetext_facet_dy(),
           theme_params = list(legend.position = "none"),
           facet_base = facet_tetext(facet),
           facet_params = list()) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(facet), is.character(facet))

    token_quo <- rlang::sym(token)
    facet_quo <- rlang::sym(facet)

    n <- NULL

    data_proc <-
      data %>%
      dplyr::count(!!facet_quo, !!token_quo) %>%
      dplyr::group_by(!!facet_quo) %>%
      filter_num_top_at("n", num_top) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!token_quo := reorder_within(!!token_quo, n, !!facet_quo))

    if (is.null(color)) {
      data_proc <- data_proc %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
    # NOTE: NEED THIS!
    data_proc <- wrangle_color_col(data_proc, color)

    viz <-
      data_proc %>%
      ggplot2::ggplot(ggplot2::aes_string(x = token, y = "n", fill = color)) +
      ggplot2::geom_col() +
      scale_x_reordered() +
      do_call_scale_manual(scale_manual_params, type = "fill")

    viz <-
      viz  +
      generate_facets(facet_base, facet_params)

    viz <-
      viz +
      labs_base + do_call_labs(labs_params) +
      theme_base + do_call_theme(theme_params)

    viz <-
      viz +
      ggplot2::coord_flip()
  }

#' @rdname visualize_cnts
#' @export
visualize_cnts_facet <-
  function(...,
           token,
           color,
           facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    facet <- rlang::quo_text(rlang::enquo(facet))
    if (missing(color)) {
      color <- NULL
    } else {
      color <- rlang::quo_text(rlang::enquo(color))
    }
    visualize_cnts_facet_at(..., token = token, color = color, facet = facet)
  }

#' Visualize counts
#'
#' @description Visualize token counts with a token cloud.
#' @details \code{facet} version is for creating multiple plots simulatenously
#' (probably using a \code{purrr} 'map' function).
#' Calls \code{wordcloud::wordcloud()}.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param token bare for NSE; character for SE. Name of column in \code{data}
#' to use for \code{words} argument in \code{wordcloud::wordcloud()}.
#' @param colors character. Passed directly to \code{colors} argument in \code{wordcloud::wordcloud()}.
#' @param num_top numeric. Number of words to show. Passed directly to \code{max.words} argument
#' @param random.order logical. Passed directly to \code{random.order} argument
#' in \code{wordcloud::wordcloud()}.
#' @inheritParams visualize_time
#' @return plot.
#' @rdname visualize_cnts_wordcloud
#' @export
visualize_cnts_wordcloud_at <-
  function(data = NULL,
           token = NULL,
           wordcloud_base = wordcloud_tetext(data, token),
           wordcloud_params = list()) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    generate_wordcloud(wordcloud_base, wordcloud_params)
  }

#' @rdname visualize_cnts_wordcloud
#' @export
visualize_cnts_wordcloud <-
  function(...,
           token) {
    stopifnot(!missing(token))
    token <- rlang::quo_text(rlang::enquo(token))
  visualize_cnts_wordcloud_at(..., token = token)
}

#' @inheritParams visualize_time_at
#' @inheritParams visualize_cnts_at
#' @inheritParams visualize_cnts_wordcloud_at
#' @param ... dots. Parameters to pass to \code{visualize_cnts_wordcloud_at()}.
#' @param facet bare for NSE; character for SE. Name of column in \code{data} to use to create
#' a separate wordcloud.
#' @param value_facet charater or numeric. Value by which to filter the column
#' specified by \code{facet}. Ignored if \code{all = TRUE}.
#' Defaults are provided internally based on value of \code{all} and number of \code{facet} values.
#' @return plots.
#' @rdname visualize_cnts_wordcloud
#' @export
visualize_cnts_wordcloud_facet_at <-
  function(data = NULL,
           ...,
           facet = NULL,
           value_facet = NULL) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(facet), is.character(facet))
    stopifnot(!is.null(value_facet))
    facet_quo <- rlang::sym(facet)
    data_proc <- data %>% dplyr::filter(!!facet_quo == value_facet)
    visualize_cnts_wordcloud_at(data = data_proc, ...)
  }

#' @rdname visualize_cnts_wordcloud
#' @export
visualize_cnts_wordcloud_facet <-
  function(...,
           token,
           facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    facet <- rlang::quo_text(rlang::enquo(facet))
    visualize_cnts_wordcloud_facet_at(..., token = token, facet = facet)
  }
