

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
           scale_manual_params = default_scale_manual(),
           labs_base = default_labs(),
           labs_params = list(title = "Count of Words"),
           theme_base = default_theme(panel.grid.major.y = ggplot2::element_blank()),
           theme_params = list()) {
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

    viz <- viz + ggplot2::coord_flip()
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
           scale_manual_params = default_scale_manual(),
           labs_base = default_labs(),
           labs_params = list(title = "Count Over Time"),
           theme_base = default_theme(panel.grid.major.y = ggplot2::element_blank(), panel.background = ggplot2::element_rect()),
           theme_params = list(),
           facet_base = default_facet(facet),
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
