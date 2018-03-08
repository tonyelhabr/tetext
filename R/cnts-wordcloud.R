
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
#' @param wordcloud_base function. Works similarly to other \code{_base} arguments.
#' @param wordcloud_params function. Works similarly to other \code{_params} arguments.
#' @return plot.
#' @rdname visualize_cnts_wordcloud
#' @export
visualize_cnts_wordcloud_at <-
  function(data = NULL,
           token = NULL,
           wordcloud_base = default_wordcloud(data, token),
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

#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @inheritParams visualize_cnts_wordcloud
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
