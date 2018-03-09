

#' Compute TF-IDF
#'
#' @description Compute term-frequency, inverse-document-frequency.
#' @details Calls \code{tidytext::bind_tf_idf()} internally. Note that the
#' output 'tfidf' column is named \code{tf_idf} because this is the convention
#' of the \code{tidytext} package.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param token bare for NSE; character for SE. Name of column in \code{data} to use
#' as \code{term} in \code{tidytext::bind_tf_idf()}.
#' @param doc bare for NSE; character for SE. Name of column in \code{data} to use
#' as \code{document} in \code{tidytext::bind_tf_idf()}.
#' @return data.frame.
#' @rdname compute_tfidf
#' @export
compute_tfidf_at <-
  function(data = NULL,
           token = NULL,
           doc = NULL) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(doc), is.character(doc))

    token_quo <- rlang::sym(token)
    doc_quo <- rlang::sym(doc)

    n <- tf_idf <- NULL

    data %>%
      dplyr::count(!!doc_quo, !!token_quo, sort = TRUE) %>%
      tidytext::bind_tf_idf(!!token_quo, !!doc_quo, n) %>%
      dplyr::arrange(!!doc_quo, dplyr::desc(tf_idf))
  }

#' @rdname compute_tfidf
#' @export
compute_tfidf <-
  function(..., token, doc) {
    stopifnot(!missing(token))
    stopifnot(!missing(doc))
    token <- rlang::quo_text(rlang::enquo(token))
    doc <- rlang::quo_text(rlang::enquo(doc))
    compute_tfidf_at(..., token = token, doc = doc)
  }

#' Visualize TF-IDF
#'
#' @description Visualize term-frequency, inverse-document-frequency.
#' @details Calls \code{compute_tfidf_at()} internally.
#' Cannot pass dots to \code{compute_tfidf_at()} internally because parameters
#' are used in subsequent processing.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @inheritParams compute_tfidf
#' @return gg.
#' @rdname visualize_tfidf
#' @export
#' @seealso \url{https://www.tidytextmining.com/tfidf.html}.
#' \url{https://juliasilge.com/blog/sherlock-holmes-stm/}
visualize_tfidf_at <-
  function(...,
           token = NULL,
           doc = NULL,
           facet = doc,
           color = facet,
           num_top = 10,
           scale_manual_base = default_scale_manual(),
           scale_manual_params = list(),
           labs_base = default_labs(),
           labs_params = list(title = "Highest TF-IDF Words"),
           theme_base =
             default_theme(axis.text.y = ggplot2::element_text(angle = 30, hjust = 1),
                           panel.grid.major.y = ggplot2::element_blank()),
           theme_params = list(),
           facet_base = default_facet(facet),
           facet_params = list()) {

    # stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(doc), is.character(doc))
    stopifnot(!is.null(facet), is.character(facet))

    data_proc <-
      compute_tfidf_at(
        # data = data,
        ...,
        token = token,
        doc = doc
      )

    token_quo <- rlang::sym(token)
    # tfidf_quo <- rlang::sym("tf_idf")
    facet_quo <- rlang::sym(facet)

    tf_idf <- NULL

    data_proc <-
      data_proc %>%
      dplyr::group_by(!!facet_quo) %>%
      filter_num_top_at("tf_idf", num_top) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        !!token_quo := reorder_within(!!token_quo, tf_idf, !!facet_quo)
      )

    if (is.null(color)) {
      data_proc <- data_proc %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
    # NOTE: THIS IS NEEDED!
    data_proc <- wrangle_color_col(data_proc, color)

    viz <-
      data_proc %>%
      ggplot2::ggplot(ggplot2::aes_string(x = token, y = "tf_idf", fill = color)) +
      ggplot2::geom_col() +
      generate_scale_manual(scale_manual_base, scale_manual_params, type = "fill") +
      scale_x_reordered()

    viz <- viz + generate_facets(facet_base, facet_params)

    viz <-
      viz +
      generate_labs(labs_base, labs_params) +
      generate_theme(theme_base, theme_params)

    viz <-
      viz +
      ggplot2::coord_flip()
  }

#' @rdname visualize_tfidf
#' @export
visualize_tfidf <-
  function(..., token, doc, facet, color) {
    stopifnot(!missing(token))
    stopifnot(!missing(doc))
    token <- rlang::quo_text(rlang::enquo(token))
    doc <- rlang::quo_text(rlang::enquo(doc))
    if (missing(facet)) {
      # NOTE: THIS IS 'UNIQUE' TO THIS FUNCTION.
      # color <- NULL
      facet <- doc
    } else {
      facet <- rlang::quo_text(rlang::enquo(facet))
    }
    if (missing(color)) {
      # NOTE: THIS IS 'UNIQUE' TO THIS FUNCTION.
      # color <- NULL
      color <- facet
    } else {
      color <- rlang::quo_text(rlang::enquo(color))
    }
    visualize_tfidf_at(..., token = token, doc = doc, facet = facet, color = color)
  }


