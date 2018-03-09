

#' Visualize bigrams
#'
#' @description Visualize bigrams with dots sized according to frequency.
#' @details \code{compute_freqs_facet_at()} should NOT be called beforehand.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @inheritParams compute_freqs_facet
#' @return gg.
#' @rdname visualize_bigram_freqs
#' @export
#' @seealso \url{https://buzzfeednews.github.io/2018-01-trump-twitter-wars/}
visualize_bigram_freqs_at <-
  function(...,
           token = NULL,
           facet = NULL,
           color = facet,
           num_top = 3,
           scale_manual_base = default_scale_manual(),
           scale_manual_params = list(),
           labs_base = default_labs(),
           labs_params = list(title = "Most Frequently Used Word Pairs"),
           theme_base = default_theme(),
           theme_params = list()) {

    # stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(facet), is.character(facet))

    data_proc <-
      compute_freqs_facet_at(
        # data = data,
        ...,
        token = token,
        facet = facet
      )

    token_quo <- rlang::sym(token)
    facet_quo <- rlang::sym(facet)

    freq <- NULL

    data_proc <-
      data_proc %>%
      dplyr::group_by(!!facet_quo) %>%
      filter_num_top_at("freq", num_top) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!facet_quo) %>%
      dplyr::mutate(!!token_quo := stringr::str_replace_all(!!token_quo, " ", "\n")) %>%
      # dplyr::mutate(!!token_quo := factor(!!token_quo))
      dplyr::mutate(!!token_quo := stats::reorder(!!token_quo, freq))


    if (is.null(color)) {
      data_proc <- data_proc %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
    # NOTE: NEED THIS!
    data_proc <- wrangle_color_col(data_proc, color)

    viz <-
      ggplot2::ggplot(
        data = data_proc,
        ggplot2::aes_string(
          x = facet,
          y = token,
          color = color,
          size = "freq")
      ) +
      ggplot2::geom_point() +
      ggplot2::scale_size_area(max_size = 25) +
      ggplot2::scale_y_discrete(position = "right") +
      generate_scale_manual(scale_manual_base, scale_manual_params, type = "color")

    viz <-
      viz +
      generate_labs(labs_base, labs_params) +
      generate_theme(theme_base, theme_params)

    viz <-
      viz +
      ggplot2::coord_flip()
  }

#' @rdname visualize_bigram_freqs
#' @export
visualize_bigram_freqs <-
  function(..., token, color, facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    facet <- rlang::quo_text(rlang::enquo(facet))
    if (missing(color)) {
      # NOTE: This is 'unique' to this function, since `color = facet` by default in the SE version.
      # color <- NULL
      color <- facet
    } else {
      color <- rlang::quo_text(rlang::enquo(color))
    }
    visualize_bigram_freqs_at(
      ...,
      token = token,
      color = color,
      facet = facet
    )
  }
