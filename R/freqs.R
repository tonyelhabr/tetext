

compute_freqs_at_old <- function(data = NULL, token = NULL) {
  stopifnot(!is.null(data), is.data.frame(data))
  stopifnot(!is.null(token), is.character(token))

  freq <- n <- NULL

  token_quo <- rlang::sym(token)

  data %>%
    dplyr::group_by(!!token_quo) %>%
    dplyr::mutate(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!token_quo) %>%
    dplyr::summarise(freq = sum(n) / n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(freq))
}

compute_freqs_old <-
  function(..., token) {
    stopifnot(!missing(token))
    token <- rlang::quo_text(rlang::enquo(token))
    compute_freqs_at_old(..., token = token)
  }

#' @inheritParams visualize_time_at
#' @inheritParams visualize_cnts_at
#' @inheritParams compute_freqs_at
#' @return data.frame.
#' @rdname compute_freqs
#' @export
compute_freqs_facet_at <-
  function(data = NULL,
           token = NULL,
           facet = NULL) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(facet), is.character(facet))

    freq <- n <- total <- NULL

    token_quo <- rlang::sym(token)
    facet_quo <- rlang::sym(facet)

    ngrams_cnt_1 <- data %>% dplyr::count(!!facet_quo)
    ngrams_cnt_2 <- data %>% dplyr::count(!!facet_quo, !!token_quo)

    ngrams_cnt_2 %>%
      dplyr::left_join(ngrams_cnt_1 %>% dplyr::rename(total = n), by = facet) %>%
      dplyr::mutate(freq = n / total) %>%
      dplyr::arrange(!!facet_quo, dplyr::desc(freq))
  }

#' @rdname compute_freqs
#' @export
compute_freqs_facet <-
  function(..., token, facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    facet <- rlang::quo_text(rlang::enquo(facet))
    compute_freqs_at(..., token = token, facet = facet)
  }

#' Compute token frequency
#'
#' @description Compute the frequency of tokens.
#' @details None.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param token bare for NSE; character for SE. Name of column in \code{data} to use for count.
#' @param facet character. Name of column in \code{data} to use for facetting.
#' It is set to NULL by default in the non-\code{facet} function even though it is not necessary.
#' If specified with the non-\code{facet} version of the function,
#' then the function acts just like the \code{_facet} version.
#' @return data.frame.
#' @rdname compute_freqs
#' @export
compute_freqs_at <-
  function(data = NULL, token = NULL, facet = NULL) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))

    if (is.null(facet)) {
      add_dummy <- TRUE
      data <- data %>% dplyr::mutate(`.facet` = "dummy")
      facet <- ".facet"
    } else {
      add_dummy <- FALSE
    }

    out <-
      data %>%
      compute_freqs_facet_at(token = token, facet = facet)
    if(add_dummy) {
      out <- out %>% dplyr::select(-dplyr::one_of(c(facet)))
    }
    out
  }

#' @rdname compute_freqs
#' @export
compute_freqs <-
  function(..., token, facet) {
    stopifnot(!missing(token))
    token <- rlang::quo_text(rlang::enquo(token))
    if(missing(facet)) {
      facet <- NULL
    } else {
      facet <- rlang::quo_text(rlang::enquo(facet))
    }
    compute_freqs_at(..., token = token, facet = facet)
  }

#' Compute token frequencies
#'
#' @description Manipulates output from \code{compute_freqs_facet_at()}
#' for \code{compute_freqs_mult_by2} function.
#' @details To be used by \code{_by2} function only.
#' @inheritParams compute_freqs_facet_at
#' @rdname compute_freqs
#' @export
compute_freqs_facet_wide_at <-
  function(...,
           token = NULL,
           facet = NULL) {

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

    out <-
      data_proc %>%
      dplyr::select(!!facet_quo, !!token_quo, freq) %>%
      tidyr::spread(!!facet_quo, freq)

    if(ncol(out) == 2) {
      out <- append_dummy_cols(out, num_cols_expect = 3)
    }
    out
  }

#' Visualize bigrams
#'
#' @description Visualize bigrams with dots sized according to frequency.
#' @details \code{compute_freqs_facet_at()} should NOT be called beforehand.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @inheritParams compute_freqs_facet
#' @return gg.
#' @rdname visualize_bigram_freqs_facet
#' @export
#' @seealso \url{https://buzzfeednews.github.io/2018-01-trump-twitter-wars/}
visualize_bigram_freqs_facet_at <-
  function(...,
           token = NULL,
           facet = NULL,
           color = facet,
           num_top = 3,
           scale_manual_params = default_scale_manual(),
           labs_base = default_labs(),
           labs_params = list(title = "Most Frequently Used Token Pairs"),
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
      dplyr::mutate(!!token_quo := factor(!!token_quo))


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
      do_call_scale_manual(scale_manual_params, type = "color")

    viz <-
      viz +
      labs_base + do_call_labs(labs_params) +
      theme_base + do_call_theme(theme_params)

    viz <-
      viz +
      ggplot2::coord_flip()
  }

#' @rdname visualize_bigram_freqs_facet
#' @export
visualize_bigram_freqs_facet <-
  function(..., token, color, facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    facet <- rlang::quo_text(rlang::enquo(facet))
    if (missing(color)) {
      # NOTE: THIS IS 'UNIQUE' TO THIS FUNCTION.
      # color <- NULL
      color <- facet
    } else {
      color <- rlang::quo_text(rlang::enquo(color))
    }
    visualize_bigram_freqs_facet_at(
      ...,
      token = token,
      color = color,
      facet = facet
    )
  }


#' Compute token frequencies in pairs
#'
#' @description Apply \code{compute_freqs_facet_wide_at()} to
#' \code{data} in pairs of \code{facet} values.
#' @details Calls \code{wrapper_func} internally.
#' @inheritParams wrapper_func
#' @inheritParams compute_freqs_facet_at
#' @param ... dots. Additional parameters to pass to \code{compute_freqs_facet_wide_at()}.
#' @return data.frame.
#' @rdname compute_freqs_facet_by2
#' @export
compute_freqs_facet_by2_at <-
  function(data = NULL,
           func = compute_freqs_facet_wide_at,
           xy_grid = NULL,
           xy_nms = NULL,
           # token = NULL,
           facet = NULL,
           ...) {

    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(func), is.function(func))
    # stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(facet), is.character(facet))


    if(is.null(xy_grid) | is.null(xy_nms)) {
      facets <-
        data %>%
        pull_distinctly_at(facet)
      xy_grid <- create_xy_grid(facets)
      xy <- NULL
      xy_nms <- xy_grid %>% dplyr::pull(xy)
      message("Generating output for all combinations of `facet`.")
    }

    wrapper_func(
      data = data,
      func = func,
      xy_grid = xy_grid,
      xy_nms = xy_nms,
      # token = token,
      facet = facet,
      ...
    )
  }

#' @rdname compute_freqs_facet_by2
#' @export
compute_freqs_facet_by2 <-
  function(...,
           token,
           facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    facet <- rlang::quo_text(rlang::enquo(facet))
    compute_freqs_facet_by2_at(
      ...,
      token = token,
      facet = facet
    )
  }


#' Visualize token frequencies in pairs
#'
#' @description Visualize token frequenceis across pairs of \code{facet} values.
#' @details \code{compute_freqs_facet_by2_at()} should NOT be called beforehand.
#' @inheritParams visualize_time_facet_at
#' @inheritParams compute_freqs_facet_at
#' @inheritParams compute_freqs_facet_by2_at
#' @param add_labels logical. Whether or not to add text labels (of the tokens).
#' @param filter_facet logical. Whether or not to filter the \code{facet} values.
#' \code{facet} values to include and exclude. Not used if \code{filter_facet = FALSE}.
#' @param facet_main of same type as \code{facet} values. Value of single 'main' facet.
#' @param filter_facet_base stuff. Work similarly to other \code{_base} arguments.
#' @param filter_facet_params list. Works similarly to other \code{_params} arguments.
#' @return gg.
#' @rdname visualize_freqs_facet_by2
#' @export
visualize_freqs_facet_by2_at <-
  function(data = NULL,
           token = NULL,
           facet = "name_xy",
           xy_nms,
           xy_grid,
           filter_facet = FALSE,
           facet_main = NULL,
           filter_facet_base = default_filter_facet(facet_main),
           filter_facet_params = list(),
           color = NULL,
           add_labels = TRUE,
           scale_manual_params = default_scale_manual(),
           labs_base = default_labs(),
           labs_params = list(title = "Relative Token Frequency"),
           theme_base = default_theme_facet(),
           theme_params = list(),
           facet_base = default_facet("name_xy"),
           facet_params = list()) {

    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(facet), is.character(facet))

    if(missing(xy_grid) | missing(xy_nms)) {
      facets <-
        data %>%
        pull_distinctly_at(facet)
      xy_grid <- create_xy_grid(facets)
      xy <- NULL
      xy_nms <- xy_grid %>% dplyr::pull(xy)
      message("Generating output for all combinations of `facet`.")
    }

    data_proc <-
      compute_freqs_facet_by2_at(
        data = data,
        xy_grid = xy_grid,
        xy_nms = xy_nms,
        token = token,
        facet = facet
      )

    data_proc <-
      data_proc %>%
      validate_x_main(
        filter_x = filter_facet,
        x = facet,
        xs = data %>% pull_distinctly_at(facet),
        x_main = facet_main
      )

    if(facet != "name_xy") {
      message("Correcting facetting variable to `name_xy`.")
    }
    data_proc <-
      data_proc %>%
      filter_data_facet_at(
        filter_facet = filter_facet,
        params = combine_base_and_params(filter_facet_base, filter_facet_params)
      )

    name_xy <- name_x <- name_y <- NULL

    data_proc <-
      data_proc %>%
      dplyr::mutate(name_xy = paste0(name_x, " vs. ", name_y))

    if (is.null(color)) {
      data_proc <- data_proc %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
    # NOTE: This is not necessary here.
    # data_proc <- wrangle_color_col(data_proc, color)

    viz <-
      data_proc %>%
      ggplot2::ggplot(ggplot2::aes_string(x = "x", y = "y", color = color))

    if(add_labels) {
      viz <-
        viz +
        ggplot2::geom_text(
          ggplot2::aes_string(
            label = token,
            size = "x + y"
          ),
          check_overlap = TRUE
        )
    } else {
      viz <-
        viz +
        ggplot2::geom_point(ggplot2::aes_string(size = "x + y"))
    }

    viz <-
      viz +
      do_call_scale_manual(scale_manual_params, type = "color") +
      ggplot2::scale_x_log10(labels = NULL) +
      ggplot2::scale_y_log10(labels = NULL) +
      ggplot2::geom_abline(color = "red")

    viz <-
      viz + generate_facets(facet_base, facet_params)
    viz <-
      viz +
      labs_base + do_call_labs(labs_params) +
      theme_base + do_call_theme(theme_params)
  }

#' @rdname visualize_freqs_facet_by2
#' @export
visualize_freqs_facet_by2 <-
  function(..., token, color, facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    facet <- rlang::quo_text(rlang::enquo(facet))
    if (missing(color)) {
      color <- NULL
    } else {
      color <- rlang::quo_text(rlang::enquo(color))
    }
    visualize_freqs_facet_by2_at(..., token = token, color = color, facet = facet)
  }

