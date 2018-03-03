

#' Compute n-gram frequency
#'
#' @description Compute the frequency of n-grams.
#' @details Inspired by \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param word character. Name of column in \code{data} to use for count. Default is provided
#' @return data.frame.
#' @rdname compute_freqs
#' @export
compute_freqs_at <- function(data = NULL, word = "word") {

    if(is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)

    freq <- n <- NULL

    word_quo <- rlang::sym(word)

    out <-
      data %>%
      dplyr::group_by(!!word_quo) %>%
      dplyr::mutate(n = n()) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!word_quo) %>%
      dplyr::summarise(freq = sum(n) / n()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(freq))
    out
}


#' @rdname compute_freqs
#' @export
compute_freqs <- compute_freqs_at

#' @inheritParams visualize_time_at
#' @inheritParams visualize_cnts_at
#' @inheritParams compute_freqs_at
#' @param multi character. Name of column in \code{data} corresponding to group
#' by which count is made.
#' @return data.frame.
#' @rdname compute_freqs
#' @export
compute_freqs_multi_at <-
  function(data = NULL,
           word = "word",
           multi = NULL) {

    if(is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)
    if(is.null(multi)) stop("`multi` cannot be NULL.", call. = FALSE)

    freq <- n <- total <- NULL

    word_quo <- rlang::sym(word)
    multi_quo <- rlang::sym(multi)

    ngrams_cnt_1 <- data %>% dplyr::count(!!multi_quo)
    ngrams_cnt_2 <- data %>% dplyr::count(!!multi_quo, !!word_quo)

    out <-
      ngrams_cnt_2 %>%
      dplyr::left_join(ngrams_cnt_1 %>% dplyr::rename(total = n), by = multi) %>%
      dplyr::mutate(freq = n / total) %>%
      dplyr::arrange(!!multi_quo, dplyr::desc(freq))
    out
  }

#' @rdname compute_freqs
#' @export
compute_freqs_multi <- compute_freqs_multi_at

#' Compute n-gram frequencies
#'
#' @description Manipulates output from \code{compute_freqs_multi_at()}
#' for \code{compute_freqs_mult_by2} function.
#' @details To be used by \code{_by2} function only.
#' @inheritParams compute_freqs_multi_at
#' @rdname compute_freqs
#' @export
compute_freqs_multi_wide_at <-
  function(data = NULL,
           word = "word",
           multi = NULL) {

    data_proc <-
      compute_freqs_multi_at(
        data = data,
        word = word,
        multi = multi
      )

    word_quo <- rlang::sym(word)
    multi_quo <- rlang::sym(multi)

    freq <- NULL

    out <-
      data_proc %>%
      dplyr::select(!!multi_quo, !!word_quo, freq) %>%
      tidyr::spread(!!multi_quo, freq)

    if(ncol(out) == 2) {
      out <- append_dummy_cols(out, num_cols_expect = 3)
    }
    out
  }

#' Visualize bigrams
#'
#' @description Visualize bigrams with dots sized according to frequency.
#' @details \code{compute_freqs_multi_at()} should NOT be called beforehand.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @inheritParams compute_freqs_multi
#' @return gg.
#' @rdname visualize_bigram_freqs_multi
#' @export
#' @seealso \url{https://buzzfeednews.github.io/2018-01-trump-twitter-wars/}
visualize_bigram_freqs_multi_at <-
  function(data = NULL,
           word = "word",
           multi = NULL,
           color = multi,
           color_value = "grey50",
           num_top = 3,
           lab_title = "Most Frequently Used Pairs of Words",
           lab_subtitle = paste0("By ", stringr::str_to_title(multi)),
           lab_x = NULL,
           lab_y = NULL,
           theme_base = theme_tetext()) {

    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    if (is.null(multi))
      stop("`multi` cannot be NULL.", call. = FALSE)

    data_proc <-
      compute_freqs_multi_at(data = data,
                             word = word,
                             multi = multi)

    data_proc <- wrangle_multi_col(data_proc, multi)

    word_quo <- rlang::sym(word)
    multi_quo <- rlang::sym(multi)

    freq <- NULL

    data_viz <-
      data_proc %>%
      dplyr::group_by(!!multi_quo) %>%
      filter_num_top_at("freq", num_top) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!multi_quo) %>%
      dplyr::mutate(!!word_quo := stringr::str_replace_all(!!word_quo, " ", "\n")) %>%
      dplyr::mutate(!!word_quo := forcats::fct_reorder(factor(!!word_quo), freq))


    if (is.null(color)) {
      data_viz <- data_viz %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"

    }
    data_viz <- wrangle_color_col(data_viz, color)

    viz <-
      ggplot2::ggplot(
        data = data_viz,
        ggplot2::aes_string(
          x = multi,
          y = word,
          color = color,
          size = "freq")
      ) +
      ggplot2::geom_point() +
      ggplot2::scale_y_discrete(position = "right") +
      ggplot2::scale_color_manual(values = color_value) +
      ggplot2::scale_size_area(max_size = 25) +
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
        legend.position = "none"
      )

    viz <-
      viz +
      viz_labs +
      viz_theme
    viz
  }

#' @rdname visualize_bigram_freqs_multi
#' @export
visualize_bigram_freqs_multi <- visualize_bigram_freqs_multi_at


#' Compute n-gram frequencies in pairs
#'
#' @description Apply \code{compute_freqs_multi_wide_at()} to
#' \code{data} in pairs of \code{multi} values.
#' @details Calls \code{wrapper_func} internally.
#' @inheritParams wrapper_func
#' @inheritParams compute_freqs_multi_at
#' @param ... dots. Additional parameters to pass to \code{compute_freqs_multi_wide_at()}.
#' @return data.frame.
#' @rdname compute_freqs_multi_by2
#' @export
compute_freqs_multi_by2_at <-
  function(data = NULL,
           func = compute_freqs_multi_wide_at,
           xy_grid = NULL,
           xy_nms = NULL,
           # word = "word",
           multi = NULL,
           ...) {

    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    # if (is.null(word))
    #   stop("`word` cannot be NULL.", call. = FALSE)
    if (is.null(multi))
      stop("`multi` cannot be NULL.", call. = FALSE)

    if(is.null(xy_grid) | is.null(xy_nms)) {
      multis <-
        data %>%
        pull_distinctly_at(multi)
      xy_grid <- create_xy_grid(multis)
      xy <- NULL
      xy_nms <- xy_grid %>% dplyr::pull(xy)
      message("Generating output for all combinations of `multi`.")
    }

    wrapper_func(
      data = data,
      func = func,
      xy_grid = xy_grid,
      xy_nms = xy_nms,
      # word = word,
      multi = multi,
      ...
    )
  }

#' @rdname compute_freqs_multi_by2
#' @export
compute_freqs_multi_by2 <- compute_freqs_multi_by2_at


#' Visualize n-gram frequencies in pairs
#'
#' @description Visualize n-gram frequenceis across pairs of \code{multi} values.
#' @details \code{compute_freqs_multi_by2_at()} should NOT be called beforehand.
#' @inheritParams visualize_time_multi_at
#' @inheritParams compute_freqs_multi_at
#' @inheritParams compute_freqs_multi_by2_at
#' @param add_labels logical. Whether or not to add text labels (of the n-grams).
#' @param filter_multi logical. Whether or not to filter the \code{multi} values.
#' @param multi_main character. Name of single \code{multi} value to use as basis.
#' Not used if \code{filter_multi = FALSE}.
#' @param x_include,y_include,x_exclude,y_exclude character (vector).
#' \code{multi} values to include and exclude. Not used if \code{filter_multi = FALSE}.
#' @return gg.
#' @rdname visualize_freqs_multi_by2
#' @export
visualize_freqs_multi_by2_at <-
  function(data = NULL,
           word = "word",
           multi = NULL,
           xy_nms,
           xy_grid,
           filter_multi = FALSE,
           multi_main = NULL,
           x_include = NULL,
           y_include = NULL,
           x_exclude = NULL,
           y_exclude = NULL,
           color = NULL,
           color_value = "grey50",
           # num_top = 3,
           add_labels = TRUE,
           lab_title = "Relative Word Frequency",
           lab_subtitle = paste0("By ", stringr::str_to_title(multi)),
           lab_x = NULL,
           lab_y = NULL,
           theme_base = theme_tetext_facet(),
           facet_scales = "free",
           facet_ncol = 3,
           facet_nrow = NULL,
           facet_strip_position = "top") {

    if(missing(xy_grid) | missing(xy_nms)) {
      multis <-
        data %>%
        pull_distinctly_at(multi)
      xy_grid <- create_xy_grid(multis)
      xy <- NULL
      xy_nms <- xy_grid %>% dplyr::pull(xy)
      message("Generating output for all combinations of `multi`.")
    }

    data_proc <-
      compute_freqs_multi_by2_at(
        data = data,
        xy_grid = xy_grid,
        xy_nms = xy_nms,
        word = word,
        multi = multi
      )

    data_proc <-
      data_proc %>%
      validate_x_main(
        filter_x = filter_multi,
        x = multi,
        xs = data %>% pull_distinctly_at(multi),
        x_main = multi_main
      )

    data_proc <-
      data_proc %>%
      filter_data_multi_at(
        filter_multi = filter_multi,
        multi_main = multi_main,
        x_include = x_include,
        y_include = y_include,
        x_exclude = x_exclude,
        y_exclude = y_exclude
      )

    name_xy <- name_x <- name_y <- NULL

    data_proc <-
      data_proc %>%
      dplyr::mutate(name_xy = paste0(name_x, " vs. ", name_y))

    data_proc <- wrangle_multi_col(data_proc, "name_xy")

    # if (is.null(color)) {
    #   data_proc <- data_proc %>% dplyr::mutate(`.dummy` = "dummy")
    #   color <- ".dummy"
    #
    # }
    # data_proc <- wrangle_color_col(data_proc, color)

    viz <-
      data_proc %>%
      ggplot2::ggplot(ggplot2::aes_string(x = "x", y = "y", color = color))

    if(add_labels) {
      viz <-
        viz +
        ggplot2::geom_text(
          ggplot2::aes_string(
            label = word,
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
      # ggplot2::scale_color_manual(values = color_value) +
      # ggplot2::scale_x_log10(labels = scales::percent_format()) +
      # ggplot2::scale_y_log10(labels = scales::percent_format()) +
      ggplot2::scale_x_log10(labels = NULL) +
      ggplot2::scale_y_log10(labels = NULL) +
      ggplot2::geom_abline(color = "red") +
      ggplot2::facet_wrap(
        ~ name_xy,
        scales = facet_scales,
        ncol = facet_ncol,
        nrow = facet_nrow,
        strip.position = facet_strip_position
      )

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
        legend.position = "none"
      )

    viz <-
      viz +
      viz_labs +
      viz_theme
    viz
  }

#' @rdname visualize_freqs_multi_by2
#' @export
visualize_freqs_multi_by2 <- visualize_freqs_multi_by2_at

