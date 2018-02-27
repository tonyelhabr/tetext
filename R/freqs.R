

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
      dplyr::arrange(dplyr::desc(freq))
    out
  }

#' @rdname compute_freqs
#' @export
compute_freqs_multi <- compute_freqs_multi_at


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

    if(is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)
    if(is.null(multi)) stop("`multi` cannot be NULL.", call. = FALSE)

    data_proc <-
      compute_freqs_multi_at(
        data = data,
        word = word,
        multi = multi
      )

    data_proc <- wrangle_multi_col(data_proc, multi)

    word_quo <- rlang::sym(word)
    multi_quo <- rlang::sym(multi)

    data_viz <-
      data_proc %>%
      dplyr::group_by(!!multi_quo) %>%
      filter_num_top("freq", num_top) %>%
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

