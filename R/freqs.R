

#' Compute n-gram frequency
#'
#' @description Compute the frequency of n-grams.
#' @details Inspired by \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param colname_word character. Name of column in \code{data} to use for count. Default is provided
#' @return data.frame.
#' @rdname compute_freqs
#' @export
#' @importFrom dplyr n count mutate summarise arrange desc
compute_freqs <- function(data = NULL, colname_word = "word") {

    if(is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)

    freq <- n <- NULL

    colname_word_quo <- rlang::sym(colname_word)

    out <-
      data %>%
      dplyr::group_by(!!colname_word_quo) %>%
      dplyr::mutate(n = n()) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!colname_word_quo) %>%
      dplyr::summarise(freq = sum(n) / n()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(freq))
    out
}

#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @inheritParams compute_freqs
#' @param colname_multi character. Name of column in \code{data} corresponding to group
#' by which count is made.
#' @return data.frame.
#' @rdname compute_freqs
#' @export
#' @importFrom dplyr left_join rename
compute_freqs_multi <-
  function(data = NULL,
           colname_word = "word",
           colname_multi = NULL) {

    if(is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)
    if(is.null(colname_multi)) stop("`colname_multi` cannot be NULL.", call. = FALSE)

    freq <- n <- total <- NULL

    colname_word_quo <- rlang::sym(colname_word)
    colname_multi_quo <- rlang::sym(colname_multi)

    ngrams_cnt_1 <- data %>% dplyr::count(!!colname_multi_quo)
    ngrams_cnt_2 <- data %>% dplyr::count(!!colname_multi_quo, !!colname_word_quo)

    out <-
      ngrams_cnt_2 %>%
      dplyr::left_join(ngrams_cnt_1 %>% dplyr::rename(total = n), by = colname_multi) %>%
      dplyr::mutate(freq = n / total) %>%
      dplyr::arrange(dplyr::desc(freq))
    out
  }




#' Visualize bigrams
#'
#' @description Visualize bigrams with dots sized according to frequency.
#' @details \code{compute_freqs_multi()} should NOT be called beforehand.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @inheritParams compute_freqs_multi
#' @return gg.
#' @rdname compute_freqs
#' @export
#' @importFrom dplyr group_by mutate row_number desc filter ungroup arrange
#' @importFrom stringr str_to_title str_replace_all
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 labs theme ggplot geom_point aes_string scale_y_discrete scale_color_manual scale_size_area coord_flip
#' @importFrom temisc theme_te_a
#' @seealso \url{https://buzzfeednews.github.io/2018-01-trump-twitter-wars/}
visualize_bigram_freqs_multi <-
  function(data = NULL,
           colname_word = "word",
           colname_multi = NULL,
           colname_color = colname_multi,
           color = "grey50",
           num_top = 3,
           lab_title = "Most Frequently Used Pairs of Words",
           lab_subtitle = paste0("By ", stringr::str_to_title(colname_multi)),
           lab_x = NULL,
           lab_y = NULL,
           theme_base = temisc::theme_te_a()) {

    if(is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)
    if(is.null(colname_multi)) stop("`colname_multi` cannot be NULL.", call. = FALSE)

    data_proc <-
      compute_freqs_multi(
        data = data,
        colname_word = colname_word,
        colname_multi = colname_multi
      )

    data_proc <- wrangle_multi_col(data_proc, colname_multi)

    freq <- word <- NULL

    colname_word_quo <- rlang::sym(colname_word)
    colname_multi_quo <- rlang::sym(colname_multi)

    data_viz <-
      data_proc %>%
      dplyr::group_by(!!colname_multi_quo) %>%
      dplyr::mutate(rank = dplyr::row_number(dplyr::desc(freq))) %>%
      dplyr::filter(rank <= num_top) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!colname_multi_quo) %>%
      dplyr::mutate(!!colname_word_quo := stringr::str_replace_all(!!colname_word_quo, " ", "\n")) %>%
      dplyr::mutate(!!colname_word_quo := forcats::fct_reorder(factor(!!colname_word_quo), freq))


    if (is.null(colname_color)) {
      data_viz$color <- "dummy"
      colname_color <- "color"

    }
    data_viz <- wrangle_color_col(data_viz, colname_color)

    viz <-
      ggplot2::ggplot(
        data = data_viz,
        ggplot2::aes_string(
          x = colname_multi,
          y = colname_word,
          color = colname_color,
          size = "freq")
      ) +
      ggplot2::geom_point() +
      ggplot2::scale_y_discrete(position = "right") +
      ggplot2::scale_color_manual(values = color) +
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




