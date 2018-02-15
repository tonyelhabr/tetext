


#' Visualize bigrams
#'
#' @description Visualize bigrams with dots.
#' @details None.
#' @inheritParams visualize_time
#' @param colname_word character. Name of column in \code{data} to use for bigram. Default: 'word'
#' @param colname_freq character. Name of column in \code{data} to use for ordering and sizing. Default: 'freq'
#' @param num_top numeric.
#' @return gg
#' @export
#' @importFrom rlang sym
#' @importFrom dplyr group_by mutate row_number desc filter ungroup arrange
#' @importFrom stringr str_to_title str_replace_all
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 labs theme ggplot geom_point scale_y_discrete scale_color_manual scale_size_area coord_flip
#' @importFrom temisc theme_te_b
#' @seealso \url{https://buzzfeednews.github.io/2018-01-trump-twitter-wars/}
visualize_bigrams_freqs_multi <-
  function(data = NULL,
           colname_multi = NULL,
           colname_word = "word",
           colname_freq = "freq",
           color = "grey50",
           num_top = 10,
           lab_title = "Most Frequently Used Pairs of Words",
           lab_subtitle = paste0("By ", stringr::str_to_title(colname_multi)),
           theme_base = temisc::theme_te_b()) {

    if(is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)
    if(is.null(colname_multi)) stop("`colname_multi` cannot be NULL.", call. = FALSE)

    freq <- word <- NULL

    colname_x_quo <- rlang::sym(colname_multi)
    colname_bigram_quo <- rlang::sym(colname_word)
    colname_freq_quo <- rlang::sym(colname_freq)

    data_proc <- dplyr::group_by(data, !!colname_multi)
    data_proc <- dplyr::mutate(data_proc, rank = dplyr::row_number(dplyr::desc(freq)))
    data_proc <- dplyr::filter(data_proc, rank <= num_top)
    data_proc <- dplyr::ungroup(data_proc)
    data_proc <- dplyr::arrange(data_proc, !!colname_multi)
    data_proc <- dplyr::mutate(data_proc, word = stringr::str_replace_all(word, " ", "\n"))
    data_proc <- dplyr::mutate(data_proc, word = forcats::fct_reorder(factor(word), freq))

    viz_labs <-
      ggplot2::labs(
        x = NULL,
        y = NULL,
        title = lab_title,
        subtitle = lab_subtitle
      )

    viz_theme <-
      theme_base +
      ggplot2::theme(
        legend.position = "none"
      )

    viz <-
      ggplot2::ggplot(
        data = data_proc,
        aes_string(x = colname_multi,
                   y = colname_word,
                   color = colname_multi,
                   size = colname_freq)
        ) +
      ggplot2::geom_point() +
      ggplot2::scale_y_discrete(position = "right") +
      ggplot2::scale_color_manual(values = color) +
      ggplot2::scale_size_area(max_size = 25) +
      ggplot2::coord_flip()
    viz
  }

