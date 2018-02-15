

#' Visualize counts
#'
#' @description Visualize n-gram counts with a word cloud.
#' @details 'multi' version is for creating multiple plots simulatenously
#' (probably using a \code{purrr} 'map' function).
#' Fairly original function.
#' @param colname_word character. Name of column in \code{data} to use for words. Default: 'word'.
#' @param colname_freq character. Name of column in \code{data} to use for frequencies. Default: 'n'.
#' @param num_top numeric. Number of words to show.
#' @param random_order logical. Passed directly to \code{random.order} parameter
#' in \code{wordcloud::wordcloud()}
#' @inheritParams visualize_time
#' @return plot
#' @export
#' @importFrom rlang sym !!
#' @importFrom dplyr select
#' @importFrom wordcloud wordcloud
visualize_cnts_wordcloud <-
  function(data = NULL,
           colname_word = "word",
           colname_freq = "n",
           color = "grey50",
           num_top = 50,
           random_order = FALSE) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)

    colname_word_quo <- rlang::sym(colname_word)
    colname_freq_quo <- rlang::sym(colname_freq)

    viz <-
      wordcloud::wordcloud(
        word = dplyr::select(data, !!colname_word_quo),
        freq = dplyr::select(data, !!colname_freq_quo),
        colors = color,
        max.words = num_top,
        random.order = random_order
      )
    viz
  }

#' @inheritParams visualize_cnts_wordcloud
#' @param ... dots. Parameters to pass to \code{visualize_cnts_wordcloud()}.
#' @param colname_multi character. Name of column in \code{data} to use to create
#' a separate wordcloud.
#' @param value_multi charater or numeric. Value by which to filter the column
#' specified by \code{colname_multi}.
#' @rdname visualize_cnts_wordcloud
#' @export
#' @importFrom rlang sym !! :=
#' @importFrom dplyr filter
visualize_cnts_wordcloud_multi <-
  function(data = NULL, ..., colname_multi = NULL, value_multi = NULL) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)

    if (is.null(colname_multi))
      stop("`colname_multi` must not be NULL.", call. = FALSE)
    if (is.null(value_multi))
      stop("`value_multi` must not be NULL.", call. = FALSE)

    colname_multi_quo <- rlang::sym(colname_multi)
    data_proc <- dplyr::filter(data, !!colname_multi_quo := value_multi)
    viz <- visualize_cnts_wordcloud(data = data_proc, ...)
    viz
  }

