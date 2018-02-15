

#' Visualize counts
#'
#' @description Visualize n-gram counts with a word cloud.
#' @details 'multi' version is for creating multiple plots simulatenously
#' (probably using a \code{purrr} 'map' function).
#' Fairly original function.
#' @param colname_word character. Name of column in \code{data} to use for words. Default: 'word'.
#' @param colname_freq character. Name of column in \code{data} to use for frequencies. Default: 'n'.
#' @inheritParams visualize_time
#' @return plot
#' @export
#' @importFrom rlang sym !!
#' @importFrom dplyr select
#' @importFrom wordcloud worcloud
visualize_cnts_wordcloud <-
  function(data = NULL,
           colname_word = "word",
           colname_freq = "n",
           color = "grey50",
           max_words = 50) {
    # TODO: Figure out how to make the "name" column generic.
    # Avoiding the dependence on `dplyr`.
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)

    colname_word_quo <- rlang::sym(colname_word)
    colname_freq_quo <- rlang::sym(colname_req)

    viz <-
      wordcloud::wordcloud(
        word = dplyr::select(data, !!colname_word_quo),
        freq = dplyr::select(data, !!colname_freq_quo),
        random.order = FALSE,
        colors = color,
        max.words = max_words
      )
    viz
  }

#' @inheritParams visualize_cnts_wordcloud
#' @param colname_multi character. Name of column in \code{data} to use to create
#' a separate wordcloud.
#' @rdname visualize_cnts_wordcloud
#' @export
#' @importFrom dplyr filter
visualize_cnts_wordcloud_multi <-
  function(..., colname_multi = NULL, value_multi = NULL) {
    colname_multi_quo <- rlang::sym(colname_multi)
    data_proc <- dplyr::filter(data, !!colname_multi_quo := value_multi)
    viz <- visualize_cnts_wordcloud(data = data_proc, ...)
    viz
  }

