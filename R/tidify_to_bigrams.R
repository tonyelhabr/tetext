

#' Prepare data for text analysis.
#'
#' @description Create a tidy data.frame of unigrams.
#' @details Heavily influence by \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' @inheritParams tidify_to_unigrams
#' @param colname_words character. Name of output column for token. Default: 'bigram'
#' @return data.frame. In tidy format. 'word1' and 'word2' columns are named
#' 'first' and 'second' respectively.
#' @export
#' @importFrom rlang sym !!
#' @importFrom dplyr mutate rename anti_join filter
#' @importFrom stringr str_replace_all str_detect
#' @importFrom tidytext unnest_tokens get_stopwords
#' @importFrom tidyr separate
tidify_to_bigrams <-
  function(data = NULL,
           colname_text = "text",
           colname_words = "bigram",
           rgx_pattern,
           rgx_replacement,
           stopwords = TRUE,
           stopwords_lexicon,
           rgx_ignore_custom) {

    if(is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)
    colname_text_quo <- rlang::sym(colname_text)
    colname_words_quo <- rlang::sym(colname_words)
    colname_word1_quo <- rlang::sym("first")
    colname_word2_quo <- rlang::sym("second")

    word <- NULL

    out <- data
    if (!missing(rgx_pattern) & !missing(rgx_replacement)) {
      out <-
        dplyr::mutate(out,
                      text = stringr::str_replace_all(!!colname_text_quo, rgx_pattern, rgx_replacement))
    }
    out <- tidytext::unnest_tokens(out, !!colname_words_quo, !!colname_text_quo, token = "ngrams", n = 2)
    out <- tidyr::separate(out, !!colname_words_quo,
                           into = c("first", "second"), sep = " ", remove = FALSE)
    if(stopwords) {

      if(missing(stopwords_lexicon)) {
        stop_words <- tidytext::stop_words
      } else {
        stop_words <- tidytext::get_stopwords(source = stopwords_lexicon)
      }
      out <- dplyr::rename(out, word = !!colname_word1_quo)
      out <- dplyr::anti_join(out, stop_words, by = "word")
      out <- dplyr::rename(out, !!colname_word1_quo := word)

      out <- dplyr::rename(out, word = !!colname_word2_quo)
      out <- dplyr::anti_join(out, stop_words, by = "word")
      out <- dplyr::rename(out, !!colname_word2_quo := word)
    }
    if (!missing(rgx_ignore_custom)) {
      out <-
        dplyr::filter(out, !stringr::str_detect(!!colname_word1_quo, rgx_ignore_custom))
      out <-
        dplyr::filter(out, !stringr::str_detect(!!colname_word2_quo, rgx_ignore_custom))
    }

    out <- dplyr::filter(out, stringr::str_detect(!!colname_word1_quo, "[a-z]"))
    out <- dplyr::filter(out, stringr::str_detect(!!colname_word2_quo, "[a-z]"))
    out
  }
