

#' Prepare data for text analysis.
#'
#' @description Create a tidy data.frame of unigrams.
#' @details Heavily influence by \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' @param data data.frame. Not in 'tidy' format.
#' @param colname_text character. Name of column in \code{data} to parse. Default: 'text'
#' @param colname_word character. Name of output column for token. Default: 'word'
#' @param rgx_pattern character. Regular expression to substitute.
#' @param rgx_replacement character. Regular expression used as replacement.
#' @param rgx_unnest character. Regular expression to use in \code{tidytext::unnest_tokens()}.
#' If not specified, then not used.
#' @param stopwords logical. Whether or not to remove stopwords. Default: TRUE
#' @param stopwords_lexicon character. If not specified, then uses all stop words
#' available in \code{tidytext} package.
#' Otherwise, must be a lexicon available in the \code{tidytext} package.
#' @param rgx_ignore_custom character. Custom regular expression to remove.
#' @return data.frame. In tidy format.
#' @importFrom rlang sym !!
#' @importFrom dplyr mutate rename anti_join filter
#' @importFrom stringr str_replace_all str_detect
#' @importFrom tidytext unnest_tokens get_stopwords
#' @seealso \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' \url{https://www.tidytextmining.com/ngrams.html}.
#' \url{https://www.tidytextmining.com/twitter.html}.
tidify_to_unigrams <-
  function(data = NULL,
           colname_text = "text",
           colname_word = "word",
           rgx_pattern,
           rgx_replacement,
           rgx_unnest,
           stopwords = TRUE,
           stopwords_lexicon,
           rgx_ignore_custom) {

    if(is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)
    colname_text_quo <- rlang::sym(colname_text)
    colname_word_quo <- rlang::sym(colname_word)

    word <- NULL

    out <- data
    if (!missing(rgx_pattern) & !missing(rgx_replacement)) {
      out <-
        dplyr:: mutate(out,
               text = stringr::str_replace_all(!!colname_text_quo, rgx_pattern, rgx_replacement))
    }

    if (missing(rgx_unnest)) {
      out <-
        tidytext::unnest_tokens(out, !!colname_word_quo, !!colname_text_quo)
    } else {
      out <-
        tidytext::unnest_tokens(out, !!colname_word_quo, !!colname_text_quo, rgx_unnest)
    }

    if(stopwords) {

      if(missing(stopwords_lexicon)) {
        stop_words <- tidytext::stop_words
      } else {
        stop_words <- tidytext::get_stopwords(source = stopwords_lexicon)
      }

      # NOTE: Not sure why, but tidyeval is not wokring with `dplyr::anti_join()`,
      # so using a work-around.
      # out <- dplyr::anti_join(out, stop_words, by = c(colname_word = "word"))
      out <- dplyr::rename(out, word = !!colname_word_quo)
      out <- dplyr::anti_join(out, stop_words, by = "word")
      out <- dplyr::rename(out, !!colname_word_quo := word)
    }

    if (!missing(rgx_ignore_custom)) {
      out <-
        dplyr::filter(out, !stringr::str_detect(!!colname_word_quo, rgx_ignore_custom))
    }

    out <- dplyr::filter(out, stringr::str_detect(!!colname_word_quo, "[a-z]"))
    out
  }
