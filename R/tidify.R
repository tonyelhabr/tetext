


#' Prepare data for text analysis.
#'
#' @description Create a tidy data.frame of unigrams.
#' @details Heavily influence by \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
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
#' @return data.frame.
#' @rdname tidify_to_unigrams
#' @export
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
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)

    colname_text_quo <- rlang::sym(colname_text)
    colname_word_quo <- rlang::sym(colname_word)

    word <- NULL

    out <- data
    if (!missing(rgx_pattern) & !missing(rgx_replacement)) {
      out <-
        out %>%
        dplyr::mutate(text = stringr::str_replace_all(!!colname_text_quo, rgx_pattern, rgx_replacement))
    }

    if (missing(rgx_unnest)) {
      out <-
        out %>%
        tidytext::unnest_tokens(!!colname_word_quo, !!colname_text_quo)
    } else {
      out <-
        out %>%
        tidytext::unnest_tokens(!!colname_word_quo, !!colname_text_quo, rgx_unnest)
    }

    if (stopwords) {
      if (missing(stopwords_lexicon)) {
        stop_words <- tidytext::stop_words
      } else {
        stop_words <- tidytext::get_stopwords(source = stopwords_lexicon)
      }

      # NOTE: Not sure why, but tidyeval is not wokring with `dplyr::anti_join()`,
      # so using a work-around.
      # out <- dplyr::anti_join(out, stop_words, by = c(colname_word = "word"))
      out <-
        out %>%
        dplyr::rename(word = !!colname_word_quo) %>%
        dplyr::anti_join(stop_words, by = "word") %>%
        dplyr::rename(!!colname_word_quo := word)
    }

    if (!missing(rgx_ignore_custom)) {
      out <-
        out %>%
        dplyr::filter(!stringr::str_detect(!!colname_word_quo, rgx_ignore_custom))
    }

    out <-
      out %>%
      dplyr::filter(stringr::str_detect(!!colname_word_quo, "[a-z]"))
    out
  }



#' Prepare data for text analysis.
#'
#' @description Create a tidy data.frame of unigrams.
#' @details Heavily influence by \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @inheritParams tidify_to_unigrams
#' @return data.frame. In tidy format. 'word1' and 'word2' columns are named
#' 'first' and 'second' respectively.
#' @rdname tidify_to_unigrams
#' @export
#' @importFrom dplyr mutate rename anti_join filter
#' @importFrom stringr str_replace_all str_detect
#' @importFrom tidytext unnest_tokens get_stopwords
#' @importFrom tidyr separate
tidify_to_bigrams <-
  function(data = NULL,
           colname_text = "text",
           colname_word = "word",
           rgx_pattern,
           rgx_replacement,
           stopwords = TRUE,
           stopwords_lexicon,
           rgx_ignore_custom) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    colname_text_quo <- rlang::sym(colname_text)
    colname_words_quo <- rlang::sym(colname_word)
    colname_word1_quo <- rlang::sym("first")
    colname_word2_quo <- rlang::sym("second")

    word <- NULL

    out <- data
    if (!missing(rgx_pattern) & !missing(rgx_replacement)) {
      out <-
        out %>%
        dplyr::mutate(text = stringr::str_replace_all(!!colname_text_quo, rgx_pattern, rgx_replacement))
    }
    out <-
      out %>%
      tidytext::unnest_tokens(!!colname_words_quo,
                              !!colname_text_quo,
                              token = "ngrams",
                              n = 2)
    out <-
      out %>%
      tidyr::separate(
        !!colname_words_quo,
        into = c("first", "second"),
        sep = " ",
        remove = FALSE
      )
    if (stopwords) {
      if (missing(stopwords_lexicon)) {
        stop_words <- tidytext::stop_words
      } else {
        stop_words <- tidytext::get_stopwords(source = stopwords_lexicon)
      }
      out <-
        out %>%
        dplyr::rename(word = !!colname_word1_quo) %>%
        dplyr::anti_join(stop_words, by = "word") %>%
        dplyr::rename(!!colname_word1_quo := word)

      out <-
        out %>%
        dplyr::rename(word = !!colname_word2_quo) %>%
        dplyr::anti_join(stop_words, by = "word") %>%
        dplyr::rename(!!colname_word2_quo := word)
    }
    if (!missing(rgx_ignore_custom)) {
      out <-
        out %>%
        dplyr::filter(!stringr::str_detect(!!colname_word1_quo, rgx_ignore_custom)) %>%
        dplyr::filter(!stringr::str_detect(!!colname_word2_quo, rgx_ignore_custom))
    }

    out <-
      out %>%
      dplyr::filter(stringr::str_detect(!!colname_word1_quo, "[a-z]")) %>%
      dplyr::filter(stringr::str_detect(!!colname_word2_quo, "[a-z]"))
    out
  }
