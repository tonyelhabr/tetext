


#' Prepare data for text analysis.
#'
#' @description Create a tidy data.frame of unigrams.
#' @details Heavily influence by \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' @inheritParams visualize_time_at
#' @inheritParams visualize_cnts_at
#' @param data data.frame. Not in 'tidy' format.
#' @param text character. Name of column in \code{data} to parse. Default: 'text'
#' @param word character. Name of output column for token. Default: 'word'
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
#' @rdname tidify_to_unigrams_at
#' @export
#' @importFrom dplyr mutate rename anti_join filter
#' @importFrom stringr str_replace_all str_detect
#' @importFrom tidytext unnest_tokens get_stopwords
#' @seealso \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' \url{https://www.tidytextmining.com/ngrams.html}.
#' \url{https://www.tidytextmining.com/twitter.html}.
tidify_to_unigrams_at <-
  function(data = NULL,
           text = "text",
           word = "word",
           rgx_pattern,
           rgx_replacement,
           rgx_unnest,
           stopwords = TRUE,
           stopwords_lexicon,
           rgx_ignore_custom) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)

    text_quo <- rlang::sym(text)
    word_quo <- rlang::sym(word)

    word <- NULL

    out <- data
    if (!missing(rgx_pattern) & !missing(rgx_replacement)) {
      out <-
        out %>%
        dplyr::mutate(text = stringr::str_replace_all(!!text_quo, rgx_pattern, rgx_replacement))
    }

    if (missing(rgx_unnest)) {
      out <-
        out %>%
        tidytext::unnest_tokens(!!word_quo, !!text_quo)
    } else {
      out <-
        out %>%
        tidytext::unnest_tokens(!!word_quo, !!text_quo, rgx_unnest)
    }

    if (stopwords) {
      if (missing(stopwords_lexicon)) {
        stop_words <- tidytext::stop_words
      } else {
        stop_words <- tidytext::get_stopwords(source = stopwords_lexicon)
      }

      # NOTE: Not sure why, but tidyeval is not wokring with `dplyr::anti_join()`,
      # so using a work-around.
      # out <- dplyr::anti_join(out, stop_words, by = c(word = "word"))
      out <-
        out %>%
        dplyr::rename(word = !!word_quo) %>%
        dplyr::anti_join(stop_words, by = "word") %>%
        dplyr::rename(!!word_quo := word)
    }

    if (!missing(rgx_ignore_custom)) {
      out <-
        out %>%
        dplyr::filter(!stringr::str_detect(!!word_quo, rgx_ignore_custom))
    }

    out <-
      out %>%
      dplyr::filter(stringr::str_detect(!!word_quo, "[a-z]"))
    out
  }



#' Prepare data for text analysis.
#'
#' @description Create a tidy data.frame of unigrams.
#' @details Heavily influence by \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' @inheritParams visualize_time_at
#' @inheritParams visualize_cnts_at
#' @inheritParams tidify_to_unigrams_at
#' @return data.frame. In tidy format. 'word1' and 'word2' columns are named
#' 'first' and 'second' respectively.
#' @rdname tidify_to_bigrams_at
#' @export
#' @importFrom dplyr mutate rename anti_join filter
#' @importFrom stringr str_replace_all str_detect
#' @importFrom tidytext unnest_tokens get_stopwords
#' @importFrom tidyr separate
tidify_to_bigrams_at <-
  function(data = NULL,
           text = "text",
           word = "word",
           rgx_pattern,
           rgx_replacement,
           stopwords = TRUE,
           stopwords_lexicon,
           rgx_ignore_custom) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    text_quo <- rlang::sym(text)
    words_quo <- rlang::sym(word)
    word1_quo <- rlang::sym("first")
    word2_quo <- rlang::sym("second")

    word <- NULL

    out <- data
    if (!missing(rgx_pattern) & !missing(rgx_replacement)) {
      out <-
        out %>%
        dplyr::mutate(text = stringr::str_replace_all(!!text_quo, rgx_pattern, rgx_replacement))
    }
    out <-
      out %>%
      tidytext::unnest_tokens(!!words_quo,
                              !!text_quo,
                              token = "ngrams",
                              n = 2)
    out <-
      out %>%
      tidyr::separate(
        !!words_quo,
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
        dplyr::rename(word = !!word1_quo) %>%
        dplyr::anti_join(stop_words, by = "word") %>%
        dplyr::rename(!!word1_quo := word)

      out <-
        out %>%
        dplyr::rename(word = !!word2_quo) %>%
        dplyr::anti_join(stop_words, by = "word") %>%
        dplyr::rename(!!word2_quo := word)
    }
    if (!missing(rgx_ignore_custom)) {
      out <-
        out %>%
        dplyr::filter(!stringr::str_detect(!!word1_quo, rgx_ignore_custom)) %>%
        dplyr::filter(!stringr::str_detect(!!word2_quo, rgx_ignore_custom))
    }

    out <-
      out %>%
      dplyr::filter(stringr::str_detect(!!word1_quo, "[a-z]")) %>%
      dplyr::filter(stringr::str_detect(!!word2_quo, "[a-z]"))
    out
  }
