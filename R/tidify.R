


#' Prepare data for text analysis.
#'
#' @description Create a tidy data.frame of unigrams.
#' @details Heavily influenced by \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' Creates output column 'word'.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param data data.frame. Not in 'tidy' format.
#' @param text character. Name of column in \code{data} to parse. Default is provided.
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
#' @seealso \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' \url{https://www.tidytextmining.com/ngrams.html}.
#' \url{https://www.tidytextmining.com/twitter.html}.
tidify_to_unigrams_at <-
  function(data = NULL,
           text = "text",
           rgx_pattern,
           rgx_replacement,
           rgx_unnest,
           stopwords = TRUE,
           stopwords_lexicon,
           rgx_ignore_custom) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)

    text_quo <- rlang::sym(text)
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
        tidytext::unnest_tokens(word, !!text_quo)
    } else {
      out <-
        out %>%
        tidytext::unnest_tokens(word, !!text_quo, token = "regex", pattern = rgx_unnest)
    }

    if (stopwords) {
      if (missing(stopwords_lexicon)) {
        stop_words <- tidytext::stop_words
      } else {
        stop_words <- tidytext::get_stopwords(source = stopwords_lexicon)
      }
      # NOTE: Not sure why, but tidyeval does not seem to work with `dplyr::anti_join()`.
      out <-
        out %>%
        dplyr::anti_join(stop_words, by = "word")
    }

    if (!missing(rgx_ignore_custom)) {
      out <-
        out %>%
        dplyr::filter(!stringr::str_detect(word, rgx_ignore_custom))
    }

    out <-
      out %>%
      dplyr::filter(stringr::str_detect(word, "[a-z]"))
    out
  }

#' @rdname tidify_to_unigrams
#' @export
tidify_to_unigrams <- tidify_to_unigrams_at

#' Prepare data for text analysis.
#'
#' @description Create a tidy data.frame of unigrams.
#' @details Heavily influenced by \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' Creates output columns 'word', 'word1', and 'word2'.
#' ('word' is simply 'word1' and 'word2' toegether.)
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @inheritParams tidify_to_unigrams
#' @return data.frame.
#' @rdname tidify_to_bigrams
#' @export
tidify_to_bigrams_at <-
  function(data = NULL,
           text = "text",
           rgx_pattern,
           rgx_replacement,
           stopwords = TRUE,
           stopwords_lexicon,
           rgx_ignore_custom) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)

    text_quo <- rlang::sym(text)
    word <- word1 <- word2 <- NULL

    out <- data
    if (!missing(rgx_pattern) & !missing(rgx_replacement)) {
      out <-
        out %>%
        dplyr::mutate(text = stringr::str_replace_all(!!text_quo, rgx_pattern, rgx_replacement))
    }
    out <-
      out %>%
      tidytext::unnest_tokens(word,
                              !!text_quo,
                              token = "ngrams",
                              n = 2)
    out <-
      out %>%
      tidyr::separate(
        word,
        into = c("word1", "word2"),
        sep = " ",
        remove = FALSE
      )
    if (stopwords) {
      if (missing(stopwords_lexicon)) {
        stop_words <- tidytext::stop_words
      } else {
        stop_words <- tidytext::get_stopwords(source = stopwords_lexicon)
      }
      out <- out %>% dplyr::anti_join(stop_words %>% dplyr::rename(word1 = word), by = "word1")
      out <- out %>% dplyr::anti_join(stop_words %>% dplyr::rename(word2 = word), by = "word2")
    }
    if (!missing(rgx_ignore_custom)) {
      out <-
        out %>%
        dplyr::filter(!stringr::str_detect(word1, rgx_ignore_custom)) %>%
        dplyr::filter(!stringr::str_detect(word2, rgx_ignore_custom))
    }

    out <-
      out %>%
      dplyr::filter(stringr::str_detect(word1, "[a-z]")) %>%
      dplyr::filter(stringr::str_detect(word2, "[a-z]"))
    out
  }

#' @rdname tidify_to_bigrams
#' @export
tidify_to_bigrams <- tidify_to_bigrams_at

