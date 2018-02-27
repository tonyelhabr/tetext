


#' Get sentiment dictionary
#'
#' @description Get words associated with a lexicon for subsequent sentiment analysis.
#' @details Called by \code{compute_sent_multi_summary()}.
#' This function is essentially a customized version of  \code{tidytext::get_sentiments()}.
#' @param lexicon character. Specifies lexicon to use. Must be a lexicon available in the \code{tidytext::package}.
#' @param normalize logical. Only releveant if \code{lexicon = 'afinn'}. Renormalizes
#' 'afinn' scores to -1 to 1 range.
#' @return data.frame
#' @rdname get_sents
#' @export
get_sents <- function(lexicon = NULL,
                      normalize = TRUE) {
  if (is.null(lexicon))
    stop("`lexicon` cannot be NULL.", call. = FALSE)

  word <- score <- sentiment <- NULL

  if (lexicon %in% c("afinn", "AFINN")) {
    out  <-
      tidytext::get_sentiments(lexicon = lexicon) %>%
      dplyr::select(word, sentiment = score)

    if (normalize) {
      out <- out %>% dplyr::mutate(sentiment = sentiment / 5)
    }
  } else {
    out <-
      tidytext::get_sentiments(lexicon = lexicon) %>%
      dplyr::select(word, sentiment)
  }
  out
}


#' @rdname compute_sent_summary
#' @export
compute_sent_summary_multi_at <-
  function(data = NULL,
           word = "word",
           multi = NULL,
           feature = NULL,
           lexicon = "bing",
           ...) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    if (is.null(multi))
      stop("`multi` cannot be NULL.", call. = FALSE)
    if (is.null(feature))
      stop("`feature` cannot be NULL.", call. = FALSE)

    word_quo <- rlang::sym(word)
    multi_quo <- rlang::sym(multi)
    feature_quo <- rlang::sym(feature)

    total_words <- sentiment <- n <- NULL

    data_multi_cnt <-
      data %>%
      dplyr::group_by(!!multi) %>%
      dplyr::mutate(total_words = n()) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(!!feature, !!multi, total_words)

    sents <- get_sents(...)

    out <-
      data_multi_cnt %>%
      dplyr::inner_join(sents, by = "word") %>%
      dplyr::count(!!feature, sentiment) %>%
      tidyr::complete(sentiment, !!feature, fill = list(n = 0)) %>%
      dplyr::inner_join(data, by = c(feature)) %>%
      dplyr::group_by(!!multi, sentiment, total_words) %>%
      dplyr::summarize(words = sum(n)) %>%
      dplyr::ungroup()
    out
  }

#' @rdname compute_sent_summary
#' @export
compute_sent_summary_multi <- compute_sent_summary_multi_at

#' Compute sentiment scores.
#'
#' @description Compute sentiment scores given n-grams.
#' @details Heavily influenced by \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' Creates output columns 'sentiment' and 'score'.
#' @param data data.frame. Must already be in 'tidy' format.
#' @param word character. Name of column in \code{data} whose values are scored.
#' @param multi character. Name of column in \code{data} used for grouping in the case
#' that there are more than one entitiy.
#' @param feature character. Name of column in \code{data} whose values serve as
#' entity across which 'word' values are scored. Probably something like 'sentence',
#' or, in the case of Twitter data, 'status_id'.
#' @param lexicon character. Name of lexicon available in \code{tidytext} package to use.
#' @param ... dots. Parameters to pass to \code{get_sents()} internally.
#' @return data.frame.
#' @rdname compute_sent_summary
#' @export
#' @seealso Don't have an explicit url.
compute_sent_summary_at <-
  function(data = NULL,
           word,
           multi,
           feature,
           ...) {
    if (missing(multi)) {
      add_dummy <- TRUE
      data <- data %>% dplyr::mutate(`.multi` = "dummy")
      multi <- ".multi"
    } else {
      add_dummy <- FALSE
    }

    # compute_sent_summary_multi_at <- NULL
    out <-
      compute_sent_summary_multi_at(
        data = data,
        multi = multi,
        ...
      )
    if (add_dummy) {
      out <- out %>% dplyr::select(-dplyr::one_of(c(multi)))
    }
    out
  }

#' @rdname compute_sent_summary
#' @export
compute_sent_summary <- compute_sent_summary_at
