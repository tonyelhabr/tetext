
get_lexicons_valid <- function() {
  c("bing", "afinn", "nrc", "loughran")
}

get_sents_valid <-
  function(lexicon = c("bing", "afinn", "nrc", "loughran")) {
    # lexicon <- match.arg(lexicon)
    # if (is.null(lexicon))
    #   stop("`lexicon` cannot be NULL.", call. = FALSE)
    sentiment <- NULL
    sents <- get_sents(lexicon = lexicon)
    sents %>% pull_distinctly_at("sentiment")
  }

#' Get sentiment dictionary
#'
#' @description Get words associated with a lexicon for subsequent sentiment analysis.
#' @details Called by \code{compute_sent_facet_summary()}.
#' This function is essentially a customized version of  \code{tidytext::get_sentiments()}.
#' @param lexicon character. Specifies lexicon to use. Must be a lexicon available in the \code{tidytext::package}.
#' @param normalize logical. Only releveant if \code{lexicon = 'afinn'}. Renormalizes
#' 'afinn' scores to -1 to 1 range.
#' @return data.frame
#' @rdname get_sents
#' @export
#' @seealso \url{https://github.com/juliasilge/tidytext/blob/master/R/sentiments.R}.
get_sents <- function(lexicon = c("bing", "afinn", "nrc", "loughran"),
                      normalize = TRUE) {
  lexicon <- match.arg(lexicon)
  if (is.null(lexicon))
    stop("`lexicon` cannot be NULL.", call. = FALSE)

  word <- score <- sentiment <- NULL

  if (lexicon %in% c("afinn")) {
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


#' @rdname compute_sent_summ
#' @export
compute_sent_summ_facet_at <-
  function(data = NULL,
           token = NULL,
           feature = NULL,
           facet = NULL,
           lexicon = "bing",
           ...) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(feature), is.character(feature))
    stopifnot(!is.null(facet), is.character(facet))

    token_quo <- rlang::sym(token)
    facet_quo <- rlang::sym(facet)
    feature_quo <- rlang::sym(feature)

    total_words <- sentiment <- n <- cnt <- sentiment_pct <- NULL

    data_facet_cnt <-
      data %>%
      dplyr::group_by(!!facet_quo) %>%
      dplyr::mutate(total_words = n()) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(!!facet_quo, !!feature_quo, total_words)

    sents <- get_sents(lexicon = lexicon, ...)

    data %>%
      dplyr::rename(word = !!token_quo) %>%
      dplyr::inner_join(sents, by = "word") %>%
      dplyr::count(!!feature_quo, sentiment) %>%
      tidyr::complete(sentiment, !!feature_quo, fill = list(n = 0)) %>%
      dplyr::inner_join(data_facet_cnt, by = feature) %>%
      dplyr::group_by(!!facet_quo, sentiment, total_words) %>%
      dplyr::summarize(cnt = sum(n)) %>%
      # dplyr::mutate(sentiment_pct = words / total_words) %>%
      dplyr::ungroup()
  }

#' @rdname compute_sent_summ
#' @export
compute_sent_summ_facet <-
  function(..., token, feature, facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(feature))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    feature <- rlang::quo_text(rlang::enquo(feature))
    facet <- rlang::quo_text(rlang::enquo(facet))
    compute_sent_summ_facet_at(..., token = token, feature = feature, facet = facet)
  }

#' Compute sentiment scores.
#'
#' @description Compute sentiment scores given tokens.
#' @details Heavily influenced by \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' Creates output columns 'sentiment' and 'score'.
#' @inheritParams get_sents
#' @param data data.frame. Must already be in 'tidy' format.
#' @param token bare for NSE; character for SE. Name of column in \code{data} whose values are scored.
#' @param facet bare for NSE; character for SE. Name of column in \code{data} used for grouping in the case
#' that there are more than one entitiy.
#' @param feature bare for NSE; character for SE. Name of column in \code{data} whose values serve as
#' entity across which 'token' values are scored. Probably something like 'sentence',
#' or, in the case of Twitter data, 'status_id'.
#' @param ... dots. Parameters to pass to \code{get_sents()} internally.
#' @return data.frame.
#' @rdname compute_sent_summ
#' @export
#' @seealso Don't have an explicit url.
compute_sent_summ_at <-
  function(data = NULL,
           # token = NULL,
           # feature = NULL,
           facet = NULL,
           ...) {
    stopifnot(!is.null(data), is.data.frame(data))
    # stopifnot(!is.null(token), is.character(token))
    # stopifnot(!is.null(feature), is.character(feature))

    if (is.null(facet)) {
      add_dummy <- TRUE
      data <- data %>% dplyr::mutate(`.facet` = "dummy")
      facet <- ".facet"
    } else {
      add_dummy <- FALSE
    }

    out <-
      compute_sent_summ_facet_at(
        data = data,
        facet = facet,
        ...
      )
    if (add_dummy) {
      out <- out %>% dplyr::select(-dplyr::one_of(c(facet)))
    }
    out
  }

#' @rdname compute_sent_summ
#' @export
compute_sent_summ <-
  function(..., token, feature, facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(feature))
    token <- rlang::quo_text(rlang::enquo(token))
    feature <- rlang::quo_text(rlang::enquo(feature))
    if(missing(facet)) {
      facet <- NULL
    } else {
      facet <- rlang::quo_text(rlang::enquo(facet))
    }
    compute_sent_summ_at(..., token = token, feature = feature, facet = facet)
  }
