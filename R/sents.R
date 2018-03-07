
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


#' @rdname comput_sent_summ
#' @export
comput_sent_summ_facet_at <-
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

#' @rdname comput_sent_summ
#' @export
comput_sent_summ_facet <-
  function(..., token, feature, facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(feature))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    feature <- rlang::quo_text(rlang::enquo(feature))
    facet <- rlang::quo_text(rlang::enquo(facet))
    comput_sent_summ_facet_at(..., token = token, feature = feature, facet = facet)
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
#' @rdname comput_sent_summ
#' @export
#' @seealso Don't have an explicit url.
comput_sent_summ_at <-
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
      comput_sent_summ_facet_at(
        data = data,
        facet = facet,
        ...
      )
    if (add_dummy) {
      out <- out %>% dplyr::select(-dplyr::one_of(c(facet)))
    }
    out
  }

#' @rdname comput_sent_summ
#' @export
comput_sent_summ <-
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
    comput_sent_summ_at(..., token = token, feature = feature, facet = facet)
  }

#' Compute token sentiment log ratios in pairs
#'
#' @description Apply \code{compute_logratios_facet_wide_at()} to
#' \code{data} in pairs of \code{facet} values, then do post-processing for sentiments.
#' @details Calls \code{wrapper_func} internally. This function is very
#' similar to \code{compute_sentratios_facet_by2_at()}, but with some post-processing.
#' Note that \code{...} are passed to \code{get_sents()} here (instead of to
#' \code{compute_logratios_facet_wide_at()}, so \code{token} and \code{cnt_min} must
#' be checked for NULL explicilty.
#' @inheritParams wrapper_func
#' @inheritParams compute_logratios_facet_wide_at
#' @inheritParams compute_logratios_facet_by2_at
#' @inheritParams get_sents
#' @param ... dots. Adddition parameters to pass to \code{get_sents()}.
#' @return data.frame.
#' @rdname compute_sentratios_facet_by2
#' @export
compute_sentratios_facet_by2_at <-
  function(data = NULL,
           func = compute_logratios_facet_wide_at,
           token = NULL,
           facet = NULL,
           xy_grid,
           xy_nms,
           cnt_min = 10,
           lexicon = "bing",
           ...) {

    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(facet), is.character(facet))

    if(missing(xy_grid) | missing(xy_nms)) {
      facets <-
        data %>%
        pull_distinctly_at(facet)
      xy_grid <- create_xy_grid(facets)
      xy <- NULL
      xy_nms <- xy_grid %>% dplyr::pull(xy)
      message("Generating output for all combinations of `facet`.")
    }

    data_proc <-
      wrapper_func(
        data = data,
        func = func,
        xy_grid = xy_grid,
        xy_nms = xy_nms,
        token = token,
        facet = facet,
        cnt_min = cnt_min
      )

    sents <- get_sents(lexicon = lexicon, ...)

    token_quo <- rlang::sym(token)

    name_xy <- sentiment <- logratio <- word <- NULL

    data_proc %>%
      dplyr::rename(word = !!token_quo) %>%
      dplyr::inner_join(sents, by = "word") %>%
      dplyr::group_by(name_xy, word, sentiment) %>%
      dplyr::mutate(logratio = mean(logratio)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(!!token_quo := word) %>%
      dplyr::arrange(name_xy, sentiment, dplyr::desc(abs(logratio)))

  }

#' @rdname compute_sentratios_facet_by2
#' @export
compute_sentratios_facet_by2 <-
  function(..., token, facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    facet <- rlang::quo_text(rlang::enquo(facet))

    compute_sentratios_facet_by2_at(
      ...,
      token = token,
      facet = facet
    )
  }

#' Visualize token sentiment log ratios in pairs
#'
#' @description Visualize token sentiment log ratios across pairs of \code{facet} values.
#' @details \code{compute_sentratios_facet_by2_at()} should NOT be called beforehand.
#' @inheritParams compute_logratios_facet_by2_at
#' @inheritParams visualize_logratios_facet_by2
#' @inheritParams compute_sentratios_facet_by2_at
#' @param ... dots. Additional parameters to pass to \code{compute_sentratios_facet_by2_at()}.
#' @param filter_sent logical. Whether or not to filter the sentiment values.
#' @param sent_main bare for NSE; character for SE. Name of single sentiment value to use as basis.
#' Not used if \code{filter_sent = FALSE}.
#' @return gg.
#' @rdname visualize_sentratios_facet_by2
#' @export
#' @seealso \url{https://www.tidytextmining.com/twitter.html#comparing-token-usage}.
#' \url{http://varianceexplained.org/r/trump-tweets/}.
#' \url{https://www.tidytextmining.com/sentiment.html#most-positive-negative}.
visualize_sentratios_facet_by2_at <-
  function(data = NULL,
           token = NULL,
           facet = NULL,
           xy_nms,
           xy_grid,
           cnt_min = 10,
           lexicon = "bing",
           ...,
           filter_facet = TRUE,
           facet_main = NULL,
           filter_facet_base = default_filter_facet(facet_main),
           filter_facet_params = list(),
           filter_sent = TRUE,
           sent_main = NULL,
           color = NULL,
           lab_other = "other",
           num_top = 5,
           scale_manual_params = default_scale_manual(values = c("grey50", "grey80")),
           labs_base = default_labs(),
           labs_params = list(title = "Most Significant Words Contributing to Sentiment Differences",
                              y = "Log Odds Ratio"),
           theme_base = default_theme_facet(),
           theme_params =
             list(legend.position = "bottom",
                  axis.text.y = ggplot2::element_text(angle = 45, hjust = 1),
                  panel.grid.major.y = ggplot2::element_blank()),
           facet_base =
             default_facet(ifelse(is.null(sent_main),
                                  "sentiment ~ name_xy",
                                  "name_xy")),
           facet_params = list()) {


    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(facet), is.character(facet))
    stopifnot(!is.null(lexicon), is.character(lexicon))

    if(missing(xy_grid) | missing(xy_nms)) {
      facets <-
        data %>%
        pull_distinctly_at(facet)
      xy_grid <- create_xy_grid(facets)
      xy <- NULL
      xy_nms <- xy_grid %>% dplyr::pull(xy)
      message("Generating output for all combinations of `facet`.")
    }

    data_proc <-
      compute_sentratios_facet_by2_at(
        data = data,
        xy_grid = xy_grid,
        xy_nms = xy_nms,
        token = token,
        facet = facet,
        cnt_min = cnt_min,
        lexicon = lexicon,
        ...
      )

    data_proc <-
      data_proc %>%
      validate_x_main(
        filter_x = filter_facet,
        x = facet,
        xs = data %>% pull_distinctly_at(facet),
        x_main = facet_main
      )

    if(!("name_xy" %in% facet)) {
      message("Correcting facetting variable.")
    }
    data_proc <-
      data_proc %>%
      filter_data_facet_at(
        filter_facet = filter_facet,
        params = combine_base_and_params(filter_facet_base, filter_facet_params)
      )

    # NOTE: This sentiment processing is not in the logratio function.
    data_proc <-
      data_proc %>%
      validate_x_main(
        filter_x = filter_sent,
        x = "sentiment",
        xs = get_sents_valid(lexicon = lexicon),
        x_main = sent_main
      )

    if(filter_sent) {
      data_proc <-
        data_proc %>%
        dplyr::filter(sentiment %in% sent_main)

      if(nrow(data_proc) == 0) {
        return(stop("No rows after filtering. Perhaps `sent_main` is not valid.", call. = FALSE))
      }
    }

    logratio_dir <- logratio <- name_xy <- name_x <- name_y <- sentiment <- NULL

    # NOTE: Unlike the logratio function, must also group by sentiment.
    # NOTE: Not sure if should use arrange with abs().
    data_proc <-
      data_proc %>%
      dplyr::mutate(logratio_dir = dplyr::if_else(logratio < 0, TRUE, FALSE)) %>%
      # dplyr::group_by(name_xy, logratio_dir) %>%
      dplyr::group_by(name_xy, sentiment, logratio_dir) %>%
      dplyr::arrange(name_xy, dplyr::desc(abs(logratio))) %>%
      filter_num_top_at("logratio", num_top) %>%
      dplyr::ungroup()

    data_proc <-
      data_proc %>%
      dplyr::mutate(name_xy = paste0(name_x, " vs. ", name_y))

    if (is.null(color)) {
      data_proc <- data_proc %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
    # NOTE: Don't need this.
    # data_proc <- wrangle_color_col(data_proc, color)

    token_quo <- rlang::sym(token)
    color_quo <- rlang::sym(color)
    facet_quo <- rlang::sym(facet)
    facet_other <- lab_other

    data_proc <-
      data_proc %>%
      dplyr::mutate(!!color_quo := dplyr::if_else(logratio_dir, name_x, facet_other)) %>%
      dplyr::mutate(!!color_quo := factor(!!color_quo, levels = c(facet_main, facet_other))) %>%
      dplyr::mutate(!!token_quo := reorder_within(!!token_quo, dplyr::desc(logratio), name_xy))
      # dplyr::mutate(!!token_quo := stats::reorder(!!token_quo, dplyr::desc(logratio))

    viz <-
      data_proc %>%
      ggplot2::ggplot(ggplot2::aes_string(x = token, y = "logratio", fill = color)) +
      # ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_col() +
      scale_x_reordered() +
      do_call_scale_manual(scale_manual_params, type = "fill") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0))

    viz <-
      viz  +
      generate_facets(facet_base, facet_params)

    viz <-
      viz +
      labs_base + do_call_labs(labs_params) +
      theme_base + do_call_theme(theme_params)

    viz <-
      viz +
      ggplot2::coord_flip()
  }

#' @rdname visualize_sentratios_facet_by2
#' @export
visualize_sentratios_facet_by2 <-
  function(..., token, color, facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    facet <- rlang::quo_text(rlang::enquo(facet))
    if (missing(color)) {
      color <- NULL
    } else {
      color <- rlang::quo_text(rlang::enquo(color))
    }
    visualize_sentratios_facet_by2_at(..., token = token, color = color, facet = facet)
  }
