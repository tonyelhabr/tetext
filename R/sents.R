
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
#' @details Called by \code{compute_sent_multi_summary()}.
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

    total_words <- sentiment <- n <- word <- words <- sentiment_pct <- NULL

    data_multi_cnt <-
      data %>%
      dplyr::group_by(!!multi_quo) %>%
      dplyr::mutate(total_words = n()) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(!!multi_quo, !!feature_quo, total_words)

    sents <- get_sents(lexicon = lexicon, ...)

    out <-
      data %>%
      dplyr::rename(word = !!word_quo) %>%
      dplyr::inner_join(sents, by = "word") %>%
      dplyr::count(!!feature_quo, sentiment) %>%
      tidyr::complete(sentiment, !!feature_quo, fill = list(n = 0)) %>%
      dplyr::inner_join(data_multi_cnt, by = c(feature)) %>%
      dplyr::group_by(!!multi_quo, sentiment, total_words) %>%
      dplyr::summarize(words = sum(n)) %>%
      # dplyr::mutate(sentiment_pct = words / total_words) %>%
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
#' @inheritParams get_sents
#' @param data data.frame. Must already be in 'tidy' format.
#' @param word character. Name of column in \code{data} whose values are scored.
#' @param multi character. Name of column in \code{data} used for grouping in the case
#' that there are more than one entitiy.
#' @param feature character. Name of column in \code{data} whose values serve as
#' entity across which 'word' values are scored. Probably something like 'sentence',
#' or, in the case of Twitter data, 'status_id'.
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

#' Compute n-gram sentiment log ratios in pairs
#'
#' @description Apply \code{compute_logratios_multi_wide_at()} to
#' \code{data} in pairs of \code{multi} values, then do post-processing for sentiments.
#' @details Calls \code{wrapper_func} internally. This function is very
#' similar to \code{compute_sentratios_multi_by2_at()}, but with some post-processing.
#' Note that \code{...} are passed to \code{get_sents()} here (instead of to
#' \code{compute_logratios_multi_wide_at()}, so \code{word} and \code{cnt_min} must
#' be checked for NULL explicilty.
#' @inheritParams wrapper_func
#' @inheritParams compute_logratios_multi_wide_at
#' @inheritParams compute_logratios_multi_by2_at
#' @inheritParams get_sents
#' @param ... dots. Adddition parameters to pass to \code{get_sents()}.
#' @return data.frame.
#' @rdname compute_sentratios_multi_by2
#' @export
compute_sentratios_multi_by2_at <-
  function(data = NULL,
           func = compute_logratios_multi_wide_at,
           word = "word",
           multi = NULL,
           xy_grid,
           xy_nms,
           cnt_min = 10,
           lexicon = "bing",
           ...) {

    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    if (is.null(word))
      stop("`word` cannot be NULL.", call. = FALSE)
    if (is.null(multi))
      stop("`multi` cannot be NULL.", call. = FALSE)

    if(missing(xy_grid) | missing(xy_nms)) {
      multis <-
        data %>%
        pull_distinctly_at(multi)
      xy_grid <- create_xy_grid(multis)
      xy <- NULL
      xy_nms <- xy_grid %>% dplyr::pull(xy)
      message("Generating output for all combinations of `multi`.")
    }

    data_proc <-
      wrapper_func(
        data = data,
        func = func,
        xy_grid = xy_grid,
        xy_nms = xy_nms,
        word = word,
        multi = multi,
        cnt_min = cnt_min
      )

    sents <- get_sents(lexicon = lexicon, ...)

    word_quo <- rlang::sym(word)

    name_xy <- sentiment <- logratio <- NULL

    out <-
      data_proc %>%
      dplyr::rename(word = !!word_quo) %>%
      dplyr::inner_join(sents, by = "word") %>%
      dplyr::group_by(name_xy, word, sentiment) %>%
      dplyr::mutate(logratio = mean(logratio)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(name_xy, sentiment, dplyr::desc(abs(logratio)))

    out
  }

#' @rdname compute_sentratios_multi_by2
#' @export
compute_sentratios_multi_by2 <- compute_sentratios_multi_by2_at

#' Visualize n-gram sentiment log ratios in pairs
#'
#' @description Visualize n-gram sentiment log ratios across pairs of \code{multi} values.
#' @details \code{compute_sentratios_multi_by2_at()} should NOT be called beforehand.
#' @inheritParams compute_logratios_multi_by2_at
#' @inheritParams visualize_logratios_multi_by2
#' @inheritParams compute_sentratios_multi_by2_at
#' @param ... dots. Additional parameters to pass to \code{compute_sentratios_multi_by2_at()}.
#' @param filter_sent logical. Whether or not to filter the sentiment values.
#' @param sent_main character. Name of single sentiment value to use as basis.
#' Not used if \code{filter_sent = FALSE}.
#' @return gg.
#' @rdname visualize_sentratios_multi_by2
#' @export
#' @seealso \url{https://www.tidytextmining.com/twitter.html#comparing-word-usage}.
#' \url{http://varianceexplained.org/r/trump-tweets/}.
#' \url{https://www.tidytextmining.com/sentiment.html#most-positive-negative}.
visualize_sentratios_multi_by2_at <-
  function(data = NULL,
           word = "word",
           multi = NULL,
           xy_nms,
           xy_grid,
           cnt_min = 10,
           lexicon = "bing",
           ...,
           filter_sent = TRUE,
           sent_main = NULL,
           filter_multi = TRUE,
           multi_main = NULL,
           x_include = NULL,
           y_include = NULL,
           x_exclude = NULL,
           y_exclude = NULL,
           color = NULL,
           color_value = c("grey50", "grey80"),
           num_top = 3,
           flip_axes = FALSE,
           lab_other = paste0("Other"),
           lab_title = "Most Significant Words Contributing to Sentiment Differences",
           lab_subtitle = ifelse(!is.null(sent_main),
                                 paste0(stringr::str_to_title(sent_main), " Sentiments"),
                                         paste0("By ", stringr::str_to_title(multi))),
           lab_x = NULL,
           lab_y = "Log Odds Ratio",
           theme_base = theme_tetext_facet(),
           facet_scales = "free",
           facet_ncol = 3,
           facet_nrow = NULL,
           facet_strip_position = "right") {

    if(missing(xy_grid) | missing(xy_nms)) {
      multis <-
        data %>%
        pull_distinctly_at(multi)
      xy_grid <- create_xy_grid(multis)
      xy <- NULL
      xy_nms <- xy_grid %>% dplyr::pull(xy)
      message("Generating output for all combinations of `multi`.")
    }

    data_proc <-
      compute_sentratios_multi_by2_at(
        data = data,
        xy_grid = xy_grid,
        xy_nms = xy_nms,
        word = word,
        multi = multi,
        cnt_min = cnt_min,
        lexicon = lexicon,
        ...
      )

    data_proc <-
      data_proc %>%
      validate_x_main(
        filter_x = filter_multi,
        x = multi,
        xs = data %>% pull_distinctly_at(multi),
        x_main = multi_main
      )

    data_proc <-
      data_proc %>%
      filter_data_multi_at(
        filter_multi = filter_multi,
        multi_main = multi_main,
        x_include = x_include,
        y_include = y_include,
        x_exclude = x_exclude,
        y_exclude = y_exclude
      )

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
    # NOTE: Not sure if should arrange with abs().
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

    data_proc <- wrangle_multi_col(data_proc, "name_xy")

    if (is.null(color)) {
      data_proc <- data_proc %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
    # data_proc <- wrangle_color_col(data_proc, color)

    word_quo <- rlang::sym(word)
    color_quo <- rlang::sym(color)
    multi_quo <- rlang::sym(multi)
    multi_other <- lab_other

    data_proc <-
      data_proc %>%
      dplyr::mutate(!!color_quo := dplyr::if_else(logratio_dir, name_x, multi_other)) %>%
      dplyr::mutate(!!color_quo := factor(!!color_quo, levels = c(multi_main, multi_other))) %>%
      dplyr::mutate(!!word_quo := reorder_within(!!word_quo, dplyr::desc(logratio), name_xy))
      # dplyr::mutate(!!word_quo := stats::reorder(!!word_quo, dplyr::desc(logratio))

    viz <-
      data_proc %>%
      ggplot2::ggplot(ggplot2::aes_string(x = word, y = "logratio", fill = color)) +
      # ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_col() +
      scale_x_reordered() +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +
      ggplot2::scale_fill_manual(values = color_value)

    if(filter_sent) {
      facet_fmla <- stats::as.formula(paste0(" ~ name_xy"))
    } else {
      facet_fmla <- stats::as.formula(paste0("sentiment ~ name_xy"))
    }
    viz <-
      viz +
      ggplot2::facet_wrap(
        facet_fmla,
        scales = facet_scales,
        ncol = facet_ncol,
        nrow = facet_nrow,
        strip.position = facet_strip_position
      )

    viz_labs <-
      ggplot2::labs(
        x = lab_x,
        y = lab_y,
        title = lab_title,
        subtitle = lab_subtitle
      )

    viz_theme <-
      theme_base +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = ggplot2::element_blank()
      )

    if(flip_axes) {
      viz_theme <-
        viz_theme +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid.major.y = ggplot2::element_blank()
        )
    } else {
      viz_theme <-
        viz_theme +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid.major.x = ggplot2::element_blank()
        )
    }

    viz <-
      viz +
      viz_labs +
      viz_theme

    if(flip_axes) {
      viz <-
        viz +
        ggplot2::coord_flip()
    }
    viz
  }

#' @rdname visualize_sentratios_multi_by2
#' @export
visualize_sentratios_multi_by2 <- visualize_sentratios_multi_by2_at
