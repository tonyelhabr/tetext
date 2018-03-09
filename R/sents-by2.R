
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
#' @inheritParams compute_logratios_facet_wide
#' @inheritParams compute_logratios_facet_by2
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
      message(sprintf("Generating output for all combinations of `%s`.", facet))
    }

    data_proc <-
      wrapper_func(
        data = data,
        func = func,
        xy_grid = xy_grid,
        xy_nms = xy_nms,
        col = facet,
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
#' @inheritParams compute_logratios_facet_by2
#' @inheritParams visualize_logratios_facet_by2
#' @inheritParams compute_sentratios_facet_by2
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
           scale_manual_base =
             default_scale_manual(values = c("grey50", get_color_hex_inverse("grey80"))),
           scale_manual_params = list(),
           labs_base = default_labs(),
           labs_params = list(title = "Most Significant Words Contributing to Sentiment Differences",
                              y = "Log Odds Ratio"),
           theme_base = default_theme(),
           theme_params =
             list(legend.position = "bottom",
                  axis.text.y = ggplot2::element_text(angle = 30, hjust = 1),
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
      message(sprintf("Generating output for all combinations of `%s`.", facet))
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
      message("Changing facetting variable to `name_xy`.")
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

    data_proc <-
      data_proc %>%
      process_logratio_by2(
        cols_group = c("name_xy", "sentiment", "logratio_dir"),
        num_top = num_top,
        token = token,
        color = color,
        facet = facet,
        facet_main = facet_main,
        lab_other = lab_other
      )
    # logratio_dir <- logratio <- name_xy <- name_x <- name_y <- sentiment <- NULL
    #
    # # NOTE: Unlike the logratio function, must also group by sentiment.
    # # NOTE: Not sure if should use arrange with abs().
    # data_proc <-
    #   data_proc %>%
    #   dplyr::mutate(
    #     logratio_dir =
    #       ifelse(logratio > 0,
    #              ifelse(name_x < name_y, TRUE, FALSE),
    #              ifelse(name_x < name_y, FALSE, TRUE)
    #       )
    #   ) %>%
    #   # dplyr::group_by(name_xy, logratio_dir) %>%
    #   dplyr::group_by(name_xy, sentiment, logratio_dir) %>%
    #   filter_num_top_at("logratio", num_top, abs = TRUE) %>%
    #   dplyr::ungroup()
    #
    # data_proc <-
    #   data_proc %>%
    #   dplyr::mutate(name_xy = paste0(name_x, " vs. ", name_y))
    #
    # if (is.null(color)) {
    #   data_proc <- data_proc %>% dplyr::mutate(`.dummy` = "dummy")
    #   color <- ".dummy"
    # }
    # # NOTE: Don't need this.
    # # data_proc <- wrangle_color_col(data_proc, color)
    #
    # token_quo <- rlang::sym(token)
    # color_quo <- rlang::sym(color)
    # facet_quo <- rlang::sym(facet)
    # facet_other <- lab_other
    #
    # data_proc <-
    #   data_proc %>%
    #   dplyr::mutate(
    #     !!color_quo :=
    #       ifelse(logratio_dir,
    #              ifelse(name_x > facet_other, facet_other, name_x),
    #              ifelse(name_x > facet_other, name_x, facet_other)
    #       )
    #   ) %>%
    #   dplyr::mutate(!!color_quo := factor(!!color_quo, levels = c(facet_main, facet_other))) %>%
    #   dplyr::mutate(!!token_quo := reorder_within(!!token_quo, dplyr::desc(logratio), name_xy))

    color <- ".dummy"
    viz <-
      data_proc %>%
      ggplot2::ggplot(ggplot2::aes_string(x = token, y = "logratio", fill = color)) +
      # ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_col() +
      scale_x_reordered() +
      # generate_scale_manual(scale_manual_base, scale_manual_params, type = "fill") +
      generate_scale_manual(scale_manual_base, scale_manual_params, type = "fill") +
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
