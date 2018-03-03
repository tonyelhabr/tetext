
#' Compute log ratios
#'
#' @description Compute log ratios for usage of n-grams between two entities.
#' @details Note that an attempt was made to make this function flexible enough,
#' to consider a \code{num_top} or \code{top_pct} parameter that might be used if
#' \code{cnt_min = NULL}, but this can be troublesome because \code{do.call()}
#' does not seem to handle \code{NULL} in \code{...}.
#' @inheritParams compute_freqs_multi_at
#' @param cnt_min numeric. If not null, serves as a filter for which to compute log ratios.
#' @return data.frame.
#' @rdname compute_logratios_multi_wide
#' @export
#' @seealso \url{https://www.tidytextmining.com/twitter.html#comparing-word-usage}.
#' \url{http://varianceexplained.org/r/trump-tweets/}.
compute_logratios_multi_wide_at <-
  function(data = NULL,
           word = "word",
           multi = NULL,
           cnt_min = 10) {
    if (is.null(data))
      stop("`data` must not be NULL.", call. = FALSE)
    if (is.null(multi))
      stop("`multi` must not be NULL.", call. = FALSE)

    word_quo <- rlang::sym(word)
    multi_quo <- rlang::sym(multi)

    n <- logratio <- x <- y <- NULL

    data_proc <-
      data %>%
      dplyr::count(!!multi_quo, !!word_quo)

    # NOTE: Have not really tested `top_pct` and `num_top` before.
    if (!is.null(cnt_min)) {
      data_proc <-
        data_proc %>%
        dplyr::filter(n >= cnt_min)

      if(nrow(data_proc) == 0)
        return(stop("No data after filtering for `cnt_min`.", call. = FALSE))
    }

    data_proc <-
      data_proc %>%
      tidyr::spread(!!multi_quo, n, fill = 0)

      if (ncol(data_proc) == 2) {
        data_proc <- append_dummy_cols(data_proc, num_cols_expect = 3)
      }

      nms <- names(data_proc)
      out <-
        data_proc %>%
        stats::setNames(c("word", "x", "y")) %>%
        dplyr::mutate_if(is.numeric, dplyr::funs((. + 1) / sum(. + 1))) %>%
        dplyr::mutate(logratio = log(x / y))

      x_i <- nms[2]
      y_i <- nms[3]
      out %>%
        stats::setNames(c("word", x_i, y_i, "logratio")) %>%
        dplyr::arrange(dplyr::desc(logratio))
      out
    }

#' @rdname compute_logratios_multi_wide
#' @export
compute_logratios_multi_wide <- compute_logratios_multi_wide_at

#' Compute n-gram log ratios in pairs
#'
#' @description Apply \code{compute_logratios_multi_wide_at()} to
#' \code{data} in pairs of \code{multi} values.
#' @details Calls \code{wrapper_func} internally.
#' @inheritParams wrapper_func
#' @inheritParams compute_logratios_multi_wide_at
#' @param ... dots. Adddition parameters to pass to \code{compute_logratios_multi_wide_at()}.
#' @return data.frame.
#' @rdname compute_logratios_multi_by2
#' @export
compute_logratios_multi_by2_at <-
  function(data = NULL,
           func = compute_logratios_multi_wide_at,
           xy_grid = NULL,
           xy_nms = NULL,
           # word = "word",
           multi = NULL,
           # cnt_min = 10,
           ...) {

    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    # if (is.null(word))
    #   stop("`word` cannot be NULL.", call. = FALSE)
    if (is.null(multi))
      stop("`multi` cannot be NULL.", call. = FALSE)

    if(is.null(xy_grid) | is.null(xy_nms)) {
      multis <-
        data %>%
        pull_distinctly_at(multi)
      xy_grid <- create_xy_grid(multis)
      xy <- NULL
      xy_nms <- xy_grid %>% dplyr::pull(xy)
      message("Generating output for all combinations of `multi`.")
    }

    wrapper_func(
      data = data,
      func = func,
      xy_grid = xy_grid,
      xy_nms = xy_nms,
      # word = word,
      multi = multi,
      # cnt_min = cnt_min
      ...
    )
  }

#' @rdname compute_logratios_multi_by2
#' @export
compute_logratios_multi_by2 <- compute_logratios_multi_by2_at

#' Visualize n-gram log ratios in pairs
#'
#' @description Visualize n-gram log ratios across pairs of \code{multi} values.
#' @details \code{compute_logratios_multi_by2_at()} should NOT be called beforehand.
#' @inheritParams compute_freqs_multi_by2_at
#' @inheritParams visualize_freqs_multi_by2_at
#' @inheritParams compute_logratios_multi_by2_at
#' @param ... dots. Additional parameters to pass to \code{compute_logratios_multi_by2_at()}.
#' @param num_top numeric. Number of n-grams to show for each \code{multi} pair.
#' @param flip_axes logical. Whether or not to call \code{ggplot2::coord_flip()}.
#' @param lab_other character. Name to give to 'opposing' factor of \code{multi_main}.
#' @param color_value character (vector). Should be a vector of length two (for dual colors)
#' if \code{filter_multi = TRUE} and \code{!is.null(multi_main) & length(multi_main) == 1}.
#' @return gg.
#' @rdname visualize_logratios_multi_by2
#' @export
#' @seealso \url{https://www.tidytextmining.com/twitter.html#comparing-word-usage}.
#' \url{http://varianceexplained.org/r/trump-tweets/}.
visualize_logratios_multi_by2_at <-
  function(data = NULL,
           word = "word",
           multi = NULL,
           xy_nms,
           xy_grid,
           # cnt_min,
           ...,
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
           lab_other = "other",
           lab_title = "Most Unique Words",
           lab_subtitle = paste0("By ", stringr::str_to_title(multi)),
           lab_x = NULL,
           lab_y = "Log Odds Ratio",
           theme_base = theme_tetext_facet(),
           facet_scales = "free",
           facet_ncol = 3,
           facet_nrow = NULL,
           facet_strip_position = "right") {

    # NOTE: Checks for NULL are made in other function(s).

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
      compute_logratios_multi_by2_at(
        data = data,
        xy_grid = xy_grid,
        xy_nms = xy_nms,
        word = word,
        multi = multi,
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

    logratio_dir <- logratio <- name_xy <- name_x <- name_y <- NULL

    data_proc <-
      data_proc %>%
      dplyr::mutate(logratio_dir = dplyr::if_else(logratio < 0, TRUE, FALSE)) %>%
      dplyr::group_by(name_xy, logratio_dir) %>%
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
      # dplyr::mutate(!!word_quo := stats::reorder(!!word_quo, dplyr::desc(logratio)))

    viz <-
      data_proc %>%
      ggplot2::ggplot(ggplot2::aes_string(x = word, y = "logratio", fill = color)) +
      # ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_col() +
      scale_x_reordered() +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +
      ggplot2::scale_fill_manual(values = color_value) +
      ggplot2::facet_wrap(
        ~ name_xy,
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
        legend.title = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.major.x = ggplot2::element_blank()
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

#' @rdname visualize_logratios_multi_by2
#' @export
visualize_logratios_multi_by2 <- visualize_logratios_multi_by2_at

