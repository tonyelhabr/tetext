
#' Compute log ratios
#'
#' @description Compute log ratios for usage of tokens between two entities.
#' @details Note that an attempt was made to make this function flexible enough,
#' to consider a \code{num_top} or \code{top_pct} parameter that might be used if
#' \code{cnt_min = NULL}, but this can be troublesome because \code{do.call()}
#' does not seem to handle \code{NULL} in \code{...}.
#' @inheritParams compute_freqs_facet
#' @param cnt_min numeric. If not null, serves as a filter for which to compute log ratios.
#' @return data.frame.
#' @rdname compute_logratios_facet_wide
#' @export
#' @seealso \url{https://www.tidytextmining.com/twitter.html#comparing-token-usage}.
#' \url{http://varianceexplained.org/r/trump-tweets/}.
compute_logratios_facet_wide_at <-
  function(data = NULL,
           token = NULL,
           facet = NULL,
           cnt_min = 10) {

    # stopifnot(!is.null(data), is.data.frame(data))
    # stopifnot(!is.null(token), is.character(token))
    # stopifnot(!is.null(facet), is.character(facet))

    token_quo <- rlang::sym(token)
    facet_quo <- rlang::sym(facet)

    n <- logratio <- x <- y <- NULL

    data_proc <-
      data %>%
      dplyr::count(!!facet_quo, !!token_quo)

    # NOTE: Have not really tested `top_pct` and `num_top` before.
    if (!is.null(cnt_min)) {
      data_proc <-
        data_proc %>%
        dplyr::filter(n >= cnt_min)
        # filter_num_top_at("n", cnt_min)

      if(nrow(data_proc) == 0)
        stop("No data after filtering for `cnt_min`.", call. = FALSE)
    }

    data_proc <-
      data_proc %>%
      tidyr::spread(!!facet_quo, n, fill = 0)

      if (ncol(data_proc) == 2) {
        data_proc <- append_dummy_cols(data_proc, num_cols_expect = 3)
      }

      nms <- names(data_proc)
      out <-
        data_proc %>%
        stats::setNames(c(token, "x", "y")) %>%
        dplyr::mutate_if(is.numeric, dplyr::funs((. + 1) / sum(. + 1))) %>%
        dplyr::mutate(logratio = log(x / y))

      x_i <- nms[2]
      y_i <- nms[3]
      out %>%
        stats::setNames(c(token, x_i, y_i, "logratio")) %>%
        dplyr::arrange(dplyr::desc(logratio))
    }

#' @rdname compute_logratios_facet_wide
#' @export
compute_logratios_facet_wide <- compute_logratios_facet_wide_at

#' Compute token log ratios in pairs
#'
#' @description Apply \code{compute_logratios_facet_wide_at()} to
#' \code{data} in pairs of \code{facet} values.
#' @details Calls \code{wrapper_func} internally.
#' @inheritParams wrapper_func
#' @inheritParams compute_logratios_facet_wide
#' @param ... dots. Adddition parameters to pass to \code{compute_logratios_facet_wide_at()}.
#' @return data.frame.
#' @rdname compute_logratios_facet_by2
#' @export
compute_logratios_facet_by2_at <-
  function(data = NULL,
           func = compute_logratios_facet_wide_at,
           xy_grid = NULL,
           xy_nms = NULL,
           # token = NULL,
           facet = NULL,
           # cnt_min = 10,
           ...) {

    stopifnot(!is.null(data), is.data.frame(data))
    # stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(facet), is.character(facet))

    if(is.null(xy_grid) | is.null(xy_nms)) {
      facets <-
        data %>%
        pull_distinctly_at(facet)
      xy_grid <- create_xy_grid(facets)
      xy <- NULL
      xy_nms <- xy_grid %>% dplyr::pull(xy)
      message(sprintf("Generating output for all combinations of `%s`.", facet))
    }

    wrapper_func(
      data = data,
      func = func,
      xy_grid = xy_grid,
      xy_nms = xy_nms,
      col = facet,
      # token = token,
      facet = facet,
      # cnt_min = cnt_min
      ...
    )
  }

#' @rdname compute_logratios_facet_by2
#' @export
compute_logratios_facet_by2 <-
  function(..., token, facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    facet <- rlang::quo_text(rlang::enquo(facet))

    compute_logratios_facet_by2_at(
      ...,
      token = token,
      facet = facet
    )
  }

#' Visualize token log ratios in pairs
#'
#' @description Visualize token log ratios across pairs of \code{facet} values.
#' @details \code{compute_logratios_facet_by2_at()} should NOT be called beforehand.
#' @inheritParams compute_freqs_facet_by2
#' @inheritParams visualize_freqs_facet_by2
#' @inheritParams compute_logratios_facet_by2
#' @param ... dots. Additional parameters to pass to \code{compute_logratios_facet_by2_at()}.
#' @param num_top numeric. Number of tokens to show for each \code{facet} pair.
#' @param lab_other character. Name to give to 'opposing' factor of \code{facet_main}.
#' @param color_main character. Opposite color is calculated for 'opposing' color.
#' It is advised to use this argument instead of setting \code{values} with the \code{scale_manual_params} argument.
#' @return gg.
#' @rdname visualize_logratios_facet_by2
#' @export
#' @seealso \url{https://www.tidytextmining.com/twitter.html#comparing-token-usage}.
#' \url{http://varianceexplained.org/r/trump-tweets/}.
visualize_logratios_facet_by2_at <-
  function(data = NULL,
           token = NULL,
           facet = NULL,
           xy_nms,
           xy_grid,
           # cnt_min,
           ...,
           filter_facet = FALSE,
           facet_main = NULL,
           filter_facet_base = default_filter_facet(facet_main),
           filter_facet_params = list(),
           color = NULL,
           lab_other = ifelse(!is.null(facet_main), paste0("Not ", facet_main), "Other"),
           num_top = 5,
           color_main = "grey50",
           scale_manual_base =
             default_scale_manual(generate_named_dual_colors(color_main, facet_main, lab_other)),
           scale_manual_params = list(),
           labs_base = default_labs(),
           labs_params = list(title = "Most Unique Words", y = "Log Odds Ratio"),
           theme_base = default_theme(),
           theme_params =
             list(legend.position = "bottom",
                  axis.text.y = ggplot2::element_text(angle = 30, hjust = 1),
                  panel.grid.major.y = ggplot2::element_blank()),
           facet_base = default_facet("name_xy"),
           facet_params = list()) {

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
      compute_logratios_facet_by2_at(
        data = data,
        xy_grid = xy_grid,
        xy_nms = xy_nms,
        token = token,
        facet = facet,
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
        params = combine_lists(filter_facet_base, filter_facet_params)
      )

    data_proc <-
      data_proc %>%
      create_logratio_dir_at(
        cols_group = list("name_xy", "logratio_dir"),
        num_top = num_top
      )

    logratio_dir <- logratio <- name_x <- name_y <- NULL

    data_proc <- create_name_xy_facet_lab(data_proc)

    if (is.null(color)) {
      data_proc <- data_proc %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
    # NOTE: Don't need this.
    # data_proc <- wrangle_color_col(data_proc, color)

    data_proc <-
      data_proc %>%
      process_logratio_dir_at(
        token = token,
        color = color,
        facet = facet,
        facet_main = facet_main,
        lab_other = lab_other
      )

    # browser()
    viz <-
      data_proc %>%
      ggplot2::ggplot(ggplot2::aes_string(x = token, y = "logratio", fill = color)) +
      ggplot2::geom_col() +
      scale_x_reordered() +
      generate_scale_manual(scale_manual_base, scale_manual_params, type = "fill") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0))

    viz <-
      viz  +
      generate_facets(facet_base, facet_params)

    viz <-
      viz +
      generate_labs(labs_base, labs_params) +
      generate_theme(theme_base, theme_params)

    viz <-
      viz +
      ggplot2::coord_flip()

  }

#' @rdname visualize_logratios_facet_by2
#' @export
visualize_logratios_facet_by2 <-
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
    visualize_logratios_facet_by2_at(..., token = token, color = color, facet = facet)
  }

