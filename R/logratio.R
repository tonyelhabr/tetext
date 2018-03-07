
#' Compute log ratios
#'
#' @description Compute log ratios for usage of tokens between two entities.
#' @details Note that an attempt was made to make this function flexible enough,
#' to consider a \code{num_top} or \code{top_pct} parameter that might be used if
#' \code{cnt_min = NULL}, but this can be troublesome because \code{do.call()}
#' does not seem to handle \code{NULL} in \code{...}.
#' @inheritParams compute_freqs_facet_at
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
    stopifnot(!is.null(data), is.data.frame(data))
      stop("`data` must not be NULL.", call. = FALSE)
    stopifnot(!is.null(facet), is.character(facet))
      stop("`facet` must not be NULL.", call. = FALSE)

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

      if(nrow(data_proc) == 0)
        return(stop("No data after filtering for `cnt_min`.", call. = FALSE))
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
        stats::setNames(c("token", "x", "y")) %>%
        dplyr::mutate_if(is.numeric, dplyr::funs((. + 1) / sum(. + 1))) %>%
        dplyr::mutate(logratio = log(x / y))

      x_i <- nms[2]
      y_i <- nms[3]
      out %>%
        stats::setNames(c("token", x_i, y_i, "logratio")) %>%
        dplyr::arrange(dplyr::desc(logratio))
      out
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
#' @inheritParams compute_logratios_facet_wide_at
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
    #   
    stopifnot(!is.null(facet), is.character(facet))
      

    if(is.null(xy_grid) | is.null(xy_nms)) {
      facets <-
        data %>%
        pull_distinctly_at(facet)
      xy_grid <- create_xy_grid(facets)
      xy <- NULL
      xy_nms <- xy_grid %>% dplyr::pull(xy)
      message("Generating output for all combinations of `facet`.")
    }

    wrapper_func(
      data = data,
      func = func,
      xy_grid = xy_grid,
      xy_nms = xy_nms,
      # token = token,
      facet = facet,
      # cnt_min = cnt_min
      ...
    )
  }

#' @rdname compute_logratios_facet_by2
#' @export
compute_logratios_facet_by2 <- compute_logratios_facet_by2_at

#' Visualize token log ratios in pairs
#'
#' @description Visualize token log ratios across pairs of \code{facet} values.
#' @details \code{compute_logratios_facet_by2_at()} should NOT be called beforehand.
#' @inheritParams compute_freqs_facet_by2_at
#' @inheritParams visualize_freqs_facet_by2_at
#' @inheritParams compute_logratios_facet_by2_at
#' @param ... dots. Additional parameters to pass to \code{compute_logratios_facet_by2_at()}.
#' @param num_top numeric. Number of tokens to show for each \code{facet} pair.
#' @param flip_axes logical. Whether or not to call \code{ggplot2::coord_flip()}.
#' @param lab_other bare for NSE; character for SE. Name to give to 'opposing' factor of \code{facet_main}.
#' @param color_value character (vector). Should be a vector of length two (for dual colors)
#' if \code{filter_facet = TRUE} and \code{!is.null(facet_main) & length(facet_main) == 1}.
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
           filter_facet = TRUE,
           facet_main = NULL,
           x_include = NULL,
           y_include = NULL,
           x_exclude = NULL,
           y_exclude = NULL,
           color = NULL,
           color_value = c("grey50", "grey80"),
           num_top = 3,
           flip_axes = FALSE,
           lab_other = "other",
           title = "Most Unique Words",
           subtitle = NULL,
           caption = NULL,
           lab_x = NULL,
           lab_y = "Log Odds Ratio",
           theme_base = theme_tetext_facet(),
           scales = "free",
           ncol = 3,
           nrow = NULL,
           strip.position = "right") {

    # NOTE: Checks for NULL are made in other function(s).

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

    data_proc <-
      data_proc %>%
      filter_data_facet_at(
        filter_facet = filter_facet,
        facet_main = facet_main,
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

    data_proc <- wrangle_facet_col(data_proc, "name_xy")

    if (is.null(color)) {
      data_proc <- data_proc %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
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
      # dplyr::mutate(!!token_quo := stats::reorder(!!token_quo, dplyr::desc(logratio)))

    viz <-
      data_proc %>%
      ggplot2::ggplot(ggplot2::aes_string(x = token, y = "logratio", fill = color)) +
      # ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_col() +
      scale_x_reordered() +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +
      ggplot2::scale_fill_manual(values = color_value) +
      ggplot2::facet_wrap(
        ~ name_xy,
        scales = scales,
        ncol = ncol,
        nrow = nrow,
        strip.position = strip.position
      )

    viz_labs <-
      ggplot2::labs(
        x = lab_x,
        y = lab_y,
        title = title,
        subtitle = subtitle,
        caption = caption
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

#' @rdname visualize_logratios_facet_by2
#' @export
visualize_logratios_facet_by2 <- visualize_logratios_facet_by2_at

