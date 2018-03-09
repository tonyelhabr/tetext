

#' Compute token frequencies in pairs
#'
#' @description Apply \code{compute_freqs_facet_wide_at()} to
#' \code{data} in pairs of \code{facet} values.
#' @details Calls \code{wrapper_func} internally.
#' @inheritParams wrapper_func
#' @inheritParams compute_freqs_facet
#' @param ... dots. Additional parameters to pass to \code{compute_freqs_facet_wide_at()}.
#' @return data.frame.
#' @rdname compute_freqs_facet_by2
#' @export
compute_freqs_facet_by2_at <-
  function(data = NULL,
           func = compute_freqs_facet_wide_at,
           xy_grid = NULL,
           xy_nms = NULL,
           # token = NULL,
           facet = NULL,
           ...) {

    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(func), is.function(func))
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
      ...
    )
  }

#' @rdname compute_freqs_facet_by2
#' @export
compute_freqs_facet_by2 <-
  function(...,
           token,
           facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    facet <- rlang::quo_text(rlang::enquo(facet))
    compute_freqs_facet_by2_at(
      ...,
      token = token,
      facet = facet
    )
  }


#' Visualize token frequencies in pairs
#'
#' @description Visualize token frequenceis across pairs of \code{facet} values.
#' @details \code{compute_freqs_facet_by2_at()} should NOT be called beforehand.
#' @inheritParams visualize_time_facet
#' @inheritParams compute_freqs_facet
#' @inheritParams compute_freqs_facet_by2
#' @param add_labels logical. Whether or not to add text labels (of the tokens).
#' @param filter_facet logical. Whether or not to filter the \code{facet} values.
#' \code{facet} values to include and exclude. Not used if \code{filter_facet = FALSE}.
#' @param facet_main of same type as \code{facet} values. Value of single 'main' facet.
#' @param filter_facet_base stuff. Work similarly to other \code{_base} arguments.
#' @param filter_facet_params list. Works similarly to other \code{_params} arguments.
#' @return gg.
#' @rdname visualize_freqs_facet_by2
#' @export
visualize_freqs_facet_by2_at <-
  function(data = NULL,
           token = NULL,
           facet = "name_xy",
           xy_nms,
           xy_grid,
           filter_facet = FALSE,
           facet_main = NULL,
           filter_facet_base = default_filter_facet(facet_main),
           filter_facet_params = list(),
           color = NULL,
           add_labels = TRUE,
           scale_manual_base = default_scale_manual(),
           scale_manual_params = list(),
           labs_base = default_labs(),
           labs_params = list(title = "Relative Word Frequency"),
           theme_base = default_theme(),
           theme_params = list(),
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
      compute_freqs_facet_by2_at(
        data = data,
        xy_grid = xy_grid,
        xy_nms = xy_nms,
        token = token,
        facet = facet
      )

    data_proc <-
      data_proc %>%
      validate_x_main(
        filter_x = filter_facet,
        x = facet,
        xs = data %>% pull_distinctly_at(facet),
        x_main = facet_main
      )

    if(facet != "name_xy") {
      message("Changing facetting variable to `name_xy`.")
    }
    data_proc <-
      data_proc %>%
      filter_data_facet_at(
        filter_facet = filter_facet,
        params = combine_lists(filter_facet_base, filter_facet_params)
      )

    name_xy <- name_x <- name_y <- NULL

    data_proc <-
      data_proc %>%
      dplyr::mutate(name_xy = paste0(name_x, " vs. ", name_y))

    if (is.null(color)) {
      data_proc <- data_proc %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
    # NOTE: This is not necessary here.
    # data_proc <- wrangle_color_col(data_proc, color)

    viz <-
      data_proc %>%
      ggplot2::ggplot(ggplot2::aes_string(x = "x", y = "y", color = color))

    if(add_labels) {
      viz <-
        viz +
        ggplot2::geom_text(
          ggplot2::aes_string(
            label = token,
            size = "x + y"
          ),
          check_overlap = TRUE
        )
    } else {
      viz <-
        viz +
        ggplot2::geom_point(ggplot2::aes_string(size = "x + y"))
    }

    viz <-
      viz +
      generate_scale_manual(scale_manual_base, scale_manual_params, type = "color") +
      ggplot2::scale_x_log10(labels = NULL) +
      ggplot2::scale_y_log10(labels = NULL) +
      ggplot2::geom_abline(color = "red")

    viz <-
      viz + generate_facets(facet_base, facet_params)
    viz <-
      viz +
      generate_labs(labs_base, labs_params) +
      generate_theme(theme_base, theme_params)
  }

#' @rdname visualize_freqs_facet_by2
#' @export
visualize_freqs_facet_by2 <-
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
    visualize_freqs_facet_by2_at(..., token = token, color = color, facet = facet)
  }

