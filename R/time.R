
#' Visualize by time period.
#'
#' @description Visualize time-based data over time.
#' @details \code{facet} version is for facetting. Lists are used for
#' customization arguments (instead of dots) because there
#' are multiple arguments that can be customized.
#' @param data data.frame.
#' @param timebin bare for NSE; character for SE. Name of column in \code{data} to use for time axis.
#' @param bin logical. Whether or not to call \code{lubridate::floor_date()} to truncate \code{timebin}.
#' @param timefloor character. Name of column passed directly to \code{unit} parameter of
#' \code{lubridate::floor_date()} if \code{bin = FALSE}.
#' @param color bare for NSE; character for SE. Name of column in \code{data} to use for color basis.
#' Even if \code{fill} is the actual \code{ggplot2} aesthetic, the internal code will 'coerce' this
#' column to \code{fill}.
#' In order to simplify internal code, MUST be specified.
#' @param geom character. 'bar' or 'hist'. 'bar' is probably best for everything except \code{Date} objects.
#' @param add_alpha logical. Whether or not to use \code{ggplot2::scale_alpha()} based on count.
#' @param alpha_range numeric (vector). Direct parameter passed to \code{range} parameter
#' of \code{ggplot2::scale_alpha()}.
#' @param scale_manual_base list. Parameters to pass to \code{ggplot2::scale_fill_manual()} or
#' \code{ggplot2::scale_color_manual()}, depending on the aesthetic mapping. A default \code{values}
#' argument is specified in the internal
#' function \code{default_scale_manual()},
#' so if overwriting explicitly, then \code{values} should be included in the list.
#' @param scale_manual_params list. Additional parameters to pass to \code{ggplot2::scale_fill_manual()}
#' or \code{ggplot2::scale_color_manual()} (e.g. \code{breaks}, \code{labels}, or \code{name}).
#' The default \code{values} specified by \code{scale_manual_base = default_scale_manual()}
#' can be overwritten a \code{values} argument in this list (or directly with a
#' \code{values} argument in the \code{scale_manual_base} list.
#' @param labs_base \code{ggplot2::labs()} function. Defaults to a pre-determined set of values.
#' It is recommended NOT to modify this argument; isntead, the \code{_params} argument should
#' be used for customization.
#' @param labs_params list. Additional parameters to pass to \code{ggplot2::labs()} to use in addition
#' to and/or override the parameters set in the \code{_base} parameter.
#' @param theme_base \code{ggplot2::theme()} function. (e.g. as \code{ggplot2::theme_minimal()}.)
#' A custom theme is supplied as a default. As with the the \code{labs_base} argument, it is NOT
#' recommended to change this directly.
#' @param theme_params list. Additional parameters to pass to \code{ggplot2::labs()}.
#' Should be used in the same manner as \code{labs_params} for customization.
#' @return gg
#' @rdname visualize_time
#' @export
#' @seealso \url{https://juliasilge.com/blog/ten-thousand-tweets/}.
visualize_time_at <-
  function(data = NULL,
           timebin = NULL,
           bin = FALSE,
           timefloor = NULL,
           color = NULL,
           geom = c("bar", "hist"),
           add_alpha = FALSE,
           alpha_range = c(0.25, 1),
           scale_manual_base = default_scale_manual(),
           scale_manual_params = list(),
           labs_base = default_labs(),
           labs_params = list(title = "Count Over Time"),
           theme_base = default_theme(panel.grid.major.x = ggplot2::element_blank()),
           theme_params = list()) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(timebin), is.character(timebin))
    geom <- match.arg(geom)

    if(bin) {
      # browser()
      timebin_quo <- rlang::sym(timebin)
      stopifnot(!is.null(timefloor), is.character(timefloor))
      data <-
        data %>%
        dplyr::mutate(!!timebin_quo :=
                        lubridate::floor_date(!!timebin_quo, unit = timefloor))

      # data <-
      #   data %>%
      #   dplyr::mutate(!!timebin_quo := as.numeric(!!timebin_quo))
    }

    timebin_class <- get_class(data, timebin)
    if (!(
      timebin_class %in% c(
        "integer",
        "double",
        "numeric",
        "character",
        "factor",
        "ordered"
      )
    ) | (timebin_class %in% c("POSIXct", "POSIXt"))) {
      swap_geom <- TRUE
    } else {
      swap_geom <- FALSE
    }

    if (swap_geom) {
      if (geom != "hist") {
        geom <- "hist"
        message("Setting `geom = \"hist\"`.")
      }
    }

    if (is.null(color)) {
      data <- data %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
    # NOTE: Don't need this because not facetting.
    # data <- wrangle_color_col(data, color)

    viz <-
      data %>%
      ggplot2::ggplot(ggplot2::aes_string(x = timebin))

    if (geom == "bar") {
      if (!add_alpha) {
        viz <-
          viz +
          ggplot2::geom_bar(ggplot2::aes_string(y = "..count..",
                                                fill = color))
      } else {
        viz <-
          viz +
          ggplot2::geom_bar(ggplot2::aes_string(
            y = "..count..",
            alpha = "..count..",
            fill = color
          )) +
          ggplot2::scale_alpha(range = alpha_range)
      }
    } else if (geom == "hist") {
      viz <-
        viz +
        ggplot2::geom_histogram(ggplot2::aes_string(y = "..count..",
                                                    fill = color),
                                bins = 30)
    }
    viz <-
      viz + generate_scale_manual(scale_manual_base, scale_manual_params, type = "fill")

    viz <-
      viz +
      labs_base + do_call_labs(labs_params) +
      theme_base + do_call_theme(theme_params)
  }

#' @rdname visualize_time
#' @export
visualize_time <- function(...,
                           timebin,
                           color) {
  stopifnot(!missing(timebin))
  timebin <- rlang::quo_text(rlang::enquo(timebin))
  if (missing(color)) {
    color <- NULL
  } else {
    color <- rlang::quo_text(rlang::enquo(color))
  }
  visualize_time_at(
    ...,
    timebin = timebin,
    color = color
  )
}

#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param ... dots. Parameters to pass directly to \code{visualize_time()}.
#' @param facet bare for NSE; character for SE. Name of column in \code{data} to use for facetting.
#' @param facet_base \code{ggplot2::facet_wrap()} function. An internal function
#' sets defaults for \code{facets}, \code{scales}, \code{ncol}, \code{nrow}, and \code{strip.position}
#' As with the \code{labs_base} and \code{theme_base} arguments, it is NOT recommended to set
#' this argument explicitly; instead, the accompanying \code{_params} argument should be used.
#' @param facet_params list. Additional parameters to pass to \code{ggplot2::facet_wrap()}. Parameters
#' passed to this argument override thos in the \code{_base} argument.
#' @rdname visualize_time
#' @export
visualize_time_facet_at <-
  function(...,
           theme_base =
             default_theme(panel.grid.major.x = ggplot2::element_blank()),
           facet = NULL,
           facet_base = default_facet(facet),
           facet_params = list()) {
    stopifnot(!is.null(facet), is.character(facet))
    viz <-
      visualize_time_at(...,
                        theme_base = theme_base)

    viz <- viz + generate_facets(facet_base, facet_params)
  }

#' @rdname visualize_time
#' @export
visualize_time_facet <-
  function(..., timebin, color, facet) {
    stopifnot(!missing(timebin))
    stopifnot(!missing(facet))
    timebin <- rlang::quo_text(rlang::enquo(timebin))
    facet <- rlang::quo_text(rlang::enquo(facet))
    if (missing(color)) {
      color <- NULL
    } else {
      color <- rlang::quo_text(rlang::enquo(color))
    }
    visualize_time_facet_at(
      ...,
      timebin = timebin,
      color = color,
      facet = facet
    )

  }


#' Visualize hourly data
#'
#' @description Visualize time-based data aggregated over hours.
#' @details This function is 'unique' because it calls \code{ggplot2::geom_violin()}.
#' Also, it does not actually use \code{facet} for facetting.
#' @inheritParams visualize_time
#' @inheritParams visualize_time_facet
#' @param facet bare for NSE; character for SE. Name of column in \code{data}
#' to use one of main axes, not for facetting.
#' @param timebin bare for NSE; character for SE. Name of column in \code{data}
#' to use for time. Probably likely something like 'hh' or 'hour'.
#' @rdname visualize_time_hh
#' @export
visualize_time_hh_at <-
  function(data = NULL,
           timebin = NULL,
           facet = NULL,
           color = facet,
           scale_manual_params = default_scale_manual(),
           labs_base = default_labs(),
           labs_params = list(title = "Count Over Time"),
           theme_base = default_theme(),
           theme_params = list(panel.grid = ggplot2::element_blank())) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(timebin), is.character(timebin))
    stopifnot(!is.null(facet), is.character(facet))

    if (is.null(color)) {
      data <- data %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"
    }
    # NOTE: DON'T NEED THIS!
    # data <- wrangle_color_col(data, color)

    viz <-
      data %>%
      ggplot2::ggplot(ggplot2::aes_string(x = facet, y = timebin, fill = color)) +
      ggplot2::scale_y_continuous(
        limits = c(1, 24),
        breaks = c(6, 12, 18),
        labels = c("6 AM", "Noon", "6 PM")
      ) +
      generate_scale_manual(scale_manual_base, scale_manual_params, type = "fill") +
      ggplot2::geom_violin(size = 0) +
      ggplot2::geom_hline(
        yintercept = seq(3, 24, by = 3),
        color = "black",
        size = 0.1
      )

    viz <-
      viz +
      labs_base + do_call_labs(labs_params) +
      theme_base + do_call_theme(theme_params)

    viz <-
      viz +
      ggplot2::coord_flip()

  }

#' @rdname visualize_time_hh
#' @export
visualize_time_hh <-
  function(..., timebin, color, facet) {
    stopifnot(!missing(timebin))
    stopifnot(!missing(facet))
    timebin <- rlang::quo_text(rlang::enquo(timebin))
    facet <- rlang::quo_text(rlang::enquo(facet))
    if (missing(color)) {
      color <- NULL
    } else {
      color <- rlang::quo_text(rlang::enquo(color))
    }
    visualize_time_hh_at(...,
                          timebin = timebin,
                          color = color,
                          facet = facet)
  }
