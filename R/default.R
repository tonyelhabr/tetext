
#' Default theme parameters
#'
#' @description A custom \code{ggplot2} theme.
#' @details This theme is based on \code{ggplot2::theme_minimal()}.
#' @param base_family character.
#' @param base_size,plot_title_size,subtitle_size numeric.
#' @param void logical. Whether or not to modifly theme to mimic \code{ggplot2::theme_void()}/
#' @param ... dots. Additional parameters passed to \code{ggplot2::theme()}.
#' @return function.
#' @export
default_theme <- function (base_family = "Arial Narrow",
                           # base_family = "",
                           base_size = 11.5,
                           # base_size = 11,
                           plot_title_size = 18,
                           # plot_title_size = 16,
                           subtitle_size = 12,
                           # subtitle_size = 12,
                           void = FALSE,
                           ...) {
  out <-
    ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  out <-
    out +
    ggplot2::theme(
      legend.position = "none",
      legend.title = ggplot2::element_blank()
    )

  out <-
    out +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )

  out <-
    out +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = plot_title_size,
        face = "bold",
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(size = subtitle_size, hjust = 0),
      plot.caption = ggplot2::element_text(face = "italic"),
      ...
    )

  if(void) {
    out <-
      out +
      ggplot2::theme(
        line = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      ) +
      ggplot2::theme()
  }

  out
}

#' Default label parameters
#'
#' @description None.
#' @details Plays a similar role as \code{default_theme()}, but for labels.
#' @param title,subtitle,caption,x,y character. It is recommended NOT
#' to use these parameters directly. (Instead, it is recommended that they are modified
#' 'after' the function is called.) Noneheless, they are provided as inputs and assigned
#' reasonable defaults (e.g. 'Count Over Time')
#' where appropriate. (Otherwise, they are given the value of NULL.)
#' @return function.
#' @rdname default_labs
#' @export
default_labs <-
  function(title = NULL,
           subtitle = NULL,
           caption = NULL,
           x = NULL,
           y = NULL) {
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x,
      y = y
    )
  }

#' Default facet parameters
#'
#' @description None.
#' @details Not exactly analogous to \code{default_theme()} and \code{default_labs()}
#' because this function returns a list, not a function.
#' @param facet,scales,ncol,nrow,strip.position,... Parameters passed directly to
#' \code{ggplot2::facet_wrap()}
#' @return list
#' @rdname default_facet
#' @export
default_facet <-
  function(facet, scales = "free", ncol = 3, nrow = NULL, strip.position = "top", ...) {
    stopifnot(!missing(facet), is.character(facet))
    # if(!plyr::is.formula(facet)) {
    if(!grepl("~", facet)) {
      if(length(facet) > 1) {
        if(length(facet) == 2) {
          facet <- paste0(facet[1], "~ ", facet[2])
        }
      } else {
        facet <- paste0("~ ", facet)
      }
      # facet <- stats::as.formula(facet)
    }
    facet <- stats::as.formula(facet)
    list(facets = facet,
         scales = scales,
         ncol = ncol,
         nrow = nrow,
         strip.position = strip.position,
         ...)
  }

#' Default word cloud parameters
#'
#' @description None.
#' @details Not exactly analogous to \code{default_theme()} and \code{default_labs()}
#' because this function returns a list, not a function. More similar to
#' \code{default_facet()}.
#' @param data data.frame. Used for computing counts for \code{token} to use for
#' \code{freq} argument in \code{wordcloud::wordcloud()}.
#' @param token bare for NSE; character for SE. Name of column in \code{data} used for
#' \code{words} argument in \code{wordcloud::wordcloud()}.
#' @param colors,max.words,random.order,... Parameters passed directly to
#' \code{ggplot2::facet_wrap()}
#' @return list
#' @rdname default_wordcloud
#' @export
default_wordcloud <-
  function(data,
           token,
           colors = "grey50",
           max.words = 50,
           random.order = FALSE,
           ...) {
    stopifnot(!missing(data))
    stopifnot(!missing(token))

    token_quo <- rlang::sym(token)
    data_proc <- data %>% dplyr::count(!!token_quo)

    if (max.words < 1) {
      max.words <- nrow(data) * (1 - max.words)
    }

    n <- NULL

    tokens <- data_proc %>% dplyr::pull(!!token_quo)
    freqs <- data_proc %>% dplyr::pull(n)
    list(words = tokens,
         freq = freqs,
         colors = colors,
         max.words = max.words,
         random.order = random.order,
         ...)
  }

#' Default word cloud parameters
#'
#' @description None.
#' @details Similar to other 'default' functions.
#' @param values character (vector). Passed directly to \code{ggplot2::scale_} function.
#' @param ... dots. Additional parameters to pass to \code{ggplot2::scale_} function.
#' @return list.
#' @rdname default_scale_manual
#' @export
default_scale_manual <-
  function(values = "grey50", ...) {
    list(values = values, ...)
  }

#' Default word cloud parameters
#'
#' @description None.
#' @details Similar to other 'default' functions.
#' @inheritParams visualize_freqs_facet_by2
#' @param x_include,y_include,x_exclude,y_exclude character (vector).
#' \code{facet} values to include and exclude. Not used if \code{filter_facet = FALSE}.
#' @return list.
#' @rdname default_filter_facet
#' @export
default_filter_facet <-
  function(facet_main = NULL,
           x_include = NULL,
           y_include = NULL,
           x_exclude = NULL,
           y_exclude = NULL) {
    list(
      facet_main = facet_main,
      x_include = x_include,
      y_include = y_include,
      x_exclude = x_exclude,
      y_exclude = y_exclude
    )
  }


