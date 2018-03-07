
# _tetext ----
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
#' @seealso \url{https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r}
theme_tetext <- function (base_family = "",
                          base_size = 11,
                          plot_title_size = 16,
                          subtitle_size = 12,
                          void = FALSE,
                          ...) {
  out <-
    ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  out <-
    out +
    ggplot2::theme(
      legend.position = "bottom",
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
      ggplot2::theme(legend.position = "none")
  }

  out
}

#' @rdname theme_tetext
#' @export
theme_tetext_dx <-
  function(...) {
    theme_tetext(panel.grid.major.x = ggplot2::element_blank(), ...)
  }

#' @rdname theme_tetext
#' @export
theme_tetext_dy <-
  function(...) {
    theme_tetext(panel.grid.major.y = ggplot2::element_blank(), ...)
  }

#' @rdname theme_tetext
#' @export
theme_tetext_facet <-
  function(...) {
    theme_tetext(panel.background = ggplot2::element_rect(), ...)
}

#' @rdname theme_tetext
#' @export
theme_tetext_facet_dx <- function(...) {
  theme_tetext(
    panel.background = ggplot2::element_rect(),
    panel.grid.major.x = ggplot2::element_blank(),
    ...
  )
}

#' @rdname theme_tetext
#' @export
theme_tetext_facet_dy <- function(...) {
  theme_tetext(
    panel.background = ggplot2::element_rect(),
    panel.grid.major.y = ggplot2::element_blank(),
    ...
  )
}


#' Default label parameters
#'
#' @description None.
#' @details Plays a similar role as \code{theme_tetext()}, but for labels.
#' @param title,subtitle,caption,lab_x,lab_y character. It is recommended NOT
#' to use these parameters directly. (Instead, it is recommended that they are modified
#' 'after' the function is called.) Noneheless, they are provided as inputs and assigned
#' reasonable defaults (e.g. 'Count Over Time')
#' where appropriate. (Otherwise, they are given the value of NULL.)
#' @return function.
#' @rdname labs_tetext
#' @export
labs_tetext <-
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
#' @details Not exactly analogous to \code{theme_tetext()} and \code{labs_tetext()}
#' because this function returns a list, not a function.
#' @param facet,scales,ncol,nrow,strip.position,... Parameters passed directly to
#' \code{ggplot2::facet_wrap()}
#' @return list
#' @rdname labs_tetext
#' @export
facet_tetext <-
  function(facet, scales = "free", ncol = 3, nrow = NULL, strip.position = "right", ...) {
    stopifnot(!missing(facet))
    list(facets = stats::as.formula(paste0("~ ", facet)),
         scales = scales,
         ncol = ncol,
         nrow = nrow,
         strip.position = strip.position,
         ...)
  }

#' Default word cloud parameters
#'
#' @description None.
#' @details Not exactly analogous to \code{theme_tetext()} and \code{labs_tetext()}
#' because this function returns a list, not a function. More similar to
#' \code{facet_tetext()}.
#' @param data data.frame. Used for computing counts for \code{token} to use for
#' \code{freq} argument in \code{wordcloud::wordcloud()}.
#' @param token bare for NSE; character for SE. Name of column in \code{data} used for
#' \code{words} argument in \code{wordcloud::wordcloud()}.
#' @param colors,max.words,random.order,... Parameters passed directly to
#' \code{ggplot2::facet_wrap()}
#' @return list
#' @rdname labs_tetext
#' @export
wordcloud_tetext <-
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

    tokens <- data_proc %>% dplyr::pull(!!token_quo)
    freqs <- data_proc %>% dplyr::pull(n)
    list(words = tokens,
         freq = freqs,
         colors = colors,
         max.words = max.words,
         random.order = random.order,
         ...)
  }

scale_manual_tetext <-
  function(values = "grey50", ...) {
    list(values = values, ...)
  }

filter_facet_tetext <-
  function(filter_main = NULL,
           x_include = NULL,
           y_include = NULL,
           x_exclude = NULL,
           y_exclude = NULL) {
    list(
      filter_main = filter_main,
      x_include = x_include,
      y_include = y_include,
      x_exclude = x_exclude,
      y_exclude = y_exclude
    )
  }

# do_call_ ----
# NOTE: Don't want to use `stopifnot()` here because returning an empty list is fine.
do_call_theme <- function(params) {
  if (!missing(params))
    do.call(ggplot2::theme, params)

}

do_call_labs <- function(params) {
  if (!missing(params)) {
    do.call(ggplot2::labs, params)
  }
}

do_call_facet <-  function(params) {
  if (!missing(params)) {
    do.call(ggplot2::facet_wrap, params)
  }
}

do_call_scale_color_manual <- function(params) {
  if (!missing(params)) {
    do.call(ggplot2::scale_color_manual, params)
  }
}

do_call_scale_fill_manual <- function(params) {
  if (!missing(params)) {
    do.call(ggplot2::scale_fill_manual, params)
  }
}

do_call_scale_manual <- function(params, type = c("color", "fill")) {
  type <- match.arg(type)
  switch(type,
         color = do_call_scale_color_manual(params),
         fill = do_call_scale_fill_manual(params)
  )
}

do_call_wordcloud <- function(params) {
  if (!missing(params)) {
    do.call(wordcloud::wordcloud, params)
  }
}

# generate_ ----
uniquify_list <- function(x) {
  x[!duplicated(names(x), fromLast = TRUE)]
}

combine_base_and_params <- function(base, params) {
  stopifnot(!missing(base), !missing(params))
  params_comb <- utils::modifyList(base, params)
  uniquify_list(params_comb)
}

generate_facets <- function(base, params) {
  do_call_facet(combine_base_and_params(base, params))
}

generate_wordcloud <- function(base, params) {
  do_call_wordcloud(combine_base_and_params(base, params))
}

