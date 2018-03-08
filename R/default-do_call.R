

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
    invisible(do.call(wordcloud::wordcloud, params))
  }
}