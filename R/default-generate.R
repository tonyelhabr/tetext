

uniquify_list <- function(x, fromLast = TRUE) {
  x[!duplicated(names(x), fromLast = fromLast)]
}

combine_base_and_params <- function(base, params) {
  stopifnot(!missing(base), !missing(params))
  params_comb <- utils::modifyList(base, params)
  uniquify_list(params_comb)
}

generate_theme <- function(base, params) {
  do_call_theme(combine_base_and_params(base, params))
}
generate_labs <- function(base, params) {
  do_call_labs(combine_base_and_params(base, params))
}

generate_facets <- function(base, params) {
  do_call_facet(combine_base_and_params(base, params))
}

generate_scale_manual <- function(base, params, type) {
  do_call_scale_manual(combine_base_and_params(base, params), type)
}


generate_wordcloud <- function(base, params) {
  do_call_wordcloud(combine_base_and_params(base, params))
}
