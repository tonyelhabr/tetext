

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
