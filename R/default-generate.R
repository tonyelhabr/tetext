

uniquify_list <- function(x, fromLast = TRUE) {
  x[!duplicated(names(x), fromLast = fromLast)]
}

combine_lists_old <- function(base, params) {
  stopifnot(!missing(base), !missing(params))
  params_comb <- utils::modifyList(base, params)
  uniquify_list(params_comb)
}

combine_lists <- function(...) {
  uniquify_list(c(...))
}

# a <- list(x = 1, y = 2)
# b <- list(y = 3, z = 4)
# c <- list(z = 5, x = 6)
# combine_lists(a, b)
# combine_lists(b, c)
# combine_lists_v2(a, b, c)

generate_theme <- function(base, params) {
  do_call_theme(combine_lists(base, params))
}

generate_labs <- function(base, params) {
  do_call_labs(combine_lists(base, params))
}

generate_facets <- function(base, params) {
  do_call_facet(combine_lists(base, params))
}

generate_scale_manual <- function(base, params, type) {
  do_call_scale_manual(combine_lists(base, params), type)
}


generate_wordcloud <- function(base, params) {
  do_call_wordcloud(combine_lists(base, params))
}
