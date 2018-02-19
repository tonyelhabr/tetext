#
# compute_ratios_multi <- function(xy_grid, xy_nms, data, ...) {
#   wrapper_func(xy_grid = xy_grid, xy_nms = xy_nms, data = data, func = compute_ratios)
# }
#
# compute_freqs_multi <- function() {}
#
# get_xy_grid <- function(xy_nms) {
#   xy_grid <-
#     dplyr::bind_cols(x = xy_nms, y = xy_nms) %>%
#     tidyr::complete(x, y)
#   if(length(xy_nms) > 1) {
#     xy_grid <- xy_grid %>% dplyr::filter(x != y)
#   } else {
#     xy_grid <- xy_grid %>% dplyr::mutate(y = paste0(x, "2"))
#   }
#
#   out <-
#     xy_grid %>%
#     dplyr::mutate(xy = paste0(x, "_", y)) %>%
#     dplyr::mutate(i = dplyr::row_number())
#   out
# }
#
# filter_xy_grid <- function(xy_grid, xy_nms, i) {
#   xy_i <- xy_nms[i]
#   xy_i_row <- xy_grid %>% dplyr:filter(xy == xy_i)
#   x_i <- xy_i_row %>%  dplyr:pull(x)
#   y_i <- xy_i_row %>%  dplyr:pull(y)
#   out <- list(x = x_i, y = y_i, xy = xy_i)
#   out
# }
#
# preprocess_xy_data <- function(data, xy_info) {
#   out <-
#     data %>%
#     dplyr:filter(name %in% c(xy_info$x, xy_info$y))
#   out
# }
#
# postprocess_xy_data <- function(data, xy_info) {
#   out <-
#     data %>%
#     dplyr:mutate(name_x = xy_info$x, name_y = xy_info$y) %>%
#     dplyr:mutate(name_xy = paste0(name_x, "_", name_y))
#   # browser()
#   if (length(setdiff(c(xy_info$x, xy_info$y), names(data))) == 0) {
#     out <-
#       out %>%
#       dplyr:rename(x = !!rlang::sym(xy_info$x),
#              y = !!rlang::sym(xy_info$y)) %>%
#       dplyr:select(name_x, name_y, name_xy, x, y,  dplyr:everything())
#   } else {
#     out <- out %>%  dplyr:select(name_x, name_y, name_xy,  dplyr:everything())
#   }
#   out
# }
#
#
# # TODO: Figure out how to use `purrr::map()` here.
# wrapper_func <- function(xy_grid, xy_nms, data, func) {
#   i <- 1
#   while (i <= length(xy_nms)) {
#     # browser()
#     xy_i_info <- filter_xy_grid(xy_grid, xy_nms, i)
#     data_i_preproc <- preprocess_xy_data(data, xy_i_info)
#     data_i_proc <- do.call(func, list(data_i_preproc))
#     # browser()
#     data_i_postproc <- postprocess_xy_data(data_i_proc, xy_i_info)
#     if (i == 1) {
#       out <- data_i_postproc
#     } else {
#       out <- dplyr::bind_rows(out, data_i_postproc)
#     }
#     i <- i + 1
#   }
#   out
# }