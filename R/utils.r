
invert_pct <- function(num) {
  if(num < 0.5) {
    num <- 1 - num
  }
  num
}


filter_num_top_at <- function(data = NULL, col = NULL, num_top = NULL) {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)
  if (is.null(col))
    stop("`col` cannot be NULL.", call. = FALSE)
  if (is.null(num_top))
    stop("`num_top` cannot be NULL.", call. = FALSE)
  if(num_top <= 0)
    stop("`num_top` must be greater than 0.", call. = FALSE)

  rank <- NULL
  col_quo <- rlang::sym(col)

  if(num_top >= 1) {
    if(num_top > nrow(data))
      return(
        stop(
          sprintf(
            "`num_top` (%.0f) must not be greater than `nrow(data)` (%.0f).",
            num_top,
            nrow(data)
          ),
          call. = FALSE
        )
      )
    out <-
      data %>%
      dplyr::mutate(rank = dplyr::row_number(dplyr::desc(!!col_quo))) %>%
      dplyr::filter(rank <= num_top)
  } else if((num_top > 0) & (num_top < 1)) {
    num_top <- invert_pct(num_top)
    out <-
      data %>%
      dplyr::arrange(dplyr::desc(!!col_quo)) %>%
      dplyr::filter(!!col_quo >= stats::quantile(!!col_quo, num_top, na.rm = TRUE))
  }
  out
}

require_ns <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf('Package "%s" needed for this function to work. Please install it.', pkg), call. = FALSE)
  }
}

# NOTE: Need this because NULLs cannot be used with lists, which would break visualize_time_batch_at().
is_nothing <- function(x) {
  any(is.null(x)) || any(is.na(x)) || any(is.nan(x))
}

coerce_col_to_factor <- function(data, colname) {
  classes <- sapply(names(data), class)
  class_i <- classes[names(classes) == colname]
  nm_i <- names(class_i)
  if (class_i != "factor") {
    data <-
      data %>% dplyr::mutate_at(dplyr::vars(dplyr::contains(nm_i)), dplyr::funs(factor))
    # message(sprintf("Coercing %s to a factor.", nm_i))
  }
  data
}

# wrangle_color_col <-
#   function(data,
#            color,
#            dummy = "color_value",
#            value_dummy = "dummy") {
#     if (!is.null(color)) {
#       out <- list(data = data, color = color)
#     } else {
#       dummy_quo <- rlang::sym(dummy)
#       value_dummy_quo <- rlang::sym(value_dummy)
#       data <- data %>% dplyr::mutate(!!dummy := !!value_dummy)
#       data_proc <- coerce_col_to_factor(data_proc, color)
#       out <- list(data = data, color = dummy)
#     }
#     data <- coerce_col_to_factor(data, color)
#     out
#   }

# NOTE: These could change in the future.
wrangle_color_col <-
  function(data, colname) {
    coerce_col_to_factor(data, colname)
  }

wrangle_multi_col <-
  function(data, colname) {
    coerce_col_to_factor(data, colname)
  }

filter_if_not_null_at <- function(data = NULL, col = NULL, value = NULL, invert = FALSE) {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)

  if(is.null(col))
    return(data)

  if(is.null(value))
    return(data)

  if(!(col %in% names(data)))
    return(stop("`col` must be in `data`.", call. = FALSE))

  col_quo <- rlang::sym(col)
  if(!invert) {
    out <-
      data %>%
      dplyr::filter(!!col_quo %in% value)
  } else {
    out <-
      data %>%
      dplyr::filter(!(!!col_quo %in% value))
  }
  out
}

pull_distinctly_at <- function(data = NULL, col = NULL) {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)

  if(!(col %in% names(data)))
    return(stop("`col` must be in `data`.", call. = FALSE))

  col_quo <- rlang::sym(col)
  data %>%
    dplyr::distinct(!!col_quo) %>%
    dplyr::arrange(!!col_quo) %>%
    dplyr::pull(!!col_quo)
}


filter_data_multi_at <-
  function(data = NULL,
           filter_multi = NULL,
           x_include = NULL,
           y_include = NULL,
           x_exclude = NULL,
           y_exclude = NULL,
           multi_main = NULL) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    if (is.null(filter_multi))
      stop("`filter_multi` cannot be NULL.", call. = FALSE)

    if (filter_multi) {
      data <-
        data %>% filter_if_not_null_at("name_x", multi_main, invert = FALSE)
      data <-
        data %>% filter_if_not_null_at("name_x", x_include, invert = FALSE)
      data <-
        data %>% filter_if_not_null_at("name_y", y_include, invert = FALSE)
      data <-
        data %>% filter_if_not_null_at("name_x", x_exclude, invert = TRUE)
      data <-
        data %>% filter_if_not_null_at("name_y", y_exclude, invert = TRUE)

    } else {
      message("It's recommended to set `filter_multi = TRUE`.")
    }
    data
  }

# validate_multi_main <-
#   function(data = NULL,
#            filter_multi = NULL,
#            multi = NULL,
#            multis = NULL,
#            multi_main = NULL) {
#     if (is.null(data))
#       stop("`data` cannot be NULL.", call. = FALSE)
#     if (is.null(filter_multi))
#       stop("`filter_multi` cannot be NULL.", call. = FALSE)
#
#     if (filter_multi) {
#       if (is.null(multi))
#         stop("`multi` cannot be NULL.", call. = FALSE)
#       if (is.null(multis))
#         stop("`multis` cannot be NULL.", call. = FALSE)
#       if (is.null(multi_main))
#         return(stop(
#           "`multi_main` must not be NULL if `filter_multi = TRUE`.",
#           call. = FALSE
#         ))
#       if (!(multi_main %in% multis))
#         return(stop(sprintf(
#           "`multi_main` is not in %s.", paste(multis, collapse = ",")
#         ), call. = FALSE))
#       if (length(multi_main) > 1)
#         return(stop("`multi_main` should be a singular value.", call. = FALSE))
#     }
#     invisible(data)
#   }
#
# validate_sent_main <-
#   function(data = NULL,
#            filter_sent = NULL,
#            sent = "sentiment",
#            sent_main = NULL) {
#     if (is.null(data))
#       stop("`data` cannot be NULL.", call. = FALSE)
#     if (is.null(filter_sent))
#       stop("`filter_sent` cannot be NULL.", call. = FALSE)
#     if (filter_sent) {
#       if (is.null(sent))
#         stop("`sent` cannot be NULL.", call. = FALSE)
#       if (is.null(sent_main))
#         return(stop(
#           "`sent_main` must not be NULL if `filter_sent = TRUE`.",
#           call. = FALSE
#         ))
#       if (length(sent_main) > 1)
#         return(stop("`sent_main` should be a singular value.", call. = FALSE))
#     }
#     invisible(data)
#   }

validate_x_main <-
  function(data = NULL,
           filter_x = NULL,
           x = NULL,
           xs = NULL,
           x_main = NULL) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    if (is.null(filter_x))
      stop("`filter_x` cannot be NULL.", call. = FALSE)

    if (filter_x) {
      if (is.null(x))
        stop("`x` cannot be NULL.", call. = FALSE)
      if (is.null(xs))
        stop("`xs` cannot be NULL.", call. = FALSE)
      if (is.null(x_main))
        return(stop(
          "`x_main` must not be NULL if `filter_x = TRUE`.",
          call. = FALSE
        ))
      if (!(x_main %in% xs))
        return(stop(sprintf(
          "`x_main` is not in %s.", paste(xs, collapse = ",")
        ), call. = FALSE))
      if (length(x_main) > 1)
        return(stop("`x_main` should be a singular value.", call. = FALSE))
    }
    data
  }

