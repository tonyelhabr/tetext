
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

filter_num_top <- filter_num_top_at

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
