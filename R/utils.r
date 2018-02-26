
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
    message(sprintf("Coercing %s to a factor.", nm_i))
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
