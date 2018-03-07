

invert_pct <- function(num) {
  if(num < 0.5) {
    # message(sprintf("Inverting %f to %f.", num, 1 - num))
    num <- 1 - num
  }
  num
}

validate_range <- function(x, max = 1, min = 0) {
  if(x > max) {
    stop("`x` must not be greater than ", max, ".", call. = FALSE)
  }
  if(x < min) {
    stop("`x` must not be less than ", min, ".", call. = FALSE)
  }
  x
}

filter_num_top_at <-
  function(data = NULL,
           col = NULL,
           num_top = NULL,
           max = nrow(data),
           min = 0) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(col))
    stopifnot(!is.null(num_top))
    num_top <- validate_range(x = num_top, max = max, min = min)

    rank <- NULL
    col_quo <- rlang::sym(col)

    if (num_top >= 1) {
      out <-
        data %>%
        dplyr::mutate(rank = dplyr::row_number(dplyr::desc(!!col_quo))) %>%
        dplyr::filter(rank <= num_top)
    } else {
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
    stop(
      sprintf(
        'Package "%s" needed for this function to work. Please install it.',
        pkg
      ),
      call. = FALSE
    )
  }
}

get_class <- function(data, col) {
  classes <- sapply(data, class)
  # classes[names(classes) == col]
  rev(classes[[col]])[1]
}

coerce_col_to_factor <- function(data, col) {
  class_i <- get_class(data, col)
  if (class_i != "factor") {
    data <-
      data %>% dplyr::mutate_at(dplyr::vars(dplyr::contains(col)), dplyr::funs(factor))
    # message(sprintf("Coercing %s to a factor.", nm_i))
  }
  data
}

# NOTE: These could change in the future.
wrangle_color_col <-
  function(data, col) {
    coerce_col_to_factor(data, col)
  }

# wrangle_facet_col <-
#   function(data, col) {
#     coerce_col_to_factor(data, col)
#   }

filter_if_not_null_at <- function(data = NULL, col = NULL, value = NULL, invert = FALSE) {
  stopifnot(!is.null(data), is.data.frame(data))


  if(is.null(col))
    return(data)

  if(is.null(value))
    return(data)

  stopifnot(!(col %in% names(data)))

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
  stopifnot(!is.null(data), is.data.frame(data))
  stopifnot(!(col %in% names(data)))

  col_quo <- rlang::sym(col)
  data %>%
    dplyr::distinct(!!col_quo) %>%
    dplyr::arrange(!!col_quo) %>%
    dplyr::pull(!!col_quo)
}


filter_data_facet_at <-
  function(data = NULL,
           filter_facet = NULL,
           params = NULL,
           x_include = NULL,
           y_include = NULL,
           x_exclude = NULL,
           y_exclude = NULL,
           facet_main = NULL) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(filter_facet), is.logical(filter_facet))

    if (filter_facet) {
      stopifnot(!is.null(params), is.list(params))
      data <-
        data %>% filter_if_not_null_at("name_x", params$facet_main, invert = FALSE)
      data <-
        data %>% filter_if_not_null_at("name_x", params$x_include, invert = FALSE)
      data <-
        data %>% filter_if_not_null_at("name_y", params$y_include, invert = FALSE)
      data <-
        data %>% filter_if_not_null_at("name_x", params$x_exclude, invert = TRUE)
      data <-
        data %>% filter_if_not_null_at("name_y", params$y_exclude, invert = TRUE)

    } else {
      message("It's recommended to set `filter_facet = TRUE`.")
    }
    data
  }

validate_x_main <-
  function(data = NULL,
           filter_x = NULL,
           x = NULL,
           xs = NULL,
           x_main = NULL) {
    stopifnot(!is.null(data), is.data.frame(data))

    if (is.null(filter_x))
      stop("`filter_x` cannot be NULL.", call. = FALSE)

    if (filter_x) {
      stopifnot(!is.null(x))
      stopifnot(!is.null(xs))
      stopifnot(!is.null(x_main))
      if (!(x_main %in% xs))
        return(stop(sprintf(
          "`x_main` is not in %s.", paste(xs, collapse = ",")
        ), call. = FALSE))
      if (length(x_main) > 1)
        return(stop("`x_main` should be a singular value.", call. = FALSE))
    }
    data
  }

