


get_color_hex_inverse <- function(color) {
  grDevices::rgb(t(255 - grDevices::col2rgb(color)), max = 255)
}

generate_named_dual_colors <-
  function(color_main,
           lab_main,
           lab_other,
           color_default = "grey50") {
    if (!is.null(color_main) & !is.null(lab_main)) {
      out <- stats::setNames(c(color_main, get_color_hex_inverse(color_main)),
                      c(lab_main, lab_other))
    } else {
      out <- c(color_default, get_color_hex_inverse(color_default))
    }
    out
  }

invert_pct <- function(num) {
  if (num < 0.5) {
    # message(sprintf("Inverting %f to %f.", num, 1 - num))
    num <- 1 - num
  }
  num
}

validate_range <- function(x, max = 1, min = 0) {
  if (x > max) {
    stop("`x` must not be greater than ", max, ".", call. = FALSE)
  }
  if (x < min) {
    stop("`x` must not be less than ", min, ".", call. = FALSE)
  }
  x
}

filter_num_top_at <-
  function(data = NULL,
           col = NULL,
           num_top = NULL,
           max = nrow(data),
           min = 0,
           desc = TRUE,
           abs = FALSE,
           keep_rank_col = FALSE) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(col))
    stopifnot(!is.null(num_top))
    num_top <- validate_range(x = num_top, max = max, min = min)

    rank <- NULL
    rank <- ".rank"
    rank_quo <- rlang::sym(".rank")
    col_quo <- rlang::sym(col)

    out <- data

    if (desc) {
      if (abs) {
        out <-
          out %>%
          dplyr::mutate(!!rank_quo := dplyr::row_number(dplyr::desc(abs(!!col_quo))))
      } else {
        out <-
          out %>%
          dplyr::mutate(!!rank_quo := dplyr::row_number(dplyr::desc(!!col_quo)))
      }
    } else {
      if (abs) {
        out <-
          out %>%
          dplyr::mutate(!!rank_quo := dplyr::row_number((abs(!!col_quo))))
      } else {
        out <-
          out %>%
          dplyr::mutate(!!rank_quo := dplyr::row_number(!!col_quo))
      }
    }

    if (num_top >= 1) {
      out <-
        out %>%
        dplyr::filter(!!rank_quo <= num_top)
    } else {
      # num_top <- (num_top) * (nrow(data))
      num_top <- invert_pct(num_top)
      out <-
        out %>%
        # dplyr::arrange(dplyr::desc(!!col_quo)) %>%
        dplyr::filter(!!col_quo >= stats::quantile(!!col_quo, num_top, na.rm = TRUE))
    }
    if (!keep_rank_col) {
      out <- out %>% dplyr::select(-dplyr::matches(rank))
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

filter_if_not_null_at <-
  function(data = NULL,
           col = NULL,
           value = NULL,
           invert = FALSE) {
    stopifnot(!is.null(data), is.data.frame(data))


    if (is.null(col))
      return(data)

    if (is.null(value))
      return(data)

    stopifnot((col %in% names(data)))

    col_quo <- rlang::sym(col)
    if (!invert) {
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
  stopifnot((col %in% names(data)))

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


create_name_xy_facet_lab <- function(data = NULL) {
  name_xy <- name_x <- name_y <- NULL
  data %>%
    dplyr::mutate(name_xy = paste0(name_x, " vs. ", name_y))
}

create_logratio_dir_at <-
  function(data = NULL,
           cols_group = NULL,
           num_top = NULL) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(cols_group), is.list(cols_group))

    logratio_dir <- logratio <- name_x <- name_y <- NULL

    cols_group_quo <- rlang::syms(cols_group)
    data_proc <-
      data %>%
      dplyr::mutate(logratio_dir =
                      ifelse(
                        logratio > 0,
                        ifelse(name_x < name_y, TRUE, FALSE),
                        ifelse(name_x < name_y, FALSE, TRUE)
                      )) %>%
      dplyr::group_by(!!!cols_group_quo) %>%
      # dplyr::group_by_at(cols_group) %>%
      filter_num_top_at("logratio", num_top, abs = TRUE) %>%
      dplyr::ungroup()
  }


process_logratio_dir_at <-
  function(data = NULL,
           token = NULL,
           color = NULL,
           facet = NULL,
           facet_main = NULL,
           lab_other = NULL) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(color), is.character(color))
    stopifnot(!is.null(facet), is.character(facet))
    stopifnot(!is.null(facet_main), is.character(facet_main))
    stopifnot(!is.null(lab_other), is.character(lab_other))

    token_quo <- rlang::sym(token)
    color_quo <- rlang::sym(color)
    facet_quo <- rlang::sym(facet)

    logratio_dir <- logratio <- name_x <- name_xy <- NULL

    data_proc <-
      data %>%
      dplyr::mutate(!!color_quo :=
                      ifelse(
                        logratio_dir,
                        ifelse(name_x > lab_other, lab_other, name_x),
                        ifelse(name_x > lab_other, name_x, lab_other)
                      )) %>%
      dplyr::mutate(!!color_quo := factor(!!color_quo, levels = c(facet_main, lab_other))) %>%
      dplyr::mutate(!!token_quo := reorder_within(!!token_quo, dplyr::desc(logratio), name_xy))
  }
