
#' Create a of name pairs
#'
#' @description Create a grid of each comibination of the values in the input vector.
#' @details This function works similarly to \code{expand.grid()}.
#' @param xy_nms character (vector). Values to use to create a 'grid' of all
#' combinations of values.
#' @return data.frame.
#' @rdname create_xy_grid
#' @export
create_xy_grid <- function(xy_nms = NULL) {
  stopifnot(!is.null(xy_nms), is.character(xy_nms))

  x <- y <- xy <- i <- NULL

  xy_grid <-
    dplyr::bind_cols(x = xy_nms, y = xy_nms) %>%
    tidyr::complete(x, y)
  if (length(xy_nms) > 1) {
    xy_grid <- xy_grid %>% dplyr::filter(x != y)
  } else {
    xy_grid <- xy_grid %>% dplyr::mutate(y = paste0(x, "2"))
  }

  xy_grid %>%
    dplyr::mutate(xy = paste0(x, "_", y)) %>%
    dplyr::mutate(i = dplyr::row_number(xy)) %>%
    dplyr::select(i, xy, x, y)
}

#' Get information from a particular row in the 'xy grid'
#'
#' @description Filter the xy grid of each comibination of the values in the input vector.
#' @details To be used exclusively by \code{wrapper_func()}.
#' @param xy_grid data.frame. Output from \code{create_xy_grid()}.
#' @param xy_nms character (vector). Value in \code{xy_grid} to filter for.
#' This is not really necessary if \code{xy_grid} has a column corresponding to \code{i}.
#' @param i numeric. Used to extract a specific value in \code{xy_nms}.
#' @return list.
get_xy_info <-
  function(xy_grid = NULL,
           xy_nms = NULL,
           i = NULL) {
    stopifnot(!is.null(xy_grid), is.data.frame(xy_grid))
    stopifnot(!is.null(xy_nms), is.character(xy_nms))
    stopifnot(!is.null(i), is.numeric(i))

    x <- y <- xy <- NULL

    xy_i <- xy_nms[i]
    xy_i_row <- xy_grid %>% dplyr::filter(xy == xy_i)
    x_i <- xy_i_row %>%  dplyr::pull(x)
    y_i <- xy_i_row %>%  dplyr::pull(y)
    list(x = x_i, y = y_i, xy = xy_i)
  }

#' Pre-process data in \code{wrapper_func()}
#'
#' @description Pre-process \code{data} using \code{xy_info}.
#' @details To be used exclusively by \code{wrapper_func()}.
#' @param data data.frame. Output from \code{create_xy_grid()}.
#' @param xy_info list. Output from \code{get_xy_info()}.
#' @return data.frame.
preprocess_xy_data <- function(data = NULL, xy_info = NULL) {
  stopifnot(!is.null(data), is.data.frame(data))
  stopifnot(!is.null(xy_info), is.list(xy_info))
  name <- NULL
  data %>%
    dplyr::filter(name %in% c(xy_info$x, xy_info$y))
}

#' Post-process data in \code{wrapper_func()}
#'
#' @description Post-process \code{data} using \code{xy_info}.
#' @details To be used exclusively by \code{wrapper_func()}.
#' Some extra logic is needed in case the expected names do not appear in \code{data}
#' after \code{do.call(...)}. Output column names cannot be specified.
#' @inheritParams preprocess_xy_data
#' @param data data.frame. Output from \code{do.call(...)}.
#' @return data.frame.
postprocess_xy_data <- function(data = NULL, xy_info = NULL) {
  stopifnot(!is.null(data), is.data.frame(data))
  stopifnot(!is.null(xy_info), is.list(xy_info))

  x <- y <- name_x <- name_y <- name_xy <- NULL
  out <-
    data %>%
    dplyr::mutate(name_x = xy_info$x, name_y = xy_info$y) %>%
    dplyr::mutate(name_xy = paste0(name_x, "_", name_y))

  if (length(setdiff(c(xy_info$x, xy_info$y), names(data))) == 0) {
    out <-
      out %>%
      dplyr::rename(x = !!rlang::sym(xy_info$x),
                    y = !!rlang::sym(xy_info$y)) %>%
      dplyr::select(name_x, name_y, name_xy, x, y, dplyr::everything())
  } else {
    out <-
      out %>% dplyr::select(name_x, name_y, name_xy, dplyr::everything())
  }
  out
}

#' 'Engine' for '_by2' functions
#'
#' @description Apply a function to \code{data} in pairs (over a \code{facet} column).
#' @details Should be replaced by a \code{purrr} function in the future.
#' Must be careful not to pass \code{NULL} in \code{...}.
#' @inheritParams create_xy_grid
#' @inheritParams get_xy_info
#' @param data data.frame. Data to apply a function over.
#' @param func function. Wrapped by \code{do.call()}.
#' @param ... dots. Additional parameters to pass to \code{func}.
#' @return data.frame.
wrapper_func <-
  function(data = NULL,
           func = NULL,
           xy_grid = NULL,
           xy_nms = NULL,
           ...) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(func), is.function(func))
    stopifnot(!is.null(xy_grid), is.data.frame(xy_grid))
    stopifnot(!is.null(xy_nms), is.character(xy_nms))

    i <- 1
    while (i <= length(xy_nms)) {
      xy_i_info <- get_xy_info(xy_grid, xy_nms, i)
      data_i_preproc <- preprocess_xy_data(data, xy_i_info)
      data_i_proc <- do.call(func, list(data_i_preproc, ...))
      data_i_postproc <- postprocess_xy_data(data_i_proc, xy_i_info)
      if (i == 1) {
        out <- data_i_postproc
      } else {
        out <- dplyr::bind_rows(out, data_i_postproc)
      }
      i <- i + 1
    }
    out
  }

#' Post-process data in \code{wrapper_func()}
#'
#' @description Add dummy columns to \code{data}, if necessary.
#' @details To be used exclusively by \code{wrapper_func()}.
#' @inheritParams preprocess_xy_data
#' @param data data.frame.
#' @param num_cols_expect numeric.
#' @param add logical. whether or not to try to add a dummy column. Only
#' applied if the difference between the expected and actual number of columns is 1
#' (where the expected is greater than the actual).
#' @return data.frame.
append_dummy_cols <- function(data = NULL, num_cols_expect = NULL, add = TRUE) {
  stopifnot(!is.null(data), is.data.frame(data))

  stopifnot(!is.null(num_cols_expect), is.numeric(num_cols_expect))

  if(ncol(data) < num_cols_expect) {
    warning(sprintf("Expected %.0f columns but see only %.0f.", num_cols_expect, ncol(data)))
    num_cols_diff <- num_cols_expect - ncol(data)
    if(num_cols_diff == 1) {
      if(add) {
        name_last <- names(data)[ncol(data)]
        out <- dplyr::bind_cols(data, data[, ncol(data)])
        names(out)[ncol(out)] <- paste0(name_last, "2")
        warning(sprintf("Added %.0f dummy column(s).", num_cols_diff))
      }
    } else {
      stop("Don't know how to add dummy columns.")
    }
  }
  out
}



