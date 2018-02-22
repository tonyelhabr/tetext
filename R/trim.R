
#' Compute time difference
#'
#' @description Computes the difference in time between two Date-time values
#' in a specified time 'unit'.
#' @details Called in \code{compute_timefilter_multi_at()}.
#' @param date_start Date-time (or character that can be converted to Date-time).
#' @param date_end Date-time (or character that can be converted to Date-time).
#' @param type character. 'unit' to use to calculate time difference.
#' One of 'year', 'month', 'day', or 'hour'.
#' @return numeric.
#' @rdname compute_time_elapsed
#' @export
#' @seealso https://stackoverflow.com/
compute_time_elapsed <-
  function(date_start = NULL,
           date_end = NULL,
           type = c("year", "month", "day", "hour")) {
    if (is.null(date_start))
      stop("`date_start` cannot be NULL.", call. = FALSE)
    if (is.null(date_end))
      stop("`date_end` cannot be NULL.", call. = FALSE)
    if (is.null(type))
      stop("`type` cannot be NULL.", call. = FALSE)
    type <- match.arg(type)

    if (type == "year" | type == "month") {
      date_start <- as.POSIXlt(date_start)
      date_end <- as.POSIXlt(date_end)
      if (type == "year") {
        out <- (date_end$year - date_start$year) - 1
      } else if (type == "month") {
        out <-
          12 * (date_end$year - date_start$year) + (date_end$mon - date_start$mon) - 1
      }
    } else if (type == "day" | type == "hour") {
      out <-
        (difftime(date_end, date_start, units = type) - 1) %>%
        round(0) %>%
        as.numeric()
    }
    out
  }


#' @param data data.frame.
#' @param timebin character. Name of column in \code{data} to use for time axis.
#' @param multi character. Name of column in \code{data} used for facetting.
#' @return list
#' @rdname compute_timefilter
#' @export
compute_timefilter_multi_at <-
  function(data = NULL,
           timebin = NULL,
           multi = NULL) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    if (is.null(timebin))
      stop("`timebin` cannot be NULL.", call. = FALSE)
    if (is.null(multi))
      stop("`multi` cannot be NULL.", call. = FALSE)

    timebin_quo <- rlang::sym(timebin)
    multi_quo <- rlang::sym(multi)

    date_start <-
      date_end <-
      yyyy_elapsed <- mm_elapsed <- dd_elpased <- hh_elapsed <- NULL

    data_proc <-
      data %>%
      dplyr::group_by(!!multi_quo) %>%
      dplyr::arrange(!!timebin_quo) %>%
      dplyr::mutate(date_start = dplyr::first(!!timebin_quo),
                    date_end = dplyr::last(!!timebin_quo)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(!!multi_quo, date_start, date_end) %>%
      dplyr::mutate(
        yyyy_elapsed = compute_time_elapsed(date_start, date_end, "year"),
        mm_elapsed = compute_time_elapsed(date_start, date_end, "month"),
        dd_elapsed = compute_time_elapsed(date_start, date_end, "day"),
        hh_elapsed = compute_time_elapsed(date_start, date_end, "hour")
      )

    out <-
      list(
        data = data_proc,
        date_start = max(data_proc$date_start),
        date_end = min(data_proc$date_end)
      )
    out
  }


#' Add time-related columns
#'
#' @description Adds useful columns for subsequent time calculations and filtering.
#' @details
#' Calls \code{compute_timefilter_multi_at()}. (Adds a dummy 'multi' column if none is specified.)
#' Adds columns for years, months, days, and hours elapsed.
#' (Calls in \code{compute_time_elapsed()} multiple times.)
#' Returns list, where \code{data} is augmented data.frame,
#' \code{date_start} is the very LAST Date-time value, and \code{date_end}
#' is the very FIRST Date-time value. The \code{date_start} and \code{date_end}
#' values are defined in this manner such that all data for each 'multi' variable
#' fits in a singular frame (i.e. there are not 'gaps' where data exists
#' for one 'multi' variable and not all others). This determination assumes
#' that the data is continuous.
#' @inheritParams compute_timefilter_multi_at
#' @return list
#' @rdname compute_timefilter
#' @export
compute_timefilter_at <-
  function(data,
           timebin,
           multi) {
    if(missing(multi)) {
      add_dummy <- TRUE
      data$multi <- "dummy"
      multi <- "multi"
    } else {
      add_dummy <- FALSE
    }

    out <- compute_timefilter_multi_at(data = data, timebin = timebin, multi = multi)
    if(add_dummy) {
      out$data <- out$data %>% dplyr::select(-dplyr::one_of(c(multi)))
    }
    out
  }

#' @rdname compute_timefilter
#' @export
compute_timefilter <- compute_timefilter_at

#' Trim data.frame by time
#'
#' @description Trim a data.frame to 'fit' within specified 'start' and 'end' Date-time values.
#' @details Should be used to a trim data.frame appropriately/dynamically
#' given an unknown data set where visualization across a single, appropriate time period
#' is desired. (The unkown data set may have different 'max' and 'min' times
#' for each 'multi' column.
#' @param data data.frame.
#' @param timebin character. Name of columin in \code{data} to use for time filtering.
#' @param start,end Date-time. If either is missing, then code{compute_timefilter_at()} is called.
#' @param ... dots. Additional parameters to pass to \code{compute_timefilter_at()}
#' in the case that start and end are not specified.
#' @return data.frame.
#' @rdname trim_bytime
#' @export
trim_bytime_at <-
  function(data = NULL,
           timebin = NULL,
           start,
           end,
           ...) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    if (is.null(timebin))
      stop("`timebin` cannot be NULL.", call. = FALSE)

    if(missing(start) | missing(end)) {
      dots <- list(...)
      # if(!("multi") %in% names(dots)) stop("`multi` must be specified." call. = FALSE)
      data_proc <- compute_timefilter_at(data = data, timebin = timebin, ...)
      message("Computing max and min dates.")
      start <- data_proc$date_start
      end <- data_proc$date_end
      # data <- data_proc$data
    }

    timebin_quo <- rlang::sym(timebin)

    out <-
      data %>%
      dplyr::filter(!!timebin_quo <= end, !!timebin_quo >= start)
    out
  }

#' @rdname trim_bytime
#' @export
trim_bytime <- trim_bytime_at

