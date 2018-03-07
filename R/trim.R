
#' Compute time difference
#'
#' @description Computes the difference in time between two Date-time values
#' in a specified time 'unit'.
#' @details Called in \code{compute_timefilter_facet_at()}.
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
    stopifnot(!is.null(date_start))
    stopifnot(!is.null(date_end))
    stopifnot(!is.null(type))
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
#' @param timebin bare for NSE; character for SE. Name of column in \code{data} to use for time axis.
#' @param facet bare for NSE; character for SE. Name of column in \code{data} used for facetting.
#' @return list.
#' @rdname compute_timefilter
#' @export
compute_timefilter_facet_at <-
  function(data = NULL,
           timebin = NULL,
           facet = NULL) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(timebin), is.character(timebin))
    stopifnot(!is.null(facet), is.character(facet))

    timebin_quo <- rlang::sym(timebin)
    facet_quo <- rlang::sym(facet)

    date_start <-
      date_end <-
      yyyy_elapsed <- mm_elapsed <- dd_elpased <- hh_elapsed <- NULL

    data_proc <-
      data %>%
      dplyr::group_by(!!facet_quo) %>%
      dplyr::arrange(!!timebin_quo) %>%
      dplyr::mutate(date_start = dplyr::first(!!timebin_quo),
                    date_end = dplyr::last(!!timebin_quo)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(!!facet_quo, date_start, date_end) %>%
      dplyr::mutate(
        yyyy_elapsed = compute_time_elapsed(date_start, date_end, "year"),
        mm_elapsed = compute_time_elapsed(date_start, date_end, "month"),
        dd_elapsed = compute_time_elapsed(date_start, date_end, "day"),
        hh_elapsed = compute_time_elapsed(date_start, date_end, "hour")
      )

    list(
      data = data_proc,
      date_start = max(data_proc$date_start),
      date_end = min(data_proc$date_end)
    )
  }

#' @rdname compute_timefilter
#' @export
compute_timefilter_facet <-
  function(..., timebin, facet) {
    stopifnot(!missing(timebin))
    stopifnot(!missing(facet))
    timebin <- rlang::quo_text(rlang::enquo(timebin))
    facet <- rlang::quo_text(rlang::enquo(facet))
    compute_timefilter_facet_at(..., timebin = timebin, facet = facet)
  }

#' Add time-related columns
#'
#' @description Adds useful columns for subsequent time calculations and filtering.
#' @details
#' Calls \code{compute_timefilter_facet_at()}. (Adds a dummy \code{facet} column if none is specified.)
#' Adds columns for years, months, days, and hours elapsed.
#' (Calls in \code{compute_time_elapsed()} facetple times.)
#' Returns list, where \code{data} is augmented data.frame,
#' \code{date_start} is the very LAST Date-time value, and \code{date_end}
#' is the very FIRST Date-time value. The \code{date_start} and \code{date_end}
#' values are defined in this manner such that all data for each \code{facet} variable
#' fits in a singular frame (i.e. there are not 'gaps' where data exists
#' for one \code{facet} variable and not all others). This determination assumes
#' that the data is continuous.
#' @inheritParams compute_timefilter_facet_at
#' @inheritParams compute_freqs
#' @return list.
#' @rdname compute_timefilter
#' @export
compute_timefilter_at <-
  function(data = NULL,
           timebin = NULL,
           facet = NULL) {
    # NOTE: Need to explicitly include `timebin` because this function might
    # be called directly from `trim_bytime_at()`, which does not convert
    # `timebin` to a quosure.
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(timebin), is.character(timebin))

    if (is.null(facet)) {
      add_dummy <- TRUE
      data <- data %>% dplyr::mutate(`.facet` = "dummy")
      facet <- ".facet"
    } else {
      add_dummy <- FALSE
    }

    out <-
      data %>%
      compute_timefilter_facet_at(
        timebin = timebin,
        facet = facet
      )

    if(add_dummy) {
      out$data <- out$data %>% dplyr::select(-dplyr::one_of(c(facet)))
    }
    out
  }

#' @rdname compute_timefilter
#' @export
compute_timefilter <-
  function(..., timebin, facet) {
    stopifnot(!missing(timebin))
    timebin <- rlang::quo_text(rlang::enquo(timebin))
    if(missing(facet)) {
      facet <- NULL
    } else {
      facet <- rlang::quo_text(rlang::enquo(facet))
    }
    compute_timefilter_at(..., timebin = timebin, facet = facet)
  }

#' Trim data.frame by time
#'
#' @description Trim a data.frame to 'fit' within specified 'start' and 'end' Date-time values.
#' @details Should be used to a trim data.frame appropriately/dynamically
#' given an unknown data set where visualization across a single, appropriate time period
#' is desired. (The unkown data set may have different 'max' and 'min' times
#' for each \code{facet} column.
#' @inheritParams compute_timefilter_facet_at
#' @param data data.frame.
#' @param timebin bare for NSE; character for SE. Name of columin in \code{data} to use for time filtering.
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
           facet) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(timebin), is.character(timebin))

    if(missing(start) | missing(end)) {
      if(missing(facet)) {
        facet <- NULL
      }
      data_proc <- compute_timefilter_at(data = data, timebin = timebin, facet = facet)
      message("Computing max and min dates.")
      start <- data_proc$date_start
      end <- data_proc$date_end
      # data <- data_proc$data
    }

    timebin_quo <- rlang::sym(timebin)

    data %>%
      dplyr::filter(!!timebin_quo <= end, !!timebin_quo >= start)
  }

#' @rdname trim_bytime
#' @export
trim_bytime <-
  function(..., timebin, facet) {
  stopifnot(!missing(timebin))
  timebin <- rlang::quo_text(rlang::enquo(timebin))
  if(missing(facet)) {
    facet <- NULL
  } else {
    facet <- rlang::quo_text(rlang::enquo(facet))
  }
  trim_bytime_at(..., timebin = timebin, facet = facet)
}

