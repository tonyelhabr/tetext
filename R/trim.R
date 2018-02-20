


# Modified from a SO answer.
#' Title
#'
#' @param date_start
#' @param date_end
#' @param type
#'
#' @return
#' @export
#'
#' @examples
compute_time_elapsed <-
  function(date_start = NULL,
           date_end = NULL,
           type = c("year", "month" "day", "hour")) {
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

#' Title
#'
#' @param data
#' @param timebin
#' @param multi
#'
#' @return
#' @export
#'
#' @examples
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
      dplyr::group_by(!!multi) %>%
      dplyr::arrange(!!timebin) %>%
      dplyr::mutate(date_start = first(!!timebin),
                    date_end = last(!!timebin)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(name, date_start, date_end) %>%
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

# This is "original" processing needed to trim data appropriately/dynamically
# given an unknown data set.
#' Title
#'
#' @param data
#' @param timebin
#' @param start
#' @param end
#'
#' @return
#' @export
#'
#' @examples
trim_data_bytime_at <-
  function(data = NULL,
           timebin = NULL,
           start = NULL,
           end = NULL) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    if (is.null(timebin))
      stop("`timebin` cannot be NULL.", call. = FALSE)
    if (is.null(start))
      stop("`start` cannot be NULL.", call. = FALSE)
    if (is.null(end))
      stop("`end` cannot be NULL.", call. = FALSE)
    timebin_quo <- rlang::sym(timebin)

    out <-
      data %>%
      dplyr::filter(timebin_quo <= end, timebin_quo >= start)
    out
  }

