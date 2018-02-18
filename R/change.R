

#' Compute change
#'
#' @description Compute change in word usage over time.
#' @details None.
#' @inheritParams visualize_time
#' @param colname_timebin character. Name of column in \code{data} specifying temporal period
#' to use to compute change.
#' @param colname_word character. Name of column in \code{data} corresponding to n-gram.
#' @param timefloor character. Passed directly to \code{lubridate::floor_date()} as\code{unit} parameter.
#' @param top_pct numeric. Number between 0 and 1. Default is provided.
#' @return data.frame
#' @export
#' @importFrom dplyr mutate group_by summarise ungroup filter arrange
#' @importFrom lubridate floor_date
#' @importFrom stats quantile glm p.adjust
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom broom tidy
#' @seealso https://www.tidytextmining.com/twitter.html#changes-in-word-use
compute_change <-
  function(data = NULL,
           colname_timebin = NULL,
           colname_word = "word",
           timefloor = c("second", "year", "hour", "day", "week", "year"),
           top_pct = 0.05) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    if (is.null(colname_timebin))
      stop("`colname_timebin` cannot be NULL.", call. = FALSE)
    timefloor <- match.arg(timefloor)
    if ((top_pct < 0) |
        (top_pct > 1))
      stop("`top_pct` is not valid.", call. = FALSE)

    n <-
      time_total <-
      word_total <-
      time_floor <- models <- term <- p.value <- adjusted_p_value <- NULL

    colname_word_quo <- rlang::sym(colname_word)
    colname_timebin_quo <- rlang::sym(colname_timebin)

    data_bytime <-
      data %>%
      dplyr::mutate(time_floor = lubridate::floor_date(!!colname_timebin_quo, unit = timefloor)) %>%
      dplyr::group_by(time_floor, !!colname_word_quo) %>%
      dplyr::summarise(n = n()) %>%
      # dplyr::summarise(n = sum(!is.na(time_floor))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(time_floor) %>%
      dplyr::mutate(time_total = sum(n)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!colname_word_quo) %>%
      dplyr::mutate(word_total = sum(n)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(word_total >= stats::quantile(word_total, 1 - top_pct)) %>%
      dplyr::arrange(dplyr::desc(word_total))

    # NOTE: Not sure if tidyr::nest() will work with this syntax!
    data_bytime_models <-
      data_bytime %>%
      tidyr::nest(-!!colname_word_quo) %>%
      dplyr::mutate(models =
                      purrr::map(data, ~ stats::glm(
                        cbind(n, time_total) ~ time_floor, ., family = "binomial"
                      )))

    out <-
      data_bytime_models %>%
      tidyr::unnest(purrr::map(models, broom::tidy)) %>%
      dplyr::filter(term == "time_floor") %>%
      dplyr::mutate(adjusted_p_value = stats::p.adjust(p.value)) %>%
      dplyr::arrange(adjusted_p_value)
    out
  }
