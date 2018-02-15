

#' Compute n-gram frequency
#'
#' @description Compute the frequency of n-grams.
#' @details Inspired by \href{https://www.tidytextmining.com/}{\emph{Text Mining with R}}.
#' @inheritParams visualize_time
#' @param colname_cnt character. Name of column in \code{data} to use for count. Default: 'word'.
#' @return data.frame.
#' @export
#' @importFrom rlang sym !! :=
#' @importFrom dplyr n count mutate arrange desc
compute_freqs <- function(data = NULL, colname_cnt = "word") {

    if(is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)

    freq <- n <- total <- NULL

    colname_cnt_quo <- rlang::sym(colname_cnt)

    out <- dplyr::mutate(data, total = dplyr::n())
    out <- dplyr::group_by(out, !!colname_cnt_quo)
    out <- dplyr::mutate(out, n = dplyr::n())
    out <- dplyr::ungroup(out)
    out <- dplyr::mutate(out, freq = n / total)
    out <- dplyr::arrange(out, dplyr::desc(freq))
    out
  }

#' @inheritParams compute_freqs
#' @param colname_multi character. Name of column in \code{data} corresponding to group
#' by which count is made.
#' @rdname compute_freqs
#' @importFrom dplyr left_join rename
compute_freqs_multi <-
  function(data = NULL,
           colname_cnt = "word",
           colname_multi = NULL) {

    if(is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)
    if(is.null(colname_multi)) stop("`colname_multi` cannot be NULL.", call. = FALSE)

    freq <- n <- total <- NULL

    colname_cnt_quo <- rlang::sym(colname_cnt)
    colname_multi_quo <- rlang::sym(colname_multi)

    ngrams_cnt_1 <- dplyr::count(data, !!colname_multi_quo)
    ngrams_cnt_2 <- dplyr::count(data, !!colname_multi_quo, !!colname_cnt_quo)

    ngrams_joined <-
      dplyr::left_join(ngrams_cnt_2, dplyr::rename(ngrams_cnt_1, total = n), by = colname_multi)
    out <- dplyr::mutate(ngrams_joined, freq = n / total)
    out <- dplyr::arrange(out, dplyr::desc(freq))
    out
  }