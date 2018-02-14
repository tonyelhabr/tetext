

#' Compute n-gram frequency
#'
#' @description Compute the frequency of n-grams.
#' @details Heavily influence by the _Tidy Text Mining with R_ book.
#' @param data data.frame.
#' @param colname_x character. Input column name of 'facetting' variable. Probably something like 'name'.
#' @param colname_cnt character. Input column name of value to count. Default: 'word'
#' @return data.frame.
#' @export
#' @importFrom rlang sym !! :=
#' @importFrom dplyr count left_join rename mutate arrange desc
get_ngrams_freqs_byx <-
  function(data = NULL,
           colname_x = NULL,
           colname_cnt = "word") {

    if(is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)
    if(is.null(colname_x)) stop("`colname_x` cannot be NULL.", call. = FALSE)
    if(is.null(colname_cnt)) stop("`colname_cnt` cannot be NULL.", call. = FALSE)

    freq <- n <- total <- NULL

    colname_x_quo <- rlang::sym(colname_x)
    colname_cnt_quo <- rlang::sym(colname_cnt)

    ngrams_cnt_1 <- dplyr::count(data, !!colname_x_quo)
    ngrams_cnt_2 <- dplyr::count(data, !!colname_x_quo, !!colname_cnt_quo)

    ngrams_joined <- dplyr::left_join(ngrams_cnt_2, dplyr::rename(ngrams_cnt_1, total = n), by = "name")
    out <- dplyr::mutate(ngrams_joined, freq := n / total)
    out <- dplyr::arrange(out, dplyr::desc(freq))
    out
  }
