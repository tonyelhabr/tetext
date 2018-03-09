

compute_freqs_at_old <- function(data = NULL, token = NULL) {
  stopifnot(!is.null(data), is.data.frame(data))
  stopifnot(!is.null(token), is.character(token))

  freq <- n <- NULL

  token_quo <- rlang::sym(token)

  data %>%
    dplyr::group_by(!!token_quo) %>%
    dplyr::mutate(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!token_quo) %>%
    dplyr::summarise(freq = sum(n) / n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(freq))
}

compute_freqs_old <-
  function(..., token) {
    stopifnot(!missing(token))
    token <- rlang::quo_text(rlang::enquo(token))
    compute_freqs_at_old(..., token = token)
  }

#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @inheritParams compute_freqs
#' @return data.frame.
#' @rdname compute_freqs
#' @export
compute_freqs_facet_at <-
  function(data = NULL,
           token = NULL,
           facet = NULL) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(facet), is.character(facet))

    freq <- n <- total <- NULL

    token_quo <- rlang::sym(token)
    facet_quo <- rlang::sym(facet)

    ngrams_cnt_1 <- data %>% dplyr::count(!!facet_quo)
    ngrams_cnt_2 <- data %>% dplyr::count(!!facet_quo, !!token_quo)

    ngrams_cnt_2 %>%
      dplyr::left_join(ngrams_cnt_1 %>% dplyr::rename(total = n), by = facet) %>%
      dplyr::mutate(freq = n / total) %>%
      dplyr::arrange(!!facet_quo, dplyr::desc(freq))
  }

#' @rdname compute_freqs
#' @export
compute_freqs_facet <-
  function(..., token, facet) {
    stopifnot(!missing(token))
    stopifnot(!missing(facet))
    token <- rlang::quo_text(rlang::enquo(token))
    facet <- rlang::quo_text(rlang::enquo(facet))
    compute_freqs_at(..., token = token, facet = facet)
  }

#' Compute token frequency
#'
#' @description Compute the frequency of tokens.
#' @details None.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param token bare for NSE; character for SE. Name of column in \code{data} to use for count.
#' @param facet character. Name of column in \code{data} to use for facetting.
#' It is set to NULL by default in the non-\code{facet} function even though it is not necessary.
#' If specified with the non-\code{facet} version of the function,
#' then the function acts just like the \code{_facet} version.
#' @return data.frame.
#' @rdname compute_freqs
#' @export
compute_freqs_at <-
  function(data = NULL, token = NULL, facet = NULL) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))

    if (is.null(facet)) {
      add_dummy <- TRUE
      data <- data %>% dplyr::mutate(`.facet` = "dummy")
      facet <- ".facet"
    } else {
      add_dummy <- FALSE
    }

    out <-
      data %>%
      compute_freqs_facet_at(token = token, facet = facet)
    if(add_dummy) {
      out <- out %>% dplyr::select(-dplyr::one_of(c(facet)))
    }
    out
  }

#' @rdname compute_freqs
#' @export
compute_freqs <-
  function(..., token, facet) {
    stopifnot(!missing(token))
    token <- rlang::quo_text(rlang::enquo(token))
    if(missing(facet)) {
      facet <- NULL
    } else {
      facet <- rlang::quo_text(rlang::enquo(facet))
    }
    compute_freqs_at(..., token = token, facet = facet)
  }

#' Compute token frequencies
#'
#' @description Manipulates output from \code{compute_freqs_facet_at()}
#' for \code{compute_freqs_mult_by2} function.
#' @details To be used by \code{_by2} function only.
#' @inheritParams compute_freqs_facet
#' @rdname compute_freqs
#' @export
compute_freqs_facet_wide_at <-
  function(...,
           token = NULL,
           facet = NULL) {

    # stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(facet), is.character(facet))

    data_proc <-
      compute_freqs_facet_at(
        # data = data,
        ...,
        token = token,
        facet = facet
      )
    token_quo <- rlang::sym(token)
    facet_quo <- rlang::sym(facet)

    freq <- NULL

    out <-
      data_proc %>%
      dplyr::select(!!facet_quo, !!token_quo, freq) %>%
      tidyr::spread(!!facet_quo, freq)

    if(ncol(out) == 2) {
      out <- append_dummy_cols(out, num_cols_expect = 3)
    }
    out
  }

