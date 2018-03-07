

# compute_sent_poisson_at <-
#   function(data = NULL,
#            sentiment = "sentiment",
#            cnt = "cnt",
#            total = "total") {
#     stopifnot(!is.null(data), is.data.frame(data))
#       stop("`data` must not be NULL.", call. = FALSE)
#     if (is.null(sentiment))
#       stop("`sentiment` must not be NULL.", call. = FALSE)
#     if (is.null(cnt))
#       stop("`cnt` must not be NULL.", call. = FALSE)
#     if (is.null(total))
#       stop("`total` must not be NULL.", call. = FALSE)
#
#     sentiment_quo <- rlang::sym(sentiment)
#     cnt_quo <- rlang::sym(cnt)
#     total_quo <- rlang::sym(total)
#
#     out <-
#       data %>%
#       dplyr::rename(cnt = !!cnt_quo, total = !!total_quo) %>%
#       dplyr::group_by(!!sentiment_quo) %>%
#       dplyr::do(broom::tidy(stats::poisson.test(.$cnt, .$total))) %>%
#       dplyr::ungroup()
#
#     out
#
#   }


