


#' Clean (and augment) \code{rtweet} data.frame
#'
#' @description Munge an \code{rtweet} data.frame (according to personal preferences)
#' for subsequent analysis.
#' @details Converts nested lists to character(s).
#' Adds a \code{timestamp} column that is derived from the \code{created_at}
#' column. Also, adds a \code{time} column that represents the hour in the day of \code{created_at}.
#' @param data data.frame (created using \code{rtweet} package).
#' @param multi character. Name of column in \code{data} used for facetting.
#' Does not need to be specified.
#' Included in \code{cols} if specified.
#' @param trim logical. Indicates whether or not to select only certain columns
#' (and drop the others).
#' @param cols character (vector). Name(s) of column(s) in \code{data} to keep.
#' Only relevant if \code{trim = TRUE}.
#' @param timezone character. Passed directly to \code{lubridate::with_tz()} as \code{tzone} parameter.
#'
#' @return data.frame
#' @rdname clean_tweets
#' @export
#' @seealso \url{https://juliasilge.com/blog/ten-thousand-data/}.
#' \url{https://buzzfeednews.github.io/2018-01-trump-twitter-wars/}.
clean_tweets_at <-
  function(data = NULL,
           multi,
           trim = TRUE,
           cols =
             c(
               "status_id",
               "created_at",
               "user_id",
               "screen_name",
               "text",
               "display_text_width",
               "reply_to_status_id",
               "is_quote",
               "is_retweet",
               "favorite_count",
               "retweet_count",
               "hashtags",
               "symbols",
               "urls_url",
               "urls_expanded_url",
               "media_expanded_url",
               "ext_media_expanded_url"
             ),
           timezone = "America/Chicago") {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)

    if (trim) {
      if (!missing(multi)) {
        data <-
          data %>%
          dplyr::select(dplyr::one_of(c(multi, cols)))
      } else {
        data <-
          data %>%
          dplyr::select(dplyr::one_of(c(cols)))
      }
    }

    created_at <- timestamp <- time <- NULL

    out <-
      data %>%
      dplyr::mutate_if(is.list, dplyr::funs(as.character)) %>%
      dplyr::mutate(timestamp = lubridate::ymd_hms(created_at)) %>%
      dplyr::mutate(timestamp = lubridate::with_tz(timestamp, timezone)) %>%
      dplyr::mutate(time = lubridate::hour(timestamp) + lubridate::minute(timestamp) / 60)

    out
  }

#' @rdname clean_tweets
#' @export
clean_tweets <- clean_tweets_at

#' Augment \code{rtweet} data.frame
#'
#' @description Add columns to \code{rtweet} data.frame for 'kind' data.
#' @details The columns added include
#' 'hashtag', 'link', 'rt', 'quote', and 'reply', all of which are logicals.
#' A 'type' column is also added--it is a factored version of 'rt', 'quote', and 'reply'.
#' @inheritParams clean_tweets
#' @return data.frame
#' @rdname add_tweet_kind_cols
#' @export

#' @seealso \url{https://juliasilge.com/blog/ten-thousand-data/}.
add_tweet_kind_cols <- function(data = NULL) {
  if (is.null(data))
    stop("`data` cannot be NULL.", call. = FALSE)

  hashtag <-
    hashtags <-
    link <-
    media_expanded_url <-
    ext_media_expanded_url <-
    rt <-
    is_retweet <-
    quote <- is_quote <- reply <- reply_to_status_id <- type <- NULL

  out <-
    data %>%
    dplyr::mutate(
      hashtag = dplyr::if_else(!is.na(hashtags), 1, 0),
      link = dplyr::if_else(
        !is.na(media_expanded_url) & !is.na(ext_media_expanded_url),
        1,
        0
      ),
      rt = dplyr::if_else(is_retweet, 1, 0),
      quote = dplyr::if_else(is_quote, 1, 0),
      reply = dplyr::if_else(!is.na(reply_to_status_id), 1, 0)
    ) %>%
    dplyr::mutate(
      type =
        dplyr::case_when(
          rt == TRUE ~ "RT",
          reply == TRUE ~ "reply",
          quote == TRUE ~ "quote",
          TRUE ~ "original"
        )
    )
  out
}

#' Get helper list for tidying
#'
#' @description Return a list of regular expression to used to 'tidify' a
#' \code{rtweet} data.frame (i.e. with the \code{tidify_to_} functions).
#' @details This function is primarily a 'convenience' function for saving
#' regular expressions that are good to use with a \code{rtweet} data.frame.
#' @param rgx_unnest character. Regular expression.
#' @param rgx_pattern character. Regular expression.
#' @param rgx_ignore_custom character. Regular expression.
#' @return list
#' @export get_tweet_rgx_tidiers
#' @seealso \url{https://www.tidytextmining.com/twitter.html}.
get_tweet_rgx_tidiers <-
  function(rgx_unnest = "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))",
           rgx_pattern =
             "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https",
           rgx_ignore_custom = "^[0-9f][0-9a-f]+$") {
    list(
      rgx_unnest = rgx_unnest,
      rgx_pattern = rgx_pattern,
      rgx_ignore_custom = rgx_ignore_custom
    )
  }
