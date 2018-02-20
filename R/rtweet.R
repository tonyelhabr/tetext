

# NOTE: `time` column creation is replicated from https://buzzfeednews.github.io/2018-01-trump-twitter-wars/.
# Other columns are replicated from https://juliasilge.com/blog/ten-thousand-data/.
#' Title
#'
#' @param data
#' @param multi
#' @param trim
#' @param colnames
#' @param timezone
#'
#' @return
#' @export
#'
#' @examples
clean_tweets_at <-
  function(data = NULL,
           multi,
           trim = TRUE,
           colnames =
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
    if (trim) {
      if(!missing(multi)) {
        data <-
          data %>%
          dplyr::select(dplyr::one_of(c(multi, colnames)))
      } else {
        data <-
          data %>%
          dplyr::select(dplyr::one_of(c(colnames)))
      }
    }
    out <-
      data %>%
      dplyr::mutate_if(is.list, dplyr::funs(as.character)) %>%
      dplyr::mutate(timestamp = lubridate::ymd_hms(created_at)) %>%
      dplyr::mutate(timestamp = lubridate::with_tz(timestamp, timezone)) %>%
      dplyr::mutate(time = lubridate::hour(timestamp) + lubridate::minute(timestamp) / 60)

    out
  }

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
add_tweet_kind_cols <- function(data = NULL) {
  out <-
    data %>%
    dplyr::mutate(
      hashtag = dplyr::if_else(!is.na(hashtags), 1, 0),
      link = dplyr::if_else(
        !is.na(media_expanded_url) & !is.na(ext_media_expanded_url),
        1,
        0
      ),
      rt = if_else(is_retweet, 1, 0),
      quote = if_else(is_quote, 1, 0),
      reply = if_else(!is.na(reply_to_status_id), 1, 0)
    ) %>%
    dplyr::mutate(type =
                    dplyr::case_when(
               rt == TRUE ~ "RT",
               reply == TRUE ~ "reply",
               quote == TRUE ~ "quote",
               TRUE ~ "original"
             ))
  out
}

#' Title
#'
#' @param rgx_unnest
#' @param rgx_pattern
#' @param rgx_ignore_custom
#'
#' @return
#' @export
#'
#' @examples
get_tweet_rgx_tidiers <-
  function(
    rgx_unnest <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))",
    rgx_pattern <-
      "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https",
    rgx_ignore_custom = "^[0-9f][0-9a-f]+$") {
    list(
      rgx_unnest = rgx_unnest,
      rgx_pattern = rgx_pattern,
      rgx_ignore_custom = rgx_ignore_custom
    )
  }


