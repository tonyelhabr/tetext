

context("funcs-tetweet")
require("bindrcpp")
require("dplyr")
require("ggplot2")
require("temisc")

dir <- file.path("tests")
filename_data_multi <- "tweets-search-nba-augmented.rds"
if (interactive()) {
  dir.exists(file.path("inst", dir))
} else {
  print(dir.exists(dir))
}
filepath_data_multi <-
  system.file(dir, filename_data_multi, package = "tetext", mustWork = TRUE)

data <- readRDS(file = filepath_data_multi)
colors_te <- temisc::colors_te

tms <- c("DAL", "HOU", "MEM", "NOP", "SAS")

message("Tests are in order of likely usage in script.")

testhat::test_that(
  "trim",
  {
    data_multi <-
      data %>%
      filter(name %in% tms) %>%
      clean_tweets_at(multi = "name")

    actual <- data_multi %>% distinct(name) %>% arrange(name) %>% pull(name)
    expected <- tms
    testthat::expect_equal(actual, expected)

    data_multi_timefilter <-
      data_multi %>%
      compute_timefilter_multi_at(timebin = "timestamp", multi = "name")

    data_multi_trim <-
      data_multi %>%
      trim_bytime_at(
        timebin = "timestamp",
        start = data_multi_timefilter$date_start,
        end = data_multi_timefilter$date_end
      )

    data_multi %>%
      trim_bytime_at(
        timebin = "timestamp",
        multi = "name"
      )
  }

testhat::test_that(
  "time",
  {
    viz_time_multi_all <-
      visualize_time_multi_at(
        data = data_multi_trim,
        timebin = "timestamp",
        geom = "hist",
        color = "name",
        color_value = colors_te,
        multi = "name",
        ncol = 1,
        scales = "fixed"
      )
    viz_time_multi_all
    testthat::expect_true(ggplot2::is.ggplot(viz_time_multi_all))

    data_multi_trim %>%
      mutate(
        hh = lubridate::hour(timestamp),
        hh4 = floor((lubridate::hour(timestamp)) / 6) + 1
      ) %>%
      arrange(desc(hh4)) %>%
      select(time, hh, hh4) %>%
      filter(hh4 <= 4)

    viz_time_multi_hh <-
      visualize_time_multi_at(
        data = data_multi_trim %>% mutate(hh4 = floor((lubridate::hour(timestamp)) / 6) + 1),
        timebin = "hh4",
        geom = "bar",
        color = "name",
        color_value = colors_te,
        multi = "name",
        ncol = length(tms),
        scales = "fixed"
      )
    viz_time_multi_hh

    viz_hh_multi_hh <-
      visualize_hh_multi_at(
        data = data_multi_trim %>% mutate(hh = (lubridate::hour(timestamp))),
        timebin = "hh",
        color = "name",
        color_value = colors_te,
        multi = "name"
      )
    viz_hh_multi_hh
  })

testhat::test_that(
  "twitter",
  {
    data_multi_trim_kind <-
      data_multi_trim %>%
      add_tweet_kind_cols()

    # TODO: Add visualize_tweet_kind_at()

    rgx_tidiers <-
      get_tweet_rgx_tidiers()

    unigrams <-
      data_multi_trim_kind %>%
      mutate(text = rtweet::plain_tweets(text)) %>%
      tidify_to_unigrams_at(
        text = "text",
        rgx_unnest = rgx_tidiers$rgx_unnest,
        rgx_pattern = rgx_tidiers$rgx_pattern,
        rgx_replacement = "",
        rgx_ignore_custom = rgx_tidiers$rgx_ignore_custom
      )


  }
)

