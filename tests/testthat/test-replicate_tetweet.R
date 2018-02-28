

context("funcs-tetweet")
require("bindrcpp")
require("dplyr")
require("ggplot2")

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
colors_tms <- c("blue", "red", "cadetblue", "purple", "black")

tms <- c("DAL", "HOU", "MEM", "NOP", "SAS")

message("Tests are in order of likely usage in script.")

testthat::test_that(
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

    testthat::expect_true(is.list(data_multi_timefilter))

    data_multi_trim <-
      data_multi %>%
      trim_bytime_at(
        timebin = "timestamp",
        start = data_multi_timefilter$date_start,
        end = data_multi_timefilter$date_end
      )

    actual <- nrow(data_multi_trim)
    expected <- 8636
    testthat::expect_equal(actual, expected)

    data_multi_trim_2 <-
      data_multi %>%
      trim_bytime_at(
        timebin = "timestamp",
        multi = "name"
      )
    actual <- data_multi_trim_2
    expected <- data_multi_trim
    testthat::expect_equal(actual, expected)

  }

testthat::test_that(
  "time",
  {
    viz_time_multi_all <-
      data_multi_trim %>%
      visualize_time_multi_at(
        timebin = "timestamp",
        geom = "hist",
        color = "name",
        color_value = colors_tms,
        multi = "name",
        ncol = 1,
        scales = "fixed"
      )
    viz_time_multi_all
    testthat::expect_true(ggplot2::is.ggplot(viz_time_multi_all))

    viz_time_multi_hh <-
      data_multi_trim %>%
      mutate(hh4 = floor((lubridate::hour(timestamp)) / 6) + 1) %>%
      visualize_time_multi_at(
        timebin = "hh4",
        geom = "bar",
        color = "name",
        color_value = colors_tms,
        multi = "name",
        ncol = length(tms),
        scales = "fixed"
      )
    viz_time_multi_hh
    testthat::expect_true(ggplot2::is.ggplot(viz_time_multi_hh))

    viz_time_multi_hh <-
      data_multi_trim %>%
      mutate(hh = (lubridate::hour(timestamp))) %>%
      visualize_hh_multi_at(
        timebin = "hh",
        color = "name",
        color_value = colors_tms,
        multi = "name"
      )
    viz_time_multi_hh
    testthat::expect_true(ggplot2::is.ggplot(viz_time_multi_hh))
  })

testthat::test_that(
  "twitter",
  {
    data_multi_trim_kind <-
      data_multi_trim %>%
      add_tweet_kind_cols()

    actual <- setdiff(names(data_multi_trim_kind), names(data_multi_trim))
    expected <- c("hashtag", "link", "rt", "quote", "reply", "type")
    testthat::expect_equal(actual, expected)

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
    actual <- nrow(unigrams)
    expect <- 78458
    testthat::expect_equal(actual, expect)

    bigrams <-
      data_multi_trim_kind %>%
      mutate(text = rtweet::plain_tweets(text)) %>%
      tidify_to_bigrams_at(
        text = "text",
        rgx_pattern = rgx_tidiers$rgx_pattern,
        rgx_replacement = "",
        rgx_ignore_custom = rgx_tidiers$rgx_ignore_custom
      )
    actual <- nrow(bigrams)
    expect <- 35447
    testthat::expect_equal(actual, expect)
  }
)

testthat::test_that(
  "cnts",
  {
    viz_cnts_multi <-
      unigrams %>%
      visualize_cnts_multi_at(
        num_top = 5,
        color = "name",
        color_value = colors_tms,
        lab_subtitle = "By Team",
        multi = "name"
      )
    viz_cnts_multi
    testthat::expect_true(ggplot2::is.ggplot(viz_cnts_multi))

    viz_cnts_wordcloud_multi <-
      unigrams %>%
      visualize_cnts_wordcloud_multi_at(
        word = "word",
        color_value = colors_tms[5],
        num_top = 50,
        multi = "name",
        value_multi = "SAS"
      )
    # viz_cnts_wordcloud_multi

    par(mfrow = c(1, 2))
    purrr::map2(
      c("DAL", "HOU"),
      colors_tms[1:2],
      ~visualize_cnts_wordcloud_multi_at(
        data = unigrams,
        word = "word",
        color_value = .y,
        num_top = 50,
        multi = "name",
        value_multi = .x
      )
    )
    par(mfrow = c(1, 1))
  }
)

testthat::test_that(
  "freqs",
  {
    viz_bigram_freqs_multi <-
      bigrams %>%
      visualize_bigram_freqs_multi_at(
        num_top = 2,
        color = "name",
        color_value = colors_tms,
        lab_subtitle = "By Team",
        multi = "name"
      )
    viz_bigram_freqs_multi
  }
)

testthat::test_that(
  "xy",
  {
    xy_grid <- create_xy_grid(tms)
    xy_nms <- xy_grid %>% dplyr::pull(xy)

    unigrams_freqs_multi <-
      unigrams %>%
      compute_freqs_multi_at(
        multi = "name"
      )

    unigrams_freqs_multi_by2 <-
      unigrams %>%
      compute_freqs_multi_by2_at(
        xy_grid = xy_grid,
        xy_nms = xy_nms,
        multi = "name"
      )
    unigrams_freqs_multi_by2

    testthat::expect_true(nrow(unigrams_freqs_multi) != nrow(unigrams_freqs_multi_by2))
    testthat::expect_true(ncol(unigrams_freqs_multi) != ncol(unigrams_freqs_multi_by2))

  }
)



