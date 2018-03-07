

context("funcs-tetweet")
require("bindrcpp")
require("dplyr")
require("ggplot2")

dir <- file.path("tests")
filename_data_facet <- "tweets-search-nba-augmented.rds"
if (interactive()) {
  dir.exists(file.path("inst", dir))
} else {
  print(dir.exists(dir))
}
path_data_facet <-
  system.file(dir, filename_data_facet, package = "tetext", mustWork = TRUE)

data <- readRDS(file = path_data_facet)
colors_tms <- c("blue", "red", "cadetblue", "purple", "black")

tms <- c("DAL", "HOU", "MEM", "NOP", "SAS")

message("Tests are in order of likely usage in script.")

data_facet <-
  data %>%
  filter(name %in% tms) %>%
  clean_tweets(facet = name)

data_facet_timefilter <-
  data_facet %>%
  compute_timefilter_facet(
    timebin = timestamp,
    facet = name
  )

data_facet_trim <-
  data_facet %>%
  trim_bytime(
    timebin = timestamp,
    start = data_facet_timefilter$date_start,
    end = data_facet_timefilter$date_end
  )

testthat::test_that(
  "trim",
  {
    actual <- data_facet %>% distinct(name) %>% arrange(name) %>% pull(name)
    expected <- tms
    testthat::expect_equal(actual, expected)

    testthat::expect_true(is.list(data_facet_timefilter))

    actual <- nrow(data_facet_trim)
    expected <- 8636
    testthat::expect_equal(actual, expected)

    data_facet_timefilter_2 <-
      data_facet %>%
      compute_timefilter(
        timebin = timestamp,
        facet = name
      )
    actual <- data_facet_timefilter_2
    expected <- data_facet_timefilter
    testthat::expect_equal(actual, expected)

    data_facet_trim_2 <-
      data_facet %>%
      trim_bytime(
        timebin = timestamp,
        facet = name
      )
    actual <- data_facet_trim_2
    expected <- data_facet_trim
    testthat::expect_equal(actual, expected)

  }
)

testthat::test_that(
  "time",
  {
    viz_time_all <-
      data_facet_trim %>%
      filter(name %in% "SAS") %>%
      visualize_time(
        timebin = timestamp,
        color = name,
        scale_manual_params = list(values = colors_tms[5])
      )
    viz_time_all

    viz_time_facet_all <-
      data_facet_trim %>%
      visualize_time_facet(
        timebin = timestamp,
        color = name,
        facet = name,
        scale_manual_params = list(values = colors_tms),
        facet_params = list(
          strip.position = "right",
          ncol = 1
        )
      )
    viz_time_facet_all
    testthat::expect_true(ggplot2::is.ggplot(viz_time_facet_all))

    viz_time_facet_hh <-
      data_facet_trim %>%
      mutate(hh4 = floor((lubridate::hour(timestamp)) / 6) + 1) %>%
      visualize_time_facet(
        timebin = hh4,
        color = name,
        facet = name,
        scale_manual_params = list(values = colors_tms)
      )
    viz_time_facet_hh
    testthat::expect_true(ggplot2::is.ggplot(viz_time_facet_hh))

    viz_time_hh <-
      data_facet_trim %>%
      mutate(hh = (lubridate::hour(timestamp))) %>%
      visualize_time_hh(
        timebin = hh,
        color = name,
        facet = name,
        scale_manual_params = list(values = colors_tms)
      )
    viz_time_hh
    testthat::expect_true(ggplot2::is.ggplot(viz_time_hh))
  }
)

data_facet_trim_kind <-
  data_facet_trim %>%
  add_tweet_kind_cols()

rgx_tidiers <-
  get_tweet_rgx_tidiers()

unigrams <-
  data_facet_trim_kind %>%
  mutate(text = rtweet::plain_tweets(text)) %>%
  tidify_to_unigrams(
    text = text,
    rgx_unnest = rgx_tidiers$rgx_unnest,
    rgx_pattern = rgx_tidiers$rgx_pattern,
    rgx_replacement = "",
    rgx_ignore_custom = rgx_tidiers$rgx_ignore_custom
  )

bigrams <-
  data_facet_trim_kind %>%
  mutate(text = rtweet::plain_tweets(text)) %>%
  tidify_to_bigrams(
    text = text,
    rgx_pattern = rgx_tidiers$rgx_pattern,
    rgx_replacement = "",
    rgx_ignore_custom = rgx_tidiers$rgx_ignore_custom
  )

testthat::test_that(
  "twitter",
  {
    actual <- setdiff(names(data_facet_trim_kind), names(data_facet_trim))
    expected <- c("hashtag", "link", "rt", "quote", "reply", "type")
    testthat::expect_equal(actual, expected)

    # TODO: Add visualize_tweet_kind_at()

    actual <- nrow(unigrams)
    expect <- 78458
    testthat::expect_equal(actual, expect)

    actual <- nrow(bigrams)
    expect <- 35447
    testthat::expect_equal(actual, expect)
  }
)

testthat::test_that(
  "cnts",
  {
    lab_title <- "Temporal Count"
    lab_subtitle <- "By Team"
    viz_cnts_facet <-
      unigrams %>%
      visualize_cnts_facet(
        token = word,
        facet = name,
        color = name,
        num_top = 10,
        scale_manual_params = list(values = colors_tms),
        labs_params = list(title = lab_title, subtitle = lab_subtitle
      )
    viz_cnts_facet
    testthat::expect_true(ggplot2::is.ggplot(viz_cnts_facet))

    viz_cnts_wordcloud_facet <-
      unigrams %>%
      visualize_cnts_wordcloud_facet(
        token = word,
        facet = name,
        value_facet = "SAS",
        wordcloud_params = list(colors = colors_tms[5])
      )
    # viz_cnts_wordcloud_facet

    par(mfrow = c(2, 3))
    purrr::map2(
      tms,
      colors_tms,
      ~visualize_cnts_wordcloud_facet(
        data = unigrams,
        token = word,
        facet = name,
        value_facet = .x,
        wordcloud_params = list(colors = .y, max.words = 15)
      )
    )
    par(mfrow = c(1, 1))
  }
)

testthat::test_that(
  "freqs",
  {
    lab_title <- "Token Frequency"
    lab_subtitle <- "By Team"
    viz_bigram_freqs_facet <-
      bigrams %>%
      visualize_bigram_freqs_facet(
        token = word,
        facet = name,
        color = name,
        num_top = 2,
        scale_manual_params = list(values = colors_tms),
        labs_params = list(title = lab_title, subtitle = lab_subtitle)
      )
    viz_bigram_freqs_facet
    testthat::expect_true(ggplot2::is.ggplot(viz_bigram_freqs_facet))
  }
)

testthat::test_that(
  "sents",
  {

    unigrams_sent_summ <-
      unigrams %>%
      filter(name %in% c("SAS")) %>%
      compute_sent_summary(
        token = word,
        feature = status_id
      )
    unigrams_sent_summ

    actual <- nrow(unigrams_sent_summ)
    expect <- 2
    testthat::expect_equal(actual, expect)

    unigrams_sent_summ_facet <-
      unigrams %>%
      compute_sent_summary_facet(
        token = word,
        feature = status_id,
        facet = name
      )
    unigrams_sent_summ_facet

    actual <- nrow(unigrams_sent_summ_facet)
    expect <- 10
    testthat::expect_equal(actual, expect)

    unigrams_sentratios <-
      unigrams %>%
      compute_sentratios_facet_by2_at(
        token = word,
        facet = name
      )
    unigrams_sentratios

    actual <- nrow(unigrams_sentratios)
    expect <- 962
    testthat::expect_equal(actual, expect)

    unigrams_sentratios %>%
      dplyr::group_by(name_xy, sentiment) %>%
      dplyr::do(head(., 2))

  }
)

testthat::test_that(
  "xy",
  {
    # xy_grid <- create_xy_grid(tms)
    # xy_nms <- xy_grid %>% dplyr::pull(xy)

    unigrams_freqs_facet <-
      unigrams %>%
      compute_freqs_facet_at(
        token = NULL,
        facet = "name"
      )

    unigrams_freqs_facet_by2 <-
      unigrams %>%
      compute_freqs_facet_by2_at(
        token = NULL,
        facet = "name"
      )
    unigrams_freqs_facet_by2

    testthat::expect_true(nrow(unigrams_freqs_facet) != nrow(unigrams_freqs_facet_by2))
    testthat::expect_true(ncol(unigrams_freqs_facet) != ncol(unigrams_freqs_facet_by2))

    viz_freqs_facet_by2 <-
      unigrams %>%
      visualize_freqs_facet_by2_at(
        token = NULL,
        facet = "name",
        filter_facet = TRUE,
        # x_include = "SAS"
        facet_main = "SAS"
      )
    viz_freqs_facet_by2
    testthat::expect_true(ggplot2::is.ggplot(viz_freqs_facet_by2))

    unigrams_logratios_facet_by2 <-
      unigrams %>%
      compute_logratios_facet_by2_at(
        token = NULL,
        facet = "name",
        cnt_min = 50
      )
    unigrams_logratios_facet_by2

    viz_logratios_facet_by2 <-
      unigrams %>%
      visualize_logratios_facet_by2_at(
        token = NULL,
        facet = "name",
        filter_facet = TRUE,
        facet_main = "SAS",
        num_top = 3,
        color_value = c(colors_tms[5], "grey50")
      )
    viz_logratios_facet_by2
    testthat::expect_true(ggplot2::is.ggplot(viz_logratios_facet_byy2)

    viz_sentratios_facet_by2 <-
      unigrams %>%
      visualize_sentratios_facet_by2_at(
        token = NULL,
        facet = "name",
        filter_facet = TRUE,
        facet_main = "SAS",
        sent_main = "positive",
        num_top = 3,
        flip_axes = TRUE,
        color_value = c(colors_tms[5], "grey50")
      )
    viz_sentratios_facet_by2
    testthat::expect_true(ggplot2::is.ggplot(viz_sentratios_facet_byy2)
  }
)



