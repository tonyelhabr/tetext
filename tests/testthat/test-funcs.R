

context("functions")
require("bindrcpp")
require("dplyr")
require("ggplot2")
require("temisc")

dir <- file.path("tests")
filename_data_multi <- "tweets-search-nba-augmented-trim-sw.rds"
filename_data <- "data-tony-cleaned.rds"
filename_unigrams <- "unigrams-tony.rds"
filename_bigrams <- "bigrams-tony.rds"
if (interactive()) {
  dir.exists(file.path("inst", dir))
} else {
  print(dir.exists(dir))
}
filepath_data_multi <-
  system.file(dir, filename_data_multi, package = "tetext", mustWork = TRUE)
filepath_data <-
  system.file(dir, filename_data, package = "tetext", mustWork = TRUE)
filepath_unigrams <-
  system.file(dir,
              filename_unigrams,
              package = "tetext",
              mustWork = TRUE)
filepath_bigrams <-
  system.file(dir,
              filename_bigrams,
              package = "tetext",
              mustWork = TRUE)
# file.exists(filepath_unigrams)

data_multi <- readRDS(file = filepath_data_multi)
data <- readRDS(file = filepath_data)
unigrams <- readRDS(file = filepath_unigrams)
bigrams <- readRDS(file = filepath_bigrams)

color_main <- "firebrick"
colors_main <- temisc::get_rpal_colors(color_main)
colors_te <- temisc::colors_te

message("Tests are in order of likely usage in script.")

clean_tweets <-
  function(data,
           trim = TRUE,
           cols_extra = "name") {
    cols_keep <-
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
      )

    if(trim) {
    data <-
      data %>%
      select(one_of(c(cols_extra, cols_keep)))
    }
    out <-
      data %>%
      mutate_if(is.list, funs(as.character)) %>%
      mutate(timestamp = lubridate::ymd_hms(created_at)) %>%
      mutate(timestamp = lubridate::with_tz(timestamp, "America/Chicago")) %>%
      # mutate(time = round_time(timestamp, 60 * 60)) %>%
      mutate(time = lubridate::hour(timestamp) + lubridate::minute(timestamp) / 60)

    out
  }
data_multi <- clean_tweets(data_multi)
testhat::test_that(
  "time",
  {
    lab_subtitle_all <-
      paste0(
        "From ",
        strftime(data$timestamp[1], "%Y-%m-%d"),
        " to ",
        strftime(rev(data$timestamp)[1], "%Y-%m-%d")
      )

    viz_time_all <-
      visualize_time_at(
        data = data,
        timebin = "timestamp",
        geom = "hist",
        color_value = colors_te,
        lab_subtitle = lab_subtitle_all
      )
    viz_time_all
    testthat::expect_true(ggplot2::is.ggplot(viz_time_all))

    viz_time_multi_all <-
      visualize_time_multi_at(
        data = data_multi,
        timebin = "timestamp",
        geom = "hist",
        color = "name",
        color_value = colors_te,
        multi = "name",
        ncol = 1,
        scales = "fixed"
      )
    viz_time_multi_all

    viz_time_yyyy <-
      visualize_time_at(
        data = data,
        timebin = "yyyy",
        geom = "bar",
        color_value = colors_te,
        lab_subtitle = "By Year"
      )
    viz_time_yyyy
    testthat::expect_true(ggplot2::is.ggplot(viz_time_yyyy))
    viz_time_mm <-
      visualize_time_at(
        data = data,
        timebin = "mm",
        geom = "bar",
        color_value = colors_te,
        lab_subtitle = "By Month"
      )
    viz_time_mm
    testthat::expect_true(ggplot2::is.ggplot(viz_time_mm))
    viz_time_wd <-
      visualize_time_at(
        data = data,
        timebin = "wd",
        geom = "bar",
        color_value = colors_te,
        lab_subtitle = "By Day of Week"
      )
    viz_time_wd
    testthat::expect_true(ggplot2::is.ggplot(viz_time_wd))
    viz_time_hh <-
      visualize_time_at(
        data = data,
        timebin = "hh",
        geom = "bar",
        add_alpha = TRUE,
        color_value = colors_te,
        lab_subtitle = "By Hour"
      )
    viz_time_hh
    testthat::expect_true(ggplot2::is.ggplot(viz_time_hh))
    viz_time_list <-
      visualize_time_batch_at(
        data = data
      )
    viz_time_list
    testthat::expect_true(ggplot2::is.ggplot(viz_time_list))
    viz_time_list <-
      visualize_time_batch_at(
        data = data,
        arrange = TRUE
      )
    # gridExtra::grid.arrange(viz_time_list)
    t# estthat::expect_true(!ggplot2::is.ggplot(viz_time_list))

    # TODO: Add a test for visualize_time_multi_at().

  })

testhat::test_that(
  "cnts",
  {
    viz_cnts <-
      visualize_cnts_at(
        data = unigrams,
        num_top = 10,
        color = NULL,
        color_value = colors_te
      )
    viz_cnts
    testthat::expect_true(ggplot2::is.ggplot(viz_cnts))
    viz_cnts_multi <-
      visualize_cnts_multi_at(
        data = unigrams,
        num_top = 10,
        color = "yyyy",
        color_value = colors_te,
        lab_subtitle = "By Year",
        multi = "yyyy"
      )
    viz_cnts_multi
    testthat::expect_true(ggplot2::is.ggplot(viz_multi))
    viz_cnts_wordcloud <-
      visualize_cnts_wordcloud_at(
        data = unigrams,
        color_value = colors_main,
        num_top = 50
      )

    # TODO: Generate wordclouds for all 'multi' values.
    viz_cnts_wordcloud_multi <-
      visualize_cnts_wordcloud_multi_at(
        data = unigrams,
        word = "word",
        color_value = colors_main,
        num_top = 50,
        multi = "yyyy",
        value_multi = 2011
      )
  })


testhat::test_that(
  "freqs",
  {
    bigrams_freqs <-
      compute_freqs_at(data = bigrams)
    actual <- bigrams_freqs$word[1:3]
    expect <- c("excel vba", "ut austin", "pl sql")
    testthat::expect_equal(actual, expect)

    viz_bigram_freqs_multi <-
      visualize_bigram_freqs_multi_at(data = bigrams,
                                   multi = "yyyy",
                                   color_value = colors_te)
    viz_bigram_freqs_multi
    testthat::expect_true(ggplot2::is.ggplot(viz_bigram_freqs_multi))
  })



testhat::test_that(
  "corrs",
  {
    num_top_ngrams <- 50
    num_top_corrs <- 50
    unigrams_corrs <-
      compute_corrs_at(
        data = unigrams,
        num_top_ngrams = num_top_ngrams,
        num_top_corrs = num_top_corrs,
        feature = "timestamp"
      )
    actual <- unigrams_corrs$item1[1:3]
    expect <- c("excel", "austin", "oracle")
    testthat::expect_equal(actual, expect)
    actual <- unigrams_corrs$item2[1:3]
    expect <- c("vba", "ut", "sql")
    testthat::expect_equal(actual, expect)

    viz_corrs_network <-
      visualize_corrs_network_at(
        data = unigrams,
        num_top_ngrams = num_top_ngrams,
        num_top_corrs = num_top_corrs,
        feature = "timestamp"
      )
    viz_corrs_network
    testthat::expect_true(ggplot2::is.ggplot(viz_corrs_network))
  })


testhat::test_that(
  "tfidf",
  {
    unigrams_tfidf <-
      compute_tfidf_at(data = unigrams,
                    doc = "yyyy")
    actual <- unigrams_tfidf$word[1]
    expect <- c("resistivity")
    testthat::expect_equal(actual, expect)

    viz_unigrams_tfidf_multi <-
      visualize_tfidf_at(
        data = unigrams,
        doc = "yyyy",
        color_value = colors_te,
        lab_subtitle = "By Year"
      )
    viz_unigrams_tfidf_multi
    testthat::expect_true(ggplot2::is.ggplot(viz_unigrams_tfidf_multi))
  })


testhat::test_that(
  "change",
  {
    unigrams_bytime <-
      compute_change_at(data = unigrams,
                     timebin = "timestamp",
                     timefloor = "year")

    expect <- c("excel", "nutrition", "vba")
    actual <- unigrams_bytime$word[1:3]
    testthat::expect_equal(actual, expect)

    viz_unigrams_bytime <-
      visualize_change_at(
        data = unigrams,
        timebin = "timestamp",
        timefloor = "year",
        color_value = colors_te
      )
    viz_unigrams_bytime
    testthat::expect_true(ggplot2::is.ggplot(viz_unigrams_bytime))
  })


