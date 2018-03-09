

context("funcs-tetweet")
require("bindrcpp")
require("dplyr")
require("ggplot2")
require("rtweet")

dir <- file.path("extdata")
filename_data_facet <- "tweets-search-nba-augmented.rds"
if (interactive()) {
  dir.exists(file.path("inst", dir))
  devtools::load_all()
} else {
  print(dir.exists(dir))
}
path_data_facet <-
  system.file(dir, filename_data_facet, package = "tetext", mustWork = TRUE)

pull_distinctly <- function(data, col) {
  col <- enquo(col)
  data %>%
    distinct(!!col) %>%
    arrange(!!col) %>%
    pull(!!col)
}


data <- readRDS(path_data_facet)
# data <- rename(data, id = name)
# saveRDS(data, file = path_data_facet)

ids <- data %>% pull_distinctly(id)
# colors <-
#   c(
#     "#061922",
#     "#ce1141",
#     "#7399c6",
#     "#002b5c",
#     "#061922"
#   )
colors <-
  c(
    "#007dc5",
    "#ce1141",
    "#0f586c",
    "#002b5c",
    "#bac3c9"
  )
if (interactive()) {
  scales::show_col(colors)
}

ids_filt <- c("DAL", "HOU", "MEM", "NOP", "SAS")
idxs_filt <- match(ids_filt, ids)
colors_filt <- colors[idxs_filt]
names(colors_filt) <- ids_filt

id_filt <- "MEM"
idx_filt <- match(id_filt, ids_filt)
color_filt <- colors_filt[idx_filt]

# color_filt_inv <- get_color_hex_inverse(color_filt)
# lab_other <- paste0("Not ", id_filt)
# names(color_filt_inv) <- lab_other
# if(interactive()) {
#   scales::show_col(c(color_filt, color_filt_inv))
# }

data_facet <-
  data %>%
  filter(id %in% ids_filt) %>%
  clean_tweets(facet = id)

data_facet_timefilter <-
  data_facet %>%
  compute_timefilter_facet(
    timebin = timestamp,
    facet = id
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
    actual <- data_facet %>% pull_distinctly(id)
    expected <- ids_filt
    testthat::expect_equal(actual, expected)

    testthat::expect_true(is.list(data_facet_timefilter))

    actual <- nrow(data_facet_trim)
    expected <- 8636
    testthat::expect_equal(actual, expected)

    data_facet_timefilter_2 <-
      data_facet %>%
      compute_timefilter(
        timebin = timestamp,
        facet = id
      )
    actual <- data_facet_timefilter_2
    expected <- data_facet_timefilter
    testthat::expect_equal(actual, expected)

    data_facet_trim_2 <-
      data_facet %>%
      trim_bytime(
        timebin = timestamp,
        facet = id
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
      filter(id %in% id_filt) %>%
      visualize_time(
        timebin = timestamp,
        color = id,
        theme_params = list(axis.text.y = ggplot2::element_text(size = 1),
                            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 30)),
        scale_manual_params = list(values = color_filt)
      )
    viz_time_all

    viz_time_facet_all <-
      data_facet_trim %>%
      visualize_time_facet(
        timebin = timestamp,
        color = id,
        facet = id,
        scale_manual_params = list(values = colors_filt),
        facet_params = list(scales = "free_y")
      )
    viz_time_facet_all
    testthat::expect_true(ggplot2::is.ggplot(viz_time_facet_all))

    viz_time_facet_hh <-
      data_facet_trim %>%
      visualize_time_facet(
        timebin = timestamp,
        bin = TRUE,
        timefloor = "hour",
        color = id,
        facet = id,
        scale_manual_params = list(values = colors_filt),
        facet_params = list(
          strip.position = "right",
          ncol = 1
        )
      )
    viz_time_facet_hh

    viz_time_facet_hh <-
      data_facet_trim %>%
      mutate(hh4 = floor((lubridate::hour(timestamp)) / 6) + 1) %>%
      visualize_time_facet(
        timebin = hh4,
        color = id,
        facet = id,
        scale_manual_params = list(values = colors)
      )
    viz_time_facet_hh
    testthat::expect_true(ggplot2::is.ggplot(viz_time_facet_hh))

    viz_time_hh <-
      data_facet_trim %>%
      mutate(hh = (lubridate::hour(timestamp))) %>%
      visualize_time_hh(
        timebin = hh,
        color = id,
        facet = id,
        scale_manual_params = list(values = colors)
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
    viz_unigram_cnts <-
      unigrams %>%
      filter(id %in% id_filt) %>%
      visualize_cnts(
        token = word,
        color = id,
        num_top = 10,
        scale_manual_params = list(values = color_filt),
        labs_params = list(title = lab_title, subtitle = id_filt)
      )
    viz_unigram_cnts
    testthat::expect_true(ggplot2::is.ggplot(viz_unigram_cnts))

    viz_unigram_cnts_facet <-
      unigrams %>%
      visualize_cnts_facet(
        token = word,
        facet = id,
        color = id,
        num_top = 5,
        scale_manual_params = list(values = colors_filt),
        labs_params = list(title = lab_title, subtitle = lab_subtitle)
      )
    viz_unigram_cnts_facet
    testthat::expect_true(ggplot2::is.ggplot(viz_unigram_cnts_facet))

    viz_unigram_cnts_wordcloud <-
      unigrams %>%
      filter(id %in% id_filt) %>%
      visualize_cnts_wordcloud(
        token = word,
        wordcloud_params = list(colors = color_filt)
      )

    viz_unigram_cnts_wordcloud_facet <-
      unigrams %>%
      visualize_cnts_wordcloud_facet(
        token = word,
        facet = id,
        value_facet = id_filt,
        wordcloud_params = list(colors = color_filt)
      )

    par(mfrow = c(2, 3))
    invisible(
      purrr::map2(
        ids_filt,
        colors_filt,
        ~visualize_cnts_wordcloud_facet(
          data = unigrams,
          token = word,
          facet = id,
          value_facet = .x,
          wordcloud_params = list(colors = .y, max.words = 30)
        )
      )
    )
    par(mfrow = c(1, 1))
  }
)

testthat::test_that(
  "freqs",
  {
    lab_title <- "Bigram Frequency"
    lab_subtitle <- "By Team"
    viz_bigram_freqs_facet <-
      bigrams %>%
      visualize_bigram_freqs(
        token = word,
        facet = id,
        color = id,
        num_top = 2,
        scale_manual_params = list(values = colors_filt),
        labs_params = list(title = lab_title, subtitle = lab_subtitle)
      )
    viz_bigram_freqs_facet
    testthat::expect_true(ggplot2::is.ggplot(viz_bigram_freqs_facet))
  }
)

testthat::test_that(
  "sents",
  {
    unigram_sent_summ <-
      unigrams %>%
      filter(id %in% c(id_filt)) %>%
      compute_sent_summ(
        token = word,
        feature = status_id
      )
    unigram_sent_summ

    actual <- nrow(unigram_sent_summ)
    expect <- 2
    testthat::expect_equal(actual, expect)

    unigram_sent_summ_facet <-
      unigrams %>%
      compute_sent_summ_facet(
        token = word,
        feature = status_id,
        facet = id
      )
    unigram_sent_summ_facet

    actual <- nrow(unigram_sent_summ_facet)
    expect <- 10
    testthat::expect_equal(actual, expect)
  }
)

testthat::test_that(
  "sents_by2",
  {
    unigram_sentratios <-
      unigrams %>%
      compute_sentratios_facet_by2(
        token = word,
        facet = id
      )
    unigram_sentratios

    actual <- nrow(unigram_sentratios)
    expect <- 962
    testthat::expect_equal(actual, expect)

    unigram_sentratios %>%
      dplyr::group_by(name_xy, sentiment) %>%
      dplyr::do(head(., 2))

  }
)

testthat::test_that(
  "freqs_by2",
  {

    unigram_freqs_facet <-
      unigrams %>%
      compute_freqs_facet(
        token = word,
        facet = id
      )

    unigram_freqs_facet_by2 <-
      unigrams %>%
      compute_freqs_facet_by2(
        token = word,
        facet = id
      )
    unigram_freqs_facet_by2

    testthat::expect_true(nrow(unigram_freqs_facet) != nrow(unigram_freqs_facet_by2))
    testthat::expect_true(ncol(unigram_freqs_facet) != ncol(unigram_freqs_facet_by2))

    viz_unigram_freqs_facet_by2 <-
      unigrams %>%
      visualize_freqs_facet_by2(
        token = word,
        facet = id,
        filter_facet = TRUE,
        facet_main = id_filt
      )
    viz_unigram_freqs_facet_by2
    testthat::expect_true(ggplot2::is.ggplot(viz_unigram_freqs_facet_by2))
  }
)

testthat::test_that(
  "logratios_by2",
  {
    unigram_logratios_facet_by2 <-
      unigrams %>%
      compute_logratios_facet_by2(
        token = word,
        facet = id,
        cnt_min = 50
      )
    unigram_logratios_facet_by2

    actual <- nrow(unigram_logratios_facet_by2)
    expect <- 882
    testthat::expect_equal(actual, expect)

    viz_unigram_logratios_facet_by2 <-
      unigrams %>%
      visualize_logratios_facet_by2(
        token = word,
        facet = id,
        num_top = 3,
        filter_facet = TRUE,
        facet_main = id_filt,
        color_main = color_filt
      )
    viz_unigram_logratios_facet_by2
    testthat::expect_true(ggplot2::is.ggplot(viz_unigram_logratios_facet_by2))
  }
)

testthat::test_that(
  "sentratios_by2",
  {
    lab_title <- "Most Signifcant Words Contributing to Sentiment Differences"
    viz_unigram_sentratios_facet_by2 <-
      unigrams %>%
      visualize_sentratios_facet_by2(
        token = word,
        facet = id,
        num_top = 3,
        filter_facet = TRUE,
        facet_main = id_filt,
        filter_sent = TRUE,
        # lab_other = lab_other,
        sent_main = "positive",
        color_main = color_filt,
        # scale_manual_params = list(values = c(color_filt, color_filt_inv)),
        labs_params = list(title = lab_title, subtitle = "positive")
      )
    viz_unigram_sentratios_facet_by2
    testthat::expect_true(ggplot2::is.ggplot(viz_unigram_sentratios_facet_by2))


    viz_unigram_sentratios_facet_by2_list <-
      purrr::map(
        c("positive", "negative"),
        ~visualize_sentratios_facet_by2(
          data = unigrams,
          token = word,
          facet = id,
          num_top = 3,
          filter_facet = TRUE,
          facet_main = id_filt,
          filter_sent = TRUE,
          sent_main = .x,
          # lab_other = lab_other,
          color_main = color_filt,
          # scale_manual_params = list(values = c(color_filt, color_filt_inv)),
          labs_params = list(title = lab_title, subtitle = .x)
        )
      )
    viz_unigram_sentratios_facet_by2_list
    testthat::expect_true(is.list(viz_unigram_sentratios_facet_by2_list))
  }
)



