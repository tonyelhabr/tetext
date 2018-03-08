

context("functions")
require("bindrcpp")
require("dplyr")
require("ggplot2")

dir <- file.path("extdata")
filename_data <- "data-tony-cleaned.rds"
filename_unigrams <- "unigrams-tony.rds"
filename_bigrams <- "bigrams-tony.rds"
if (interactive()) {
  dir.exists(file.path("inst", dir))
  devtools::load_all()
} else {
  print(dir.exists(dir))
}

path_data <-
  system.file(dir, filename_data, package = "tetext", mustWork = TRUE)
path_unigrams <-
  system.file(dir,
              filename_unigrams,
              package = "tetext",
              mustWork = TRUE)
path_bigrams <-
  system.file(dir,
              filename_bigrams,
              package = "tetext",
              mustWork = TRUE)


data <- readRDS(file = path_data)
unigrams <- readRDS(file = path_unigrams)
bigrams <- readRDS(file = path_bigrams)

colors_div <- c("black", "grey", "red", "green", "blue", "orange", "purple", "yellow", "pink", "cyan")
color_main <- colors_div[3]
colors_main <- paste0(color_main, c("", as.character(seq(1, 4, 1))))

testthat::test_that(
  "time",
  {
    lab_title <- "Temporal Count"
    viz_time_yyyy <-
      data %>%
      visualize_time(
        timebin = yyyy,
        scale_manual_params = list(values = colors_div),
        labs_params = list(title = lab_title, subtitle = "By Year")
      )
    viz_time_yyyy
    testthat::expect_true(ggplot2::is.ggplot(viz_time_yyyy))

    viz_time_mm <-
      data %>%
      visualize_time(
        timebin = mm,
        scale_manual_params = list(values = colors_div),
        labs_params = list(title = lab_title, subtitle = "By Month")
      )
    viz_time_mm
    testthat::expect_true(ggplot2::is.ggplot(viz_time_mm))

    viz_time_wd <-
      data %>%
      visualize_time(
        timebin = wd,
        scale_manual_params = list(values = colors_div),
        labs_params = list(title = lab_title, subtitle = "By Weekday")
      )
    viz_time_wd
    testthat::expect_true(ggplot2::is.ggplot(viz_time_wd))

    viz_time_hh <-
      data %>%
      visualize_time(
        timebin = hh,
        add_alpha = TRUE,
        scale_manual_params = list(values = colors_div),
        labs_params = list(title = lab_title, subtitle = "By Hour")
      )
    viz_time_hh
    testthat::expect_true(ggplot2::is.ggplot(viz_time_hh))
  })

testthat::test_that(
  "corrs",
  {
    num_top_ngrams <- 100
    num_top_corrs <- 100

    unigrams_corrs <-
      unigrams %>%
      compute_corrs(
        token = word,
        feature = timestamp,
        num_top_ngrams = num_top_ngrams,
        num_top_corrs = num_top_corrs
      )
    actual <- unigrams_corrs$item1[1:3]
    # expect <- c("excel", "austin", "oracle")
    expect <- c("san", "excel", "potato")
    testthat::expect_equal(actual, expect)
    actual <- unigrams_corrs$item2[1:3]
    # expect <- c("vba", "ut", "sql")
    expect <- c("antonio", "vba", "sweet")
    testthat::expect_equal(actual, expect)

    unigrams_corrs_2 <-
      unigrams %>%
      compute_corrs(
        token = word,
        feature = timestamp,
        num_top_ngrams = num_top_ngrams / nrow(unigrams),
        # num_top_ngrams = num_top_ngrams / nrow(count(unigrams, word)),
        num_top_corrs = num_top_corrs / nrow(unigrams)
        # num_top_corrs = num_top_corrs / nrow(count(unigrams, word))
      )
    testthat::expect_true(nrow(unigrams_corrs) >= nrow(unigrams_corrs_2))

    viz_corrs_network <-
      unigrams %>%
      visualize_corrs_network(
        token = word,
        feature = timestamp,
        num_top_ngrams = num_top_ngrams,
        num_top_corrs = num_top_corrs
      )
    viz_corrs_network
    testthat::expect_true(ggplot2::is.ggplot(viz_corrs_network))
  })


testthat::test_that(
  "tfidf",
  {
    unigrams_tfidf <-
      unigrams %>%
      compute_tfidf(
        token = word,
        doc = yyyy
      )
    actual <- unigrams_tfidf$word[1]
    expect <- c("resistivity")
    testthat::expect_equal(actual, expect)

    unigrams_tfidf <-
      unigrams %>%
      dplyr::filter(yyyy == 2017) %>%
      dplyr:: mutate(hh4 = ceiling((hh + 1) / 4)) %>%
      compute_tfidf(
        token = word,
        doc = hh4
      )
    actual <- unigrams_tfidf$word[1]
    expect <- c("displaying")
    testthat::expect_equal(actual, expect)

    viz_unigrams_tfidf <-
      unigrams %>%
      visualize_tfidf(
        token = word,
        doc = yyyy,
        scale_manual_params = list(values = colors_div),
        labs_params = list(title = "Highest TF-IDF Tokens", subtitle = "By Hour")
      )
    viz_unigrams_tfidf
    testthat::expect_true(ggplot2::is.ggplot(viz_unigrams_tfidf))
  })


testthat::test_that(
  "change",
  {
    unigrams_change <-
      unigrams %>%
      compute_change(
        token = word,
        timebin = timestamp,
        timefloor = "year"
      )
    expect <- c("excel", "nutrition", "vba")
    actual <- unigrams_change$word[1:3]
    testthat::expect_equal(actual, expect)

    unigrams_change <-
      unigrams %>%
      compute_change(
        token = word,
        timebin = yyyy,
        bin = FALSE
      )
    actual <- unigrams_change$word[1:3]
    testthat::expect_equal(actual, expect)

    viz_unigrams_change <-
      unigrams %>%
      visualize_change(
        token = word,
        timebin = timestamp,
        timefloor = "year",
        add_labels = TRUE,
        scale_manual_params = list(values = colors_div)
      )
    viz_unigrams_change
    testthat::expect_true(ggplot2::is.ggplot(viz_unigrams_change))
  })


