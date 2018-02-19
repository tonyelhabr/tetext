

context("functions")
require("bindrcpp")
require("dplyr")
require("viridisLite")
require("temisc")


dir <- file.path("tests")
filename_data <- "data-tony-cleaned.rds"
filename_unigrams <- "unigrams-tony.rds"
filename_bigrams <- "bigrams-tony.rds"
if(interactive()) {
  dir.exists(file.path("inst", dir))
} else {
  print(dir.exists(dir))
}
filepath_data <-
  system.file(dir, filename_data, package = "tetext", mustWork = TRUE)
filepath_unigrams <-
  system.file(dir, filename_unigrams, package = "tetext", mustWork = TRUE)
filepath_bigrams <-
  system.file(dir, filename_bigrams, package = "tetext", mustWork = TRUE)
# file.exists(filepath_unigrams)

data <- readRDS(file = filepath_data)
unigrams <- readRDS(file = filepath_unigrams)
bigrams <- readRDS(file = filepath_bigrams)

colors_viridis <- viridisLite::viridis(option = "D", n = 10)
color_main <- "firebrick"
get_rpal_byname <- function(name) {
  paste0(name, c("", as.character(seq(1, 4, 1))))
}
colors_main <- get_rpal_byname(color_main)
colors_te <- temisc::colors_te

testhat::test_that("change", {

  unigrams_bytime <-
    compute_change(
      data = unigrams,
      colname_timebin = "timestamp",
      timefloor = "year",
      top_pct = 0.05
    )

  expect <- c("excel", "nutrition", "vba")
  actual <- unigrams_bytime$word[1:3]
 testthat::expect_equal(actual, expect)
})

testhat::test_that("cnts", {
  viz_cnts <-
    visualize_cnts(
      data = unigrams,
      num_top = 10,
      colname_color = NULL,
      color = "grey50"
    )
  viz_cnts

  viz_cnts_multi <-
    visualize_cnts_multi(
      data = unigrams,
      num_top = 10,
      colname_color = "yyyy",
      color = colors_te,
      lab_subtitle = "By Year",
      colname_multi = "yyyy"
    )
  viz_cnts_multi

  viz_cnts_wordcloud <-
    visualize_cnts_wordcloud(
      data = unigrams,
      color = colors_main,
      num_top = 50
    )

  # TODO: Generate wordclouds for all 'multi' values.
  viz_cnts_wordcloud_multi <-
    visualize_cnts_wordcloud_multi(
      data = unigrams,
      colname_word = "word",
      color = colors_main,
      num_top = 50,
      colname_multi = "yyyy",
      value_multi = 2011
    )
}

testhat::test_that("corrs", {
  num_top_ngrams <- 50
  num_top_corrs <- 50
  unigrams_corrs <-
    compute_corrs(
      data = unigrams,
      num_top_ngrams = num_top_ngrams,
      num_top_corrs = num_top_corrs,
      colname_feature = "timestamp"
    )
  actual <- unigrams_corrs$item1[1:3]
  expect <- c("excel", "austin", "oracle")
 testthat::expect_equal(actual, expect)
  actual <- unigrams_corrs$item2[1:3]
  expect <- c("vba", "ut", "sql")
 testthat::expect_equal(actual, expect)

  viz_corrs_network <-
    visualize_corrs_network(
      data = unigrams,
      num_top_ngrams = num_top_ngrams,
      num_top_corrs = num_top_corrs,
      colname_feature = "timestamp"
    )
  viz_corrs_network
}

testhat::test_that("freqs", {
  bigrams_freqs <-
    compute_freqs(
      data = bigrams
    )
  actual <- bigrams_freqs$word[1:3]
  expect <- c("excel vba", "ut austin", "pl sql")
 testthat::expect_equal(actual, expect)

  viz_bigram_freqs_multi <-
    visualize_bigram_freqs_multi(
      data = bigrams,
      colname_multi = "yyyy",
      color = colors_te
    )
  viz_bigram_freqs_multi
}

testhat::test_that("tfidf", {
  unigrams_tfidf <-
    compute_tfidf(
      data = unigrams,
      colname_doc = "yyyy"
    )
  actual <- unigrams_tfidf$word[1]
  expect <- c("resistivity")
  testthat::expect_equal(actual, expect)

  viz_unigrams_tfidf_multi <-
    visualize_tfidf_multi(
      data = unigrams,
      colname_doc = "yyyy",
      color = colors_te,
      lab_subtitle = "By Year"
    )
  viz_unigrams_tfidf_multi
}

