

context("functions")
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

# file.exists(filepath_unigrams)

data_multi <- readRDS(file = filepath_data_multi)
colors_te <- temisc::colors_te

tms <- c("DAL", "HOU", "MEM", "NOP", "SAS")

message("Tests are in order of likely usage in script.")

data_multi <- clean_tweets(data_multi)
testhat::test_that(
  "time",
  {
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

  })
