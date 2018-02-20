
temisc <img src="man/figures/logo.png" align="right"/>
======================================================

Introduction
------------

This package containts functions that I often use.

### Installation

`devtools::install_github("tonyelhabr/temisc")`.

Notes
-----

Here is a list of all functions in the package.

Here are some short descriptions. Functions are listed in order of recommended use in a script.

-   **time:** `visualize_time_at()`, `visualize_time_multi_at()`, `visualize_time_batch()`: Visualize data over time.
-   **tidify:** `tidify_to_unigrams_at()`, `tidify_to_bigrams_at()`: Tokenize data to tidy format with unigrams/bigrams.
-   **cnts:** `visualize_cnts_at()`, `visualize_cnts_multi_at()`, `visualize_cnts_wordcloud_at()`, `visualize_cnts_wordcloud_multi_at()`: Visualize counts of n-grams.
-   **freqs:** `compute_freqs_at()`, `compute_freqs_multi_at()`, `visualize_bigrams_freqs_multi_at()`: Compute and visualize frequencies of n-grams.
-   **corrs:** `compute_corrs_at()`, `visualize_corrs_network_at()`: Compute and visualize pairwise correlations (of bigrams).
-   **tfidf:** `compute_tfidf_at()`, `visualize_tfidf_at()`: Compute and visualize change in n-gram usage across documents.
-   **change:** `compute_change_at()`, `visualize_change_at()`: Compute and visualize change in n-gram usage across documents.

### To Add?

#### In Progress

-   `get_xy_grid()`, `filter_xy_grid()`, `preprocess_xy_data()`, `postprocess_xy_data()`, `wrapper_func()`, `add_dummy_cols()`: See the functions with the same names in my `tetweets` project.

#### Somewhat Likely

-   `model_lda()`, `visualize_lda_betas()`, `visualize_lda_gammas()`

#### Low Priority

-   `compute_freqs_wide()`: See `compute_unigrams_freqs()` in my `tetweets` project. Then create `compute_freqs_wide_multi()` using `wrapper_funct()`.
-   `compute_logratios()`: See the function with the same name in my `tetweets` project. Then create `compute_logratios_multi()`using `wrapper_funct()`.
-   `create_sents_ratios_wide()`, `visualize_sents_ratios()`: See the functions with the same names in my `tetweets` project.

#### Very Unlikely

-   ~~`compute_sentdiff_poisson()`, `prepare_sents_diffs_poisson()`, `visualize_sents_diffs_poission()`~~

Examples
--------
