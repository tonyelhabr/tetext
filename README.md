
temisc <img src="man/figures/logo.png" align="right"/>
======================================================

Introduction
------------

This package containts functions that I often use.

### Installation

`devtools::install_github("tonyelhabr/temisc")`.

Notes
-----

### Inspiration

This package is heavily influenced by the blogs of [David Robinson](http://varianceexplained.org/posts/) and [Julia Silge](juliasilge.com/blog/), as well as their co-authored book [*Text Mining with R*](https://www.tidytextmining.com/). Most of the functions in this package implement snippets of code that they have shared.

### Syntax

This package follows the `dplyr` convention of suffixing standard evaluation (SE) function with `_at`. These functions expect characters (as inputs) to indicate column names.

There are non-suffixed versions of all major functions that serve as aliases to the `_at` SE functions. In the future, if non-standard evaluation (NSE) is implemented, the non-suffixed versions will be altered to refer to the NSE versions. (This would comply with the `dplyr` convetion for NSE functions)

Here is a list of all functions in the package.

Here are some short descriptions of the functions, grouped generally by usage. Functions are listed in order of recommended use in a script (and in the order in which I wrote them).

-   **time:** `visualize_time_at()`, `visualize_time_multi_at()`, `visualize_time_batch()`: Visualize data over time.
-   **tidify:** `tidify_to_unigrams_at()`, `tidify_to_bigrams_at()`: Tokenize data to tidy format with unigrams or bigrams.
-   **cnts:** `visualize_cnts_at()`, `visualize_cnts_multi_at()`, `visualize_cnts_wordcloud_at()`, `visualize_cnts_wordcloud_multi_at()`: Visualize counts of n-grams.
-   **freqs:** `compute_freqs_at()`, `compute_freqs_multi_at()`, `visualize_bigrams_freqs_multi_at()`, `compute_freqs_multi_by2_at()`, `visualize_freqs_multi_by2_at()`: Compute and visualize frequencies of n-grams.
-   **corrs:** `compute_corrs_at()`, `visualize_corrs_network_at()`: Compute and visualize pairwise correlations (of bigrams).
-   **tfidf:** `compute_tfidf_at()`, `visualize_tfidf_at()`: Compute and visualize change in n-gram usage across documents.
-   **change:** `compute_change_at()`, `visualize_change_at()`: Compute and visualize change in n-gram usage across documents.
-   **sents:** `compute_sent_summary_at()`, `compute_sent_summary_multi_at()` Compute sentiment scores for n-grams.
-   **xy:** `compute_freqs_multi_by2_at()`, `visualize_freqs_multi_by2_at()`, `compute_logratios_multi_by2_at()`, `visualize_logratios_multi_by2_at()`, `compute_sentratios_multi_by2_at()`, `visualize_sentratios_multi_by2_at()`, Compute metrics across multiple `multi` entities. Uses a handful of internal function that are not intended to be called directly (although they can be). These internal functions include the following: `create_xy_grid()`, `filter_xy_grid()`, `preprocess_xy_data()`, `postprocess_xy_data()`, `wrapper_func()`, `add_dummy_cols()`. Also, there are more specific internal functions, such as: `compute_freqs_multi_wide_at()`, `compute_logratios_multi_wide_at()`, `compute_sentratios_multi_wide_at()`

### Notes To Self

#### TODO

Add `lab_caption` for visualization functions.

-   ~~**model:**~~ `model_lda()`, `visualize_lda_betas()`, `visualize_lda_gammas()`
-   ~~**poisson:**~~ `compute_sentdiff_poisson()`, `prepare_sents_diffs_poisson()`, `visualize_sents_diffs_poission()`~~

Examples
--------

For now, see the tests.
