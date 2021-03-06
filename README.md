
teproj <img src="man/figures/logo.png" align="right"/>
======================================================

Introduction
------------

This package contains functions that I use for quick and "tidy" text analysis.

### Installation

`devtools::install_github("tonyelhabr/tetext")`.

Notes
-----

### Inspiration

This package is heavily influenced by the blogs of [David Robinson](http://varianceexplained.org/posts/) and [Julia Silge](juliasilge.com/blog/), as well as their co-authored book [*Text Mining with R*](https://www.tidytextmining.com/). Most of the functions in this package implement snippets of code that they have shared.

### Main Functions

~~The following is a list of all functions in the package.~~

The following is a list is a list of the **main** functions in the package. (Not shown: the SE aliases and various helper functions.)

    #>  [1] "compute_change"                 "compute_corrs"                 
    #>  [3] "compute_freqs"                  "compute_freqs_facet"           
    #>  [5] "compute_freqs_facet_by2"        "compute_logratios_facet_by2"   
    #>  [7] "compute_logratios_facet_wide"   "compute_sent_summ"             
    #>  [9] "compute_sent_summ_facet"        "compute_sentratios_facet_by2"  
    #> [11] "compute_tfidf"                  "compute_time_elapsed"          
    #> [13] "compute_timefilter"             "compute_timefilter_facet"      
    #> [15] "tidify_to_bigrams"              "tidify_to_unigrams"            
    #> [17] "trim_bytime"                    "visualize_bigram_freqs"        
    #> [19] "visualize_change"               "visualize_cnts"                
    #> [21] "visualize_cnts_facet"           "visualize_cnts_wordcloud"      
    #> [23] "visualize_cnts_wordcloud_facet" "visualize_corrs_network"       
    #> [25] "visualize_freqs_facet_by2"      "visualize_logratios_facet_by2" 
    #> [27] "visualize_sentratios_facet_by2" "visualize_tfidf"               
    #> [29] "visualize_time"                 "visualize_time_facet"          
    #> [31] "visualize_time_hh"

Here are some short descriptions of the functions, grouped generally by usage. Functions are listed in order of recommended use in a script (and in the order in which I wrote them).

-   **time:** `visualize_time()`, `visualize_time_facet()`, `visualize_time_hh()`: Visualize data over time.
-   **tidify:** `tidify_to_unigrams()`, `tidify_to_bigrams()`: Tokenize data to tidy format with unigrams or bigrams.
-   **cnts:** `visualize_cnts()`, `visualize_cnts_facet()`, `visualize_cnts_wordcloud()`, `visualize_cnts_wordcloud_facet()`: Visualize counts of n-grams.
-   **freqs:** `compute_freqs()`, `compute_freqs_facet()`, `visualize_bigrams_freqs_facet()`, `compute_freqs_facet_by2()`, `visualize_freqs_facet_by2()`: Compute and visualize frequencies of n-grams.
-   **corrs:** `compute_corrs()`, `visualize_corrs_network()`: Compute and visualize pairwise correlations (of bigrams).
-   **tfidf:** `compute_tfidf()`, `visualize_tfidf()`: Compute and visualize change in n-gram usage across documents.
-   **change:** `compute_change()`, `visualize_change()`: Compute and visualize change in n-gram usage across documents.
-   **sents:** `compute_sent_summary()`, `compute_sent_summary_facet()` Compute sentiment scores for n-grams.
-   **xy:** `compute_freqs_facet_by2()`, `visualize_freqs_facet_by2()`, `compute_logratios_facet_by2()`, `visualize_logratios_facet_by2()`, `compute_sentratios_facet_by2()`, `visualize_sentratios_facet_by2()`, Compute metrics across facets `facet` entities. Uses a handful of internal function that are not intended to be called directly (although they can be). These internal functions include the following: `create_xy_grid()`, `filter_xy_grid()`, `preprocess_xy_data()`, `postprocess_xy_data()`, `wrapper_func()`, `add_dummy_cols()`. Also, there are more specific internal functions, such as: `compute_freqs_facet_wide()`, `compute_logratios_facet_wide()`, `compute_sentratios_facet_wide()`

### Function Idioms

All major functions in this function have non-standard evaluation (NSE) and standard evaluation (SE) versions To distinguish the NSE and SE functions, the `dplyr` convention of suffixing standard evaluation (SE) functions with `_at` is used. (These functions expect characters instead of "bare" string to indicate column names.

Main functions used to return data.frames mostly begin with the verb `compute`. Visualization functions that call these functions internally begin with `visualize`. There are various `default_*()` functions (e.g. `default_theme()`, `default_labs()`, `default_facet()`) that are used to specify default arguments in the `visualize_*()` functions--these arguments end with the noun `_base` (e.g. `theme_base`, `labs_base`, `facet_base`, etc.). Accompanying these `_base` arguments are similarly-named `_params` arguments (e.g. `theme_params`, `labs_params`, `facet_params`, etc.). The intended use of this framework is that the `_base` arguments, which default to some `default_*()` function, are not to be altered; instead, this is the purpose of the `_params` arguments. This design is used as a means of providing "good" defaults while also providing a suitable means of customization.

To explain this format more clearly, consider an example of proper usage of these arguments. In the `visualize_cnts()` function, there is a `theme_base` argument set equal to `default_theme()` by default--this function argument defines a "baseline" theme (very similar to `ggplot2::theme_minimal()`) that should not be directly changed. To make modifications to the `default_theme()`, the accompanying `theme_params` argument should be set equal to a names list corresponding to the appropriate `ggplot2::theme()` parameters that are to be customized. For example, one might set `theme_params = list(legend.title = ggplot2::element_text("Legend Title"))`. (The `default_theme()` function sets `ggplot::theme(legend.title = ggplot2::element_blank()`.)

(Notably, the only `_base`/`_params`/`default_*()` combination that does not truly follow this design pattern is that for `scale_manual`. There is a `default_scale_manual()` function for a `scale_manual_params` argument in the visualization function, but no corresponding `_base` argument.)

### Functions to add?

-   ~~**model:**~~ `model_lda()`, `visualize_lda_betas()`, `visualize_lda_gammas()`
-   ~~**poisson:**~~ `compute_sentdiff_poisson()`, `prepare_sents_diffs_poisson()`, `visualize_sents_diffs_poission()`

Examples
--------

Check out [this blog post analyzing the text in *R Weekly posts*](https://tonyelhabr.rbind.io/posts/tidy-text-analysis-rweekly/) to see usage of an earlier version of this package:

(*Warning:* At the time, only SE functions had been implemented. Also, since then, some function names and arguments may have been changed.)

Additionally, see [this blog post analyzing personal Google search history](https://tonyelhabr.rbind.io/posts/tidy-text-analysis-google-search-history/) to see code that are similar to the code used to implement this package's functions:

Finally, the tests files can provide worthwhile examples of function usage
