

compute_corrs <-
  function(data = NULL,
           colname_word = NULL,
           colname_feature = NULL,
           num_top_ngrams = 50,
           num_top_corrs = 50,
           return_corrs = TRUE,
           return_words = FALSE,
           return_both = FALSE) {
    if(is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    if(is.null(colname_word))
      stop("`colname_word` cannot be NULL.", call. = FALSE)
    if(is.null(colname_feature))
      stop("`colname_feature` cannot be NULL.", call. = FALSE)

    colname_word_quo <- rlang::sym(colname_word)
    colname_feature_quo <- rlang::sym(colname_feature)

    data_cnt <- dplyr::count(data, !!colname_word_quo, sort = TRUE)

    data_cnt_ranked <-
      dplyr::mutate(data_cnt, rank = dplyr::row_number(dplyr::desc(n)))
    data_cnt_top <-
      dplyr::filter(data_cnt_ranked, rank <= num_top_ngrams)

    data_joined <-
      dplyr::semi_join(data, data_cnt_top, by = colname_word)

    data_joined_renamed <-
      dplyr::rename(
        data_joined,
        word = !!colname_word_quo,
        feature = !!colname_feature_quo
      )

    data_corrs <-
      widyr::pairwise_cor(
        data_joined_renamed,
        word,
        feature,
        sort = TRUE,
        upper = FALSE
      )

    data_corrs_ranked <-
      dplyr::mutate(data_corrs, rank = dplyr::row_number(dplyr::desc(correlation)))

    data_corrs_top <-
      dplyr::filter(data_corrs_ranked, rank <= num_top_corrs)

    if(return_both | (return_words & return_corrs)) {
      out <- list(words = data_cnt_top, corrs = data_corrs_top)
    } else if (return_corrs) {
      out <- data_corrs_top
    } else if (return_words) {
      out <- data_cnt_top
    }
    out
  }

