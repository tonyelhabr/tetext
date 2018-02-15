


visualize_bigrams_forx_freqs <-
  function(data = NULL,
           colname_x = NULL,
           colname_bigram = "bigram",
           colname_freq = "freq",
           num_top = 10) {

    if(is.null(data)) stop("`data` cannot be NULL.", call. = FALSE)
    if(is.null(colname_x)) stop("`colname_x` cannot be NULL.", call. = FALSE)

    colname_x_quo <- rlang::sym(colname_x)
    colname_bigram_quo <- rlang::sym(colname_bigram)
    colname_freq_quo <- rlang::sym(colname_freq)

    bigrams_freqs <-
      data %>%
      group_by(!!colname_x) %>%
      mutate(rank = row_number(desc(freq))) %>%
      filter(rank <= num_top) %>%
      ungroup() %>%
      arrange(!!colname_x) %>%
      mutate(bigram = str_replace_all(bigram, " ", "\n")) %>%
      mutate(bigram = forcats::fct_reorder(factor(bigram), freq))

    bigrams_freqs_byx_viz %>%
      ggplot(aes_string(x = colname_x, y = colname_bigram, color = colname_x, size = colname_freq)) +
      geom_point() +
      scale_y_discrete(position = "right") +
      scale_color_manual(values = params$color_main) +
      scale_size_area(max_size = 25) +
      temisc::theme_te_b() +
      theme(legend.position = "none") +
      coord_flip() +
      labs(x = NULL, y = NULL) +
      labs(title = "Most Frequently Used Pairs of Words", subtitle = "By !!colname_x")
  }


visualize_bigrams_freqs_byx <- function() {

}
num_top <- 10
bigrams_freqs_byx_viz <-
  data %>%
  group_by(!!colname_x) %>%
  mutate(rank = row_number(desc(freq))) %>%
  filter(rank <= num_top) %>%
  ungroup() %>%
  arrange(!!colname_x) %>%
  mutate(bigram = str_replace_all(bigram, " ", "\n")) %>%
  mutate(bigram = forcats::fct_reorder(factor(bigram), freq))

viz_bigrams_freqs_byx <-
  bigrams_freqs_byx_viz %>%
  ggplot(aes(x = !!colname_x, y = bigram, color = !!colname_x, size = freq)) +
  geom_point() +
  scale_y_discrete(position = "right") +
  scale_color_manual(values = params$color_main) +
  scale_size_area(max_size = 25) +
  temisc::theme_te_b() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  labs(title = "Most Frequently Used Pairs of Words", subtitle = "By !!colname_x")
viz_bigrams_freqs_byx