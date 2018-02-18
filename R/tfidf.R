

#' Compute TF-IDF
#'
#' @description Compute term-frequency, inverse-document-frequency.
#' @details Calls \code{tidytext::bind_tf_idf()} internally.
#' @inheritParams visualize_time
#' @param colname_word character. Name of column in \code{data} to use
#' as \code{term} in \code{tidytext::bind_tf_idf()}. Default is provided.
#' @param colname_doc character. Name of column in \code{data} to use
#' as \code{document} in \code{tidytext::bind_tf_idf()}. Default is provided.
#' @export
#' @importFrom dplyr count
#' @importFrom tidytext bind_tf_idf
compute_tfidf <-
  function(data = NULL,
           colname_word = "word",
           colname_doc = "document") {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)

    colname_word_quo <- rlang::sym(colname_word)
    colname_doc_quo <- rlang::sym(colname_doc)

    n <- NULL

    out <-
      data %>%
      dplyr::count(!!colname_doc_quo, !!colname_word_quo, sort = TRUE) %>%
      tidytext::bind_tf_idf(!!colname_word_quo, !!colname_doc_quo, n)
    out
  }

#' Visualize TF-IDF
#'
#' @description Visualize term-frequency, inverse-document-frequency.
#' @details Does not call \code{compute_tfidf()} internally.
#' @inheritParams visualize_cnts
#' @param colname_tfidf character Name of column in \code{data} to use for y-axis. Default is provided
#' @export
#' @importFrom dplyr group_by top_n ungroup mutate
#' @importFrom drlib reorder_within scale_x_reordered
#' @importFrom stringr str_to_title
#' @importFrom temisc theme_te_a_facet
#' @importFrom ggplot2 labs theme element_blank ggplot aes_string geom_col scale_fill_manual facet_wrap coord_flip
#' @importFrom stats as.formula
#' @seealso \url(https://www.tidytextmining.com/tfidf.html}.
#' \url{https://juliasilge.com/blog/sherlock-holmes-stm/}
visualize_tfidf_multi <-
  function(data = NULL,
           colname_word = "word",
           colname_tfidf = "tfidf",
           colname_multi = "document",
           num_top = 10,
           colname_color = colname_multi,
           color = "grey50",
           lab_title = "Highest TF-IDF Words",
           lab_subtitle = paste0("By ", stringr::str_to_title(colname_multi)),
           theme_base = temisc::theme_te_a_facet()) {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)

    colname_word_quo <- rlang::sym(colname_word)
    colname_tfidf_quo <- rlang::sym(colname_tfidf)
    colname_multi_quo <- rlang::sym(colname_multi)

    if (is.null(colname_color)) {
      data$color <- "dummy"
      colname_color <- "color"
    }

    data_viz <-
      data %>%
      dplyr::group_by(!!colname_multi_quo) %>%
      dplyr::top_n(num_top, !!colname_tfidf_quo) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!colname_multi_quo := factor(!!colname_multi_quo)) %>%
      dplyr::mutate(
        !!colname_word_quo :=
          drlib::reorder_within(colname_word_quo, !!colname_tfidf_quo, !!colname_multi_quo)
      )

    viz_labs <-
      ggplot2::labs(
        x = NULL,
        y = NULL,
        title = lab_title,
        subtitle = lab_subtitle
      )
    viz_theme <-
      theme_base +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_blank())

    viz <-
      data_viz %>%
      ggplot2::ggplot(ggplot2::aes_string(x = colname_word, y = colname_tfidf, fill = colname_color)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = color) +
      ggplot2::facet_wrap(stats::as.formula(paste0("~ ", colname_multi)), scales = "free") +
      drlib::scale_x_reordered() +
      ggplot2::coord_flip() +
      viz_labs +
      viz_theme

    viz
  }
