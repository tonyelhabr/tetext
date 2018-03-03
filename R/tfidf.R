

#' Compute TF-IDF
#'
#' @description Compute term-frequency, inverse-document-frequency.
#' @details Calls \code{tidytext::bind_tf_idf()} internally. Note that the
#' output 'tfidf' column is named \code{tf_idf} because this is the convention
#' of the \code{tidytext} package.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param word character. Name of column in \code{data} to use
#' as \code{term} in \code{tidytext::bind_tf_idf()}.
#' @param doc character. Name of column in \code{data} to use
#' as \code{document} in \code{tidytext::bind_tf_idf()}.
#' @return data.frame.
#' @rdname compute_tfidf
#' @export
compute_tfidf_at <-
  function(data = NULL,
           word = "word",
           doc = "document") {
    if (is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)

    word_quo <- rlang::sym(word)
    doc_quo <- rlang::sym(doc)

    n <- tf_idf <- NULL

    out <-
      data %>%
      dplyr::count(!!doc_quo, !!word_quo, sort = TRUE) %>%
      tidytext::bind_tf_idf(!!word_quo, !!doc_quo, n) %>%
      dplyr::arrange(!!doc_quo, dplyr::desc(tf_idf))
    out
  }

#' @rdname compute_tfidf
#' @export
compute_tfidf <- compute_tfidf_at

#' Visualize TF-IDF
#'
#' @description Visualize term-frequency, inverse-document-frequency.
#' @details Calls \code{compute_tfidf_at()} internally.
#' Cannot pass dots to \code{compute_tfidf_at()} internally because parameters
#' are used in subsequent processing.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @inheritParams compute_tfidf
#' @return gg.
#' @rdname visualize_tfidf
#' @export
#' @seealso \url{https://www.tidytextmining.com/tfidf.html}.
#' \url{https://juliasilge.com/blog/sherlock-holmes-stm/}
visualize_tfidf_at <-
  function(data = NULL,
           word = "word",
           doc = "document",
           multi = doc,
           num_top = 10,
           color = multi,
           color_value = "grey50",
           lab_title = "Highest TF-IDF Words",
           lab_subtitle = paste0("By ", stringr::str_to_title(multi)),
           theme_base = theme_tetext_facet()) {
    data_proc <-
      compute_tfidf_at(
        data = data,
        word = word,
        doc = doc
      )

    word_quo <- rlang::sym(word)
    # tfidf_quo <- rlang::sym(tfidf)
    multi_quo <- rlang::sym(multi)

    tf_idf <- rank <- NULL

    data_viz <-
      data_proc %>%
      dplyr::group_by(!!multi_quo) %>%
      filter_num_top_at("tf_idf", num_top) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        !!word_quo := reorder_within(!!word_quo, tf_idf, !!multi_quo)
      )

    if (is.null(color)) {
      data_viz <- data_viz %>% dplyr::mutate(`.dummy` = "dummy")
      color <- ".dummy"

    }
    data_viz <- wrangle_color_col(data_viz, color)

    viz <-
      data_viz %>%
      ggplot2::ggplot(ggplot2::aes_string(x = word, y = "tf_idf", fill = color)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = color_value) +
      ggplot2::facet_wrap(stats::as.formula(paste0("~ ", multi)), scales = "free") +
      scale_x_reordered() +
      ggplot2::coord_flip()


    viz_labs <-
      ggplot2::labs(
        x = NULL,
        y = NULL,
        title = lab_title,
        subtitle = lab_subtitle
      )
    viz_theme <-
      theme_base +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_blank()
      )

    viz <-
      viz +
      viz_labs +
      viz_theme

    viz
  }

#' @rdname visualize_tfidf
#' @export
visualize_tfidf <- visualize_tfidf_at


