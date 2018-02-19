

#' Compute correlations
#'
#' @description Compute correlations among pairs of words.
#' @details Call \code{widyr::pairwise_cor()} internally.
#' Is called by \code{visualize_corrs_network()}, so there is no need to call this directly.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param colname_word character. Name of column in \code{data} to use as \code{item}
#' in \code{widyr::pairwise_cor()}. Default is provided.
#' @param colname_feature character. Name of column in \code{data} to use as \code{feature}
#' in \code{widyr::pairwise_cor()}. No default is provided
#' @param num_top_ngrams numeric. Useful primarily to prevent \code{widyr::pairwise_cor()}
#' from hanging up. Default is provided.
#' @param num_top_corrs numeric. Useful primarily to limit input to a network visualation.
#' does not hang up. Default is provided.
#' @param return_corrs logical. Whether to return just the correlations and word pairs,
#' but not the word counts. This is the default option.
#' @param return_words logical. Whether to return the counts used to compute
#' the correlations. Useful when creating a network visualization.
#' @param return_both logical. Useful when creating a network visualization so that
#' words can be used as nodes and correlations can be used weights.
#' @return data.frame.
#' @rdname compute_corrs
#' @export
#' @importFrom dplyr count mutate row_number desc filter semi_join rename
#' @importFrom widyr pairwise_cor
#' @seealso \url{http://varianceexplained.org/r/seven-fav-packages/}.
compute_corrs <-
  function(data = NULL,
           colname_word = "word",
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

    word <- feature <- correlation <- NULL

    colname_word_quo <- rlang::sym(colname_word)
    colname_feature_quo <- rlang::sym(colname_feature)

    data_cnt_top <-
      dplyr::count(data, !!colname_word_quo, sort = TRUE) %>%
      dplyr::mutate(rank = dplyr::row_number(dplyr::desc(n))) %>%
      dplyr::filter(rank <= num_top_ngrams)

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

    data_corrs_top <-
      data_corrs %>%
      dplyr::mutate(rank = dplyr::row_number(dplyr::desc(correlation))) %>%
      dplyr::filter(rank <= num_top_corrs)

    if(return_both | (return_words & return_corrs)) {
      out <- list(words = data_cnt_top, corrs = data_corrs_top)
    } else if (return_corrs) {
      out <- data_corrs_top
    } else if (return_words) {
      out <- data_cnt_top
    }
    out
  }


#' Visualize correlations
#'
#' @description Visualize correlations with a network
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param ... dots. Parameters passed directly to \code{compute_corrs()}.
#' @param resize_points logical. Indicates whether or not to make size of points
#' correspond to count of words.
#' @param colname_size_point character. Name of column in \code{data} to use for point sizing.
#' @param add_point_labels logical. Indicates whether or not to add labels to points.
#' @param color_point character. Hex value of color for points. Default is provided.
#' @param shape_point numeric. Default is provided.
#' @param seed numeric. Used to by \code{ggraph::ggraph}. Default is provided.
#' @inheritParams visualize_time
#' @return gg.
#' @rdname compute_corrs
#' @export
#' @importFrom ggplot2 theme_void labs theme aes_string
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text
#' @seealso \url{https://www.tidytextmining.com/ngrams.html}.
#' \url{http://varianceexplained.org/r/seven-fav-packages/}.
visualize_corrs_network <-
  function(...,
           resize_points = TRUE,
           colname_size_point = "n",
           add_point_labels = TRUE,
           color_point = "grey50",
           shape_point = 21,
           seed = 42,
           lab_title = "Network of Pairwise Correlations",
           lab_subtitle = NULL,
           lab_x = NULL,
           lab_y = NULL,
           theme_base = ggplot2::theme_void()) {

    corrs <-
      compute_corrs(
        ...,
        return_both = TRUE
      )
    data_viz <-
      igraph::graph_from_data_frame(
        d = corrs$corrs,
        vertices = corrs$words
      )

    set.seed(seed)
    viz <-
      ggraph::ggraph(data_viz, layout = "fr") +
      ggraph::geom_edge_link(edge_width = 1)

    if (resize_points) {
      viz <-
        viz +
        ggraph::geom_node_point(
          ggplot2::aes_string(size = colname_size_point),
          fill = color_point,
          shape = shape_point
        )
    } else {
      viz <-
        viz +
        ggraph::geom_node_point(fill = color_point, shape = shape_point)
    }

    if (add_point_labels) {
      # NOTE: This is from `data_viz`, not `data`.
      colname_label <- "name"
      viz <-
        viz +
        ggraph::geom_node_text(ggplot2::aes_string(label = colname_label), repel = TRUE)
    }

    viz_labs <-
      ggplot2::labs(
        x = lab_x,
        y = lab_y,
        title = lab_title,
        subtitle = lab_subtitle
      )
    viz_theme <-
      theme_base +
      ggplot2::theme(
        legend.position = "none"
      )

    viz <-
      viz +
      viz_labs +
      viz_theme
    viz
  }


