

#' Compute correlations
#'
#' @description Compute correlations among pairs of words.
#' @details Call \code{widyr::pairwise_cor()} internally.
#' Is called by \code{visualize_corrs_network_at()}, so there is no need to call this directly.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param word character. Name of column in \code{data} to use as \code{item}
#' in \code{widyr::pairwise_cor()}.
#' @param feature character. Name of column in \code{data} to use as \code{feature}
#' in \code{widyr::pairwise_cor()}. No default is provided
#' @param num_top_ngrams numeric. Useful primarily to prevent \code{widyr::pairwise_cor()}
#' from hanging up. If between 0 and 1, then interpreted as a percentage.
#' @param num_top_corrs numeric. Useful primarily to limit input to a network visualation.
#' does not hang up. If between 0 and 1, then interpreted as a percentage.
#' @param return_corrs logical. Whether to return just the correlations and word pairs,
#' but not the word counts. This is the default option.
#' @param return_words logical. Whether to return the counts used to compute
#' the correlations. Useful when creating a network visualization.
#' @param return_both logical. Useful when creating a network visualization so that
#' words can be used as nodes and correlations can be used weights.
#' @return data.frame.
#' @rdname compute_corrs
#' @export
#' @seealso \url{http://varianceexplained.org/r/seven-fav-packages/}.
compute_corrs_at <-
  function(data = NULL,
           word = "word",
           feature = NULL,
           num_top_ngrams = 50,
           num_top_corrs = 50,
           return_corrs = TRUE,
           return_words = FALSE,
           return_both = FALSE) {
    if(is.null(data))
      stop("`data` cannot be NULL.", call. = FALSE)
    if(is.null(word))
      stop("`word` cannot be NULL.", call. = FALSE)
    if(is.null(feature))
      stop("`feature` cannot be NULL.", call. = FALSE)

    word_quo <- rlang::sym(word)
    feature_quo <- rlang::sym(feature)

    word <- feature <- correlation <- n <- NULL

    data_cnt_top <-
      data %>%
      dplyr::count(!!word_quo, sort = TRUE) %>%
      filter_num_top_at("n", num_top_ngrams)

    data_joined <-
      data %>%
      dplyr::semi_join(data_cnt_top, by = word)

    data_joined_renamed <-
      data_joined %>%
      dplyr::rename(
        word = !!word_quo,
        feature = !!feature_quo
      )

    data_corrs <-
      data_joined_renamed %>%
      widyr::pairwise_cor(
        word,
        feature,
        sort = TRUE,
        upper = FALSE
      )

    data_corrs_top <-
      data_corrs %>%
      filter_num_top_at("correlation", num_top_corrs)


    if(return_both | (return_words & return_corrs)) {
      out <- list(words = data_cnt_top, corrs = data_corrs_top)
    } else if (return_corrs) {
      out <- data_corrs_top
    } else if (return_words) {
      out <- data_cnt_top
    }
    out
  }

#' @rdname compute_corrs
#' @export
compute_corrs <- compute_corrs_at

#' Visualize correlations
#'
#' @description Visualize correlations with a network
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param ... dots. Parameters passed directly to \code{compute_corrs_at()}.
#' @param resize_points logical. Indicates whether or not to make size of points
#' correspond to count of words.
#' @param size_point character. Name of column in \code{data} to use for point sizing.
#' @param add_point_labels logical. Indicates whether or not to add labels to points.
#' @param color_point character. Hex value of color_value for points.
#' @param shape_point numeric.
#' @param seed numeric. Used to by \code{ggraph::ggraph}.
#' @inheritParams visualize_time
#' @return gg.
#' @rdname visualize_corrs_network
#' @export
#' @seealso \url{https://www.tidytextmining.com/ngrams.html}.
#' \url{http://varianceexplained.org/r/seven-fav-packages/}.
visualize_corrs_network_at <-
  function(...,
           resize_points = TRUE,
           size_point = "n",
           add_point_labels = TRUE,
           color_point = "grey50",
           shape_point = 21,
           seed = 42,
           lab_title = "Network of Pairwise Correlations",
           lab_subtitle = NULL,
           lab_caption = NULL,
           lab_x = NULL,
           lab_y = NULL,
           theme_base = theme_tetext(void = TRUE)) {

    require_ns("ggraph")

    corrs <-
      compute_corrs_at(
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
          ggplot2::aes_string(size = size_point),
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
      label <- "name"
      viz <-
        viz +
        ggraph::geom_node_text(ggplot2::aes_string(label = label), repel = TRUE)
    }

    viz_labs <-
      ggplot2::labs(
        x = lab_x,
        y = lab_y,
        title = lab_title,
        subtitle = lab_subtitle,
        caption = lab_caption
      )
    viz_theme <-
      theme_base

    viz <-
      viz +
      viz_labs +
      viz_theme
    viz
  }

#' @rdname visualize_corrs_network
#' @export
visualize_corrs_network <- visualize_corrs_network_at
