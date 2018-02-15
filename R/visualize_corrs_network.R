

#' Visualize correlations
#'
#' @description Visualize correlations with a network
#' @param ... dots. Parameters passed directly to \code{compute_corrs()}.
#' @param resize_points logical. Indicates whether or not to make size of points
#' correspond to count of words.
#' @param colname_size_point character. Name of column in \code{data} to use for point sizing.
#' @param add_point_labels logical. Indicates whether or not to add labels to points.
#' @param color_point character. Hex value of color for points. Default is provided.
#' @param shape_point numeric. Default is provided.
#' @param seed numeric. Used to by \code{ggraph::ggraph}. Default is provided.
#' @inheritParams visualize_time
#' @return gg
#' @export
#' @importFrom ggplot2 theme_void labs theme aes_string
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text
visualize_corrs_network <-
  function(...,
           resize_points = TRUE,
           colname_size_point = "n",
           add_labels = TRUE,
           color_point = "grey50",
           shape_point = 21,
           seed = 42,
           lab_title = "Network of Pairwise Correlations",
           lab_subtitle = NULL,
           theme_base = ggplot2::theme_void()) {

    corrs <-
      compute_corrs(
        ...,
        return_both = TRUE
      )
    data_proc <-
      igraph::graph_from_data_frame(
        d = corrs$corrs,
        vertices = corrs$words
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
      ggplot2::theme(
        legend.position = "none"
      )

    set.seed(seed)
    viz <-
      ggraph::ggraph(data_proc, layout = "fr") +
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
      # NOTE: This is from `data_proc`, not `data`.
      colname_label <- "name"
      viz <-
        viz +
        ggraph::geom_node_text(ggplot2::aes_string(label = colname_label), repel = TRUE)
    }

    viz <-
      viz +
      viz_labs +
      viz_theme
    viz
  }
