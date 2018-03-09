

#' Compute correlations
#'
#' @description Compute correlations among pairs of words.
#' @details Calls \code{widyr::pairwise_cor()} internally.
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @param token bare for NSE; character for SE. Name of column in \code{data} to use as \code{item}
#' in \code{widyr::pairwise_cor()}.
#' @param feature bare for NSE; character for SE. Name of column in \code{data} to use as \code{feature}
#' in \code{widyr::pairwise_cor()}.
#' @param num_top_ngrams numeric. Useful primarily to prevent \code{widyr::pairwise_cor()}
#' from hanging up. If between 0 and 1, then interpreted as a percentage.
#' @param num_top_corrs numeric. Useful primarily to limit input to a network visualation.
#' does not hang up. If between 0 and 1, then interpreted as a percentage.
#' @param adjust logical. Indicates whether or not to scale \code{num_top_ngrams} and
#' \code{num_top_corrs} (if they are between 0 and 1) to the count of unique words.
#' Is is likely that the input percentage values are specified with the number of words in mind.
#' @param return_corrs logical. Whether to return just the correlations and token pairs,
#' but not the token counts. This is the default option.
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
           token = NULL,
           feature = NULL,
           num_top_ngrams = 50,
           num_top_corrs = 50,
           adjust = FALSE,
           return_corrs = TRUE,
           return_words = FALSE,
           return_both = FALSE) {
    stopifnot(!is.null(data), is.data.frame(data))
    stopifnot(!is.null(token), is.character(token))
    stopifnot(!is.null(feature), is.character(token))

    token_quo <- rlang::sym(token)
    feature_quo <- rlang::sym(feature)

    token <- feature <- correlation <- n <- NULL

    data_cnt <-
      data %>%
      dplyr::count(!!token_quo, sort = TRUE)


    if(num_top_ngrams < 1) {
      if(adjust) {
        num_top_ngrams <- num_top_ngrams * (nrow(data_cnt) / nrow(data))
        message("Adjusting `num_top_ngrams`.")
      }
    }
    if(num_top_corrs < 1) {
      if(adjust) {
        num_top_corrs <- num_top_corrs * (nrow(data_cnt) / nrow(data))
        message("Adjusting `num_top_corrs`.")
      }
    }

    data_cnt_top <-
      data_cnt %>%
      filter_num_top_at("n", num_top_ngrams)

    data_joined <-
      data %>%
      dplyr::semi_join(data_cnt_top, by = token)

    data_corrs <-
      data_joined %>%
      dplyr::rename(
        token = !!token_quo,
        feature = !!feature_quo
      ) %>%
      widyr::pairwise_cor(
        token,
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
compute_corrs <-
  function(...,
           token,
           feature) {
    stopifnot(!missing(token))
    stopifnot(!missing(feature))
    token <- rlang::quo_text(rlang::enquo(token))
    feature <- rlang::quo_text(rlang::enquo(feature))
    compute_corrs_at(..., token = token, feature = feature)
  }

#' Visualize correlations
#'
#' @description Visualize correlations with a network
#' @details It's not straightforward to create \code{_base} and \code{_params} arguments
#' for this function to map to the \code{ggraph} arguments, so explicit arguments
#' are used (i.e. \code{color_point} \code{shape_point}
#' @inheritParams visualize_time
#' @inheritParams visualize_cnts
#' @inheritParams compute_corrs
#' @param ... dots. Parameters passed directly to \code{compute_corrs()}.
#' @param resize_points logical. Indicates whether or not to make size of points
#' correspond to count of words.
#' @param size_point bare for NSE; character for SE. Name of column in \code{data} to use for point sizing.
#' @param add_point_labels logical. Indicates whether or not to add labels to points.
#' @param color_point character. Hex value of color_value for points.
#' @param shape_point numeric.
#' @param seed numeric. Used to initialize \code{ggraph::ggraph()}.
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
           labs_base = default_labs(),
           labs_params = list(title = "Network of Word Pairwise Correlations"),
           theme_base = default_theme(void = TRUE),
           theme_params = list()) {

    require_ns("ggraph")

    corrs <-
      compute_corrs_at(
        ...,
        return_both = TRUE
      )
    data_proc <-
      igraph::graph_from_data_frame(
        d = corrs$corrs,
        vertices = corrs$words
      )

    set.seed(seed)

    viz <-
      data_proc %>%
      ggraph::ggraph(layout = "fr") +
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
      # NOTE: This is from `data_proc`, not `data`.
      label <- "name"
      viz <-
        viz +
        ggraph::geom_node_text(ggplot2::aes_string(label = label), repel = TRUE)
    }

    viz <-
      viz +
      generate_labs(labs_base, labs_params) +
      generate_theme(theme_base, theme_params)
  }

#' @rdname visualize_corrs_network
#' @export
visualize_corrs_network <-
  function(...,
           token,
           feature) {
    stopifnot(!missing(token))
    stopifnot(!missing(feature))
    token <- rlang::quo_text(rlang::enquo(token))
    feature <- rlang::quo_text(rlang::enquo(feature))
    visualize_corrs_network_at(..., token = token, feature = feature)
  }
