
#'  Custom theme
#'
#' @description A custom \code{ggplot2} theme (that is similar to \code{ggplot2::theme_minimal()}.
#' @details This theme is 'copy-pasted' of theme from \code{teplot} package.
#' @param base_family character.
#' @param base_size,plot_title_size,subtitle_size numeric.
#' @param void logical. Whether or not to modifly theme to mimic \code{ggplot2::theme_void()}/
#' @param ... dots. Additional parameters passed to \code{ggplot2::theme()}.
#' @export
#' @seealso \url{https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r}
theme_tetext <- function (base_family = "",
                          base_size = 11,
                          plot_title_size = 16,
                          subtitle_size = 12,
                          void = FALSE,
                          ...) {
  out <-
    ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  out <-
    out +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank())

  out <-
    out +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )

  out <-
    out +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = plot_title_size,
        face = "bold",
        hjust = 0
      ),
      # plot.subtitle = ggplot2::element_text(size = subtitle_size, face = "bold.italic", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = subtitle_size, hjust = 0),
      plot.caption = ggplot2::element_text(face = "italic"),
      ...
    )

  # out <-
  #   out +
  #   ggplot2::theme(legend.position = "none")

  if(void) {
    out <-
      out +
      ggplot2::theme(
        line = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      ) +
      ggplot2::theme(legend.position = "none")
  }
  out
}

#' @rdname theme_tetext
#' @export
theme_tetext_dx <-
  function(...)
    theme_tetext(panel.grid.major.x = ggplot2::element_blank(), ...)

#' @rdname theme_tetext
#' @export
theme_tetext_facet <-
  function(...)
    theme_tetext(panel.background = ggplot2::element_rect(), ...)


#' @rdname theme_tetext
#' @export
theme_tetext_facet_dx <- function(...)
  theme_tetext(
    panel.background = ggplot2::element_rect(),
    panel.grid.major.x = ggplot2::element_blank(),
    ...
  )


