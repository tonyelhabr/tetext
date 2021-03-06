% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time.R
\name{visualize_time_hh_at}
\alias{visualize_time_hh_at}
\alias{visualize_time_hh}
\title{Visualize hourly data}
\usage{
visualize_time_hh_at(data = NULL, timebin = NULL, facet = NULL,
  color = facet, scale_manual_base = default_scale_manual(),
  scale_manual_params = list(), labs_base = default_labs(),
  labs_params = list(title = "Count by Hour of Day"),
  theme_base = default_theme(), theme_params = list(panel.grid =
  ggplot2::element_blank()))

visualize_time_hh(..., timebin, color, facet)
}
\arguments{
\item{data}{data.frame.}

\item{timebin}{bare for NSE; character for SE. Name of column in \code{data}
to use for time. Probably likely something like 'hh' or 'hour'.}

\item{facet}{bare for NSE; character for SE. Name of column in \code{data}
to use one of main axes, not for facetting.}

\item{color}{bare for NSE; character for SE. Name of column in \code{data} to use for color basis.
Even if \code{fill} is the actual \code{ggplot2} aesthetic, the internal code will 'coerce' this
column to \code{fill}.
In order to simplify internal code, MUST be specified.}

\item{scale_manual_base}{list. Parameters to pass to \code{ggplot2::scale_fill_manual()} or
\code{ggplot2::scale_color_manual()}, depending on the aesthetic mapping. A default \code{values}
argument is specified in the internal
function \code{default_scale_manual()},
so if overwriting explicitly, then \code{values} should be included in the list.}

\item{scale_manual_params}{list. Additional parameters to pass to \code{ggplot2::scale_fill_manual()}
or \code{ggplot2::scale_color_manual()} (e.g. \code{breaks}, \code{labels}, or \code{name}).
The default \code{values} specified by \code{scale_manual_base = default_scale_manual()}
can be overwritten a \code{values} argument in this list (or directly with a
\code{values} argument in the \code{scale_manual_base} list.}

\item{labs_base}{\code{ggplot2::labs()} function. Defaults to a pre-determined set of values.
It is recommended NOT to modify this argument; isntead, the \code{_params} argument should
be used for customization.}

\item{labs_params}{list. Additional parameters to pass to \code{ggplot2::labs()} to use in addition
to and/or override the parameters set in the \code{_base} parameter.}

\item{theme_base}{\code{ggplot2::theme()} function. (e.g. as \code{ggplot2::theme_minimal()}.)
A custom theme is supplied as a default. As with the the \code{labs_base} argument, it is NOT
recommended to change this directly.}

\item{theme_params}{list. Additional parameters to pass to \code{ggplot2::labs()}.
Should be used in the same manner as \code{labs_params} for customization.}

\item{...}{dots. Parameters to pass directly to \code{visualize_time()}.}
}
\description{
Visualize time-based data aggregated over hours.
}
\details{
This function is 'unique' because it calls \code{ggplot2::geom_violin()}.
Also, it does not actually use \code{facet} for facetting.
}
