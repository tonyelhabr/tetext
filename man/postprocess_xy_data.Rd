% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/by2-helpers.R
\name{postprocess_xy_data}
\alias{postprocess_xy_data}
\title{Post-process data in \code{wrapper_func()}}
\usage{
postprocess_xy_data(data = NULL, xy_info = NULL)
}
\arguments{
\item{data}{data.frame. Output from \code{do.call(...)}.}

\item{xy_info}{list. Output from \code{get_xy_info()}.}
}
\value{
data.frame.
}
\description{
Post-process \code{data} using \code{xy_info}.
}
\details{
To be used exclusively by \code{wrapper_func()}.
Some extra logic is needed in case the expected names do not appear in \code{data}
after \code{do.call(...)}. Output column names cannot be specified.
}
