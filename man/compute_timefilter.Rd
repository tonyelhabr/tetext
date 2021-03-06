% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim.R
\name{compute_timefilter_facet_at}
\alias{compute_timefilter_facet_at}
\alias{compute_timefilter_facet}
\alias{compute_timefilter_at}
\alias{compute_timefilter}
\title{Add time-related columns}
\usage{
compute_timefilter_facet_at(data = NULL, timebin = NULL, facet = NULL)

compute_timefilter_facet(..., timebin, facet)

compute_timefilter_at(data = NULL, timebin = NULL, facet = NULL)

compute_timefilter(..., timebin, facet)
}
\arguments{
\item{data}{data.frame.}

\item{timebin}{bare for NSE; character for SE. Name of column in \code{data} to use for time axis.}

\item{facet}{bare for NSE; character for SE. Name of column in \code{data} used for facetting.}

\item{...}{dots. Parameters to pass directly to \code{visualize_time()}.}
}
\value{
list.

list.
}
\description{
Adds useful columns for subsequent time calculations and filtering.
}
\details{
Calls \code{compute_timefilter_facet_at()}. (Adds a dummy \code{facet} column if none is specified.)
Adds columns for years, months, days, and hours elapsed.
(Calls in \code{compute_time_elapsed()} facetple times.)
Returns list, where \code{data} is augmented data.frame,
\code{date_start} is the very LAST Date-time value, and \code{date_end}
is the very FIRST Date-time value. The \code{date_start} and \code{date_end}
values are defined in this manner such that all data for each \code{facet} variable
fits in a singular frame (i.e. there are not 'gaps' where data exists
for one \code{facet} variable and not all others). This determination assumes
that the data is continuous.
}
