% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplotRenouvTList.R
\name{autoplot.RenouvTList}
\alias{autoplot.RenouvTList}
\title{Generate a \code{ggplot} from a \code{RenouvTList} Object}
\usage{
\method{autoplot}{RenouvTList}(
  object,
  show = list(quant = TRUE, conf = TRUE, allObs = TRUE),
  predOptions = NULL,
  posOptions = NULL,
  byBlockStyle = NULL,
  facets,
  ...
)
}
\arguments{
\item{object}{A \code{RenouvTList} object.}

\item{show}{A list with its elements named \code{"quant"},
\code{"conf"} and \code{"allObs"}.}

\item{predOptions}{Alist of arguments to be passed to the method
\code{\link{predict.RenouvTList}}.}

\item{posOptions}{Options for the plotting positions.}

\item{byBlockStyle}{XXX}

\item{facets}{Logical. If \code{TRUE} the returned ggplot should
be if possible a trellis graph with one facet for each
threshold. The choice may not be possible to achieve either
because there are too many threshold values or because the
graph must show the confidence intervals.}

\item{...}{Not used yet.}
}
\description{
Generate a \code{ggplot} from a \code{RenouvTList}
    object. The plot shows the return-level curve using a
    log-scale for the return period.  It also displays the
    (possibly censored) observations with suitable plotting
    positions, and confidence intervals on the return levels.
}
\seealso{
\code{\link{autoplot.coef.RenouvTList}} and
    \code{\link{autoplot.coSd.RenouvTList}} to (auto)plot the
    estimated coefficients possibly with confidence intervals and
    \code{\link{autoplot.coef.RenouvTList}} to (auto) plot the
    return levels possibly with confidence intervals.
}
