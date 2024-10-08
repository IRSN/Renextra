% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplotRenouv.R
\name{autolayer.Renouv}
\alias{autolayer.Renouv}
\title{Create a \code{ggplot} Layer Appropriate to a Particular Data Type}
\usage{
\method{autolayer}{Renouv}(
  object,
  level = 0.95,
  which = c("quant", "conf", "allObs", "emptyOTS"),
  posOptions = NULL,
  byBlockStyle = TRUE,
  ...
)
}
\arguments{
\item{object}{A \code{Renouv} object.}

\item{level}{Confidence level. Used only when \code{which} is
\code{"conf"}.}

\item{which}{The layer to show, see \bold{Details}.}

\item{posOptions, byBlockStyle}{See \code{\link{autoplot.Renouv}}.}

\item{...}{Further argument to be passed to the \code{geom_} function.}
}
\description{
Create a \code{ggplot} layer appropriate to represent
    part of the information stored in a \code{Renouv} object.
}
\details{
OLD CODE!!!


   \itemize{
        \item{\code{"quant"} }{Show the quantile (a.k.a return level) line.}
        \item{\code{"conf"} }{Show the confidence interval(s).}
        \item{\code{"allObs"} }{Show all the observations: OT and historical \code{MAX} or
        \code{OTS}.}
        \item{\code{"emptyOS"} }{Show the empty OTS blocks as horizontal segments.}
   }
}
\examples{
example(Renouv, ask = FALSE, echo = FALSE)
## show the observations only
g <- autoplot(fit3, show = list(OT = TRUE, OTS = FALSE))
## add the quantile/Return Level line
g +  autolayer(fit3, which = "quant")
g <- autoplot(fit3, show = list(conf = TRUE), lev = c(0.70, 0.95))
g +  autolayer(fit3, which = "quant")
g <- autoplot(fit3, show = list(quant = TRUE))
## use a new scale to avoid errors
g +  ggnewscale::new_scale_colour( ) + autolayer(fit3, which = "allObs")
g + autolayer(fit3, which = "emptyOTS")

}
