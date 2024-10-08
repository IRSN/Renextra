% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R
\name{coSd}
\alias{coSd}
\title{Extract Model Coefficients along with their Standard
    Deviation}
\usage{
coSd(object, ...)
}
\arguments{
\item{object}{An object typically representing a fitted model from
which one can extract (estimated) coefficients and their
standard deviation.}

\item{...}{Further arguments for methods.}
}
\description{
\code{coSd} is a generic function which extracts
    coefficients and their standard deviation from objects
    returned by modelling functions.
}
\details{
The result will typically be given a specific S3 class in
    order to print or plot.
}
