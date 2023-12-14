##' @description This data set is derived from
##'     \code{\link[Renext]{Garonne}} and is similarly provided as a
##'     \code{"Rendata"} object. The difference with \code{Garonne} is
##'     that the observations have been "jitterized" to weaken the
##'     effect of the rounding errors.
##'
##' @details The observations of the main sample (a.k.a. \code{OT}
##'     observations) have been jitterized by adding a normal centred
##'     noise with standard deviation of 30 cumec
##'     (\eqn{\textsf{m}^3/\textsf{s}}). For the historical \code{MAX}
##'     observations a centred normal noise with standard deviation
##'     \code{60} cumec has been used. There is some evidence that the
##'     \code{OT} observations of \code{Garonne} are rounded to a (the
##'     next?)  mutliple of \eqn{10} cumec while the \code{MAX}
##'     observations are rounded to a (the next?)  mutliple of \eqn{100}
##'     cumec. This creates weird phenomena when the POT threshold is
##'     changed.
##'
##' @title Jitterized Version of the \code{Garonne} Data
##'
##' @usage GaronneJit
##' 
##' @name GaronneJit
##'
##' @docType data
##'
##' @importFrom stats rnorm
##' 
##' @seealso \code{\link[Renext]{Garonne}}
##' 
##' @keywords data
##'
##' @examples
##' 
##' table(Garonne$OTdata$Flow %% 10)
##' table(GaronneJit$OTdata$Flow %% 10)
##'
##' table(Garonne$MAXdata$Flow %% 100)
##' table(GaronneJit$MAXdata$Flow %% 100)
##' 
##' 
"GaronneJit"
