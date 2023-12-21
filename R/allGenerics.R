
##' 
##' @description \code{coSd} is a generic function which extracts
##'     coefficients and their standard deviation from objects
##'     returned by modelling functions.
##'
##' @details The result will typically be given a specific S3 class in
##'     order to print or plot.
##' 
##' @title Extract Model Coefficients along with their Standard
##'     Deviation
##' 
##' @param object An object typically representing a fitted model from
##'     which one can extract (estimated) coefficients and their
##'     standard deviation.
##' 
##' @param ... Further arguments for methods.
##'
##' @export
##' 
coSd <- function(object, ...)  {
    UseMethod("coSd")
}
