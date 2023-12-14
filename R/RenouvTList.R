##' @description Fit a \code{Renouv} or POT model for several
##'     candidate thresholds. The result is an object having the S3
##'     class \code{"RenouvTList"} for which several methods have been
##'     implemented in \pkg{Renextra}.
##'
##' @details Although the classical methods like \code{coef} are
##'     mainly obtained with \code{lapply} or \code{sapply}, the
##'     result is in some cases given a special class that allows
##'     further invetigation. See the \bold{Example} section.
##'
##' @title \code{Renouv} or POT Models with several Thresholds
##'
##' @param x A numeric vector or a suitable \code{Rendata} object, see
##'   \code{\link{Renouv}}.
##'
##' @param threshold A numeric vector of thresholds.
##' 
##' @param effDuration,distname.y Effective duration and distribution
##'     See \code{\link{Renouv}}.
##'
##' @param loopIni Logical. If \code{TRUE} the initial values are This
##'     is because no convergence problems should arise as long as no
##'     historical/censored observations are used.
##' 
##' @param ... Further arguments to be passed to
##'     \code{\link{Renouv}}. Graphical arguments should not be used.
##' 
##' @return An object with (S3) class \code{"RenouvTList"}. This S3
##'     class has a number of methods.
##'
##' @section Caution: the default distribution for the exceedances is
##'     taken to be the usual two-parameter Generalised Pareto
##'     \code{"GPD"} rather than exponential.
##' 
##' @export
##' 
##' @examples
##' fit <- RenouvTList(Garonne,
##'                    threshold = seq(from = 2401, to = 3001, by = 100),
##'                    distname.y = "GPD")
##'
##' methods(class = "RenouvTList")
##' autoplot(predict(fit))
##' fitJit <- RenouvTList(GaronneJit,
##'                       threshold = seq(from = 2401, to = 3001, by = 100),
##'                       distname.y = "GPD")
##' autoplot(predict(fitJit))
##' 
RenouvTList <- function(x,
                        threshold = NULL,
                        effDuration = NULL,
                        distname.y = "GPD",
                        ...) {
    
    threshold <- sort(threshold)

    res <- list()
    
    for (i in seq_along(threshold)) {
        res[[i]] <- Renouv(x = x, threshold = threshold[i],
                           distname.y = distname.y,
                           plot = FALSE,
                           ...)
    }

    names(res) <- paste0("u = " , format(threshold))
    attr(res, "threshold") <- threshold
    class(res) <- c("RenouvTList", "RenouvList")
    res

}

## *****************************************************************************

##' @title Coefficients of a \code{RenouvTList} Object
##' 
##' @param object An object with class \code{"RenouvTList"} as created
##'     by using \code{\link{RenouvTList}}.
##'
##' @param ... Not used yet.
##'
##' @return A numeric matrix with its rows corresponding to the
##'     thresholds.
##'
##' @export
##' @method coef RenouvTList
##'
##' @examples
##' fit <- RenouvTList(Garonne,
##'                    threshold = seq(from = 2401, to = 3001, by = 10),
##'                    distname.y = "GPD")
##' autoplot(coef(fit))
##' 
coef.RenouvTList <- function(object, reParam = TRUE, ...) {
    u <- attr(object, "threshold")
    mat <- t(sapply(object, coef))
    dn <- sapply(object, function(x) x$distname.y)
    if (length(dn <- unique(dn)) > 1) {
        stop("all elements of 'object' must have the same ",
             "'distname.y' element")
    }
    if (reParam) {
        if (dn == "GPD") {
            mat[ , "scale"] <- mat[ , "scale"] - u * mat[ , "shape"]
            colnames(mat)[2] <- c("scale ind.")
        } else {
            stop("'reParam' can only be 'TRUE' when the distribution ",
                 "is \"GPD\"")
        }
    } 
    
    attr(mat, "threshold") <- attr(object, "threshold")
    class(mat) <- "coef.RenouvTList"
    mat
}

## *****************************************************************************

##' @method print coef.RenouvTList
##' @export
##' 
print.coef.RenouvTList <- function(x, ...) {
    attr(x, "threshold") <- NULL
    class(x) <- "matrix"
    x <- round(x, digits = 3)
    print(x)
}

## *****************************************************************************


coSd.RenouvTList <- function(object, ...)  {

    coSdR <- function(x) {
        sprintf("%6.3f [%5.3f]", x[["estimate"]], x[["sigma"]])
    }
    mat <- noquote(t(sapply(fit, coSdR)))
    colnames(mat) <- names(coef(object[[1]]))
    mat
}

## *****************************************************************************

##' @description Compute a data frame of return levels for the all the
##'     \code{Renouv} objects stored in \code{object}.
##'
##' @details The returned object is given a special class in order to
##'     allow the use of some S3 methods such as \code{autoplot}.
##' 
##' @title Predict Method for \code{RenouvTList} Objects
##'
##' @param object An object with class \code{"RenouvTList"}.
##'
##' @param ... Further arguments to be passed to \code{predict}.
##'
##' @return An object with class \code{"predict.RenouvTList"}
##'     inheriting from \code{"data.frame"}
##'
##' @method predict RenouvTList
##' @export
##' 
predict.RenouvTList <- function(object,
                                newdata,
                                level = 0.95,
                                ## format = c("long", "wide"),
                                ...) {
    
    noData <- (missing(newdata) || is.null(newdata))
    if (noData) {
        newdata <- as.vector(outer(c(1, 2, 3, 5, 7), c(1, 10, 100, 1000)))
    }
    
    predLong <- function(object, newdata, level, ...) {
        p <- predict(object, newdata = newdata, level = level, ...)
        names(p) <- c("Period", "Quantile", "L", "U")
        cbind(p, Threshold = object$threshold, Level = level)
    }

    pred <- list()
    for (i in seq_along(level)) {
        p <- lapply(object, predLong, newdata = newdata, level = level[i], ...)
        pred[[i]] <- as.data.frame(rbindlist(p))
    }
    
    pred <- as.data.frame(rbindlist(pred))
    class(pred) <- c("predict.RenouvTList", "data.frame")
    pred
}

summary.RenouvTList <- function(object, ...) {
    
    
}
