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
##' @param loopIni Logical. If \code{TRUE} the initial values of the
##'     parameters for a threshold are derived from the known
##'     converged cases obtained before. This is mainy useful in the
##'     cas where historical data are used, because no convergence
##'     problems should arise as long as no historical/censored
##'     observations are used. \bold{NOT IMPLEMENTED YET}.
##' 
##' @param ... Further arguments to be passed to
##'     \code{\link{Renouv}}. Graphical arguments should not be used.
##' 
##' @return An object with (S3) class \code{"RenouvTList"}. This S3
##'     class has a number of methods.
##'
##' @note The warnings generated when iteratively calling
##'     \code{\link[Renext]{Renouv}} are suppressed.
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
##' autoplot(coSd(fit))
##'
##' ## Use more thresholds and compare with the jittered data
##' fit <- RenouvTList(Garonne,
##'                    threshold = seq(from = 2401, to = 3001, by = 10),
##'                    distname.y = "GPD")
##' autoplot(coSd(fit))
##' fitJit <- RenouvTList(GaronneJit,
##'                       threshold = seq(from = 2401, to = 3001, by = 10),
##'                       distname.y = "GPD")
##' autoplot(coSd(fitJit))
RenouvTList <- function(x,
                        threshold = NULL,
                        effDuration = NULL,
                        distname.y = "GPD",
                        loopIni = FALSE,
                        ...) {
    
    threshold <- sort(threshold)

    res <- list()
    
    suppressWarnings({
        for (i in seq_along(threshold)) {        
            res[[i]] <- Renouv(x = x,
                               threshold = threshold[i],
                               effDuration = effDuration,
                               distname.y = distname.y,
                               plot = FALSE,
                               ...)
            
        }
    })
    
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
##' @param reParam Logical. Can only be used when the distribution is
##'     \code{"GPD"}. If \code{TRUE} the GPD scale parameter
##'     \eqn{\sigma} is replaced by the modified scale
##'     \eqn{\sigma^\star := \sigma - u \xi}, where \eqn{u} is the
##'     threshold and \eqn{\xi} is the GPD shape. The parameter
##'     \eqn{\sigma^\star} does not depend on the threshold. Remind
##'     that the distribution is specified by using the
##'     \code{distname.y} argument of the creator \code{RenouvTList}.
##'
##' @param lambda Logical. If \code{TRUE} the Poisson rate is included
##'     in the results. The choice \code{FALSE} is relevant for
##'     threshold stability analyzes.
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
coef.RenouvTList <- function(object, reParam, lambda = TRUE, ...) {
    
    u <- attr(object, "threshold")
    dn <- sapply(object, function(x) x$distname.y)
    if (length(dn <- unique(dn)) > 1) {
        stop("all elements of 'object' must have the same ",
             "'distname.y' element")
    }
    
    if (missing(reParam)) {
        reParam <- ifelse(dn %in% c("GPD"), TRUE, FALSE)
    }
    
    mat <- t(sapply(object, coef))
    if (!lambda) {
        ind <- colnames(mat) != "lambda"
        mat <- mat[ , ind]
    }
    
    if (reParam) {
        if (dn %in% c("GPD")) {
            mat[ , "scale"] <- mat[ , "scale"] - u * mat[ , "shape"]
            colnames(mat) <- sub("^scale$", "scale ind", colnames(mat))
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

##' @description Extract the estimated coefficients and their standard
##'     deviation as an object with class \code{"coSd.RenouvTList"} for
##'     which some methods are available such as \code{autoplot} and
##'     \code{print}.
##' 
##' @details The POT parameters of a \code{Renouv} object with a GP
##'     distribution of the eceedances are named \code{"lambda"},
##'     \code{"scale"} and \code{"shape"}. If a reparameterization is
##'     used \code{reParam = TRUE}, the modified GP scale
##'     \eqn{\sigma^\star} is named \code{scale.ind} to recall the
##'     independence from the threshold. Note that although the
##'     \code{print} method displays the object as a matrix, it
##'     actually consists in a list of two matrices.
##' 
##' @title Estimated Coefficients and their Standard Deviation
##'
##' @param object A \code{RenouvTList} object.
##' 
##' @param reParam Logical. Can only be used when the distribution is
##'     \code{"GPD"}. If \code{TRUE} the GPD scale parameter
##'     \eqn{\sigma} is replaced by the modified scale
##'     \eqn{\sigma^\star := \sigma - u \xi}, where \eqn{u} is the
##'     threshold and \eqn{\xi} is the GPD shape. The parameter
##'     \eqn{\sigma^\star} does not depend on the threshold. Remind
##'     that the distribution is specified by using the
##'     \code{distname.y} argument of the creator \code{RenouvTList}.
##'
##' @param lambda Logical. If \code{TRUE} the Poisson rate is included
##'     in the results. The choice \code{FALSE} is relevant for
##'     threshold stability analyzes.
##' 
##' @param ... Not used yet.
##'
##' @references Coles S. (2001). \emph{An Introduction to Statistical
##'     Modeling of Extreme Values} Springer-Verlag.
##' 
##' @method coSd RenouvTList
##'
##' @importFrom stats vcov
##' @export
coSd.RenouvTList <- function(object, reParam, lambda = TRUE,  ...)  {

    u <- attr(object, "threshold")
    dn <- sapply(object, function(x) x$distname.y)
    if (length(dn <- unique(dn)) > 1) {
        stop("all elements of 'object' must have the same ",
             "'distname.y' element")
    }
    
    if (missing(reParam)) {
        reParam <- ifelse(dn  %in% c("GPD"), TRUE, FALSE)
    }
    est  <- t(sapply(object, coef))
    sigma  <- t(sapply(object, function(x) x$sigma))
    ## sigma  <- t(sapply(object, function(x) sqrt(diag(vcov(x)))))
    if (!lambda) {
        ind <- colnames(est) != "lambda"
        est <- est[ , ind]
        ind <- colnames(sigma) != "lambda"
        sigma <- sigma[ , ind]
     }
    
    if (reParam) {
        if (dn %in% c("GPD")) {
            Sigma <- lapply(object, vcov)
            est[ , "scale"] <- est[ , "scale"] - u * est[ , "shape"]
            for (i in seq_along(u)) {
                sigma[i, "scale"] <- sqrt(Sigma[[i]]["scale", "scale"] -
                                          2 * u[i] * Sigma[[i]]["scale", "shape"] +
                    u[i]^2 *  Sigma[[i]]["shape", "shape"])
            }
            colnames(est) <- colnames(sigma) <- sub("^scale$", "scale ind", colnames(est))
        } else {
            stop("'reParam' can only be 'TRUE' when the distribution ",
                 "is \"GPD\"")
        }
    } 
    
    res <- list(est = est, sigma = sigma)
    attr(res, "threshold") <- u
    class(res) <- "coSd.RenouvTList"
    res
}

##' @method print coSd.RenouvTList
##' @export
##' 
print.coSd.RenouvTList <- function(x, ...) {
    mat <- array("", dim = dim(x$est), dimnames = dimnames(x$est))
    
    for (i in 1:ncol(mat)) {
        mat[ , i] <- sprintf("%6.3f [%5.3f]",
                             x[["est"]][ , i], x[["sigma"]][ , i])
    }
    print(noquote(mat))
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
##' @param newdata A vector containing the periods at which the return
##'     levels are to be computed. If missing a default choice is
##'     made.
##' 
##' @param level The confidence level.
##' 
##' @param ... Further arguments to be passed to \code{predict}.
##'
##' @return An object with class \code{"predict.RenouvTList"}
##'     inheriting from \code{"data.frame"}
##'
##' @method predict RenouvTList
##' @export
##'
##' @importFrom data.table rbindlist
##' 
predict.RenouvTList <- function(object,
                                newdata,
                                level = 0.95,
                                ## format = c("long", "wide"),
                                ...) {
    
    noData <- (missing(newdata) || is.null(newdata))
    if (noData) {
        newdata <- as.vector(outer(c(1, 2, 3, 5, 7, 10.1), c(1, 10, 100)))
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

##' @export
##' @method summary RenouvTList
##' 
summary.RenouvTList <- function(object, ...) {
    x <- object
    x$coSd <- coSd(object)
    x$KS <- round(t(sapply(object,
                           function(o) c(n = o$nb.OT,
                                         o$KS$stat,
                                         "p.value" = o$KS$p.value))),
                  digits = 4)
    class(x) <- "summary.RenouvTList"
    x
}

##' @export
##' @method print summary.RenouvTList
##' 
print.summary.RenouvTList <- function(x,
                                      digits = max(3, getOption("digits") - 3),
                                      symbolic.cor = x$symbolic.cor,
                                      signif.stars = getOption("show.signif.stars"),
                                      ...) {
    cat("RenouvTList object\n")
    cat("o Estimated coefficients\n")
    print(x$coSd)
    cat("o  Kolmogorov-Smirnov test\n")
    print(x$KS)
}
