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
##' @param effDuration,distname.y The effective duration and the
##'     distribution name to be passed to \code{\link{Renouv}}. These
##'     must be numeric/character vectors with length one. In other
##'     words the same effective duration and the same distribution is
##'     used for all thresholds.
##' 
##' @param loopIni Logical. If \code{TRUE} the initial values of the
##'     parameters for a threshold are derived from the known
##'     converged cases obtained before. This is mainy useful in the
##'     cas where historical data are used, because no convergence
##'     problems should arise as long as no historical/censored
##'     observations are used. \bold{NOT IMPLEMENTED YET}.
##' 
##' @param start.par.y Used to provide the parameter names and initial
##'     values in \code{\link[Renext]{Renouv}} when the distribution
##'     is not one of the special distributions. This can be either a
##'     single vector of initial values to be used for all thresholds,
##'     or a collection of initial values, one by threshold. In the
##'     first case, \code{start.par.y} must be a named numeric vector
##'     or a named list to be passed as the \code{start.par.y}
##'     argument of \code{\link{Renouv}}. In the second case, one must
##'     use either a numeric matrix with \code{length(threshold)}
##'     rows, or a list of \code{length(threshold)} numeric vectors or
##'     lists. The argument to be passed to \code{\link{Renouv}} will
##'     then be given by looping over the rows of the matrix or over
##'     the elements of the list.
##'
##' @param ... Further arguments to be passed to
##'     \code{\link{Renouv}}. Graphical arguments should not be used.
##' 
##' @param effDuration,distname.y Effective duration and distribution
##'     See \code{\link{Renouv}}.
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
##' autoplot(coef(fit))
##'
##' ## Use more thresholds and compare with the jittered data
##' fit <- RenouvTList(Garonne,
##'                    threshold = seq(from = 2401, to = 3001, by = 10),
##'                    distname.y = "GPD")
##' autoplot(coef(fit))
##' fitJit <- RenouvTList(GaronneJit,
##'                       threshold = seq(from = 2401, to = 3001, by = 10),
##'                       distname.y = "GPD")
##' autoplot(coef(fitJit))
##'
##' ## Use an Extended GP Distribution fo the excesses. To get initial values
##' ## for the parameters, we add a column of ones to the coefficients of
##' ## 'fitJit'. Note that we must use `reParam = FALSE` to match the names
##' ## of the parameters of the Renext GPD.
##' fitJit0 <- RenouvTList(GaronneJit,
##'                        threshold = seq(from = 2401, to = 3001, by = 50),
##'                        distname.y = "GPD")
##' spy <- cbind(kappa = 1.0, coef(fitJit0, reParam = FALSE))
##' spy <- spy[ , c("scale", "shape", "kappa")]
##' \dontrun{
##' fitJit1 <- RenouvTList(GaronneJit,
##'                        threshold = seq(from = 2401, to = 3001, by = 50),
##'                        distname.y = "EGPD3", start.par.y = spy)
##' autoplot(fitJit1)
##' }
RenouvTList <- function(x,
                        threshold = NULL,
                        effDuration = NULL,
                        distname.y = "GPD",
                        loopIni = FALSE,
                        start.par.y = NULL,
                        ...) {

    threshold <- sort(threshold)
    
    if (!is.null(spy <- start.par.y)) {
        if (!is.null(u <- attr(spy, "threshold"))) {
            if (!all.equal(u, threshold)) {
                warning("'start.par.y' has a \"threshold\" attribute",
                        "which differs from the given value of 'threshold'")
            }
        }
        if (inherits(spy, "matrix")) {
            if (nrow(spy) != length(threshold)) {
                stop("if 'start.par.y' is given and is a matrix",
                     " it must have `length(threshold)' rows")
            }
            spy <- lapply(seq_len(nrow(spy)), function(i) spy[i, ])
        } else if (is.numeric(spy) || (is.list(spy) && !all(sapply(spy, is.list)))) {
            spyL <- list()
            for (iu in seq_along(threshold)) spyL[[iu]] <- spy
            names(spyL) <- paste0("threshold = ", threshold) 
            spy <- spyL
        } else if (!is.list(spy) || length(spy) != length(threshold)) {
            stop("Bad value for 'start.par.y'. See help for the accepted ",
                 "types of values")
        }
    }
    

    res <- list()
    
    suppressWarnings({
        for (i in seq_along(threshold)) {        
            res[[i]] <- Renouv(x = x,
                               threshold = threshold[i],
                               effDuration = effDuration,
                               distname.y = distname.y,
                               plot = FALSE,
                               start.par.y = spy[[i]],
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
##' @param sd Logical. If \code{TRUE} the result is a list of
##'     \emph{two} matrices: one for the estimates and one for the
##'     standard deviations of these, a.k.a. the \emph{standard
##'     errors}.
##' 
##' @param ... Not used yet.
##'
##' @return A numeric matrix with its rows corresponding to the
##'     thresholds.
##'
##' @section Caution: the confidence intervals are obtained by using
##'     the "delta" method. The intervals are known to have a coverage
##'     rate which is smaller than the expected rate when the
##'     Generalised Pareto Distribution is used.
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
coef.RenouvTList <- function(object, reParam, lambda = TRUE, sd = TRUE, ...) {
    
    u <- attr(object, "threshold")
    dn <- sapply(object, function(x) x$distname.y)
    if (length(dn <- unique(dn)) > 1) {
        stop("all elements of 'object' must have the same ",
             "'distname.y' element")
    }
    
    if (missing(reParam)) {
        reParam <- ifelse(dn %in% c("GPD"), TRUE, FALSE)
    }
    
    est <- t(sapply(object, coef))
    if (sd) sigma <- t(sapply(object, function(x) x$sigma))
    
    if (!lambda) {
        ind <- colnames(est) != "lambda"
        est <- est[ , ind]
        if (sd) {
            ind <- colnames(sigma) != "lambda"
            sigma <- sigma[ , ind]
        }
    }
    
    if (reParam) {
        if (dn %in% c("GPD")) {
            est[ , "scale"] <- est[ , "scale"] - u * est[ , "shape"]
            colnames(est) <- sub("^scale$", "scale ind", colnames(est))

            if (sd) {
                Sigma <- lapply(object, vcov)
                
                for (i in seq_along(u)) {
                    sigma[i, "scale"] <- sqrt(Sigma[[i]]["scale", "scale"] -
                                              2 * u[i] * Sigma[[i]]["scale", "shape"] +
                                              u[i]^2 * Sigma[[i]]["shape", "shape"])
                }
                colnames(sigma) <- sub("^scale$", "scale ind", colnames(est))
            }
        } else {
            stop("'reParam' can only be 'TRUE' when the distribution ",
                 "is \"GPD\"")
        }
    }
    if (sd) {
        attr(est, "sd") <- TRUE
        attr(est, "sigma") <- sigma
    } else {
        attr(est, "sd") <- FALSE
    }
    
    nb.OT <- sapply(object, \(x) x$nb.OT)
    
    attr(est, "threshold") <- attr(object, "threshold")
    attr(est, "nb.OT") <- nb.OT
    attr(est, "lambda") <- as.logical(lambda)
    
    class(est) <- "coef.RenouvTList"
    est
    
}

## *****************************************************************************

##' @method print coef.RenouvTList
##' @export
##' 
print.coef.RenouvTList <- function(x, ...) {
    attr(x, "threshold") <- NULL

    if (!attr(x, "sd")) {
        class(x) <- "matrix" 
        x <- round(x, digits = 3)
        attr(x, "sd") <- NULL
        print(x)
    } else {
        mat <- array("", dim = dim(x), dimnames = dimnames(x))
        for (i in 1:ncol(mat)) {
            mat[ , i] <- sprintf("%6.3f [%5.3f]",
                                 x[ , i], attr(x, "sigma")[ , i])
        }
        print(noquote(mat))
    }
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

    .Deprecated("coef(object, sd = TRUE)")
    
    u <- attr(object, "threshold")
    dn <- sapply(object, function(x) x$distname.y)
    if (length(dn <- unique(dn)) > 1) {
        stop("all elements of 'object' must have the same ",
             "'distname.y' element")
    }
    if (missing(reParam)) {
        reParam <- ifelse(dn %in% c("GPD"), TRUE, FALSE)
    }
    est <- t(sapply(object, coef))
    sigma <- t(sapply(object, function(x) x$sigma))
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
                    u[i]^2 * Sigma[[i]]["shape", "shape"])
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
   
    ## The largest rate should be the first,  
    lambdaMax <- max(coef(object)[ , "lambda"])
    logGrid <- seq(from = -log(lambdaMax, base = 10) + 1e-6,
                   to = log(1100, base = 10),
                   length.out = 100)
    
    newdata <- 10^logGrid

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
    x$coSd <- coef(object, sd = TRUE)
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
    cat("o Kolmogorov-Smirnov test\n")
    print(x$KS)
}
