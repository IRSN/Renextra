
## ****************************************************************************

##' Generate a \code{ggplot} for a \code{predict.RenouvTList} object.
##'
##' The plot shows the return level against the (log-) return period.
##' This plot is useful to assess the impact of the threshold choice
##' on the return level curve and on the corresponding confidence
##' intervals. However, the number of thresholds should be kept \eqn{\leq
##' 12}.
##' 
##' @title Generate a \code{ggplot} for a \code{predict.RenouvTList}
##'     Object
##' 
##' @param object An object with class \code{"predict.RenouvTList"}.
##' 
##' @param confInt Logical. If \code{TRUE} the confidence intervals or
##'     bands will be shown.
##'  
##' @param facets Logical. If \code{TRUE} a trellis graph is built,
##'     with its facets corresponding to the thresholds. This option
##'     is useful when the number of thresholds is moderate, say
##'     \eqn{\leq 12}.
##'
##' @param ... Not used yet
##'
##' @return A \code{ggplot} object.
##'
##' @export
##' @method autoplot predict.RenouvTList
##' 
##' @examples
##' fit <- RenouvTList(Garonne,
##'                    threshold = seq(from = 2401, to = 3001, by = 100),
##'                    distname.y = "GPD")
##' autoplot(predict(fit))
##' \dontrun{
##'     library(mev)
##'     fit <- RenouvTList(nidd,
##'                        effDuration = 35,
##'                        threshold = seq(from = 65.08, to = 88.61, len = 40),
##'                        distname.y = "GPD")
##'      fitE <- RenouvTList(nidd,
##'                        effDuration = 35,
##'                        threshold = seq(from = 65.08, to = 88.61, len = 40),
##'                        start.par.y = c(scale = 30, shape = 0.0, kappa = 1.0),
##'                        distname.y = "EGPD3")
##'      autoplot(coef(fitE, lambda = FALSE, sd = TRUE))
##' }
autoplot.predict.RenouvTList <- function(object,
                                         confInt = FALSE,
                                         facets,
                                         ...) {

    Threshold <- Period <- Quantile <- Level <- L <- U <- NULL
    
    if (missing(facets)) {
        facets <- ifelse(confInt, TRUE, FALSE)
    }

    threshFact <- FALSE
    if (length(unique(object$Threshold)) <= 12) {
        object <- within(object, Threshold <- factor(Threshold))
        threshFact <- TRUE
    } else {
        if (facets) {
            warning("'facets = TRUE' is only possible with <= 12 threshold values")
            facets <- FALSE
        }
    }
    
    if (confInt) {
        lev <- format(object$Level)
        lev <- factor(lev, levels = sort(unique(lev), decreasing = TRUE))
        object$Level <- lev
    }
    
    g <- ggplot(data = object)
    if (confInt) {
        if (facets) {
            g <- g + geom_ribbon(mapping = aes(x = Period, ymin = L, ymax = U,
                                               fill = Level, linetype = Level),
                                 colour = "darkgray")
            g <- g + scale_fill_manual(values = translude(c("SteelBlue1", "SteelBlue3"),
                                                          alpha = 0.3))
        } else {
            if (nlevels(object$Level) > 1) {
                stop("When 'facets' is FALSE only one confidence level is allowed")
            }
            g <- g + geom_line(mapping = aes(x = Period, y = L,
                                             group = Threshold, colour = Threshold),
                               linetype = 2)
            g <- g + geom_line(mapping = aes(x = Period, y = U,
                                             group = Threshold, colour = Threshold),
                               linetype = 2)
        }
    }
    
    if (facets) {
        g <- g + geom_line(mapping = aes(x = Period, y = Quantile,
                                         group = Threshold))
    } else {
        g <- g + geom_line(mapping = aes(x = Period, y = Quantile,
                                         group = Threshold, colour = Threshold))
    }
    
    ## g <- g + geom_hline(mapping = aes(yintercept = Threshold))
    
    g <- g + scale_x_log10()
    
    if (threshFact) {
        g <- g + scale_colour_brewer(palette = "Spectral")
        if (facets) {
            g <- g + facet_wrap(. ~ Threshold, labeller = label_both)
        }
    }
    g
}

## ****************************************************************************

##' @details Build a \code{ggplot} object for the coefficients of a
##'     \code{RenouvTList} object. If the \code{coef.RenouvTList} was
##'     called with \code{sd = TRUE} then the confidence intervals on
##'     the coefficients will be shown as a ribbon.
##'
##' @title Build a \code{ggplot} for the Coeficients of a
##'     \code{RenouvTList} Object
##'
##' @param object An object with class \code{"coef.RenouvTList"} as
##'     created by applying the \code{\link{coef.RenouvTList}} method
##'     to a \code{RenouvTList} object.
##'
##' @param facets Logical. It \code{TRUE} each parameter is shown in
##'     one facet of a trellis graphics.
##'
##' @param ... Not used yet.
##' 
##' @return A \code{ggplot} object.
##'
##' @seealso \code{\link{coef.RenouvTList}}.
##' 
##' @method autoplot coef.RenouvTList
##' @export
##'
##' @examples
##' fit <- RenouvTList(Garonne,
##'                    threshold = seq(from = 2401, to = 3001, by = 10),
##'                    distname.y = "GPD")
##' fitJit <- RenouvTList(GaronneJit,
##'                       threshold = seq(from = 2401, to = 3001, by = 10),
##'                       distname.y = "GPD")
##' autoplot(coef(fit, sd = TRUE))
##' autoplot(coef(fitJit))
##' autoplot(coef(fitJit, sd = TRUE))
autoplot.coef.RenouvTList <- function(object,
                                      facets,
                                      ...) {

    Threshold <- Value <- Param <- Sd <- NULL

    df <- as.data.frame(unclass(object))
    df <- data.frame(Threshold = attr(object, "threshold"), df)
    df <- tidyr::gather(df, key = "Param", value = "Value", -Threshold)
    
    if (!attr(object, "sd")) {
        g <- ggplot(data = df)
        g <- g + geom_point(mapping = aes(x = Threshold, y = Value, group = Param))
        g <- g + geom_line(mapping = aes(x = Threshold, y = Value, group = Param))
        g <- g + facet_grid(Param ~ ., scales="free_y", labeller = label_both)
        g
    } else {
        dfSigma <- data.frame(Threshold = attr(object, "threshold"),
                              attr(object, "sigma"))
        dfSigma <- tidyr::gather(dfSigma, key = "Param", value = "Sd", -Threshold)
        df <- merge(df, dfSigma, by = c("Threshold", "Param"))
        Two <- qnorm(0.975)
        df <- within(df, {
            L <- Value - Two * Sd;
            U <- Value + Two * Sd
        })
        g <- ggplot(data = df)
        g <- g + geom_ribbon(mapping = aes(x = Threshold, ymin = L, ymax = U,
                                           group = Param), fill = "SteelBlue1",
                             alpha = 0.3)
        g <- g + geom_point(mapping = aes(x = Threshold, y = Value, group = Param))
        g <- g + geom_line(mapping = aes(x = Threshold, y = Value, group = Param))
        g <- g + facet_grid(Param ~ ., scales="free_y", labeller = label_both)
        g
    }

}

## ****************************************************************************

##' @description The estimates of the POT parameters are plotted
##'     against the threshold. These plots are sometimes called
##'     "threshold stability plots".
##' 
##' @title Generate a \code{ggplot} from a \code{coSd.RenouvTList} Object
##' 
##' @param object An object with class \code{"coSd.RenouvTlist"} as
##'     created by applying the \code{coSd} method to a
##'     \code{RenouvTlist} object.
##' 
##' @param facets Logical. If \code{TRUE} the created \code{ggplot}
##'     object is a trellis graphics with one facet for each threshold
##'     value.
##'
##' @param ... Not used yet.
##' 
##' @export
##'
##' @method autoplot coSd.RenouvTList
##' 
##' @importFrom stats qnorm
##'
##' @examples
##' fitJit <- RenouvTList(GaronneJit,
##'                       threshold = seq(from = 2401, to = 3001, by = 100),
##'                       distname.y = "GPD")
##' cs <- coef(fitJit, sd = TRUE)
##' autoplot(cs)
##'
##' 
autoplot.coSd.RenouvTList <- function(object,
                                      facets,
                                      ...) {

    Threshold <- Value <- Param <- Sd <- NULL
    
    dfEst <- data.frame(Threshold = attr(object, "threshold"), object$est)
    dfSigma <- data.frame(Threshold = attr(object, "threshold"), object$sigma)
    dfEst <- tidyr::gather(dfEst, key = "Param", value = "Value", -Threshold)
    dfSigma <- tidyr::gather(dfSigma, key = "Param", value = "Sd", -Threshold)
    df <- merge(dfEst, dfSigma, by = c("Threshold", "Param"))
    Two <- qnorm(0.975)
    
    df <- within(df, {
        L <- Value - Two * Sd;
        U <- Value + Two * Sd
    })

    g <- ggplot(data = df)
    g <- g + geom_ribbon(mapping = aes(x = Threshold, ymin = L, ymax = U,
                                       group = Param), fill = "SteelBlue1",
                         alpha = 0.3)
    g <- g + geom_point(mapping = aes(x = Threshold, y = Value, group = Param))
    g <- g + geom_line(mapping = aes(x = Threshold, y = Value, group = Param))
    g <- g + facet_grid(Param ~ ., scales="free_y", labeller = label_both)
    g
                        
}

## ****************************************************************************

##' @description Generate a \code{ggplot} from a \code{RenouvTList}
##'     object. The plot shows the return-level curve using a
##'     log-scale for the return period.  It also displays the
##'     (possibly censored) observations with suitable plotting
##'     positions, and confidence intervals on the return levels.
##' 
##' @title Generate a \code{ggplot} from a \code{RenouvTList} Object
##'
##' @param object A \code{RenouvTList} object.
##'
##' @param show A list with its elements named \code{"quant"},
##'     \code{"conf"} and \code{"allObs"}.
##'
##' @param predOptions Alist of arguments to be passed to the method
##'     \code{\link{predict.RenouvTList}}.
##'
##' @param posOptions Options for the plotting positions.
##'
##' @param byBlockStyle XXX
##'
##' @param facets Logical. If \code{TRUE} the returned ggplot should
##'     be if possible a trellis graph with one facet for each
##'     threshold. The choice may not be possible to achieve either
##'     because there are too many threshold values or because the
##'     graph must show the confidence intervals. 
##' 
##' @param ... Not used yet.
##' 
##' @export
##'
##' @method autoplot RenouvTList
##' 
##' @seealso \code{\link{autoplot.coef.RenouvTList}} and
##'     \code{\link{autoplot.coSd.RenouvTList}} to (auto)plot the
##'     estimated coefficients possibly with confidence intervals and
##'     \code{\link{autoplot.coef.RenouvTList}} to (auto) plot the
##'     return levels possibly with confidence intervals.
##' 
autoplot.RenouvTList <- function(object,
                                 show = list(quant = TRUE, conf = TRUE, allObs = TRUE),
                                 predOptions = NULL,
                                 posOptions = NULL,
                                 byBlockStyle = NULL,
                                 facets,
                                 ...) {
    

    Threshold <- Period <- Quantile <- Level <- L <- U <- NULL
    
    if (missing(facets)) {
        facets <- ifelse(isTRUE(show$conf), TRUE, FALSE)
    }
    
    if (isTRUE(show$quant) || isTRUE(show$conf)) {
        ## pred <- predict(object)
        L <- c(list(object = object), predOptions)
        pred <- do.call(predict, L)
        g <- autoplot(pred, confInt = isTRUE(show$conf))
    } else {
       stop("For now, at least one of  `show$quant` or `show$conf` must be TRUE")
    }
    
    if (isTRUE(show$allObs)) {
        
        g <- g + ggnewscale::new_scale_colour()
        g <- g + ggnewscale::new_scale_fill()

        g <- g + autolayer(object[[1]], which = "allObs", alpha = 0.6)
        g <- g + autolayer(object[[1]], which = "emptyOTS")
        
        g <- g + scale_shape_manual(values = c(16, 21, 24))
        g <- g + scale_colour_manual(name = "Group",
                                     values = c("black", "orangered", "ForestGreen"))
        g <- g + scale_fill_manual(values = c("black", "gold", "Chartreuse"))
    }
    
    if (facets) {
            g <- g + facet_wrap(. ~ Threshold, labeller = label_both)
    }

    g
    
}
