
##' Generate a \code{ggplot} for a \code{predict.RenouvTList} object.
##'
##' The plot shows the return level against the (log) return period.
##' This plot is useful to asses the impact of the threshold on the
##' return level curve and the corresponding confidence
##' intervals. However, the number of threshold should be < 10.
##' 
##' @title Generate a \code{ggplot} for a \code{predict.RenouvTList}
##'     Object
##' 
##' @param object An object with class \code{"predict.RenouvTList"}.
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
##' 
autoplot.predict.RenouvTList <- function(object,
                                         confInt = FALSE,
                                         facets,
                                         ...) {

    if (missing(facets)) {
        if (confInt) {
            facets <- TRUE
        } else {
            facets <- FALSE
        }
    }
    
    object <- within(object, Threshold <- factor(Threshold))
    
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
                stop("When 'facets' is FALSE only one condifence level is allowed")
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
    g <- g + scale_colour_brewer(palette = "Spectral")
    if (facets) {
        g <- g + facet_wrap(. ~ Threshold, labeller = label_both)
    }
    g
}

##' @title Build a \code{ggplot} for the Coeficients of a
##'     \code{RenouvTList} Object
##'
##' @param object A \code{RenouvTList} object
##'
##' @param facets Logical. It \code{TRUE} each parameter is shown in
##'     one facet oa trellis graphics.
##'
##' @param ... Not used yet.
##' 
##' @return A \code{ggplot} object.
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
##' autoplot(coef(fit))
##' autoplot(coef(fitJit))
##' 
autoplot.coef.RenouvTList <- function(object,
                                      facets,
                                      ...) {
    df <- as.data.frame(unclass(object))
    df <- data.frame(Threshold =  attr(object, "threshold"), df)
    df <- tidyr::gather(df, key = "Param", value = "Value", -Threshold)
    g <- ggplot(data = df)
    g <- g + geom_point(mapping = aes(x = Threshold, y = Value, group = Param))
    g <- g + geom_line(mapping = aes(x = Threshold, y = Value, group = Param))
    g <- g + facet_grid(Param ~ ., scales="free_y", labeller = label_both)
    g
                        
    
    
}
