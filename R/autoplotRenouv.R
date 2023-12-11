## example(Renouv, ask = FALSE)


##
##' 
##' Generate a \code{ggplot} from a \code{Renouv} object. The plot
##' shows the return-level curve using a log-scale for the return
##' period.  It also displays the (possibly censored) observations
##' with suitable plotting positions, and confidence intervals on the
##' return levels.
##'
##' @details Although this can be viewed as as substitute of
##'     \code{plot.Renouv} method of \pkg{Renext}, some differences in
##'     the appearence result from the use of \pkg{ggplot2} instead of
##'     \pkg{graphics}. The content of the legend(s) can be quite
##'     different from that of the single legend obtained with
##'     \code{plot.Renouv}.
##' 
##' @title Generate a \code{ggplot} from a \code{Renouv} Object
##' 
##' @param object An object with class \code{"Renouv"} representing a
##'     fitted Marked Poisson process model.
##'
##' @param level The confidence level.
##'
##' @param show A named list describing the elements to be shown. A
##'     missing information is equivalent to FALSE.
##'
##' @param posOptions A named list of arguments to be passed to the
##'     \code{\link{SandT}} function. Mind that in order to be
##'     consistent with the \code{plot.Renouv} method, the "p-points"
##'     are used rather the "H-points" (or Nelson's points).
##'
##' @param byBlockStyle As in \code{\link{plot.Renouv}}.
##'
##' @param ... Not used yet.
##'
##' @method autoplot Renouv
##' @export
##' 
##' @examples
##' example(Renouv, ask = FALSE, echo = FALSE)
##' autoplot(fit3) 
autoplot.Renouv <- function(object,
                            level = 0.95,
                            show = list(OT = TRUE, quant = TRUE, conf = TRUE,
                                        MAX = TRUE, OTS = TRUE),
                            posOptions = NULL,
                            byBlockStyle = NULL,
                            ...) {

    if (length(level) > 2) {
        stop("the number of confidence levels must be <= 2")
    }
    
    lambdaHat <- coef(object)["lambda"]

    ## =========================================================================
    ## Compute predictions. Unlike the plot method, this is done in
    ## order to control the confidence level(s). Also the results are
    ## turned into a table in long format. The ribbons are plotted in the
    ## decreasing confidence order.
    ## =========================================================================
    
    periods <- as.vector(outer(c(1, 2, 3, 5, 7), c(1, 10, 100, 1000)))

    L <- lapply(level, function(lev) {
        p <- predict(object, level = lev, newdata = periods)
        names(p) <- c("Period", "Quantile", "L", "U")
        cbind(p, Level = lev)
    })
    
    pred <- as.data.frame(data.table::rbindlist(L))
    levs <- rev(unique(format(pred$Level)))
    pred <- within(pred, Level <- factor(format(Level), levels = levs))

    ## =========================================================================
    ## Compute the plotting positions for the "points".
    ## =========================================================================
    
    ST <- SandT(object)
    dfST <- data.frame(Period = ST$T,
                       Quantile = ST$x,
                       Group = ST$groupNames[ST$group])

    ## CAUTION ther are some inconsistencies in Renext namings!
    dfST$Group <- gsub("OTS\\.", "OTS ", dfST$Group)
    dfST$Group <- sub("MAX\\.", "MAX ", dfST$Group)
    dfST$Group <- sub("(\\d+)$", " \\1", dfST$Group)
    
    ## =========================================================================
    ## Special care for the empty OTS blocks, if any.
    ## =========================================================================

    type <- "OTS"
    hist <- object[[paste("history", type, sep = ".")]]
    if (!is.null(hist)) {
        ind <- (hist$r == 0)
        if (any(ind)) {
            dfSeg <- data.frame(x = 0,  xend = hist$effDuration[ind],
                                y = hist$threshold[ind], yend = hist$threshold[ind],
                                Group = hist$blockNames[ind])
            if (TRUE) {
                dfST <- rbind(dfST,
                              data.frame(Period = hist$effDuration[ind],
                                         Quantile = hist$threshold[ind],
                                         Group = hist$blockNames[ind]))
            }
        } else {
            dfSeg <- NULL
        }
    }

    ## XXX TODO : re-arrange the levels so that they comme in a
    ## certain order, with the OT in first place.
    Levs <- unique(dfST$Group)
    op <- c("OT", "MAX", "OTS")
    opLev <- op[op %in% dfST$Group]
    
    dfST <- within(dfST, Group <- factor(Group, levels = Levs))
    
    ## =========================================================================
    ## Now le t us build the layers of the ggplot  
    ## =========================================================================
    
    g <- ggplot(data = pred)
    g <- g + scale_x_log10()
    
    if (isTRUE(show$conf)) {
        g <- g + geom_ribbon(mapping = aes(x = Period, ymin = L, ymax = U,
                                           fill = Level, linetype = Level),
                             colour = "darkgray")
        g <- g + scale_fill_manual(values = translude(c("SteelBlue1", "SteelBlue3"),
                                                      alpha = 0.3))
    }
    if (isTRUE(show$quant)) {
        g <- g + geom_line(mapping = aes(x = Period, y = Quantile))
    }
    if (isTRUE(show$OT)) {
        if (require(ggnewscale)) {
            g <- g + new_scale_fill() 
        }
        g <- g + geom_point(data = dfST,
                            mapping = aes(x = Period, y = Quantile,
                                          shape = Group, col = Group,
                                          fill = Group),
                        stroke = 1.5)
        ## g <- g + scale_fill_manual(values = c("black", "gold", "SpringGreen2", "SteelBlue2"))
        g <- g + scale_shape_manual(values = c(16, 21, 24))
        g <- g + scale_colour_manual(values = c("black", "orangered", "ForestGreen"))
        g <- g + scale_fill_manual(values = c("black", "gold", "Chartreuse"))
    }
    g <- g + geom_hline(yintercept = object$threshold)

    if (!is.null(dfSeg)) {
        g <- g + new_scale_colour() 
        g <- g + geom_segment(data = dfSeg,
                              mapping = aes(x = x, xend = xend,
                                            y = y, yend = yend,
                                            colour = Group,
                                            group = Group),
                              linetype = 2, size = 0.9)
        g <- g + scale_colour_manual(values = c("orangered", "SpringGreen3"))
    }
    
    g
    
    
      
}

if (FALSE) {
    huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
    library(plyr) # to access round_any
    huron$decade <- round_any(huron$year, 10, floor)
    
    ggplot(huron, aes(x =year, group = decade)) + 
        geom_ribbon(aes(ymin = level-1, ymax = level+1, 
                        colour = factor(decade),
                        linetype = factor(decade),
                        fill = factor(decade)), 
                    alpha= 0.1)
}
