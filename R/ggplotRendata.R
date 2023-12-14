

## ****************************************************************************
##' Generate a \code{ggplot} from a \code{Rendata} Object.
##'
##' The "historial" periods ("OTS" and "MAX") and their observations
##' may be shown or not. Mind that an \code{"OTS"} period may have no
##' observations so it is a good idea to used \code{fillPeriods =
##' TRUE} when \code{showHist} is \code{TRUE}.
##'
##' @title Generate a \code{ggplot} from a \code{Rendata} Object.
##' 
##' @param object An object with class \code{"Rendata"} as defined by.
##'     \pkg{Renext} package.
##' 
##' @param textover The \code{"OT"} observations exceeding this value
##'     will be marked with their date.
##' 
##' @param fillPeriods Logical. If \code{TRUE} the periods are shown
##'     using semi-transparent filled rectangles.
##'
##' @param showHist Logical. If \code{TRUE} the "historial" periods ("OTS" and
##' "MAX") and their observations will be shown.
##' 
##' @param ... Not used yet.
##'
##' @export
##' @method autoplot Rendata
##'
##' @return An object inheriting from \code{"ggplot"}.
##'
##' @examples
##' autoplot(Garonne)
##' set.seed(1234)
##' rd <- rRendata(effDuration = 60,
##'                MAX.effDuration = rep(3, 6), MAX.r = rep(4, 6),
##'                distname.y = "exp", par.y = c(rate = 1/100))
##' autoplot(rd)
##' rd3 <- rRendata(effDuration = 10,
##'                 OTS.effDuration = rep(60, 2),
##'                 OTS.threshold = rep(80, 2),
##'                 simDate = FALSE,
##'                 distname.y = "gpd", par.y = c(scale = 20, shape = 0.16))
##' autoplot(rd3)
##' 
autoplot.Rendata <- function(object,
                             textover = quantile(object$OTdata[ , object$info$varName],
                                                 probs = 0.99),
                             fillPeriods = TRUE,
                             showHist = TRUE,
                             ...)  {
    
    yLab <- object$info$varName
    if (!is.null(ou <- object$info$varUnit)) {
        yLab <- sprintf("%s (%s)", yLab, ou)
    } 

    dfObs <- allObs.Rendata(object)
    dfPeriods<- allPeriods.Rendata(object)

    ## Choose the levels so that the legends show the types of
    ## observations and of periods in a suitable order. 

    op <- c("OT", "OT missing", "MAX", "OTS")
    opLev <- op[op %in% dfObs$type]
    dfObs <- within(dfObs, type <- factor(type, levels = opLev))
    opLev <- op[op %in% dfPeriods$period]
    dfPeriods <- within(dfPeriods, period <- factor(period, levels = opLev))

    ## If the historical periods are NOT to be shown, then turn off the
    ## filling of the missing periods 
    if (!showHist) {
        if (missing(fillPeriods)) fillPeriods <- FALSE
        dfObs <- subset(dfObs, type == "OT")
        dfPeriods <- subset(dfPeriods, period %in% c("OT", "OT missing"))
    }

    counts <- tapply(dfPeriods$block, dfPeriods$period, length)
    
    ## dfPeriods <- within(dfPeriods, period <- factor(paste(period, block)))
    g <- ggplot(data = dfObs)
    
    if (fillPeriods) {
        ## using ymin = -Inf and ymax = Inf works but not with ggploty
        ## yRange <- layer_scales(g)$y$range$range
        g <- g + geom_rect(data = dfPeriods,
                           mapping = aes(xmin = start, xmax = end,
                                         ymin = -Inf, ymax = Inf,
                                         fill = period))
    }

    dfText <- subset(dfObs, type == "OT" & yend > textover)
    dfText <- within(dfText, text <- format(x))
    if (nrow(dfText)) {
        g <- g + geom_text(data = dfText,
                            mapping = aes(x = x, y = yend, label = text),
                                          vjust = 0, nudge_y = 0.8, size = 3)
    }
    
    ## draw the observations as segments
    g <- g + geom_segment(data = dfObs,
                          mapping = aes(x = x, xend = xend,
                                        y = y, yend = yend, colour = type))
    g <- g + ggtitle(object$info$shortLab)
    g <- g + xlab("") + ylab(yLab)
    g <- g + scale_fill_manual(values = c("OT" = translude("orange", alpha = 0.3),
                                          "OT missing"= "white",
                                          "MAX" = translude("SpringGreen3", alpha = 0.3),
                                          "OTS" = translude("SteelBlue1", alpha = 0.3)))
    g <- g + scale_colour_manual(values = c("OT" = "orangered",
                                            "MAX" = "ForestGreen",
                                            "OTS" = "SteelBlue3"))
    if (!is.null(u <- object$OTinfo$threshold)) {
        g <- g + geom_hline(yintercept = u, col = "gray", alpha = 0.6) 
    }

    
    g

}

 
