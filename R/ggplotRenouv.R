## example(Renouv, ask = FALSE)


##
##' 
##' Generate a \code{ggplot} from a \code{Renouv} object. The plot
##' shows the return-level curve using a log-scale for the return
##' period.  It also displays the (possibly censored) observations
##' with suitable plotting positions, and confidence intervals on the
##' return levels.
##'
##' @details Although this method can be viewed as as substitute of
##'     \code{\link[Renext]{plot.Renouv}} method of \pkg{Renext}, some
##'     differences in the appearance result from the use of
##'     \pkg{ggplot2} instead of \pkg{graphics}. The content of the
##'     legend(s) can be quite different from that of the single
##'     legend obtained with \code{plot.Renouv}. Note that one can
##'     choose to show or not the observations, but the choice is for
##'     \emph{all the observations}: \code{OT} and the historical
##'     observations \code{OTS} and \code{MAX} if they exist.
##' 
##' @title Generate a \code{ggplot} from a \code{Renouv} Object
##' 
##' @param object An object with class \code{"Renouv"} representing a
##'     fitted Marked Poisson process model.
##'
##' @param level The confidence level. A numeric vector \emph{with
##'     length} \code{1} or \code{2} containing the confidence levels
##'     e.g., \code{0.95} or \code{c(0.95, 0.70)}.
##'
##' @param show A named list describing the elements to be shown. A
##'     missing information is equivalent to \code{FALSE}. The value
##'     of the element \code{"quant"} tells if the quantile (or return
##'     level) line is to be shown or not. The element \code{"conf"}
##'     is for the confidence interval(s) and the element
##'     \code{"allObs"} is for the observations.
##'
##' @param pct.conf Logical. If \code{TRUE} the legend will show each
##'     confidence levels as a percent. If \code{FALSE}, one use
##'     instead a (decimal) level. The percents are rounded to the
##'     next integer and the levels are rounded to two decimal place
##'     in the displayed legend, but not in the computation.
##' 
##' @param posOptions A named list of arguments to be passed to the
##'     \code{\link{allObs.Renouv}} and then to the
##'     \code{\link[Renext]{SandT}} function. Mind that in order to be
##'     consistent with the \code{\link[Renext]{plot.Renouv}} method
##'     of \pkg{Renext}, the "p-points" are used by default rather the
##'     "H-points" (a.k.a. Nelson's points). Also the default choice
##'     is corresponds to the so-called \emph{Weibull positions}
##'     corresponding to \code{a = 0} in \code{\link[stats]{ppoints}}
##'     and these positions are biassed.
##'
##' @param byBlockStyle As in \code{\link{plot.Renouv}}.
##'
##' @param xlim Optional vector of limits for the horizontal
##'     axise. This argument is actually passed as the `limits`
##'     argument of \code{\link[ggplot2]{scale_x_log10}} function.
##'     Note that the limits of the vertical axis can be set in a
##'     standard way by using \code{+ ylim()}, see
##'     \code{\link[ggplot2]{ylim}}.
##' 
##' @param quant.col,conf.col,conf.fill,conf.lty,obs.col,obs.fill,obs.shape
##'     Can be used to change or set the graphical parameters such as
##'     colours. This remains experimental and some change may occur.
##' 
##' @param ... Not used yet.
##'
##' @method autoplot Renouv
##' @export
##'
##' @importFrom stats coef predict
##' @importFrom ggnewscale new_scale_colour new_scale_colour
##' 
##' @examples
##' example(Renouv, ask = FALSE, echo = FALSE)
##' autoplot(fit3)
##' autoplot(fit3, byBlockStyle = list(OTS = FALSE))
##' autoplot(fit3, level = c(0.7, 0.90))
##'
##' ## Using fictious historical information
##' fit <- Renouv(x = GaronneJit$OTdata$Flow,
##'               effDuration = GaronneJit$OTinfo$effDuration,
##'               threshold = 3500, dist = "GPD",
##'               MAX.data = list(Hist1 = c(7500, 6947),
##'                               Hist2 = c(6200, 6470, 7010),
##'                               Hist3 = c(7800)),
##'               MAX.effDuration = c(100, 150, 190), plot = FALSE)
##' autoplot(fit)
##' autoplot(fit, pct = FALSE)
##' autoplot(fit, level = 0.95)
##' autoplot(fit, quant.col = "chartreuse")
##' ## control the shown elements
##' autoplot(fit, level = c(0.95, 0.70), quant.col = "cyan",
##'          show = list(quant = FALSE, conf = TRUE))
##' autoplot(fit, level = 0.95, quant.col = "orchid",
##'          show = list(quant = TRUE, conf = FALSE))
##' autoplot(fit, level = 0.95, quant.col = "orchid",
##'          show = list(quant = TRUE, conf = TRUE, allObs = FALSE))
##' 
autoplot.Renouv <- function(object,
                            level = c(0.70, 0.95),
                            show = list(quant = TRUE, conf = TRUE, allObs = TRUE),
                            pct.conf = TRUE,
                            posOptions = NULL,
                            byBlockStyle = NULL,
                            quant.col = "OrangeRed",
                            conf.col = "SteelBlue3",
                            conf.fill = translude(c("SteelBlue1", "SteelBlue3"), alpha = 0.3),
                            conf.lty = c("dashed", "dotted"),
                            obs.col = c("black", "orangered", "ForestGreen", "orchid"),
                            obs.fill = c("black", "gold", "Chartreuse", "pink"),
                            obs.shape = c(16, 21, 24, 23),
                            xlim = NULL,
                            ...) {
    
    Period <- Quantile <- U <- L <- Group <- Level <- NULL
    x <- xend <- y <- yend <- NULL
    
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
   
    logGrid <- seq(from = -log(coef(object)["lambda"], base = 10) + 1e-6,
                   to = log(1100, base = 10),
                   length.out = 100)
    
    periods <- 10^logGrid
    
    L <- lapply(level, function(lev) {
        p <- predict(object, level = lev, newdata = periods)
        names(p) <- c("Period", "Quantile", "L", "U")
        cbind(p, Level = lev)
    })
    
    if (isTRUE(show$quant)) {
        LQ <- within(L[[1]], {
            L <- Quantile;
            U <-  Quantile;
            Level = 0.0
        })
        L[[length(L) + 1]] <- LQ
        lty <- c("solid", conf.lty)
        col <- c(quant.col, conf.col, conf.col)
    } else {
        lty <- conf.lty
        col <- c(conf.col, conf.col)
    }
    
    pred <- as.data.frame(data.table::rbindlist(L))
    if (pct.conf) {
        pred$LevelText <- sprintf("%2d%% conf.", 100 * pred$Level)
    } else {
        pred$LevelText <- sprintf("%4.2f conf.", pred$Level)
    }

    levs <- rev(unique(pred$LevelText))
    pred <- within(pred, LevelText <- ordered(format(LevelText), levels = levs))
    
    if (isTRUE(show$quant)) {
        levels(pred$LevelText)[1] <- "quantile"
    }
    
    ## =========================================================================
    ## ## Compute the plotting positions for the "points".
    ## ## =========================================================================
    
    L <- allObs.Renouv(object, byBlockStyle = byBlockStyle,
                       posOptions = posOptions)
    
    ## =========================================================================
    ## Now let us build the layers of the ggplot  
    ## =========================================================================
    
    g <- ggplot(data = pred)
    g <- g + scale_x_log10(limits = xlim)
    
    if (isTRUE(show$conf)) {
        g <- g + geom_ribbon(data = subset(pred, Level != 0), 
                             mapping = aes(x = Period, ymin = L, ymax = U,
                                           fill = LevelText, linetype = LevelText),
                             colour = "darkgray", show.legend = FALSE)
        g <- g +   scale_fill_manual(values = conf.fill)
        ## 2 new layers
        g <- g + geom_line(data = subset(pred, Level != 0),
                           mapping = aes(x = Period, y = L,
                                         linetype = LevelText, colour = LevelText))
        
        g <- g + geom_line(data = subset(pred, Level != 0),
                           mapping = aes(x = Period, y = U,
                                         linetype = LevelText, colour = LevelText))

        ## g <- g + scale_fill_manual(values = conf.fill) +
        ##    scale_linetype_manual(values = conf.lty)

    }
    
    if (isTRUE(show$quant)) {
        g <- g + geom_line(data = subset(pred, Level == 0),
                           mapping = aes(x = Period, y = L,
                                         linetype = LevelText, colour = LevelText))
    }
    g <- g + scale_linetype_manual(values = lty, breaks = levels(pred$LevelText)) +
        scale_colour_manual(values = col, breaks = levels(pred$LevelText))

    ## For ONE legend for the two cues 'linetype' and 'colour'
    g <- g + labs(linetype = "", colour = "")


    if (isTRUE(show$allObs)) {
       
        g <- g + ggnewscale::new_scale_fill() 
        g <- g + ggnewscale::new_scale_colour()
        
        g <- g + geom_point(data = L$dfST,
                            mapping = aes(x = Period, y = Quantile,
                                          shape = Group, col = Group,
                                          fill = Group),
                            stroke = 1.5)
        g <- g + scale_shape_manual(values = obs.shape)
        g <- g + scale_colour_manual(name = "Group", values = obs.col)
        g <- g + scale_fill_manual(values = obs.fill)
    }
    
    g <- g + geom_hline(yintercept = object$threshold, col = "darkgrey")

    if (isTRUE(show$allObs) && nrow(L$dfSeg)) {
       
        g <- g + ggnewscale::new_scale_colour() 
        g <- g + geom_segment(data = L$dfSeg,
                              mapping = aes(x = x, xend = xend,
                                            y = y, yend = yend,
                                            colour = Group,
                                            group = Group),
                              linetype = 2, size = 0.9)
        g <- g + scale_colour_manual(name = "Group",
                                     values = c("orangered", "SpringGreen3"))
    }
    
    g <- g + labs(x = "Period", y = "Quantile")
    
    g 
      
    
    
}
##' OLD CODE!!!
## autoplot.Renouv <- function(object,
##                             level = 0.95,
##                             show = list(quant = TRUE, conf = TRUE, allObs = TRUE),
##                             posOptions = NULL,
##                             byBlockStyle = NULL,
##                             ...) {

##     col.quant <- "ForestGreen"
##     col.conf <- "SteelBlue3"
##     fill.conf <- translude(c("SteelBlue1", "SteelBlue3"), alpha = 0.3)
##     lty.conf <- c("dashed", "dotted")
    
##     Period <- Quantile <- U <- L <- Group <- Level <- NULL
##     x <- xend <- y <- yend <- NULL
    
##     if (length(level) > 2) {
##         stop("the number of confidence levels must be <= 2")
##     }
    
##     lambdaHat <- coef(object)["lambda"]

##     ## =========================================================================
##     ## Compute predictions. Unlike the plot method, this is done in
##     ## order to control the confidence level(s). Also the results are
##     ## turned into a table in long format. The ribbons are plotted in the
##     ## decreasing confidence order.
##     ## =========================================================================
    
##     ## periods <- as.vector(outer(c(0.1, 1, 2, 3, 5, 7, 10.10), c(1, 10, 100)))
##     ## periods <- sort(c(periods, 1000))

##     logGrid <- seq(from = -log(coef(object)["lambda"], base = 10) + 1e-6,
##                    to = log(1100, base = 10),
##                    length.out = 100)
  
##     periods <- 10^logGrid

##     L <- lapply(level, function(lev) {
##         p <- predict(object, level = lev, newdata = periods)
##         names(p) <- c("Period", "Quantile", "L", "U")
##         cbind(p, Level = lev)
##     })
    
##     pred <- as.data.frame(data.table::rbindlist(L))
##     levs <- rev(unique(format(pred$Level)))
##     pred <- within(pred, Level <- factor(format(Level), levels = levs))

##     ## ## =========================================================================
##     ## ## Compute the plotting positions for the "points".
##     ## ## =========================================================================
    
##     L <- allObs.Renouv(object, byBlockStyle = byBlockStyle,
##                        posOptions = posOptions)
    
##     ## =========================================================================
##     ## Now let us build the layers of the ggplot  
##     ## =========================================================================
    
##     g <- ggplot(data = pred)
##     g <- g + scale_x_log10()
    
##     if (isTRUE(show$conf)) {
##         g <- g + geom_ribbon(mapping = aes(x = Period, ymin = L, ymax = U,
##                                            fill = Level, linetype = Level),
##                              colour = "darkgray", show.legend = FALSE)
##         ## 2 new layers
##         g <- g + geom_line(mapping = aes(x = Period, y = L, linetype = Level),
##                            colour = col.conf)
##         g <- g + geom_line(mapping = aes(x = Period, y = U, linetype = Level),
##                              colour = col.conf)
##         g <- g + scale_fill_manual(values = fill.conf) +
##             scale_linetype_manual(values = lty.conf)
##     }

##     if (isTRUE(show$quant)) {
##         g <- g + geom_line(mapping = aes(x = Period, y = Quantile),
##                            colour = col.quant)
##     }
    
##     if (isTRUE(show$allObs)) {
       
##         g <- g + ggnewscale::new_scale_fill() 
        
##         g <- g + geom_point(data = L$dfST,
##                             mapping = aes(x = Period, y = Quantile,
##                                           shape = Group, col = Group,
##                                           fill = Group),
##                             stroke = 1.5)
##         ## g <- g + scale_fill_manual(values = c("black", "gold", "SpringGreen2", "SteelBlue2"))
##         g <- g + scale_shape_manual(values = c(16, 21, 24))
##         g <- g + scale_colour_manual(name = "Group",
##                                      values = c("black", "orangered", "ForestGreen"))
##         g <- g + scale_fill_manual(values = c("black", "gold", "Chartreuse"))
##     }
    
##     g <- g + geom_hline(yintercept = object$threshold)

##     if (isTRUE(show$allObs) && nrow(L$dfSeg)) {
       
##         g <- g + ggnewscale::new_scale_colour() 
##         g <- g + geom_segment(data = L$dfSeg,
##                               mapping = aes(x = x, xend = xend,
##                                             y = y, yend = yend,
##                                             colour = Group,
##                                             group = Group),
##                               linetype = 2, size = 0.9)
##         g <- g + scale_colour_manual(name = "Group",
##                                      values = c("orangered", "SpringGreen3"))
##     }

##     g <- g + labs(x = "Period", y = "Quantile")
    
##     g 
      
## }

##' @description Create a \code{ggplot} layer appropriate to represent
##'     part of the information stored in a \code{Renouv} object.
##' 
##' @details
##'    \describe{
##'         \item{\code{"quant"} }{Show the quantile (a.k.a return level) line.}
##'         \item{\code{"conf"} }{Show the confidence interval(s).}
##'         \item{\code{"allObs"} }{Show all the observations: OT and historical \code{MAX} or
##'         \code{OTS}.}
##'         \item{\code{"emptyOS"} }{Show the empty OTS blocks as horizontal segments.}
##'    }
##' 
##' @title Create a \code{ggplot} Layer Appropriate to a Particular Data Type
##' 
##' @param object A \code{Renouv} object.
##'
##' @param level Confidence level. Used only when \code{which} is
##'     \code{"conf"}.
##'
##' @param which The layer to show, see \bold{Details}.  
##'
##' @param posOptions,byBlockStyle See \code{\link{autoplot.Renouv}}.
##'
##' @param ... Further argument to be passed to the \code{geom_} function.
##'    
##'
##' @export
##' @method autolayer Renouv
##'
##' @examples
##' example(Renouv, ask = FALSE, echo = FALSE)
##' ## show the observations only
##' g <- autoplot(fit3, show = list(OT = TRUE, OTS = FALSE))
##' ## add the quantile/Return Level line
##' g +  autolayer(fit3, which = "quant")
##' g <- autoplot(fit3, show = list(conf = TRUE), lev = c(0.70, 0.95))
##' g +  autolayer(fit3, which = "quant")
##' g <- autoplot(fit3, show = list(quant = TRUE))
##' ## use a new scale to avoid errors
##' g +  ggnewscale::new_scale_colour( ) + autolayer(fit3, which = "allObs")
##' g + autolayer(fit3, which = "emptyOTS")
##' 
autolayer.Renouv <- function(object,
                             level = 0.95,
                             which = c("quant", "conf", "allObs", "emptyOTS"),
                             posOptions = NULL,
                             byBlockStyle = TRUE,
                             ...) {

    Period <- Quantile <- U <- L <- Group <- Level <- NULL
    x <- xend <- y <- yend <- NULL
 
    which <- match.arg(which)
    ## periods <- as.vector(outer(c(1, 2, 3, 5, 7, 10.10), c(1, 10, 100)))
    logGrid <- seq(from = -log(coef(object)["lambda"], base = 10) + 1e-6,
                   to = log(1100, base = 10),
                   length.out = 100)
    periods <- 10^logGrid
    
    L <- lapply(level, function(lev) {
        p <- predict(object, level = lev, newdata = periods)
        names(p) <- c("Period", "Quantile", "L", "U")
        cbind(p, Level = lev)
    })
    
    pred <- as.data.frame(data.table::rbindlist(L))
    levs <- rev(unique(format(pred$Level)))
    pred <- within(pred, Level <- factor(format(Level), levels = levs))

    if (which %in% c("allObs", "emptyOTS")) {
         L <- allObs.Renouv(object, byBlockStyle = byBlockStyle)
    }
 
    if (which == "quant") {
        geom_line(data = pred,
                  mapping = aes(x = Period, y = Quantile),
                  ...)
    } else if (which == "conf") {
        geom_ribbon(mapping = aes(x = Period, ymin = L, ymax = U,
                                  fill = Level, linetype = Level),
                    colour = "darkgray",
                    ...)
    } else if (which == "allObs") {
        
        ggnewscale::new_scale_fill( )
        ggnewscale::new_scale_colour( )
        geom_point(data = L$dfST,
                   mapping = aes(x = Period, y = Quantile,
                                 shape = Group,
                                 col = Group,
                                 fill = Group),
                   stroke = 1.5, ...) 
    } else if (which == "emptyOTS") {
        if (nrow(L$dfSeg)) {
            geom_segment(data = L$dfSeg,
                         mapping = aes(x = x, xend = xend,
                                       y = y, yend = yend,
                                       colour = Group,
                                       group = Group),
                         linetype = 2, size = 0.9, show.legend = FALSE, ...)
        }
    }

}



