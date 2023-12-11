## *****************************************************************************

##' Gather the different types of observations of \code{object} into a
##' single data frame that can be used to build a \code{ggplot}.
##'
##' The \code{"OT"} observations will be displayed as vertical
##' segments, while the \code{"MAX"} or \code{"OTS"} may be displayed
##' as horizontal segments when their date is not known. The columns
##' \code{x}, \code{xend}, \code{y} and \code{yend} are defined on
##' purpose.
##' 
##' @title Retrieve All Observations of a \code{Rendata} object as Rows of a single
##' Data Frame
##'
##' @param object An object with (S3) class \code{"Rendata"} from the
##'     \pkg{Renext} package.
##'
##' @param ... Not used yet.
##' 
##' @return A data frame that can be used with the \code{ggplot}
##'     function. This data frame has the four columns \code{x},
##'     \code{xend}, \code{y} and \code{yend} matching in the
##'     aesthetic of a \code{\link{geom_segment}} layer. It also has
##'     the columns \code{type} (with values \code{"OT"}, \code{"MAX"}
##'     and \code{"OTS"}) and \code{block} which identifies the
##'     block. Mind that \code{x} and \code{xend} are objects with
##'     class \code{"Date"} or \code{"POSIXct"}.
##'
##' @import ggplot2
##' @import Renext
##' 
##' @export
##' @keywords internal
##' 
##' @examples
##' rd3 <- rRendata(effDuration = 10,
##'                 OTS.effDuration = rep(60, 2),
##'                 OTS.threshold = rep(80, 2),
##'                 simDate = FALSE,
##'                 distname.y = "gpd", par.y = c(scale = 20, shape = 0.16))
##' df <- allObs.Rendata(rd3)
##' g <- ggplot(data = df)
##' g <- g + geom_segment(mapping = aes(x = x, xend = xend,
##'                                     y = y, yend = yend, colour = type))
##' g
##' 
allObs.Rendata <- function(object, ...) {
    
    ## nsk <- skip2noskip(object$OTmissing,
    ## start = object$OTinfo$start, end = object$OTinfo$end)

    ## First, the "OT" data
    x <- object$OTdata$date
    y <- object$OTdata[[object$info$varName]]

    if (FALSE) {
        yMin <- object$OTinfo$threshold
        if (is.null(yMin)) yMin <- -Inf
    } else {
        yMin <- -Inf
    }
        
    df <- data.frame(block = 1,
                     type = "OT",
                     x = x, xend = x, 
                     y = yMin, yend = y)
    
    ## Now "MAXdata" and "OTSdata" if needed.
    for (typData in c("MAX", "OTS")) {

        nmInfo <- paste0(typData, "info")
        nmData <- paste0(typData, "data")
        
        if (!is.null(MI <- object[[nmInfo]])) {    
            if (is.null(MI$block)) MI$block <- 1:nrow(MI)
            od <- object[[nmData]]
            MI2 <- data.frame(block = od$block,
                              date = od$date,
                              Var = od[[object$info$varName]])
            MI <- merge(MI[ , c("block", "start", "end")], MI2, by = "block")
            x <- MI$start
            xend <- MI$end
            ind <- !is.na(MI$date)
            x[ind] <- MI$date[ind]
            xend[ind] <- MI$date[ind]
            y <- yend <- MI$Var
            y[ind] <- -Inf
            df <- rbind(df, data.frame(block = MI$block,
                                       type = typData,
                                       x = x, xend = xend,
                                       y = y, yend = yend))
        }
    }
    df
    
}

## *****************************************************************************

##' Gather all the periods described in \code{object} into a single
##' data frame that can be used to build a \code{ggplot}.
##'
##' @title Periods in a \code{Rendata} Object
##' 
##' @param object An object with class \code{"Rendata"}.
##' 
##' @param ... Not used yet.
##'
##' @return A data frame that can be used with the \code{ggplot} function.
##'
##' @export
##' @keywords internal
##' 
##' @examples
##' rd3 <- rRendata(effDuration = 10,
##'                 OTS.effDuration = rep(60, 2),
##'                 OTS.threshold = rep(80, 2),
##'                 simDate = FALSE,
##'                 distname.y = "gpd", par.y = c(scale = 20, shape = 0.16))
##' df <- allPeriods.Rendata(Brest)
##' g <- ggplot(data = df)
##' g <- g + geom_rect(mapping = aes(xmin = start, xmax = end, ymin = 0, ymax = 1, colour = period,
##'                                  fill = period))
##' g
allPeriods.Rendata <- function(object, ...) {

    dfPeriods <- data.frame(start = object$OTinfo$start,
                            end = object$OTinfo$end,
                            duration = object$OTinfo$effDuration,
                            block = 1,
                            comment = "",
                            period = "OT")
    
    if (!is.null(OTM <- object$OTmissing)) {
        ## nsk <- skip2noskip(object$OTmissing,
        ##                    start = object$OTinfo$start, end = object$OTinfo$end)
        dur <- round(as.numeric(OTM$end - OTM$start) / 365.25, digits = 3)
        a <- data.frame(start = OTM$start,
                        end = OTM$end,
                        duration = dur,
                        block = 0,
                        comment = "",
                        period = "OT missing")
        dfPeriods <- rbind(dfPeriods, a)
    }
    
    if (!is.null(MI <- object$MAXinfo)) {
        if (is.null(MI$block)) MI$block <- 1:nrow(MI)
        dfPeriods <- rbind(dfPeriods,
                           data.frame(start = MI$start,
                                      end = MI$end,
                                      duration = MI$duration,
                                      block = MI$block,
                                      comment = "",
                                      period = "MAX"))
    }
    if (!is.null(OI <- object$OTSinfo)) {
        if (is.null(OI$block)) OI$block <- 1:nrow(OI)
        dfPeriods <- rbind(dfPeriods,
                           data.frame(start = OI$start,
                                      end = OI$end,
                                      duration = OI$duration,
                                      block = OI$block,
                                      comment = "",
                                      period = "OTS"))
    }

    dfPeriods
}


##' Gather the different types of observations described in the
##' \code{Renouv} object \code{object} and related \emph{plotting
##' positions} into a single data frame that can be used to build a
##' \code{ggplot}.
##'
##' For each observation, a plotting position is provided to show the
##' observation as a point on the \emph{return level plot}. A special
##' case concerns \code{"OTS"} blocks with no exceedance over the
##' threshold hence with no observation. Such a block will be shown by
##' using a horizontal segment ending at a suitable return-period.
##' 
##' @title Retrieve All Observations of a \code{Rendata} object as
##'     Rows of a single Data Frame
##'
##' @param object An object with class \code{"Rendata"}.
##'
##' @param byBlockStyle A named list with logical elements.
##'
##' @param ... Not used yet.
##' 
##' @return A list of two data frames that can be used with the
##'     \code{ggplot} function.
##' 
##' \itemize{
##'    \item{\code{dfST} }{
##'
##'        This data frame is to be used in a \code{geom_point} layer.
##'        It has variables \code{Period}, \code{Quantile} and
##'        \code{Group} providing \code{x}, \code{x} and \code{group} or
##'        \code{colour} in the aesthetic.
##' 
##'    }
##'    \item{\code{dfSeg} }{ 
##' 
##'        Has variables \code{x}, \code{xend}, \code{y} and
##'        \code{yend} related to the aesthetic of a
##'        \code{\link{geom_segment}} layer. The column \code{Group}
##'        identifies the group. \bold{Caution}: can have zero rows.
##'
##'    }
##' }
##' 
##' @import ggplot2
##' @import Renext
##' 
##' @export
##' @keywords internal
##' 
##' @examples
##' example(Renouv, ask = FALSE, echo = FALSE)
##' res <- allObs.Renouv(fit3)
##' 
allObs.Renouv <- function(object,
                          byBlockStyle = NULL,
                          ...) {
    
    ## After this, 'byBlockStyle' is a list with both elements "MAX" and "OTS"
    ## existing
    if (is.null(byBlockStyle)) {
        byBlockStyle <- list()
        if (is.null(object$x.OT)) {
            ## maybe block maxima with OTS data ?
            byBlockStyle$MAX <- FALSE
            byBlockStyle$OTS <- TRUE
        } 
    } else {
        byBlockStyle <- as.list(byBlockStyle)
        if (!(all(names(byBlockStyle) %in% c("MAX", "OTS")))) 
            warning("'byBlockStyle' names differing from \"MAX\" or \"OTS\": ",
                    "ignored")
    }
    ## now fill poissibly missing elements
    for (type in c("MAX", "OTS")) {
        if (is.null(byBlockStyle[[type]])) {
            ht <- object[[paste("history", type, sep = ".")]]
            if (ht$flag) byBlockStyle[[type]] <- (nlevels(ht$block) <= 10L)
            else byBlockStyle[[type]] <- FALSE 
        }
    }
    
    ## =========================================================================
    ## Compute the plotting positions for the "points".
    ## =========================================================================
    
    ST <- SandT(object)
    dfST <- data.frame(Period = ST$T,
                       Quantile = ST$x,
                       Group = ST$groupNames[ST$group])
    
    ## CAUTION there are some inconsistencies in Renext namings!
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

            ## add rows (this is for the right end-point of the segment. 
            dfST <- rbind(dfST,
                          data.frame(Period = hist$effDuration[ind],
                                     Quantile = hist$threshold[ind],
                                     Group = hist$blockNames[ind]),
                          deparse.level = 0)
        } else {
            dfSeg <- data.frame(x = numeric(0),  xend = numeric(0),
                                y = numeric(0), yend = numeric(0),
                                Group = character(0))
        }
    }

    rownames(dfST) <- NULL
    
    ## =========================================================================
    ## Manage the levels of the `Group` column for its use in a
    ## legend. There are two cases depending on whether the blocks are
    ## to be considered as distinct or not within the \code{OTS} and
    ## \code{MAX} data.
    ## =========================================================================

    print(byBlockStyle)
    
    Levs <- character(0)

    if (!is.null(object$x.OT)) {
        Levs <- c(Levs, "OT")
    }
    
    if (!byBlockStyle[["MAX"]]) {
        Levs <- c(Levs, "MAX")
    } else {
        Lev1 <- dfST$Group
        Levs <- c(Levs, sort(unique(Lev1[grep("MAX", Lev1)])))
    }
    
    if (!byBlockStyle[["OTS"]]) {
        Levs <- c(Levs, "MAX")
    } else {
        Lev1 <- dfST$Group
        Levs <- c(Levs, sort(unique(Lev1[grep("OTS", Lev1)])))
    }
    print(Levs)
    
    dfST <- within(dfST, Group <- factor(Group, levels = Levs))
    dfSeg <- within(dfSeg, Group <- factor(Group))
    
    list(dfST = dfST, dfSeg = dfSeg) 
    
}

