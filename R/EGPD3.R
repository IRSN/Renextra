## Extended Generalised Pareto "EGPD3" of Papasthopoulos et Tawn.
## Quick implementation to be cleaned before using it in Renext???

## =========================================================================
##' The Extended Generalised Pareto Distribution type 3 was introduced
##' by Papastathopoulos and Tawn in 2013, along with two other
##' distributions built in a similar way. These distributions are
##' intended to be used in the Peak Over Threshold framework with a
##' reduced impact of the threshold choice.
##'
##' The distribution depends on the three parameters: \emph{scale}
##' \eqn{\sigma >0}, \emph{shape} \eqn{\xi} and a "non-tail" parameter
##' \eqn{\kappa >0}. Its distribution function
##' \eqn{G_{\text{EGPD3}}(x;\,\sigma,\,\xi,\,\kappa)}{G(x; \sigma, \xi, \kappa)}
##' relates to the GPD
##' \eqn{F_{\text{GPD}}(x;\,\sigma,\,\xi)}{F(x; \sigma, \xi, \kappa)}
##' according to
##' \deqn{F_{\text{EGPD3}}(x; \sigma,\,\xi,\,\kappa) =
##' F_{\text{GPD}}(x; \,\sigma,\,\xi)^\kappa}{G(x, \sigma, \xi,
##' \kappa) = F(x, \sigma, \xi)^\kappa} The distribution is
##' tail-equivalent to the two-parameter GPD with the same scale
##' \eqn{\sigma} and shape \eqn{\xi}.
##'
##' @title Extended Generalised Pareto Distribution, Type 3
##'
##' @param x Vector of quantiles.
##'
##' @param p Vector of probabilities.
##'
##' @param q Vector of quantiles.
##'
##' @param n Number of observations.
##'
##' @param scale,shape GPD-like scale and shape parameters. 
##'
##' @param kappa Shape coefficient \eqn{\kappa}.
##'
##' @param log Logical. If \code{TRUE} the log-density is returned.
##'
##' @param lower.tail Logical. If \code{TRUE} (default) the
##' probabilities are \eqn{\textrm{Pr}[X \leq x]}{Pr[X <= x]} and
##' \eqn{\textrm{Pr}[X > x]}{Pr[X > x]} otherwise.
##' 
##' @return \code{dEGPD3} is the density function, \code{pEGPD3} is
##' the distribution function, \code{qEGPD3} is the quantile function
##' and \code{rEGPD3} generates random variables.
##'
##' @export
##' @rdname EGPD3
##' 
##' @references
##'
##' Iaonnis Papastathopoulos and Jonathan A. Tawn (2013).
##' "Extended generalised Pareto models for tail estimation".
##' \emph{J. Stat. Plan. Inference}, \bold{143}(3): 131-143.
##' 
##' @examples
##' xi <- 0.2
##' kappa <- 1.1
##' x <- qEGPD3(c(0.001, 0.999), shape = xi, kappa = kappa)
##' x <- seq(from = x[1], to = x[2], length.out = 300)
##' plot(x, dEGPD3(x, shape = xi, kappa = kappa), type = "l",
##'      xlab = "", ylab = "dens", main = "EGPD3 density")
##'
##' fit <- RenouvTList(x = GaronneJit, distname.y = "GPD",
##'                    threshold = seq(from = 2401, to = 3001, by = 100),
##'                    start.par.y = c(scale = 2000, shape = -0.2, kappa = 1.0))
##' autoplot(predict(fit))
##' ## Now use the 'EGPD3' distribution for the exceedances.
##' \dontrun{
##' fitE <- RenouvTList(x = GaronneJit, distname.y = "EGPD3",
##'                    threshold = seq(from = 2401, to = 3001, by = 100),
##'                    start.par.y = c(scale = 2000, shape = -0.2, kappa = 1.0))
##' coef(fitE, reParam = FALSE)
##' autoplot(predict(fitE))
##' }
dEGPD3 <- function(x, scale = 1.0, shape = 0.0, kappa = 1.0, log = FALSE) {
    ind <- (x > 0.0)
    f <- rep(0.0, length(x))
    if (any(ind)) {
        Find <- Renext::pGPD(x[ind], loc = 0.0, scale, shape = shape,
                             lower.tail = TRUE)
        find <- Renext::dGPD(x[ind], loc = 0.0, scale = scale, shape = shape)
        f[ind] <- kappa * find * Find^(kappa - 1.0)
    }
    if (log) f <- log(f)
    f
}

##' @export
##' @rdname EGPD3
pEGPD3 <-  function(q, scale = 1.0, shape = 0.0, kappa = 1.0,
                    lower.tail = TRUE) {
    F <- Renext::pGPD(q = q, loc = 0.0, scale = scale, shape = shape,
                      lower.tail = TRUE)^kappa
    if (lower.tail) return(F)
    else return (1.0 - F)
}

##' @export
##' @rdname EGPD3
qEGPD3 <-  function(p, scale = 1.0, shape = 0.0, kappa = 1.0) {
    Renext::qGPD(p^(1.0 / kappa), loc = 0.0, scale = scale, shape = shape)
}

##' @export
##' @rdname EGPD3
##' @importFrom stats runif
##' 
rEGPD3 <- function(n = 1L, scale = 1.0, shape = 0.0, kappa = 1.0) {
    U <- runif(n)
    qEGPD3(U, scale = scale, shape = shape, kappa = kappa)
}
