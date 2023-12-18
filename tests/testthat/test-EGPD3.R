
## *****************************************************************************
## AUTHOR: Yves Deville <deville.yves@alpestat.com>
##
## GOAL: Tests (for now limited) the implementation of the EGPD3
## distribution.
## *****************************************************************************

library(Renextra)
library(testthat)
## Choose random parameters
set.seed(903)
ns <-  1000
scale <- rgamma(1, shape = 2)
shape <- rnorm(1, sd = 0.08)
kappa <- rgamma(1 , shape = 3, scale = 1)

## ==========================================================================
## Test that the probability density is the derivative of the
## distribution function
## ==========================================================================
x <-  seq(from = 0, to = qEGPD3(0.999, scale = scale, shape = shape,
                                    kappa = kappa), length.out = 500)
h <- diff(x[1:2])
plot(x, f <- dEGPD3(x, scale = scale, shape = shape, kappa = kappa),
     type = "l", col = "orangered", xlab = "", ylab = "",
     main = sprintf("sigma = %4.1f, xi = %4.2f, kappa = %4.2f",
                    scale, shape, kappa))
F <- pEGPD3(x, scale = scale, shape = shape, kappa = kappa)
lines(x, c(NA, diff(F) / diff(x[1:2])))

test_that(desc = "EGPD3 density is the derivative of the distribution fun",
          { expect_lt(max(abs(f[-1] - diff(F) / diff(x[1:2]))), 1e-2) })


## ==========================================================================
## Test that the distribution and the quantile functions are
## reciprocals
## ==========================================================================

p <-  runif(1000)
q <- qEGPD3(p, scale = scale, shape = shape, kappa = kappa)
pp <- pEGPD3(q, scale = scale, shape = shape, kappa = kappa)
test_that(desc = "EGPD3 distribution and quantile functions are reciprocals",
          { expect_lt(max(abs(p - pp)), 1e-10) })

