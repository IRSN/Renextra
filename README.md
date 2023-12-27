R package Renextra
================

## Goals

The **Renextra** R package has been financed by IRSN Behrig. It aims at
enhancing the **Renext** package which is available [on
CRAN](https://cran.r-project.org/web/packages/Renext/index.html) and on
the [IRSN GitHub](https://github.com/IRSN/Renext) public repos.

The present features of **Renextra** are

- Enhanced graphics relying on the **ggplot2** package. The `autoplot`
  method and (to a lesser extend) `autolayer` method can be used to get
  standard plots as used in Peaks Over Threshold analyses, such as the
  return-level plot.

- S3 class `RenouvTList` for threshold choice and so-called *threshold
  stability plot*.

- Experimental implementation of the Extended Generalized Pareto
  Distribution EGPD3 of Papatatopoulos and Tawn (2013).

Note that the use of the EGPD3 relies on the the general features of the
**Renext** pacakge, in which the distribution of the excesses over the
threshold can be quite arbitrary. So, some initial values of the
parameters must be given. The estimated values of the `scale` and
`shape` parameters obtained by using the `"GPD"` distribution can be
used alogn with `kappa = 1.0`, see the examples.

## Installing

## Examples

### Garonne (Jitterized)

``` r
library(Renextra)
autoplot(GaronneJit)
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
fitGJ <- Renouv(GaronneJit, distname.y = "GPD", plot = FALSE)
autoplot(fitGJ)
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Nidd River

``` r
 library(mev)
 fit <- RenouvTList(nidd,
                    effDuration = 35,
                    threshold = seq(from =70, to = 140, by = 10),
                    distname.y = "GPD")
 summary(fit)
```

    ## RenouvTList object
    ## o Estimated coefficients
    ##         lambda         scale ind        shape         
    ## u =  70  3.943 [0.336] -0.989 [10.104]   0.323 [0.114]
    ## u =  80  2.457 [0.265] -2.213 [16.772]   0.343 [0.164]
    ## u =  90  1.629 [0.216] 12.104 [24.891]   0.238 [0.204]
    ## u = 100  1.114 [0.178] 50.288 [33.156]   0.003 [0.214]
    ## u = 110  0.886 [0.159] 64.108 [41.456]  -0.070 [0.240]
    ## u = 120  0.686 [0.140] 101.478 [49.078] -0.249 [0.238]
    ## u = 130  0.629 [0.134] 77.930 [57.890]  -0.142 [0.294]
    ## u = 140  0.514 [0.121] 98.568 [69.280]  -0.236 [0.322]
    ## o  Kolmogorov-Smirnov test
    ##           n      D p.value
    ## u =  70 138 0.0750  0.4187
    ## u =  80  86 0.0536  0.9545
    ## u =  90  57 0.0749  0.8828
    ## u = 100  39 0.1008  0.7862
    ## u = 110  31 0.0988  0.8939
    ## u = 120  24 0.1081  0.9135
    ## u = 130  22 0.1132  0.9107
    ## u = 140  18 0.1442  0.7981

``` r
 autoplot(fit, show = list(quant = TRUE, allObs= TRUE))
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
 ## Threshold Stability with ECGPD3
 fitE <- RenouvTList(nidd,
                     effDuration = 35,
                     threshold = seq(from = 65.08, to = 88.61, len = 40),
                     start.par.y = c(scale = 30, shape = 0.0, kappa = 1.0),
                     distname.y = "EGPD3")
 autoplot(coSd(fitE, lambda = FALSE))
```

![](README_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

## References

Davison A.C. and Smith R.L. (1990) “Models for Exceedances over High
Thresholds” *J.R. Statist. Soc. B* (**52**) pp. 393-442.

Papastathopoulos I. and Tawn J.A. (2013) “Extended Generalized Pareto
Models for Tail Estimation”, *Journal of Statistical Planning and
Inference* (**143**), pp. 131-143.
