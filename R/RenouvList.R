
RenouvList <- function(..., names) {
    L <- list(...)
    
    ck <- sapply(L, inherits, "Renouv") | sapply(L, inherits, "try-error")
    
    if (!all(ck)) {
        stop("all objects given in `...` must inherit ",
             "\"Renouv\" or \"try-error\"")
    }
    
    class(L) <- "RenouvList"
    L
}

RenouvList <- function(x, threshold = NULL,
                       effDuration = NULL,
                       distname.y = "exponential", 
                       MAX.data = NULL, MAX.effDuration = NULL, OTS.data = NULL, 
                       OTS.effDuration = NULL, OTS.threshold = NULL,
                       fixed.par.y = NULL,  start.par.y = NULL, force.start.H = FALSE,
                       numDeriv = TRUE, 
                       trans.y = NULL, jitter.KS = TRUE, pct.conf = c(95, 70), rl.prob = NULL, 
                       prob.max = 1 - 1e-04, pred.period = NULL, suspend.warnings = TRUE, 
                       control = list(maxit = 300, fnscale = -1),
                       control.H = list(maxit = 300, fnscale = -1),
                       trace = 0, plot = TRUE, label = "", ...) 
{

}
