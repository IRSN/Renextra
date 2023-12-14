
GaronneJit <- Renext::Garonne
set.seed(1234)
GaronneJit$OTdata <-
    within(GaronneJit$OTdata,
           Flow <- round(Flow + stats::rnorm(length(Flow), sd = 30), digits = 0))

GaronneJit$MAXdata <-
    within(GaronneJit$MAXdata,
           Flow <- round(Flow + stats::rnorm(length(Flow), sd = 60), digits = 0))
