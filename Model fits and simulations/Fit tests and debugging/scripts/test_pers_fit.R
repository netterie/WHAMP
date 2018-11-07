########################
# Test persistent model
########################

rm(list = ls())
suppressMessages(library("EpiModelHIV"))
library("parallel")
library("here")
np <- detectCores()

# 1. Edges only ----
#' in this simple example, we don't include any mortality in the dissolution model,
#' and we start with an empty network without any attributes

## Initialize the network
nw.empty <- network.initialize(10000,directed=FALSE)

# target stats
stats1 <- 2017.5
dur <- 221
time.step <- 7

formation1 <- ~edges
stats <- stats1

# Fit model
#' Note, with this model, if init.method="zeros", it gives the degeneracy error ("Number of edges in a simulated network exceeds that in the observed by a factor of more than 20")

fit.p1 <- netest(nw.empty, 
                formation = formation1, 
                target.stats = stats1,
                coef.diss = dissolution_coefs(dissolution = ~offset(edges), duration = dur / time.step),
                edapprox = TRUE,
                set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 MCMLE.maxit = 400,
                                                 parallel = np/2, 
                                                 parallel.type="PSOCK"))

save(fit.p1, file = here("Model fits and simulations/Fit tests and debugging/est/fit.p.edgesonly.Rda"))

