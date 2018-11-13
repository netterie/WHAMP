########################
# Test edges-only persistent model
########################

rm(list = ls())
suppressMessages(library("EpiModelHIV"))
library("parallel")
library("here")
np <- detectCores()

# 1. Edges only ----
#' in this simple example, we don't include any mortality in the dissolution model,
#' and we start with an empty network without any attributes. We also use default control settings without
#' changing the MCMC settings.

## Initialize the network
nw.empty <- network.initialize(10000,directed=FALSE)

# target stats
stats <- 2017.5
dur <- 221
time.step <- 7

formation <- ~edges

# Fit model
#' Note, with this model, if init.method="zeros", it gives the degeneracy error ("Number of edges in a simulated network exceeds that in the observed by a factor of more than 20")

fit.p1 <- netest(nw.empty, 
                formation = formation, 
                target.stats = stats,
                coef.diss = dissolution_coefs(dissolution = ~offset(edges), duration = dur / time.step),
                edapprox = TRUE,
                set.control.ergm = control.ergm(MPLE.max.dyad.types = 1e9,
                                                MCMLE.maxit = 250))

save(fit.p1, file = here("Model fits and simulations/Fit tests and debugging/est/fit.p.edgesonly.Rda"))


# 2. Edges only + duration = 100 time steps----
#' in this simple example, we don't include any mortality in the dissolution model,
#' and we start with an empty network without any attributes. We also use default control settings without
#' changing the MCMC settings.

## Initialize the network
nw.empty <- network.initialize(10000,directed=FALSE)

# target stats
stats <- 2017.5
dur <- 700
time.step <- 7

formation <- ~edges

# Fit model
#' Note, with this model, if init.method="zeros", it gives the degeneracy error ("Number of edges in a simulated network exceeds that in the observed by a factor of more than 20")

fit.p2 <- netest(nw.empty, 
                 formation = formation, 
                 target.stats = stats,
                 coef.diss = dissolution_coefs(dissolution = ~offset(edges), duration = dur / time.step),
                 edapprox = TRUE,
                 set.control.ergm = control.ergm(MPLE.max.dyad.types = 1e9,
                                                 MCMLE.maxit = 250))

save(fit.p2, file = here("Model fits and simulations/Fit tests and debugging/est/fit.p.edgesonly_longdur.Rda"))

