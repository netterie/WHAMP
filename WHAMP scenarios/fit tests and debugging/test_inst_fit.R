rm(list=ls())
suppressMessages(library("EpiModelHIV"))
library("parallel")
np <- detectCores()

# Initialize network
n <- 10000
nw <- network::network.initialize(n, directed = FALSE)

# Fit edges-only model
stats <- 479.1586
fit.i <- ergm(formula = nw ~ edges,
              target.stats = stats,
              control = control.ergm(MCMC.interval = 1e+5,
                                     MCMC.samplesize = 7500,
                                     MCMC.burnin = 1e+6,
                                     MPLE.max.dyad.types = 1e+7,
                                     init.method = "zeros",
                                     MCMLE.maxit = 400,
                                     parallel = np/2, 
                                     parallel.type="PSOCK"))