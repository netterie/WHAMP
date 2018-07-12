##----------------------------------------------------------------
## This script runs 1 simulation over 10 steps for use in debugging the model ##
##----------------------------------------------------------------

## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))

## Parameters
load("/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/est/nwstats.whamp.rda") 
load("/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/est/fit.whamp.rda")

param <- param_msm_whamp(nwstats = st)
init <- init_msm_whamp(nwstats = st)
control <- control_msm_whamp(nsteps = 10,
                             nsims = 1,
                             verbose = FALSE)

## Simulation
sim.test.1 <- netsim(est, param, init, control) # rename the simluation file if want to save it for future reference
save(sim.test.1, file="/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/fit tests and debugging/sim.test.rda") # rename the simluation file if want to save it for future reference



