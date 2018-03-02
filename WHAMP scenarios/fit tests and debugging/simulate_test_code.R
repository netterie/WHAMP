##----------------------------------------------------------------
## This script runs 1 simulation over 10 steps for use in debugging the model ##
##----------------------------------------------------------------

## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))

## Parameters
load("WHAMP scenarios/est/nwstats.whamp.rda") 
load("WHAMP scenarios/est/fit.whamp.rda")

param <- param_msm_whamp(nwstats = st,
                   rgc.tprob = 0.398,
                   ugc.tprob = 0.320,
                   rct.tprob = 0.221,
                   uct.tprob = 0.205,
                   rct.asympt.int = 254.1,
                   uct.asympt.int = 254.1,
                   hiv.rgc.rr = 2.78,
                   hiv.ugc.rr = 1.73,
                   hiv.rct.rr = 2.78,
                   hiv.uct.rr = 1.73,
                   hiv.dual.rr = 0.2)
init <- init_msm_whamp(nwstats = st)
control <- control_msm_whamp(nsteps = 10,
                             nsims = 1,
                             verbose = FALSE)

## Simulation
sim.test1 <- netsim(est, param, init, control) # rename the simluation file if want to save it for future reference
save(sim.test1, file="WHAMP scenarios/fit tests and debugging/sim.test.rda") # rename the simluation file if want to save it for future reference



##----------------------------------------------------------------
## This script runs 1 simulation over 10 steps IN THE ABSENCE OF HIV ##
##----------------------------------------------------------------

## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))

## Parameters
load("WHAMP scenarios/est/nwstats.whamp.rda") 
load("WHAMP scenarios/est/fit.whamp.rda")

param <- param_msm_whamp(nwstats = st,
                         rgc.tprob = 0.398,
                         ugc.tprob = 0.320,
                         rct.tprob = 0.221,
                         uct.tprob = 0.205,
                         rct.asympt.int = 254.1,
                         uct.asympt.int = 254.1,
                         hiv.rgc.rr = 2.78,
                         hiv.ugc.rr = 1.73,
                         hiv.rct.rr = 2.78,
                         hiv.uct.rr = 1.73,
                         hiv.dual.rr = 0.2)
init.nohiv <- init_msm_whamp(nwstats = st, 
                       prev.B = 0,
                       prev.W = 0,
                       prev.H..wa = 0,
                       prev.B..wa = 0,
                       prev.O..wa = 0)
control <- control_msm_whamp(nsteps = 10,
                             nsims = 1,
                             verbose = FALSE)

## Simulation
sim.test.nohiv <- netsim(est, param, init.nohiv, control) # rename the simluation file if want to save it for future reference
save(sim.test.nohiv, file="WHAMP scenarios/fit tests and debugging/sim.test.nohiv.rda") # rename the simluation file if want to save it for future reference
