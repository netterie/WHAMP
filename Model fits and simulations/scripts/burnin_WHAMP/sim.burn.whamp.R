
## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))

# ## Environmental Arguments - use these only when running simulations on Hyak
# simno <- Sys.getenv("SIMNO")
# jobno <- Sys.getenv("PBS_ARRAYID")
# njobs <- as.numeric(Sys.getenv("NJOBS"))
# fsimno <- paste(simno, jobno, sep = ".")

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
# Use this code for control when running on csde or home machine
control <- control_msm_whamp(nsteps = 3120,
                             nsims = 16, ncores=16,
                             verbose = FALSE)

## Simulation
  # Use this version when running simulations on CSDE or home machine
  sim1 <- netsim(est, param, init, control)

  ## Use this code when running simulations on Hyak
  # netsim_hpc(est, param, init, control, verbose = FALSE)
  # process_simfiles(simno = simno, min.n = njobs, compress = TRUE,
  #                  outdir = "data/", verbose = FALSE)
