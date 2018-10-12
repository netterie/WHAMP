# WHAMP network estimation for the main model#
# This file tries to identify which term added to a model with nodefactor and nodematch race creates issues with MCMC diagnostics for nodematch race

rm(list = ls())
suppressMessages(library("EpiModelHIV"))
library("parallel")
np <- detectCores()

# Indicate whether using the balanced or unbalanced target stats
balanced = 1

# Load the appropriate file (balanced or unbalanced)
if (balanced == 1) {
  load(file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/est/nwstats.balanced.whamp.rda")
} else {
  load(file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/est/nwstats.unbalanced.whamp.rda")
}

# Initialize main network and assign degree -----------------------------------------------------------
nw.main <- base_nw_msm_whamp(st)

nw.main <- assign_degree_whamp(nw.main, deg.type = "pers", nwstats = st)

# 1. Edges + nodefactor(race) + nodematch(race) -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m1a <- ~edges +
  nodefactor("race..wa", base=3) + 
  nodematch("race..wa", diff=TRUE) +
  degrange(from = 2) +
  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 


stats.m1a <- c(st$stats.m[c(1,4,5,8:10)], 0)

# Fit model
fit.m1a <- netest(nw.main,
                 formation = formation.m1a,
                 target.stats = stats.m1a,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = st$coef.diss.m,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/2, 
                                                 parallel.type="PSOCK"))

# 2. Edges + nodefactor(race) + nodematch(race) + nodefactor(degpers)-----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m1b <- ~edges +
  nodefactor("deg.pers") + 
  nodefactor("race..wa", base=3) + 
  nodematch("race..wa", diff=TRUE) +
  degrange(from = 2) +
  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 


stats.m1b <- c(st$stats.m[c(1:5,8:10)], 0)

# Fit model
fit.m1b <- netest(nw.main,
                  formation = formation.m1b,
                  target.stats = stats.m1b,
                  coef.form = c(-Inf, -Inf),
                  coef.diss = st$coef.diss.m,
                  set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                  MCMC.samplesize = 7500,
                                                  MCMC.burnin = 1e+6,
                                                  MPLE.max.dyad.types = 1e+7,
                                                  init.method = "zeros",
                                                  MCMLE.maxit = 400,
                                                  parallel = np/2, 
                                                  parallel.type="PSOCK"))

# 3. Edges + nodefactor(race) + nodematch(race) + nodefactor(region) -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m1c <- ~edges +
  nodefactor("race..wa", base=3) + 
  nodefactor("region", base=2) +
  nodematch("race..wa", diff=TRUE) +
  degrange(from = 2) +
  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 


stats.m1c <- c(st$stats.m[c(1,4:10)], 0)

# Fit model
fit.m1c <- netest(nw.main,
                  formation = formation.m1c,
                  target.stats = stats.m1c,
                  coef.form = c(-Inf, -Inf),
                  coef.diss = st$coef.diss.m,
                  set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                  MCMC.samplesize = 7500,
                                                  MCMC.burnin = 1e+6,
                                                  MPLE.max.dyad.types = 1e+7,
                                                  init.method = "zeros",
                                                  MCMLE.maxit = 400,
                                                  parallel = np/2, 
                                                  parallel.type="PSOCK"))

# 4. Edges + nodefactor(race) + nodematch(race) + nodefactor(region) + nodefactor(deg.pers) -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m1d <- ~edges +
  nodefactor("deg.pers") +
  nodefactor("race..wa", base=3) +
  nodefactor("region", base=2) +
  nodematch("race..wa", diff=TRUE) +
  degrange(from = 2) +
  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 


stats.m1d <- c(st$stats.m[c(1:10)], 0)

# Fit model
fit.m1d <- netest(nw.main,
                  formation = formation.m1d,
                  target.stats = stats.m1d,
                  coef.form = c(-Inf, -Inf),
                  coef.diss = st$coef.diss.m,
                  set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                  MCMC.samplesize = 7500,
                                                  MCMC.burnin = 1e+6,
                                                  MPLE.max.dyad.types = 1e+7,
                                                  init.method = "zeros",
                                                  MCMLE.maxit = 400,
                                                  parallel = np/2, 
                                                  parallel.type="PSOCK"))

# Save fits ---------------------------------------------------------------
if (balanced == 1) {
  est.m.testracemix.bal <- list(fit.m1a, fit.m1b, fit.m1c, fit.m1d)
  save(est.m.testracemix.bal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.m.testracemix.bal.rda")
} else {
  est.m.testracemix.unbal <- list(fit.m1a, fit.m1b, fit.m1c, fit.m1d)
  save(est.m.testracemix.unbal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.m.testracemix.unbal.rda")
}

