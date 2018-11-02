# WHAMP network estimation for the main model#
# This file tests main models with different specifications for regional mixing #

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



#1. 60% regional assortativity -----------------------------------------------------------

# Formulas
formation.m1 <- ~edges +
  nodefactor("deg.pers") +
  nodefactor("race..wa", base=3) + 
  nodefactor("region", base=2) +
  nodematch("race..wa", diff=TRUE) +
  absdiff("sqrt.age") +
  nodematch("region", diff=FALSE) +
  degrange(from = 2) + 
  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

# Calculate target stat for regional mixing
regionmatch <- 0.6*st$stats.m[1]

# Add target stat for degrange
stats.m1 = c(st$stats.m, regionmatch, 0)

# Fit model
fit.m1 <- netest(nw.main,
                 formation = formation.m1,
                 target.stats = stats.m1,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = st$coef.diss.m,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/4, 
                                                 parallel.type="PSOCK"))

#2. 70% regional assortativity -----------------------------------------------------------

# Formulas
formation.m2 <- ~edges +
  nodefactor("deg.pers") +
  nodefactor("race..wa", base=3) + 
  nodefactor("region", base=2) +
  nodematch("race..wa", diff=TRUE) +
  absdiff("sqrt.age") +
  nodematch("region", diff=FALSE) +
  degrange(from = 2) + 
  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

# Calculate target stat for regional mixing
regionmatch <- 0.7*st$stats.m[1]

# Add target stat for degrange
stats.m2 = c(st$stats.m, regionmatch, 0)

# Fit model
fit.m2 <- netest(nw.main,
                 formation = formation.m2,
                 target.stats = stats.m2,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = st$coef.diss.m,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/4, 
                                                 parallel.type="PSOCK"))

#3. 80% regional assortativity -----------------------------------------------------------

# Formulas
formation.m3 <- ~edges +
  nodefactor("deg.pers") +
  nodefactor("race..wa", base=3) + 
  nodefactor("region", base=2) +
  nodematch("race..wa", diff=TRUE) +
  absdiff("sqrt.age") +
  nodematch("region", diff=FALSE) +
  degrange(from = 2) + 
  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

# Calculate target stat for regional mixing
regionmatch <- 0.8*st$stats.m[1]

# Add target stat for degrange
stats.m3 = c(st$stats.m, regionmatch, 0)

# Fit model
fit.m3 <- netest(nw.main,
                 formation = formation.m3,
                 target.stats = stats.m3,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = st$coef.diss.m,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/4, 
                                                 parallel.type="PSOCK"))

#4. 90% regional assortativity -----------------------------------------------------------

# Formulas
formation.m4 <- ~edges +
  nodefactor("deg.pers") +
  nodefactor("race..wa", base=3) + 
  nodefactor("region", base=2) +
  nodematch("race..wa", diff=TRUE) +
  absdiff("sqrt.age") +
  nodematch("region", diff=FALSE) +
  degrange(from = 2) + 
  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

# Calculate target stat for regional mixing
regionmatch <- 0.8*st$stats.m[1]

# Add target stat for degrange
stats.m4 = c(st$stats.m, regionmatch, 0)

# Fit model
fit.m4 <- netest(nw.main,
                 formation = formation.m4,
                 target.stats = stats.m4,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = st$coef.diss.m,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/4, 
                                                 parallel.type="PSOCK"))

#5. 100% regional assortativity-------------------------------------------------

# Formulas
formation.m5 <- ~edges +
  nodefactor("deg.pers") +
  nodefactor("race..wa", base=3) + 
  nodefactor("region", base=2) +
  nodematch("race..wa", diff=TRUE) +
  absdiff("sqrt.age") +
  degrange(from = 2) + 
  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) +
  offset(nodemix("region", base = c(1,3,6)))

# Add target stat for degrange
stats.m5 = c(st$stats.m, 0)

# Fit model
fit.m5 <- netest(nw.main,
                 formation = formation.m5,
                 target.stats = stats.m5,
                 coef.form = c(-Inf, -Inf, -Inf, -Inf, -Inf),
                 coef.diss = st$coef.diss.m,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/4, 
                                                 parallel.type="PSOCK"))

# Save fits ---------------------------------------------------------------
if (balanced == 1) {
  est.m.testregionmix.bal <- list(fit.m1, fit.m2, fit.m3, fit.m4, fit.m5)
  save(est.m.testregionmix.bal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.m.testregionmix.bal.rda")
} else {
  est.m.testregionmix.unbal <- list(fit.m1, fit.m2, fit.m3, fit.m4, fit.m5)
  save(est.m.testregionmix.unbal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.m.testregionmix.unbal.rda")
}

