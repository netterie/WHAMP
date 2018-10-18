# WHAMP network estimation for the main model#
# This file examines the main model fit with and without the age mixing term. We look at the full model
# with the constraint that all ties are within-region and the full model assuming 90% within-region ties

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

# 1.a) Full model with 100% regional assorativity and without age mixing
#       Edges + nodefactor(race) + nodefactor(region) + nodefactor(deg.pers) + nodematch(race) + offset for region -----------------------------------------------------------
#       (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m1a <- ~edges +
  nodefactor("deg.pers") +
  nodefactor("race..wa", base=3) + 
  nodefactor("region", base=2) +
  nodematch("race..wa", diff=TRUE) +
  degrange(from = 2) + 
  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) +
  offset(nodemix("region", base = c(1,3,6)))

# Add target stat for degrange
stats.m1a = c(st$stats.m[-11], 0)

# Fit model
fit.m1a <- netest(nw.main,
                 formation = formation.m1a,
                 target.stats = stats.m1a,
                 coef.form = c(-Inf, -Inf, -Inf, -Inf, -Inf),
                 coef.diss = st$coef.diss.m,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/2, 
                                                 parallel.type="PSOCK"))

# 1.b) Full model with 100% regional assorativity WITH age mixing
#     Edges + nodefactor(race) + nodefactor(region) + nodefactor(deg.pers) + nodematch(race) + absdiff(sqrtage) + offset for region -----------------------------------------------------------
#     (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m1b <- ~edges +
                  nodefactor("deg.pers") +
                  nodefactor("race..wa", base=3) + 
                  nodefactor("region", base=2) +
                  nodematch("race..wa", diff=TRUE) +
                  absdiff("sqrt.age") +
                  degrange(from = 2) + 
                  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) +
                  offset(nodemix("region", base = c(1,3,6)))

# Add target stat for degrange
stats.m1b = c(st$stats.m, 0)

# Fit model
fit.m1b <- netest(nw.main,
                 formation = formation.m1b,
                 target.stats = stats.m1b,
                 coef.form = c(-Inf, -Inf, -Inf, -Inf, -Inf),
                 coef.diss = st$coef.diss.m,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/2, 
                                                 parallel.type="PSOCK"))

# 2.a) Alternate full model with 90% regional assorativity and WITHOUT age mixing
#       Edges + nodefactor(race) + nodefactor(region) + nodefactor(deg.pers) + nodematch(race) + nodematch(region) -----------------------------------------------------------
#       (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m2a <- ~edges +
                  nodefactor("deg.pers") +
                  nodefactor("race..wa", base=3) + 
                  nodefactor("region", base=2) +
                  nodematch("race..wa", diff=TRUE) +
                  nodematch("region", diff=FALSE) +
                  degrange(from = 2) + 
                  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

# Calculate target stat for regional mixing
regionmatch <- 0.9*st$stats.m[1]

# Add target stat for degrange
stats.m2a = c(st$stats.m[-11], regionmatch, 0)

# Fit model
fit.m2a <- netest(nw.main,
                 formation = formation.m2a,
                 target.stats = stats.m2a,
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

# 2.b) Alternate full model with 90% regional assorativity and WITH age mixing
#       Edges + nodefactor(race) + nodefactor(region) + nodefactor(deg.pers) + nodematch(race) + absdiff(sqrtage) + nodematch(region) -----------------------------------------------------------
#       (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m2b <- ~edges +
                  nodefactor("deg.pers") +
                  nodefactor("race..wa", base=3) + 
                  nodefactor("region", base=2) +
                  nodematch("race..wa", diff=TRUE) +
                  absdiff("sqrt.age") +
                  nodematch("region", diff=FALSE) +
                  degrange(from = 2) + 
                  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

# Calculate target stat for regional mixing
regionmatch <- 0.9*st$stats.m[1]

# Add target stat for degrange
stats.m2b = c(st$stats.m, regionmatch, 0)

# Fit model
fit.m2b <- netest(nw.main,
                  formation = formation.m2b,
                  target.stats = stats.m2b,
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
  est.m.testagemix.bal <- list(fit.m1a, fit.m1b, fit.m2a, fit.m2b)
  save(est.m.testagemix.bal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.m.testagemix.bal.rda")
} else {
  est.m.testagemix.unbal <- list(fit.m1a, fit.m1b, fit.m2a, fit.m2b)
  save(est.m.testagemix.unbal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.m.testagemix.unbal.rda")
}

