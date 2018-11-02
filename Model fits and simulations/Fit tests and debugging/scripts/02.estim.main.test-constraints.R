# This file tests issues with the main model fit

#' We see slight issues with the fit as early as model 3 in the main.buildup file.
#' Model 3 is pretty simple (edges + nodefactor(race) + nodematch(race)), so here 
#' we test whether the fit issues are due to the constraint that men can have at most 
#' one partner at a time or the constraint on role class by fitting a model without those
#' constraints and adding them individually and together to see the impact on fit

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


# 1) Model without constraints: Edges + nodefactor(race) + nodefactor(region) -----------------------------------------------------------

# Formulas
formation.m1 <- ~edges +
  nodefactor("race..wa", base=3) + 
  nodematch("race..wa", diff=TRUE) 


stats.m1 <- c(st$stats.m[c(1,4,5,8:10)])

# Fit model
fit.m1 <- netest(nw.main,
                 formation = formation.m1,
                 target.stats = stats.m1,
                 coef.diss = st$coef.diss.m,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 MCMLE.maxit = 400,
                                                 parallel = np/4, 
                                                 parallel.type="PSOCK"))

# 2) Model with deg constraint: Edges + nodefactor(race) + nodefactor(region) + degrange-----------------------------------------------------------

# Formulas
formation.m2 <- ~edges +
  nodefactor("race..wa", base=3) + 
  nodematch("race..wa", diff=TRUE) +
  degrange(from = 2)


stats.m2 <- c(st$stats.m[c(1,4,5,8:10)], 0)

# Fit model
fit.m2 <- netest(nw.main,
                 formation = formation.m2,
                 target.stats = stats.m2,
                 coef.diss = st$coef.diss.m,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 MCMLE.maxit = 400,
                                                 parallel = np/4, 
                                                 parallel.type="PSOCK"))

# 3) Model with role class constraint: Edges + nodefactor(race) + nodefactor(region) + offset(nodematch)-----------------------------------------------------------

# Formulas
formation.m3 <- ~edges +
  nodefactor("race..wa", base=3) + 
  nodematch("race..wa", diff=TRUE) +
  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 

stats.m3 <- c(st$stats.m[c(1,4,5,8:10)])

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
                                                 MCMLE.maxit = 400,
                                                 parallel = np/4, 
                                                 parallel.type="PSOCK"))

# 4) Model with both constraints: Edges + nodefactor(race) + nodefactor(region) + degrange + offset(nodematch)-----------------------------------------------------------

# Formulas
formation.m4 <- ~edges +
  nodefactor("race..wa", base=3) + 
  nodematch("race..wa", diff=TRUE) +
  degrange(from = 2) +
  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 

stats.m4 <- c(st$stats.m[c(1,4,5,8:10)], 0)

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
                                                 MCMLE.maxit = 400,
                                                 parallel = np/4, 
                                                 parallel.type="PSOCK"))
