# WHAMP network estimation for the instantaneous model#
# This file sequentially builds up the model to identify where fit and convergence issues arise #

rm(list = ls())
suppressMessages(library("EpiModelHIV"))
library("parallel")
np <- detectCores()

# Indicate whether using the balanced or unbalanced target stats
balanced = 0

# Load the appropriate file (balanced or unbalanced)
if (balanced == 1) {
load(file = "/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/est/nwstats.balanced.whamp.rda")
  } else {
load(file = "/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/est/nwstats.unbalanced.whamp.rda")
  }

# Initialize main, pers, and inst networks -----------------------------------------------------------

# Initialize main network
nw.main <- base_nw_msm_whamp(st)

  # Assign degree
  nw.main <- assign_degree_whamp(nw.main, deg.type = "pers", nwstats = st)

# Initialize pers network
nw.pers <- nw.main

  # Assign degree
  nw.pers <- assign_degree_whamp(nw.pers, deg.type = "main", nwstats = st)

# Initialize inst network
nw.inst <- nw.main

  # Assign degree
  nw.inst <- set.vertex.attribute(nw.inst, "deg.main", nw.pers %v% "deg.main")
  nw.inst <- set.vertex.attribute(nw.inst, "deg.pers", nw.main %v% "deg.pers")
  table(nw.inst %v% "deg.main", nw.inst %v% "deg.pers")
  
  
# 0. Try fitting as an ergm with edges only -------------------------------
  stats.i <- st$stats.i[1]
  fit.i <- ergm(formula = nw.inst ~ edges,
                target.stats = stats.i,
                control = control.ergm(MCMC.interval = 1e+5,
                                       MCMC.samplesize = 7500,
                                       MCMC.burnin = 1e+6,
                                       MPLE.max.dyad.types = 1e+7,
                                       init.method = "zeros",
                                       MCMLE.maxit = 400,
                                       parallel = np/2, 
                                       parallel.type="PSOCK"))

# 1. Edges only ----------------------------------------------------------
#    (+ offset for role class)
  
  # Formulas
  formation.i1 <- ~edges +
                  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
  
  stats.i1 <- st$stats.i[1]
  
  # Fit model
  fit.i1 <- netest(nw.inst,
                  formation = formation.i1,
                  target.stats = stats.i1,
                  coef.form = c(-Inf, -Inf),
                  coef.diss = dissolution_coefs(~offset(edges), 1),
                  set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                  MCMC.samplesize = 7500,
                                                  MCMC.burnin = 1e+6,
                                                  MPLE.max.dyad.types = 1e+7,
                                                  init.method = "zeros",
                                                  MCMLE.maxit = 400,
                                                  parallel = np/2, 
                                                  parallel.type="PSOCK"))

# 2. Edges + nodefactor(race) ----------------------------------------------------------
#    (+ offset for role class)
  
  
  # Formulas
  formation.i2 <- ~edges +
                  nodefactor("race..wa", base=3) + 
                  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
  
  stats.i2 <- st$stats.i[c(1,14,15)]
  
  # Fit model
  fit.i2 <- netest(nw.inst,
                   formation = formation.i2,
                   target.stats = stats.i2,
                   coef.form = c(-Inf, -Inf),
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                   MCMC.samplesize = 7500,
                                                   MCMC.burnin = 1e+6,
                                                   MPLE.max.dyad.types = 1e+7,
                                                   init.method = "zeros",
                                                   MCMLE.maxit = 400,
                                                   parallel = np/2, 
                                                   parallel.type="PSOCK"))

# 2. Edges + nodefactor(race) + nodefactor(region)----------------------------------------------------------
#    (+ offset for role class)
  
  
  # Formulas
  formation.i3 <- ~edges +
                  nodefactor("race..wa", base=3) + 
                  nodefactor("region", base=2) +
                  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
  
  stats.i3 <- st$stats.i[c(1,14:17)]
  
  # Fit model
  fit.i3 <- netest(nw.inst,
                   formation = formation.i3,
                   target.stats = stats.i3,
                   coef.form = c(-Inf, -Inf),
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                   MCMC.samplesize = 7500,
                                                   MCMC.burnin = 1e+6,
                                                   MPLE.max.dyad.types = 1e+7,
                                                   init.method = "zeros",
                                                   MCMLE.maxit = 400,
                                                   parallel = np/2, 
                                                   parallel.type="PSOCK"))
  
# 3. Edges + nodefactor(race) + nodefactor(region) + nodefactor(deg.main, deg.pers)----------------------------------------------------------
#    (+ offset for role class)

  
  # Formulas
  formation.i3 <- ~edges +
                  nodefactor(c("deg.main", "deg.pers")) +
                  nodefactor("race..wa", base=3) + 
                  nodefactor("region", base=2) +
                  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
                
  stats.i3 <- st$stats.i[c(1:6,14:17)]
  
  # Fit model
  fit.i3 <- netest(nw.inst,
                   formation = formation.i3,
                   target.stats = stats.i3,
                   coef.form = c(-Inf, -Inf),
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                   MCMC.samplesize = 7500,
                                                   MCMC.burnin = 1e+6,
                                                   MPLE.max.dyad.types = 1e+7,
                                                   init.method = "zeros",
                                                   MCMLE.maxit = 400,
                                                   parallel = np/2, 
                                                   parallel.type="PSOCK"))

...


XXX THIS IS THE FULL MODEL WILL BUILD UP TO:

# Formulas
formation.i <- ~edges +
                nodefactor(c("deg.main", "deg.pers")) +
                nodefactor("riskg") +
                nodefactor("race..wa", base=3) + 
                nodefactor("region", base=2) +
                nodematch("race..wa", diff=TRUE) +
                nodematch("region", diff=FALSE) +
                absdiff("sqrt.age") +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 

# Start clock to calculate runtime
startclock_inst <- proc.time()

# Fit model
fit.i <- netest(nw.inst,
                formation = formation.i,
                target.stats = st$stats.i,
                coef.form = c(-Inf, -Inf),
                coef.diss = dissolution_coefs(~offset(edges), 1),
                set.control.ergm = control.ergm(MCMC.interval = 1e+4,
                                                MCMC.samplesize = 7500,
                                                MCMC.burnin = 1e+6,
                                                MPLE.max.dyad.types = 1e+7,
                                                init.method = "zeros",
                                                MCMLE.maxit = 400,
                                                parallel = np/2, 
                                                parallel.type="PSOCK"))

# Fit time
runtime_min_fit_inst <- (proc.time()-startclock_inst)['elapsed']/60


# Save data ---------------------------------------------------------------
if (balanced == 1) {
  est.bal <- list(fit.m, fit.p, fit.i, runtime_min_fit_main, runtime_min_fit_pers, runtime_min_fit_inst)
  save(est.bal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/est/fit.bal.whamp.rda")
} else {
  est.unbal <- list(fit.m, fit.p, fit.i, runtime_min_fit_main, runtime_min_fit_pers, runtime_min_fit_inst)
  save(est.unbal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/est/fit.unbal.whamp.rda")
}

