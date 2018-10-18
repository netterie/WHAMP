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
load(file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/est/nwstats.balanced.whamp.rda")
  } else {
load(file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/est/nwstats.unbalanced.whamp.rda")
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
  
  
# # 0. Try fitting as an ergm with edges only -------------------------------
#   stats.i <- st$stats.i[1]
#   fit.i <- ergm(formula = nw.inst ~ edges,
#                 target.stats = stats.i,
#                 control = control.ergm(MCMC.interval = 1e+5,
#                                        MCMC.samplesize = 7500,
#                                        MCMC.burnin = 1e+6,
#                                        MPLE.max.dyad.types = 1e+7,
#                                        init.method = "zeros",
#                                        MCMLE.maxit = 400,
#                                        parallel = np/2, 
#                                        parallel.type="PSOCK"))

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
                                                   MCMLE.maxit = 400,
                                                   parallel = np/2, 
                                                   parallel.type="PSOCK"))

# 3. Edges + nodefactor(race) + nodematch(race)----------------------------------------------------------
#    (+ offset for role class)
  
  # Formulas
  formation.i3 <- ~edges +
                  nodefactor("race..wa", base=3) + 
                  nodematch("race..wa", diff=TRUE) +
                  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
                
  stats.i3 <- st$stats.i[c(1,14,15,18:20)]
  
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
                                                   MCMLE.maxit = 400,
                                                   parallel = np/2, 
                                                   parallel.type="PSOCK"))

# 4. Edges + nodefactor(race) + nodematch(race) + nodefactor(deg)----------------------------------------------------------
#    (+ offset for role class)

  # Formulas
  formation.i4 <- ~edges +
                    nodefactor(c("deg.main", "deg.pers")) +
                    nodefactor("race..wa", base=3) + 
                    nodematch("race..wa", diff=TRUE) +
                    offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
  
  stats.i4 <- st$stats.i[c(1:6,14,15,18:20)]
  
  # Fit model
  fit.i4 <- netest(nw.inst,
                   formation = formation.i4,
                   target.stats = stats.i4,
                   coef.form = c(-Inf, -Inf),
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                   MCMC.samplesize = 7500,
                                                   MCMC.burnin = 1e+6,
                                                   MPLE.max.dyad.types = 1e+7,
                                                   MCMLE.maxit = 400,
                                                   parallel = np/2, 
                                                   parallel.type="PSOCK"))
  
# 5. Edges + nodefactor(race) + nodematch(race) + nodefactor(deg) + nodefactor(region)----------------------------------------------------------
#    (+ offset for role class)
  
  # Formulas
  formation.i5 <- ~edges +
                    nodefactor(c("deg.main", "deg.pers")) +
                    nodefactor("race..wa", base=3) + 
                    nodefactor("region", base=2) +
                    nodematch("race..wa", diff=TRUE) +
                    offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
  
  stats.i5 <- st$stats.i[c(1:6,14:20)]
  
  # Fit model
  fit.i5 <- netest(nw.inst,
                   formation = formation.i5,
                   target.stats = stats.i5,
                   coef.form = c(-Inf, -Inf),
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                   MCMC.samplesize = 7500,
                                                   MCMC.burnin = 1e+6,
                                                   MPLE.max.dyad.types = 1e+7,
                                                   MCMLE.maxit = 400,
                                                   parallel = np/2, 
                                                   parallel.type="PSOCK"))  
  
# 6. Edges + nodefactor(race) + nodematch(race) + nodefactor(deg) + nodefactor(region) + absdiff(sqrtage)----------------------------------------------------------
#    (+ offset for role class)
  
  # Formulas
  formation.i6 <- ~edges +
                    nodefactor(c("deg.main", "deg.pers")) +
                    nodefactor("race..wa", base=3) + 
                    nodefactor("region", base=2) +
                    nodematch("race..wa", diff=TRUE) +
                    absdiff("sqrt.age") +
                    offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
  
  stats.i6 <- st$stats.i[c(1:6,14:20,22)]
  
  # Fit model
  fit.i6 <- netest(nw.inst,
                   formation = formation.i6,
                   target.stats = stats.i6,
                   coef.form = c(-Inf, -Inf),
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                   MCMC.samplesize = 7500,
                                                   MCMC.burnin = 1e+6,
                                                   MPLE.max.dyad.types = 1e+7,
                                                   MCMLE.maxit = 400,
                                                   parallel = np/2, 
                                                   parallel.type="PSOCK")) 

# 7. Edges + nodefactor(race) + nodematch(race) + nodefactor(deg) + nodefactor(region) + absdiff(sqrtage) + nodefactor(riskg)----------------------------------------------------------
#    (+ offset for role class)
  
  # Formulas
  formation.i7 <- ~edges +
                    nodefactor(c("deg.main", "deg.pers")) +
                    nodefactor("riskg") +
                    nodefactor("race..wa", base=3) + 
                    nodefactor("region", base=2) +
                    nodematch("race..wa", diff=TRUE) +
                    absdiff("sqrt.age") +
                    offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
  
  stats.i7 <- st$stats.i[c(1:20,22)]
  
  # Fit model
  fit.i7 <- netest(nw.inst,
                   formation = formation.i7,
                   target.stats = stats.i7,
                   coef.form = c(-Inf, -Inf),
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                   MCMC.samplesize = 7500,
                                                   MCMC.burnin = 1e+6,
                                                   MPLE.max.dyad.types = 1e+7,
                                                   MCMLE.maxit = 400,
                                                   parallel = np/2, 
                                                   parallel.type="PSOCK")) 

# 8. Full model: Edges + nodefactor(race) + nodematch(race) + nodefactor(deg) + nodefactor(region) + absdiff(sqrtage) + nodefactor(riskg) + nodematch(region)----------------------------------------------------------
#    (+ offset for role class)
  
  # Formulas
  formation.i8 <- ~edges +
                    nodefactor(c("deg.main", "deg.pers")) +
                    nodefactor("riskg") +
                    nodefactor("race..wa", base=3) + 
                    nodefactor("region", base=2) +
                    nodematch("race..wa", diff=TRUE) +
                    nodematch("region", diff=FALSE) +
                    absdiff("sqrt.age") +
                    offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
  
  stats.i8 <- st$stats.i
  
  # Fit model
  fit.i8 <- netest(nw.inst,
                   formation = formation.i8,
                   target.stats = stats.i8,
                   coef.form = c(-Inf, -Inf),
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                   MCMC.samplesize = 7500,
                                                   MCMC.burnin = 1e+6,
                                                   MPLE.max.dyad.types = 1e+7,
                                                   MCMLE.maxit = 400,
                                                   parallel = np/2, 
                                                   parallel.type="PSOCK")) 
  

# Save data ---------------------------------------------------------------
  if (balanced == 1) {
    est.i.buildup.bal <- list(fit.i1, fit.i2, fit.i3, fit.i4, fit.i5, fit.i6, fit.i7, fit.i8)
    save(est.i.buildup.bal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.i.buildup.bal.rda")
  } else {
    est.i.buildup.unbal <- list(fit.i1, fit.i2, fit.i3, fit.i4, fit.i5, fit.i6, fit.i7, fit.i8)
    save(est.i.buildup.unbal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.i.buildup.unbal.rda")
  }

