# WHAMP network estimation for the instantaneous model#
# This file tests fitting a model with only edges and risk groups.

rm(list = ls())
suppressMessages(library("EpiModelHIV"))
library("parallel")
np <- detectCores()

# SETUP ----

# Indicate whether using the balanced or unbalanced target stats
balanced = 1

# Load the appropriate file (balanced or unbalanced)
if (balanced == 1) {
    load(file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/est/nwstats.balanced.whamp.rda")
} else {
    load(file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/est/nwstats.unbalanced.whamp.rda")
}

# Define alternate target stats for risk groups bump up lowest quartiles from 0 to 0.0001.
num <- 10000
time.unit <- 7
agestr <- sumto1(c(0.1594, 0.1319, 0.1292, 0.1173, 0.1183, 0.1148, 0.1071, 0.122))
num.50to59 <- num*sum(agestr[7:8])
num.18to49 <- num*sum(agestr[1:6])
qnts.18to49 <- c(0, 0.000608, 0.005247, 0.056484) 
qnts.50to59 <- c(0, 0, 0.00171, 0.027315) 
qnts.18to49.bump <- c(0.0001, 0.000608, 0.005247, 0.056484) 
qnts.50to59.bump <- c(0.0001, 0.0001, 0.00171, 0.027315) 
num.riskg.50to59 <- (0.25*num.50to59) * qnts.50to59 * time.unit
num.riskg.18to49 <- (0.25*num.18to49) * qnts.18to49 * time.unit
num.riskg <- c(num.riskg.50to59, num.riskg.18to49)
num.riskg.50to59.bump <- (0.25*num.50to59) * qnts.50to59.bump * time.unit
num.riskg.18to49.bump <- (0.25*num.18to49) * qnts.18to49.bump * time.unit
num.riskg.bump <- c(num.riskg.50to59.bump, num.riskg.18to49.bump)
riskg <- num.riskg[-8]
riskg.bump <- num.riskg.bump[-8]


# Initialize main, pers, and inst networks 

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


# Set control settings
control.i <- control.ergm(MCMC.interval = 1e+5,
                        MCMC.samplesize = 7500,
                        MCMC.burnin = 1e+6,
                        MPLE.max.dyad.types = 1e+7,
                        MCMLE.maxit = 400,
                        parallel = np/2, 
                        parallel.type="PSOCK")

# 1. Edges + riskg with lowest groups as 0 ----------------------------------------------------------
#    (+ offset for role class)

# Formulas
formation.i1 <- ~edges +
                nodefactor("riskg", base=8) +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 

stats.i1 <- c(st$stats.i[1], riskg)

# Fit model
fit.i1 <- netest(nw.inst,
                 formation = formation.i1,
                 target.stats = stats.i1,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = dissolution_coefs(~offset(edges), 1),
                 set.control.ergm = control.i)

# Save fit
if (balanced == 1) {
  est.i.testriskg_zeros.bal <- fit.i1
  save(est.i.testriskg_zeros.bal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.i.testriskg_zeros.bal.rda")
} else {
  est.i.testriskg_zeros.unbal <- fit.i1
  save(est.i.testriskg_zeros.unbal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.i.testriskg_zeros.unbal.rda")
}

# 2. Edges + riskg with lowest groups as 0.0001 ----------------------------------------------------------
#    (+ offset for role class)
### THIS MODEL DOESNT FIT - GIVES DEGENERACY WARNING ###

  # Formulas
  formation.i2 <- ~edges +
      nodefactor("riskg", base=8) +
      offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
  
  stats.i2 <- c(st$stats.i[1], riskg.bump)


  # Fit model
  fit.i2 <- netest(nw.inst,
                   formation = formation.i2,
                   target.stats = stats.i2,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = dissolution_coefs(~offset(edges), 1),
                 set.control.ergm = control.i)

  # Save fit
  if (balanced == 1) {
    est.i.testriskg_bumped.bal <- fit.i1
    save(est.i.testriskg_bumped.bal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.i.testriskg_bumped.bal.rda")
  } else {
    est.i.testriskg_bumped.unbal <- fit.i1
    save(est.i.testriskg_bumped.unbal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.i.testriskg_bumped.unbal.rda")
  }
  
# 3. Simplified scenario with 4 risk groups with made up target stats, highest as reference, no offset for role class ----------------------------------------------------------
  n <- 10000
  nw.test <- network.initialize(n = n, directed = FALSE)
  nw.test <- set.vertex.attribute(nw.test, attrname = "groups", value = rep(c("G1", "G2", "G3", "G4"), each = n/4))
  nw.test <- set.vertex.attribute(nw.test, attrname = "role.class", value = sample(apportion_lr(n, c("I", "R", "V"), st$role.prob)))
  
  # target stats
  edges <- st$stats.i[1]
  groups <- c(0, 10, 100, edges*2 - 110)
  targets.test1 <- c(edges, groups[-4])
  
  # Fit model
  formation.i3 <- ~edges +
    nodefactor("groups", base=4) 
  
  fit.i3 <- netest(nw.test,
                   formation = formation.i3,
                   target.stats = targets.test1,
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   set.control.ergm = control.i)

# 4. Simplified scenario with 4 risk groups with made up target stats, lowest as reference, no offset for role class ----------------------------------------------------------

  # target stats
  edges <- st$stats.i[1]
  groups <- c(0, 10, 100, edges*2 - 110)
  targets.test2 <- c(edges, groups[-1])
  
  # Fit model
  formation.i4 <- ~edges +
    nodefactor("groups", base=1) 
  
  fit.i4 <- netest(nw.test,
                   formation = formation.i4,
                   target.stats = targets.test2,
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   set.control.ergm = control.i)

# 5. Simplified scenario with 4 risk groups with made up target stats, highest as reference, WITH offset for role class ----------------------------------------------------------

  # target stats
  edges <- st$stats.i[1]
  groups <- c(0, 10, 100, edges*2 - 110)
  targets.test3 <- c(edges, groups[-4])
  
  # Fit model
  formation.i5 <- ~edges +
    nodefactor("groups", base=4) +
    offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
  
  
  # Fit model
  fit.i5 <- netest(nw.test,
                   formation = formation.i5,
                   target.stats = targets.test3,
                   coef.form = c(-Inf, -Inf),
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   set.control.ergm = control.i)

# 6. Simplified scenario with edges and offset for role class only ----------------------------------------------------------
  
  # target stats
  edges <- st$stats.i[1]
  targets.test4 <-edges
  
  # Fit model
  formation.i6 <- ~edges +
    offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
  
  
  # Fit model
  fit.i6 <- netest(nw.test,
                   formation = formation.i6,
                   target.stats = targets.test4,
                   coef.form = c(-Inf, -Inf),
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   set.control.ergm = control.i)
  
  
  

# Save data from the three test scenarios
est.i.testriskg_fakegroups <- list(fit.i3, fit.i4, fit.i5, fit.i6)
save(est.i.testriskg_fakegroups, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.i.testriskg_fakegroups.bal.rda")



