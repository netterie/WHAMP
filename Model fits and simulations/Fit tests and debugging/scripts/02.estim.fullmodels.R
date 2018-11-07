# WHAMP network estimation - full models#

# This file fits the full research versions of each of our three network models #

#############
## SETUP ----
#############

rm(list = ls())
suppressMessages(library("EpiModelHIV"))
library("parallel")
library("here")
np <- detectCores()

# Indicate whether using the balanced or unbalanced target stats
balanced = 1

# Load the appropriate file (balanced or unbalanced)
if (balanced == 1) {
  load(file = here("Model fits and simulations/est/nwstats.balanced.whamp.rda"))
} else {
  load(file = here("Model fits and simulations/est/nwstats.unbalanced.whamp.rda"))
}

## Initialize networks and assign degree 
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

#############
## MAIN, using bdmaxout ----
#############

# Formulas
formation.m <- ~edges +
                nodefactor("deg.pers") +
                nodefactor("race..wa", base=3) + 
                nodefactor("region", base=2) +
                nodematch("race..wa", diff=TRUE) +
                absdiff("sqrt.age") +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) +
                offset(nodemix("region", base = c(1,3,6)))

# Add target stat for degrange
stats.m = st$stats.m

# Fit model
fit.m <- netest(nw.main,
                 formation = formation.m,
                 target.stats = stats.m,
                 coef.form = c(-Inf, -Inf, -Inf, -Inf, -Inf),
                 coef.diss = st$coef.diss.m,
                 constraints = ~bd(maxout = 1),
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/4, 
                                                 parallel.type="PSOCK"))
if (balanced == 1) {
  save(est.m.bal.bdmaxout, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/est.m.bal.bdmaxout.rda")
} else {
  save(est.m.unbal.bdmaxout, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/est.m.unbal.bdmaxout.rda")
}

#############
## PERSISTENT, using bdmaxout ----
#############

# Formulas
formation.p <- ~edges +
                nodefactor("deg.main") +
                nodefactor("race..wa", base=3) +
                nodefactor("region", base=2) +
                concurrent +
                nodematch("race..wa", diff=TRUE) +
                nodematch("region", diff=FALSE) +
                absdiff("sqrt.age") +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2))

stats.p <- st$stats.p

# Fit model
fit.p <- netest(nw.pers,
                 formation = formation.p,
                 target.stats = stats.p,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = st$coef.diss.p,
                 constraints = ~bd(maxout = 2),
                 edapprox = TRUE,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/2,
                                                 parallel.type="PSOCK"))

if (balanced == 1) {
  save(est.p.bal.bdmaxout, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/est.p.bal.bdmaxout.rda")
} else {
  save(est.p.unbal.bdmaxout, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/est.p.unbal.bdmaxout.rda")
}


#############
## INSTANTANEOUS ----
#############

# Formulas
formation.i <- ~edges +
                nodefactor(c("deg.main", "deg.pers")) +
                nodefactor("riskg", base=8) +
                nodefactor("race..wa", base=3) + 
                nodefactor("region", base=2) +
                nodematch("race..wa", diff=TRUE) +
                nodematch("region", diff=FALSE) +
                absdiff("sqrt.age") +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 

stats.i <- st$stats.i

# Fit model
fit.i <- netest(nw.inst,
                 formation = formation.i,
                 target.stats = stats.i,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = dissolution_coefs(~offset(edges), 1),
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 MCMLE.maxit = 400,
                                                 parallel = np/2, 
                                                 parallel.type="PSOCK"))

if (balanced == 1) {
  save(est.i.bal.bdmaxout, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/est.i.bal.bdmaxout.rda")
} else {
  save(est.i.unbal.bdmaxout, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/est.i.unbal.bdmaxout.rda")
}

