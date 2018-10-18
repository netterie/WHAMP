# WHAMP network estimation for the main model#
# This file sequentially builds up the model to identify where fit issues arise #

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

# Initialize main and pers networks and assign degree -----------------------------------------------------------
nw.main <- base_nw_msm_whamp(st)
nw.main <- assign_degree_whamp(nw.main, deg.type = "pers", nwstats = st)

nw.pers <- nw.main
nw.pers <- assign_degree_whamp(nw.pers, deg.type = "main", nwstats = st)



# 1. Edges only -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.p1 <- ~edges +
                degrange(from = 3) +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 

stats.p1 <- c(st$stats.p[1], 0)

# Fit model
fit.p1 <- netest(nw.pers,
               formation = formation.p1,
               target.stats = stats.p1,
               coef.form = c(-Inf, -Inf),
               coef.diss = st$coef.diss.p,
               edapprox = TRUE,
               set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                               MCMC.samplesize = 7500,
                                               MCMC.burnin = 1e+6,
                                               MPLE.max.dyad.types = 1e+7,
                                               init.method = "zeros",
                                               MCMLE.maxit = 400,
                                               parallel = np/2, 
                                               parallel.type="PSOCK"))

# 2. Edges + nodefactor(race) -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class)

# Formulas
formation.p2 <- ~edges +
              nodefactor("race..wa", base=3) +
              degrange(from = 3) +
              offset(nodematch("role.class", diff = TRUE, keep = 1:2))

stats.p2 <- c(st$stats.p[c(1,3,4)], 0)

# Fit model
fit.p2 <- netest(nw.pers,
                 formation = formation.p2,
                 target.stats = stats.p2,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = st$coef.diss.p,
                 edapprox = TRUE,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/2,
                                                 parallel.type="PSOCK"))

# 3. Edges + nodefactor(race) + nodematch(race) -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class)

# Formulas
formation.p3 <- ~edges +
  nodefactor("race..wa", base=3) +
  nodematch("race..wa", diff=TRUE) +
  degrange(from = 3) +
  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

stats.p3 <- c(st$stats.p[c(1,3,4,8:10)], 0)

# Fit model
fit.p3 <- netest(nw.pers,
                 formation = formation.p3,
                 target.stats = stats.p3,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = st$coef.diss.p,
                 edapprox = TRUE,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/2,
                                                 parallel.type="PSOCK"))

# 4. Edges + nodefactor(race) + nodematch(race) + nodefactor(deg.main) -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class)

# Formulas
formation.p4 <- ~edges +
                  nodefactor("deg.main") +
                  nodefactor("race..wa", base=3) +
                  nodematch("race..wa", diff=TRUE) +
                  degrange(from = 3) +
                  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

stats.p4 <- c(st$stats.p[c(1:4,8:10)], 0)

# Fit model
fit.p4 <- netest(nw.pers,
                 formation = formation.p4,
                 target.stats = stats.p4,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = st$coef.diss.p,
                 edapprox = TRUE,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/2,
                                                 parallel.type="PSOCK"))


# 5. Edges + nodefactor(race) + nodematch(race) + nodefactor(deg.main) + nodefactor(region) -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class)

# Formulas
formation.p5 <- ~edges +
                  nodefactor("deg.main") +
                  nodefactor("race..wa", base=3) +
                  nodefactor("region", base=2) +
                  nodematch("race..wa", diff=TRUE) +
                  degrange(from = 3) +
                  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

stats.p5 <- c(st$stats.p[c(1:6,8:10)], 0)

# Fit model
fit.p5 <- netest(nw.pers,
                 formation = formation.p5,
                 target.stats = stats.p5,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = st$coef.diss.p,
                 edapprox = TRUE,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/2,
                                                 parallel.type="PSOCK"))

# 6. Edges + nodefactor(race) + nodematch(race) + nodefactor(deg.main) + nodefactor(region) + absdiff(sqrt.age)-----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class)

# Formulas
formation.p6 <- ~edges +
                  nodefactor("deg.main") +
                  nodefactor("race..wa", base=3) +
                  nodefactor("region", base=2) +
                  nodematch("race..wa", diff=TRUE) +
                  absdiff("sqrt.age") +
                  degrange(from = 3) +
                  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

stats.p6 <- c(st$stats.p[c(1:6,8:10,12)], 0)

# Fit model
fit.p6 <- netest(nw.pers,
                 formation = formation.p6,
                 target.stats = stats.p6,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = st$coef.diss.p,
                 edapprox = TRUE,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/2,
                                                 parallel.type="PSOCK"))


# 7. Edges + nodefactor(race) + nodematch(race) + nodefactor(deg.main) + nodefactor(region) + absdiff(sqrt.age) + concurrent-----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class)

# Formulas
formation.p7 <- ~edges +
                  nodefactor("deg.main") +
                  nodefactor("race..wa", base=3) +
                  nodefactor("region", base=2) +
                  concurrent +
                  nodematch("race..wa", diff=TRUE) +
                  absdiff("sqrt.age") +
                  degrange(from = 3) +
                  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

stats.p7 <- c(st$stats.p[c(1:10,12)], 0)

# Fit model
fit.p7 <- netest(nw.pers,
                 formation = formation.p7,
                 target.stats = stats.p7,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = st$coef.diss.p,
                 edapprox = TRUE,
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 init.method = "zeros",
                                                 MCMLE.maxit = 400,
                                                 parallel = np/2,
                                                 parallel.type="PSOCK"))

# 8. Edges + nodefactor(race) + nodematch(race) + nodefactor(deg.main) + nodefactor(region) + absdiff(sqrt.age) + concurrent + nodematch(region)-----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class)

# Formulas
formation.p8 <- ~edges +
                nodefactor("deg.main") +
                nodefactor("race..wa", base=3) +
                nodefactor("region", base=2) +
                concurrent +
                nodematch("race..wa", diff=TRUE) +
                nodematch("region", diff=FALSE) +
                absdiff("sqrt.age") +
                degrange(from = 3) +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2))

stats.p8 <- c(st$stats.p, 0)

# Fit model
fit.p8 <- netest(nw.pers,
                 formation = formation.p8,
                 target.stats = stats.p8,
                 coef.form = c(-Inf, -Inf),
                 coef.diss = st$coef.diss.p,
                 edapprox = TRUE,
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
  est.p.buildup.bal <- list(fit.p1, fit.p2, fit.p3, fit.p4, fit.p5, fit.p6, fit.p7, fit.p8)
  save(est.p.buildup.bal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.p.buildup.bal.rda")
} else {
  est.p.buildup.unbal <- list(fit.p1, fit.p2, fit.p3, fit.p4, fit.p5, fit.p6, fit.p7, fit.p8)
  save(est.p.buildup.unbal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.p.buildup.unbal.rda")
}

