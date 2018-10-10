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
load(file = "/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/est/nwstats.balanced.whamp.rda")
  } else {
load(file = "/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/est/nwstats.unbalanced.whamp.rda")
  }

# Initialize main network and assign degree -----------------------------------------------------------
nw.main <- base_nw_msm_whamp(st)

nw.main <- assign_degree_whamp(nw.main, deg.type = "pers", nwstats = st)

# 1. Edges only -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m1 <- ~edges +
                degrange(from = 2) +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 

stats.m1 <- c(st$stats.m[1], 0)

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
                                                parallel = np/2, 
                                                parallel.type="PSOCK"))


# 2. Edges + nodefactor(race) -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m2 <- ~edges +
              nodefactor("race..wa", base=3) + 
              degrange(from = 2) + 
              offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 

stats.m2 <- c(st$stats.m[c(1,4,5)], 0)

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
                                                 parallel = np/2, 
                                                 parallel.type="PSOCK"))

# 3. Edges + nodefactor(race) + nodefactor(region) -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m3 <- ~edges +
                nodefactor("race..wa", base=3) + 
                nodefactor("region", base=2) +
                degrange(from = 2) +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 

stats.m3 <- c(st$stats.m[c(1,4,5,6,7)], 0)

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
                                                 parallel = np/2, 
                                                 parallel.type="PSOCK"))

# 4. Edges + nodefactor(race) + nodefactor(region) + nodefactor(deg.pers) -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m4 <- ~edges +
                nodefactor("deg.pers") +
                nodefactor("race..wa", base=3) + 
                nodefactor("region", base=2) +
                degrange(from = 2) +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 


stats.m4 <- c(st$stats.m[c(1:7)], 0)

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
                                                 parallel = np/2, 
                                                 parallel.type="PSOCK"))


# 5. Edges + nodefactor(race) + nodefactor(region) + nodefactor(deg.pers) + nodematch(race) -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m5 <- ~edges +
                nodefactor("deg.pers") +
                nodefactor("race..wa", base=3) + 
                nodefactor("region", base=2) +
                nodematch("race..wa", diff=TRUE) +
                degrange(from = 2) +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 
                

stats.m5 <- c(st$stats.m[c(1:10)], 0)

# Fit model
fit.m5 <- netest(nw.main,
                 formation = formation.m5,
                 target.stats = stats.m5,
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

# 6. Edges + nodefactor(race) + nodefactor(region) + nodefactor(deg.pers) + nodematch(race) + absdiff(sqrtage) -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m6 <- ~edges +
                nodefactor("deg.pers") +
                nodefactor("race..wa", base=3) + 
                nodefactor("region", base=2) +
                nodematch("race..wa", diff=TRUE) +
                absdiff("sqrt.age") +
                degrange(from = 2) +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 


stats.m6 <- c(st$stats.m, 0)

# Fit model
fit.m6 <- netest(nw.main,
                 formation = formation.m6,
                 target.stats = stats.m6,
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

#7. Full model: Edges + nodefactor(race) + nodefactor(region) + nodefactor(deg.pers) + nodematch(race) + absdiff(sqrtage) + offset for region -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m7 <- ~edges +
                nodefactor("deg.pers") +
                nodefactor("race..wa", base=3) + 
                nodefactor("region", base=2) +
                nodematch("race..wa", diff=TRUE) +
                absdiff("sqrt.age") +
                degrange(from = 2) + 
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) +
                offset(nodemix("region", base = c(1,3,6)))

# Add target stat for degrange
stats.m7 = c(st$stats.m, 0)

# Fit model
fit.m7 <- netest(nw.main,
                formation = formation.m7,
                target.stats = stats.m7,
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

#8. Alt model: 90% regional assortativity -----------------------------------------------------------
# (+ constraint for max one ongoing and offset for role class) 

# Formulas
formation.m8 <- ~edges +
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
stats.m8 = c(st$stats.m, regionmatch, 0)

# Fit model
fit.m8 <- netest(nw.main,
                 formation = formation.m8,
                 target.stats = stats.m8,
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
  est.m.buildup.bal <- list(fit.m1, fit.m2, fit.m3, fit.m4, fit.m5, fit.m6, fit.m7, fit.m8)
  save(est.m.buildup.bal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/est/fit.m.buildup.bal.rda")
} else {
  est.m.buildup.unbal <- list(fit.m1, fit.m2, fit.m3, fit.m4, fit.m5, fit.m6, fit.m7, fit.m8)
  save(est.m.buildup.unbal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/est/fit.m.buildup.unbal.rda")
}

