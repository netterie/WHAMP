# Full main network with bdmaxout instead of degrange

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

# Fit full model with 100% regional assortativity: Edges + nodefactor(race) + nodefactor(region) + nodefactor(deg.pers) + nodematch(race) + absdiff(sqrtage) + offset for region -----------------------------------------------------------
  # (+ constraint for max one ongoing and offset for role class) 
  
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
stats.m = c(st$stats.m)

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

save(fit.m, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/main_bal_bdmaxout.rda")

load(file="/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/main_bal_bdmaxout.rda")

# MCMC diagnostics ----
mcmc.diagnostics(fit.m$fit)

# Summary of model fit ----
summary(fit.m)

# Network diagnostics ----
(dx_main <- netdx(fit.m, nsims = 10, nsteps = 1000, ncores = 4, set.control.stergm = control.simulate.network(MCMC.burnin.min = 1e+5, MCMC.burnin.max = 1e+5)))
plot(dx_main, type="formation")
plot(dx_main, type="duration")
plot(dx_main, type="dissolution")

