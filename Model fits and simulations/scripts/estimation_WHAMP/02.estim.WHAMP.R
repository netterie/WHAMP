# WHAMP network estimation

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

# 1. Main Model -----------------------------------------------------------

# Initialize network
nw.main <- base_nw_msm_whamp(st)

# Assign degree
nw.main <- assign_degree_whamp(nw.main, deg.type = "pers", nwstats = st)

# Formulas
formation.m <- ~edges +
                nodefactor("deg.pers") +
                nodefactor("race..wa", base=3) + 
                nodefactor("region", base=2) +
                nodematch("race..wa", diff=TRUE) +
                absdiff("sqrt.age") +
                degrange(from = 2) + 
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) +
                offset(nodemix("region", base = c(1,3,6)))

# Add target stat for degrange
stats.m = c(st$stats.m, 0)

# Start clock to calculate runtime
startclock_main <- proc.time()

# Fit model
fit.m <- netest(nw.main,
                formation = formation.m,
                target.stats = stats.m,
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


# Fit time
runtime_min_fit_main <- (proc.time()-startclock_main)['elapsed']/60


# 2. Persistent Model ---------------------------------------------------------

# Initialize network
nw.pers <- nw.main

# Assign degree
nw.pers <- assign_degree_whamp(nw.pers, deg.type = "main", nwstats = st)

# Formulas
formation.p <- ~edges +
                nodefactor("deg.main") +
                nodefactor("race..wa", base=3) + 
                nodefactor("region", base=2) +
                concurrent +
                nodematch("race..wa", diff=TRUE) +
                nodematch("region", diff=FALSE) +
                absdiff("sqrt.age") +
                degrange(from=3) +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2))

# Add target stat for degrange
stats.p = c(st$stats.p, 0)

# Start clock to calculate runtime
startclock_pers <- proc.time()

# Fit model
fit.p <- netest(nw.pers,
                formation = formation.p,
                target.stats = stats.p,
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

# Fit time
runtime_min_fit_pers <- (proc.time()-startclock_pers)['elapsed']/60


# Fit inst model ----------------------------------------------------------

# Initialize network
nw.inst <- nw.main

# Assign degree
nw.inst <- set.vertex.attribute(nw.inst, "deg.main", nw.pers %v% "deg.main")
nw.inst <- set.vertex.attribute(nw.inst, "deg.pers", nw.main %v% "deg.pers")
table(nw.inst %v% "deg.main", nw.inst %v% "deg.pers")

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
                set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                MCMC.samplesize = 7500,
                                                MCMC.burnin = 1e+6,
                                                MPLE.max.dyad.types = 1e+7,
                                                MCMLE.maxit = 400,
                                                parallel = np/2, 
                                                parallel.type="PSOCK"))

# Fit time
runtime_min_fit_inst <- (proc.time()-startclock_inst)['elapsed']/60


# Save data ---------------------------------------------------------------
if (balanced == 1) {
  est.bal <- list(fit.m, fit.p, fit.i, runtime_min_fit_main, runtime_min_fit_pers, runtime_min_fit_inst)
  save(est.bal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/est/fit.bal.whamp.rda")
} else {
  est.unbal <- list(fit.m, fit.p, fit.i, runtime_min_fit_main, runtime_min_fit_pers, runtime_min_fit_inst)
  save(est.unbal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/est/fit.unbal.whamp.rda")
}

