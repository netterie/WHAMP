# WHAMP network estimation for the instantaneous model#
# This file tests fitting a model with only edges and risk groups.

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

# Define atlernate target stats for risk groups where keep lowest quantiles at 0
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
riskg.alt <- num.riskg[-8]
riskg.bump <- num.riskg.bump[-8]


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

# 1. Edges + riskg with lowest groups as 0 ----------------------------------------------------------
#    (+ offset for role class)

# Formulas
formation.i1 <- ~edges +
                nodefactor("riskg", base=8) +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) 

stats.i1 <- c(st$stats.i[1], riskg.alt)

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

# 2. Edges + riskg with lowest groups as 0.0001 ----------------------------------------------------------
#    (+ offset for role class)

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
                 set.control.ergm = control.ergm(MCMC.interval = 1e+5,
                                                 MCMC.samplesize = 7500,
                                                 MCMC.burnin = 1e+6,
                                                 MPLE.max.dyad.types = 1e+7,
                                                 MCMLE.maxit = 400,
                                                 parallel = np/2, 
                                                 parallel.type="PSOCK"))


# Save data ---------------------------------------------------------------
if (balanced == 1) {
    est.i.testriskg.bal <- list(fit.i1, fit.i2)
    save(est.i.testriskg.bal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.i.testriskg.bal.rda")
} else {
    est.i.testriskg.unbal <- list(fit.i1, fit.i2)
    save(est.i.testriskg.unbal, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/Model fits and simulations/Fit tests and debugging/est/fit.i.testriskg.unbal.rda")
}
