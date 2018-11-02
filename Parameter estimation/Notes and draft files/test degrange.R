nw <- network.initialize(n = 100, directed = FALSE)

# Estimation
formation <- ~edges + degrange(from = 2)
target.stats <- c(45, 0)
coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 4)
est <- netest(nw, formation, target.stats, coef.diss, verbose = FALSE)

# Diagnostics
dx <- netdx(est, nsims = 10, nsteps = 1000, ncores = 4, nwstats.formula = ~edges + degree(2) + degree(3) + degrange(2))
plot(dx, type = "formation")

# Simulation
param <- param.net(inf.prob = 0.2)
init <- init.net(i.num = 5)
control <- control.net(type = "SI", nsteps = 25, nsims = 1, verbose = FALSE)
sim <- netsim(est, param, init, control)

plot(sim, type = "network", col.status = TRUE, at = 5)