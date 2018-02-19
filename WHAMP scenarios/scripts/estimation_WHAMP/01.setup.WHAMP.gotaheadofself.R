## WHAMP setup file ##

rm(list = ls())
suppressMessages(library("EpiModelHIV"))

# Time unit for simulation, relative to 1 day
time.unit <- 7

#Network size
num <- 10000

# Population size by race and region
num.H.KC <- num * 0.0549
num.B.KC <- num * 0.0421
num.O.KC <- num * 0.4739
num.H.OW <- num * 0.0309
num.B.OW <- num * 0.0166
num.O.OW <- num * 0.2807
num.H.EW <- num * 0.0222
num.B.EW <- num * 0.0021
num.O.EW <- num - sum(num.H.KC, num.B.KC, num.O.KC, num.H.OW, num.B.OW, num.O.OW, num.H.EW, num.B.EW) #set value for last group to remainder to avoid rounding error

num.H..wa <- num.H.KC + num.H.OW + num.H.EW
num.B..wa <- num.B.KC + num.B.OW + num.B.EW
num.O..wa <- num.O.KC + num.O.OW + num.O.EW

num.KC <- num.H.KC + num.B.KC + num.O.KC
num.OW <- num.H.OW + num.B.OW + num.O.OW
num.EW <- num.H.EW + num.B.EW + num.E.EW

  #ORIGINAL CODE FOR BLACK/WHITE RACE - DELETE WHEN FINISH DE-BUGGING
  num.B <- .5*num
  num.W <- .5*num
  
# mean/pers degree distributions matrices
deg.mp <- (matrix(c(0.4073, 0.0766, 0.0675,
              0.3472, 0.0473, 0.0541), byrow = TRUE, nrow = 2))

# Instant rates by main/pers degree
mdeg.inst <- (matrix(c(0.015843, 0.013102, 0.013102,
                       0.007080, 0.022174, 0.022174), byrow = TRUE, nrow = 2))

# Quantile distribution of overall AI rates - means within quantiles
qnts.18to49 <- c(0.000000, 0.000508, 0.004995, 0.05502)
qnts.50to59 <- c(0.000000, 0.000000, 0.001387, 0.021098)

# Proportion in same-race partnerships (main, casl, inst)
    # prop.hom.mpi.B <- prop.hom.mpi.W <- (c(0.9484, 0.9019, 0.9085) +
    #                                          c(0.9154, 0.8509, 0.8944))/2

# Mean age diffs (main, casl, inst)
# sqrt.adiff.BB <- c(0.417, 0.498, 0.456)
# sqrt.adiff.BW <- c(0.454, 0.629, 0.585)
# sqrt.adiff.WW <- c(0.520, 0.632, 0.590)

# Mean durations
durs.main <- 1020
durs.pers <- 192
  
rates.main <- 1/durs.main
rates.pers <- 1/durs.pers

# Age-sex-specific mortality rates
ages <- 18:59
asmr.H <- c(rep(0, 17),
            1-(1-c(rep(0.00056, 2),
                   rep(0.00108, 5),
                   rep(0.00109, 5),
                   rep(0.00115, 5),
                   rep(0.00132, 5),
                   rep(0.00175, 5),
                   rep(0.00263, 5),
                   rep(0.00430, 5),
                   rep(0.00651, 5)))^(1/(365/time.unit)), 1)

asmr.B <- c(rep(0, 17),
            1-(1-c(rep(0.00110, 2),
                   rep(0.00203, 5),
                   rep(0.00230, 5),
                   rep(0.00258, 5),
                   rep(0.00310, 5),
                   rep(0.00380, 5),
                   rep(0.00539, 5),
                   rep(0.00862, 5),
                   rep(0.01348, 5)))^(1/(365/time.unit)), 1)

asmr.O <- c(rep(0, 17),
            1-(1-c(rep(0.00060, 2),
                   rep(0.00119, 5),
                   rep(0.00146, 5),
                   rep(0.00171, 5),
                   rep(0.00198, 5),
                   rep(0.00252, 5),
                   rep(0.00371, 5),
                   rep(0.00597, 5),
                   rep(0.00885, 5)))^(1/(365/time.unit)), 1)

# # I, R, V role frequencies
# role.B.prob <- role.W.prob <- (c(0.242, 0.321, 0.437) +
#                                    c(0.228, 0.228, 0.544))/2
# 

# Create meanstats
st <- calc_nwstats_msm_whamp(
    time.unit = time.unit,
    num.H..wa <- num.H..wa,
    num.B..wa <- num.B..wa,
    num.O..wa <- num.O..wa,
    num.KC <- num.KC,
    num.OW <- num.OW,
    num.EW <- num.EW,
    deg.mp = deg.mp,
    mdeg.inst = mdeg.inst,
    qnts.18to49 = qnts.18to49,
    qnts.50to59 = qnts.50to59,
    diss.main = ~offset(edges),
    diss.pers = ~offset(edges),
    durs.main = durs.main,
    durs.pers = durs.pers,
    ages = ages,
    asmr.H = asmr.H,
    asmr.B = asmr.B,
    asmr.O = asmr.O)

save(st, file = "WHAMP scenarios/est/nwstats.whamp.rda")
rm(list = ls())
