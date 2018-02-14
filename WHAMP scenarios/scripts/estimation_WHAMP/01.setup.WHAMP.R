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

# mean/pers degree distributions matrices.
deg.mp <- (matrix(c(0.4073, 0.0766, 0.0675,
              0.3472, 0.0473, 0.0541), byrow = TRUE, nrow = 2))

# Instant rates by mean/pers degree
mdeg.inst <- (matrix(c(0.015843, 0.013102, 0.013102,
                       0.007080, 0.022174, 0.022174), byrow = TRUE, nrow = 2))

# # Quintile distribution of overall AI rates
# qnts.W <- qnts.B <- c(0.0000,
#                       0.0010,
#                       0.0054,
#                       0.0102,
#                       0.0315)
# 
# # Proportion in same-race partnerships (main, casl, inst)
# prop.hom.mpi.B <- prop.hom.mpi.W <- (c(0.9484, 0.9019, 0.9085) +
#                                          c(0.9154, 0.8509, 0.8944))/2
# 
# # Mean age diffs (main, casl, inst)
# sqrt.adiff.BB <- c(0.417, 0.498, 0.456)
# sqrt.adiff.BW <- c(0.454, 0.629, 0.585)
# sqrt.adiff.WW <- c(0.520, 0.632, 0.590)

# Mean durations
durs.main <- 1020
durs.pers <- 192
  
rates.main <- 1/durs.main
rates.pers <- 1/durs.pers

# Age-sex-specific mortality rates - FOR NOW SET TO ASMRS FROM STIPREP FILE, BUT EXTEND LAST CAT OUT ANOTHER 20 YRS
ages <- 18:59
asmr.H <- c(rep(0, 17),
            1-(1-c(rep(0.00103, 7),
                   rep(0.00133, 10),
                   rep(0.00214, 25)))^(1/(365/time.unit)), 1)

asmr.B <- c(rep(0, 17),
            1-(1-c(rep(0.00103, 7),
                   rep(0.00133, 10),
                   rep(0.00214, 25)))^(1/(365/time.unit)), 1)

asmr.O <- c(rep(0, 17),
            1-(1-c(rep(0.00103, 7),
                   rep(0.00133, 10),
                   rep(0.00214, 25)))^(1/(365/time.unit)), 1)

# # I, R, V role frequencies
# role.B.prob <- role.W.prob <- (c(0.242, 0.321, 0.437) +
#                                    c(0.228, 0.228, 0.544))/2
# 

# Create meanstats
st <- calc_nwstats_msm_whamp(
    time.unit = time.unit,
    num.H.KC = num.H.KC,
    num.B.KC = num.B.KC,
    num.O.KC = num.O.KC,
    num.H.OW = num.H.OW,
    num.B.OW = num.B.OW,
    num.O.OW = num.O.OW,
    num.H.EW = num.H.EW,
    num.B.EW = num.B.EW,
    num.O.EW = num.O.EW,
    deg.mp = deg.mp,
    mdeg.inst = mdeg.inst,
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
