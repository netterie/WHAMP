
## WHAMP setup file ##

rm(list = ls())
suppressMessages(library("EpiModelHIV"))


# Time unit for simulation, relative to 1 day
time.unit <- 7

#Network size
num <- 10000

# Population size by race and region
    ##--ORIGINAL CODE - DELETE WHEN FINISH DE-BUGGING
    num.B <- 5000
    num.W <- 5000
  ## Vector with the proportion of Hispanic, black, and other race/ethnicity men in King County, other western WA, and eastern WA
  prop.race.region <- c(0.0549, 0.0421, 0.4739, 0.0309, 0.0166, 0.2807, 0.0222, 0.0021)
  prop.race.region[9] <- 1 - sum(prop.race.region[1:8]) # Set last proportion to avoid rounding errors
  race.region <- apportion_lr(num, c("H.KC", "B.KC", "O.KC", "H.OW", "B.OW", "O.OW", "H.EW", "B.EW", "O.EW"),
                             prop.race.region)
  
  num.H..wa <- sum(race.region %in% c("H.KC", "H.OW", "H.EW"))
  num.B..wa <- sum(race.region %in% c("B.KC", "B.OW", "B.EW"))
  num.O..wa <- sum(race.region %in% c("O.KC", "O.OW", "O.EW"))
  
  num.KC <- sum(race.region %in% c("H.KC", "B.KC", "O.KC"))
  num.OW <- sum(race.region %in% c("H.OW", "B.OW", "O.OW"))
  num.EW <- sum(race.region %in% c("H.EW", "B.EW", "O.EW"))
  
# Age structure (proportion in each age group 18-24, 25-29... 55-59)
agestr <- c(0.1594, 0.1319, 0.1292, 0.1173, 0.1183, 0.1148, 0.1071, 0.122)
    
# mean/pers degree distributions matrices.
deg.mp.B <- deg.mp.W <-
  (matrix(c(0.506, 0.151, 0.053,
            0.207, 0.061, 0.022), byrow = TRUE, nrow = 2) +
   matrix(c(0.435, 0.184, 0.095,
            0.233, 0.033, 0.020), byrow = TRUE, nrow = 2))/2

# Instant rates
mdeg.inst.B <- mdeg.inst.W <-
  (matrix(c(0.010402, 0.012954, 0.011485,
            0.007912, 0.007424, 0.007424), byrow = TRUE, nrow = 2) +
   matrix(c(0.008186, 0.012017, 0.013024,
            0.008151, 0.008341, 0.008341), byrow = TRUE, nrow = 2))/2

# Quintile distribution of overall AI rates
qnts.W <- qnts.B <- c(0.0000,
                      0.0010,
                      0.0054,
                      0.0102,
                      0.0315)

# Proportion in same-race partnerships (main, casl, inst)
prop.hom.mpi.B <- prop.hom.mpi.W <- (c(0.9484, 0.9019, 0.9085) +
                                     c(0.9154, 0.8509, 0.8944))/2

# Mean age diffs (main, casl, inst)
sqrt.adiff.BB <- c(0.417, 0.498, 0.456)
sqrt.adiff.BW <- c(0.454, 0.629, 0.585)
sqrt.adiff.WW <- c(0.520, 0.632, 0.590)

# Mean durations
rates.main <- mean(c(0.002876,
                     0.002692,
                     0.001803))
rates.pers <- mean(c(0.007617,
                     0.003501,
                     0.006931))

durs.main <- 1/rates.main
durs.pers <- 1/rates.pers

# Age-sex-specific mortality rates
ages <- 18:59

asmr.B <- c(rep(0, 17),
            1-(1-c(rep(0.00159, 7),
                   rep(0.00225, 10),
                   rep(0.00348, 25)))^(1/(365/time.unit)), 1) #-- To make old code work with new age str, I added 20 to the last deaths vector

asmr.W <- c(rep(0, 17),
            1-(1-c(rep(0.00103, 7),
                   rep(0.00133, 10),
                   rep(0.00214, 25)))^(1/(365/time.unit)), 1) #-- To make old code work with new age str, I added 20 to the last deaths vector

asmr.H..wa <- c(rep(0, 17),
            1-(1-c(rep(0.00056, 2),
                   rep(0.00108, 5),
                   rep(0.00109, 5),
                   rep(0.00115, 5),
                   rep(0.00132, 5),
                   rep(0.00175, 5),
                   rep(0.00263, 5),
                   rep(0.00430, 5),
                   rep(0.00651, 5)))^(1/(365/time.unit)), 1)

asmr.B..wa <- c(rep(0, 17),
            1-(1-c(rep(0.00110, 2),
                   rep(0.00203, 5),
                   rep(0.00230, 5),
                   rep(0.00258, 5),
                   rep(0.00310, 5),
                   rep(0.00380, 5),
                   rep(0.00539, 5),
                   rep(0.00862, 5),
                   rep(0.01348, 5)))^(1/(365/time.unit)), 1)

asmr.O..wa <- c(rep(0, 17),
            1-(1-c(rep(6e-04, 2),
                   rep(0.00119, 5),
                   rep(0.00146, 5),
                   rep(0.00171, 5),
                   rep(0.00198, 5),
                   rep(0.00252, 5),
                   rep(0.00371, 5),
                   rep(0.00597, 5),
                   rep(0.00885, 5)))^(1/(365/time.unit)), 1)

# I, R, V role frequencies
role.B.prob <- role.W.prob <- (c(0.242, 0.321, 0.437) +
                               c(0.228, 0.228, 0.544))/2


# Create meanstats
st <- calc_nwstats_msm_whamp(
  method = 1,
  time.unit = time.unit,
  num.B = num.B,
  num.W = num.W,
  num.H..wa <- num.H..wa,
  num.B..wa <- num.B..wa,
  num.O..wa <- num.O..wa,
  num.KC <- num.KC,
  num.OW <- num.OW,
  num.EW <- num.EW,
  agestr <- agestr,
  deg.mp.B = deg.mp.B,
  deg.mp.W = deg.mp.W,
  mdeg.inst.B = mdeg.inst.B,
  mdeg.inst.W = mdeg.inst.W,
  qnts.B = qnts.B,
  qnts.W = qnts.W,
  prop.hom.mpi.B = prop.hom.mpi.B,
  prop.hom.mpi.W = prop.hom.mpi.W,
  balance = "mean",
  sqrt.adiff.BB = sqrt.adiff.BB,
  sqrt.adiff.WW = sqrt.adiff.WW,
  sqrt.adiff.BW = sqrt.adiff.BW,
  diss.main = ~offset(edges),
  diss.pers = ~offset(edges),
  durs.main = durs.main,
  durs.pers = durs.pers,
  ages = ages,
  asmr.B <- asmr.B,
  asmr.W <- asmr.W,
  asmr.H..wa = asmr.H..wa,
  asmr.B..wa = asmr.B..wa,
  asmr.O..wa = asmr.O..wa,
  role.B.prob = role.B.prob,
  role.W.prob = role.W.prob)

save(st, file = "WHAMP scenarios/est/nwstats.whamp.rda")
rm(list = ls())
