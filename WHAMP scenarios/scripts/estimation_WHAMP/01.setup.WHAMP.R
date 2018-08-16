
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
  prop.race.region <- sumto1(c(0.0549, 0.0421, 0.4739, 0.0309, 0.0166, 0.2807, 0.0222, 0.0021, 0.0767))
  race.region <- apportion_lr(num, c("H.KC", "B.KC", "O.KC", "H.OW", "B.OW", "O.OW", "H.EW", "B.EW", "O.EW"),
                             prop.race.region)
  
  num.H.KC <- sum(race.region %in% "H.KC")
  num.B.KC <- sum(race.region %in% "B.KC")
  num.O.KC <- sum(race.region %in% "O.KC")
  num.H.OW <- sum(race.region %in% "H.OW")
  num.B.OW <- sum(race.region %in% "B.OW")
  num.O.OW <- sum(race.region %in% "O.OW")
  num.H.EW <- sum(race.region %in% "H.EW")
  num.B.EW <- sum(race.region %in% "B.EW")
  num.O.EW <- sum(race.region %in% "O.EW")
  
# Age structure (proportion in each age group 18-24, 25-29... 55-59)
agestr <- sumto1(c(0.1594, 0.1319, 0.1292, 0.1173, 0.1183, 0.1148, 0.1071, 0.122))
    
# mean/pers degree distributions matrices.
deg.mp <- matrix(sumto1(c(0.4073, 0.0766, 0.0675, 
                          0.3472, 0.0473, 0.0541)), byrow = TRUE, nrow = 2)
deg.mp.H <- matrix(sumto1(c(0.37, 0.0804, 0.0377, 
                            0.4134, 0.0551, 0.0434)), byrow = TRUE, nrow = 2)
deg.mp.B <- matrix(sumto1(c(0.5413, 0.048, 0.0961, 
                            0.2171, 0.0225, 0.0751)), byrow = TRUE, nrow = 2)
deg.mp.O <- matrix(sumto1(c(0.4027, 0.0781, 0.0692, 
                            0.3481, 0.048, 0.0539)), byrow = TRUE, nrow = 2)
deg.mp.KC <- matrix(sumto1(c(0.3588, 0.0832, 0.0739, 
                             0.3798, 0.0462, 0.058)), byrow = TRUE, nrow = 2)
deg.mp.OW <- matrix(sumto1(c(0.4902, 0.0691, 0.0511, 
                             0.2853, 0.0518, 0.0526)), byrow = TRUE, nrow = 2)
deg.mp.EW <- matrix(sumto1(c(0.4271, 0.0619, 0.0825, 
                             0.3547, 0.0389, 0.0349)), byrow = TRUE, nrow = 2)
deg.mp.40to49 <- matrix(sumto1(c(0.3057, 0.1098, 0.0984, 
                                 0.3275, 0.0576, 0.1011)), byrow = TRUE, nrow = 2)
deg.mp.otherages <- matrix(sumto1(c(0.4401, 0.0659, 0.0575, 
                                    0.3536, 0.0439, 0.0389)), byrow = TRUE, nrow = 2)

# Instant rates (Mean rate of one-off partnerships (degree) per day by momentary degree 
  # (order: 0 main 0 pers, 0 main 1 pers, 0 main 2+ pers, 1 main 0 pers, 1 main 1 pers, 1 main 2+ pers)
mdeg.inst <- matrix(c(0.015843, 0.006068, 0.021088, 
                             0.00708, 0.009796, 0.032999), byrow = TRUE, nrow = 2) 

# Quintile distribution of overall AI rates
qnts.18to49 <- c(0, 0.000508, 0.004995, 0.05502)
qnts.50to59 <- c(0, 0, 0.001387, 0.021098)

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
  pers.by.age = 0,
  time.unit = time.unit,
  num.B = num.B,
  num.W = num.W,
  num.H.KC = num.H.KC,
  num.B.KC = num.B.KC,
  num.O.KC = num.O.KC,
  num.H.OW = num.H.OW,
  num.B.OW = num.B.OW,
  num.O.OW = num.O.OW,
  num.H.EW = num.H.EW,
  num.B.EW = num.B.EW,
  num.O.EW = num.O.EW,
  agestr = agestr,
  deg.mp = deg.mp,
  deg.mp.H = deg.mp.H,
  deg.mp.B = deg.mp.B,
  deg.mp.O = deg.mp.O,
  deg.mp.KC = deg.mp.KC,
  deg.mp.OW = deg.mp.OW,
  deg.mp.EW = deg.mp.EW,
  deg.mp.40to49 = deg.mp.40to49,
  deg.mp.otherages = deg.mp.otherages,
  mdeg.inst = mdeg.inst,
  qnts.18to49 = qnts.18to49,
  qnts.50to59 = qnts.50to59,
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

save(st, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/est/nwstats.whamp.rda")
rm(list = ls())
