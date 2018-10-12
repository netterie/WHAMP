
## WHAMP setup file ##
# This file uses balanced racial/ethnic mixing parameters and degree distributions adjusted to account for the balancing 

rm(list = ls())
suppressMessages(library("EpiModelHIV"))


# Time unit for simulation, relative to 1 day
time.unit <- 7

#Network size
num <- 10000

# Population size by race and region
    ##--ORIGINAL CODE - DELETE WHEN FINISH DE-BUGGING
    num.B <- num*0.5
    num.W <- num*0.5
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
deg.mp <- matrix(sumto1(c(0.3947, 0.0792, 0.078, 
                          0.3402, 0.0474, 0.0605)), byrow = TRUE, nrow = 2)
deg.mp.H <- matrix(sumto1(c(0.3576, 0.082, 0.065, 
                            0.3954, 0.0518, 0.0481)), byrow = TRUE, nrow = 2)
deg.mp.B <- matrix(sumto1(c(0.4963, 0.059, 0.1026, 
                            0.2556, 0.0241, 0.0624)), byrow = TRUE, nrow = 2)
deg.mp.O <- matrix(sumto1(c(0.3921, 0.0804, 0.0779, 
                            0.3393, 0.0485, 0.0619)), byrow = TRUE, nrow = 2)
deg.m.region <- sumto1(c(0.0994, 0.6153, 0.2852)) # EW, KC, OW
deg.p.region <- sumto1(c(0.0911, 0.6159, 0.293)) # EW, KC, OW

# Instantaneous partner rates (Mean rate (degree) of inst partnerships per day by momentary main/pers degree)
  # (order: 0 main 0 pers, 0 main 1 pers, 0 main 2+ pers, 1 main 0 pers, 1 main 1 pers, 1 main 2+ pers)
mdeg.inst <- matrix(c(0.015521, 0.006713, 0.024792, 
                      0.007272, 0.011021, 0.034444), byrow = TRUE, nrow = 2) 

# Quintile distribution of overall instantaneous AI rates
qnts.18to49 <- c(0.0001, 0.000608, 0.005247, 0.056484) #set to 0.0001 if 0 to give non-zero prob
qnts.50to59 <- c(0.0001, 0.0001, 0.00171, 0.027315)  #set to 0.0001 if 0 to give non-zero prob

# Mean rate of instantaneous partnerships by race (black, Hispanic, other)
inst.bho <- c(0.017666, 0.013338, 0.013424)

# Distribution of instantaneous partnerships by region (EW, KC, OW)
inst.region <- sumto1(c(0.0871, 0.6598, 0.253))

# Proportion in same-race partnerships (main, pers, inst)
prop.hom.mpi.H <- c(0.461, 0.2633, 0.2633)
prop.hom.mpi.B <- c(0.2998, 0.0675, 0.0675)
prop.hom.mpi.O <- c(0.877, 0.7346, 0.7346)

# Mean age diffs (main, pers, inst)
sqrt.adiff.mpi <- c(0.5384, 0.8252, 0.7941)

# Region mixing (main, pers, inst)
prop.hom.region.mpi <- c(1.0, 0.8, 0.8)

# Mean durations
durs.main <- 1071
durs.pers <- 221

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
            1-(1-c(rep(0.00089, 2),
                   rep(0.00107, 5),
                   rep(0.00112, 5),
                   rep(0.00123, 5),
                   rep(0.00126, 5),
                   rep(0.00186, 5),
                   rep(0.00207, 5),
                   rep(0.00343, 5),
                   rep(0.00495, 5)))^(1/(365/time.unit)), 1)

asmr.B..wa <- c(rep(0, 17),
            1-(1-c(rep(0.00168, 2),
                   rep(0.00117, 5),
                   rep(0.00161, 5),
                   rep(0.00169, 5),
                   rep(0.00267, 5),
                   rep(0.00224, 5),
                   rep(0.00428, 5),
                   rep(0.00619, 5),
                   rep(0.01311, 5)))^(1/(365/time.unit)), 1)

asmr.O..wa <- c(rep(0, 17),
            1-(1-c(rep(0.00068, 2),
                   rep(0.00101, 5),
                   rep(0.00119, 5),
                   rep(0.00138, 5),
                   rep(0.00161, 5),
                   rep(0.00198, 5),
                   rep(0.00306, 5),
                   rep(0.00517, 5),
                   rep(0.00781, 5)))^(1/(365/time.unit)), 1)

# I, R, V role frequencies
role.prob <- c(0.149, 0.213, 0.638)


# Create meanstats
st <- calc_nwstats_msm_whamp(
  method = 1,
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
  deg.m.region = deg.m.region,
  deg.p.region = deg.p.region,
  mdeg.inst = mdeg.inst,
  qnts.18to49 = qnts.18to49,
  qnts.50to59 = qnts.50to59,
  inst.bho = inst.bho,
  inst.region = inst.region,
  prop.hom.mpi.H = prop.hom.mpi.H,
  prop.hom.mpi.B = prop.hom.mpi.B,
  prop.hom.mpi.O = prop.hom.mpi.O,
  sqrt.adiff.mpi = sqrt.adiff.mpi,
  prop.hom.region.mpi,
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
  role.prob = role.prob)

save(st, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/est/nwstats.unbalanced.whamp.rda")
rm(list = ls())
