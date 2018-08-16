
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
deg.mp <- matrix(sumto1(c(0.4086, 0.0759, 0.0671, 
                          0.3481, 0.047, 0.0534)), byrow = TRUE, nrow = 2)
deg.mp.H <- matrix(sumto1(c(0.2366, 0.1575, 0.0391,
                            0.3413, 0.1759, 0.0496)), byrow = TRUE, nrow = 2)
deg.mp.B <- matrix(sumto1(c(0.4923, 0.0814, 0.0839, 
                            0.2307, 0.0395, 0.0723)), byrow = TRUE, nrow = 2)
deg.mp.O <- matrix(sumto1(c(0.4275, 0.062, 0.0696, 
                            0.3548, 0.0337, 0.0523)), byrow = TRUE, nrow = 2)
deg.mp.KC <- matrix(sumto1(c(0.3609, 0.0824, 0.0735, 
                             0.3792, 0.0464, 0.0577)), byrow = TRUE, nrow = 2)
deg.mp.OW <- matrix(sumto1(c(0.4885, 0.0688, 0.0517, 
                             0.289, 0.0502, 0.0518)), byrow = TRUE, nrow = 2)
deg.mp.EW <- matrix(sumto1(c(0.4186, 0.0621, 0.0806, 
                             0.364, 0.0401, 0.0346)), byrow = TRUE, nrow = 2)

# Instantaneous partner rates (Mean rate (degree) of inst partnerships per day by momentary main/pers degree)
  # (order: 0 main 0 pers, 0 main 1 pers, 0 main 2+ pers, 1 main 0 pers, 1 main 1 pers, 1 main 2+ pers)
mdeg.inst <- matrix(c(0.015734, 0.005987, 0.021057, 
                      0.007052, 0.009709, 0.032598), byrow = TRUE, nrow = 2) 

# Quintile distribution of overall instantaneous AI rates
qnts.18to49 <- c(0, 0.000525, 0.005024, 0.054838)
qnts.50to59 <- c(0, 0, 0.001551, 0.021283)

# Mean rate of instantaneous partnerships by race (black, Hispanic, other)
inst.bho <- c(0.0202, 0.012978, 0.012418)

# Mean rate of instantaneous partnerships by region (EW, KC, OW)
inst.region <- c(0.011216, 0.014955, 0.01)

# Proportion in same-race partnerships (main, pers, inst)
prop.hom.mpi.H <- c(0.4229, 0.1631, 0.1787)
prop.hom.mpi.B <- c(0.2939, 0.0701, 0.0853)
prop.hom.mpi.O <- c(0.8911, 0.7847, 0.7666)

# Mean age diffs (main, casl, inst) #-- UPDATE
sqrt.adiff.BB <- c(0.417, 0.498, 0.456)
sqrt.adiff.BW <- c(0.454, 0.629, 0.585)
sqrt.adiff.WW <- c(0.520, 0.632, 0.590)

# Mean durations
durs.main <- 1020
durs.pers <- 192

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
role.prob <- c(0.15, 0.205, 0.645)


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
  deg.mp.KC = deg.mp.KC,
  deg.mp.OW = deg.mp.OW,
  deg.mp.EW = deg.mp.EW,
  mdeg.inst = mdeg.inst,
  qnts.18to49 = qnts.18to49,
  qnts.50to59 = qnts.50to59,
  inst.bho = inst.bho,
  inst.region = inst.region,
  prop.hom.mpi.H = prop.hom.mpi.H,
  prop.hom.mpi.B = prop.hom.mpi.B,
  prop.hom.mpi.O = prop.hom.mpi.O,
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
  role.prob = role.prob)

save(st, file = "/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/est/nwstats.whamp.rda")
rm(list = ls())
