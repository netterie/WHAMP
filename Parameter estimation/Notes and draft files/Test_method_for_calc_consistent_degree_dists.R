#' This file tests methods for calculating degree distributions and rates of inst partners 
#' by attribute (e.g. by race, region, and age) that are consistent with overall estimates.
#' The issues is that there is differential missingness by attribute on variables used to
#' construct the degree distribution / rate of instantaneous partnerships (i.e. number of 
#' ongoing main and persistent partners, number of instantaneous partnerships in the past
#' 12 months). As a result, when the calculated distributions are multiplied by the number 
#' of men with each attribute value in the network, the resulting number of partnerships do
#' not add up to the same total.
#' 
#' One way to address this would be to define separate degree distributions for each combination
#' of attributes (race, region and age; age and momentary degree for the rate of inst partners),
#' weight each by the inverse proportion of people in each group that was non-missing on degree,
#' and then define overall distributions as weighted averages of these adjusted distributions. 
#' This is esssentially the same as imputing degree for those with missing data, and assumes 
#' non-differential missingness.

####################################################################################
# Setup
####################################################################################
#-----------------------------------------------------------------------------------
# Load packages
#-----------------------------------------------------------------------------------

    library("tidyverse")
    library("ggplot2")
    library("survey")
    library("knitr")
    library("xtable")
    library("kableExtra")

#-----------------------------------------------------------------------------------
# Load data
#-----------------------------------------------------------------------------------
    load(file="Data/InternetSurveySample.Rdata")
    load(file="Data/InternetSurvey_reweighted_all.Rdata")
    load(file="Data/InternetSurvey_reweighted_neg.Rdata")
  

####################################################################################
# Data manipulation
####################################################################################
## Change degree_main variable to factor and add labels have character labels
sample_rake_all$variables$degree_main <- factor(sample_rake_all$variables$degree_main, levels = c(0, 1), labels = c("No main", "Main"))

## Define indicator of age 40-49 and MRP age 40-49
sample_rake_all$variables$age40to49 <- ifelse(sample_rake_all$variables$age %in% c(40:49), 1,
                                              ifelse(!is.na(sample_rake_all$variables$age), 0,
                                                     NA))

sample_rake_all$variables$mrp_age40to49 <- ifelse(sample_rake_all$variables$mrp_ageinyears_approx %in% c(40:49), 1,
                                                  ifelse(!is.na(sample_rake_all$variables$mrp_ageinyears_approx), 0,
                                                         NA))

    
## Proportion non-missing to total number of each attribute group----
prop.nonmissing <- (svytable(~age40to49 + hbo + region, sample_rake_all[!is.na(sample_rake_all$variables$degree_main) &
                                                                            !is.na(sample_rake_all$variables$degreecat_cas)],
                             round=FALSE)) / (svytable(~age40to49 + hbo + region, sample_rake_all, round=FALSE))

## Degree distribution by race/ethnicity, region, and age (40-49 vs other)----

degmp.KC.H.otherages <- svytable(~degree_main + degreecat_cas, 
                                                 sample_rake_all[sample_rake_all$variables$region %in% "King County" &
                                                                     sample_rake_all$variables$hbo %in% "Hispanic" &
                                                                     !(sample_rake_all$variables$age %in% c(40:49))], 
                                                 round=FALSE)
degmp.KC.H.40to49 <- svytable(~degree_main + degreecat_cas, 
                              sample_rake_all[sample_rake_all$variables$region %in% "King County" &
                                                  sample_rake_all$variables$hbo %in% "Hispanic" &
                                                  sample_rake_all$variables$age %in% c(40:49)], 
                              round=FALSE)

degmp.KC.B.otherages <- svytable(~degree_main + degreecat_cas, 
                                                 sample_rake_all[sample_rake_all$variables$region %in% "King County" &
                                                                     sample_rake_all$variables$hbo %in% "Black" &
                                                                     !(sample_rake_all$variables$age %in% c(40:49))], 
                                                 round=FALSE)
degmp.KC.B.40to49 <- svytable(~degree_main + degreecat_cas, 
                              sample_rake_all[sample_rake_all$variables$region %in% "King County" &
                                                  sample_rake_all$variables$hbo %in% "Black" &
                                                  sample_rake_all$variables$age %in% c(40:49)], 
                              round=FALSE)

degmp.KC.O.otherages <- svytable(~degree_main + degreecat_cas, 
                                                 sample_rake_all[sample_rake_all$variables$region %in% "King County" &
                                                                     sample_rake_all$variables$hbo %in% "Other" &
                                                                     !(sample_rake_all$variables$age %in% c(40:49))], 
                                                 round=FALSE)
degmp.KC.O.40to49 <- svytable(~degree_main + degreecat_cas, 
                              sample_rake_all[sample_rake_all$variables$region %in% "King County" &
                                                  sample_rake_all$variables$hbo %in% "Other" &
                                                  sample_rake_all$variables$age %in% c(40:49)], 
                              round=FALSE)


degmp.OW.H.otherages <- svytable(~degree_main + degreecat_cas, 
                                                 sample_rake_all[sample_rake_all$variables$region %in% "Western WA" &
                                                                     sample_rake_all$variables$hbo %in% "Hispanic" &
                                                                     !(sample_rake_all$variables$age %in% c(40:49))], 
                                                 round=FALSE)
degmp.OW.H.40to49 <- svytable(~degree_main + degreecat_cas, 
                              sample_rake_all[sample_rake_all$variables$region %in% "Western WA" &
                                                  sample_rake_all$variables$hbo %in% "Hispanic" &
                                                  sample_rake_all$variables$age %in% c(40:49)], 
                              round=FALSE)

degmp.OW.B.otherages <- svytable(~degree_main + degreecat_cas, 
                                                 sample_rake_all[sample_rake_all$variables$region %in% "Western WA" &
                                                                     sample_rake_all$variables$hbo %in% "Black" &
                                                                     !(sample_rake_all$variables$age %in% c(40:49))], 
                                                 round=FALSE)
degmp.OW.B.40to49 <- svytable(~degree_main + degreecat_cas, 
                              sample_rake_all[sample_rake_all$variables$region %in% "Western WA" &
                                                  sample_rake_all$variables$hbo %in% "Black" &
                                                  sample_rake_all$variables$age %in% c(40:49)], 
                              round=FALSE)

degmp.OW.O.otherages <- svytable(~degree_main + degreecat_cas, 
                                                 sample_rake_all[sample_rake_all$variables$region %in% "Western WA" &
                                                                     sample_rake_all$variables$hbo %in% "Other" &
                                                                     !(sample_rake_all$variables$age %in% c(40:49))], 
                                                 round=FALSE)
degmp.OW.O.40to49 <- svytable(~degree_main + degreecat_cas, 
                              sample_rake_all[sample_rake_all$variables$region %in% "Western WA" &
                                                  sample_rake_all$variables$hbo %in% "Other" &
                                                  sample_rake_all$variables$age %in% c(40:49)], 
                              round=FALSE)


degmp.EW.H.otherages <- svytable(~degree_main + degreecat_cas, 
                                                 sample_rake_all[sample_rake_all$variables$region %in% "Eastern WA" &
                                                                     sample_rake_all$variables$hbo %in% "Hispanic" &
                                                                     !(sample_rake_all$variables$age %in% c(40:49))], 
                                                 round=FALSE)
degmp.EW.H.40to49 <- svytable(~degree_main + degreecat_cas, 
                              sample_rake_all[sample_rake_all$variables$region %in% "Eastern WA" &
                                                  sample_rake_all$variables$hbo %in% "Hispanic" &
                                                  sample_rake_all$variables$age %in% c(40:49)], 
                              round=FALSE)

degmp.EW.B.otherages <- svytable(~degree_main + degreecat_cas, 
                                                 sample_rake_all[sample_rake_all$variables$region %in% "Eastern WA" &
                                                                     sample_rake_all$variables$hbo %in% "Black" &
                                                                     !(sample_rake_all$variables$age %in% c(40:49))], 
                                                 round=FALSE)
degmp.EW.B.40to49 <- 0 #There are 0 Black men aged 40-49 in Eastern WA

degmp.EW.O.otherages <- svytable(~degree_main + degreecat_cas, 
                                                 sample_rake_all[sample_rake_all$variables$region %in% "Eastern WA" &
                                                                     sample_rake_all$variables$hbo %in% "Other" &
                                                                     !(sample_rake_all$variables$age %in% c(40:49))], 
                                                 round=FALSE)
degmp.EW.O.40to49 <- svytable(~degree_main + degreecat_cas, 
                              sample_rake_all[sample_rake_all$variables$region %in% "Eastern WA" &
                                                  sample_rake_all$variables$hbo %in% "Other" &
                                                  sample_rake_all$variables$age %in% c(40:49)], 
                              round=FALSE)

## Reweight degree matrices to account for proportion missing
degmp.KC.H.otherages.rwt <- prop.table(degmp.KC.H.otherages*(1/prop.nonmissing[1]))
degmp.KC.H.40to49.rwt <- prop.table(degmp.KC.H.40to49*(1/prop.nonmissing[2]))
degmp.KC.B.otherages.rwt <- prop.table(degmp.KC.B.otherages*(1/prop.nonmissing[3]))
degmp.KC.B.40to49.rwt <- prop.table(degmp.KC.B.40to49*(1/prop.nonmissing[4]))
degmp.KC.O.otherages.rwt <- prop.table(degmp.KC.O.otherages*(1/prop.nonmissing[5]))
degmp.KC.O.40to49.rwt <- prop.table(degmp.KC.O.40to49*(1/prop.nonmissing[6]))

degmp.OW.H.otherages.rwt <- prop.table(degmp.OW.H.otherages*(1/prop.nonmissing[7]))
degmp.OW.H.40to49.rwt <- prop.table(degmp.OW.H.40to49*(1/prop.nonmissing[8]))
degmp.OW.B.otherages.rwt <- prop.table(degmp.OW.B.otherages*(1/prop.nonmissing[9]))
degmp.OW.B.40to49.rwt <- prop.table(degmp.OW.B.40to49*(1/prop.nonmissing[10]))
degmp.OW.O.otherages.rwt <- prop.table(degmp.OW.O.otherages*(1/prop.nonmissing[11]))
degmp.OW.O.40to49.rwt <- prop.table(degmp.OW.O.40to49*(1/prop.nonmissing[12]))

degmp.EW.H.otherages.rwt <- prop.table(degmp.EW.H.otherages*(1/prop.nonmissing[13]))
degmp.EW.H.40to49.rwt <- prop.table(degmp.EW.H.40to49*(1/prop.nonmissing[14]))
degmp.EW.B.otherages.rwt <- prop.table(degmp.EW.B.otherages*(1/prop.nonmissing[15]))
degmp.EW.B.40to49.rwt <- 0
degmp.EW.O.otherages.rwt <- prop.table(degmp.EW.O.otherages*(1/prop.nonmissing[17]))
degmp.EW.O.40to49.rwt <- prop.table(degmp.EW.O.40to49*(1/prop.nonmissing[18]))

## Percent of the sample in each group defined by region, race/eth, and age----
sampledist <- prop.table(svytable(~age40to49 + hbo + region, sample_rake_all, round=FALSE))
racedist <- prop.table(svytable(~hbo, sample_rake_all, round=FALSE))
regiondist <- prop.table(svytable(~region, sample_rake_all, round=FALSE))
agedist <- prop.table(svytable(~age40to49, sample_rake_all, round=FALSE))

## Weighted distributions for overall, by race/eth, region, and age----
degmp.H <- degmp.KC.H.40to49.rwt*(sampledist[2]/racedist[1]) + 
    degmp.KC.H.otherages.rwt*(sampledist[1]/racedist[1]) + 
    degmp.OW.H.40to49.rwt*(sampledist[8]/racedist[1]) + 
    degmp.OW.H.otherages.rwt*(sampledist[7]/racedist[1]) +
    degmp.EW.H.40to49.rwt*(sampledist[14]/racedist[1]) + 
    degmp.EW.H.otherages.rwt*(sampledist[13]/racedist[1])

degmp.B <- degmp.KC.B.40to49.rwt*(sampledist[4]/racedist[2]) + 
    degmp.KC.B.otherages.rwt*(sampledist[3]/racedist[2]) + 
    degmp.OW.B.40to49.rwt*(sampledist[10]/racedist[2]) + 
    degmp.OW.B.otherages.rwt*(sampledist[9]/racedist[2]) +
    degmp.EW.B.otherages.rwt*(sampledist[15]/racedist[2])

degmp.O <- degmp.KC.O.40to49.rwt*(sampledist[6]/racedist[3]) + 
    degmp.KC.O.otherages.rwt*(sampledist[5]/racedist[3]) + 
    degmp.OW.O.40to49.rwt*(sampledist[12]/racedist[3]) + 
    degmp.OW.O.otherages.rwt*(sampledist[11]/racedist[3]) +
    degmp.EW.O.40to49.rwt*(sampledist[18]/racedist[3]) + 
    degmp.EW.O.otherages.rwt*(sampledist[17]/racedist[3])

degmp.KC <- degmp.KC.H.40to49.rwt*(sampledist[2]/regiondist[1]) + 
    degmp.KC.H.otherages.rwt*(sampledist[1]/regiondist[1]) + 
    degmp.KC.B.40to49.rwt*(sampledist[4]/regiondist[1]) + 
    degmp.KC.B.otherages.rwt*(sampledist[3]/regiondist[1]) +
    degmp.KC.O.40to49.rwt*(sampledist[6]/regiondist[1]) + 
    degmp.KC.O.otherages.rwt*(sampledist[5]/regiondist[1])

degmp.OW <- degmp.OW.H.40to49.rwt*(sampledist[8]/regiondist[2]) + 
    degmp.OW.H.otherages.rwt*(sampledist[7]/regiondist[2]) + 
    degmp.OW.B.40to49.rwt*(sampledist[10]/regiondist[2]) + 
    degmp.OW.B.otherages.rwt*(sampledist[9]/regiondist[2]) +
    degmp.OW.O.40to49.rwt*(sampledist[12]/regiondist[2]) + 
    degmp.OW.O.otherages.rwt*(sampledist[11]/regiondist[2])

degmp.EW <- degmp.EW.H.40to49.rwt*(sampledist[14]/regiondist[3]) + 
    degmp.EW.H.otherages.rwt*(sampledist[13]/regiondist[3]) + 
    degmp.EW.B.otherages.rwt*(sampledist[15]/regiondist[3]) +
    degmp.EW.O.40to49.rwt*(sampledist[18]/regiondist[3]) + 
    degmp.EW.O.otherages.rwt*(sampledist[17]/regiondist[3])

degmp.40to49 <- degmp.KC.H.40to49.rwt*(sampledist[2]/agedist[2]) + 
    degmp.KC.B.40to49.rwt*(sampledist[4]/agedist[2]) + 
    degmp.KC.O.40to49.rwt*(sampledist[6]/agedist[2]) + 
    degmp.OW.H.40to49.rwt*(sampledist[8]/agedist[2]) + 
    degmp.OW.B.40to49.rwt*(sampledist[10]/agedist[2]) + 
    degmp.OW.O.40to49.rwt*(sampledist[12]/agedist[2]) + 
    degmp.EW.H.40to49.rwt*(sampledist[14]/agedist[2]) + 
    degmp.EW.O.40to49.rwt*(sampledist[18]/agedist[2])

degmp.otherages <- degmp.KC.H.otherages.rwt*(sampledist[1]/agedist[1]) + 
    degmp.KC.B.otherages.rwt*(sampledist[3]/agedist[1]) + 
    degmp.KC.O.otherages.rwt*(sampledist[5]/agedist[1]) + 
    degmp.OW.H.otherages.rwt*(sampledist[7]/agedist[1]) + 
    degmp.OW.B.otherages.rwt*(sampledist[9]/agedist[1]) + 
    degmp.OW.O.otherages.rwt*(sampledist[11]/agedist[1]) + 
    degmp.EW.H.otherages.rwt*(sampledist[13]/agedist[1]) + 
    degmp.EW.B.otherages.rwt*(sampledist[15]/agedist[1]) + 
    degmp.EW.O.otherages.rwt*(sampledist[17]/agedist[1])

## Compare estimates of overall degree distribution from averages of distributions by race, region, and age----
degmp1 <- round(degmp.H*racedist[1] + degmp.B*racedist[2] + degmp.O*racedist[3], 4)  
degmp2 <- round(degmp.KC*regiondist[1] + degmp.OW*regiondist[2] + degmp.EW*regiondist[3], 4)
degmp3 <- round(degmp.40to49*agedist[2] + degmp.otherages*agedist[1], 4)
degmp1 == degmp2 
degmp1 == degmp3

## Implied number of partnerships----
num.edges.H <- degmp.H*svytable(~hbo, sample_rake_all, round=FALSE)[1]
num.edges.B <- degmp.B*svytable(~hbo, sample_rake_all, round=FALSE)[2]
num.edges.O <- degmp.O*svytable(~hbo, sample_rake_all, round=FALSE)[3]
num.edges1 <- (degmp.H*racedist[1] + degmp.B*racedist[2] + degmp.O*racedist[3])*sum(weights(sample_rake_all))
### Main edges
sum(num.edges.H[2,], num.edges.B[2,], num.edges.O[2,])
sum(num.edges1[2,])
### Pers edges
sum(num.edges.H[,2:3], num.edges.B[,2:3], num.edges.O[,2:3])
sum(num.edges1[,2:3])

num.edges.KC <- degmp.KC*svytable(~region, sample_rake_all, round=FALSE)[1]
num.edges.OW <- degmp.OW*svytable(~region, sample_rake_all, round=FALSE)[2]
num.edges.EW <- degmp.EW*svytable(~region, sample_rake_all, round=FALSE)[3]
num.edges2 <- (degmp.KC*regiondist[1] + degmp.OW*regiondist[2] + degmp.EW*regiondist[3])*sum(weights(sample_rake_all))
### Main edges
sum(num.edges.KC[2,], num.edges.OW[2,], num.edges.EW[2,])
sum(num.edges2[2,])
### Pers edges
sum(num.edges.KC[,2:3], num.edges.OW[,2:3], num.edges.EW[,2:3])
sum(num.edges2[,2:3])

num.edges.40to49 <- degmp.40to49*svytable(~age40to49, sample_rake_all, round=FALSE)[2]
num.edges.otherages <- degmp.otherages*svytable(~age40to49, sample_rake_all, round=FALSE)[1]
num.edges3 <- (degmp.40to49*agedist[2] + degmp.otherages*agedist[1])*sum(weights(sample_rake_all))
### Main edges
sum(num.edges.40to49[2,], num.edges.otherages[2,])
sum(num.edges3[2,])
### Pers edges
sum(num.edges.40to49[,2:3], num.edges.otherages[,2:3])
sum(num.edges3[,2:3])

round(sum(num.edges1[2,]),4) == round(sum(num.edges2[2,]),4)
round(sum(num.edges1[,2:3]),4) == round(sum(num.edges2[,2:3]),4)

round(sum(num.edges1[2,]),4) == round(sum(num.edges3[2,]),4)
round(sum(num.edges1[,2:3]),4) == round(sum(num.edges3[,2:3]),4)
