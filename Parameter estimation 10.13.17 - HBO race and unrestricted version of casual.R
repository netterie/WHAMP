##################################################################################
#Analysis of data from the 2017 Internet survey to inform network model parameters
##################################################################################

##load packages-----------------------------------------------------------------------------------------------------
    #install.packages("plyr")
    #library("plyr")

    #install.packages("tidyverse")
    library("tidyverse")

    #install.packages("psych") #To use "describe" function for continuous vars
    library("psych")

    #install.packages("nnet") #For multinomial logistic regression
    library("nnet")

    #install.packages("lmtest") #For likelihood ratio test
    library("lmtest")

    #install.packages("ggplot2")
    library("ggplot2")
    
    #install.packages("survey")
    library("survey")

##Prepare workspace by clearing the environment
    rm(list=ls())
    
##import data -----------------------------------------------------------------------------------------------------
    prepsurvey <- read.csv("/Volumes/Survey data_manipulated/prepsurvey_clean4R.csv",
                         header=TRUE, sep=",", row.names = "id", na.strings = c("NA", "", "."))
    prepsurvey <- read.csv("D:/prepsurvey_clean4R.csv",
                           header=TRUE, sep=",", row.names = "id", na.strings = c("NA", "", "."))

#Restrict sample 
    #' drop if invalid, remove_qa=1, not cisgender male, reported no sex with males 
    #' in the past 12 months or 0 sex partners, age<18 (set max age below), status disqualified or did not 
    #' get past screener
    sample_allages <- filter(prepsurvey, invalid==0
                      & is.na(remove_qa)
                      & !is.na(region)
                      & gender=="Male"
                      & malessexwith==1 & (numpartners!=0 | is.na(numpartners))
                      & (statuscalc=="Complete" | statuscalc=="Partial"))
    
##Generate new variables / modify variables for this analysis -----------------------------------------------------
    
    #redefine age_cat
        sample_allages$age_cat <- cut(sample_allages$age, c(17, 19, 29, 39, 49, 59), labels=c("18-19", "20-29", "30-39", 
                                                                              "40-49", "50-59"))     
        sample_allages$age_cat2 <- cut(sample_allages$age, breaks=c(17, 39, 44, 49, 54, 59, 82), 
                                               labels=c("18-39", "40-44", "45-49", "50-54", "55-59", "60-max"))
        
    #reorder the levels of race_eth_r and mrp_race_eth_r
        sample_allages$race_eth_r <- factor(sample_allages$race_eth_r, levels(sample_allages$race_eth_r)[c(3,6,2,1,5,4)])
        sample_allages$mrp_race_eth_r <- factor(sample_allages$mrp_race_eth_r, levels(sample_allages$mrp_race_eth_r)[c(4,7,2,1,6,5,3)])
    
        #Define version with white as referent value for regression
        sample_allages$race_eth_r.bW <- relevel(sample_allages$race_eth_r, ref="White")
        
    #Redefine race_eth_cat and mrp_race_eth_cat as Hispanic, black (alone or in combination), and all others
        sample_allages$hbo[!is.na(sample_allages$race_eth_r)] <- "Other"
        sample_allages$hbo[sample_allages$race_eth_r %in% "Black" | sample_allages$blackafricanamericanrace %in% 1] <- "Black"
        sample_allages$hbo[sample_allages$race_eth_r %in% "Hispanic"] <- "Hispanic"
        sample_allages$hbo <- factor(sample_allages$hbo)
        sample_allages$hbo <- factor(sample_allages$hbo, levels(sample_allages$hbo)[c(2, 1, 3)])
        
        sample_allages$mrp_hbo[!is.na(sample_allages$mrp_race_eth_r) & sample_allages$mrp_race_eth_r!="Dont know"] <- "Other"
        sample_allages$mrp_hbo[sample_allages$mrp_race_eth_r %in% "Black" | sample_allages$blackafricanamericanmrp_race %in% 1] <- "Black"
        sample_allages$mrp_hbo[sample_allages$mrp_race_eth_r %in% "Hispanic"] <- "Hispanic"
        sample_allages$mrp_hbo <- factor(sample_allages$mrp_hbo)
        sample_allages$mrp_hbo <- factor(sample_allages$mrp_hbo, levels(sample_allages$mrp_hbo)[c(2, 1, 3)])
        
        #Define version with Other as referent value for regression
        sample_allages$hbo.bO <- relevel(sample_allages$hbo, ref="Other") 
    
    #Dyad race
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Hispanic" & sample_allages$mrp_race_eth_r=="Hispanic")] <- "HH"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Hispanic" & sample_allages$mrp_race_eth_r=="White") | 
                               (sample_allages$race_eth_r %in% "White" & sample_allages$mrp_race_eth_r=="Hispanic")] <- "HW"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Hispanic" & sample_allages$mrp_race_eth_r=="Black") | 
                               (sample_allages$race_eth_r %in% "Black" & sample_allages$mrp_race_eth_r=="Hispanic")] <- "HB"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Hispanic" & sample_allages$mrp_race_eth_r=="Asian") | 
                               (sample_allages$race_eth_r %in% "Asian" & sample_allages$mrp_race_eth_r=="Hispanic")] <- "HA"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Hispanic" & sample_allages$mrp_race_eth_r=="Other") | 
                               (sample_allages$race_eth_r %in% "Other" & sample_allages$mrp_race_eth_r=="Hispanic")] <- "HO"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Hispanic" & sample_allages$mrp_race_eth_r=="Multiple") | 
                               (sample_allages$race_eth_r %in% "Multiple" & sample_allages$mrp_race_eth_r=="Hispanic")] <- "HM"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Hispanic" & sample_allages$mrp_race_eth_r=="Hispanic")] <- "WW"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "White" & sample_allages$mrp_race_eth_r=="Black") | 
                               (sample_allages$race_eth_r %in% "Black" & sample_allages$mrp_race_eth_r=="White")] <- "WB"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "White" & sample_allages$mrp_race_eth_r=="Asian") | 
                               (sample_allages$race_eth_r %in% "Asian" & sample_allages$mrp_race_eth_r=="White")] <- "WA"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "White" & sample_allages$mrp_race_eth_r=="Other") | 
                               (sample_allages$race_eth_r %in% "Other" & sample_allages$mrp_race_eth_r=="White")] <- "WO"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "White" & sample_allages$mrp_race_eth_r=="Multiple") | 
                               (sample_allages$race_eth_r %in% "Multiple" & sample_allages$mrp_race_eth_r=="White")] <- "WM"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Black" & sample_allages$mrp_race_eth_r=="Black")] <- "BB"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Black" & sample_allages$mrp_race_eth_r=="Asian") | 
                               (sample_allages$race_eth_r %in% "Asian" & sample_allages$mrp_race_eth_r=="Black")] <- "BA"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Black" & sample_allages$mrp_race_eth_r=="Other") | 
                               (sample_allages$race_eth_r %in% "Other" & sample_allages$mrp_race_eth_r=="Black")] <- "BO"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Black" & sample_allages$mrp_race_eth_r=="Multiple") | 
                               (sample_allages$race_eth_r %in% "Mulitple" & sample_allages$mrp_race_eth_r=="Black")] <- "BM"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Asian" & sample_allages$mrp_race_eth_r=="Asian")] <- "AA"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Asian" & sample_allages$mrp_race_eth_r=="Other") | 
                               (sample_allages$race_eth_r %in% "Other" & sample_allages$mrp_race_eth_r=="Asian")] <- "AO"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Asian" & sample_allages$mrp_race_eth_r=="Multiple") | 
                               (sample_allages$race_eth_r %in% "Multiple" & sample_allages$mrp_race_eth_r=="Asian")] <- "AM"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Other" & sample_allages$mrp_race_eth_r=="Other")] <- "OO"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Other" & sample_allages$mrp_race_eth_r=="Multiple") | 
                               (sample_allages$race_eth_r %in% "Multiple" & sample_allages$mrp_race_eth_r=="Other")] <- "OM"
        sample_allages$dyad_race_r[(sample_allages$race_eth_r %in% "Multiple" & sample_allages$mrp_race_eth_r=="Multiple")] <- "MM"
        sample_allages$dyad_race_r <- factor(sample_allages$dyad_race_r)

        sample_allages$dyad_race_hbo[(sample_allages$hbo %in% "Hispanic" & sample_allages$mrp_hbo=="Hispanic")] <- "HH"
        sample_allages$dyad_race_hbo[(sample_allages$hbo %in% "Hispanic" & sample_allages$mrp_hbo=="Black") | 
                               (sample_allages$hbo %in% "Black" & sample_allages$mrp_hbo=="Hispanic")] <- "HB"
        sample_allages$dyad_race_hbo[(sample_allages$hbo %in% "Hispanic" & sample_allages$mrp_hbo=="Other") | 
                                 (sample_allages$hbo %in% "Other" & sample_allages$mrp_hbo=="Hispanic")] <- "HO"
        sample_allages$dyad_race_hbo[(sample_allages$hbo %in% "Black" & sample_allages$mrp_hbo=="Black")] <- "BB"
        sample_allages$dyad_race_hbo[(sample_allages$hbo %in% "Black" & sample_allages$mrp_hbo=="Other") | 
                                 (sample_allages$hbo %in% "Other" & sample_allages$mrp_hbo=="Black")] <- "BO"
        sample_allages$dyad_race_hbo[(sample_allages$hbo %in% "Other" & sample_allages$mrp_hbo=="Other")] <- "OO"
        sample_allages$dyad_race_hbo <- factor(sample_allages$dyad_race_hbo)
        sample_allages$dyad_race_hbo <- factor(sample_allages$dyad_race_hbo, levels(sample_allages$dyad_race_hbo)[c(4, 3, 5, 1, 2, 6)])
        
    #Define version of race/eth variables to use in the model
        sample_allages$race_eth_m <- sample_allages$hbo
        sample_allages$race_eth_m.reg <- sample_allages$hbo.bO #Define vesion to use for regressoin that sets reference value
        sample_allages$mrp_race_eth_m <- sample_allages$mrp_hbo
        sample_allages$dyad_race_m <- sample_allages$dyad_race_hbo
        
    #Reorder levels of region
        sample_allages$region <- factor(sample_allages$region, levels(sample_allages$region)[c(2,3,1)])
   
        #Define version with KC as referent value to use in regression
        sample_allages$region.bKC <- relevel(sample_allages$region, ref="King County")
        
    #degree
        #Unrestricted definition for casual (count if ongoing regardless of sex freq)
        sample_allages$deg_matrix[sample_allages$degree_main==0 & sample_allages$degreecat_cas=="None"] <- "0 main 0 pers"
        sample_allages$deg_matrix[sample_allages$degree_main==0 & sample_allages$degreecat_cas=="One"] <- "0 main 1 pers"
        sample_allages$deg_matrix[sample_allages$degree_main==0 & sample_allages$degreecat_cas=="Two or more"] <- "0 main 2+ pers"
        sample_allages$deg_matrix[sample_allages$degree_main==1 & sample_allages$degreecat_cas=="None"] <- "1 main 0 pers"
        sample_allages$deg_matrix[sample_allages$degree_main==1 & sample_allages$degreecat_cas=="One"] <- "1 main 1 pers"
        sample_allages$deg_matrix[sample_allages$degree_main==1 & sample_allages$degreecat_cas=="Two or more"] <- "1 main 2+ pers"
        sample_allages$deg_matrix[is.na(sample_allages$degree_main) | is.na(sample_allages$degreecat_cas)] <- NA
        sample_allages$deg_matrix <- as.factor(sample_allages$deg_matrix)
    
    #Any ongoing partnership
        sample_allages$somepartners[!is.na(sample_allages$deg_matrix)] <- 1
        sample_allages$somepartners[sample_allages$deg_matrix %in% "0 main 0 pers"] <- 0
        
    #Any concurrent partnerships
        sample_allages$concurrent <- ifelse((sample_allages$deg_matrix %in% c("0 main 2+ pers", "1 main 1 pers") | 
                              sample_allages$deg_matrix %in% "1 main 2+ pers") & !is.na(sample_allages$deg_matrix), 1,
                                ifelse(!is.na(sample_allages$deg_matrix), 0, NA))
        
    #Redefine MRP type to change "unspecified" to NA and reorder levels so main is first
        sample_allages$mrp_type_r[sample_allages$mrp_type_r=="Unspecified"] <- NA
        sample_allages$mrp_type_r <- factor(sample_allages$mrp_type_r)
        
        sample_allages$mrp_type_r <- factor(sample_allages$mrp_type_r, levels(sample_allages$mrp_type_r)[c(2,1,3)])
        
    #Ongoing most recent partners
        sample_allages$mrp_type_ongoing[sample_allages$mrp_type_r %in% "Main" & sample_allages$mrp_ongoing %in% 1] <- "Main"
        sample_allages$mrp_type_ongoing[sample_allages$mrp_type_r %in% "Casual" & sample_allages$mrp_ongoing %in% 1] <- "Persistent"
        sample_allages$mrp_type_ongoing[is.na(sample_allages$mrp_type_r) | is.na(sample_allages$mrp_ongoing)] <- NA
        sample_allages$mrp_type_ongoing <- as.factor(sample_allages$mrp_type_ongoing)
            
    #AI rate - using data from all partnerships (not just ongoing)
        #Look at unreasonable values
        #sample_allages %>% select(mrp_type_r, mrp_morethanonce, mrp_aitimeslte10, duration_p12_days, mrp_aifreq_monthly, airate) %>% filter(airate>1 & !is.na(airate))
        
        #Drop unreasonable values (>=2)
        sample_allages$airate[sample_allages$airate>=2] <- NA

        #Define vars for main and persistent partnerships
        sample_allages$airate_main <- ifelse(sample_allages$mrp_type_r=="Main" & !is.na(sample_allages$mrp_type_r), sample_allages$airate, NA)
        sample_allages$airate_pers <- ifelse(sample_allages$mrp_type_r=="Casual" & !is.na(sample_allages$mrp_type_r), sample_allages$airate, NA)
       
    #Rate of instantaneous (one-time) partners
        sample_allages$rate_inst <- sample_allages$numonetime_r / 365.25

    #Quantiles of instantaneous partnership rates
        meanbyqnt <- function(x, y) {
            qnts <- c((mean(x[x>=0 & x<=y[1]], na.rm=TRUE)),
                      (ifelse(y[1]<y[2], mean(x[x>y[1] & x<=y[2]], na.rm=TRUE), ifelse(y[1]==y[2], y[2], NA))),
                      (ifelse(y[2]<y[3], mean(x[x>y[2] & x<=y[3]], na.rm=TRUE), ifelse(y[2]==y[3], y[3], NA))),
                      (ifelse(y[3]<y[4], mean(x[x>y[3] & x<=y[4]], na.rm=TRUE), ifelse(y[3]==y[4], y[4], NA))),
                      (ifelse(y[4]<y[5], mean(x[x>y[4] & x<=y[5]], na.rm=TRUE), ifelse(y[4]==y[5], y[5], NA))))
            return(qnts)
        }

    #Partnership age
        sample_allages$pship_age[sample_allages$pship_age<0] <- NA
        sample_allages$pship_age_main <- ifelse(sample_allages$mrp_type_ongoing=="Main" & !is.na(sample_allages$mrp_type_ongoing), sample_allages$pship_age, NA)
        sample_allages$pship_age_pers <- ifelse(sample_allages$mrp_type_ongoing=="Persistent" & !is.na(sample_allages$mrp_type_ongoing), sample_allages$pship_age, NA)
    
    #Condoms - among dyads where both partners are negative or unknown status. Using data from all dyads (not just ongoing)
        #first gen variable for condoms probability
        sample_allages$condoms_prob[sample_allages$mrp_condoms_once %in% 1] <- 1
        sample_allages$condoms_prob[sample_allages$mrp_condoms_once %in% c(0, 98)] <- 0
        sample_allages$condoms_prob[sample_allages$mrp_condoms_mtonce %in% "Every time"] <- 1
        sample_allages$condoms_prob[sample_allages$mrp_condoms_mtonce %in% "More than half the time"] <- 0.75
        sample_allages$condoms_prob[sample_allages$mrp_condoms_mtonce %in% "About half the time"] <- 0.5
        sample_allages$condoms_prob[sample_allages$mrp_condoms_mtonce %in% "Less than half the time"] <- 0.25
        sample_allages$condoms_prob[sample_allages$mrp_condoms_mtonce %in% c("Never", "I don't remember")] <- 0
        sample_allages$condoms_prob[(is.na(sample_allages$mrp_condoms_mtonce) & is.na(sample_allages$mrp_condoms_once))
                            | sample_allages$mrp_condoms_mtonce %in% "I prefer not to answer"] <- NA
         
        sample_allages$condoms_main <- ifelse(sample_allages$mrp_type_r=="Main" & !is.na(sample_allages$mrp_type_r) & 
                                    (sample_allages$mrp_statuscat_now=="HIV-negative" | sample_allages$mrp_statuscat_now=="Unknown")
                                     & !is.na(sample_allages$mrp_statuscat_now), sample_allages$condoms_prob, NA)
        sample_allages$condoms_pers <- ifelse(sample_allages$mrp_type_r=="Casual" & !is.na(sample_allages$mrp_type_r) & 
                                    (sample_allages$mrp_statuscat_now=="HIV-negative" | sample_allages$mrp_statuscat_now=="Unknown")
                                    & !is.na(sample_allages$mrp_statuscat_now), sample_allages$condoms_prob, NA)
        sample_allages$condoms_inst <- ifelse(sample_allages$mrp_type_r=="One time" & !is.na(sample_allages$mrp_type_r) & 
                                    (sample_allages$mrp_statuscat_now=="HIV-negative" | sample_allages$mrp_statuscat_now=="Unknown")
                                    & !is.na(sample_allages$mrp_statuscat_now), sample_allages$condoms_prob, NA)
        
    #Difference in SQRT of ages
        sample_allages$sqrt_agediff <- abs(sqrt(sample_allages$age) - sqrt(sample_allages$mrp_ageinyears_approx))
    
    #Never tested for HIV by age 40
        evertested <- sample_allages %>%
            filter(!is.na(evertest_r) & !is.na(race_eth_m)) %>%
            group_by(age) %>%
            summarise(prop = mean(evertest_r, na.rm=TRUE))
        
    #Intertest interval
        #Fix variable for days since last test
            #If tested in the month of the survey, set days ago to half as many days as have elapsed in the month
            extract.day <- function(x) {
                char_x <- as.character(x)
                split_x <- strsplit(char_x, "[a-z]")
                day_x <- lapply(split_x, `[[`, 1)
                day_x_num <- as.numeric(unlist(day_x))
                
                return(day_x_num)
                }
        
                sample_allages$day.of.survey <- extract.day(sample_allages$dtstart_td)
        
            samemonth <- which(sample_allages$month_lasttestdate_lasttest == sample_allages$monthsurvey & sample_allages$year_lasttestdate_lasttest == sample_allages$yearsurvey &
                                   !is.na(sample_allages$month_lasttestdate_lasttest) & !is.na(sample_allages$year_lasttestdate_lasttest))   
            sample_allages$daysago_lasttest[samemonth] <- ceiling(sample_allages$day.of.survey[samemonth] / 2)
        
            #Set to missing for the one person who indicated a date in the future
            sample_allages$daysago_lasttest[sample_allages$daysago_lasttest<0] <- NA
        
            #Set to missing if month was unknown or not provided - could keep them b/c most tested >2 years ago so won't really matter
            sample_allages$daysago_lasttest[sample_allages$month_lasttestdate_lasttest==98 | sample_allages$month_lasttestdate_lasttest==99] <- NA
            
            #replace to 6570 (18 years) if they indicated last test was before 2000
            sample_allages$daysago_lasttest[sample_allages$year_lasttestdate_lasttest=="1999"] <- 6570
        
        #Test interval
            #If we assume it's an interval process - set to 2x time since last test
                #sample_allages$iti <- 2*sample_allages$daysago_lasttest
            #Assuming it's a memoryless process
            sample_allages$iti <- sample_allages$daysago_lasttest
        
        #PrEP adherence
            sample_allages$prep_adherence_gp <- ifelse(sample_allages$prep_adherence %in% 0, "0 pills",
                                                       ifelse(sample_allages$prep_adherence %in% 1:8, "<2 pills",
                                                              ifelse(sample_allages$prep_adherence %in% 9:17, "2-3 pills",
                                                                     ifelse(sample_allages$prep_adherence %in% 18:30, "4+ pills",
                                                                            NA))))
    #Define sample age limit
        sample <- sample_allages %>%
            filter(age>17 & age<40)
        
        sample_18to64 <- sample_allages %>%
            filter(age>17 & age<65)
        
##Load data on population composition from WA census and define variables-------------------------------------------------       
    #Source: Small Area Demographic Estimates (SADE) by Age, Sex, Race and Hispanic Origin
    # Washington State Office of Financial Management, Forecasting and Research Division
    # Version: 20161203_R03_VM)
    detach(package:plyr)
    wa_census <- read.csv("Washington pop by county.csv", header=TRUE, sep=",")
    wa_census2016_15up <- filter(wa_census, Year=="2016" & !is.na(Year) 
                                 & Age.Group %in% c("15-19", "20-24", "25-29", "30-34", "35-39" ) #,"40-44", "45-49", "50-54", "55-59")
                                 & !is.na(Age.Group))
    wa_census2016_males15up_factor <- select(wa_census2016_15up, Area.Name, Male, Age.Group, Year, Hispanic.Male, Non.Hispanic.White.Male,
                                             Non.Hispanic.Black.Male, Non.Hispanic.AIAN.Male, Non.Hispanic.Asian.Male,
                                             Non.Hispanic.NHOPI.Male, Non.Hispanic.Two.or.More.Races.Male, Non.Hispanic.Male)
    
    #Convert pop counts from factor to numeric
    wa_census2016_males15up <- wa_census2016_males15up_factor
    rm(wa_census2016_males15up_factor)
    
    #Define function to convert from factor to character, remove comma, and convert to numeric
    factor2numeric <- function(var) {
        #convert to character
        var.as.char <- as.character(var)
        #remove commas
        var.no.commas <- gsub(",", "", var.as.char)
        #convert to numeric
        var.as.num <- as.numeric(var.no.commas)
        #return
        return(var.as.num)
    }
    
    #Apply function
    wa_census2016_males15up <- mutate_at(wa_census2016_males15up, .vars = c("Male", "Hispanic.Male", "Non.Hispanic.White.Male",
                                        "Non.Hispanic.Black.Male", "Non.Hispanic.AIAN.Male", "Non.Hispanic.Asian.Male",
                                        "Non.Hispanic.NHOPI.Male", "Non.Hispanic.Two.or.More.Races.Male", "Non.Hispanic.Male"),
                                         .funs = factor2numeric)
    
    #Define "other' to include Native Hawaiian/Pacific Islander and Native Indian/Alaska Native 
    wa_census2016_males15up$Non.Hispanic.Other.Male <- wa_census2016_males15up$Non.Hispanic.NHOPI.Male + wa_census2016_males15up$Non.Hispanic.AIAN.Male
    
    #Add up the numbers for each race by county
    wa_males_bycounty <- wa_census2016_males15up %>%
        group_by(Area.Name) %>%
        summarise(total.males = sum(Male),
                  total.hispanic = sum(Hispanic.Male), 
                  total.white = sum(Non.Hispanic.White.Male),
                  total.black = sum(Non.Hispanic.Black.Male),
                  total.asian = sum(Non.Hispanic.Asian.Male),
                  total.other = sum(Non.Hispanic.Other.Male),
                  total.multiple = sum(Non.Hispanic.Two.or.More.Races.Male))
    
    #generate a variable for region
    wa_males_bycounty$region <- ifelse(wa_males_bycounty$Area.Name=="King", "King County",
                                       ifelse(wa_males_bycounty$Area.Name %in% c("Clallam", "Clark",
                                             "Cowlitz", "Grays Harbor", "Island", "Jefferson",
                                             "Kitsap", "Lewis", "Mason", "Pacific", "Pierce", 
                                             "San Juan", "Skagit", "Skamania", "Snohomish", 
                                             "Thurston", "Wahkiakum", "Whatcom"), "Other Western",
                                          ifelse(wa_males_bycounty$Area.Name=="Washington", NA,
                                                 "Eastern")))
    
    #group by region and take sum
    wa_males_byregion <- wa_males_bycounty %>%
        group_by(region) %>%
        summarise(total.males.region = sum(total.males),
                  total.hispanic.region = sum(total.hispanic), 
                  total.white.region = sum(total.white),
                  total.black.region = sum(total.black),
                  total.asian.region = sum(total.asian),
                  total.other.region = sum(total.other),
                  total.multiple.region = sum(total.multiple))
    
    #Redefine race groups as Hispanic, Black, Other
        
        #Percent of multiple race category that identifies as black, based on 2010 national data
        #https://www.census.gov/prod/cen2010/briefs/c2010br-13.pdf #s from Table 2, also in Figure 3 it just states 34.3% black in combination :)
        #BUT note that this includes men who identified as Hispanic!
        multi.black <- (1834212 + 269421 + 185595 + 50308 + 314571 + 230848 +61511 + 9245 + 46641 +
                            9460 + 2142 + 8236 + 7295 + 8122 + 4233 + 19018 + 2673 + 8757 + 4852 +
                            2520 + 560 + 1011 + 539 + 212 + 574 + 6605 + 1023 + 182 + 268 + 98 + 792) / 
                        9009073
        
    wa_males_byregion_hbo <- wa_males_byregion %>%
        group_by(region) %>%
        mutate(total.anyblack.region = (total.black.region + multi.black*total.multiple.region), 
               total.hispanic.region = total.hispanic.region,
               total.anyother.region = (total.white.region + total.asian.region + total.other.region +
                   (1-multi.black)*total.multiple.region))
    
    #remove datasets don't need
    rm(wa_census2016_15up)
    rm(wa_census)
    rm(wa_census2016_males15up)
    
    #Males by region
    prop.KC <- wa_males_byregion$total.males.region[wa_males_byregion$region=="King County" & !is.na(wa_males_byregion$region)] / 
        sum(wa_males_byregion$total.males.region[!is.na(wa_males_byregion$region)])
    prop.OW <- wa_males_byregion$total.males.region[wa_males_byregion$region=="Other Western" & !is.na(wa_males_byregion$region)] / 
        sum(wa_males_byregion$total.males.region[!is.na(wa_males_byregion$region)])
    prop.E <- wa_males_byregion$total.males.region[wa_males_byregion$region=="Eastern" & !is.na(wa_males_byregion$region)] / 
        sum(wa_males_byregion$total.males.region[!is.na(wa_males_byregion$region)])
    
    #Males by race/eth
        #Using detailed race/eth groups
        prop.H <- wa_males_byregion$total.hispanic.region[is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[is.na(wa_males_byregion$region)]
        prop.W <- wa_males_byregion$total.white.region[is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[is.na(wa_males_byregion$region)]
        prop.B <- wa_males_byregion$total.black.region[is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[is.na(wa_males_byregion$region)]
        prop.A <- wa_males_byregion$total.asian.region[is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[is.na(wa_males_byregion$region)]
        prop.O <- wa_males_byregion$total.other.region[is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[is.na(wa_males_byregion$region)]
        prop.M <- wa_males_byregion$total.multiple.region[is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[is.na(wa_males_byregion$region)]
    
        #Using HBO race/eth groups
        HBO.prop.H <- wa_males_byregion_hbo$total.hispanic.region[is.na(wa_males_byregion_hbo$region)] / 
            wa_males_byregion_hbo$total.males.region[is.na(wa_males_byregion_hbo$region)]
        HBO.prop.B <- wa_males_byregion_hbo$total.anyblack.region[is.na(wa_males_byregion_hbo$region)] / 
            wa_males_byregion_hbo$total.males.region[is.na(wa_males_byregion_hbo$region)]
        HBO.prop.O <- wa_males_byregion_hbo$total.anyother.region[is.na(wa_males_byregion_hbo$region)] / 
            wa_males_byregion_hbo$total.males.region[is.na(wa_males_byregion_hbo$region)]
            
    #Males by race within each region
        #Using detailed race/eth groups
        prop.H.KC <- wa_males_byregion$total.hispanic.region[wa_males_byregion$region=="King County" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="King County" & !is.na(wa_males_byregion$region)]
        prop.W.KC <- wa_males_byregion$total.white.region[wa_males_byregion$region=="King County" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="King County" & !is.na(wa_males_byregion$region)]
        prop.B.KC <- wa_males_byregion$total.black.region[wa_males_byregion$region=="King County" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="King County" & !is.na(wa_males_byregion$region)]
        prop.A.KC <- wa_males_byregion$total.asian.region[wa_males_byregion$region=="King County" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="King County" & !is.na(wa_males_byregion$region)]
        prop.O.KC <- wa_males_byregion$total.other.region[wa_males_byregion$region=="King County" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="King County" & !is.na(wa_males_byregion$region)]
        prop.M.KC <- wa_males_byregion$total.multiple.region[wa_males_byregion$region=="King County" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="King County" & !is.na(wa_males_byregion$region)]
        
        prop.H.OW <- wa_males_byregion$total.hispanic.region[wa_males_byregion$region=="Other Western" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="Other Western" & !is.na(wa_males_byregion$region)]
        prop.W.OW <- wa_males_byregion$total.white.region[wa_males_byregion$region=="Other Western" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="Other Western" & !is.na(wa_males_byregion$region)]
        prop.B.OW <- wa_males_byregion$total.black.region[wa_males_byregion$region=="Other Western" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="Other Western" & !is.na(wa_males_byregion$region)]
        prop.A.OW <- wa_males_byregion$total.asian.region[wa_males_byregion$region=="Other Western" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="Other Western" & !is.na(wa_males_byregion$region)]
        prop.O.OW <- wa_males_byregion$total.other.region[wa_males_byregion$region=="Other Western" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="Other Western" & !is.na(wa_males_byregion$region)]
        prop.M.OW <- wa_males_byregion$total.multiple.region[wa_males_byregion$region=="Other Western" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="Other Western" & !is.na(wa_males_byregion$region)]
        
        prop.H.E <- wa_males_byregion$total.hispanic.region[wa_males_byregion$region=="Eastern" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="Eastern" & !is.na(wa_males_byregion$region)]
        prop.W.E <- wa_males_byregion$total.white.region[wa_males_byregion$region=="Eastern" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="Eastern" & !is.na(wa_males_byregion$region)]
        prop.B.E <- wa_males_byregion$total.black.region[wa_males_byregion$region=="Eastern" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="Eastern" & !is.na(wa_males_byregion$region)]
        prop.A.E <- wa_males_byregion$total.asian.region[wa_males_byregion$region=="Eastern" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="Eastern" & !is.na(wa_males_byregion$region)]
        prop.O.E <- wa_males_byregion$total.other.region[wa_males_byregion$region=="Eastern" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="Eastern" & !is.na(wa_males_byregion$region)]
        prop.M.E <- wa_males_byregion$total.multiple.region[wa_males_byregion$region=="Eastern" & !is.na(wa_males_byregion$region)] / 
            wa_males_byregion$total.males.region[wa_males_byregion$region=="Eastern" & !is.na(wa_males_byregion$region)]
    
        #Using HBO race/eth groups
        HBO.prop.H.KC <- wa_males_byregion_hbo$total.hispanic.region[wa_males_byregion_hbo$region=="King County" & !is.na(wa_males_byregion_hbo$region)] / 
            wa_males_byregion_hbo$total.males.region[wa_males_byregion_hbo$region=="King County" & !is.na(wa_males_byregion_hbo$region)]
        HBO.prop.B.KC <- wa_males_byregion_hbo$total.anyblack.region[wa_males_byregion_hbo$region=="King County" & !is.na(wa_males_byregion_hbo$region)] / 
            wa_males_byregion_hbo$total.males.region[wa_males_byregion_hbo$region=="King County" & !is.na(wa_males_byregion_hbo$region)]
        HBO.prop.O.KC <- wa_males_byregion_hbo$total.anyother.region[wa_males_byregion_hbo$region=="King County" & !is.na(wa_males_byregion_hbo$region)] / 
            wa_males_byregion_hbo$total.males.region[wa_males_byregion_hbo$region=="King County" & !is.na(wa_males_byregion_hbo$region)]
        
        HBO.prop.H.OW <- wa_males_byregion_hbo$total.hispanic.region[wa_males_byregion_hbo$region=="Other Western" & !is.na(wa_males_byregion_hbo$region)] / 
            wa_males_byregion_hbo$total.males.region[wa_males_byregion_hbo$region=="Other Western" & !is.na(wa_males_byregion_hbo$region)]
        HBO.prop.B.OW <- wa_males_byregion_hbo$total.anyblack.region[wa_males_byregion_hbo$region=="Other Western" & !is.na(wa_males_byregion_hbo$region)] / 
            wa_males_byregion_hbo$total.males.region[wa_males_byregion_hbo$region=="Other Western" & !is.na(wa_males_byregion_hbo$region)]
        HBO.prop.O.OW <- wa_males_byregion_hbo$total.anyother.region[wa_males_byregion_hbo$region=="Other Western" & !is.na(wa_males_byregion_hbo$region)] / 
            wa_males_byregion_hbo$total.males.region[wa_males_byregion_hbo$region=="Other Western" & !is.na(wa_males_byregion_hbo$region)]
        
        HBO.prop.H.E <- wa_males_byregion_hbo$total.hispanic.region[wa_males_byregion_hbo$region=="Eastern" & !is.na(wa_males_byregion_hbo$region)] / 
            wa_males_byregion_hbo$total.males.region[wa_males_byregion_hbo$region=="Eastern" & !is.na(wa_males_byregion_hbo$region)]
        HBO.prop.B.E <- wa_males_byregion_hbo$total.anyblack.region[wa_males_byregion_hbo$region=="Eastern" & !is.na(wa_males_byregion_hbo$region)] / 
            wa_males_byregion_hbo$total.males.region[wa_males_byregion_hbo$region=="Eastern" & !is.na(wa_males_byregion_hbo$region)]
        HBO.prop.O.E <- wa_males_byregion_hbo$total.anyother.region[wa_males_byregion_hbo$region=="Eastern" & !is.na(wa_males_byregion_hbo$region)] / 
            wa_males_byregion_hbo$total.males.region[wa_males_byregion_hbo$region=="Eastern" & !is.na(wa_males_byregion_hbo$region)]
            

#Compare main/persistent most recent partners reported by those with 0 vs 1+ persistent/main partner------------
    sample$degbin_cas <- ifelse(sample$degreecat_cas %in% c("One", "Two or more"), 1, 
                                ifelse(sample$degreecat_cas %in% "None", 0, NA))
    sample$same_race <- ifelse(sample$mrp_race_eth_m %in% "Dont know", NA,
                               ifelse(sample$race_eth_m==sample$mrp_race_eth_m, 1, 
                                      ifelse(!is.na(sample$race_eth_m) & !is.na(sample$mrp_race_eth_m), 0,
                                             NA)))
    sample$mrp_statuscat_now_clean <- sample$mrp_statuscat_now
    sample$mrp_statuscat_now_clean[sample$mrp_statuscat_now_clean=="I prefer not to answer"] <- NA
    sample$mrp_statuscat_now_clean <- factor(sample$mrp_statuscat_now_clean)
    
    #Main partners among those with/without a persistent ongoing partner
        describe <- sample %>%
                #filter(mrp_type_ongoing=="Main") %>%
                group_by(degbin_cas) %>%
                summarise_at(vars(pship_age_main, airate_main, sqrt_agediff, condoms_main), funs(mean(., na.rm=TRUE), median(., na.rm=TRUE), sum(!is.na(.))))
        t(describe)
            kruskal.test(pship_age_main ~ degbin_cas, sample)
            kruskal.test(airate_main ~ degbin_cas, sample)
            kruskal.test(sqrt_agediff ~ degbin_cas, sample)
            kruskal.test(condoms_main ~ degbin_cas, sample)
        
        catvars_main <- function(x) {
            tbl <- table(sample$degbin_cas[sample$mrp_type_ongoing=="Main"], x)
            X2 <- chisq.test(tbl)
            fisher <- fisher.test(tbl)
            return(list(tbl, X2, fisher))
        }
        
        catvars_main(sample$same_race[sample$mrp_type_ongoing=="Main"])
        catvars_main(sample$mrp_statuscat_now_clean[sample$mrp_type_ongoing=="Main"])
     
    #Persistent partners among those with/without a main ongoing partner
        describe <- sample %>%
            #filter(mrp_type_ongoing=="Persistent") %>%
            group_by(degree_main) %>%
            summarise_at(vars(pship_age_pers, airate_pers, sqrt_agediff, condoms_pers), funs(mean(., na.rm=TRUE), median(., na.rm=TRUE), sum(!is.na(.))))
        t(describe)
        
        kruskal.test(pship_age_pers ~ degree_main, sample)
        kruskal.test(airate_pers ~ degree_main, sample)
        kruskal.test(sqrt_agediff ~ degree_main, sample)
        kruskal.test(condoms_pers ~ degree_main, sample)
        
        catvars_pers <- function(x) {
            tbl <- table(sample$degree_main[sample$mrp_type_ongoing=="Persistent"], x)
            X2 <- chisq.test(tbl)
            fisher <- fisher.test(tbl)
            return(list(tbl, X2, fisher))
        }
        
        catvars_pers(sample$same_race[sample$mrp_type_ongoing=="Persistent"])
        catvars_pers(sample$mrp_statuscat_now_clean[sample$mrp_type_ongoing=="Persistent"])
    
        
##Analyze data--------------------------------------------------------------------------------------------------    
#General settings
    #Set digits to show 3 decimal places
        options(digits=3)
    #Set options for dplyr to print all rows
        options(dplyr.print_min = Inf)
    
    #save plot settings for white background and light grey lines and darkening the colors a bit
        plot_background <- theme(panel.background = element_rect(fill="white", colour = "black")) +
            theme(panel.grid.major = element_line(colour = "grey90"))
        darken_color <- scale_colour_hue(l=50)
        theme_title <- theme(plot.title = element_text(hjust = 0.5, size=12)) 
        
    #Define multiplot function
        multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
            library(grid)
            
            # Make a list from the ... arguments and plotlist
            plots <- c(list(...), plotlist)
            
            numPlots = length(plots)
            
            # If layout is NULL, then use 'cols' to determine layout
            if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
            }
            
            if (numPlots==1) {
                print(plots[[1]])
                
            } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                    # Get the i,j matrix positions of the regions that contain this subplot
                    matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                    
                    print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                    layout.pos.col = matchidx$col))
                }
            }
        }
        
#Look at age to think about capping it at a lower age
    hist(sample$age)
    ggplot(sample, aes(x=age)) + geom_histogram(binwidth=1) + facet_grid(race_eth_m~.)
    table(sample$age_cat)
    
#Composition of the network
    prop.table(table(sample$region))
    prop.table(table(sample$race_eth_m))
    
    #From census data
        prop.KC
        prop.OW
        prop.E
        
        HBO.prop.H
        HBO.prop.B
        HBO.prop.O

        HBO.prop.H.KC
        HBO.prop.B.KC
        HBO.prop.O.KC
      
        HBO.prop.H.OW
        HBO.prop.B.OW
        HBO.prop.O.OW
        
        HBO.prop.H.E
        HBO.prop.B.E
        HBO.prop.O.E
      
#Degree distribution
    #overall
      prop.table(table(sample$deg_matrix))
    
    #See if it varies by race and/or region
        #One way to do it: multinomial model
            multinom.mdeg <- multinom(deg_matrix ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, data=sample)
            summary(multinom.mdeg)
            z <- summary(multinom.mdeg)$coefficients / summary(multinom.mdeg)$standard.errors
            #2-tailed z test
            p <- (1-pnorm(abs(z), 0, 1)) * 2
            p
            
            #possible way to test for joint significance
                #test interaction
                multinom.mdeg0 <-multinom(deg_matrix ~ race_eth_m.reg + region.bKC, data=sample)
                anova(multinom.mdeg, multinom.mdeg0)
                
                #test region
                multinom.mdeg01 <- multinom(deg_matrix ~ race_eth_m.reg, data=sample)
                anova(multinom.mdeg0, multinom.mdeg01)
                
                #test race
                multinom.mdeg02 <- multinom(deg_matrix[!is.na(sample$race_eth_m)] ~ region.bKC[!is.na(sample$race_eth_m)], data=sample)
                anova(multinom.mdeg0, multinom.mdeg02)
            
        #Other way: separate logistic models for prob of having any partners and prob of being concurrent
                #Percent with 1+ partners
                glm.somepartners <- glm(somepartners ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, family=binomial(link="logit"), 
                                        data=sample[!is.na(sample$race_eth_m.reg),])
                glm.somepartners_noint <- glm(somepartners ~ race_eth_m.reg + region.bKC, family=binomial(link="logit"), 
                                             data=sample[!is.na(sample$race_eth_m.reg),])
                glm.somepartners_noregion <- glm(somepartners ~ race_eth_m.reg, family=binomial(link="logit"), 
                                                 data=sample[!is.na(sample$race_eth_m.reg),])
                glm.somepartners_norace <- glm(somepartners ~ region.bKC, family=binomial(link="logit"),
                                                data=sample[!is.na(sample$race_eth_m.reg),])
                
                lrtest(glm.somepartners, glm.somepartners_noint)
                lrtest(glm.somepartners_noint, glm.somepartners_noregion)
                lrtest(glm.somepartners_noint, glm.somepartners_norace)
                
                #Percent concurrent
                glm.concurrent <- glm(concurrent ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, family=binomial(link="logit"), 
                                        data=sample[!is.na(sample$race_eth_m.reg),])
                glm.concurrent_noint <- glm(concurrent ~ race_eth_m.reg + region.bKC, family=binomial(link="logit"), 
                                              data=sample[!is.na(sample$race_eth_m.reg),])
                glm.concurrent_noregion <- glm(concurrent ~ race_eth_m.reg, family=binomial(link="logit"), 
                                                 data=sample[!is.na(sample$race_eth_m.reg),])
                glm.concurrent_norace <- glm(concurrent ~ region.bKC, family=binomial(link="logit"),
                                               data=sample[!is.na(sample$race_eth_m.reg),])
                
                lrtest(glm.concurrent, glm.concurrent_noint)
                lrtest(glm.concurrent_noint, glm.concurrent_noregion)
                lrtest(glm.concurrent_noint, glm.concurrent_norace)
                
    #By race
        table(sample$race_eth_m[!is.na(sample$deg_matrix)]) #look at Ns by race
        prop.table(table(sample$race_eth_m, sample$deg_matrix), 1)
        
    #Plot by race and region
        #By race
            #To get plot points to layer with blacks and hispanics on top, change levels
            deg_matrixXrace <- sample %>% group_by(race_eth_m, deg_matrix) %>% summarise(n=n()) %>%
                filter(!is.na(race_eth_m) & !is.na(deg_matrix)) %>%
                group_by(race_eth_m) %>% mutate(proportion = n / sum(n))
         
            ggplot(deg_matrixXrace, aes(x=deg_matrix, y=proportion, color=race_eth_m)) +
                geom_point(shape=16, size=3) + plot_background + theme_title +
                scale_color_manual(name="Race/ethnicity", values = c("Hispanic" = "black", "White"="royalblue", "Black"="red3",
                                             "Asian"="yellowgreen", "Other"="goldenrod1", "Multiple"="mediumpurple"),
                                             breaks=c("Hispanic", "White", "Black", "Asian", "Other", "Multiple"), 
                                             labels=c("Hispanic (n=175)", "White (n=592)", "Black (n=36)",
                                             "Asian (n=31)", "Other (n=13)", "Multiple (n=41)")) +
                labs(x="Momentary degree", y="Proportion", title="Degree distribution by race/ethnicity")
            
        #By race and region
            #Difficult to visualize by region b/c too many categories and some missing cells. Numbers for some races ~1 by region (printed next to points).
            # deg_matrixXraceregion <- sample %>% group_by(region, race_eth_m, deg_matrix) %>% summarise(n=n()) %>%
            #     filter(!is.na(race_eth_m) & !is.na(deg_matrix) & !is.na(region)) %>%
            #     group_by(region, race_eth_m) %>% mutate(proportion = n / sum(n))
            # 
            #  ggplot(deg_matrixXraceregion, aes(x=deg_matrix, y=proportion, color=race_eth_m)) +
            #     geom_point(shape=16, size=3) + plot_background + theme_title +
            #     facet_grid(region ~ .) + geom_text(aes(label=n), hjust=-1, vjust=0.5, size=3) +
            #     scale_color_manual(name="Race/ethnicity", values = c("Hispanic" = "black", "White"="royalblue", "Black"="red3",
            #                         "Asian"="yellowgreen", "Other"="goldenrod1", "Multiple"="mediumpurple"),
            #                         breaks=c("Hispanic", "White", "Black", "Asian", "Other", "Multiple")) +
            #     scale_x_discrete(labels=c("0 main \n 0 pers", "0 main \n 1 pers", "0 main \n 2+ pers", "1 main \n 0 pers", "1 main \n 1 pers", "1 main \n 2+ pers")) +
            #     labs(x="Momentary degree", y="Percent", title="Degree distribution by race/ethnicity and region")
            
           
    #Look at any concurrent overall and by race
        prop.table(table(sample$concurrent))
        prop.table(table(sample$race_eth_m, sample$concurrent), 1)

        concurrentXrace <- (as.data.frame(prop.table(table(sample$race_eth_m, sample$concurrent), 1))) 
        concurrentXrace <- concurrentXrace %>% filter(Var2==1) %>% mutate(definition = "Unrestricted")
        ggplot(concurrentXrace, aes(x=Var1, y=Freq)) + 
            geom_point(shape=16, size=3) + darken_color + plot_background + theme_title +
            labs(x="Race/ethnicity", y="Percent concurrent", title="Concurrency by race/ethnicity")
    
        concurrentXraceregion <- sample %>% group_by(region, race_eth_m, concurrent) %>% summarise(n=n()) %>%
            filter(!is.na(race_eth_m) & !is.na(concurrent) & !is.na(region)) %>%
            group_by(region, race_eth_m) %>% mutate(proportion = n / sum(n)) %>% filter(concurrent==1)
        ggplot(concurrentXraceregion, aes(x=race_eth_m, y=proportion, color=region)) + 
            geom_point(shape=16, size=3) + darken_color + plot_background + theme_title +
            geom_text(aes(label=n), hjust=-1, vjust=0.5, size=3) +
            labs(x="Race/ethnicity", y="Percent concurrent", title="Concurrency by race/ethnicity and region")
        
#Rate of instantaneous partnerships
    #Overall
        mean(sample$rate_inst, na.rm = TRUE)
        describe(sample$rate_inst)
    
    #See if it varies by race and/or region
        lm.rate_inst <- lm(rate_inst ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
        summary(lm.rate_inst)
        
        #one way to test joint significance - another possible way: regTermTest in survey package
        lm.rate_inst.noint <- lm(rate_inst ~ race_eth_m.reg + region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
        lrtest(lm.rate_inst, lm.rate_inst.noint)
        
        lm.rate_inst.noregion <- lm(rate_inst ~ race_eth_m.reg, data=sample[!is.na(sample$race_eth_m.reg),])
        lrtest(lm.rate_inst.noint, lm.rate_inst.noregion)
        
        lm.rate_inst.norace <- lm(rate_inst ~ region.bKC, data=sample[!is.na(sample$race_eth_m.reg),])
        lrtest(lm.rate_inst.noint, lm.rate_inst.norace)
          
    #By race
        table(sample$race_eth_m[!is.na(sample$rate_inst)]) #look at Ns by race
        sample %>% 
            filter(!is.na(deg_matrix) & !is.na(race_eth_m)) %>%
            group_by(race_eth_m, deg_matrix) %>% 
            summarize(mean(rate_inst, na.rm=TRUE), n())
        
        #Plot by race and region - note this is overall, not by degree 
            rate_inst_Xrace <- sample %>% filter(!is.na(race_eth_m) & !is.na(rate_inst)) %>% group_by(race_eth_m) %>% 
                summarise(mean = mean(rate_inst, na.rm=TRUE), median=median(rate_inst, na.rm=TRUE), n=n())
            ggplot(rate_inst_Xrace, aes(x=race_eth_m, y=mean)) + geom_point(shape=16, size=3) + plot_background +
                scale_x_discrete(labels=c("Hispanic \n (n=149)", "Black \n (n=37)", "Other \n (n=472)")) +
                labs(x="Race/ethnicity", y="Mean rate per day", title="Mean rate of instant partnerships by race/ethnicity") +
                theme_title
            
            rate_inst_Xraceregion <- sample %>% filter(!is.na(race_eth_m) & !is.na(rate_inst)) %>% group_by(race_eth_m, region) %>% 
                summarise(mean = mean(rate_inst, na.rm=TRUE), median=median(rate_inst, na.rm=TRUE))
            ggplot(rate_inst_Xraceregion, aes(x=race_eth_m, y=mean, color=region)) + geom_point(shape=16, size=3) + 
                scale_color_brewer(type="qual", palette=2) + plot_background + theme_title +
                labs(x="Race/ethnicity", y="Mean rate per day", title="Mean rate of instant partnerships by race/ethnicity and region")
    
    #Quantiles by race
        table(sample$race_eth_m[!is.na(sample$rate_inst)]) #look at Ns by race
        
        qnt_limits.H <- quantile(sample$rate_inst[sample$race_eth_m=="Hispanic"], probs=seq(0.2, 1, 0.2), na.rm=TRUE)
        qnts.H <- meanbyqnt(sample$rate_inst[sample$race_eth_m %in% "Hispanic"], qnt_limits.H)
        
        qnt_limits.B <- quantile(sample$rate_inst[sample$race_eth_m=="Black"], probs=seq(0.2, 1, 0.2), na.rm=TRUE)
        qnts.B <- meanbyqnt(sample$rate_inst[sample$race_eth_m %in% "Black"], qnt_limits.B)
        
        qnt_limits.O <- quantile(sample$rate_inst[sample$race_eth_m=="Other"], probs=seq(0.2, 1, 0.2), na.rm=TRUE)
        qnts.O <- meanbyqnt(sample$rate_inst[sample$race_eth_m %in% "Other"], qnt_limits.O)
        
        qnts.H
        qnts.B
        qnts.O
        
        #IF USING RACE_ETH_R FOR MODELED RACE GROUP, QNTS.B WILL BE FOR BLACK RACE ALONE AND OTHER WILL NOT INCLUDE ASIANS AND WHITES. WILL ALSO USE BELOW:
            # qnt_limits.W <- quantile(sample$rate_inst[sample$race_eth_m=="White"], probs=seq(0.2, 1, 0.2), na.rm=TRUE)
            # qnts.W <- meanbyqnt(sample$rate_inst[sample$race_eth_m %in% "White"], qnt_limits.W)
            # 
            # qnt_limits.A <- quantile(sample$rate_inst[sample$race_eth_m=="Asian"], probs=seq(0.2, 1, 0.2), na.rm=TRUE)
            # qnts.A <- meanbyqnt(sample$rate_inst[sample$race_eth_m %in% "Asian"], qnt_limits.A)
            # 
            # qnt_limits.M <- quantile(sample$rate_inst[sample$race_eth_m=="Multiple"], probs=seq(0.2, 1, 0.2), na.rm=TRUE)
            # qnts.M <- meanbyqnt(sample$rate_inst[sample$race_eth_m %in% "Multiple"], qnt_limits.M)
        
            # qnts.W
            # qnts.A
            # qnts.M

    
#Selective mixing by race 
    table(sample$race_eth_m[!is.na(sample$mrp_race_eth_m) & sample$mrp_race_eth_m!="Dont know"], 
          sample$mrp_type_r[!is.na(sample$mrp_race_eth_m) & sample$mrp_race_eth_m!="Dont know"]) #look at Ns by race and mrp_type
    
    prop.table(table(sample$race_eth_m[sample$mrp_race_eth_m!="Dont know" & sample$mrp_type_r %in% "Main"], 
                     sample$mrp_race_eth_m[sample$mrp_race_eth_m!="Dont know" & sample$mrp_type_r %in% "Main"]), 1)
    prop.table(table(sample$race_eth_m[sample$mrp_race_eth_m!="Dont know" & sample$mrp_type_r %in% "Casual"], 
                     sample$mrp_race_eth_m[sample$mrp_race_eth_m!="Dont know" & sample$mrp_type_r %in% "Casual"]), 1)
    prop.table(table(sample$race_eth_m[sample$mrp_race_eth_m!="Dont know" & sample$mrp_type_r %in% "One time"], 
                     sample$mrp_race_eth_m[sample$mrp_race_eth_m!="Dont know" & sample$mrp_type_r %in% "One time"]), 1)
    
    #Look at whether proportion homophilous differs by race and region - this is looking overall - not by partner type
        sample$mrp_race_eth_m_removedk <- sample$mrp_race_eth_m
        sample$mrp_race_eth_m_removedk[sample$mrp_race_eth_m_removedk=="Dont know"] <- NA
        sample$mrp_race_eth_m_removedk <- factor(sample$mrp_race_eth_m_removedk)
        sample$same_race <- ifelse(sample$race_eth_m==sample$mrp_race_eth_m_removedk, 1,
                                   ifelse(!is.na(sample$race_eth_m) & !is.na(sample$mrp_race_eth_m_removedk), 0,
                                          NA))    

        glm.same_race <- glm(same_race ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC + mrp_type_r, family=binomial(link="logit"), data=sample[!is.na(sample$race_eth_m),])
        summary(glm.same_race)
        
        #one way to test joint significance
            glm.same_race.noint <- glm(same_race ~ race_eth_m.reg + region.bKC + mrp_type_r, family=binomial(link="logit"), data=sample[!is.na(sample$race_eth_m),])
            lrtest(glm.same_race, glm.same_race.noint)
        
            glm.same_race.noregion <- glm(same_race ~ race_eth_m.reg + mrp_type_r, family=binomial(link="logit"), data=sample[!is.na(sample$race_eth_m),])
            lrtest(glm.same_race.noint, glm.same_race.noregion)
            
            glm.same_race.norace <- glm(same_race ~ region.bKC + mrp_type_r, family=binomial(link="logit"), data=sample[!is.na(sample$race_eth_m),])
            lrtest(glm.same_race.noint, glm.same_race.norace)

    #Plot proportion homophilous on race
        race_mixing <- sample %>% 
            filter(!is.na(race_eth_m) & !is.na(mrp_race_eth_m) & !is.na(mrp_type_r) & mrp_race_eth_m!="Dont know") %>%
            group_by(mrp_type_r, race_eth_m, mrp_race_eth_m) %>% 
            summarise(n=n()) %>%
            group_by(mrp_type_r, race_eth_m) %>% mutate(total_n = sum(n), proportion = n / sum(n))
        #Get proportion same race - first drop "Don't know" level
        race_mixing$mrp_race_eth_m <- factor(race_mixing$mrp_race_eth_m)
        same_race <- race_mixing %>% filter(race_eth_m==mrp_race_eth_m)    
    
        p.m_samerace <- ggplot(same_race[same_race$mrp_type_r=="Main", ], aes(x=race_eth_m, y=proportion)) + geom_col() + 
            plot_background + scale_x_discrete(labels=c("Hispanic \n (n=85)", "Black \n (n=15)", "Other \n (n=208)")) + 
            labs(x="", y="Proportion same race", title="Proportion of main partnerships same race") + theme_title +
            ylim(0, 0.9)
        p.p_samerace <- ggplot(same_race[same_race$mrp_type_r=="Casual", ], aes(x=race_eth_m, y=proportion)) + geom_col() + 
            plot_background + scale_x_discrete(labels=c("Hispanic \n (n=24)", "Black \n (n=11)", "Other \n (n=110)")) + 
            labs(x="", y="Proportion same race", title="Proportion of persistent partnerships same race") + theme_title +
            ylim(0, 0.9)
        p.i_samerace <- ggplot(same_race[same_race$mrp_type_r=="One time", ], aes(x=race_eth_m, y=proportion)) + geom_col() + 
            plot_background + scale_x_discrete(labels=c("Hispanic \n (n=28)", "Black \n (n=8)", "Other \n (n=82)")) + 
            labs(x="Ego race/ethnicity", y="Proportion same race", title="Proportion of instantaneous partnerships same race") + 
            theme_title +  ylim(0, 0.9)
        
        multiplot(p.m_samerace, p.p_samerace, p.i_samerace, cols=1)
            
    
#Selective mixing by age
    #Overall
        mean(sample$sqrt_agediff, na.rm = TRUE)
        describe(sample$sqrt_agediff)
        
    #See if it varies by race and/or region overall. Note this does not look within parter types
        #To look if it varies by dyad race, use var dyad_race_m instead of race_eth_m
        lm.sqrt_adiff <- lm(sqrt_agediff ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, data=sample[!is.na(sample$race_eth_m),])
        summary(lm.sqrt_adiff)
    
        #one way to test joint significance - another possible way: regTermTest in survey package
            lm.sqrt_adiff.noint <- lm(sqrt_agediff ~ race_eth_m.reg + region.bKC, data=sample[!is.na(sample$race_eth_m),])
            lrtest(lm.sqrt_adiff, lm.sqrt_adiff.noint)
            
            lm.sqrt_adiff.noregion <- lm(sqrt_agediff ~ race_eth_m.reg, data=sample[!is.na(sample$race_eth_m),])
            lrtest(lm.sqrt_adiff.noint, lm.sqrt_adiff.noregion)
            
            lm.sqrt_adiff.norace <- lm(sqrt_agediff ~ region.bKC, data=sample[!is.na(sample$race_eth_m),])
            lrtest(lm.sqrt_adiff.noint, lm.sqrt_adiff.norace)
    
    #By race
        table(sample$race_eth_m[!is.na(sample$sqrt_agediff) & !is.na(sample$mrp_type_r) & !is.na(sample$mrp_race_eth_m)]) #look at Ns by race
        
        sample %>%
            filter(!is.na(dyad_race_m) & !is.na(sqrt_agediff) & !is.na(mrp_type_r)) %>%
            group_by(dyad_race_m, mrp_type_r) %>%
            summarize(mean(sqrt_agediff, na.rm=TRUE), n())

    #Plot by race and region
    sqrt_agediffXrace <- sample %>% 
        filter(!is.na(dyad_race_m) & !is.na(sqrt_agediff) & !is.na(mrp_type_r)) %>% 
        group_by(dyad_race_m, mrp_type_r) %>%
        summarise(mean = mean(sqrt_agediff, na.rm=TRUE), median=median(sqrt_agediff, na.rm=TRUE), n=n())
    ggplot(sqrt_agediffXrace, aes(x=dyad_race_m, y=mean)) + geom_point(shape=16, size=3) + facet_grid(mrp_type_r~.) +
        darken_color + plot_background + theme_title +
        labs(x="Dyad race/ethnicity", y="Mean absolute difference", title="Mean absolute difference in sqrt of ages by race/ethnicity")

    sqrt_agediffXregion <- sample %>% 
        filter(!is.na(region) & !is.na(sqrt_agediff) & !is.na(mrp_type_r)) %>% 
        group_by(region, mrp_type_r) %>%
        summarise(mean = mean(sqrt_agediff, na.rm=TRUE), median=median(sqrt_agediff, na.rm=TRUE), n=n())
    ggplot(sqrt_agediffXregion, aes(x=region, y=mean)) + geom_point(shape=16, size=3) + facet_grid(mrp_type_r~.) +
        darken_color + plot_background + theme_title + ylim(0, 1) + 
        labs(x="Region", y="Mean absolute difference", title="Mean absolute difference in sqrt of ages by region")
    
#Partnership age and dissolution (1/pship age)
    #Main
        #Overall
        summary(sample$pship_age_main, na.rm=TRUE)

        #plot with exponential overlay
        hist(sample$pship_age_main, probability=TRUE, breaks=66, main="Age of main partnerships")
        curve(dexp(x, rate = log(2)/(median(sample$pship_age_main, na.rm=TRUE))), col = 2, lty = 2, lwd = 2, add = TRUE)

        #Look at Ns by ego and dyad race
        table(sample$race_eth_m[!is.na(sample$pship_age_main)])
        table(sample$dyad_race_m[!is.na(sample$pship_age_main)])

        #Look at association with race and region
            #To look if it varies by dyad race, use var dyad_race_m instead of race_eth_m
            lm.pship_age_main <- lm(pship_age_main ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, data=sample[!is.na(sample$race_eth_m.reg),])
            summary(lm.pship_age_main)
            
            #one way to test joint significance - another possible way: regTermTest in survey package
                lm.pship_age_main.noint <- lm(pship_age_main ~ race_eth_m.reg + region.bKC, data=sample[!is.na(sample$race_eth_m.reg),])
                lrtest(lm.pship_age_main, lm.pship_age_main.noint)
                
                lm.pship_age_main.noregion <- lm(pship_age_main ~ race_eth_m.reg, data=sample[!is.na(sample$race_eth_m.reg),])
                lrtest(lm.pship_age_main.noint, lm.pship_age_main.noregion)
                
                lm.pship_age_main.norace <- lm(pship_age_main ~ region.bKC, data=sample[!is.na(sample$race_eth_m.reg),])
                lrtest(lm.pship_age_main.noint, lm.pship_age_main.norace)
    
        #Calculate mean given by observed median based on exponential distribution
            sample %>%
                filter(!is.na(pship_age_main) & !is.na(dyad_race_m)) %>%
                group_by(dyad_race_m) %>%
                summarise(n=n(), median = median(pship_age_main, na.rm=TRUE)) %>%
                mutate(exp_mean = median/log(2), rate=1/exp_mean)
            
    #Persistent
        #Overall
        summary(sample$pship_age_pers, na.rm=TRUE)

        #plot with exponential overlay
        hist(sample$pship_age_pers, probability=TRUE, breaks=33, main ="Age of persistent partnerships")
        curve(dexp(x, rate = log(2)/(median(sample$pship_age_pers, na.rm=TRUE))), col = 2, lty = 2, lwd = 2, add = TRUE)
        
        #Look at Ns by ego and dyad race
        table(sample$race_eth_m[!is.na(sample$pship_age_pers)])  
        table(sample$dyad_race_m[!is.na(sample$pship_age_pers)])
        
        #Look at association with race and region
            #To look if it varies by dyad race, use var dyad_race_m instead of race_eth_m
            lm.pship_age_pers <- lm(pship_age_pers ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, data=sample[!is.na(sample$race_eth_m.reg),])
            summary(lm.pship_age_pers)
        
            #one way to test joint significance - another possible way: regTermTest in survey package
                lm.pship_age_pers.noint <- lm(pship_age_pers ~ race_eth_m.reg + region.bKC, data=sample[!is.na(sample$race_eth_m.reg),])
                lrtest(lm.pship_age_pers, lm.pship_age_pers.noint)
                
                lm.pship_age_pers.noregion <- lm(pship_age_pers ~ race_eth_m.reg, data=sample[!is.na(sample$race_eth_m.reg),])
                lrtest(lm.pship_age_pers.noint, lm.pship_age_pers.noregion)
                
                lm.pship_age_pers.norace <- lm(pship_age_pers ~ region.bKC, data=sample[!is.na(sample$race_eth_m.reg),])
                lrtest(lm.pship_age_pers.noint, lm.pship_age_pers.norace)
        
        #Calculate mean given by observed median based on exponential distribution
        sample %>%
            filter(!is.na(pship_age_pers) & !is.na(dyad_race_m)) %>%
            group_by(dyad_race_m) %>%
            summarise(n=n(), median = median(pship_age_pers, na.rm=TRUE)) %>%
            mutate(exp_mean = median/log(2), rate=1/exp_mean)
        
#Proportion never tested by age ... 40?
    ggplot(evertested, aes(x=age, y=prop)) + geom_point(shape=16, size=3) + geom_smooth(method="loess") +
            plot_background + theme_title +
            labs(x="Age", y="Percent ever tested", title="Percent ever tested by age")        
    
    sample_18to64 %>%
        filter(!is.na(evertest_r) & !is.na(age_cat2) & !is.na(race_eth_m)) %>%
        group_by(race_eth_m, age_cat2) %>%
        summarise(n=n(), prop = mean(evertest_r, na.rm=TRUE))
    
    sample_18to64 %>%
        filter(!is.na(evertest_r) & !is.na(age_cat2)) %>%
        group_by(age_cat2) %>%
        summarise(n=n(), prop = mean(evertest_r, na.rm=TRUE))
    
#Last test interval
    #overall
    summary(sample$iti)
    hist(sample$iti)
    
    #Number non-missing by race
    table(sample$race_eth_m[!is.na(sample$iti)])
    
    #Look at distribution
    ggplot(sample, aes(x=iti)) + geom_histogram(binwidth=30) + 
        geom_vline(xintercept=mean(sample$iti, na.rm=TRUE), color="blue") + 
        geom_vline(xintercept=median(sample$iti, na.rm=TRUE), color="red") +
        labs(title="Intertest interval")
        #by race
        ggplot(sample, aes(x=iti)) + geom_histogram(binwidth=30) + 
            facet_grid(race_eth_m~.) +
            geom_vline(xintercept=mean(sample$iti, na.rm=TRUE), color="blue") + 
            geom_vline(xintercept=median(sample$iti, na.rm=TRUE), color="red") +
            labs(title="Intertest interval")
    
    #Look for diffs by race and region
    lm.iti <- lm(iti ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
    summary(lm.iti)
    
        #one way to test joint significance - another possible way: regTermTest in survey package
        lm.iti.noint <- lm(iti ~ race_eth_m.reg + region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
        lrtest(lm.iti, lm.iti.noint)
        
        lm.iti.noregion <- lm(iti ~ race_eth_m.reg, data=sample[!is.na(sample$race_eth_m.reg), ])
        lrtest(lm.iti.noint, lm.iti.noregion)
        
        lm.iti.norace <- lm(iti ~ region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
        anova(lm.iti.noint, lm.iti.norace)
        
    #Plots by race (and region?)
    itiXrace <- sample %>% group_by(race_eth_m) %>% filter(!is.na(race_eth_m) & !is.na(iti)) %>%
                summarise(mean = mean(iti, na.rm=TRUE), median=median(iti, na.rm=TRUE))
    ggplot(itiXrace, aes(x=race_eth_m, y=median)) + geom_point(shape=16, size=3) + 
        darken_color + plot_background + theme_title +
        scale_x_discrete(labels=c("Hispanic \n (n=113)", "Black \n (n=38)", "Other \n (n=392)")) +
        labs(x="Race/ethnicity", y="Intertest interval", title="Median intertest interval by race/ethnicity") 
        
    itiXraceregion <- sample %>% group_by(race_eth_m, region) %>% filter(!is.na(race_eth_m) & !is.na(iti) & !is.na(region)) %>%
        summarise(mean = mean(iti, na.rm=TRUE), median=median(iti, na.rm=TRUE))
    ggplot(itiXraceregion, aes(x=race_eth_m, y=median, color=region)) + geom_point(shape=16, size=3) + 
        scale_color_brewer(type="qual", palette = 2) + plot_background + theme_title +
        scale_x_discrete(labels=c("Hispanic \n (n=113)", "Black \n (n=38)", "Other \n (n=392)")) +
        labs(x="Race/ethnicity", y="Intertest interval", title="Median intertest interval by race/ethnicity and region")
        
    #Get mean implied by sample median under exponential distribution
    sample %>%
        filter(!is.na(iti) & !is.na(race_eth_m)) %>%
        group_by(race_eth_m) %>%
        summarise(n=n(), median = median(iti, na.rm=TRUE)) %>%
        mutate(exp_mean = median/log(2))
    
#Coital frequency
    #Look at Ns by race
    table(sample$race_eth_m[!is.na(sample$airate_main)])
    table(sample$dyad_race_m[!is.na(sample$airate_main)])
    
    table(sample$race_eth_m[!is.na(sample$airate_pers)])
    table(sample$dyad_race_m[!is.na(sample$airate_pers)])
    
    #Look at distribution
    ggplot(sample, aes(x=airate_main)) + geom_histogram(binwidth=0.01) + 
        geom_vline(xintercept=mean(sample$airate_main, na.rm=TRUE), color="blue") + 
        geom_vline(xintercept=median(sample$airate_main, na.rm=TRUE), color="red") +
        labs(title="Coital frequency in main partnerships")
        
        #by region
        ggplot(sample, aes(x=airate_main)) + geom_histogram(binwidth=0.01) + 
            facet_grid(region ~.) + theme_title +
            labs(x="Coital frequency", y="Frequency", title="Coital frequency in main partnerships by region")
        
    ggplot(sample, aes(x=airate_pers)) + geom_histogram(binwidth=0.01) + 
        geom_vline(xintercept=mean(sample$airate_pers, na.rm=TRUE), color="blue") + 
        geom_vline(xintercept=median(sample$airate_pers, na.rm=TRUE), color="red") +
        labs(title="Coital frequency in persistent partnerships")
        
        #by region
        ggplot(sample, aes(x=airate_pers)) + geom_histogram(binwidth=0.01) + 
            facet_grid(region~.) + theme_title +
            labs(x="Coital frequency", y="Frequency", title="Coital frequency in persistent partnerships by region")
        
    #See if it differs by race and region
        #to look by dyad race, use var dyad_race_m instead of race_eth_m
        lm.airate_main <- lm(airate_main ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
        summary(lm.airate_main)
        
            #one way to test joint significance - another possible way: regTermTest in survey package
            lm.airate_main.noint <- lm(airate_main ~ race_eth_m.reg + region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
            lrtest(lm.airate_main, lm.airate_main.noint)
            
            lm.airate_main.noregion <- lm(airate_main ~ race_eth_m.reg, data=sample[!is.na(sample$race_eth_m.reg), ])
            lrtest(lm.airate_main.noint, lm.airate_main.noregion)
            
            lm.airate_main.norace <- lm(airate_main ~ region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
            lrtest(lm.airate_main.noint, lm.airate_main.norace)
            
        lm.airate_pers <- lm(airate_pers ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
        summary(lm.airate_pers)
        
            #one way to test joint significance - another possible way: regTermTest in survey package
            lm.airate_pers.noint <- lm(airate_pers ~ race_eth_m.reg + region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
            lrtest(lm.airate_pers, lm.airate_pers.noint)
            
            lm.airate_pers.noregion <- lm(airate_pers ~ race_eth_m.reg, data=sample[!is.na(sample$race_eth_m.reg), ])
            lrtest(lm.airate_pers.noint, lm.airate_pers.noregion)
            
            lm.airate_pers.norace <- lm(airate_pers ~ region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
            lrtest(lm.airate_pers.noint, lm.airate_pers.norace)
            
    #Look at mean and median by dyad race
    sample %>% 
        filter(!is.na(sample$dyad_race_m) & !is.na(sample$airate_main)) %>%
        group_by(dyad_race_m) %>% 
        summarise(n=n(), mean = mean(airate_main, na.rm=TRUE), median=median(airate_main, na.rm=TRUE))
    sample %>% 
        filter(!is.na(sample$dyad_race_m) & !is.na(sample$airate_pers)) %>%
        group_by(dyad_race_m) %>% 
        summarise(n=n(), mean = mean(airate_pers, na.rm=TRUE), median=median(airate_pers, na.rm=TRUE))
    
    #Plot by race and region
        airate_mainXrace <- sample %>% filter(!is.na(dyad_race_m) & !is.na(airate_main)) %>% group_by(dyad_race_m) %>% 
            summarise(mean = mean(airate_main, na.rm=TRUE), median=median(airate_main, na.rm=TRUE))
        plot_airate_main <- ggplot(airate_mainXrace, aes(x=dyad_race_m, y=mean)) + geom_point(shape=16, size=3) + 
            darken_color + plot_background + theme_title +
            scale_x_discrete(labels=c("HH \n (n=48)", "HB \n (n=8)", "HO \n (n=48)", "BB \n (n=5)", "BO \n (n=13)", "OO \n (n=164)")) +
            labs(x="Dyad race/ethnicity", y="Mean rate per day", title="Mean coital frequency in main partnerships")  + ylim(0, 0.3)
    
        airate_persXrace <- sample %>% filter(!is.na(race_eth_m) & !is.na(airate_pers)) %>% group_by(race_eth_m) %>% 
            summarise(mean = mean(airate_pers, na.rm=TRUE), median=median(airate_pers, na.rm=TRUE))
        plot_airate_pers <- ggplot(airate_persXrace, aes(x=race_eth_m, y=mean)) + geom_point(shape=16, size=3) + 
            darken_color + plot_background + theme_title +
            scale_x_discrete(labels=c("HH \n (n=10)", "HB \n (n=1)", "HO \n (n=18)", "BB \n (n=3)", "BO \n (n=25)", "OO \n (n=82)")) +
            labs(x="Dyad race/ethnicity", y="Mean rate per day", title="Mean coital frequency in persistent partnerships")  + ylim(0, 0.3)
        
    multiplot(plot_airate_main, plot_airate_pers, cols=1)
    
#Sex role
    table(sample$race_eth_m[!is.na(sample$position_cat)])
    prop.table(table(sample$race_eth_m, sample$position_cat), 1)
    
    #See if it varies by race/region
    #first define vars for yes/no top and yes/no bottom
        sample$position_top <- ifelse(sample$position_cat=="Exclusively top", 1, 
                                      ifelse(!is.na(sample$position_cat), 0, NA))
        sample$position_bottom <- ifelse(sample$position_cat=="Exclusively bottom", 1, 
                                      ifelse(!is.na(sample$position_cat), 0, NA))
    
    glm.top <- glm(position_top ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, family=binomial(link="logit"), data=sample[!is.na(sample$race_eth_m.reg),])
    summary(glm.top)
    
        #one way to test joint significance
        glm.top.noint <- glm(position_top ~ race_eth_m.reg + region.bKC, family=binomial(link="logit"), data=sample[!is.na(sample$race_eth_m.reg),])
        lrtest(glm.top, glm.top.noint)
        
        glm.top.noregion <- glm(position_top ~ race_eth_m.reg, family=binomial(link="logit"), data=sample[!is.na(sample$race_eth_m.reg),])
        lrtest(glm.top.noint, glm.top.noregion)
        
        glm.top.norace <- glm(position_top ~ region.bKC, family=binomial(link="logit"), data=sample[!is.na(sample$race_eth_m.reg),])
        lrtest(glm.top.noint, glm.top.norace)
        
    #Plot by race and region
    positionXrace <- (as.data.frame(prop.table(table(sample$race_eth_m, sample$position_cat), 1))) 
    ggplot(positionXrace, aes(x=Var1, y=Freq, color=Var2)) + 
        geom_point(shape=16, size=3) + plot_background + theme_title +
        scale_color_brewer(type="qual", palette = 2) +
        scale_x_discrete(labels=c("Hispanic \n (n=145", "Black \n (n=35)", "Other \n (n=436)")) +
        labs(x="Race/ethnicity", y="Percent", title="Sex role by race_ethnicity") 
    
    positionXraceregion <- sample %>% group_by(region, race_eth_m, position_cat) %>% summarise(n=n()) %>%
        filter(!is.na(race_eth_m) & !is.na(position_cat) & !is.na(region)) %>%
        group_by(region, race_eth_m) %>% mutate(proportion = n / sum(n))
    ggplot(positionXraceregion, aes(x=race_eth_m, y=proportion, color=region, shape=position_cat)) + 
        geom_point(size=3) + plot_background + theme_title +
        scale_color_brewer(type="qual", palette = 2) +
        scale_x_discrete(labels=c("Hispanic \n (n=145", "Black \n (n=35)", "Other \n (n=436)")) +
        labs(x="Race/ethnicity", y="Percent", title="Sex role by race_ethnicity and region")
    
#Condom use in negative/unknown status dyads
    #May want to restrict to data from ongoing partnerships (would need to redefine condoms_main, condoms_pers... above)
        
    #Look at Ns by race and dyad race
    table(sample$race_eth_m[!is.na(sample$condoms_main)])
    table(sample$dyad_race_m[!is.na(sample$condoms_main)])
    
    table(sample$race_eth_m[!is.na(sample$condoms_pers)])
    table(sample$dyad_race_m[!is.na(sample$condoms_pers)])
    
    table(sample$race_eth_m[!is.na(sample$condoms_inst)])
    table(sample$dyad_race_m[!is.na(sample$condoms_inst)])
    
    #See if it differs by race and region
    lm.condoms_main <- lm(condoms_main ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
    summary(lm.condoms_main)
    
        #one way to test joint significance - another possible way: regTermTest in survey package
        lm.condoms_main.noint <- lm(condoms_main ~ race_eth_m.reg + region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
        lrtest(lm.condoms_main, lm.condoms_main.noint)
        
        lm.condoms_main.noregion <- lm(condoms_main ~ race_eth_m.reg, data=sample[!is.na(sample$race_eth_m.reg), ])
        lrtest(lm.condoms_main.noint, lm.condoms_main.noregion)
        
        lm.condoms_main.norace <- lm(condoms_main ~ region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
        lrtest(lm.condoms_main.noint, lm.condoms_main.norace)
    
    lm.condoms_pers <- lm(condoms_pers ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
    summary(lm.condoms_pers)
        
        #one way to test joint significance - another possible way: regTermTest in survey package
        lm.condoms_pers.noint <- lm(condoms_pers ~ race_eth_m.reg + region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
        lrtest(lm.condoms_pers, lm.condoms_pers.noint)
        
        lm.condoms_pers.noregion <- lm(condoms_pers ~ race_eth_m.reg, data=sample[!is.na(sample$race_eth_m.reg), ])
        lrtest(lm.condoms_pers.noint, lm.condoms_pers.noregion)
        
        lm.condoms_pers.norace <- lm(condoms_pers ~ region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
        lrtest(lm.condoms_pers.noint, lm.condoms_pers.norace)
        
    
    lm.condoms_inst <- lm(condoms_inst ~ race_eth_m.reg + region.bKC + race_eth_m.reg*region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
    summary(lm.condoms_inst)
    
        #one way to test joint significance - another possible way: regTermTest in survey package
        lm.condoms_inst.noint <- lm(condoms_inst ~ race_eth_m.reg + region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
        lrtest(lm.condoms_inst, lm.condoms_inst.noint)
        
        lm.condoms_inst.noregion <- lm(condoms_inst ~ race_eth_m.reg, data=sample[!is.na(sample$race_eth_m.reg), ])
        lrtest(lm.condoms_inst.noint, lm.condoms_inst.noregion)
        
        lm.condoms_inst.norace <- lm(condoms_inst ~ region.bKC, data=sample[!is.na(sample$race_eth_m.reg), ])
        lrtest(lm.condoms_inst.noint, lm.condoms_inst.norace)
        
    #Look at mean and median by race_eth_m
    sample %>% 
        filter(!is.na(dyad_race_m) & !is.na(condoms_main)) %>%
        group_by(dyad_race_m) %>% 
        summarise(n=n(), mean = mean(condoms_main, na.rm=TRUE), median=median(condoms_main, na.rm=TRUE))
    sample %>% 
        filter(!is.na(dyad_race_m) & !is.na(condoms_pers)) %>%
        group_by(dyad_race_m) %>% 
        summarise(n=n(), mean = mean(condoms_pers, na.rm=TRUE), median=median(condoms_pers, na.rm=TRUE))
    sample %>% 
        filter(!is.na(dyad_race_m) & !is.na(condoms_inst)) %>%
        group_by(dyad_race_m) %>% 
        summarise(n=n(), mean = mean(condoms_inst, na.rm=TRUE), median=median(condoms_inst, na.rm=TRUE))
    
    #Plot by race and region
    condoms_mainXrace <- sample %>% filter(!is.na(dyad_race_m) & !is.na(condoms_main)) %>% group_by(dyad_race_m) %>% 
        summarise(n=n(), mean = mean(condoms_main, na.rm=TRUE), median=median(condoms_main, na.rm=TRUE))
    plot_condoms_main <- ggplot(condoms_mainXrace, aes(x=dyad_race_m, y=mean)) + geom_point(shape=16, size=3) + 
        darken_color + plot_background + theme_title +  ylim(0, 1) +
        scale_x_discrete(labels=c("HH \n (n=33)", "HB \n (n=8)", "HO \n (n=48)", "BB \n (n=4)", "BO \n (n=13)", "OO \n (n=146)")) +
        labs(x="Dyad race/ethnicity", y="Probability of using condoms", title="Condom use in main partnerships") 
    
    condoms_persXrace <- sample %>% filter(!is.na(dyad_race_m) & !is.na(condoms_pers)) %>% group_by(dyad_race_m) %>% 
        summarise(n=n(), mean = mean(condoms_pers, na.rm=TRUE), median=median(condoms_pers, na.rm=TRUE))
    plot_condoms_pers <- ggplot(condoms_persXrace, aes(x=dyad_race_m, y=mean)) + geom_point(shape=16, size=3) + 
        darken_color + plot_background + theme_title + ylim(0, 1) +
        scale_x_discrete(labels=c("HH \n (n=11)", "HB \n (n=1)", "HO \n (n=19)", "BB \n (n=3)", "BO \n (n=17)", "OO \n (n=73)")) +
        labs(x="Dyad race/ethnicity", y="Probability of using condoms", title="Condom use in persistent partnerships") 
    
    condoms_instXrace <- sample %>% filter(!is.na(dyad_race_m) & !is.na(condoms_inst)) %>% group_by(dyad_race_m) %>% 
        summarise(n=n(), mean = mean(condoms_inst, na.rm=TRUE), median=median(condoms_inst, na.rm=TRUE))
    plot_condoms_inst <- ggplot(condoms_instXrace, aes(x=dyad_race_m, y=mean)) + geom_point(shape=16, size=3) + 
        darken_color + plot_background + theme_title + ylim(0, 1) +
        scale_x_discrete(labels=c("HH \n (n=8)", "HB \n (n=6)", "HO \n (n=33)", "BB \n (n=1)", "BO \n (n=7)", "OO \n (n=54)")) +
        labs(x="Dyad race/ethnicity", y="Probability of using condoms", title="Condom use in instantaneous partnerships") 
    
    multiplot(plot_condoms_main, plot_condoms_pers, plot_condoms_inst, cols=1 )
    
#PrEP adherence
    table(sample$race_eth_m[!is.na(sample$prep_adherence_gp)])
    table(sample$race_eth_m, sample$prep_adherence_gp)
    prop.table(table(sample$race_eth_m, sample$prep_adherence_gp), 1)
    
#PrEP coverage
    table(sample$prepelig_r[!is.na(sample$prep_use_r)])
    prop.table(table(sample$prepelig_r, sample$prep_use_r), 1)
    
    #Among "Other" men in KC
    table(sample$prepelig_r[!is.na(sample$prep_use_r) & sample$region=="King County" & sample$race_eth_m=="Other"])
    prop.table(table(sample$prepelig_r[sample$region=="King County" & sample$race_eth_m=="Other"], 
                     sample$prep_use_r[sample$region=="King County" & sample$race_eth_m=="Other"]), 1)
   
    #log-binomial regression
    install.packages("logbin")
    library("logbin")
    
    logbin.prep <- logbin(prep_use_curr ~ race_eth_m.reg + region.bKC, data=sample, subset=prepelig_r=="Recommend")
    summary(logbin.prep)
    rr <- exp(logbin.prep$coefficients)
    
    