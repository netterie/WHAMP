##Load data on population composition from WA census and define variables-------------------------------------------------       
    #Source: Small Area Demographic Estimates (SADE) by Age, Sex, Race and Hispanic Origin
    # Washington State Office of Financial Management, Forecasting and Research Division
    # Version: 20161203_R03_VM)

detach(package:plyr)
wa_census <- read.csv("Washington population.csv", header=TRUE, sep=",")
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
