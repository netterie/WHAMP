#calculate the % of men ages 15+ in each race group from WA census data

detach(package:plyr)
wa_census <- read.csv("/Users/darcywhite/Documents/UW/Dissertation/Network modeling/Internet survey analysis/Washington pop by county.csv", header=TRUE, sep=",")
wa_census2016_15older <- filter(wa_census, Year=="2016" & !is.na(Year) 
                             & !(Age.Group %in% c("0-4", "5-9", "10-14", "Total")) #c("15-19", "20-24", "25-29", "30-34", "35-39" ) #,"40-44", "45-49", "50-54", "55-59")
                             & !is.na(Age.Group))
wa_census2016_males15older_factor <- select(wa_census2016_15older, Area.Name, Male, Age.Group, Year, Hispanic.Male, Non.Hispanic.White.Male,
                                         Non.Hispanic.Black.Male, Non.Hispanic.AIAN.Male, Non.Hispanic.Asian.Male,
                                         Non.Hispanic.NHOPI.Male, Non.Hispanic.Two.or.More.Races.Male, Non.Hispanic.Male)

#Convert pop counts from factor to numeric
wa_census2016_males15older <- wa_census2016_males15older_factor
rm(wa_census2016_males15older_factor)

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
wa_census2016_males15older <- mutate_at(wa_census2016_males15older, .vars = c("Male", "Hispanic.Male", "Non.Hispanic.White.Male",
                                                                        "Non.Hispanic.Black.Male", "Non.Hispanic.AIAN.Male", "Non.Hispanic.Asian.Male",
                                                                        "Non.Hispanic.NHOPI.Male", "Non.Hispanic.Two.or.More.Races.Male", "Non.Hispanic.Male"),
                                     .funs = factor2numeric)

#Define "other' to include Native Hawaiian/Pacific Islander and Native Indian/Alaska Native 
wa_census2016_males15older$Non.Hispanic.Other.Male <- wa_census2016_males15older$Non.Hispanic.NHOPI.Male + wa_census2016_males15older$Non.Hispanic.AIAN.Male


#Add up the numbers for each race overall
wa_males_total <- wa_census2016_males15older %>%
    filter(Area.Name %in% "Washington") %>%
    summarise(total.males = sum(Male),
              total.hispanic = sum(Hispanic.Male), 
              total.white = sum(Non.Hispanic.White.Male),
              total.black = sum(Non.Hispanic.Black.Male),
              total.asian = sum(Non.Hispanic.Asian.Male),
              total.other = sum(Non.Hispanic.Other.Male),
              total.multiple = sum(Non.Hispanic.Two.or.More.Races.Male))

#Calculate the % in each race group
wa_males_total$total.hispanic/wa_males_total$total.males
wa_males_total$total.white/wa_males_total$total.males
wa_males_total$total.black/wa_males_total$total.males
wa_males_total$total.asian/wa_males_total$total.males
wa_males_total$total.other/wa_males_total$total.males
wa_males_total$total.multiple/wa_males_total$total.males


