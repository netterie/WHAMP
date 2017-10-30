##Compare black race in combination to black race alone and other (non-Hispanic) race##-------------

compare_race <- sample_18to64

#' Define a variable to distinguish men who identify as black in combination with other races from those 
#' who identify as black alone or other (non-Hispanic)
compare_race$blackmulti[compare_race$hbo %in% "Black"] <- "Black alone"
compare_race$blackmulti[compare_race$racemultiple>1 & compare_race$blackafricanamericanrace %in% 1] <- "Black multi"
compare_race$blackmulti[compare_race$hbo %in% "Other"] <- "Other"
compare_race$blackmulti[compare_race$hbo %in% "Hispanic"] <- NA

compare_race$mrp_blackmulti[compare_race$mrp_hbo %in% "Black"] <- "Black alone"
compare_race$mrp_blackmulti[compare_race$racemultiple_mrp>1 & compare_race$blackafricanamericanmrp_race %in% 1] <- "Black multi"
compare_race$mrp_blackmulti[compare_race$mrp_hbo %in% "Other"] <- "Other"
compare_race$mrp_blackmulti[compare_race$mrp_hbo %in% "Hispanic"] <- NA

compare_race$dyad_blackmulti[compare_race$blackmulti=="Black alone" & compare_race$mrp_blackmulti=="Black alone"] <- "BB"
compare_race$dyad_blackmulti[compare_race$blackmulti=="Black alone" & compare_race$mrp_blackmulti=="Black multi"] <- "BBm"
compare_race$dyad_blackmulti[compare_race$blackmulti=="Black multi" & compare_race$mrp_blackmulti=="Black alone"] <- "BBm"
compare_race$dyad_blackmulti[compare_race$blackmulti=="Black alone" & compare_race$mrp_blackmulti=="Other"] <- "BO"
compare_race$dyad_blackmulti[compare_race$blackmulti=="Other" & compare_race$mrp_blackmulti=="Black alone"] <- "BO"
compare_race$dyad_blackmulti[compare_race$blackmulti=="Black multi" & compare_race$mrp_blackmulti=="Black multi"] <- "BmBm"
compare_race$dyad_blackmulti[compare_race$blackmulti=="Black multi" & compare_race$mrp_blackmulti=="Other"] <- "BmO"
compare_race$dyad_blackmulti[compare_race$blackmulti=="Other" & compare_race$mrp_blackmulti=="Black multi"] <- "BmO"
compare_race$dyad_blackmulti[compare_race$blackmulti=="Other" & compare_race$mrp_blackmulti=="Other"] <- "OO"

#Look at N's by newly defined race group. Note Ns will be smaller for some vars due to missing data
table(compare_race$blackmulti)

#Compare vars
    #Degree distribution
    table(compare_race$blackmulti[!is.na(compare_race$deg_matrix)]) #look at Ns by race
    prop.table(table(compare_race$blackmulti, compare_race$deg_matrix), 1)
            
        deg_matrixXblackmulti <- compare_race %>% group_by(blackmulti, deg_matrix) %>% summarise(n=n()) %>%
            filter(!is.na(blackmulti) & !is.na(deg_matrix)) %>%
            group_by(blackmulti) %>% mutate(proportion = n / sum(n))
        
        ggplot(deg_matrixXblackmulti, aes(x=blackmulti, y=proportion, color=deg_matrix)) +
            geom_point(shape=16, size=3) + plot_background + theme_title +
            labs(x="Race", y="Proportion", title="Degree distribution by race")
    
    #Percent concurrent
    table(compare_race$blackmulti[!is.na(compare_race$concurrent)]) #look at Ns by race
    prop.table(table(compare_race$blackmulti, compare_race$concurrent), 1)
        
        concurrentXblackmulti <- (as.data.frame(prop.table(table(compare_race$blackmulti, compare_race$concurrent), 1))) 
        concurrentXblackmulti <- concurrentXblackmulti %>% filter(Var2==1) %>% mutate(definition = "Unrestricted")
        ggplot(concurrentXblackmulti, aes(x=Var1, y=Freq)) + 
            geom_point(shape=16, size=3) + darken_color + plot_background + theme_title +
            labs(x="Race", y="Percent concurrent", title="Concurrency by race") + ylim(0, 0.25)
        
    #Rate of instantaneous partnerships
    table(compare_race$blackmulti[!is.na(compare_race$rate_inst)]) #look at Ns by race
    compare_race %>% 
        filter(!is.na(deg_matrix) & !is.na(blackmulti)) %>%
        group_by(blackmulti, deg_matrix) %>% 
        summarize(mean(rate_inst, na.rm=TRUE), n())
        
        rate_inst_Xblackmulti <- compare_race %>% filter(!is.na(blackmulti) & !is.na(rate_inst)) %>% group_by(blackmulti) %>% 
            summarise(mean = mean(rate_inst, na.rm=TRUE), median=median(rate_inst, na.rm=TRUE), n=n())
        ggplot(rate_inst_Xblackmulti, aes(x=blackmulti, y=mean)) + geom_point(shape=16, size=3) + plot_background +
            labs(x="Race", y="Mean rate per day", title="Mean rate of instant partnerships by race") +
            theme_title + ylim(0, 0.025)
        
    #Selective mixing by race
    table(compare_race$blackmulti[!is.na(compare_race$mrp_blackmulti) & compare_race$mrp_blackmulti!="Dont know"], 
          compare_race$mrp_type_r[!is.na(compare_race$mrp_blackmulti) & compare_race$mrp_blackmulti!="Dont know"]) #look at Ns by race and mrp_type
    
        blackmulti_mixing <- compare_race %>% 
            filter(!is.na(blackmulti) & !is.na(mrp_blackmulti)) %>%
            group_by(blackmulti, mrp_blackmulti) %>% 
            summarise(n=n()) %>%
            group_by(blackmulti) %>% mutate(total_n = sum(n), proportion = n / sum(n))
        #Get proportion same race - first drop "Don't know" level
        blackmulti_samerace <- blackmulti_mixing %>% filter(blackmulti==mrp_blackmulti)    
        
       ggplot(blackmulti_samerace, aes(x=blackmulti, y=proportion)) + geom_col() + 
            plot_background + 
            labs(x="", y="Proportion same race", title="Proportion same race") + theme_title 
        
    #Selective mixing by age
    table(compare_race$blackmulti[!is.na(compare_race$sqrt_agediff) & !is.na(compare_race$mrp_blackmulti)]) #look at Ns by race
    compare_race %>%
       filter(!is.na(dyad_blackmulti) & !is.na(sqrt_agediff) & !is.na(mrp_type_r)) %>%
       group_by(dyad_blackmulti, mrp_type_r) %>%
       summarize(mean(sqrt_agediff, na.rm=TRUE), n())
   
       #Plot by race and region
       sqrt_agediffXrace <- compare_race %>% 
           filter(!is.na(dyad_blackmulti) & !is.na(sqrt_agediff) & !is.na(mrp_type_r)) %>% 
           group_by(dyad_blackmulti, mrp_type_r) %>%
           summarise(mean = mean(sqrt_agediff, na.rm=TRUE), median=median(sqrt_agediff, na.rm=TRUE), n=n())
       ggplot(sqrt_agediffXrace, aes(x=dyad_blackmulti, y=mean)) + geom_point(shape=16, size=3) + facet_grid(mrp_type_r~.) +
           darken_color + plot_background + theme_title +
           labs(x="Dyad race", y="Mean absolute difference", title="Mean absolute difference in sqrt of ages by race")
       
    #Last compare_race interval
    table(compare_race$blackmulti[!is.na(compare_race$iti)]) # Look at Ns by race
    
       ggplot(compare_race, aes(x=iti)) + geom_histogram(binwidth=30) + 
           facet_grid(blackmulti~.) +
           geom_vline(xintercept=mean(compare_race$iti, na.rm=TRUE), color="blue") + 
           geom_vline(xintercept=median(compare_race$iti, na.rm=TRUE), color="red") +
           labs(title="Intercompare_race interval")
       
    #Coital frequency
    table(compare_race$blackmulti[!is.na(compare_race$airate_main)])    #Look at Ns by race
    table(compare_race$dyad_blackmulti[!is.na(compare_race$airate_main)])
    
    table(compare_race$blackmulti[!is.na(compare_race$airate_pers)])
    table(compare_race$dyad_blackmulti[!is.na(compare_race$airate_pers)])
    
    compare_race %>% 
        filter(!is.na(compare_race$dyad_blackmulti) & !is.na(compare_race$airate_main)) %>%
        group_by(dyad_blackmulti) %>% 
        summarise(n=n(), mean = mean(airate_main, na.rm=TRUE), median=median(airate_main, na.rm=TRUE))
    compare_race %>% 
        filter(!is.na(compare_race$dyad_blackmulti) & !is.na(compare_race$airate_pers)) %>%
        group_by(dyad_blackmulti) %>% 
        summarise(n=n(), mean = mean(airate_pers, na.rm=TRUE), median=median(airate_pers, na.rm=TRUE))
    
    #Sex role
    table(compare_race$blackmulti[!is.na(compare_race$position_cat)]) #Look at Ns by race
    prop.table(table(compare_race$blackmulti, compare_race$position_cat), 1)
    
        positionXblackmulti <- (as.data.frame(prop.table(table(compare_race$blackmulti, compare_race$position_cat), 1))) 
        ggplot(positionXblackmulti, aes(x=Var1, y=Freq, color=Var2)) + 
            geom_point(shape=16, size=3) + plot_background + theme_title +
            scale_color_brewer(type="qual", palette = 2) +
            labs(x="Race", y="Percent", title="Sex role by race") 
    
    #Condom use in negative/unknown status dyads
    table(compare_race$blackmulti[!is.na(compare_race$condoms_main)])  #Look at Ns by race and dyad race
    table(compare_race$dyad_blackmulti[!is.na(compare_race$condoms_main)])
    
    table(compare_race$blackmulti[!is.na(compare_race$condoms_pers)])
    table(compare_race$dyad_blackmulti[!is.na(compare_race$condoms_pers)])
    
    table(compare_race$blackmulti[!is.na(compare_race$condoms_inst)])
    table(compare_race$dyad_blackmulti[!is.na(compare_race$condoms_inst)])
    
    #Look at mean and median by blackmulti
    compare_race %>% 
        filter(!is.na(dyad_blackmulti) & !is.na(condoms_main)) %>%
        group_by(dyad_blackmulti) %>% 
        summarise(n=n(), mean = mean(condoms_main, na.rm=TRUE), median=median(condoms_main, na.rm=TRUE))
    compare_race %>% 
        filter(!is.na(dyad_blackmulti) & !is.na(condoms_pers)) %>%
        group_by(dyad_blackmulti) %>% 
        summarise(n=n(), mean = mean(condoms_pers, na.rm=TRUE), median=median(condoms_pers, na.rm=TRUE))
    compare_race %>% 
        filter(!is.na(dyad_blackmulti) & !is.na(condoms_inst)) %>%
        group_by(dyad_blackmulti) %>% 
        summarise(n=n(), mean = mean(condoms_inst, na.rm=TRUE), median=median(condoms_inst, na.rm=TRUE))
    
    
       