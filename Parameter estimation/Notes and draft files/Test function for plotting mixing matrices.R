# Mixing plot function

# mm is a mixing matrix with counts in the cells

mixplot <- function(mm){
    # Calculated alphaij terms (observed / expected cell counts)
        nXn <- dim(mm)
        alphaij <- matrix(rep(NA, nXn[1]*nXn[2]), nrow = nXn[1])
        for(i in 1:nXn[1]){
            for(j in 1:nXn[2]){
                alphaij[i,j] <- mm[i,j] / ((sum(mm[i,])*sum(mm[,j]))/sum(mm))
            }
        }
        alphaij <- as.data.frame(alphaij, row.names = rownames(mm))
        names(alphaij) <- names(mm)
        
        # Convert to long format
        alphaij <- cbind.data.frame("ego" = rownames(alphaij), alphaij)
        alphaij_l <- melt(alphaij, id="ego")
        alphaij_l$log_alphaij <- log(alphaij_l$value)
        colnames(alphaij_l)[colnames(alphaij_l)=="variable"] <- "alter"
        
    # Observed mixing matrix as conditional row probabilities
        probs <- rbind.data.frame(mm[1, ]/sum(mm[1,]), mm[2,]/sum(mm[2,]), mm[3,]/sum(mm[3,]))
    
        # Convert to long format
        probs <- cbind.data.frame("ego" = rownames(probs), probs)
        probs_l <- melt(probs, id="ego")
        colnames(probs_l)[colnames(probs_l)=="variable"] <- "alter"
        
    # Combined object to plot
        mm_plot <- cbind.data.frame(probs_l, alphaij = alphaij_l$value, log_alphaij = alphaij_l$log_alphaij)
        
        # Add column with proportion as percent to plot
        mm_plot$label <- paste0(round(mm_plot$value*100, 1),"%")
    
        # Define variable outline to outline the on-diagonal points
        mm_plot$ondiag <- ifelse(as.character(mm_plot$ego)==as.character(mm_plot$alter), 
                                 mm_plot$value, NA)
    
    # Reorder levels of alter factor so on-diagonal goes from left to right
        mm_plot$alter <- factor(mm_plot$alter, levels = rev(levels(mm_plot$ego)))
    
    # # Plot
    # plot <-  ggplot(mm_plot, aes(x=alter, y=ego)) + 
    #     geom_point(aes(size = value, color = alphaij)) + 
    #     geom_point(aes(size = ondiag), colour = "black", shape = 1) + 
    #     geom_text(aes(label = label), hjust=0.6, size=4) +
    #     scale_size_continuous(range = c(1,50), guide = "none") + 
    #     scale_colour_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 1, guide = "colourbar") + 
    #     labs(x="Alter", y="Ego", title="Racial/ethnic mixing row conditional probabilities: Persistent") + 
    #     plot_background + theme_title 
    
    # plot_2 <-  ggplot(mm_plot, aes(x=alter, y=ego)) + 
    #     geom_point(aes(size = alphaij, color = value)) + 
    #     geom_point(aes(size = ondiag), colour = "black", shape = 1) + 
    #     geom_text(aes(label = label), hjust=0.6, size=4) +
    #     scale_size_continuous(range = c(1,50), guide = "none") + 
    #     scale_colour_gradient(low = "white", high = "red", guide = "colourbar") + 
    #     labs(x="Alter", y="Ego", title="Racial/ethnic mixing row conditional probabilities: Persistent") + 
    #     plot_background + theme_title 
    
    return(mm_plot)
}

# test on age and race mixing ----

load(file="Data/race_mixing.Rdata")
load(file="Data/race_mixing_inst.Rdata")



## Race mixing
racemain <- mixplot(race_mixing$racemix_main_bal)
racepers <- mixplot(race_mixing$racemix_pers_bal)
raceinst <- mixplot(race_mixing_inst$racemix_inst_bal)

# Plots

# 1 size as value and no color coding
(racemain_plot1 <-  ggplot(racemain, aes(x=alter, y=ego)) + 
        geom_point(aes(size = value), color = "mediumpurple1") + 
        geom_point(aes(size = ondiag), colour = "black", shape = 1) + 
        geom_text(aes(label = label), hjust=0.6, size=4) +
        scale_size_continuous(range = c(1,50), guide = "none") + 
        scale_colour_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 0, guide = "colourbar") + 
        labs(x="Alter", y="Ego", title="Racial/ethnic mixing row conditional probabilities: Main") + 
        plot_background + theme_title)

# 1 size as value and alphaij as color
(racemain_plot1 <-  ggplot(racemain, aes(x=alter, y=ego)) + 
    geom_point(aes(size = value, color = log_alphaij)) + 
   # geom_point(aes(size = ondiag), colour = "black", shape = 1) + 
    geom_text(aes(label = label), hjust=0.6, size=4) +
    scale_size_continuous(range = c(1,50), guide = "none") + 
    scale_colour_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 0, guide = "colourbar") + 
    labs(x="Alter", y="Ego", title="Racial/ethnic mixing row conditional probabilities: Main") + 
    plot_background + theme_title)

(racemain_plot2 <-  ggplot(racemain, aes(x=alter, y=ego)) + 
    geom_point(aes(size = alphaij, color = value)) + 
    geom_text(aes(label = label), hjust=0.6, size=4) +
    scale_size_continuous(range = c(1,50), guide = "none") + 
    scale_colour_gradient(low = "white", high = "red", guide = "colourbar") + 
    labs(x="Alter", y="Ego", title="Racial/ethnic mixing row conditional probabilities: Main") + 
    plot_background + theme_title)

(racemain_plot3 <-  ggplot(racemain, aes(x=alter, y=ego)) + 
        geom_point(aes(color = alphaij), size = 30) + 
        geom_text(aes(label = label), hjust=0.6, size=4) +
        scale_size_continuous(range = c(1,50), guide = "none") + 
        scale_colour_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 1, guide = "colourbar") + 
        labs(x="Alter", y="Ego", title="Racial/ethnic mixing row conditional probabilities: Main") + 
        plot_background + theme_title)

(racepers_plot1 <-  ggplot(racepers, aes(x=alter, y=ego)) + 
        geom_point(aes(size = value, color = alphaij)) + 
        geom_point(aes(size = ondiag), colour = "black", shape = 1) + 
        geom_text(aes(label = label), hjust=0.6, size=4) +
        scale_size_continuous(range = c(1,50), guide = "none") + 
        scale_colour_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 1, guide = "colourbar") + 
        labs(x="Alter", y="Ego", title="Racial/ethnic mixing row conditional probabilities: Persistent") + 
        plot_background + theme_title)

(racepers_plot2 <-  ggplot(racepers, aes(x=alter, y=ego)) + 
        geom_point(aes(size = alphaij, color = value)) + 
        geom_text(aes(label = label), hjust=0.6, size=4) +
        scale_size_continuous(range = c(1,50), guide = "none") + 
        scale_colour_gradient(low = "white", high = "red", guide = "colourbar") + 
        labs(x="Alter", y="Ego", title="Racial/ethnic mixing row conditional probabilities: Persistent") + 
        plot_background + theme_title)

(raceinst_plot1 <-  ggplot(raceinst, aes(x=alter, y=ego)) + 
        geom_point(aes(size = value, color = alphaij)) + 
        geom_point(aes(size = ondiag), colour = "black", shape = 1) + 
        geom_text(aes(label = label), hjust=0.6, size=4) +
        scale_size_continuous(range = c(1,50), guide = "none") + 
        scale_colour_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 1, guide = "colourbar") + 
        labs(x="Alter", y="Ego", title="Racial/ethnic mixing row conditional probabilities: Instantaneous") + 
        plot_background + theme_title)

(raceinst_plot2 <-  ggplot(raceinst, aes(x=alter, y=ego)) + 
        geom_point(aes(size = alphaij, color = value)) + 
        geom_text(aes(label = label), hjust=0.6, size=4) +
        scale_size_continuous(range = c(1,50), guide = "none") + 
        scale_colour_gradient(low = "white", high = "red", guide = "colourbar") + 
        labs(x="Alter", y="Ego", title="Racial/ethnic mixing row conditional probabilities: Instantaneous") + 
        plot_background + theme_title)

## Age mixing

agemain <- mixplot(agemix_main_bal)
agepers <- mixplot(agemix_pers_bal)
ageinst <- mixplot(agemix_inst_bal)

# Plots
(agemain_plot1 <-  ggplot(agemain, aes(x=alter, y=ego)) + 
        geom_point(aes(size = value, color = alphaij)) + 
        geom_point(aes(size = ondiag), colour = "black", shape = 1) + 
        geom_text(aes(label = label), hjust=0.6, size=4) +
        scale_size_continuous(range = c(1,50), guide = "none") + 
        scale_colour_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 1, guide = "colourbar") + 
        labs(x="Alter", y="Ego", title="Age mixing row conditional probabilities: Main") + 
        plot_background + theme_title)

(agemain_plot2 <-  ggplot(agemain, aes(x=alter, y=ego)) + 
        geom_point(aes(size = alphaij, color = value)) + 
        geom_text(aes(label = label), hjust=0.6, size=4) +
        scale_size_continuous(range = c(1,50), guide = "none") + 
        scale_colour_gradient(low = "white", high = "red", guide = "colourbar") + 
        labs(x="Alter", y="Ego", title="Age mixing row conditional probabilities: Main") + 
        plot_background + theme_title)

(agepers_plot1 <-  ggplot(agepers, aes(x=alter, y=ego)) + 
        geom_point(aes(size = value, color = alphaij)) + 
        geom_point(aes(size = ondiag), colour = "black", shape = 1) + 
        geom_text(aes(label = label), hjust=0.6, size=4) +
        scale_size_continuous(range = c(1,50), guide = "none") + 
        scale_colour_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 1, guide = "colourbar") + 
        labs(x="Alter", y="Ego", title="Age mixing row conditional probabilities: Persistent") + 
        plot_background + theme_title)

(agepers_plot2 <-  ggplot(agepers, aes(x=alter, y=ego)) + 
        geom_point(aes(size = alphaij, color = value)) + 
        geom_text(aes(label = label), hjust=0.6, size=4) +
        scale_size_continuous(range = c(1,50), guide = "none") + 
        scale_colour_gradient(low = "white", high = "red", guide = "colourbar") + 
        labs(x="Alter", y="Ego", title="Age mixing row conditional probabilities: Persistent") + 
        plot_background + theme_title)

(ageinst_plot1 <-  ggplot(ageinst, aes(x=alter, y=ego)) + 
        geom_point(aes(size = value, color = alphaij)) + 
        geom_point(aes(size = ondiag), colour = "black", shape = 1) + 
        geom_text(aes(label = label), hjust=0.6, size=4) +
        scale_size_continuous(range = c(1,50), guide = "none") + 
        scale_colour_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 1, guide = "colourbar") + 
        labs(x="Alter", y="Ego", title="Age mixing row conditional probabilities: Instantaneous") + 
        plot_background + theme_title)

(ageinst_plot2 <-  ggplot(ageinst, aes(x=alter, y=ego)) + 
        geom_point(aes(size = alphaij, color = value)) + 
        geom_text(aes(label = label), hjust=0.6, size=4) +
        scale_size_continuous(range = c(1,50), guide = "none") + 
        scale_colour_gradient(low = "white", high = "red", guide = "colourbar") + 
        labs(x="Alter", y="Ego", title="Age mixing row conditional probabilities: Instantaneous") + 
        plot_background + theme_title)