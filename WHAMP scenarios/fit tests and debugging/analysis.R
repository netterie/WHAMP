##----------------------------------------------------------------
## Code to analyze simulation results and compare to input data ##
##----------------------------------------------------------------

#---SETUP
# Load simulation file
load(file="/homes/dpwhite/R/GitHub Repos/WHAMP/WHAMP scenarios/fit tests and debugging/sim.test.rda")

# Store number of steps as time variable
time <- 1:length(sim.test1$epi$num[,1])

#---Age

#--Race/ethnicity

#--Region

#---Prevalence
plot(time, sim.test1$epi$i.prev[,1], type="l", main = "Prevalence", xlab="Weeks", ylab="Prevalence")