#output used is AllRollSelectionOptions dataframe


# After pre-run, run only for optimal count, to save parent roll assignment
if (optimalRun) {
  scenCountList <- c(as.numeric(skuTargets[mill]))
} else if (maxPRolls>300) {  
  scenCountList <- c(seq(minPRolls, maxPRolls/2, parentNIncrement),maxPRolls)
} else {
  scenCountList <- seq(minPRolls, maxPRolls, parentNIncrement) # 
}  #seq(minPRolls, maxPRolls, parentNIncrement)

#define length based on number of scenarios (min to max # parent rolls)
#maxPRolls and minPRolls comes from Data Read In.R (lines 184 and 99)
dfLength <- maxPRolls-minPRolls+1 

#set up empty data frame to hold results, one line per scenario 
results <- data.frame("Mill" = c(rep_len(mill,dfLength)),
                  "Number of Parent Rolls" = c(rep_len(0,dfLength)),
                  "Avg On Hand Inv" = c(rep_len(0,dfLength)),
                  "Stabilized Avg" = c(rep_len(0,dfLength)),
                  "Max On Hand Inv" = c(rep_len(0,dfLength)),
                  "Stabilized Max" = c(rep_len(0,dfLength)),
                  "Avg Backorders" = c(rep_len(0,dfLength)),
                  "Tons Backordered" = c(rep_len(0,dfLength)),
                  "Days with Backorders" = c(rep_len(0,dfLength)),
                  "Tons Consumed" = c(rep_len(0,dfLength)), 
                  "Paper Making Trim" = c(rep_len(0,dfLength)), 
                  "AverageTrimPercentage" = c(rep_len(0,dfLength)),
                  "Side Roll Trim" = c(rep_len(0,dfLength)),
                  "Converting Trim Tons" = c(rep_len(0,dfLength)),
                  "Paper Machine Cost" = c(rep_len(0,dfLength)),
                  "Roll Trim Cost" = c(rep_len(0,dfLength)))
  
  
  
  
# specify number of cores to use for parallel processing
#registerDoParallel(cores=3)
#print("Par Registered")

#iterate through max number of parent rolls
print("Running Scenarios")
for (x in scenCountList) { 
  print(paste("Scenario: N=", x, sep=""))
  #define number of parent rolls
  numParentRolls <- x 
  #runs combining parent rolls which runs parent roll iterations and single
    #inventory simulation
  source("mod_R/Parent Roll Simulations.R")
  #sets number of parent rolls 
  results$Number.of.Parent.Rolls[x-minPRolls+1] <- numParentRolls
  #sets average on hand, from combining parent rolls (line 46)
  results$Avg.On.Hand.Inv[x-minPRolls+1] <- avg_on_hand
  #sets average backorders, from combining parent rolls (line 60)
  results$Avg.Backorders[x-minPRolls+1] <- avg_backorders
  #sets number of days with backorder, from combining parent rolls (line 68)
  results$Days.with.Backorders[x-minPRolls+1] <- days_backorders
  #sets max inventory on hand level, from combining parent rolls (line 64)
  results$Max.On.Hand.Inv[x-minPRolls+1] <- max_on_hand
  #sets total tons backordered during simulation, from combining parent 
    #rolls (line 71)
  results$Tons.Backordered[x-minPRolls+1] <- tons_backordered
  #sets tons consumed before converting, from combining parent rolls (line 73)
  results$Tons.Consumed[x-minPRolls+1] <- tons_ordered
  #sets cost of tons trimmed at converting, from parent roll iterations 
    #(line 118)
  results$Roll.Trim.Cost[x-minPRolls+1] <- totalRollTrimCost
  #sets average on hand inventory after stabilizationPeriod (line 49)
  results$Stabilized.Avg[x-minPRolls+1]<- stabilized_avg
  #sets max on hand inventory after stabilizationPeriod (line 57)
  results$Stabilized.Max[x-minPRolls+1]<- stabilized_max
  #runs Paper Machine Trim.R if needed
  if (runPMTrim) {
    source("mod_R/Paper Machine Trim.R") 
    #sets total trim tons from paper machine roll, Paper Machine Trim.R line 55
    results$Paper.Making.Trim[x-minPRolls+1] <- paperTrimTons
    #sets avg trim % from paper machine roll, Paper Machine Trim.R line 53 
    results$AverageTrimPercentage[x-minPRolls+1] <- avgTrimPercentage
    #sets tons from side roll trim, Paper Machine Trim.R line 57
    results$Side.Roll.Trim[x-minPRolls+1]<-sideRollTrimTons
    #sets cost of trim from paper machine roll, Paper Machine Trim.R line 59
    results$Paper.Machine.Cost[x-minPRolls+1]<- machineTrimCost
    }
  #move converting trim tons to results frame
  results$Converting.Trim.Tons[x-minPRolls+1]<-rollSelectionCost$Total.Trim[x-minPRolls+1]
  }

#plots number of parent rolls versus average on hand inventory level 
# plot(ss_costs$Number.of.Parent.Rolls, ss_costs$Avg.On.Hand.Inv, 
#      type = "l", main = "Number of Parent Rolls and Estimated Inventory", 
#      xlab = "Number of Parent Rolls", ylab = "Average On Hand Inventory", 
#      col = "red", bg = "green", pch =16)

# Write sim results if needed 
if (optimalRun) {
  write.csv(final_output, simResultsPath, quote = FALSE)  
            # Alternatively save combined_output for aggregated data set
} else {
  #export results data frame to csv
  #file path define in User Inputs.R 
  write.csv(results, resultsPath, quote = FALSE)
}