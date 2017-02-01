
#select all the parents
parent_types <- rawData %>% select(Grade,Caliper)
#find list of distinct parents 
parent_types <- distinct(parent_types)

#create data frame to hold slitter limitation information
allNoSlitter <- data.frame("Grade"=c(0),"Caliper"=c(0),"Width"=c(0),
                           "Plant"=c(0))
#iterate through each plant with slitter limiation
#slitterLimitPlants defined in User Inputs.R 
for (plant in slitterLimitPlants){
  #selects data from specified plant with slitter limitation
  #select only Grade, Caliper, Width, Plant
  noSlitter <- rawData %>% select (Grade, Caliper, Width, Plant)
  #select data only from Plant that has slitter limitation
  noSlitter <- noSlitter %>% filter (Plant == plant)
  #select only the distinct Grade, Caliper, Widths from that plant
  noSlitter <- noSlitter %>% distinct(Grade, Caliper, Width, Plant)
  #append to larger data frame
  allNoSlitter <- rbind(allNoSlitter,noSlitter)
}
#remove first row of zeros
allNoSlitter <- allNoSlitter[-c(1),]

#create data frame to hold options for all grade, caliper combinations
AllRollSelectionOptions <- data.frame("Grade" = 0, "Caliper" = 0, "Width" = 0, 
                                      "Parent" = 0, "# Parents" = 0,
                                      "Trim Tons" = 0, "Marginal Benefit"= 0)

# specify number of cores to use for parallel processing
#registerDoParallel(cores=3)
#print("Par Registered")

#for each unique parent roll type (Grade / Caliper), run optimization to generate options 
print("Selecting Parent Roll Assignments")
for (p in 1:nrow(parent_types)) {
  print(paste("Parent Roll Type ",p,": ",parent_types$Grade[p]," ",parent_types$Caliper[p], sep="" ))
  #filter to find data for one parent
  one_parent <- rawData %>% filter(Grade == parent_types$Grade[p] 
                                   & Caliper == parent_types$Caliper[p]) %>% 
    select(Grade, Caliper, Width, Tons) %>% 
    group_by(Grade, Caliper, Width) %>%
    summarise("Consumption" = sum(Tons))
  
  #list of potential parent rolls
  rWidth <- as.numeric(one_parent$Width)
  #list of products
  pWidth <- data.frame(width= rWidth, dmd=one_parent$Consumption)
  #grade
  one_parent_grade <- parent_types$Grade[p]
  #caliper
  one_parent_caliper <- parent_types$Caliper[p]
  #noSlitter subset 
  slitter_limits <- allNoSlitter %>% filter (Grade == one_parent_grade &
                                               Caliper == one_parent_caliper) %>% select (Width)
  #formatting
  slitter_limits <- as.data.frame(slitter_limits)
  #make into list 
  slitter_limits_list <- slitter_limits[,1]
  #run parent roll selection
  source("mod_R/Parent Roll Selection Model.R")
  #add roll selection options to list of all roll selection options
  AllRollSelectionOptions <- rbind(AllRollSelectionOptions,
                                   RollSelectionOptions)
}

#formatting (gets rid of first row of zeros)
AllRollSelectionOptions <- AllRollSelectionOptions[-c(1),]
#for each parent roll, make selection order 1 for minimum number of parents
AllRollSelectionOptions <- AllRollSelectionOptions %>% 
  group_by(Grade,Caliper) %>% mutate("Selection Order"=
                                       ifelse(X..Parents==min(X..Parents),1,0))
#ungroup data frame
AllRollSelectionOptions <- AllRollSelectionOptions %>% ungroup(Grade,Caliper)

#select Grade, Caliper, # Parent, Marginal benefit
numOptions <- AllRollSelectionOptions %>% 
  select(Grade,Caliper,X..Parents,Marginal.Benefit)
#find distinct number of these columns, represents number of options remaining 
#apart from minimum
numOptions <-distinct(numOptions)
#select Grade, Caliper, and #Parent from the options to create a reference on  
#how many have been selected so far
optionsIndex <- numOptions[,c(1,2,3)]
#take out the minimum # of parents because that already has Selection Order 1 
optionsIndex <- optionsIndex %>% group_by(Grade,Caliper) %>% 
  filter(X..Parents==min(X..Parents))
#ungroup data frame
optionsIndex <- optionsIndex %>% ungroup(Grade,Caliper)
#rename column
names(optionsIndex)[3] <- "Parents Selected"
#number of options remaining
totalOptions <- nrow(numOptions)-nrow(optionsIndex)
#calculates the minimum number of parent rolls we can stock
minPRolls <- sum(optionsIndex$`Parents Selected`)

#for the rest of the selection, starts with 2 because we already did 1 
for (x in 2:(totalOptions+1)){
  #creates a empty data frame to list all of our next options
  nextOptions <- data.frame("Grade"=c(rep_len(0,nrow(optionsIndex))), 
                            "Caliper" = c(rep_len(0,nrow(optionsIndex))), 
                            "Number Parents" = c(rep_len(0,nrow(optionsIndex))), 
                            "Marginal Benefit" = c(rep_len(0,nrow(optionsIndex))))
  #for each Grade, Caliper combination
  for (y in 1:nrow(optionsIndex)){
    #grade
    grade <- optionsIndex$Grade[y]
    #caliper
    caliper <- optionsIndex$Caliper[y]
    #how many we have already selected of this Grade, Caliper
    currentNumber <- optionsIndex$`Parents Selected`[y]
    #filter the options list to fit these criteria
    option <- numOptions %>% filter (Grade==grade,Caliper==caliper,
                                     X..Parents==currentNumber+1)
    #if there aren't any option for this criteria (i.e. we selected all 
    #available widths) then go to next Grade, Caliper combination
    if (nrow(option)==0) {next}
    #if there is an option with this criteria, add it to the list
    nextOptions[y,] <- option
  }
  #select the next option with the largest marginal benefit
  selected <- nextOptions %>% filter(Marginal.Benefit==max(Marginal.Benefit))
  #if there is more than one option, choose the one with the larger parent #
  selected <- if(nrow(selected)>1) (selected %>% filter(Number.Parents==
                                                          max(Number.Parents))) else selected
  #if still more than one option, choose the one with the larger caliper
  selected <- if(nrow(selected)>1) (selected %>% 
                                      filter(Caliper==max(Caliper))) else selected 
  #if still more than one option, chosse the one with larger Grade 
  selected <- if(nrow(selected)>1) (selected %>% 
                                      filter(Grade==max(Grade))) else selected
  #filter options list to show only selected option
  optionsIndex <- optionsIndex %>% mutate (`Parents Selected` = ifelse
                                           (optionsIndex$Grade==selected$Grade & 
                                               optionsIndex$Caliper==selected$Caliper,
                                             selected$Number.Parents,
                                             optionsIndex$`Parents Selected`))
  #add selection order to the option that has been selected
  AllRollSelectionOptions <- AllRollSelectionOptions %>% 
    mutate("Selection Order"= 
             ifelse(AllRollSelectionOptions$Grade==selected$Grade &
                      AllRollSelectionOptions$Caliper==selected$Caliper & 
                      AllRollSelectionOptions$X..Parents==selected$Number.Parents,
                    x,AllRollSelectionOptions$`Selection Order`))
}

#create data frame to hold costs of each selection #  
rollSelectionCost<-data.frame("Number of Parents"=c(rep_len(0,totalOptions+1)),
                              "Total Trim" = c(rep_len(0,totalOptions+1)))
#identify first selection
firstSelection <- AllRollSelectionOptions %>% 
  filter(`Selection Order`==1)
firstSelection <- firstSelection %>% distinct(Grade,Caliper,Trim.Tons)
#sum tons in first selection
firstSelection <- sum(firstSelection$Trim.Tons)
#add this selection to rollSelectionCost
rollSelectionCost$Number.of.Parents[1] <- 1 
rollSelectionCost$Total.Trim[1] <- firstSelection

#for selections above 1 through all the available selections
for (t in 1:totalOptions+1) { 
  #select everything 1-t selections
  upToSelection <- AllRollSelectionOptions %>% filter(`Selection Order`<=t) %>% 
    distinct(Grade,Caliper,`Selection Order`,
             Trim.Tons)
  #choose only top selection order # per Grade, Caliper
  upToSelection <- upToSelection %>% group_by (Grade,Caliper) %>% 
    filter(`Selection Order`==max(`Selection Order`))
  #sum tons in t selection
  costSelection <- sum(upToSelection$Trim.Tons)
  #add t selection to rollSelectionCost
  rollSelectionCost$Number.of.Parents[t] <- t 
  rollSelectionCost$Total.Trim[t] <- round(costSelection, digits = 2)
}

## Save low cost assignemnt if needed
if (optimalRun) {
  print("Saving Best Assignment")
  t <- as.numeric(skuTargets[mill])-minPRolls + 1
  targetRollSelectionOption <- AllRollSelectionOptions %>% filter(`Selection Order`<=t) %>% group_by (Grade,Caliper) %>% 
    filter(`Selection Order`==max(`Selection Order`))
  write.csv(targetRollSelectionOption, rollSelectionAssignOutputPath, quote = FALSE)  
}


#change to reflect # parents not selection order
rollSelectionCost <- rollSelectionCost %>% 
  mutate("Number.of.Parents"=Number.of.Parents + minPRolls -1)
#calculate maximum number of rolls possible
maxPRolls <- tail(rollSelectionCost, n=1)$Number.of.Parents

#plot #parents vs. total trim
plot(rollSelectionCost$Number.of.Parents, rollSelectionCost$Total.Trim, 
     type = "p", main = "Roll Size Selection", xlab = "Number of Parent Rolls",
     ylab = "Trim Tons", col = "green", bg = "green", pch =16)

#save rollSelectionCost to csv
#path defined in User Inputs.R 
write.csv(rollSelectionCost, rollSelectionOutputPath , quote = FALSE)