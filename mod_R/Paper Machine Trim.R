#Trim Output# 
##run to export CSV file with information need for paper machine trim needs##

#remove unneccessary columns
#final_output is a result of Combining Parent Rolls.R 
trim_data <- final_output[,c(3,13,14,15,16)]

#sum consumption based on roll and cycle
trim_data <- aggregate(trim_data$consumption ~ 
              trim_data$parent_roll.Grade + 
              trim_data$parent_roll.Caliper + 
              trim_data$parent_roll.ParentW + 
              trim_data$cycle, data=trim_data, sum)

#rename column headers
names(trim_data)[1] <- "Parent Grade"
names(trim_data)[2] <- "Parent Caliper"
names(trim_data)[3] <- "Parent Width"
names(trim_data)[4] <- "Cycle" 
names(trim_data)[5] <- "Consumption"

#remove unecessary rows included previously for calculations
trim_data <- trim_data %>% filter(trim_data$Cycle > 0)
#calculate trim in terms of rolls 
#tonsPerRoll defined in User Inputs.R 
trim_data <- trim_data %>% mutate("Consumption" = 
                               trim_data$Consumption/(trim_data$`Parent Width`/inchesPerTon))

#add columns with cycle start and cycle end based on cycle number
#cycle_length defined in User Inputs.R
trim_data <- trim_data %>% mutate("Cycle Start" = start_date + 
                                 ((trim_data$Cycle-1)*cycle_length))
trim_data <- trim_data %>% mutate("Cycle End" = trim_data$`Cycle Start` 
                                  + cycle_length - 1)
#filter out cycles where no consumption occurs 
trim_data <- trim_data %>% filter(trim_data$Consumption >0)

#export to CSV file
#path defined in User Inputs.R 
write.csv(trim_data, pythonInputPath, quote = FALSE)
#run python script 
preTrimTime = Sys.time()
#model file name defined in User Inputs.R 
trimmedData <- system2("python", args=c(pythonModel, pythonInputPath, mill), stdout=TRUE)  ##, trim_data))
postTrimTime <- Sys.time()
print(postTrimTime - preTrimTime)
#For debugging, store original
trimmedDataDB <- trimmedData
# remove any ptintouts that weren't results
trimmedData <- trimmedData[str_count(trimmedData,",")==6]
#read in output from python script; convert from list to data frame format 
trimmedData <- ldply(strsplit(trimmedData,","))   
colnames(trimmedData) <- gsub(" ", ".", lapply(trimmedData[1, ], trimws))
trimmedData <- as_data_frame(trimmedData[-1,])
#convert number fields to numeric types from char
trimmedData$Inches.Trimmed <- as.numeric(trimmedData$Inches.Trimmed)
trimmedData$`%.Trimmed` <- as.numeric(trimmedData$`%.Trimmed`)
trimmedData$Side.Roll.Waste <- as.numeric(trimmedData$Side.Roll.Waste)
trimmedData$Cost<- as.numeric(trimmedData$Cost)


#file path defined in User Inputs.R
#trimmedData <- read.csv(pyTrimResults, head = TRUE, sep=",")   #Should be unnecessary with use of stdout above
#convert inches trimmed to tons trimmed
trimmedData <- trimmedData %>% mutate("Tons Trimmed" = 
                               trimmedData$Inches.Trimmed/inchesPerTon)
#convert side roll inches trimmed to tons trimmed
trimmedData <- trimmedData %>% mutate("Side Roll Tons Trimmed" = 
                             trimmedData$Side.Roll.Waste/inchesPerTon)
#calculates total tons trimmed
paperTrimTons <- sum(trimmedData$`Tons Trimmed`)
#calculates average trim percentage
totalTonsProduced = sum(trimmedData %>% filter(trimmedData$`Tons Trimmed`>0) %>% transmute(totalTons = Inches.Trimmed/`%.Trimmed`))
avgTrimPercentage <- paperTrimTons / totalTonsProduced
#calculates total tons trimmed as side rolls
sideRollTrimTons <- sum(trimmedData$`Side Roll Tons Trimmed`)
#calculates total cost of trim
machineTrimCost <- sum(trimmedData$Cost)

