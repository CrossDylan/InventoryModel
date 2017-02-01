#Parent Roll Iteration#
##run to pull in data and create simulation for each parent roll## 

#read the data into a dataframe 
#rawData pulled from Data Read In.R line 5
all_data <- rawData
#add column to record parent width 
all_data <- cbind(all_data, "ParentW" = c(rep_len(0,nrow(all_data))))
#format width type to be numeric 
all_data$Width <- as.numeric(all_data$Width)
#select information on rolls that are parents given number of parent rolls
#numParentRolls comes from Simulation Iterations.R line 35 
#minPRolls comes from Data Read In.R line 99
#filter on AllRollSelectionOptions which is output from Data Read In.R 
parentFinder <- AllRollSelectionOptions %>% filter(`Selection Order` <= 
                                                 numParentRolls - minPRolls +1)
#select largest selection order for each Grade, Caliper, Width
parentFinder <- parentFinder %>% group_by(Grade,Caliper,Width) %>% 
                            filter(`Selection Order` == max(`Selection Order`))
#add assigned parent roll to ParentW column
  # Perhaps the loop here can be avoided?

for(entry in 1:nrow(parentFinder)){
  all_data <- all_data %>% mutate("ParentW" = ifelse(all_data$Grade ==
              parentFinder$Grade[entry] & all_data$Caliper == 
              parentFinder$Caliper[entry] & all_data$Width == 
              parentFinder$Width[entry], parentFinder$Parent[entry],
              all_data$ParentW))
}

#order data by date
all_data_by_date <- arrange(all_data, Date)

#find start date
start_date <- as.Date(first(all_data_by_date$Date))

#find end date
end_date <- as.Date(last(all_data_by_date$Date)) 

#make list of unique parent rolls
parent_rolls <- all_data_by_date %>% distinct(Grade, Caliper, ParentW) %>% 
                      select(Grade, Caliper, ParentW) 

#count number of parent rolls
num_parents <- nrow(parent_rolls) 

#break into groups based on Grade and Caliper
production_groups <- all_data_by_date %>% group_by(Grade, Caliper) %>%
                      summarise(Tons = sum(Tons))

#for each group add up the demand 
production_demand <- sum(production_groups$Tons)

# Find average cycle length
avg_cycle_length = mean(cycle_length_list$cycle_length)

#find proportional amount of days product is produced based on demand
production_groups <- production_groups %>%
              mutate("Production Days" = 
              (Tons*avg_cycle_length)/production_demand)

#create cumulative sum of production days to indicate time during cycle 
#product is to be produced
cummulative <- cumsum(production_groups$`Production Days`)

#add this cumulative sum to production gropus data frame
cummulativedf <- data.frame("Production Start" = cummulative)
production_groups <- bind_cols(production_groups,cummulativedf)
production_groups$Production.Start <- floor(production_groups$Production.Start)

#create empty data base to add each parent roll simulation data to 
final_output <- data.frame(order_date=c(0), init_on_hand = c(0), 
                           consumption = c(0), after_consumption=c(0), 
                           orders_placed=c(0),last_order_date=c(0), 
                           orders_received=c(0), orders_transit=c(0), 
                           total_inv=c(0), backorders=c(0), 
                           backorders_filled =c(0), end_on_hand =c(0), 
                           parent_roll.Grade=c(0), parent_roll.Caliper=c(0), 
                           parent_roll.ParentW = c(0), cycle = c(0), dmd_class = c(0))

#matrix for future validation purposes
#sets initial_conditions to begin new simulation
initial_conditions <- data.frame("Grade"=c(rep_len(0,numParentRolls)),
                  "Caliper"=c(rep_len(0,numParentRolls)),"ParentW"=
                  c(rep_len(0,numParentRolls)),"End on Hand"=
                  c(rep_len(0,numParentRolls)),"Order Up To"=
                  c(rep_len(0,numParentRolls)),"Safety Stock"=
                  c(rep_len(0,numParentRolls)),"Start Production"=
                  c(rep_len(0,numParentRolls)), "Production Days" =
                  c(rep_len(0,numParentRolls)),"Backorders"=
                  c(rep_len(0,numParentRolls)),"Orders Transit"=
                  c(rep_len(0,numParentRolls)),"Last Order Date" = 
                  c(rep_len(0,numParentRolls)),"Orders Placed" = 
                  c(rep_len(0,numParentRolls)),"Orders Received"=
                  c(rep_len(0,numParentRolls)),"Total Inv"=
                  c(rep_len(0,numParentRolls)),"Backorders Filled" = 
                  c(rep_len(0,numParentRolls)))

#iterate through each parent roll and call single invetory simulation
#start variable to track total trim cost 
totalRollTrimCost <- 0 
#start with first roll in parent list
roll <- 1

#for each roll in parent list 
print("Simulating Inventory")
while (roll <= num_parents) {
  print(paste('SKU: ', roll, sep=""))
  #filter data to show only data concerning that roll 
  dataset <- filter(all_data_by_date,
                    all_data_by_date$ParentW == parent_rolls$ParentW[roll] & 
                      all_data_by_date$Grade == parent_rolls$Grade[roll] & 
                      all_data_by_date$Caliper == parent_rolls$Caliper[roll])
  #using this data, sum tons by Width, Plant, ParentW 
  transCost <- dataset %>% group_by (Width, Plant, ParentW) %>% 
                                           summarise(Tons=sum(Tons))
  #ungroup data 
  transCost <- transCost %>% ungroup (Width, Plant, ParentW)
  #add column with calculated trim per ton from parent roll to product
  transCost <- transCost %>% mutate ("Trim per Ton"=((ParentW%%Width)/Width))
  #calculate total trim over a year by multiplying trim/ton by total tons
  transCost <- transCost %>% mutate ("Trim"= `Trim per Ton`*Tons)
  #bring in shipping costs that are dependent on plant
  transCost <- transCost %>% left_join(shippingCosts,by='Plant')
  #calculate total cost of trim by multiplying trim by shipping costs
  transCost <- transCost %>% mutate ("Trim Cost" = Trim * `Cost/Ton`)
  #add together all the shipping costs to get total trim cost
  rollTrimCost <- sum(transCost$`Trim Cost`)
  #Error Checking
  if(is.na(totalRollTrimCost)) {stop("The trim cost failed to calculate. Try checking that transportation rates exist for every mill to plant lane.")}
  #add this trim cost to overall trim cost 
  totalRollTrimCost <- totalRollTrimCost + rollTrimCost
  #runs Single Inventory Simulation.R for each parent roll,and for serveral service levels 
  #results stored in data frame called output
  source("mod_R/Single Inventory Simulation.R")
  #all of the below assign values calculated in Single Inventory Simulation.R
  #parent grade
  initial_conditions$Grade[roll] <- parent_grade
  #parent caliper
  initial_conditions$Caliper[roll] <- parent_caliper
  #parent width
  initial_conditions$ParentW[roll] <- parent$ParentW[1]
  #end on hand from last day of simulation
  initial_conditions$End.on.Hand[roll] <- output$end_on_hand[n_output]
  #order up to level
  initial_conditions$Order.Up.To[roll] <- order_up_to
  #safety stock level
  initial_conditions$Safety.Stock[roll] <- ss
  #number of days between start date and production start
  initial_conditions$Start.Production[roll] <- start_production
  #days item is produced during cylce
  initial_conditions$Production.Days[roll] <- production_days
  #number of backorders on last day of simulation
  initial_conditions$Backorders[roll] <- output$backorders[n_output]
  #orders in transit on last day of simulation
  initial_conditions$Orders_Transit[roll] <- output$orders_transit[n_output]
  #last order date from simulation
  initial_conditions$Last.Order.Date[roll] <- output$last_order_date[n_output]
  #orders placed on last day of simulation
  initial_conditions$Orders.Placed[roll] <- output$orders_placed[n_output]
  #orders received on last day of simulation
  initial_conditions$Orders.Received[roll] <- output$orders_received[n_output]
  #total inventory position on last day of simulation
  initial_conditions$Total.Inv[roll] <- output$total_inv[n_output]
  #backorders filled on last day of simulation
  initial_conditions$Backorders.Filled[roll] <- output$backorders_filled[n_output]
  #add output from Single Inventory Simulation.R to final_output data frame
  final_output <- rbind(final_output, output)
  roll = roll + 1
}

#format final_output database dates 
final_output$order_date <- as.Date(final_output$order_date, 
                                   origin = "1970-01-01")
final_output$last_order_date <- as.Date(final_output$last_order_date,
                                        origin = "1970-01-01")
#get rid of first row of zeros
final_output <- final_output[-c(1),]


#remove uneccessary columns 
combined_output <- final_output[,c("order_date","init_on_hand","consumption",
                                   "after_consumption","orders_placed","orders_received",
                                   "orders_transit","total_inv","backorders",
                                   "backorders_filled","end_on_hand")]
#group by order date and calculate sum of different columns
combined_output <- combined_output %>% group_by(order_date) %>% 
  summarise("Initial On Hand" = sum(init_on_hand), 
            "Consumption" = sum(consumption), "After Consumption" = 
              sum(after_consumption), "Orders Placed"=sum(orders_placed),
            "Orders Received"=sum(orders_received),"Orders in Transit"=
              sum(orders_transit), "Total Inventory Position"=
              sum(total_inv),"Backorders"=sum(backorders),
            "Backorders Filled"=sum(backorders_filled),
            "End on Hand"=sum(end_on_hand))

#reference cycle data from Parent Roll Iterations.R 
cycle_df <- data.frame(cycle_data)

#add cycle data to combined output
combined_output<- bind_cols(combined_output,cycle_df)

#change column names
names(combined_output)[1] <- "Order Date"
names(combined_output)[12] <- "Cycle"

#calculate mimimum number of backorders 
min_backorders <- min(combined_output$'Backorders')

#calculate maximum inventory position
max_inventory_position <- max(combined_output$'Total Inventory Position')

#formatting to get rid of first row of zeros 
combined_output <- combined_output[-1,]

#index for last day in simulation
days <- nrow(combined_output)

#average of end on hand inventory level 
avg_on_hand <- mean(combined_output$`End on Hand`)
#average of end on hand inventory level after 1 month of simulation
#stabilizationPeriod defined in User Inputs.R 
stabilized_avg <- mean(combined_output$`End on Hand`[stabilizationPeriod:days])

#minimum on hand during simulation 
min_on_hand <- min(combined_output$`End on Hand`)
#maximum on hand during simulation 
max_on_hand <- max(combined_output$`End on Hand`)
#maximum on hand after 1 month of simulation 
#stabilizationPeriod defined in User Inputs.R
stabilized_max <- max(combined_output$`End on Hand`[stabilizationPeriod:days])

#average tons backordered during simulation
avg_backorders <- mean(combined_output$Backorders)
#minimum tons backordered during simulation
min_backorders <- min(combined_output$Backorders)
#maximum tons backordered during simulation 
max_backorders <- max(combined_output$Backorders)
#counts number of days that have a backorder during simulation 
days_backorders <- combined_output$Backorders < 0 
days_backorders <- table(days_backorders)
days_backorders <- days_backorders["TRUE"][[1]]

#counts total tons that are backordered
tons_backordered <- sum(combined_output$`Backorders Filled`)
#counts total tons that are consumed before converting 
tons_ordered <- sum(combined_output$Consumption)
