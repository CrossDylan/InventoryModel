#Simulation Creation#
##run this to create discrete event simulation for 1 parent roll##

simstart <- Sys.time()

#finds name of current parent roll
parent <- parent_rolls[roll,]

#uses dataset defined in Parent Roll Iterations.R line 103
#calculate total consumption of parent roll (takes into account trim)
dataset <- dataset %>% mutate("Total Consumption" = 
            ((as.numeric(dataset$ParentW)%%as.numeric(dataset$Width))/
              as.numeric(dataset$Width)+1) * dataset$Tons)

#if (parent==testParent) {
#  stop()
#}

#find parent grade
parent_grade <- dataset$Grade[1]

#find parent caliper 
parent_caliper <- dataset$Caliper[1]

#set correct cycle
cycle_length <- cycle_length_list %>% filter(Mill==mill) %>%
                                   filter(parent_roll.Grade==parent_grade) %>%
                                   filter(parent_roll.Caliper==parent_caliper)
cycle_length <- cycle_length$cycle_length

#take subset of columns from data 
dataset <- dataset[,c("Date","Total Consumption")]

  
#convert date to first date of week
dataset$Date <- floor_date(dataset$Date, unit="week")

#add consumption by date 
dataset <- dataset %>% group_by(Date) %>% summarise(`Total Consumption`=sum(`Total Consumption`))
dataset <- dataset %>% ungroup(Date)


#rename column
names(dataset)[2] <- "Consumption"

#format dates correctly
dataset$Date <- as.Date(dataset$Date, origin="1970-01-01")


# fix annoying date problem
if (dataset$Date[1]==as.Date("2015-06-28")) {
  dataset$Consumption[2] <- dataset$Consumption[2] + dataset$Consumption[1]
  dataset$Date[1] <- NULL
}

#get demand classification
#start_date <- min(dataset$Date)
#end_date <- max(dataset$Date)

product_dmd_class <- demand_class(dataset, start_date, end_date,
                            "week", dmd_parameters)$dclass

#create day_by_day from start date to end date 
day_by_day <- data.frame("Date"= seq(as.Date(start_date, origin="1970-01-01"), 
                                     as.Date(end_date, origin="1970-01-01"), by = "1 day"))

#add cycle number to day_by_day
#cycle length list defined in User Inputs.R 
day_by_day <- day_by_day %>% mutate("Cycle" = 
                        ceiling((day_by_day$Date - start_date)/cycle_length))
#change first row entry from 0 to 1 
day_by_day[1,2] <- 1

#join day_by_day with consumption data 
day_by_day <- day_by_day %>% full_join(dataset, day_by_day, by='Date')
#set all days with NA for consumption to 0
day_by_day[is.na(day_by_day)] <- 0 

#find the mean demand over the entire period
mean_demand <- mean(day_by_day$Consumption)

#find the variance in demand over the entire period
var_demand <- var(day_by_day$Consumption)

#calculate mean demand during lead time
#transportation_length defined in User Inputs.R
mean_lead_time <- mean_demand * (transportation_length + cycle_length)

#calculate variance of demand during lead time
var_lead_time <- var_demand * (transportation_length + cycle_length)

## Increment through service levels looking for adequate service results

for (service_level in seq(service_level_min, service_level_max, service_level_inc)){

  #calculate safety stock level
  #service_level defined in User Inputs.R
  ss <- qnorm(service_level,0,1)*(var_lead_time)^0.5
  
  #calculate order up to level
  order_up_to <- ceiling(mean_lead_time+ss)
  
  #create references for data needed for output
  consumption_data <- c(0,day_by_day$Consumption)
  cycle_data <- c(0,day_by_day$Cycle)
  
  #create empty data frame for event simulation 
  output <- data.frame(order_date = seq(as.Date(start_date)-1,
                      as.Date(end_date, origin="1970-01-01"),by="day"), 
                      init_on_hand = c(0), consumption = consumption_data, 
                      after_consumption = c(0), orders_placed = c(0), 
                      last_order_date = seq(as.Date
                      (start_date, origin="1970-01-01")-1,
                      as.Date(end_date, origin="1970-01-01"),by="day"),
                      orders_received = c(0), orders_transit= c(0), 
                      total_inv=c(0), backorders=c(0), 
                      backorders_filled= c(0), end_on_hand=c(0), 
                      parent_roll=c(parent), cycle=cycle_data,
                      dmd_class=product_dmd_class)
  
  #find the production group the current parent roll is in
  #production_groups from Parent Roll Iterations.R
  production_group <- production_groups %>% filter(Grade==
                      parent_grade & Caliper==parent_caliper)
  
  #how many days after start will production of this product begin
  start_production <- production_group$Production.Start[1]
  
  #how many days during cycle product will be in production
  production_days <- production_group$`Production Days`
  
  #set up first row of event simulation
  #set initial on hand to order up to level 
  output$init_on_hand[2]<-order_up_to
  #after consumption = initial on hand - consumption
  output$after_consumption[2]<- max(output$init_on_hand[2]
                                - output$consumption[2],0)
  #backorders occure if consumption is more than initial on hand 
  #backorder = initial on hand - consumption + backorders from previous step
  output$backorders[2] <- if ((output$init_on_hand[2]-output$consumption[2])<=0) 
                          (output$init_on_hand[2]-output$consumption[2]+
                             output$backorders[1]) else output$backorders[1]
  #end on hand = after consumption + orders received
  output$end_on_hand[2] <- output$after_consumption[2] + 
                            output$orders_received[2]
  #order is placed if the order date + start date + start production is a 
    #mutliple of the cycle length
  #if order placed = order up to - end on hand - backorders
  output$orders_placed[2]<- if(((as.numeric(output$order_date[2]-
                            start_date+start_production))%%(cycle_length))==0)
              (order_up_to-output$end_on_hand[2]-output$backorders[2]) else 0 
  #orders in transit = orders placed-orders received+order transit from previous
  output$orders_transit[2] <- output$orders_placed[2] - 
                         output$orders_received[2] + output$orders_transit[1]
  #total inventory = end on hand + orders in transit 
  output$total_inv[2] <- output$end_on_hand[2] + output$orders_transit[2]
  
  #iterate through remaining time steps 
  n_output <- as.numeric(end_date-start_date)+2
  
  z <- 3
  while (z <=n_output){
    #initial on hand = end on hand + backorders, or 0 if that is negative
    output$init_on_hand[z] <- max((output$end_on_hand[z-1] + 
                                     output$backorders[z-1]),0)
    #after consumption = initial on hand - consumption, or 0 if that is negative
    output$after_consumption[z] <- max(output$init_on_hand[z] - 
                                         output$consumption[z],0)
    #orders received = orders placed transportation length days ago
    output$orders_received[z] <- output$orders_placed[max(z-transportation_length,1)]  #Assumes orders placed>=0 
                            #if (
                            #output$orders_placed[max(z-(transportation_length),1)]>0) 
                            #output$orders_placed[max(z-(transportation_length),1)] 
                            #else 0
    #backorders occur if consumption is more than initial on hand 
    #backorder = initial on hand - consumption + backorders from previous step + 
      #orders received from previous step 
    output$backorders[z]<- if ((output$init_on_hand[z]-output$consumption[z])<=0) 
                             (min((output$init_on_hand[z]-output$consumption[z]+
                                  output$backorders[z-1]+output$orders_received[z-1]),0)) 
                            else 0
    #backorders filled when orders are received and there were backorders 
      #the previous step
    #backorder filled is the minimum of the amount received or backordered 
    output$backorders_filled[z] <- if (output$orders_received[z-1]>output$backorders_filled[z-1]) 
                                   (min(output$orders_received[z-1],-1*output$backorders[z-1])) 
                                    else 0
    #end on hand = after consumption + orders received 
    output$end_on_hand[z] <- output$after_consumption[z] + 
                              output$orders_received[z]
    #orders placed if date - start date + start production is a multiple of cycle
      #length 
    #orders placed = order up to - end on hand - backorders
    output$orders_placed[z] <- if (((as.numeric(output$order_date[z]-start_date+start_production))%%(cycle_length))==0) 
                                (order_up_to-output$end_on_hand[z]-output$backorders[z]) 
                               else 0
    #order placed if order date - start date + start production =start production
    output$orders_placed[z] <- if((as.numeric(output$order_date[z]-start_date+start_production)==start_production)) 
                               (order_up_to-output$end_on_hand[z]-output$backorder[z]) else output$orders_placed[z]
    
    #last order date is updated when an order is placed    #not needed / not reused elsewhere
    #output$last_order_date[z] <- if (output$orders_placed[z]>0)
      #                  output$order_date[z] else output$last_order_date[z-1]
    #orders in transit = orders placed - orders receieved + orders transit from 
      #last step 
    output$orders_transit[z] <- output$orders_placed[z] - output$orders_received[z]+
                                 output$orders_transit[(z-1)]
    #move to the next day
    z <- z+1
  }
  ##Non-looped stats
  #total inventory level = end on hand + orders in transit 
  output$total_inv <- output$end_on_hand + output$orders_transit
  
  service_level_res <- 1-sum(output$backorders_filled)/sum(output$consumption)
  if(service_level_res >= service_level_tgt) {break}
}

simend <- Sys.time()
print(c(service_level_res, service_level))
print(simend-simstart)
