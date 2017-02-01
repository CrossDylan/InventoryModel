#Roll Selection Data Input and Output# 

#read data from file
#path defined in User Inputs.R
rawData <- as_data_frame(read.xlsx(mainDataPath)) #, sheet = 1, col_name = TRUE, col_type = NULL, 
                       # na = "", skip = 0)
# Filter irrelevant data 
rawData <- filter(rawData, Grade.Type=="SUS")
rawData$Dia[rawData$Dia<72] <- 72 
rawData <- filter(rawData, Grade %in% c("AKOS", "AKPG", "FC02", "PKXX", "OMXX"))
rawData <- filter(rawData, !(MachineModel %in% c("W1", "W")))
rawData <- filter(rawData, !(Plant %in% c("PLT00040","PLT00031","PLT00033")))

# Add fields
rawData$Wind <- substr(rawData$Description,nchar(rawData$Description),nchar(rawData$Description))
rawData$Caliper <- paste(rawData$Dia,rawData$`Cal/lb`,rawData$Wind,sep="-")

# Consolidate duplicate diameters, particularly relevant for beverage
rawData <- rawData %>% 
  group_by(MachineModel, Grade, `Cal/lb`, Wind, Calc.width) %>%
  mutate(Max.Diam = max(Dia)) %>%
  mutate(Dia = Max.Diam)

rawData <- ungroup(rawData)

## M1 debugging
#rawData <- filter(rawData, Grade %in% c("OMXX"))
#rawData <- filter(rawData, Caliper %in% c("72-18-F"))

#format date into date class
rawData$Ac.GI.date <- as.Date(rawData$Ac.GI.date, origin = '1900-1-1')

rawData <- transmute(rawData, Mill=MachineModel, Date=Ac.GI.date, Grade, 
                  Caliper, Width=Calc.width, Plant, Tons=Net.Ton)

  
rawData <- rawData %>% group_by(Mill, Date, Grade, Caliper, Width, Plant) %>% 
                        summarise(Tons=sum(Tons)) %>% ungroup()

write.csv(x = rawData, file = "mod_input/All_Consumption.csv")

#Temp> Run only beverage
#rawData <- filter(rawData, Plant %in% c("PLT00068","PLT00070"))

# Select desired mill
rawData <- filter(rawData,Mill==mill)

#read shipping cost data from file
shippingCosts <- read_excel(shippingDataPath, sheet = 1, col_name = TRUE, 
                            col_type = NULL, na = "", skip = 0) 

shippingCosts <- filter(shippingCosts, shippingCosts$Mill==mill)
