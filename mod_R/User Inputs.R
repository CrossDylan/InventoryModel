###Source this script and the entire thing will run

## Indicate Desired Mill

mills <- c('BC','KZ','MT','SC','MC','WM','WGA')
print(mill)
  
runName <- "Varied Service"

#user defined inputs, change here and scripts will reflect change
#scripts listed in order of execution

##Simulation Iterations.R 
#path to shipping data
shippingDataPath <- paste("mod_input/ShipmentData.xlsx", sep="")
#file path for final output, line 82
resultsPath <- paste("mod_output/","Varied Service/",mill,"_FinalResults.csv", sep="")
#increment between number of parent rolls to run, i.e. every 1, 5th, 10th etc., line 41
parentNIncrement = 5
# Should paper machine trim be run or not, True=Yes, False=No
runPMTrim <- F
# Run only at optimal count & Save parent roll assignment; True=Yes, False=No
optimalRun <- F   ###SET TARGET IF TRUE###
# Create SKU Target table by Mill; c('BC','KZ','MT','SC','MC','WM')
  # Pull value from tableau as min cost number of parent rolls
millTargets <- c('BC','KZ','MT','SC','MC','WM','WGA','M1','M2','W6','W7')
targets <- c(151,193,127,201,266,422,175,239,90,163,244)
skuTargets <- as.list(targets)
names(skuTargets) <- millTargets
# Path for simulation results for optimal scenario
simResultsPath <- paste("mod_output/","Varied Service/",mill,"_SimResults.csv", sep="")


##Data Read In.R Script
#path to the original data, line 5 
mainDataPath <- paste("mod_input/All_Consumption.xlsx", sep="")
#name of file to save roll selection cost information to, line 193
rollSelectionOutputPath <- paste("mod_output/","Varied Service/",mill,"_RollSelection.csv", sep="")
#name of file to save roll selection assignment information to, line 193
rollSelectionAssignOutputPath <- paste("mod_output/","Varied Service/",mill,"_RollSelectionAssignments.csv", sep="")
#list of locations with slitter limitations, line 20 
slitterLimitPlants <- c('GPI-STONE MOUNTAIN,GA', 'GPI-CAROL STREAM,IL', 'GPI-MARION,OH', 
                        'GPI-OAKS,PA', 'GPI-LUMBERTON,NC', 'GPI-FORT SMITH,AR', 
                        'GPI-CENTRALIA,IL', 'GPI-MITCHELL,SD', 'GPI-KENDALLVILLE,IN', 
                        'GPI-GORDONSVILLE,TN', 'GPI-KALAMAZOO,MI', 'GPI-LAWRENCEBURG,TN', 
                        'GPI-WAUSAU,WI', 'GPI-COBOURG', 'GPI-PORTLAND,OR', 'GPI-TUSCALOOSA,AL', 
                        'PLT00006', 'PLT00008', 'PLT00015', 'PLT00020', 'PLT00023', 'PLT00041', 
                        'PLT00042', 'PLT00043', 'PLT00044', 'PLT00047', 'PLT00048', 'PLT00053', 
                        'PLT00054', 'PLT00068', 'PLT00070', 'PLT00EUR')

#Parent Roll Selection.R Script
#big M used in IP to select parents and assign products, line 80
bigM <- 100 
#total trim limit at converting plant in inches (3 --> 1.5" from each side)
#lines 101, 121, and 127
trimLimit <- 3
#list of cutter and slitter widths in converting plants in inches, line 115
cutters <- c(32, 33.5, 40, 42.375, 55, 56, 64)

#Combining Parent Rolls.R Script 
#days allowed for simulation to stabilize before metrics are taken (line 48,55)
stabilizationPeriod <- 31 

#Single Inventory Simulation.R Script 
cycleLengthPath <- "mod_input/cycle_lengths.xlsx"
#cycle length used in lines 39,56,59,114,157; Also used in Parent Roll Iterations Line 55 
cycle_length_list <- as_data_frame(read_excel(cycleLengthPath, sheet = 1, col_name = TRUE, col_type = NULL, 
                      na = "", skip = 0))
#cycle_length <- 21  # Use to set fixed cycle lengths if appropriate

#transportation_length (mill to plant), lines 56,59,134,135
transportation_length <- 2
#service level, line 63
service_level_min <- 0.98
service_level_max <- 0.98
service_level_inc <- 0.01
service_level_tgt <- 0.935

#Paper Machine Trim.R 
#average tons/roll, line 26
# tonsPerRoll <- 2.04 # Changed calculation to estimate weight based on roll dimensions for better accuracy
#path to export data to use as input for python model,line 39 
pythonInputPath <- paste("mod_output/","Varied Service/",mill,"_output_for_trim.csv", sep="")
#file name of python model to run from, line 42
pythonModel <- 'mod_pyTrim/cutstock_looper.py'
#file name of results of python model, line 45
#pyTrimResults <- paste("mod_output/","Varied Service/",mill,"_MachineTrimResults.csv", sep="")  # Rewritten to pass through stdout back to R
#incher per ton, lines 48, 51 
inchesPerTon <- 18

# FuncDemandClassification
dmd_parameters <- c(nbr_dmd = 5, # Extremely Slow nbr demands
                    dmd_mean = 20, # Extremely Small demand mean
                    outlier = 10, 
                    intermit = 1.9, # Intermittency demand interval
                    variability = 4, # non-zero dmd std dev
                    dispersion = 0.49) # CV^2, non inter:smooth, 

#line that runs the rest of the scripts

