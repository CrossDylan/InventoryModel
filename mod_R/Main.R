## Main file for running SKU consolidation model

# optional: set working directory to file path where model's stored
setwd("C:/Users/crossd/Desktop/_SantaClara VMI/SantaClara Inventory Modeling/Roll Size Assortment Optimization Model")

source("mod_R/Libraries.R")

for (mill in c('W6')){     ##,'W7','M1','M2')){
  source("mod_R/User Inputs.R")  
  source("mod_R/Data Read In.R")
  source("mod_R/Parent Roll Selection.R")
  source("mod_R/Scenario Iterations.R")
}


