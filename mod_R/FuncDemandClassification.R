###############################################################################
# 
# Classify the demands for all the items
# Use the Llamasoft logic
# 
################################################################################
#
# Pass a time series dataframe with [date, grade, caliper, width, Consumption], start_date,
# end date.  Dates should be a string "YYYY-mm-dd", e.g., "2015-01-01"
#
# dmd_parameters <- c(nbr_dmd = 5, # Extremely Slow nbr demands
#                     dmd_mean = 20, # Extremely Small demand mean
#                     outlier = 10, 
#                     intermit = 1.9, # Intermittency demand interval
#                     variability = 4, # non-zero dmd std dev
#                     dispersion = 0.49) # CV^2, non inter:smooth, 

demand_class <- function(tsdf, start, end, dfreq, dparams) {
  # Create a data frame with all the days of the year to be used for 
  # Standard Deviation and safety stock calculations
  tDays <- data.frame(seq.Date(as.Date(start), 
                               as.Date(end), by = dfreq ))
  colnames(tDays)[1] <- "tDate"
  
  ts <- full_join(tDays, dataset, by = c("tDate" = "Date"))
  
  ts <- mutate(ts, Consumption = ifelse(is.na(Consumption), 0, Consumption))
  
  ts <- as_data_frame(ts)
  ts_stats <- summarise(ts, 
              dmd_mean = mean(Consumption),
              dmd_sum = sum(Consumption),
              dmd_std_dev = sd(Consumption),
              nz_dmd_count = nrow(filter(ts, Consumption > 0)),
              nz_dmd_mean = sum(Consumption)/nrow(filter(ts, Consumption > 0)),
              nz_dmd_std_dev = sd(ts$Consumption[ts$Consumption > 0]),
              nz_cv2 = (sd(ts$Consumption[ts$Consumption > 0])/
                          (sum(Consumption)/nrow(filter(ts, Consumption > 0))))^2,
              nperiods = n(),
              p = n()/nrow(filter(ts, Consumption > 0)))
  
  # Function to determine demand classification
  dmd_class <- function(x) {
    if (x$nz_dmd_count < dparams["nbr_dmd"]) {x$dclass <- "Extremely Slow"
    } else { 
      
      if (x$nz_dmd_mean < dparams["dmd_mean"]) {x$dclass <- "Extremely Small"
      
      } else {
        if (x$p < dparams["intermit"]) {
          if (x$nz_cv2 < dparams["dispersion"]) {
            x$dclass <- "Non-Intermittent, Smooth"
          } else {
            x$dclass <- "Non-Intermittent, Erratic"
          }
        } else {
          if (x$nz_dmd_std_dev < dparams["variability"]) {
            if (x$nz_cv2 < dparams["dispersion"]) {
              x$dclass <- "Intermittent, Low Variable, Slow"
            } else {
              x$dclass <- "Intermittent, Low Variable, Lumby"
            }
          } else {
            if (x$nz_cv2 < dparams["dispersion"]) {
              x$dclass <- "Intermittent, Highly Variable, Slow"
            } else {
              x$dclass <- "Intermittent, Highly Variable, Lumpy"
            }
          }
        }
      }
      
    }
    return(x)
  }
  
  # Compute the demand classification
  ts_stats <-  dmd_class(ts_stats)
  
  return(ts_stats)
}



