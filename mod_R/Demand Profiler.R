###############################################################################
# 
# Classify the demands for all the items
# Use the Llamasoft logic
# 
################################################################################

rd_stats_df <- rawData %>% 
  group_by(Date, Grade, Caliper, Width) %>% # sum over the Plant
  summarize(Tons = sum(Tons)) %>%
  separate(Caliper, c("Caliper", "Diam", "Wind"), sep = "-") %>%
  ungroup()
start_date <- min(rd_stats_df$Date)
end_date <- max(rd_stats_df$Date)
dmd_parameters <- c(nbr_dmd = 5, # Extremely Slow nbr demands
                    dmd_mean = 20, # Extremely Small demand mean
                    outlier = 10, 
                    intermit = 1.9, # Intermittency demand interval
                    variability = 4, # non-zero dmd std dev
                    dispersion = 0.49) # CV^2, non inter:smooth, 
                                       # inter: eratic - slow, lumpy

rd_stats_df <- demand_class(rd_stats_df, start_date, end_date,
                            "day", dmd_parameters)
rd_stats_df <- rd_stats_df %>%
  unite(Caliper, Caliper, Diam, Wind, sep = "-")


# some plots
# Count
b <- ggplot(rd_stats_df, aes(dclass)) +
  geom_bar() +
  coord_flip()
b
rm(b)

# Tons
b <- ggplot(rd_stats_df, aes(dclass, dmd_sum)) +
  geom_bar(stat = "identity") +
  coord_flip()
b
rm(b)

# Facets for all the items
#t <- ggplot(ldply(rd_split), aes(tDate, Tons)) +
#  geom_line() + facet_wrap(~.id)
# t  # This takes a while

#rm(t)
#rm(rd_unsplit)
#rm(rd_split)

llamasoft <- F
# Test with Llamasoft
if (llamasoft) {
  rawDataLS <- rawData %>% unite(id, c(Grade, Caliper, Width), sep = ".")
  rawDataLS <- rawDataLS %>% mutate(site = "Rose City")
  rawDataLS <- rawDataLS %>% select(id, site, Tons, Date)
  
  write.csv(rawDataLS, "Data/DemandDataForLamasoft.csv", row.names = FALSE)
  rm(rawDataLS)
  write.csv(rd_stats_df, "Data/DemandClassification.csv", row.names = FALSE)
}
rm(llamasoft)
