# using the core based stat area statistics, we can attach that to the 
# physical inspection data
library(tidyverse)

# we can read in the data from the CMS 
# https://data.cms.gov/summary-statistics-on-use-and-payments/program-integrity-market-saturation-by-type-of-service/market-saturation-utilization-core-based-statistical-areas/data
cms_cbsa_data = read.csv("Data/Raw Data/Market_Saturation_and_Utilization_Release_CBSA_Q4_2023.csv")

# check unique date periods 
cms_cbsa_data$reference_period %>% unique()

# make year variable
cms_cbsa_data = cms_cbsa_data %>% 
  mutate(year = reference_period %>% substr(1,4) %>% as.numeric()) %>% 
  dplyr::rename(CBSA_CODE = cbsa)

# need to do the same for the inspection data
# merge with the inspection scores data 

## IMPORTANT: this is NOT a tidy data set as we have types of service 
total_cms_long = inspection_scores_df %>%
  mutate(year = INSPECTION_DATE %>% lubridate::year() %>% as.numeric()) %>% 
  merge(cms_cbsa_data,
         by = c("year","CBSA_CODE"),all.x = T)



# Here is the root file for the CBSA data
# https://www2.census.gov/programs-surveys/popest/datasets/
# still need to get 2010 I think
