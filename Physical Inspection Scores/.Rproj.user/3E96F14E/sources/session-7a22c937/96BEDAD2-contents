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


############################## 

load("Data/Cleaned Data/inspection_scores_df.RData")


# Install and load the necessary packages
library(tidycensus)
# Set your Census API key (replace "YOUR_API_KEY" with your actual Census API key)
census_api_key("805c55e2e35a7092f352eef54d37823f41218c5e")


#################
# looking at years 2000 to 2023
years <- c(2000:2023)

# Loop over the years
cbsa_census_data = lapply(years,function(y){
  tryCatch({
    # Retrieve demographic data for each CBSA for the current year
    cbsa_data <- get_acs(
      year = y,
      geography = "metropolitan statistical area/micropolitan statistical area",
      variables = c("B01001_001", "B02001_002", "B02001_003", "B19013_001","B25077_001"),
      summary_var = "B01001_001",
      geometry = F
    ) %>% 
      mutate(y = y)
    cbsa_data %>% return()
  }, error = function(e) {
    # Print error message if an error occurs
    cat(paste("Error occurred for year", year, ":", conditionMessage(e), "\n"))
    # If there's an error with the ACS data file, specify the correct one
    if (grepl("ACS", conditionMessage(e))) {
      cat("Please ensure that you are using the correct ACS data file for year", year, "\n")
    }
  })
}) %>% do.call(rbind,.) %>% 
  mutate(variable = plyr::mapvalues(variable,
                                    from = c("B01001_001", "B02001_002",
                                             "B02001_003", "B19013_001",
                                             "B25077_001"),
                                    to = c("Total_Pop", "White_Pop", 
                                           "Non_White_Pop", "Median_HHI",
                                           "Median_Home_Value"))) %>% 
  pivot_wider(id_cols = c(GEOID,NAME,y),
              values_from = estimate,names_from = variable)

# looking at ratio of home value to hhi
cbsa_census_data %>% mutate(r = Median_Home_Value/Median_HHI) %>% pull(r) %>% 
  summary()


# lets aggregate the inspection data to the cbsa level
inspection_scores_df %>% 
  pull(filename) %>% unique()





#######################################################

# ignore this, old code
if(T==F){
  
  # Here is the root file for the CBSA data
  # https://www2.census.gov/programs-surveys/popest/datasets/
  # still need to get 2010 I think
  cbsa_est_df = lapply(list.files("Data/Raw Data/CBSA Census/",pattern = "csv"),
                       function(x){
                         temp = read.csv(paste0("Data/Raw Data/CBSA Census/",x)) %>% 
                           mutate(year = str_extract(x,"\\d{4}"))
                         temp %>% return()
                       }) %>% 
    do.call(gtools::smartbind,.) %>% 
    filter(CBSA !="") %>% filter(CBSA !="CBSA")
  
  
  
  
}