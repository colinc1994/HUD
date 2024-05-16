# using the core based stat area statistics, we can attach that to the 
# physical inspection data
library(tidyverse)
library(tidycensus)

# load inspection data
load("../Data/Cleaned Data/inspection_scores_df.RData")

# we can read in the data from the CMS 
# https://data.cms.gov/summary-statistics-on-use-and-payments/program-integrity-market-saturation-by-type-of-service/market-saturation-utilization-core-based-statistical-areas/data
# do not always have to run this since it takes some time
if(T == F){
# Base URL of the API endpoint
base_url <- "https://data.cms.gov/data-api/v1/dataset/9b0e7798-d945-48fc-9861-d38bb5083a74/data"

# Initialize an empty list to store the data
all_data <- list()

# Total number of rows to retrieve
total_rows <- 163035

# Increment size by 5000 until total_rows is reached
offset <- 0
size <- 5000

while (offset < total_rows) {
  # Make the API call with the updated offset and size
  response <- httr::GET(sprintf("%s?offset=%d&size=%d", base_url, offset, size))
  
  # Check if the request was successful (status code 200)
  if (http_status(response)$reason == "OK") {
    # Parse the JSON response
    json_data <- content(response, "text") %>%
      jsonlite::fromJSON()
    
    # Append the data to the list
    all_data[[length(all_data) + 1]] <- json_data
    
    # Increment the offset by 5000
    offset <- offset + size
  } else {
  }
}

# Combine all the data into a single data frame
cms_cbsa_data <- do.call(rbind, all_data)

# make year variable
cms_cbsa_data = cms_cbsa_data %>% 
  mutate(year = reference_period %>% substr(1,4) %>% as.numeric()) %>% 
  dplyr::rename(CBSA_CODE = cbsa)

# Print the combined data
print(cms_cbsa_data)

# save the data
save(cms_cbsa_data,file = "../Data/Cleaned Data/cms_cbsa_data.RData")
}
# load data, this not always needed
load(cms_cbsa_data)


# checking types of service
cms_cbsa_data %>% 
  filter(aggregation_level == "CBSA") %>% 
  pull(type_of_service) %>% unique()

# need to do the same for the inspection data
# merge with the inspection scores data 

## IMPORTANT: this is NOT a tidy data set as we have types of service 
total_cms_long = inspection_scores_df %>%
  mutate(year = INSPECTION_DATE %>% lubridate::year() %>% as.numeric()) %>% 
  merge(cms_cbsa_data,
         by.x = c("report_year","CBSA_CODE"),
         by.y = c("year","CBSA_CODE"),all.x = T)


############################## 
# start here for census api calls
census_api_key("key goes here")


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

# add the year of report release variable, this is pulled from the file name (I did have to make some edits to the file names to make them line up)
inspection_scores_df = inspection_scores_df %>% 
  mutate(report_year = filename %>% str_extract("\\d{4}") %>% as.numeric()) %>% 
  mutate(year_diff = report_year - lubridate::year(INSPECTION_DATE) %>% as.numeric()) 

# do a check on the delays in reports (years)
inspection_scores_df %>% 
  filter(!is.na(report_year)) %>% 
  group_by(report_year,TYPE) %>% 
  summarise(median_years = median(year_diff,na.rm = T),
            mean_years = mean(year_diff,na.rm = T)) %>% 
  arrange(report_year)


# lets aggregate the inspection data to the cbsa level, then merge
# after merging, we will aggregate to the type of building, report year and code
inspection_scores_df %>% 
  filter(LOCATION_QUALITY != "T") %>%  # remove the ones deemed "Not Valid"
  group_by(CBSA_CODE,report_year,TYPE) %>% 
  summarise(mean_y_diff = mean(year_diff,na.rm = T),
            median_y_diff = median(year_diff,na.rm = T),
            n = n()) %>% 
  filter(!is.na(report_year)) %>% filter(report_year < 2022) %>% 
  filter(CBSA_CODE != 99999) %>% 
  merge(cbsa_census_data,
        by.x = c("report_year","CBSA_CODE"),
        by.y = c("y","GEOID")) %>% 
  group_by(report_year,CBSA_CODE,TYPE) %>% 
  summarise(mean_y_diff = mean(mean_y_diff),
            median_y_diff = median(median_y_diff),
            Total_Pop,White_Pop,Non_White_Pop,Median_HHI,Median_Home_Value
            ) 
  





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