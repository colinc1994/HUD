
# This script will read in the data from the HUD and make sure the variables and data are usable
library(tidyverse)
library(data.table)

# read all the raw files, either reads xlsx or xls
inspection_scores_df = lapply(list.files("Data/Raw Data/"),function(x){
print(x)
  if(grepl("x",x)){
    temp = readxl::read_excel(paste0("Data/Raw Data/",x)) %>% 
      mutate(type = ifelse(grepl("multi",x),
                           "Multi-Family",
                           "Public Housing")) 
    # make all the variable names the same
    colnames(temp) = colnames(temp) %>% toupper()
    
    # some data seems to be missing, make missing data explicit
    if(!"ZIP" %in% colnames(temp)){temp  = temp %>% mutate(ZIP = NA)} 
    if(!"LOCATION_QUALITY" %in% colnames(temp)){temp  = temp %>% mutate(LOCATION_QUALITY = NA)} 
    
    temp %>% 
      select(ADDRESS,ZIP,STATE_CODE,LATITUDE,LONGITUDE,
             ZIP,TYPE,LOCATION_QUALITY,INSPECTION_SCORE,
             CBSA_CODE,COUNTY_NAME) %>% 
      return()
  }
}) %>% 
  do.call(rbind,.)

# save this dataframe
save("Data/Cleaned Data/inspection_scores_df.RData")
