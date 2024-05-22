
# This script will read in the data from the HUD and make sure the variables and data are usable
library(tidyverse)
library(data.table)
library(anytime)

# read all the raw files, either reads xlsx or xls
inspection_scores_df = lapply(list.files("../Data/Raw Data/Inspection Scores/",pattern = "inspect"),function(x){
print(x)
  if(grepl("x",x)){
    temp = readxl::read_excel(paste0("Data/Raw Data/",x)) %>% 
      mutate(type = ifelse(grepl("multi",x),
                           "Multi-Family",
                           "Public Housing")) 
    # make all the variable names the same
    colnames(temp) = colnames(temp) %>% toupper()
    
    # fix dates
    # special case if it is this fle since the date format is different
    if(grepl("multifamily_physical_inspection_scores_2020|public_housing_physical_inspection_scores_2020",x)){
      temp$INSPECTION_DATE = temp$INSPECTION_DATE %>% lubridate::dmy()
    }else{
    temp$INSPECTION_DATE = temp$INSPECTION_DATE %>%  anytime()
    }
    

    
    # some data seems to be missing, make missing data explicit
    if(!"ZIP" %in% colnames(temp)){temp  = temp %>% mutate(ZIP = NA)} 
    if(!"LOCATION_QUALITY" %in% colnames(temp)){temp  = temp %>% mutate(LOCATION_QUALITY = NA)} 
    print(colnames(temp)) # check variable names
    print(temp$INSPECTION_DATE %>% head())
    temp %>% 
      mutate() %>% 
      select(ADDRESS,ZIP,STATE_CODE,LATITUDE,LONGITUDE,
             ZIP,TYPE,LOCATION_QUALITY,INSPECTION_SCORE,
             CBSA_CODE,COUNTY_NAME,INSPECTION_DATE) %>% 
      mutate(filename = x) %>% 
      return()
  }
}) %>% 
  do.call(rbind,.)

# save this dataframe
save(inspection_scores_df,file = "../Data/Cleaned Data/inspection_scores_df.RData")
