# This script reads in HOME program rent limit data
library(tidyverse)

# data pulled from:
# https://www.huduser.gov/portal/datasets/HOME-rent-limits.html

# get the file names
HOME_rent_limit_list = lapply(list.files("../Data/Raw Data/HOME Rent Limits/"),function(x){
  print(x)
  if(grepl("xlsx",x)){
    temp = openxlsx::read.xlsx(paste0("../Data/Raw Data/HOME Rent Limits/",x)) %>% 
      mutate(year = x %>% str_extract("\\d{4}")) 
    colnames(temp) = toupper(colnames(temp)) # make variable names uniform
    temp %>%
      select(YEAR,CBSASUB,STATE,AREANAME,starts_with("LOWRENT"),
             starts_with("FMR"),starts_with("HIGHRENT"),
             starts_with("HOMERENT")
             ) %>% 
      return()
  }else{
    temp = readxl::read_xls(paste0("../Data/Raw Data/HOME Rent Limits/",x)) %>% 
      mutate(year = x %>% str_extract("\\d{4}")) 
    colnames(temp) = toupper(colnames(temp)) # 
    temp %>%
      select(YEAR,CBSASUB,STATE,AREANAME,starts_with("LOWRENT"),
             starts_with("FMR"),starts_with("HIGHRENT"),
             starts_with("HOMERENT")
      ) %>% 
      return()
  }
}) 

# make everything the same class, then put together in one data frame
HOME_rent_limit_df = lapply(HOME_rent_limit_list,as.data.frame) %>% 
  do.call(gtools::smartbind,.)

# save the data  
save(HOME_rent_limit_df,file = "../Data/Cleaned Data/HOME_rent_limit_df.RData")
