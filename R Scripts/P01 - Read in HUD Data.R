
# This script will read in the data from the HUD and make sure the variables and data are usable
library(tidyverse)

# read all the raw files

raw_data_list = lapply(list.files("Data/Raw Data/"),function(x){

  openxlsx::read.xlsx(paste0("Data/Raw Data/",x)) %>% 
    mutate(type = ifelse(grepl("multi",x),
                         "Multi-Family",
                         "Public Housing"))
})
