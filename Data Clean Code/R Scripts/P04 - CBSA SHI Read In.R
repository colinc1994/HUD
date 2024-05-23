# read in the SHI inventory data
# https://www.huduser.gov/portal/datasets/assthsg.html#year2009-2023
library(tidyverse)
library(gtsummary)

cbsa_shi_list = lapply(list.files("../Data/Raw Data/CBSA SHI/"),function(x){
  print(x)
  temp = openxlsx::read.xlsx(paste0("../Data/Raw Data/CBSA SHI/",x),detectDates = T) 
  if("Quarter" %in% colnames(temp)){
    temp = temp %>% 
      mutate(year = openxlsx::convertToDate(Quarter)  %>% lubridate::year())
  }else{
    temp = temp %>% 
      mutate(Quarter = NA)
  }
  print(colnames(temp))
  temp %>% return()
})

# get columns that should be the same between all of them and put into a df
cbsa_shi_df = lapply(cbsa_shi_list,function(x){
  
  # make these conform
  colnames(x)[grepl("cbsa|CBSA",colnames(x))] = "CBSA"
  
  x %>% 
    select(year,Quarter,gsl,states,CBSA,code,entities,          
           sumlevel,program_label,program,              
           sub_program,name,        
           total_units,pct_occupied,number_reported,
           pct_reported,months_since_report,pct_movein,           
           people_per_unit,people_total,rent_per_month,      
           spending_per_month,hh_income,person_income,        
           pct_lt5k,pct_5k_lt10k,pct_10k_lt15k,   
           pct_15k_lt20k,pct_ge20k,pct_wage_major,  
           pct_welfare_major,pct_other_major,pct_median,           
           pct_lt50_median,pct_lt30_median,pct_2adults,          
           pct_1adult,pct_female_head,pct_female_head_child,
           pct_disabled_lt62,pct_disabled_ge62,pct_disabled_all,     
           pct_lt24_head,pct_age25_50,pct_age51_61,        
           pct_age62plus,pct_age85plus,pct_minority,         
           pct_hispanic,months_waiting,months_from_movein,
           pct_utility_allow,ave_util_allow,pct_bed1,
           pct_bed2,pct_bed3,pct_overhoused,      
           tpoverty,tminority,tpct_ownsfd,
           fedhse,place,latitude,longitude,state,pha_total_units,      
           ha_size)}) %>% 
  do.call(rbind,.)
# remove duplicated rows if any
cbsa_shi_df = distinct(cbsa_shi_df)

# fix state variable
cbsa_shi_df$entities %>% unique()

# fix the dates
cbsa_shi_df = cbsa_shi_df %>% 
  mutate(Quarter = Quarter %>% openxlsx::convertToDate() %>% 
           lubridate::year()) %>% 
  mutate(year = ifelse(is.na(year),Quarter,year))

# remove list to free up space
rm("cbsa_shi_list")


# pull the states
entities_key = data.frame(full_entities = unique(cbsa_shi_df$entities),
states = lapply(unique(cbsa_shi_df$entities),
       function(x){
         # getting non empty data frame, need to include territories
         temp = str_locate(x,
                           pattern = c(state.abb,
                                       "PR","AS","GU","MP",
                                       "TT","VI")) %>% 
           as.data.frame() %>% filter(!is.na(start))
         x %>% substr(start = min(temp),
                      stop = max(temp)) %>% return()
       }) %>% unlist())

# add states
cbsa_shi_df = cbsa_shi_df %>% 
    mutate(states = entities %>% plyr::mapvalues(from = entities_key$full_entities,
                                                 to = entities_key$states)) %>% 
    mutate(states = ifelse(states == "MISSIN",NA,states))

# there are some values that need to be changed from character to numeric as well as 
# negatives removed where they should be

# check the summary to get an idea
cbsa_shi_df %>% summary()

# make total units numeric
cbsa_shi_df$total_units = cbsa_shi_df$total_units %>% as.numeric()

# remove negatives
for(i in 12:ncol(cbsa_shi_df)){
  cbsa_shi_df[,i][cbsa_shi_df[,i]<0&!is.na(cbsa_shi_df[,i])] = NA
}

# check summary for each summary level
lapply(unique(cbsa_shi_df$program_label),function(x){
  cbsa_shi_df %>% 
    filter(program_label == x) %>% 
    summary() %>% return()
})

# save the data
save(cbsa_shi_df,file = "../Data/Cleaned Data/cbsa_shi_df.RData")

# check tbl summary
cbsa_shi_df %>% 
  select(-(year:sumlevel),-program,-name,-sub_program) %>% 
  tbl_summary(by = program_label)
