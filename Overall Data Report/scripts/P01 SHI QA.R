# Here is where we will read in the SHI data and create QA reports
library(tidyverse)
library(gtsummary)
library(ggplot2)
library(gt)

# read in data
load("../Data/Cleaned Data/cbsa_shi_df.RData")

# The data is not tidy and broken into the various programs such as S8 and S236,
# we will break them down from their respective categories and examine data 
# reported over the years

# examine the different categories
unique(cbsa_shi_df$program_label)

# I realize that Section 8 NC/SR and Project Based Section 8 are the same and should be 
# classified together



###############################
###### Public Housing #########
###############################



# change gt theme
theme_gtsummary_compact()
theme_gtsummary_printer()
# fix font size
fontsize=5
ph_tbl = cbsa_shi_df %>% 
  filter(grepl("Public Housing",program_label)) %>% 
  select(year,total_units,people_total,
         rent_per_month,hh_income,pct_female_head,
         pct_female_head_child,pct_movein,
         pct_disabled_all,pct_overhoused,pct_minority,
         months_waiting,months_from_movein,spending_per_month,
         tpoverty) %>% 
  mutate(ratio_hhi_rent = round(100*(12*rent_per_month)/hh_income,2)) %>% 
  tbl_summary(by = year,
              label = list(total_units ~ "Total Units",
                           people_total ~ "People Total",
                           rent_per_month ~ "Rent Per Month",
                           hh_income ~ "Household Income",
                           ratio_hhi_rent ~ "% of Yearly Rent to HHI",                           pct_female_head ~ "% of Female Lead Household",
                           pct_female_head_child ~ "% of Female Lead Household With Child",
                           pct_movein ~ "% of households who were in the program less than a year from the date of Picture snapshot ",
                           pct_disabled_all ~ "% of all persons in assisted households who have a disability",
                           pct_overhoused ~ "% More Bedrooms Than People",
                           pct_minority ~ "% Minority Race",
                           months_waiting ~ "Average Months on Waiting List",
                           months_from_movein ~ "Average months since households moved in as difference between the admission date
and date of Picture snapshot",
                           spending_per_month ~ "Average HUD Expenditure per Month ($)",
                           tpoverty ~ "Surrounding census tract % in Poverty"),
              type = all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{sum}",
                                                    "{mean} ({sd})",
                                                    "{median} ({p25}, {p75})",
                                                    "{min}, {max}") )) %>% 
  modify_header(label = "**Measured by CBSA**") %>% 
  bold_labels() %>% 
  as_gt() 


###############################
###### Section 8      #########
###############################
s8_tbl = cbsa_shi_df %>% 
  filter(grepl("Section 8",program_label)) %>% 
  select(year,total_units,people_total,
         rent_per_month,hh_income,pct_female_head,
         pct_female_head_child,pct_movein,
         pct_disabled_all,pct_overhoused,pct_minority,
         months_from_movein,spending_per_month,
         tpoverty) %>% 
  mutate(ratio_hhi_rent = round(100*(12*rent_per_month)/hh_income,2)) %>% 
  tbl_summary(by = year,
              label = list(total_units ~ "Total Units",
                           people_total ~ "People Total",
                           rent_per_month ~ "Rent Per Month",
                           hh_income ~ "Household Income",
                           ratio_hhi_rent ~ "% of Yearly Rent to HHI",                           pct_female_head ~ "% of Female Lead Household",
                           pct_female_head_child ~ "% of Female Lead Household With Child",
                           pct_movein ~ "% of households who were in the program less than a year from the date of Picture snapshot ",
                           pct_disabled_all ~ "% of all persons in assisted households who have a disability",
                           pct_overhoused ~ "% More Bedrooms Than People",
                           pct_minority ~ "% Minority Race",
                           months_from_movein ~ "Average months since households moved in as difference between the admission date
and date of Picture snapshot",
                           spending_per_month ~ "Average HUD Expenditure per Month ($)",
                           tpoverty ~ "Surrounding census tract % in Poverty"),
              type = all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{sum}",
                                                    "{mean} ({sd})",
                                                    "{median} ({p25}, {p75})",
                                                    "{min}, {max}") )) %>% 
  modify_header(label = "**Section 8
                Measured by CBSA**") %>% 
  bold_labels() %>% 
  as_gt() 




###############################
#####    Section 236  #########
###############################
s236_tbl = cbsa_shi_df %>% 
  filter(grepl("236",program_label)) %>% 
  select(year,total_units,people_total,
         rent_per_month,hh_income,pct_female_head,
         pct_female_head_child,pct_movein,
         pct_disabled_all,pct_overhoused,pct_minority,
         months_from_movein,spending_per_month,
         tpoverty) %>% 
  mutate(ratio_hhi_rent = round(100*(12*rent_per_month)/hh_income,2)) %>% 
  tbl_summary(by = year,
              label = list(total_units ~ "Total Units",
                           people_total ~ "People Total",
                           rent_per_month ~ "Rent Per Month",
                           hh_income ~ "Household Income",
                           ratio_hhi_rent ~ "% of Yearly Rent to HHI",                           pct_female_head ~ "% of Female Lead Household",
                           pct_female_head_child ~ "% of Female Lead Household With Child",
                           pct_movein ~ "% of households who were in the program less than a year from the date of Picture snapshot ",
                           pct_disabled_all ~ "% of all persons in assisted households who have a disability",
                           pct_overhoused ~ "% More Bedrooms Than People",
                           pct_minority ~ "% Minority Race",
                           months_from_movein ~ "Average months since households moved in as difference between the admission date
and date of Picture snapshot",
                           spending_per_month ~ "Average HUD Expenditure per Month ($)",
                           tpoverty ~ "Surrounding census tract % in Poverty"),
              type = all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{sum}",
                                                    "{mean} ({sd})",
                                                    "{median} ({p25}, {p75})",
                                                    "{min}, {max}") )) %>% 
  modify_header(label = "**Section 236
                Measured by CBSA**") %>% 
  bold_labels() %>% 
  as_gt() 

###############################
###### Multi-Family Housing ###
###############################

mf_tbl = cbsa_shi_df %>% 
  filter(grepl("Multi-",program_label)) %>% 
  select(year,total_units,people_total,
         rent_per_month,hh_income,pct_female_head,
         pct_female_head_child,pct_movein,
         pct_disabled_all,pct_overhoused,pct_minority,
         months_from_movein,spending_per_month,
         tpoverty) %>% 
  mutate(ratio_hhi_rent = round(100*(12*rent_per_month)/hh_income,2)) %>% 
  tbl_summary(by = year,
              label = list(total_units ~ "Total Units",
                           people_total ~ "People Total",
                           rent_per_month ~ "Rent Per Month",
                           hh_income ~ "Household Income",
                           ratio_hhi_rent ~ "% of Yearly Rent to HHI",                           pct_female_head ~ "% of Female Lead Household",
                           pct_female_head_child ~ "% of Female Lead Household With Child",
                           pct_movein ~ "% of households who were in the program less than a year from the date of Picture snapshot ",
                           pct_disabled_all ~ "% of all persons in assisted households who have a disability",
                           pct_overhoused ~ "% More Bedrooms Than People",
                           pct_minority ~ "% Minority Race",
                           months_from_movein ~ "Average months since households moved in as difference between the admission date
and date of Picture snapshot",
                           spending_per_month ~ "Average HUD Expenditure per Month ($)",
                           tpoverty ~ "Surrounding census tract % in Poverty"),
              type = all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{sum}",
                                                    "{mean} ({sd})",
                                                    "{median} ({p25}, {p75})",
                                                    "{min}, {max}") )) %>% 
  modify_header(label = "**Multi-Family
                Measured by CBSA**") %>% 
  bold_labels() %>% 
  as_gt() 


###############################
###### Section 811          ###
###############################



s811_tbl = cbsa_shi_df %>% 
  filter(grepl("811",program_label)) %>% 
  select(year,total_units,people_total,
         rent_per_month,hh_income,pct_female_head,
         pct_female_head_child,pct_movein,
         pct_disabled_all,pct_overhoused,pct_minority,
         months_from_movein,spending_per_month,
         tpoverty) %>% 
  mutate(ratio_hhi_rent = round(100*(12*rent_per_month)/hh_income,2)) %>% 
  tbl_summary(by = year,
              label = list(total_units ~ "Total Units",
                           people_total ~ "People Total",
                           rent_per_month ~ "Rent Per Month",
                           hh_income ~ "Household Income",
                           ratio_hhi_rent ~ "% of Yearly Rent to HHI",
                           pct_female_head ~ "% of Female Lead Household",
                           pct_female_head_child ~ "% of Female Lead Household With Child",
                           pct_movein ~ "% of households who were in the program less than a year from the date of Picture snapshot ",
                           pct_disabled_all ~ "% of all persons in assisted households who have a disability",
                           pct_overhoused ~ "% More Bedrooms Than People",
                           pct_minority ~ "% Minority Race",
                           months_from_movein ~ "Average months since households moved in as difference between the admission date
and date of Picture snapshot",
                           spending_per_month ~ "Average HUD Expenditure per Month ($)",
                           tpoverty ~ "Surrounding census tract % in Poverty"),
              type = all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{sum}",
                                                    "{mean} ({sd})",
                                                    "{median} ({p25}, {p75})",
                                                    "{min}, {max}") )) %>% 
  modify_header(label = "**Section 811
                Measured by CBSA**") %>% 
  bold_labels() %>% 
  as_gt() 
# save env for markdown
save.image("RData/P01_env.RData")

# Here is where we render
file_name = paste0(Sys.Date(),"_SHI_Picture_Report", ".docx") %>% gsub("-","_",.)
quarto::quarto_render("markdowns/Subsidized Housing Picture.qmd",
              output_file = file_name, 
              output_format = "docx")
# move output
file.rename(from = file_name,
            to = "Reports/2024_05_22_SHI_Picture_Report.docx")

