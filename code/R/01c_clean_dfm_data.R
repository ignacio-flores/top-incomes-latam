
library(readr)
library(tidyr)
library(magrittr)
library(dplyr)

dfm <- read_csv("input_data/derosa_flores_morgan/smicrofile_long_grouped_jan2024.csv") 
fil <- dfm %>% filter(step == "bfm" & unit == "esn" & variable %in% c('adpop', 'average')) %>% 
  select(country, year, variable, value)

ready <- pivot_wider(fil, id_cols = c('country', 'year'), names_from = 'variable', values_from = 'value') %>% 
  rename(adpop_bfm = `adpop`, average_income_bfm = `average`) %>% 
  mutate(bfm_totinc = average_income_bfm * adpop_bfm)

path <- "intermediary_data"
if (!dir.exists(path)) dir.create(path)
path <- "intermediary_data/dfm_totals"
if (!dir.exists(path)) dir.create(path)

write.csv(ready, "intermediary_data/dfm_totals/dfm_denominator.csv")
