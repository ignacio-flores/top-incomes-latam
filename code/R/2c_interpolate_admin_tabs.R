# devtools::install_github("world-inequality-database/gpinter")

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(gpinter)
library(tibble)
library(stringr)

#load necessary functions 
source("code/R/functions/fit_and_tab.R")
source("code/R/functions/gpinterize_country.R")
source("code/R/functions/collapse_and_clean.R")
source("code/R/functions/enforce_avg_strictly_inside.R")

#define target fractiles 
p_grid <- c(
  seq(0.9,     0.99,    by = 0.01),
  seq(0.991,   0.999,   by = 0.001),
  seq(0.9991,  0.9999,  by = 0.0001)
)

#gpinterize some countries 
ctries <- c("BRA", "CHL", "COL", "DOM", "PER", "SLV", "URY") %>% 
  set_names()
countries1 <- map_dfr(ctries, gpinterize_country, .id = "country") %>%
  select(-c("b"))

#define function for other countries (microdata)
load_other_tabs <- function(ctry, path, pattern) {
  files <- list.files(path, pattern = pattern, full.names = T)
  df_tabs <- map_dfr(files, function(f) {
    lin_val <- sub(".*([A-Za-z0-9]{4})\\.xlsx$", "\\1", basename(f))
    df <- read_excel(f) %>% filter(p >= 0.9) %>% mutate(country = paste0(ctry))
    df$year <- lin_val
    df <- df[, !grepl("^__", names(df))]
    df <- select(df, country, year, p, thr, bracketavg, topavg)
    return(df)
  })
  
}

#bring arg, cri and mex 
arg_tabs <- load_other_tabs(ctry = "ARG", path = "input_data/admin_data/ARG", pattern = "\\.xlsx$")
cri_tabs <- load_other_tabs(ctry = "CRI", path = "input_data/admin_data/CRI", pattern = "diverse.*\\.xlsx$")
mex_tabs <- load_other_tabs(ctry = "MEX", path = "input_data/admin_data/MEX/_clean", pattern = "\\.xlsx$")


#last exception for ecu 

#bring all toghether 
countries2 <- rbind(arg_tabs, cri_tabs, mex_tabs) %>% 
  rename(avg = `bracketavg`)

#keep two versions 
all <- rbind(countries1, countries2)
sel <- all %>% filter(p %in% c(0.9, 0.99, 0.999, 0.9999))

#make room 
if (!dir.exists("output")) dir.create("output")
if (!dir.exists("output/gpinter")) dir.create("output/gpinter")

write.csv(all, "output/gpinter/detailed.csv")
write.csv(sel, "output/gpinter/selected.csv")


