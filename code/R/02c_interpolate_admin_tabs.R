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
source("code/R/functions/recompute_total_avg.R")
source("code/R/functions/enforce_avg_strictly_inside.R")
source("code/R/functions/bra_nonincome_shares.R")
source("code/R/functions/dom_corrections.R")

#define target fractiles 
p_grid <- c(
  seq(0.9,     0.99,    by = 0.01),
  seq(0.991,   0.999,   by = 0.001),
  seq(0.9991,  0.9999,  by = 0.0001)
)

#gpinterize some countries (SLV is read pre-built below: 02b2)
ctries <- c("URY", "ARG", "BRA", "CHL", "DOM", "PER", "COL") %>%
  set_names()
countries1 <- map_dfr(ctries, gpinterize_country, .id = "country")

#corrections to Brazil and the Dominican Republic, applied to the income of
#each top group (no re-ranking, so thr and avg keep the original ranking).
#Decided with the coauthors 2026-10-14/15; see results_v2.tex, last section.
# BRA: remove non-income items (bra_nonincome_shares.R) and split joint
#      declarations in two: a share J_SPLIT of top income is on joint returns,
#      and with a Pareto tail (a = b / (b - 1)) splitting them scales top-group
#      income by ((1 - J) + J * 2^(1 - a))^(1 / a).
# DOM: subtract the lump sums and, in 2012-2016, the missing cost of sales
#      (dom_corrections.R).
J_SPLIT <- 0.144  # Receita GN AC2021-24 (Tab2 by declaration type, Tab8 items)
bra_s1 <- bra_nonincome_shares(p_grid) %>%
  transmute(country = "BRA", year, p = round(p, 6), s1)
dom_rm <- dom_corrections(p_grid) %>%
  transmute(country = "DOM", year, p = round(p, 6), remove, dom_adults = adults)
countries1 <- countries1 %>%
  mutate(p = round(p, 6)) %>%
  left_join(bra_s1, by = c("country", "year", "p")) %>%
  left_join(dom_rm, by = c("country", "year", "p")) %>%
  mutate(a = b / (b - 1),
         topavg = case_when(
           country == "BRA" ~ topavg * (1 - s1) *
             ((1 - J_SPLIT) + J_SPLIT * 2^(1 - a))^(1 / a),
           country == "DOM" ~ topavg - remove / ((1 - p) * dom_adults),
           TRUE ~ topavg))
stopifnot(!anyNA(countries1$topavg[countries1$country %in% c("BRA", "DOM")]),
          all(countries1$b[countries1$country == "BRA"] > 1),
          all(countries1$topavg > 0, na.rm = TRUE))
countries1 <- countries1 %>% select(-c(b, a, s1, remove, dom_adults))

#define function for other countries (microdata)
load_other_tabs <- function(ctry, path, pattern) {
  files <- list.files(path, pattern = pattern, full.names = T)
  df_tabs <- map_dfr(files, function(f) {
    lin_val <- sub(".*([A-Za-z0-9]{4})\\.xlsx$", "\\1", basename(f))
    df <- read_excel(f) %>% mutate(country = paste0(ctry))
    df$year <- lin_val
    df <- df[, !grepl("^__", names(df))]
    df <- select(df, country, year, p, thr, bracketavg, topavg)
    return(df)
  })
}

#bring arg, cri and mex 
#arg_tabs <- load_other_tabs(ctry = "ARG", path = "input_data/admin_data/ARG", pattern = "wage.*\\.xlsx$")
cri_tabs <- load_other_tabs(ctry = "CRI", path = "input_data/admin_data/CRI", pattern = "diverse.*\\.xlsx$")
mex_tabs <- load_other_tabs(ctry = "MEX", path = "input_data/admin_data/MEX/_clean", pattern = "total-pre.*\\.xlsx$")

slv_tabs <- load_other_tabs(ctry = "SLV", path = "input_data/admin_data/SLV/_clean", pattern = "^total-pre-SLV-[0-9]{4}\\.xlsx$")

#bring all toghether
countries2 <- rbind(cri_tabs, mex_tabs, slv_tabs) %>%
  rename(avg = `bracketavg`) %>% 
  mutate(minp = NA)

#keep two versions 
all <- rbind(countries1, countries2) %>% mutate(p = round(p, 6)) 
sel <- all %>% filter(p %in% c(0.9, 0.99, 0.999, 0.9999))

#make room 
if (!dir.exists("output")) dir.create("output")
if (!dir.exists("output/gpinter")) dir.create("output/gpinter")

write.csv(all, "output/gpinter/detailed.csv")
write.csv(sel, "output/gpinter/selected.csv")


