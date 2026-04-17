###############################################
# 3a – BUILD BASE PANEL: START
# Only loading data + creating a clean structure
###############################################

rm(list = ls())

suppressPackageStartupMessages({
  library(readr)
  library(haven)
  library(dplyr)
  library(tidyr)
  library(writexl)
  
})

###############################################
# 0. PATHS (edit as needed)
###############################################

PATH_NUMERATOR <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/output/gpinter/selected.csv"
PATH_DENOMINATOR1 <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/output/national_accounts/sna-cei.dta"
PATH_DENOMINATOR2 <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/intermediary_data/dfm_totals/dfm_denominator.csv"
PATH_SUPPLEMENT_SNA <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/output/national_accounts/sna-wid.dta"
PATH_POPS <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/input_data/wid_population/pops.dta"

###############################################
# 1. LOAD NUMERATOR (GPINTER SELECTED)
###############################################

numerator_raw <- read_csv(PATH_NUMERATOR, show_col_types = FALSE)

# Keep only the essential columns now.
numerator <- numerator_raw %>%
  select(country, year, p, thr, avg, topavg)

###############################################
# 2. LOAD & PREPARE DENOMINATOR (SNA–CEI)
###############################################

denominator_raw1 <- read_dta(PATH_DENOMINATOR1)
denominator_raw2 <- read_csv(PATH_DENOMINATOR2 , show_col_types = FALSE)


# ------------------------------------------------------
# CONCEPT 1 — SIMPLE DENOMINATOR (B5g = b5n from cei - fcc_hh from wid
# Special rule for SLV & ARG: from 2000 onward use TOT_B5g_wid
# ------------------------------------------------------

# Load the additional WID variable
supplement_raw <- read_dta(PATH_SUPPLEMENT_SNA)

# Keep only what we need from supplement.
# ARG (2000+) uses TOT_B5g_wid (only B5g-related WID series with usable
# coverage for ARG); SLV (2000+) uses HH_B5n_wid.
supplement_vars <- supplement_raw %>%
  select(country, year, cfc_hh, HH_B5n_wid, TOT_B5g_wid)

# ------------------------------------------------------
# CONCEPT 1A — GENERAL: B5g = B5n(CEI) - cfc_hh(WID)
# (computed only if BOTH available)
# ------------------------------------------------------
denom_simple_general <- denominator_raw1 %>%
  select(country, year, B5g_cei) %>%
  left_join(supplement_vars %>% select(country, year, cfc_hh),
            by = c("country", "year")) %>%
  mutate(
    denom_total = if_else(
      !is.na(B5g_cei) & !is.na(cfc_hh),
      B5g_cei - cfc_hh,
      NA_real_
    ),
    denom_concept = "upper",
    denom_source  = "SNA_CEI + SNA_WID"
  ) %>%
  select(country, year, denom_total, denom_concept, denom_source)

# ------------------------------------------------------
# CONCEPT 1B — SPECIAL OVERRIDE: ARG/SLV from 2000+ use TOT_B5g_wid
# ------------------------------------------------------
denom_simple_override <- supplement_vars %>%
  filter(country %in% c("ARG", "SLV"), year >= 2000) %>%
  mutate(
    denom_total = case_when(
      country == "ARG" ~ TOT_B5g_wid,
      country == "SLV" ~ HH_B5n_wid,
      TRUE             ~ NA_real_
    ),
    denom_concept = "upper",
    denom_source  = case_when(
      country == "ARG" ~ "WID TOT_B5g_wid (2000+)",
      country == "SLV" ~ "WID HH_B5n_wid (2000+)",
      TRUE             ~ NA_character_
    )
  ) %>%
  select(country, year, denom_total, denom_concept, denom_source)

# ------------------------------------------------------
# FINAL denom_simple:
# remove ARG/SLV 2000+ from general, then add override
# ------------------------------------------------------
denom_simple <- denom_simple_general %>%
  filter(!(country %in% c("ARG", "SLV") & year >= 2000)) %>%
  bind_rows(denom_simple_override) %>%
  arrange(country, year)



# ------------------------------------------------------
# CONCEPT 2 — bfm DENOMINATOR 
# ------------------------------------------------------

denom_bfm <- denominator_raw2 %>%
  select(country, year, bfm_totinc) %>%
  rename(denom_total = bfm_totinc) %>%
  mutate(
    denom_concept = "lower",
    denom_source  = "BFM"
  )

# ------------------------------------------------------
# CONCEPT 3 — "MIDDLE" SNA-CONSTRUCTED DENOMINATOR (ACTUAL)
# Denom^(3)_actual = B5g_HH + D62 - D61 - D44 - B2g - CFC_HH
#   B5g, D62, D61, D44, B2g from CEI–SNA
#   CFC_HH (cfc_hh) from WID SNA supplement
# ------------------------------------------------------

denom_middle_actual <- denominator_raw1 %>%
  select(
    country, year,
    B5g_cei,   # B5g_HH
    D62_cei,       # D.62
    D61_cei,       # D.61 (611+612+613+614+615)
    D44_cei,       # D.44 (441+442+443)
    B2g_cei        # B.2g (HH operating surplus, gross) proxy for imputed rent
  ) %>%
  left_join(
    supplement_vars %>% select(country, year, cfc_hh),   # P.51c (HH)
    by = c("country", "year")
  ) %>%
  mutate(
    denom_total = if_else(
      !is.na(B5g_cei) &
        !is.na(D62_cei) &
        !is.na(D61_cei) &
        !is.na(D44_cei) &
        !is.na(B2g_cei) &
        !is.na(cfc_hh),
      B5g_cei + D62_cei - D61_cei - D44_cei - B2g_cei - cfc_hh,
      NA_real_
    ),
    denom_concept = "middle",
    denom_source  = "SNA_CEI + SNA_WID"
  ) %>%
  select(country, year, denom_total, denom_concept, denom_source)


# ------------------------------------------------------
# ADD CONCEPT 3 TO THE DENOMINATOR STACK
# ------------------------------------------------------
denominator <- bind_rows(
  denom_simple,
  denom_bfm,
  denom_middle_actual
)


###############################################
# 3. LOAD & PREPARE POPULATION (WID POPS)
###############################################

pops_raw <- read_dta(PATH_POPS)

# Configuration block — easy to change in future
POP_VAR     <- "npopul_adults"   # column used
POP_CONCEPT <- "adults"          # meaning of the column
POP_SOURCE  <- "WID"             # origin of the data

pops <- pops_raw %>%
  filter(year >= 1990) %>%          # <-- NEW: restrict to years >= 1990
  select(country, year, all_of(POP_VAR)) %>%
  rename(pop = all_of(POP_VAR)) %>%
  mutate(
    pop_concept = POP_CONCEPT,
    pop_source  = POP_SOURCE
  )

###############################################
# 3.5 AVAILABILITY CHECK (SANITY TABLE)
# Put this BEFORE inner_join() drops years.
###############################################

# Denominator availability (by concept) — ONLY if denom_total is actually present
avail_denominator <- denominator %>%
  filter(!is.na(denom_total)) %>%              # <-- key fix
  distinct(country, year, denom_concept) %>%
  mutate(has_denominator = TRUE)


avail_numerator <- numerator %>%
  filter(!is.na(thr) | !is.na(avg) | !is.na(topavg)) %>%  # any useful info
  distinct(country, year) %>%
  mutate(has_numerator = TRUE)


# Population availability
avail_population <- pops %>%
  distinct(country, year) %>%
  mutate(has_population = TRUE)

all_concepts <- tibble(denom_concept = c("upper","middle","lower"))

country_year_grid <- bind_rows(
  avail_numerator  %>% select(country, year),
  avail_population %>% select(country, year),
  avail_denominator %>% select(country, year)
) %>%
  distinct() %>%
  crossing(all_concepts)


availability_check <- country_year_grid %>%
  left_join(avail_numerator,   by = c("country","year")) %>%
  left_join(avail_population,  by = c("country","year")) %>%
  left_join(avail_denominator, by = c("country","year","denom_concept")) %>%
  mutate(
    has_numerator   = coalesce(has_numerator, FALSE),
    has_population  = coalesce(has_population, FALSE),
    has_denominator = coalesce(has_denominator, FALSE),
    has_all         = has_numerator & has_population & has_denominator
  ) %>%
  arrange(country, year, denom_concept)



# Optional: quickly see which years will survive the merge
availability_survive <- availability_check %>%
  filter(has_all)


###############################################
# 4. ALIGN YEARS & MERGE ALL SOURCES
###############################################

# First merge numerator × denominator
merged_temp <- numerator %>%
  inner_join(denominator, by = c("country", "year"))

# Now merge with population
top_income_df <- merged_temp %>%
  inner_join(pops, by = c("country", "year"))

###############################################
# 5. CLEAN ENVIRONMENT
###############################################

rm(
  numerator_raw,
  denominator_raw,
  pops_raw,
  numerator,
  denominator,
  pops,
  denom_simple,
  # denom_composite,   # uncomment when it exists
  merged_temp,
  avail_numerator,
  avail_denominator,
  avail_population,
  country_year_grid
  # keep availability_check and availability_survive on purpose
  # so you can inspect why years dropped
)


###############################################
# 6. SAVE INTERMEDIATE OUTPUT (CSV)
###############################################

# Folder for intermediate data
PATH_INTERMEDIATE <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/intermediary_data"

# Create folder if it doesn't exist
if (!dir.exists(PATH_INTERMEDIATE)) {
  dir.create(PATH_INTERMEDIATE, recursive = TRUE)
}

# Save CSVs
write_csv(top_income_df,
          file.path(PATH_INTERMEDIATE, "top_income_df.csv"))

write_csv(availability_check,
          file.path(PATH_INTERMEDIATE, "availability_check.csv"))

write_csv(availability_survive,
          file.path(PATH_INTERMEDIATE, "availability_survive.csv"))
