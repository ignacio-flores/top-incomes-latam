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

# -------------------------------------------------------------------------
# COVERAGE FLAG  (added 2026-09)
# -------------------------------------------------------------------------
# THE PROBLEM
# A top group can only be measured if the tax tabulation actually reaches
# it. ARG's tabulation covers only the richest ~2-4% of adults, SLV's ~2-7%,
# COL's ~5-9% before 2019. Asking those countries for a "top 10%" is asking
# about people who are not in the data.
#
# gpinter answers anyway. Everyone below the lowest observed bracket was
# assumed to earn zero, so every group WIDER than the data returns the same
# total. ARG 2019 gives an identical number for the top 10%, 9%, 8% ... 3%.
# The published "ARG top 10% = 6%" is really the share of its top 2.4%, i.e.
# just the people who file. Affects the top 10% only:
#     ARG all years, SLV all years, COL 2014-2018   (73 rows in top_income_df)
# Top 1% and narrower sit inside coverage everywhere and are unaffected.
#
# WHY THOSE ROWS EXIST AT ALL
# 02c line ~18 hardcodes the percentile grid to start at 0.9 for every
# country, whatever that country observes. The interpolator is asked for the
# top 10% and always returns something. Nothing between there and the figures
# ever says no. Saying no is what this block adds.
#
# WHAT minp IS (and what the upstream fixes will NOT do)
# minp = the lowest p a country's tabulation actually observes. It is a
# recorded fact sitting next to the data, nothing more. It is ALREADY correct
# for 62 of the 73 bad rows, and those rows are still plotted, because no
# code has ever compared p against minp. The upstream fixes below only fill
# in the remaining blank cells. They delete no rows and change no numbers.
# On their own they would leave every figure exactly as it is today. The
# filter has to live somewhere; for now it lives here.
#
# TARGET: read minp straight from selected.csv and compare.
# ACTIVE: detect the plateau in detailed.csv, because minp has blanks in the
#         exact years we need. Both branches produce `in_coverage`, so 03b
#         does not change when we switch.
#
# UPSTREAM TO-DO -- bookkeeping only, neither edit removes anything
#   1. code/R/functions/gpinterize_country.R  (~line 27)
#        add   mutate(country = c)   just before  select(country, year, p)
#      Puts the right country code on the little minp lookup table so the
#      left_join on line 34 finds a match. Effect: one cell goes from blank
#      to 0.9727 for ARG 2013-2018. It is blank today because the row drop
#      on line 17 leaves the sheet's country column empty, and because the
#      BRA 2000/2002/2006 sheets spell it "Brazil" instead of "BRA".
#   2. code/R/02c_interpolate_admin_tabs.R  (~line 51)
#        replace   mutate(minp = NA)   with
#        group_by(country, year) %>% mutate(minp = min(p, na.rm = TRUE)) %>% ungroup()
#      Same idea: MEX and CRI have no minp at all. This fills it in.
#
#   After BOTH: set USE_UPSTREAM_MINP <- TRUE, rerun, and diff against the
#   current output. It should be identical. Then delete the plateau branch.
#   Do not flip it before both are done: coalesce(.., TRUE) keeps rows whose
#   minp is still blank, so ARG 2013-2018 would quietly come back.
#
#   ALTERNATIVE, if you would rather enforce it once at the source:
#   after fixes 1 and 2, add to 02c ~line 56
#        all <- ... %>% mutate(in_coverage = coalesce(p >= minp - 1e-9, TRUE))
#        sel <- all %>% filter(p %in% c(0.9, 0.99, 0.999, 0.9999), in_coverage)
#   Then the bad rows never reach 03, and this whole block plus the filter in
#   03b can be deleted. Same figures either way.
# -------------------------------------------------------------------------

# >>> WHEN THE TWO UPSTREAM FIXES ABOVE ARE DONE, COLLAPSE THIS <<<
# Delete the flag, the if/else scaffolding and the whole else branch.
# Keep ONLY the body of the first branch. What should survive is:
#
#     numerator <- numerator_raw %>%
#       mutate(p = round(p, 6),
#              in_coverage = coalesce(p >= minp - 1e-9, TRUE)) %>%
#       select(country, year, p, thr, avg, topavg, in_coverage)
#
# Do it in two steps so you can check your work: first set the flag to TRUE
# and rerun (output should be byte-identical to the plateau version -- if it
# is not, an upstream fix did not land), then delete the dead branch.
# Do NOT restore the "# ORIGINAL" block at the bottom of this section. That
# is the pre-fix code, it has no in_coverage column, and 03b will error.

USE_UPSTREAM_MINP <- FALSE   # TRUE once both upstream fixes above are in

if (USE_UPSTREAM_MINP) {

  # Straightforward version: a group is reportable if the tabulation
  # reaches it. coalesce(.., TRUE) keeps rows whose minp is still unknown
  # -- without it, any country with minp = NA is silently deleted whole.
  numerator <- numerator_raw %>%
    mutate(p = round(p, 6),
           in_coverage = coalesce(p >= minp - 1e-9, TRUE)) %>%
    select(country, year, p, thr, avg, topavg, in_coverage)

} else {

  # Plateau detection, never reads minp so the NA problem cannot bite.
  # Inside coverage, group income = topavg * (1 - p) must strictly FALL as
  # p rises. On the broken rows it is flat, because the extra people swept
  # in contribute zero. Checked against coverage recomputed from the raw
  # tabulations: 511 rows, 0 disagreements.
  # Limit: needs the plateau to span >= 2 points of the p grid. No current
  # country-year is close to that edge; the upstream fix removes the caveat.
  coverage_flag <- read_csv(file.path(dirname(PATH_NUMERATOR), "detailed.csv"),
                            show_col_types = FALSE) %>%
    mutate(p = round(p, 6), grp_inc = topavg * (1 - p)) %>%
    arrange(country, year, p) %>%
    group_by(country, year) %>%
    mutate(
      flat_here        = !is.na(lead(grp_inc)) &
        abs(grp_inc - lead(grp_inc)) <= 1e-6 * pmax(abs(grp_inc), 1),
      outside_coverage = as.logical(rev(cummax(rev(
        as.integer(coalesce(flat_here, FALSE))))))
    ) %>%
    ungroup() %>%
    select(country, year, p, outside_coverage)

  numerator <- numerator_raw %>%
    mutate(p = round(p, 6)) %>%
    left_join(coverage_flag, by = c("country", "year", "p")) %>%
    mutate(in_coverage = !coalesce(outside_coverage, FALSE)) %>%
    select(country, year, p, thr, avg, topavg, in_coverage)

}

# ORIGINAL (replaced above: dropped minp, so nothing could ever filter on
# coverage). Restore only together with removing the filter in 03b.
# numerator <- numerator_raw %>%
#   select(country, year, p, thr, avg, topavg)

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
