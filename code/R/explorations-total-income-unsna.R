# ================== Packages ==================
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(countrycode)
library(purrr)

# ================== Data path ==================
sna_path <- "output/national_accounts/sna-un.dta"

# ================== Load ==================
df <- read_dta(sna_path)

# Try to get a robust country name
# Prefer iso3 when present; else iso2; fall back to iso column as-is.
iso3 <- if ("_ISO3C_" %in% names(df)) df[["_ISO3C_"]] else NA_character_
iso2 <- if ("iso" %in% names(df)) df[["iso"]] else NA_character_

country_name <- countrycode(iso3, origin = "iso3c", destination = "country.name", warn = FALSE)
missing_name <- is.na(country_name)
country_name[missing_name] <- countrycode(iso2[missing_name], origin = "iso2c", destination = "country.name", warn = FALSE)
# final fallback to the raw iso code
country_name[is.na(country_name)] <- if (!is.null(iso2)) as.character(iso2[is.na(country_name)]) else "Unknown"

df$country_name <- country_name

# ================== Helpers ==================
na_zero <- function(x) ifelse(is.na(x) | x == 0, NA_real_, as.numeric(x))

# Return the first matching column *name* used (rowwise)
first_used_col <- function(data, patterns) {
  cols <- unlist(lapply(patterns, function(p) names(data)[str_detect(names(data), paste0("^", p, "$"))]))
  if (length(cols) == 0) return(rep(NA_character_, nrow(data)))
  mat <- as.data.frame(lapply(cols, function(cn) na_zero(data[[cn]])))
  apply(mat, 1, function(r) {
    i <- which(!is.na(r))[1]
    if (is.na(i)) NA_character_ else cols[i]
  })
}

# Coalesce values rowwise using patterns (value)
coalesce_rowwise <- function(data, patterns) {
  cols <- unlist(lapply(patterns, function(p) names(data)[str_detect(names(data), paste0("^", p, "$"))]))
  if (length(cols) == 0) return(rep(NA_real_, nrow(data)))
  mat <- as.data.frame(lapply(cols, function(cn) na_zero(data[[cn]])))
  apply(mat, 1, function(r) {
    i <- which(!is.na(r))[1]
    if (is.na(i)) NA_real_ else r[i]
  })
}

# Sum across a set of patterns rowwise, and also record which columns contributed (names)
sum_rowwise_with_sources <- function(data, patterns) {
  cols <- unlist(lapply(patterns, function(p) names(data)[str_detect(names(data), paste0("^", p, "$"))]))
  if (length(cols) == 0) {
    vals <- rep(0, nrow(data))
    srcs <- rep(NA_character_, nrow(data))
    return(list(value = vals, sources = srcs))
  }
  mat <- as.data.frame(lapply(cols, function(cn) na_zero(data[[cn]])))
  vals <- rowSums(replace(mat, is.na(mat), 0))
  srcs <- apply(mat, 1, function(r) {
    used <- cols[!is.na(r) & r != 0]
    if (length(used) == 0) NA_character_ else paste(unique(used), collapse = "; ")
  })
  list(value = vals, sources = srcs)
}

modal_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

collapse_unique <- function(x) {
  x <- unique(x[!is.na(x) & nzchar(x)])
  if (!length(x)) return(NA_character_)
  paste(x, collapse = " | ")
}

# ================== Column patterns (actual prefixes) ==================
# Reference income: B5g, household resource preferred; fallback to U; else HH_NPISH
pref_B5g_hh_res <- c("HH_B5g_R","HH_B5g_U","HH_NPISH_B5g_R","HH_NPISH_B5g_U")

# D62 uses: Financial corporations + General government
pref_D62_fc_use <- c("FC_D62_U","FC_D62_R")
pref_D62_gg_use <- c("GG_D62_U","GG_D62_R")

# D61 uses: Households (fallback HH_NPISH)
pref_D61_hh_use <- c("HH_D61_U","HH_D61_R","HH_NPISH_D61_U","HH_NPISH_D61_R")

# K1 uses: Households (fallback HH_NPISH)
pref_K1_hh_use  <- c("HH_K1_U","HH_K1_R","HH_NPISH_K1_U","HH_NPISH_K1_R")

# Total economy B5g (denominator; prefer Resource)
pref_TOT_B5g    <- c("TOT_B5g_R","TOT_B5g_U")

# ================== Compute row-by-row ==================
# Values
B5g_val  <- coalesce_rowwise(df, pref_B5g_hh_res)
D61_val  <- coalesce_rowwise(df, pref_D61_hh_use)
K1_val   <- coalesce_rowwise(df, pref_K1_hh_use)
TOT_val  <- coalesce_rowwise(df, pref_TOT_B5g)

D62_fc   <- sum_rowwise_with_sources(df, pref_D62_fc_use)
D62_gg   <- sum_rowwise_with_sources(df, pref_D62_gg_use)

# Sources (which columns were picked)
src_B5g  <- first_used_col(df, pref_B5g_hh_res)
src_D61  <- first_used_col(df, pref_D61_hh_use)
src_K1   <- first_used_col(df, pref_K1_hh_use)
src_TOT  <- first_used_col(df, pref_TOT_B5g)
src_D62_fc <- D62_fc$sources
src_D62_gg <- D62_gg$sources

df_calc <- df %>%
  mutate(
    country_name = country_name,
    B5g_hh_res   = B5g_val,
    D62_fin      = D62_fc$value,
    D62_gov      = D62_gg$value,
    D61_hh_use   = D61_val,
    K1_hh_use    = K1_val,
    TOT_B5g      = TOT_val,
    TFI          = B5g_hh_res + (D62_fin + D62_gov) - D61_hh_use - K1_hh_use,
    TFI_pct_B5g_total = 100 * TFI / na_zero(TOT_B5g),
    
    # provenance columns
    src_B5g_hh_res = src_B5g,
    src_D62_fin    = src_D62_fc,
    src_D62_gov    = src_D62_gg,
    src_D61_hh_use = src_D61,
    src_K1_hh_use  = src_K1,
    src_TOT_B5g    = src_TOT
  ) %>%
  select(country_name, iso, ctry_srs, year,
         B5g_hh_res, D62_fin, D62_gov, D61_hh_use, K1_hh_use, TOT_B5g, TFI, TFI_pct_B5g_total,
         src_B5g_hh_res, src_D62_fin, src_D62_gov, src_D61_hh_use, src_K1_hh_use, src_TOT_B5g)

# ================== Plot ==================
plot_data <- df_calc %>% filter(!is.na(TFI_pct_B5g_total))

ggplot(
  plot_data,
  aes(x = year, y = TFI_pct_B5g_total,
      color = country_name,           # same color for same country
      linetype = ctry_srs,            # different pattern per series
      shape = ctry_srs,               # different point symbol per series
      group = ctry_srs)
) +
  geom_line() +
  geom_point(size = 1.5) +
  labs(
    title = "Total Fiscal Income as Share of National Income (B.5g total)",
    subtitle = "Color = country; line type & shape = country–series (ctry_srs). Zeros treated as missing.",
    x = "Year", y = "Percent of national income",
    color = "Country", linetype = "Series", shape = "Series"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# ================== Provenance / completeness report ==================
# For each country–series, tell which columns were used
provenance_report <- df_calc %>%
  group_by(country_name, iso, ctry_srs) %>%
  summarise(
    n_years          = n(),
    # modal (most frequently used) source column per component
    modal_B5g        = modal_or_na(src_B5g_hh_res),
    modal_D62_fin    = modal_or_na(src_D62_fin),
    modal_D62_gov    = modal_or_na(src_D62_gov),
    modal_D61        = modal_or_na(src_D61_hh_use),
    modal_K1         = modal_or_na(src_K1_hh_use),
    modal_TOT_B5g    = modal_or_na(src_TOT_B5g),
    # list all sources that ever appeared (unique)
    all_B5g          = collapse_unique(src_B5g_hh_res),
    all_D62_fin      = collapse_unique(src_D62_fin),
    all_D62_gov      = collapse_unique(src_D62_gov),
    all_D61          = collapse_unique(src_D61_hh_use),
    all_K1           = collapse_unique(src_K1_hh_use),
    all_TOT_B5g      = collapse_unique(src_TOT_B5g),
    # how many years produce a ratio
    ratio_years      = sum(!is.na(TFI_pct_B5g_total)),
    .groups = "drop"
  ) %>%
  arrange(country_name, ctry_srs)

# View in console
print(provenance_report, n = 100)

