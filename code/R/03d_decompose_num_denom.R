###############################################
# 03d - WHY ARE OUR TOP 1% SHARES ABOVE DINA?
#       NUMERATOR AND DENOMINATOR IN LEVELS
#
# A top share is a ratio. When ours comes out above a distribuciones series,
# it is because our numerator is larger, our denominator is smaller, or both.
# The share figures in 03c cannot separate those. This script puts numerator
# and denominator side by side in levels, in 2020 USD per adult, for the three
# countries the coauthors flagged: Brazil, Colombia and El Salvador.
#
# WHAT IS COMPARABLE WITH WHAT
#
# Everything here is nominal local currency in the source files (ours and
# theirs alike) and is deflated to 2020 USD with the same WID price index and
# 2020 market exchange rate that 03b section 3.3 uses, so the levels in these
# figures are directly comparable with denom_percap_panel_all.pdf.
#
# One comparison is exact and one is not, and the difference matters:
#
#   * The BFM denominator panel is LIKE FOR LIKE on the denominator. Our BFM
#     denominator is the distribuciones bfm average income per adult, put on
#     our adult count (03a builds it as average x WID adults). Verified below:
#     per adult it is identical to machine precision across all overlapping
#     country-years. So in that panel the whole difference in shares is a
#     numerator difference, and the ratio of shares equals the ratio of
#     per-adult numerators exactly.
#
#   * The Broad and Narrow panels are NOT like for like against DINA. Ours are
#     household-sector aggregates; DINA's denominator is pre-tax national
#     income, which additionally contains undistributed corporate profits, net
#     taxes on production and a residual. It is roughly 1.5x larger. A higher
#     share against DINA is therefore expected arithmetic, not an anomaly.
#
# A caveat on "per adult": we divide by WID adults and they divide by their own
# adpop. For the BFM denominator this is handled in 03a, so the per-adult
# figures match and the totals differ by WID adults / adpop.
###############################################

rm(list = ls())

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(readxl)
  library(showtext)
  library(sysfonts)
})

font_add_google("EB Garamond", "garamond")
showtext_auto()

###############################################
# 0. PATHS AND SETTINGS
###############################################

ROOT <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam"

PATH_TOP_INCOME_DF <- file.path(ROOT, "intermediary_data/top_income_df.csv")
PATH_DIST <- file.path(ROOT,
  "input_data/distribuciones_data/smicrofile_long_grouped_jan2024.csv")
PATH_DEFLATE_NER <- file.path(ROOT, "input_data/deflate_and_ner")

PATH_FIGURES          <- file.path(ROOT, "output/figures/decomposition")
PATH_TABLES           <- file.path(ROOT, "output/tables")
PATH_FIGURES_OVERLEAF <- "C:/Users/dsanc/Dropbox/Apps/Overleaf/aeq_top_incomes/figures"

MIRROR_OVERLEAF <- TRUE

for (d in c(PATH_FIGURES, PATH_TABLES)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

P_TARGET  <- 0.99
BASE_YEAR <- 2020

COUNTRIES <- c("BRA", "COL", "SLV")

# Representative year per country for the tables. Chosen as the latest year in
# which that country has the most of its own denominators available alongside
# all four distribuciones steps. El Salvador never has the narrow denominator
# and has the broad one only for 2014-2017, hence 2017.
TABLE_YEAR <- c(BRA = 2021, COL = 2021, SLV = 2017)

country_labels <- c("BRA" = "Brazil", "COL" = "Colombia", "SLV" = "El Salvador")

denom_concept_labels <- c(
  "upper"  = "Broad",
  "middle" = "Narrow",
  "lower"  = "BFM denominator"
)
panels <- c("Broad", "Narrow", "BFM denominator")

BENCH_STEPS <- c(
  "raw" = "Survey",
  "bfm" = "Survey adjusted tax data (BFM_all)",
  "res" = "Scaled DINA",
  "nat" = "DINA"
)

# Same palette as 03c section 9 so the two sets of figures read together.
series_levels <- c("Tax", unname(BENCH_STEPS))
series_colors <- c(
  "Tax"                                = "#C0392B",
  "Survey"                             = "#9A9A9A",
  "Survey adjusted tax data (BFM_all)" = "#27AE60",
  "Scaled DINA"                        = "#3E8FC1",
  "DINA"                               = "#1F4E79"
)
series_styles <- c(
  "Tax"                                = "solid",
  "Survey"                             = "dotted",
  "Survey adjusted tax data (BFM_all)" = "longdash",
  "Scaled DINA"                        = "dotdash",
  "DINA"                               = "dashed"
)
series_shapes <- c(
  "Tax"                                = 16,
  "Survey"                             = 15,
  "Survey adjusted tax data (BFM_all)" = 18,
  "Scaled DINA"                        = 4,
  "DINA"                               = 17
)

safe_filename <- function(x) gsub("[^A-Za-z0-9_.-]", "_", x)

###############################################
# 1. 2020 USD CONVERSION
# Reproduces 03b section 3.3 exactly:
#   conv = (PI_2020 / PI_year) / ner_2020
###############################################

ner <- read_excel(file.path(PATH_DEFLATE_NER, "ner_2020.xlsx")) %>%
  rename(xrate = lcu_per_usd_2020)

priceindex <- read_excel(file.path(PATH_DEFLATE_NER, "deflator_widpriceindex.xlsx"))

conv <- priceindex %>%
  inner_join(priceindex %>% filter(year == BASE_YEAR) %>%
               select(country, pi_base = priceindex),
             by = "country") %>%
  mutate(deflator = pi_base / priceindex) %>%
  inner_join(ner, by = "country") %>%
  transmute(country, year, conv = deflator / xrate)

###############################################
# 2. OUR SIDE
# numerator = average income of the top 1% x the number of adults in it.
###############################################

ours <- read_csv(PATH_TOP_INCOME_DF, show_col_types = FALSE) %>%
  filter(in_coverage,
         as.numeric(p) == P_TARGET,
         !is.na(denom_total),
         country %in% COUNTRIES) %>%
  transmute(country, year,
            series      = "Tax",
            denom_label = denom_concept_labels[denom_concept],
            denom_total,
            num_total   = topavg * (1 - P_TARGET) * pop,
            adults      = pop) %>%
  distinct()

###############################################
# 3. THEIR SIDE
# average and adpop exist only for group = "tot", so the top 1% income is
# recovered as inc_sh x total. That is definitional, not an approximation.
###############################################

dist_raw <- read_csv(PATH_DIST, show_col_types = FALSE) %>%
  filter(unit == "esn", step %in% names(BENCH_STEPS), country %in% COUNTRIES)

dist_tot <- dist_raw %>%
  filter(group == "tot", variable %in% c("average", "adpop")) %>%
  select(country, year, step, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value)

dist_sh <- dist_raw %>%
  filter(group == "t1", variable == "inc_sh") %>%
  select(country, year, step, inc_sh = value)

theirs <- inner_join(dist_tot, dist_sh, by = c("country", "year", "step")) %>%
  transmute(country, year,
            series      = unname(BENCH_STEPS[step]),
            denom_total = average * adpop,
            num_total   = inc_sh * average * adpop,
            adults      = adpop)

###############################################
# 4. ASSEMBLE IN 2020 USD PER ADULT
###############################################

to_usd_pa <- function(df) {
  df %>%
    inner_join(conv, by = c("country", "year")) %>%
    mutate(denom_pa = denom_total / adults * conv,
           num_pa   = num_total   / adults * conv,
           share    = num_total / denom_total)
}

ours_pa   <- to_usd_pa(ours)
theirs_pa <- to_usd_pa(theirs)

# The benchmark series do not depend on our denominator choice, so they are
# repeated in every panel; only our own line moves. Same convention as 03c.
bench_panelled <- theirs_pa %>%
  crossing(denom_label = panels)

decomp <- bind_rows(
    ours_pa   %>% select(country, year, series, denom_label, denom_pa, num_pa, share),
    bench_panelled %>% select(country, year, series, denom_label, denom_pa, num_pa, share)
  ) %>%
  mutate(
    denom_label = factor(denom_label, levels = panels),
    series      = factor(series, levels = series_levels)
  )

###############################################
# 5. IDENTITY CHECK
# Our BFM denominator must equal the distribuciones bfm total per adult
# exactly. If this ever stops holding, the "like for like" claim in the paper
# is void, so it is asserted rather than merely printed.
###############################################

id_check <- ours_pa %>%
  filter(denom_label == "BFM denominator") %>%
  select(country, year, ours = denom_pa) %>%
  inner_join(theirs_pa %>%
               filter(series == "Survey adjusted tax data (BFM_all)") %>%
               select(country, year, theirs = denom_pa),
             by = c("country", "year")) %>%
  mutate(rel_diff = abs(ours - theirs) / theirs)

message(sprintf(
  "Identity check, BFM denominator vs distribuciones bfm total, per adult: %d country-years, max relative difference %.3g",
  nrow(id_check), max(id_check$rel_diff)))
stopifnot(max(id_check$rel_diff) < 1e-9)

###############################################
# 6. TABLE DATA
###############################################

table_rows <- decomp %>%
  filter(!is.na(denom_pa)) %>%
  # our own rows are concept-specific; the benchmarks are not, so take them once
  filter(series == "Tax" | denom_label == "Broad") %>%
  mutate(row_label = if_else(series == "Tax",
                             as.character(denom_label),
                             as.character(series))) %>%
  inner_join(tibble(country = names(TABLE_YEAR), year = unname(TABLE_YEAR)),
             by = c("country", "year")) %>%
  transmute(country, year, row_label,
            denom_pa, num_pa,
            top1_avg = num_pa * 100,   # per-adult numerator x 100 = mean income of a top-1% adult
            share) %>%
  arrange(match(country, COUNTRIES),
          match(row_label, c(panels, unname(BENCH_STEPS))))

write_csv(table_rows, file.path(PATH_TABLES, "decomposition_top1.csv"))
message("Wrote: ", file.path(PATH_TABLES, "decomposition_top1.csv"))

print(as.data.frame(table_rows %>%
  mutate(across(c(denom_pa, num_pa, top1_avg), ~ round(.x)),
         share = round(share, 4))))

###############################################
# 7. PLOT
###############################################

usd_axis <- function() {
  scale_y_continuous(
    labels = scales::label_number(prefix = "$", big.mark = ",", accuracy = 1),
    breaks = scales::pretty_breaks(n = 5),
    minor_breaks = NULL,
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.06))
  )
}

theme_decomp <- function() {
  theme_classic(base_family = "garamond", base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey75", linewidth = 0.35, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = NA),
      strip.text       = element_text(size = 13, face = "bold"),
      plot.title       = element_text(size = 18, face = "bold", hjust = 0),
      plot.subtitle    = element_text(size = 13, hjust = 0, color = "grey25"),
      axis.text.x  = element_text(size = 11),
      axis.text.y  = element_text(size = 10),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.line  = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      legend.position   = "bottom",
      legend.text       = element_text(size = 13, family = "garamond"),
      legend.key        = element_rect(fill = "white", color = NA),
      legend.key.width  = unit(1.6, "lines"),
      legend.background = element_blank(),
      panel.spacing = unit(1.0, "lines"),
      plot.margin = margin(8, 8, 8, 8)
    )
}

# which = "denom" or "num"
plot_decomp <- function(iso3, which = c("denom", "num")) {

  which <- match.arg(which)
  value_col <- if (which == "denom") "denom_pa" else "num_pa"

  df <- decomp %>%
    filter(country == iso3, !is.na(.data[[value_col]])) %>%
    rename(value = !!value_col)

  if (nrow(df) == 0) return(NULL)

  subtitle <- if (which == "denom") {
    "Total income per adult, the denominator of the top 1% share"
  } else {
    paste0("Top 1% income per adult, the numerator of the top 1% share. ",
           "Our line is identical across panels;\nthe panels differ only in which years have a denominator.")
  }

  ggplot(df, aes(x = year, y = value,
                 color = series, linetype = series, group = series)) +
    geom_line(linewidth = 0.80) +
    geom_point(aes(shape = series), size = 1.7) +
    facet_wrap(~ denom_label, ncol = length(panels), drop = FALSE) +
    scale_color_manual(values = series_colors, drop = FALSE) +
    scale_linetype_manual(values = series_styles, drop = FALSE) +
    scale_shape_manual(values = series_shapes, drop = FALSE) +
    usd_axis() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4),
                       minor_breaks = NULL,
                       expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = "Year",
         y = "2020 USD per adult",
         title = country_labels[[iso3]],
         subtitle = subtitle,
         color = NULL, linetype = NULL, shape = NULL) +
    theme_decomp() +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
}

save_plot_both <- function(g, filename, width = 12, height = 6.4) {
  if (is.null(g)) {
    message("Skipped (no data): ", filename)
    return(invisible(NULL))
  }
  out_file <- safe_filename(filename)
  out_path <- file.path(PATH_FIGURES, out_file)
  ggsave(filename = out_path, plot = g, device = cairo_pdf,
         width = width, height = height, units = "in")
  message("Saved: ", out_path)
  if (MIRROR_OVERLEAF) {
    file.copy(out_path, file.path(PATH_FIGURES_OVERLEAF, out_file), overwrite = TRUE)
    message("Mirrored to Overleaf: ", file.path(PATH_FIGURES_OVERLEAF, out_file))
  }
}

for (iso3 in COUNTRIES) {
  save_plot_both(plot_decomp(iso3, "denom"),
                 paste0("decomp_denom_top1_", iso3, ".pdf"))
  save_plot_both(plot_decomp(iso3, "num"),
                 paste0("decomp_num_top1_", iso3, ".pdf"))
}
