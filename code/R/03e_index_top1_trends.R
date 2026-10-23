###############################################
# 03e - TRENDS: TOP 1% INCOME AGAINST SURVEY INCOME AND THE BFM DENOMINATOR
#
# The other figures compare levels. These compare only how the series move:
# each one is in real terms and set to 100 in the first year of the tax
# series, and the two distribuciones series are drawn for every year
# distribuciones has, before and after the tax years alike.
#
#   Top 1% income (tax) = topavg x (1 - 0.99) x WID adults   (03a's numerator)
#   Survey income       = distribuciones raw average x WID adults
#   BFM denominator     = distribuciones bfm average x WID adults   (as in 03a)
#
# Two versions: totals, and per adult: the average income of a top-1% adult
# for the tax line and of all adults for the other two (the top 1%'s income
# over all adults would give the same index). Every source is in nominal
# local currency and is deflated with the WID price index, as in 03b and 03d.
# An exchange rate would cancel in an index, so none is used.
#
# BASE YEAR: the first year of the tax series. If distribuciones has no
# survey that year but has one on each side (CHL 2005, MEX 2009), its value
# is interpolated linearly, so its line, drawn straight between surveys,
# crosses 100 there. If the tax series starts before the first survey (BRA
# and SLV, 2000), the base is the first tax year with a survey (2002, 2001).
###############################################

rm(list = ls())

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(readxl)
  library(haven)
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
PATH_SELECTED      <- file.path(ROOT, "output/gpinter/selected.csv")
PATH_DIST <- file.path(ROOT,
  "input_data/distribuciones_data/smicrofile_long_grouped_jan2024.csv")
PATH_POPS       <- file.path(ROOT, "input_data/wid_population/pops.dta")
PATH_PRICEINDEX <- file.path(ROOT, "input_data/deflate_and_ner/deflator_widpriceindex.xlsx")

PATH_FIGURES          <- file.path(ROOT, "output/figures/index")
PATH_TABLES           <- file.path(ROOT, "output/tables")
PATH_FIGURES_OVERLEAF <- "C:/Users/dsanc/Dropbox/Apps/Overleaf/aeq_top_incomes/figures_v2"  # results_v2.tex

MIRROR_OVERLEAF <- TRUE

for (d in c(PATH_FIGURES, PATH_TABLES, PATH_FIGURES_OVERLEAF)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

P_TARGET <- 0.99

COUNTRIES <- c("ARG", "BRA", "CHL", "COL", "CRI", "DOM", "MEX", "PER", "SLV", "URY")

country_labels <- c("ARG" = "Argentina", "BRA" = "Brazil", "CHL" = "Chile", "COL" = "Colombia",
                    "CRI" = "Costa Rica", "DOM" = "Dominican Republic", "MEX" = "Mexico",
                    "PER" = "Peru", "SLV" = "El Salvador", "URY" = "Uruguay")

DIST_STEPS <- c("raw" = "Survey income", "bfm" = "BFM denominator")

# Same colours as the Tax, Survey and BFM lines in 03c and 03d.
series_levels <- c("Top 1% income (tax)", unname(DIST_STEPS))
series_colors <- c("Top 1% income (tax)" = "#C0392B",
                   "Survey income"       = "#9A9A9A",
                   "BFM denominator"     = "#27AE60")
series_styles <- c("Top 1% income (tax)" = "solid",
                   "Survey income"       = "dotted",
                   "BFM denominator"     = "longdash")
series_shapes <- c("Top 1% income (tax)" = 16,
                   "Survey income"       = 15,
                   "BFM denominator"     = 18)

VERSIONS <- c("total" = "Totals", "per_adult" = "Per adult")

safe_filename <- function(x) gsub("[^A-Za-z0-9_.-]", "_", x)

###############################################
# 1. INPUTS
###############################################

adults <- read_dta(PATH_POPS) %>%
  transmute(country, year = as.integer(year), adults = npopul_adults)

priceindex <- read_excel(PATH_PRICEINDEX) %>%
  transmute(country, year = as.integer(year), priceindex)

# Our side: every year with a top 1% in the tabulation's reach. Checked
# against gpinter's output, so no tax year is lost on the way through 03a.
tax <- read_csv(PATH_TOP_INCOME_DF, show_col_types = FALSE) %>%
  filter(country %in% COUNTRIES, abs(p - P_TARGET) < 1e-9) %>%
  distinct(country, year, topavg, pop, in_coverage) %>%
  mutate(year = as.integer(year))
stopifnot(!anyDuplicated(tax[c("country", "year")]))

tax_years_gpinter <- read_csv(PATH_SELECTED, show_col_types = FALSE) %>%
  filter(country %in% COUNTRIES, abs(p - P_TARGET) < 1e-9, !is.na(topavg)) %>%
  distinct(country, year = as.integer(year))
lost <- anti_join(tax_years_gpinter, tax, by = c("country", "year"))
if (nrow(lost) > 0) stop("Tax years in selected.csv but not in top_income_df: ",
                         paste(lost$country, lost$year, collapse = ", "))

tax <- tax %>% filter(in_coverage)

# Their side: average income per adult, the raw and bfm steps.
dist <- read_csv(PATH_DIST, show_col_types = FALSE) %>%
  filter(unit == "esn", group == "tot", variable == "average",
         step %in% names(DIST_STEPS), country %in% COUNTRIES, !is.na(value)) %>%
  transmute(country, year = as.integer(year), series = unname(DIST_STEPS[step]), per_adult = value)
stopifnot(!anyDuplicated(dist[c("country", "year", "series")]))

###############################################
# 2. REAL SERIES, TOTALS AND PER ADULT
###############################################

# avg: the group's average income; group_share: the group's share of adults
levels_long <- bind_rows(
    tax  %>% transmute(country, year, series = "Top 1% income (tax)",
                       avg = topavg, group_share = 1 - P_TARGET, pop),
    dist %>% transmute(country, year, series,
                       avg = per_adult, group_share = 1, pop = NA_real_)
  ) %>%
  left_join(adults, by = c("country", "year")) %>%
  left_join(priceindex, by = c("country", "year"))

stopifnot(!anyNA(levels_long$adults), !anyNA(levels_long$priceindex))
# 03a's numerator uses the same WID adults
with(filter(levels_long, !is.na(pop)), stopifnot(max(abs(pop / adults - 1)) < 1e-12))

levels_long <- levels_long %>%
  mutate(total     = avg * group_share * adults / priceindex,
         per_adult = avg / priceindex) %>%
  select(country, year, series, total, per_adult) %>%
  pivot_longer(c(total, per_adult), names_to = "version", values_to = "value")

###############################################
# 3. BASE YEAR AND INDEX
###############################################

# y is reachable if the series has it or has a year on each side of it
reachable <- function(yrs, y) y %in% yrs || (min(yrs) < y && y < max(yrs))

base_years <- bind_rows(lapply(COUNTRIES, function(c) {
  ty <- sort(tax$year[tax$country == c])
  sy <- lapply(unname(DIST_STEPS), function(s) dist$year[dist$country == c & dist$series == s])
  ok <- sapply(ty, function(y) all(sapply(sy, reachable, y = y)))
  by <- ty[which(ok)[1]]
  tibble(country = c, tax_start = ty[1], base_year = by,
         interpolated = !all(sapply(sy, function(v) by %in% v)))
}))
stopifnot(!anyNA(base_years$base_year))

base_value <- function(yrs, vals, y) {
  if (y %in% yrs) return(vals[yrs == y])
  approx(yrs, vals, xout = y)$y   # linear, between the surveys on either side
}

idx <- levels_long %>%
  inner_join(base_years %>% select(country, base_year), by = "country") %>%
  group_by(country, series, version) %>%
  mutate(base  = base_value(year, value, first(base_year)),
         index = 100 * value / base) %>%
  ungroup()
stopifnot(!anyNA(idx$index))

# every series sits at exactly 100 in the base year when it is observed there
chk <- idx %>% filter(year == base_year)
stopifnot(max(abs(chk$index - 100)) < 1e-9)

message("Base years:")
print(as.data.frame(base_years))

write_csv(idx %>%
            select(country, year, version, series, value, index, base_year) %>%
            arrange(match(country, COUNTRIES), version, series, year),
          file.path(PATH_TABLES, "index_top1_trends.csv"))
message("Wrote: ", file.path(PATH_TABLES, "index_top1_trends.csv"))

###############################################
# 4. PLOT
###############################################

theme_index <- function() {
  theme_classic(base_family = "garamond", base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey75", linewidth = 0.35, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(size = 18, face = "bold", hjust = 0),
      plot.subtitle    = element_text(size = 13, hjust = 0, color = "grey25"),
      axis.text.x  = element_text(size = 11),
      axis.text.y  = element_text(size = 11),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.line  = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      legend.position   = "bottom",
      legend.text       = element_text(size = 13, family = "garamond"),
      legend.key        = element_rect(fill = "white", color = NA),
      legend.key.width  = unit(1.6, "lines"),
      legend.background = element_blank(),
      plot.margin = margin(8, 8, 8, 8)
    )
}

plot_index <- function(iso3, ver) {

  b  <- base_years %>% filter(country == iso3)
  df <- idx %>%
    filter(country == iso3, version == ver) %>%
    mutate(series = factor(series, levels = series_levels))

  if (nrow(df) == 0) return(NULL)

  what <- if (ver == "total") "Total income of each group" else
    "Average income of a top-1% adult (tax) and of all adults (survey, BFM)"
  note <- if (b$base_year != b$tax_start) {
    sprintf("\nThe tax series starts in %d, before the first survey; the base is the first tax year with one.",
            b$tax_start)
  } else if (b$interpolated) {
    sv <- sort(unique(dist$year[dist$country == iso3]))
    sprintf("\nNo survey in %d: survey and BFM values interpolated between %d and %d.",
            b$base_year, max(sv[sv < b$base_year]), min(sv[sv > b$base_year]))
  } else ""

  ggplot(df, aes(x = year, y = index,
                 color = series, linetype = series, shape = series, group = series)) +
    geom_hline(yintercept = 100, color = "grey40", linewidth = 0.4) +
    geom_vline(xintercept = b$base_year, color = "grey55", linewidth = 0.4, linetype = "dashed") +
    geom_line(linewidth = 0.80) +
    geom_point(size = 1.9) +
    scale_color_manual(values = series_colors, drop = FALSE) +
    scale_linetype_manual(values = series_styles, drop = FALSE) +
    scale_shape_manual(values = series_shapes, drop = FALSE) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                       minor_breaks = NULL,
                       expand = expansion(mult = c(0.02, 0.02))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                       minor_breaks = NULL) +
    labs(x = "Year",
         y = sprintf("Index, %d = 100", b$base_year),
         title = country_labels[[iso3]],
         subtitle = paste0(what, ", real terms, ", b$base_year, " = 100", note),
         color = NULL, linetype = NULL, shape = NULL) +
    theme_index()
}

save_plot_both <- function(g, filename, width = 9, height = 5.4) {
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
  save_plot_both(plot_index(iso3, "total"),
                 paste0("index_total_top1_", iso3, ".pdf"))
  save_plot_both(plot_index(iso3, "per_adult"),
                 paste0("index_peradult_top1_", iso3, ".pdf"))
}
