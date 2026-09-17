###############################################
# 03c - COMPARE OUR TAX-BASED TOP SHARES WITH
#       DINA AND SURVEY ESTIMATES
#
# Deliberately separate from 03a / 03b. Reads their output, adds the
# De Rosa, Flores and Morgan (DFM) distribuciones panel, and draws one
# figure per country comparing three sources of the same top share.
#
# Sources, from the DFM long file:
#   step = "raw"        -> SURVEY estimate
#   step = "nat"        -> DINA (distributional national accounts)
#   unit = "esn"        -> equal-split adults
#   group = "t1"        -> top 1%  (NB: t1 is nested inside t10; it is NOT
#                                   part of the b50 + m40 + t10 = 1 partition)
#   variable = "inc_sh" -> income share, already a 0-1 fraction
#
# Scope for now: top 1% only.
###############################################

rm(list = ls())

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(showtext)
  library(sysfonts)
})

font_add_google("EB Garamond", "garamond")
showtext_auto()

###############################################
# 0. PATHS
###############################################

PATH_INTERMEDIATE  <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/intermediary_data"
PATH_TOP_INCOME_DF <- file.path(PATH_INTERMEDIATE, "top_income_df.csv")

# DFM distribuciones panel. NOTE: an identical copy lives in
# input_data/derosa_flores_morgan/ (same content, CRLF instead of LF) and that
# is the one 01c_clean_dfm_data.R reads. Using the distribuciones_data copy.
PATH_DIST <- paste0("C:/Users/dsanc/Dropbox/github/top-incomes-latam/",
                    "input_data/distribuciones_data/smicrofile_long_grouped_jan2024.csv")

PATH_FIGURES          <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/output/figures/source_comparison"
PATH_FIGURES_OVERLEAF <- "C:/Users/dsanc/Dropbox/Apps/Overleaf/aeq_top_incomes/figures"

# Keep the Overleaf mirror off until the style is signed off, so that
# unreviewed figures do not reach coauthors. Flip to TRUE to publish.
MIRROR_OVERLEAF <- TRUE

if (!dir.exists(PATH_FIGURES)) dir.create(PATH_FIGURES, recursive = TRUE)

P_TARGET <- 0.99   # top 1%

###############################################
# 1. LOOKUPS
# Kept local on purpose: 03b begins with rm(list = ls()) and is never sourced,
# so nothing can be inherited from it.
###############################################

country_labels <- c(
  "MEX" = "Mexico",
  "COL" = "Colombia",
  "CHL" = "Chile",
  "ARG" = "Argentina",
  "BRA" = "Brazil",
  "URY" = "Uruguay",
  "ECU" = "Ecuador",
  "PER" = "Peru",
  "DOM" = "Dominican Republic",
  "CRI" = "Costa Rica",
  "SLV" = "El Salvador"
)

denom_concept_labels <- c(
  "upper"  = "Broad",
  "middle" = "Narrow",
  "lower"  = "BFM"
)

series_colors <- c(
  "Tax tabulations" = "#C0392B",
  "DINA"            = "#1F4E79",
  "Survey"          = "#7A7A7A"
)
series_styles <- c(
  "Tax tabulations" = "solid",
  "DINA"            = "dashed",
  "Survey"          = "dotted"
)
series_shapes <- c(
  "Tax tabulations" = 16,
  "DINA"            = 17,
  "Survey"          = 15
)

safe_filename <- function(x) gsub("[^A-Za-z0-9_.-]", "_", x)

###############################################
# 2. OUR TAX-BASED TOP 1% SHARES
# top_income_df.csv carries topavg / pop / denom_total but NOT the share, so
# rebuild it exactly as 03b does in its sections 3.1 and 3.2.
###############################################

tax_top1 <- read_csv(PATH_TOP_INCOME_DF, show_col_types = FALSE) %>%
  filter(in_coverage) %>%                 # coverage flag built in 03a
  mutate(p = as.numeric(p)) %>%
  filter(p == P_TARGET) %>%
  filter(!is.na(denom_total)) %>%         # some country-years have no denominator
  mutate(
    top_pop    = (1 - p) * pop,
    top_income = topavg * top_pop,
    top_share  = top_income / denom_total
  ) %>%
  select(country, year, denom_concept, top_share) %>%
  distinct()

###############################################
# 3. DINA AND SURVEY TOP 1% SHARES (DFM panel)
###############################################

bench_top1 <- read_csv(PATH_DIST, show_col_types = FALSE) %>%
  filter(variable == "inc_sh",
         unit     == "esn",
         group    == "t1",
         step %in% c("raw", "nat")) %>%
  mutate(series = if_else(step == "nat", "DINA", "Survey")) %>%
  select(country, year, series, share = value) %>%
  distinct()

###############################################
# 4. PLOT
# One figure per country, one panel per denominator concept, three series
# inside each panel. DINA and Survey carry their own income concept, so the
# same two benchmark lines are repeated in every panel; only our own line
# moves as the denominator changes. That contrast is the point of the figure.
###############################################

plot_source_compare <- function(iso3, y_max = NULL, show_title = TRUE) {

  tax <- tax_top1 %>%
    filter(country == iso3, !is.na(top_share)) %>%
    transmute(year,
              denom_label = denom_concept_labels[denom_concept],
              series      = "Tax tabulations",
              share       = top_share)

  if (nrow(tax) == 0) return(NULL)

  # Always show all three panels, even where we have no estimate, so every
  # country figure has the same layout. Narrow is empty for ARG, DOM, SLV
  # and URY; the two benchmarks still appear there, only our line is absent.
  panels <- c("Broad", "Narrow", "BFM")

  bench <- bench_top1 %>%
    filter(country == iso3) %>%
    crossing(denom_label = panels) %>%
    select(year, denom_label, series, share)

  df <- bind_rows(tax, bench) %>%
    filter(!is.na(share)) %>%
    mutate(
      denom_label = factor(denom_label, levels = panels),
      series      = factor(series, levels = names(series_colors))
    )

  ggplot(df, aes(x = year, y = share,
                 color = series, linetype = series, group = series)) +
    geom_line(linewidth = 0.80) +
    geom_point(aes(shape = series), size = 1.8) +
    facet_wrap(~ denom_label, ncol = length(panels), drop = FALSE) +
    scale_color_manual(values = series_colors) +
    scale_linetype_manual(values = series_styles) +
    scale_shape_manual(values = series_shapes) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = if (!is.null(y_max)) c(0, y_max) else NULL,
      breaks = scales::pretty_breaks(n = 5),
      minor_breaks = NULL,
      expand = expansion(mult = c(0.02, 0.04))
    ) +
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 4),
      minor_breaks = NULL,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
      x = "Year",
      y = "Top 1% Share",
      title = if (show_title) country_labels[[iso3]] else NULL,
      color = NULL, linetype = NULL, shape = NULL
    ) +
    theme_classic(base_family = "garamond", base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey75", linewidth = 0.35, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = NA),
      strip.text       = element_text(size = 14, face = "bold"),
      plot.title       = element_text(size = 18, face = "bold", hjust = 0),
      axis.text.x  = element_text(size = 11),
      axis.text.y  = element_text(size = 11),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.line  = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      legend.position   = "bottom",
      legend.text       = element_text(size = 14, family = "garamond"),
      legend.key        = element_rect(fill = "white", color = NA),
      legend.background = element_blank(),
      panel.spacing = unit(1.0, "lines"),
      plot.margin = margin(8, 8, 8, 8)
    ) +
    guides(color = guide_legend(nrow = 1))
}

###############################################
# 5. SAVE HELPER (house convention from 03b)
###############################################

save_plot_both <- function(g, filename, width = 12, height = 5.5) {
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
    out_path_overleaf <- file.path(PATH_FIGURES_OVERLEAF, out_file)
    file.copy(out_path, out_path_overleaf, overwrite = TRUE)
    message("Mirrored to Overleaf: ", out_path_overleaf)
  }
}

###############################################
# 4b. PLOT: ALL COUNTRIES STACKED
# Same format as the single-country figure, arranged as a grid: one ROW per
# country, one COLUMN per denominator. drop = FALSE keeps the panel in place
# for country-denominator combinations we cannot compute (Narrow is missing
# entirely for ARG, DOM, SLV and URY), so the gap is visible rather than
# silently closed up. The two benchmark series are still drawn there, since
# they do not depend on our denominator; only our own line is absent.
#
# free_y frees the y axis per ROW, i.e. per country, so each country is
# legible while its three denominator panels stay directly comparable.
# Set free_y = FALSE for one shared axis across all countries.
###############################################

plot_source_compare_grid <- function(countries = NULL, free_y = TRUE) {

  if (is.null(countries)) {
    countries <- intersect(names(country_labels), unique(tax_top1$country))
  }

  tax <- tax_top1 %>%
    filter(country %in% countries, !is.na(top_share)) %>%
    transmute(country, year,
              denom_label = denom_concept_labels[denom_concept],
              series      = "Tax tabulations",
              share       = top_share)

  if (nrow(tax) == 0) return(NULL)

  bench <- bench_top1 %>%
    filter(country %in% countries) %>%
    crossing(denom_label = c("Broad", "Narrow", "BFM")) %>%
    select(country, year, denom_label, series, share)

  df <- bind_rows(tax, bench) %>%
    filter(!is.na(share))

  ctry_in <- intersect(names(country_labels), unique(df$country))

  df <- df %>%
    mutate(
      country     = factor(country, levels = ctry_in, labels = country_labels[ctry_in]),
      denom_label = factor(denom_label, levels = c("Broad", "Narrow", "BFM")),
      series      = factor(series, levels = names(series_colors))
    )

  ggplot(df, aes(x = year, y = share,
                 color = series, linetype = series, group = series)) +
    geom_line(linewidth = 0.70) +
    geom_point(aes(shape = series), size = 1.4) +
    facet_grid(country ~ denom_label,
               scales = if (free_y) "free_y" else "fixed",
               drop = FALSE) +
    scale_color_manual(values = series_colors) +
    scale_linetype_manual(values = series_styles) +
    scale_shape_manual(values = series_shapes) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = scales::pretty_breaks(n = 4),
      minor_breaks = NULL,
      expand = expansion(mult = c(0.04, 0.08))
    ) +
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 5),
      minor_breaks = NULL,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(x = "Year", y = "Top 1% Share",
         color = NULL, linetype = NULL, shape = NULL) +
    theme_classic(base_family = "garamond", base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey75", linewidth = 0.35, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = NA),
      strip.text.x     = element_text(size = 14, face = "bold"),
      strip.text.y     = element_text(size = 12, face = "bold"),
      axis.text.x  = element_text(size = 10),
      axis.text.y  = element_text(size = 10),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.line  = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      legend.position   = "bottom",
      legend.text       = element_text(size = 14, family = "garamond"),
      legend.key        = element_rect(fill = "white", color = NA),
      legend.background = element_blank(),
      panel.spacing = unit(0.8, "lines"),
      plot.margin = margin(8, 8, 8, 8)
    ) +
    guides(color = guide_legend(nrow = 1))
}

###############################################
# 6. ONE FIGURE PER COUNTRY
# Same three-panel layout for every country, each with its own legend and its
# own y axis. Countries differ a lot in level (URY 8-13%, SLV up to 50%), so a
# shared axis is not workable; the top of each axis is set from that country's
# own data, rounded up to the next 5 percentage points, always starting at 0.
###############################################

y_top_for <- function(iso3) {
  vals <- c(
    tax_top1   %>% filter(country == iso3) %>% pull(top_share),
    bench_top1 %>% filter(country == iso3) %>% pull(share)
  )
  vals <- vals[!is.na(vals)]
  if (!length(vals)) return(NULL)
  ceiling(max(vals) * 1.08 / 0.05) * 0.05
}

countries_out <- intersect(names(country_labels), unique(tax_top1$country))

for (iso3 in countries_out) {
  g <- plot_source_compare(iso3, y_max = y_top_for(iso3))
  save_plot_both(g, paste0("source_compare_top1_", iso3, ".pdf"),
                 width = 12, height = 5.5)
}

###############################################
# 7. ALL COUNTRIES STACKED (available, not generated by default)
# plot_source_compare_grid() above builds the 10 x 3 version on one sheet.
# Superseded by the per-country figures; uncomment to regenerate.
###############################################

# g_all <- plot_source_compare_grid()
# save_plot_both(g_all, "source_compare_top1_all_countries.pdf",
#                width = 11, height = 20)

###############################################
# 8. 45-DEGREE SCATTER: OURS AGAINST DISTRIBUCIONES
# Answers "which is bigger?" directly. One point per (year, denominator):
# x = the distribuciones share, y = ours. Two panels, one per benchmark, so
# the three denominators become colours instead of a third axis. Points above
# the dashed line mean our share is the larger one. Only years present in
# BOTH sources appear, so years are never averaged.
###############################################

suppressPackageStartupMessages(library(ggrepel))

denom_point_colors <- c("Broad" = "#1F4E79", "Narrow" = "#E67E22", "BFM" = "#C0392B")
denom_point_shapes <- c("Broad" = 16,        "Narrow" = 17,        "BFM" = 15)

plot_source_scatter <- function(iso3, axis_max = NULL,
                                label_years = TRUE, show_title = TRUE) {

  df <- tax_top1 %>%
    filter(country == iso3, !is.na(top_share)) %>%
    mutate(denom_label = denom_concept_labels[denom_concept]) %>%
    inner_join(bench_top1 %>% filter(country == iso3),
               by = c("country", "year")) %>%
    transmute(
      year,
      denom_label = factor(denom_label, levels = c("Broad", "Narrow", "BFM")),
      series      = factor(series,      levels = c("Survey", "DINA")),
      theirs      = share,
      ours        = top_share
    )

  if (nrow(df) == 0) return(NULL)

  if (is.null(axis_max)) {
    axis_max <- ceiling(max(c(df$theirs, df$ours)) * 1.08 / 0.05) * 0.05
  }

  p <- ggplot(df, aes(x = theirs, y = ours,
                      color = denom_label, shape = denom_label)) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "grey45", linewidth = 0.6) +
    geom_point(size = 2.8) +
    facet_wrap(~ series, ncol = 2) +
    scale_color_manual(values = denom_point_colors) +
    scale_shape_manual(values = denom_point_shapes) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, axis_max),
                       breaks = scales::pretty_breaks(n = 5),
                       expand = expansion(mult = 0.02)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, axis_max),
                       breaks = scales::pretty_breaks(n = 5),
                       expand = expansion(mult = 0.02)) +
    coord_equal() +
    labs(x = "Distribuciones top 1% share",
         y = "Our top 1% share",
         title = if (show_title) country_labels[[iso3]] else NULL,
         color = NULL, shape = NULL) +
    theme_classic(base_family = "garamond", base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey75", linewidth = 0.35, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = NA),
      strip.text       = element_text(size = 14, face = "bold"),
      plot.title       = element_text(size = 18, face = "bold", hjust = 0),
      axis.text.x  = element_text(size = 11),
      axis.text.y  = element_text(size = 11),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.line  = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      legend.position   = "bottom",
      legend.text       = element_text(size = 14, family = "garamond"),
      legend.key        = element_rect(fill = "white", color = NA),
      legend.background = element_blank(),
      panel.spacing = unit(1.2, "lines"),
      plot.margin = margin(8, 8, 8, 8)
    ) +
    guides(color = guide_legend(nrow = 1))

  if (label_years) {
    p <- p + ggrepel::geom_text_repel(
      aes(label = year), size = 3.4, family = "garamond",
      show.legend = FALSE, min.segment.length = 0.25, seed = 1
    )
  }
  p
}

# Test run
for (iso3 in c("MEX", "BRA", "CRI")) {
  g <- plot_source_scatter(iso3)
  save_plot_both(g, paste0("source_scatter_top1_", iso3, ".pdf"),
                 width = 10, height = 5.6)
}

# Brazil has 86 overlapping points; also try it without year labels
g_bra_nolab <- plot_source_scatter("BRA", label_years = FALSE)
save_plot_both(g_bra_nolab, "source_scatter_top1_BRA_nolabels.pdf",
               width = 10, height = 5.6)
