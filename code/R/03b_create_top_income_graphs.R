###############################################
# 0 — TOP INCOME SHARES: GRAPH PIPELINE (STARTER)
# Goal: modular graphs by denominator concept × percentile
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
  library(readxl)
})


font_add_google("EB Garamond", "garamond")
showtext_auto()

###############################################
# 1. PATHS (edit as needed)
###############################################

PATH_INTERMEDIATE <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/intermediary_data"

PATH_TOP_INCOME_DF <- file.path(PATH_INTERMEDIATE, "top_income_df.csv")
# Optional (diagnostics from previous script)
PATH_AVAIL_CHECK   <- file.path(PATH_INTERMEDIATE, "availability_check.csv")
PATH_AVAIL_SURVIVE <- file.path(PATH_INTERMEDIATE, "availability_survive.csv")

# Output folder for graphs
PATH_FIGURES          <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/output/figures/top_shares"
PATH_FIGURES_OVERLEAF <- "C:/Users/dsanc/Dropbox/Apps/Overleaf/aeq_top_incomes/figures"

# Create output folders if missing
if (!dir.exists(PATH_FIGURES))          dir.create(PATH_FIGURES,          recursive = TRUE)
if (!dir.exists(PATH_FIGURES_OVERLEAF)) dir.create(PATH_FIGURES_OVERLEAF, recursive = TRUE)

###############################################
# 2. LOAD DATA
###############################################

top_income_df <- read_csv(PATH_TOP_INCOME_DF, show_col_types = FALSE)

# Optional: load availability tables if they exist
availability_check <- if (file.exists(PATH_AVAIL_CHECK)) {
  read_csv(PATH_AVAIL_CHECK, show_col_types = FALSE)
} else NULL

availability_survive <- if (file.exists(PATH_AVAIL_SURVIVE)) {
  read_csv(PATH_AVAIL_SURVIVE, show_col_types = FALSE)
} else NULL

###############################################
# 3. BASIC CLEANING / STANDARDIZATION
###############################################

top_income_df <- top_income_df %>%
  mutate(
    country = as.character(country),
    year    = as.integer(year),
    p       = as.numeric(p),
    thr     = as.numeric(thr),
    avg     = as.numeric(avg),
    topavg  = as.numeric(topavg),
    denom_total   = as.numeric(denom_total),
    denom_concept = as.character(denom_concept),
    denom_source  = as.character(denom_source),
    pop = as.numeric(pop)
  )

###############################################
# 3.1. POPULATION ADJUSTMENT (top-group pop)
###############################################

top_income_df <- top_income_df %>%
  mutate(
    top_pop = (1 - p) * pop
  )

###############################################
# 3.2. COMPUTE TOP INCOME + TOP SHARE
###############################################

top_income_df <- top_income_df %>%
  mutate(
    top_income = topavg * top_pop,
    top_share  = top_income / denom_total
  )

###############################################
# 3.3. REAL 2020 USD CONVERSION
# Self-contained: reads deflator + NER from input_data/deflate_and_ner.
#   (a) Deflator: WID priceindex (inyixx, national income deflator --
#       typically GDP deflator, CPI fallback). Snapshot extracted from
#       WID-sourced sna-wid.dta.
#       File: input_data/deflate_and_ner/deflator_widpriceindex.xlsx
#              ('data' sheet + 'source' sheet with provenance)
#   (b) NER: 2020 period-average market (nominal, NOT PPP) exchange
#       rate, LCU per USD. Source: World Bank WDI PA.NUS.FCRF (WID
#       equivalent: xlcusx999i). Dollarized economies (ECU, SLV) = 1.
#       File: input_data/deflate_and_ner/ner_2020.xlsx
#              ('data' sheet + 'source' sheet with provenance)
#
# conversion_factor = (PI_2020 / PI_year) / ner_2020
# real-2020-USD value = nominal_LCU * conversion_factor
###############################################

PATH_DEFLATE_NER <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/input_data/deflate_and_ner"
BASE_YEAR        <- 2020

xrate_df <- read_excel(
  file.path(PATH_DEFLATE_NER, "ner_2020.xlsx"),
  sheet = "data"
) %>%
  rename(xrate = lcu_per_usd_2020)

cpi_df <- read_excel(
  file.path(PATH_DEFLATE_NER, "deflator_widpriceindex.xlsx"),
  sheet = "data"
) %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(priceindex))

cpi_base <- cpi_df %>%
  filter(year == BASE_YEAR) %>%
  select(country, pi_base = priceindex)

cpi_df <- cpi_df %>%
  inner_join(cpi_base, by = "country") %>%
  mutate(deflator = pi_base / priceindex) %>%
  select(country, year, deflator)

top_income_df <- top_income_df %>%
  left_join(cpi_df,   by = c("country", "year")) %>%
  left_join(xrate_df, by = "country") %>%
  mutate(
    conv_factor      = deflator / xrate,
    denom_total_real = denom_total * conv_factor,
    topavg_real      = topavg      * conv_factor,
    top_income_real  = top_income  * conv_factor
  )

###############################################
# 4. PLOT: TOP 10% SHARE (p = 0.90), all countries
###############################################

country_palette <- c(
  "MEX" = "#1B5E20",  # Mexico - dark green
  "COL" = "#D4A017",  # Colombia - brown / ochre (FIXED)
  "CHL" = "#8B0000",  # Chile - dark red
  "ARG" = "#7FB3D5",  # Argentina - light blue
  "BRA" = "#66C266",  # Brazil - light green
  "URY" = "#003366",  # Uruguay - dark blue
  "ECU" = "#F1C40F",  # Ecuador - light yellow
  "PER" = "#000000",  # Peru - terracotta / brick (FIXED)"#FF7F0E"
  "DOM" = "#F5B7B1",  # Dominican Republic - light pink
  "CRI" = "#7A7A7A",  # Costa Rica - black
  "SLV" = "#C2185B"   # El Salvador - dark pink
)

###############################################
# 4A. DATA (Top 10% = p 0.90) — ONLY REAL DATA
###############################################

###NEED TO AUTOMIZE BUT LATER:
### upper lower middle

plot_df <- top_income_df %>%
  filter(p == 0.90) %>%
  filter(denom_concept == "middlebound_sna_actual") %>%
  filter(country %in% names(country_palette)) %>%
  filter(!is.na(top_share)) %>%
  select(country, year, top_share) %>%
  arrange(country, year)

# keep a stable ordering (palette order, but only those present)
countries_in_data <- intersect(names(country_palette), unique(plot_df$country))

plot_df <- plot_df %>%
  mutate(country = factor(country, levels = countries_in_data))

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

country_palette_used <- country_palette[countries_in_data]
country_labels_used  <- country_labels[countries_in_data]

###############################################
# 5. AUTOMATED PLOTS: denom concept × top group
# Assumes denom_concept is already one of: "upper", "middle", "lower"
# Saves: {upper|lower|middle}_denom_top{10|5|1|0.5|0.1|0.05|0.01}.pdf
###############################################

denom_levels <- c("upper", "lower", "middle")

top_groups <- tibble::tribble(
  ~top_label, ~p_val,  ~y_lab,              ~y_max,
  "top10",    0.90,    "Top 10% Share",     1.00,
  "top5",     0.95,    "Top 5% Share",      NA_real_,
  "top1",     0.99,    "Top 1% Share",      0.60,
  "top0.5",   0.995,   "Top 0.5% Share",    NA_real_,
  "top0.1",   0.999,   "Top 0.1% Share",    0.40,
  "top0.05",  0.9995,  "Top 0.05% Share",   NA_real_,
  "top0.01",  0.9999,  "Top 0.01% Share",   0.20
)

safe_filename <- function(x) gsub("[^A-Za-z0-9_.-]", "_", x)

plot_topshare <- function(df, p_target, denom_concept_target,
                          y_label, y_max = NULL,
                          countries_keep = names(country_palette),
                          palette = country_palette,
                          labels = country_labels) {
  
  plot_df <- df %>%
    filter(p == p_target) %>%
    filter(denom_concept == denom_concept_target) %>%
    filter(country %in% countries_keep) %>%
    filter(!is.na(top_share)) %>%
    select(country, year, top_share) %>%
    arrange(country, year)
  
  if (nrow(plot_df) == 0) return(NULL)
  
  countries_in_data <- intersect(names(palette), unique(plot_df$country))
  
  plot_df <- plot_df %>%
    mutate(country = factor(country, levels = countries_in_data))
  
  palette_used <- palette[countries_in_data]
  labels_used  <- labels[countries_in_data]
  
  ggplot(plot_df, aes(x = year, y = top_share, color = country, group = country)) +
    geom_line(color = "black", linewidth = 1.25) +
    geom_line(linewidth = 0.70) +
    geom_point(color = "black", size = 3.2, stroke = 0.55) +
    geom_point(size = 2.65, stroke = 0.20) +
    scale_color_manual(values = palette_used, labels = labels_used) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = if (!is.null(y_max)) c(0, y_max) else NULL,
      breaks = scales::pretty_breaks(n = 5),
      minor_breaks = NULL,
      expand = expansion(mult = c(0.02, 0.04))
    ) +
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 7),
      minor_breaks = NULL,
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    labs(x = "Year", y = y_label, color = NULL) +
    theme_classic(base_family = "garamond", base_size = 16) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey65", linewidth = 0.45, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.text.x  = element_text(size = 22),
      axis.text.y  = element_text(size = 22),
      axis.title.x = element_text(size = 24),
      axis.title.y = element_text(size = 24),
      axis.line  = element_line(color = "black", linewidth = 0.6),
      axis.ticks = element_line(color = "black", linewidth = 0.45),
      legend.position = "bottom",
      legend.text = element_text(size = 24, family = "garamond"),
      legend.key = element_rect(fill = "white", color = NA),
      legend.background = element_blank(),
      plot.margin = margin(8, 8, 8, 8)
    ) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
}

plot_topshare(
  df = top_income_df,
  p_target = 0.90,
  denom_concept_target = "upper",
  y_label = "top10"
)


for (denom_short in denom_levels) {
  for (i in seq_len(nrow(top_groups))) {
    
    p_target  <- top_groups$p_val[[i]]
    top_label <- top_groups$top_label[[i]]
    y_lab     <- top_groups$y_lab[[i]]
    y_max_i   <- top_groups$y_max[[i]]

    g <- plot_topshare(
      df = top_income_df,
      p_target = p_target,
      denom_concept_target = denom_short,
      y_label = y_lab,
      y_max   = if (!is.na(y_max_i)) y_max_i else NULL
    )
    
    if (is.null(g)) {
      message("No data for: ", denom_short, " / ", top_label, " (p=", p_target, ")")
      next
    }
    
    out_name <- paste0(denom_short, "_denom_", top_label, ".pdf")
    out_file <- safe_filename(out_name)

    out_path          <- file.path(PATH_FIGURES,          out_file)
    out_path_overleaf <- file.path(PATH_FIGURES_OVERLEAF, out_file)

    ggsave(
      filename = out_path,
      plot = g,
      device = cairo_pdf,
      width = 12,
      height = 8,
      units = "in"
    )

    file.copy(out_path, out_path_overleaf, overwrite = TRUE)

    message("Saved: ", out_path)
    message("Mirrored to Overleaf: ", out_path_overleaf)
  }
}


###############################################
# 6. HELPERS & DATA PREP FOR PANEL PLOTS
###############################################

save_plot_both <- function(g, filename) {
  if (is.null(g)) {
    message("Skipped (no data): ", filename)
    return(invisible(NULL))
  }

  out_path          <- file.path(PATH_FIGURES,          filename)
  out_path_overleaf <- file.path(PATH_FIGURES_OVERLEAF, filename)

  ggsave(filename = out_path, plot = g, device = cairo_pdf,
         width = 12, height = 8, units = "in")
  file.copy(out_path, out_path_overleaf, overwrite = TRUE)

  message("Saved: ",             out_path)
  message("Mirrored to Overleaf: ", out_path_overleaf)
}

denom_by_concept <- top_income_df %>%
  select(country, year, denom_concept, denom_total = denom_total_real) %>%
  distinct() %>%
  filter(country %in% names(country_palette))

denom_percap <- top_income_df %>%
  select(country, year, denom_concept, denom_total = denom_total_real, pop) %>%
  distinct() %>%
  mutate(denom_percap = denom_total / pop) %>%
  filter(country %in% names(country_palette))

numerator_raw <- top_income_df %>%
  filter(country %in% names(country_palette)) %>%
  select(country, year, p, topavg = topavg_real, pop) %>%
  distinct() %>%
  mutate(top_income_total = topavg * (1 - p) * pop)


###############################################
# 7. PANEL PLOTS: all 3 denom definitions overlaid per country
# One figure for totals, one for per-cap (2 files total)
###############################################

denom_line_styles <- c(
  "Broad"   = "solid",
  "Narrow"  = "dashed",
  "BFM"     = "dotted"
)

denom_line_colors <- c(
  "Broad"   = "black",
  "Narrow"  = "grey40",
  "BFM"     = "grey65"
)

denom_concept_labels <- c(
  "upper"  = "Broad",
  "middle" = "Narrow",
  "lower"  = "BFM"
)

plot_all_denom_panel <- function(df, value_col, y_label,
                                 labels = country_labels,
                                 y_overrides = NULL) {

  df <- df %>%
    filter(!is.na(.data[[value_col]])) %>%
    filter(country %in% names(labels)) %>%
    mutate(denom_label = denom_concept_labels[denom_concept])

  if (nrow(df) == 0) return(NULL)

  countries_in_data <- intersect(names(labels), unique(df$country))

  df <- df %>%
    mutate(
      country     = factor(country,
                           levels = countries_in_data,
                           labels = labels[countries_in_data]),
      denom_label = factor(denom_label,
                           levels = c("Broad", "Narrow", "BFM"))
    )

  # Per-country y-axis overrides via geom_blank
  y_anchor_df <- NULL
  if (!is.null(y_overrides)) {
    y_anchor_df <- bind_rows(lapply(names(y_overrides), function(cc) {
      lbl <- labels[[cc]]
      if (is.null(lbl) || !(lbl %in% levels(df$country))) return(NULL)
      tibble(country = factor(lbl, levels = levels(df$country)),
             value   = y_overrides[[cc]])
    }))
  }

  p <- ggplot(df, aes(x = year, y = .data[[value_col]],
                 color = denom_label, linetype = denom_label,
                 group = denom_label))

  mid_year <- median(df$year, na.rm = TRUE)

  if (!is.null(y_anchor_df) && nrow(y_anchor_df) > 0) {
    y_anchor_df$year <- mid_year
    p <- p + geom_point(data = y_anchor_df,
                        aes(x = year, y = value),
                        inherit.aes = FALSE,
                        alpha = 0, size = 0)
  }

  p + geom_line(linewidth = 0.80) +
    geom_point(aes(shape = denom_label), size = 1.6) +
    facet_wrap(~ country, scales = "free_y", ncol = 4) +
    scale_color_manual(values = denom_line_colors) +
    scale_linetype_manual(values = denom_line_styles) +
    scale_shape_manual(values = c("Broad" = 16, "Narrow" = 17, "BFM" = 15)) +
    scale_y_continuous(
      labels = scales::label_number(
        scale_cut = scales::cut_short_scale(),
        accuracy  = 0.1
      ),
      breaks = scales::pretty_breaks(n = 4),
      minor_breaks = NULL,
      expand = expansion(mult = 0.02)
    ) +
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 4),
      minor_breaks = NULL,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(x = "Year", y = y_label, color = NULL, linetype = NULL, shape = NULL) +
    theme_classic(base_family = "garamond", base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey75", linewidth = 0.35, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = NA),
      strip.text       = element_text(size = 14, face = "bold"),
      axis.text.x  = element_text(size = 11),
      axis.text.y  = element_text(size = 11),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.line  = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      legend.position  = "bottom",
      legend.text      = element_text(size = 14, family = "garamond"),
      legend.key       = element_rect(fill = "white", color = NA),
      legend.background = element_blank(),
      panel.spacing = unit(1.0, "lines"),
      plot.margin = margin(8, 8, 8, 8)
    ) +
    guides(color = guide_legend(nrow = 1))
}

g_total_all <- plot_all_denom_panel(
  denom_by_concept,
  value_col   = "denom_total",
  y_label     = "Denominator, total (2020 USD)"
)
save_plot_both(g_total_all, "denom_total_panel_all.pdf")

# Fixed y-axis: 0 to 15,000 USD/adult for all countries on all denom panels.
ymax_default <- 15000
y_overrides_common <- setNames(
  lapply(names(country_palette), function(cc) c(0, ymax_default)),
  names(country_palette)
)

g_percap_all <- plot_all_denom_panel(
  denom_percap,
  value_col   = "denom_percap",
  y_label     = "Denominator per adult (2020 USD)",
  y_overrides = y_overrides_common
)
save_plot_both(g_percap_all, "denom_percap_panel_all.pdf")


###############################################
# 8. PANEL PLOTS: denominator + 4 numerator percentiles in same plot
# 5 lines per country: 1 denom + top 10/1/0.1/0.01
# One figure per denom definition (3 files). Each country = one panel.
# Y-axis expanded by +/- 300 billion to keep all series visible.
###############################################

num_groups <- tibble::tribble(
  ~p_val,  ~series_label,
  0.90,    "Top 10%",
  0.99,    "Top 1%",
  0.999,   "Top 0.1%",
  0.9999,  "Top 0.01%"
)

denom_labels <- c(
  "upper"  = "Denominator (Broad)",
  "lower"  = "Denominator (BFM)",
  "middle" = "Denominator (Narrow)"
)

num_colors <- c(
  "Top 10%"     = "#C0392B",
  "Top 1%"      = "#E67E22",
  "Top 0.1%"    = "#27AE60",
  "Top 0.01%"   = "#8E44AD"
)

plot_denom_num_panel <- function(denom_df, num_df, denom_concept_name, y_label,
                                 labels = country_labels,
                                 y_overrides = NULL) {

  denom_label <- denom_labels[[denom_concept_name]]

  denom_long <- denom_df %>%
    filter(!is.na(denom_total)) %>%
    transmute(country, year,
              value  = denom_total,
              series = denom_label)

  countries_with_denom <- unique(denom_long$country)

  num_long <- num_df %>%
    filter(!is.na(top_income_total)) %>%
    filter(country %in% countries_with_denom) %>%
    inner_join(num_groups, by = "p_val") %>%
    transmute(country, year,
              value  = top_income_total,
              series = series_label)

  df <- bind_rows(denom_long, num_long) %>%
    filter(country %in% names(labels))

  if (nrow(df) == 0) return(NULL)

  countries_in_data <- intersect(names(labels), unique(df$country))

  series_colors <- c(setNames("#1F4E79", denom_label), num_colors)
  series_order  <- names(series_colors)

  # Anchor y-axis: use y_overrides (named list ISO3 -> c(ymin, ymax)) if
  # provided, else fall back to 0 to 1.15× denom max per country.
  if (!is.null(y_overrides)) {
    anchor_rows <- lapply(countries_in_data, function(cc) {
      lim <- y_overrides[[cc]]
      if (is.null(lim)) return(NULL)
      tibble(country = cc, y_min = lim[1], y_max = lim[2])
    })
    denom_max <- bind_rows(anchor_rows)
  } else {
    denom_max <- denom_long %>%
      filter(country %in% countries_in_data) %>%
      group_by(country) %>%
      summarise(y_max = max(value, na.rm = TRUE) * 1.15, .groups = "drop") %>%
      mutate(y_min = 0)
  }

  y_anchors <- denom_max %>%
    pivot_longer(cols = c(y_min, y_max), values_to = "value") %>%
    select(country, value) %>%
    mutate(
      country = factor(country,
                       levels = countries_in_data,
                       labels = labels[countries_in_data])
    )

  df <- df %>%
    mutate(
      country = factor(country,
                       levels = countries_in_data,
                       labels = labels[countries_in_data]),
      series  = factor(series, levels = series_order)
    )

  ggplot(df, aes(x = year, y = value, color = series, group = series)) +
    geom_blank(data = y_anchors, aes(x = NA, y = value),
               inherit.aes = FALSE) +
    geom_line(linewidth = 0.80) +
    geom_point(size = 1.4) +
    facet_wrap(~ country, scales = "free_y", ncol = 4) +
    scale_color_manual(values = series_colors) +
    scale_y_continuous(
      labels = scales::label_number(
        scale_cut = scales::cut_short_scale(),
        accuracy  = 0.1
      ),
      breaks = scales::pretty_breaks(n = 4),
      minor_breaks = NULL,
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 4),
      minor_breaks = NULL,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(x = "Year", y = y_label, color = NULL) +
    theme_classic(base_family = "garamond", base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey75", linewidth = 0.35, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = NA),
      strip.text       = element_text(size = 14, face = "bold"),
      axis.text.x  = element_text(size = 11),
      axis.text.y  = element_text(size = 11),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.line  = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      legend.position  = "bottom",
      legend.text      = element_text(size = 14, family = "garamond"),
      legend.key       = element_rect(fill = "white", color = NA),
      legend.background = element_blank(),
      panel.spacing = unit(1.0, "lines"),
      plot.margin = margin(8, 8, 8, 8)
    ) +
    guides(color = guide_legend(nrow = 1))
}

# Per-capita versions for denom_num panel (divide denominator and numerator by pop)
# Uses denom_total_real (CPI-deflated to 2020) so values match denom_percap_panel_all.pdf
denom_by_concept_pc <- top_income_df %>%
  select(country, year, denom_concept, denom_total = denom_total_real, pop) %>%
  distinct() %>%
  mutate(denom_total = denom_total / pop) %>%
  select(-pop) %>%
  filter(country %in% names(country_palette))

numerator_raw_pc <- numerator_raw %>%
  mutate(top_income_total = topavg * (1 - p))

for (denom_short in denom_levels) {
  num_df_for_join <- numerator_raw_pc %>%
    rename(p_val = p)

  g <- plot_denom_num_panel(
    denom_df           = denom_by_concept_pc %>% filter(denom_concept == denom_short),
    num_df             = num_df_for_join,
    denom_concept_name = denom_short,
    y_label            = "2020 USD per adult",
    y_overrides        = y_overrides_common
  )
  save_plot_both(g, paste0("denom_num_panel_", denom_short, ".pdf"))
}


###############################################
# 9. ADMIN TABULATION COVERAGE
# For each country-year, extract the first-row p of the "total-pre-"
# admin tabulation and plot coverage = 1 - p (the widest top-group
# share observed in the raw admin data). CRI has no pre- tabulation
# and is skipped. MEX has one xlsx per year, others have one xlsx
# with a sheet per year. ECU file is named total-pre-ECU.xlsx (no
# "-adults" suffix).
###############################################

PATH_ADMIN <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/input_data/admin_data"

coverage_countries <- setdiff(names(country_palette), c("CRI", "ECU"))

first_p_from_sheet <- function(path, sheet) {
  x <- suppressMessages(read_excel(path, sheet = sheet, n_max = 1))
  as.numeric(x[["p"]][1])
}

extract_coverage_one <- function(iso3) {
  if (iso3 == "MEX") {
    files <- list.files(
      file.path(PATH_ADMIN, "MEX", "_clean"),
      pattern = "^total-pre-MEX-\\d{4}\\.xlsx$", full.names = TRUE
    )
    years <- as.integer(sub(".*-(\\d{4})\\.xlsx$", "\\1", files))
    p_first <- vapply(files, function(f) first_p_from_sheet(f, 1),
                      numeric(1))
  } else {
    fname <- if (iso3 == "ECU") "total-pre-ECU.xlsx" else
      paste0("total-pre-", iso3, "-adults.xlsx")
    path <- file.path(PATH_ADMIN, iso3, "_clean", fname)
    sheets <- excel_sheets(path)
    years <- suppressWarnings(as.integer(sheets))
    keep <- !is.na(years)
    sheets <- sheets[keep]
    years  <- years[keep]
    p_first <- vapply(sheets, function(s) first_p_from_sheet(path, s),
                      numeric(1))
  }
  tibble::tibble(country = iso3, year = years, p_min = p_first,
                 coverage = 1 - p_first)
}

coverage_df <- bind_rows(lapply(coverage_countries, extract_coverage_one)) %>%
  arrange(country, year)

countries_in_cov <- intersect(names(country_palette), unique(coverage_df$country))
coverage_df <- coverage_df %>%
  mutate(country = factor(country,
                          levels = countries_in_cov,
                          labels = country_labels[countries_in_cov]))

g_coverage <- ggplot(coverage_df, aes(x = year, y = coverage)) +
  geom_line(color = "#1F4E79", linewidth = 0.80) +
  geom_point(color = "#1F4E79", size = 1.6) +
  facet_wrap(~ country, ncol = 4, scales = "free_y") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 0.8, 0.2),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02)),
    limits = c(0, 0.80)
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 4),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(x = "Year",
       y = "Admin tabulation coverage (1 - lowest p in year)") +
  theme_classic(base_family = "garamond", base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey75", linewidth = 0.35, linetype = "dotted"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey92", color = NA),
    strip.text       = element_text(size = 14, face = "bold"),
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.line  = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    panel.spacing = unit(1.0, "lines"),
    plot.margin = margin(8, 8, 8, 8)
  )

save_plot_both(g_coverage, "admin_coverage_panel.pdf")

