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
PATH_FIGURES <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/output/figures/top_shares"

# Create output folder if missing
if (!dir.exists(PATH_FIGURES)) dir.create(PATH_FIGURES, recursive = TRUE)

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
  ~top_label, ~p_val,  ~y_lab,
  "top10",    0.90,    "Top 10% Share",
  "top5",     0.95,    "Top 5% Share",
  "top1",     0.99,    "Top 1% Share",
  "top0.5",   0.995,   "Top 0.5% Share",
  "top0.1",   0.999,   "Top 0.1% Share",
  "top0.05",  0.9995,  "Top 0.05% Share",
  "top0.01",  0.9999,  "Top 0.01% Share"
)

safe_filename <- function(x) gsub("[^A-Za-z0-9_.-]", "_", x)

plot_topshare <- function(df, p_target, denom_concept_target,
                          y_label,
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
    
    g <- plot_topshare(
      df = top_income_df,
      p_target = p_target,
      denom_concept_target = denom_short,
      y_label = y_lab
    )
    
    if (is.null(g)) {
      message("No data for: ", denom_short, " / ", top_label, " (p=", p_target, ")")
      next
    }
    
    out_name <- paste0(denom_short, "_denom_", top_label, ".pdf")
    out_path <- file.path(PATH_FIGURES, safe_filename(out_name))
    
    ggsave(
      filename = out_path,
      plot = g,
      device = cairo_pdf,
      width = 12,
      height = 8,
      units = "in"
    )
    
    message("Saved: ", out_path)
  }
}


