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
})

###############################################
# 1. PATHS (edit as needed)
###############################################

PATH_INTERMEDIATE <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam/intermediate_data"

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
  "PER" = "#C65D3A",  # Peru - terracotta / brick (FIXED)
  "DOM" = "#F5B7B1",  # Dominican Republic - light pink
  "CRI" = "#000000",  # Costa Rica - black
  "SLV" = "#C2185B"   # El Salvador - dark pink
)


# --- Filter to top 10% ---
plot_df_top10 <- top_income_df %>%
  filter(p == 0.9999) %>%
  filter(country %in% names(country_palette)) %>%
  arrange(country, year)

# --- Plot ---
p_top10 <- ggplot(plot_df_top10, aes(x = year, y = top_share, color = country, group = country)) +
  geom_line(linewidth = 0.9, alpha = 0.95) +
  geom_point(size = 2.2, stroke = 0.25) +  # filled points for observed years
  scale_color_manual(values = country_palette, breaks = names(country_palette)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = pretty_breaks(n = 7)) +
  labs(
    title = "Top 10% income share",
    subtitle = "All available countries",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_family = "Garamond") +
  theme(
    # Gridlines: light grey + spaced feel
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey92", linewidth = 0.25),
    
    # Axis text
    axis.text = element_text(size = 11, color = "grey20"),
    
    # Titles
    plot.title = element_text(size = 16, face = "bold", color = "grey10"),
    plot.subtitle = element_text(size = 12, color = "grey25"),
    
    # Legend styling
    legend.position = "bottom",
    legend.text = element_text(size = 11, color = "grey15"),
    legend.key.width = unit(18, "pt"),
    
    # Clean background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

p_top10

