###############################################
# 0 — TOP INCOME SHARES: GRAPH PIPELINE (STARTER)
# Goal: modular graphs by denominator concept × percentile
###############################################

# d613  households (not  Employees’ social contributions  but close) social contributions 
# (12 = 121 122) (44 = 441 + (442+443) -> quitar todo 44); 
#en vez de quitar 121, y 122 que son = 611 y 612, quitamos 61 y entonces botamos tambien 614 y 615
# so we propose b5g + d62 - fcf (wid) - d61 (large coverage and includes 611,2,3 which we were going to throw
# anyway) - d44 (we lose 442 and 443 more than we want) 
#  - Imputed rent of owner occupiers - part of b2 (?)
# argentina y el salvador wid denominator - DONE
# Fixed capital consumption === wid sna - DONE

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
  "PER" = "#FF7F0E",  # Peru - terracotta / brick (FIXED)
  "DOM" = "#F5B7B1",  # Dominican Republic - light pink
  "CRI" = "#7A7A7A",  # Costa Rica - black
  "SLV" = "#C2185B"   # El Salvador - dark pink
)

###############################################
# 4A. DATA (Top 10% = p 0.90) — ONLY REAL DATA
###############################################

plot_df <- top_income_df %>%
  filter(p == 0.99) %>%
  filter(denom_source == "SNA_CEI + SNA_WID") %>%
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
# 4B. PLOT — WHITE + DOTTED MAJOR GRID (JOURNAL)
###############################################
p_top10 <- ggplot(
  plot_df,
  aes(x = year, y = top_share, color = country, group = country)
) +
  # --- LINES: black underlay + color (tiny bit smaller) ---
  geom_line(color = "black", linewidth = 1.25) +   # underlay
  geom_line(linewidth = 0.70) +                    # colored line
  
  # --- POINTS: black underlay + color (tiny bit smaller) ---
  geom_point(color = "black", size = 3.2, stroke = 0.55) +  # underlay
  geom_point(size = 2.65, stroke = 0.20) +                  # colored point
  
  
  scale_color_manual(
    values = country_palette_used,
    labels = country_labels_used
  ) +
  
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
  
  labs(
    x = "Year",          # <-- set axis title text here
    y = "Top 10% Share",  # <-- set axis title text here
    color = NULL
  ) +
  
  theme_classic(base_family = "garamond", base_size = 16) +
  
  theme(
    # White backgrounds
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    # Dotted major grid only
    panel.grid.major = element_line(color = "grey65",
                                    linewidth = 0.45,
                                    linetype = "dotted"),
    panel.grid.minor = element_blank(),
    
    # ---- AXIS TEXT SIZE CONTROLS ----
    axis.text.x  = element_text(size = 22),  # X tick labels
    axis.text.y  = element_text(size = 22),  # Y tick labels
    
    axis.title.x = element_text(size = 24),  # X title
    axis.title.y = element_text(size = 24),  # Y title
    
    axis.line  = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.45),
    
    # ---- LEGEND TEXT SIZE ----
    legend.position = "bottom",
    legend.text = element_text(size = 24, family = "garamond"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.background = element_blank(),
    
    plot.margin = margin(8, 8, 8, 8)
  ) +
  
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

p_top10


