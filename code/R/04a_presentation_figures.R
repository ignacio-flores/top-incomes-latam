###############################################
# 04a - FIGURES FOR THE MONTEVIDEO 2026 PRESENTATION
#
# Cross-country summaries of the top 1% for the talk, drawn at slide size in
# the deck's font. Same inputs as 03c / 03d:
#   ours:   intermediary_data/top_income_df.csv (p = 0.99, in_coverage)
#   theirs: distribuciones jan2024 panel, unit = "esn" (equal-split adults)
#
# Everything is a ratio within a country-year, so currency and price base
# cancel and nothing needs converting:
#   our top-1% income per adult     = topavg * (1 - p)    (topavg x 1% of adults / adults)
#   our denominator per adult       = denom_total / pop   (pop = WID adults)
#   their top-1% income per adult   = inc_sh(t1) * average(tot)
#   their denominator per adult     = average(tot)
#
# Each country is summarised by the mean over its COMMON YEARS: the years in
# which every series drawn for it in that figure exists. The period is printed
# next to the country.
#
# Figures (output/figures/presentation/, copied to the deck's figs/):
#   pres_top1_income_ladder.pdf  G1  top-1% income per adult, relative to the survey
#   pres_top1_share_ladder.pdf   G2  top-1% shares: ours (Broad, Narrow) and the DINA ladder
#   pres_dina_gap.pdf            G3  DINA / ours: top-1% income vs denominator
#   pres_dina_notax.pdf          G4  share of DINA's top-1% income that no tax return has
#   pres_tax_vs_bfm.pdf          G5  tax / BFM_all top-1% income (appendix)
#   pres_trends_main.pdf         G6  top 1% income against survey income and the BFM
#                                    denominator, indexed (03e): Brazil, Colombia, Chile, Uruguay
#   pres_trends_years.pdf        G6  the same four, with the years that have tax data marked
#   pres_trends_other.pdf        G6  the other six countries (appendix)
###############################################

rm(list = ls())

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(showtext)
  library(sysfonts)
})

# The deck's text font, Latin Modern Sans (keep in step with the Beamer preamble). It is not on
# Google Fonts, so it comes from the TeX distribution's OpenType files.
LM_OTF_DIR <- "C:/Users/dsanc/AppData/Local/Programs/MiKTeX/fonts/opentype/public/lm"
font_add("deck",
         regular    = file.path(LM_OTF_DIR, "lmsans10-regular.otf"),
         bold       = file.path(LM_OTF_DIR, "lmsans10-bold.otf"),
         italic     = file.path(LM_OTF_DIR, "lmsans10-oblique.otf"),
         bolditalic = file.path(LM_OTF_DIR, "lmsans10-boldoblique.otf"))
showtext_auto()

###############################################
# 0. PATHS AND SETTINGS
###############################################

ROOT <- "C:/Users/dsanc/Dropbox/github/top-incomes-latam"

PATH_TOP_INCOME_DF <- file.path(ROOT, "intermediary_data/top_income_df.csv")
PATH_DIST <- file.path(ROOT,
  "input_data/distribuciones_data/smicrofile_long_grouped_jan2024.csv")

PATH_FIGURES <- file.path(ROOT, "output/figures/presentation")
PATH_TABLES  <- file.path(ROOT, "output/tables")
PATH_DECK    <- "C:/Users/dsanc/Dropbox/Apps/Overleaf/presentations/aeq_top_incomes_prezi/figs"

COPY_TO_DECK <- TRUE

for (d in c(PATH_FIGURES, PATH_TABLES)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

P_TARGET <- 0.99

COUNTRIES <- c("ARG", "BRA", "CHL", "COL", "CRI", "DOM", "MEX", "PER", "SLV", "URY")

country_labels <- c("ARG" = "Argentina", "BRA" = "Brazil", "CHL" = "Chile", "COL" = "Colombia",
                    "CRI" = "Costa Rica", "DOM" = "Dominican Rep.", "MEX" = "Mexico",
                    "PER" = "Peru", "SLV" = "El Salvador", "URY" = "Uruguay")

BENCH_STEPS <- c(
  "raw" = "Survey",
  "bfm" = "BFM_all",
  "res" = "Scaled DINA",
  "nat" = "DINA"
)

# Series colours and shapes of 03c / 03d, so the talk reads like the paper.
series_colors <- c(
  "Tax"         = "#C0392B",
  "Tax, Broad"  = "#C0392B",
  "Tax, Narrow" = "#C0392B",
  "Survey"      = "#9A9A9A",
  "BFM_all"     = "#27AE60",
  "Scaled DINA" = "#3E8FC1",
  "DINA"        = "#1F4E79"
)
series_shapes <- c(
  "Tax"         = 16,
  "Tax, Broad"  = 16,
  "Tax, Narrow" = 21,
  "Survey"      = 15,
  "BFM_all"     = 18,
  "Scaled DINA" = 4,
  "DINA"        = 17
)

###############################################
# 1. DATA
###############################################

ours <- read_csv(PATH_TOP_INCOME_DF, show_col_types = FALSE) %>%
  filter(in_coverage,
         round(as.numeric(p), 6) == P_TARGET,
         country %in% COUNTRIES) %>%
  mutate(num_pa = topavg * (1 - P_TARGET),
         den_pa = denom_total / pop,
         share  = num_pa / den_pa)

# One numerator per country-year, whatever the denominator.
num_tax <- ours %>%
  group_by(country, year) %>%
  summarise(n_num = n_distinct(round(num_pa, 6)), num_tax = first(num_pa), .groups = "drop")
stopifnot(all(num_tax$n_num == 1))
num_tax <- select(num_tax, -n_num)

our_shares <- ours %>%
  filter(!is.na(denom_total)) %>%
  distinct(country, year, denom_concept, .keep_all = TRUE) %>%
  select(country, year, denom_concept, share, den_pa) %>%
  pivot_wider(names_from = denom_concept, values_from = c(share, den_pa))

dist <- read_csv(PATH_DIST, show_col_types = FALSE) %>%
  filter(unit == "esn", step %in% names(BENCH_STEPS), country %in% COUNTRIES)

theirs <- inner_join(
    dist %>% filter(group == "tot", variable == "average") %>% select(country, year, step, avg = value),
    dist %>% filter(group == "t1", variable == "inc_sh")   %>% select(country, year, step, sh = value),
    by = c("country", "year", "step")) %>%
  mutate(num = sh * avg)

theirs_wide <- theirs %>%
  pivot_wider(names_from = step, values_from = c(avg, sh, num))

panel <- num_tax %>%
  left_join(our_shares, by = c("country", "year")) %>%
  inner_join(theirs_wide, by = c("country", "year"))

period_label <- function(years) {
  y <- range(years)
  if (y[1] == y[2]) as.character(y[1]) else sprintf("%d\u2013%02d", y[1], y[2] %% 100)
}

# Country order shared by all figures: our Broad share, highest first.
country_order <- panel %>%
  filter(!is.na(share_upper)) %>%
  group_by(country) %>%
  summarise(broad = mean(share_upper), .groups = "drop") %>%
  arrange(desc(broad)) %>%
  pull(country)

row_factor <- function(df) {
  df %>%
    mutate(row = paste0(country_labels[country], "  ", period)) %>%
    mutate(row = factor(row, levels = rev(unique(row[order(match(country, country_order))]))))
}

###############################################
# 2. THEME AND SAVING
###############################################

theme_deck <- function(base_size = 18) {
  theme_minimal(base_family = "deck", base_size = base_size) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.35),
      panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.35),
      axis.text          = element_text(colour = "grey25"),
      axis.text.y        = element_text(hjust = 0),
      axis.title         = element_text(colour = "grey25"),
      legend.position    = "top",
      legend.location    = "plot",          # align with the country labels, not the panel
      legend.justification = "left",
      legend.title       = element_blank(),
      legend.text        = element_text(size = rel(0.8)),
      legend.key.width   = unit(0.9, "lines"),
      legend.margin      = margin(0, 0, 0, 0),
      plot.margin        = margin(4, 10, 4, 4)
    )
}

save_pres <- function(g, filename, width = 6.6, height = 4.6) {
  out_path <- file.path(PATH_FIGURES, filename)
  ggsave(out_path, g, device = cairo_pdf, width = width, height = height, units = "in")
  message("Saved: ", out_path)
  if (COPY_TO_DECK) {
    file.copy(out_path, file.path(PATH_DECK, filename), overwrite = TRUE)
    message("Copied to the deck: ", file.path(PATH_DECK, filename))
  }
}

ratio_axis <- function(breaks = c(1, 1.5, 2, 3, 4, 6, 8)) {
  scale_x_log10(breaks = breaks, labels = function(x) paste0(x, "\u00d7"))
}

###############################################
# 3. G1 - TOP-1% INCOME PER ADULT, RELATIVE TO THE SURVEY
# Common years: our numerator and all four steps.
###############################################

g1_data <- panel %>%
  filter(!is.na(num_raw), !is.na(num_bfm), !is.na(num_res), !is.na(num_nat)) %>%
  mutate(Tax = num_tax / num_raw, BFM_all = num_bfm / num_raw,
         `Scaled DINA` = num_res / num_raw, DINA = num_nat / num_raw) %>%
  group_by(country) %>%
  mutate(period = period_label(year), n_years = n()) %>%
  group_by(country, period, n_years) %>%
  summarise(across(c(Tax, BFM_all, `Scaled DINA`, DINA), mean),
            tax_min = min(num_tax / num_raw), tax_below_survey = sum(num_tax < num_raw),
            .groups = "drop") %>%
  row_factor()

g1_long <- g1_data %>%
  pivot_longer(c(Tax, BFM_all, `Scaled DINA`, DINA), names_to = "series", values_to = "ratio") %>%
  mutate(series = factor(series, levels = c("BFM_all", "Tax", "Scaled DINA", "DINA")))

g1 <- ggplot(g1_long, aes(x = ratio, y = row)) +
  geom_vline(xintercept = 1, colour = series_colors[["Survey"]], linewidth = 0.8) +
  geom_segment(data = g1_data, aes(x = 1, xend = DINA, y = row, yend = row),
               inherit.aes = FALSE, colour = "grey80", linewidth = 0.6) +
  geom_point(aes(colour = series, shape = series), size = 3.6, stroke = 1.1) +
  annotate("text", x = 1, y = Inf, label = "Survey = 1", family = "deck", size = 4.6,
           colour = "grey35", hjust = -0.08, vjust = 1.4) +
  scale_colour_manual(values = series_colors) +
  scale_shape_manual(values = series_shapes) +
  ratio_axis() +
  labs(x = "Top-1% income per adult (survey = 1)", y = NULL) +
  theme_deck()

save_pres(g1, "pres_top1_income_ladder.pdf")

###############################################
# 4. G2 - TOP-1% SHARES
# Common years: Broad, the four steps, and Narrow where the country has it.
###############################################

has_narrow <- panel %>%
  filter(!is.na(share_upper), !is.na(share_middle), !is.na(sh_raw), !is.na(sh_bfm),
         !is.na(sh_res), !is.na(sh_nat)) %>%
  distinct(country) %>% pull(country)

g2_years <- panel %>%
  filter(!is.na(share_upper), !is.na(sh_raw), !is.na(sh_bfm), !is.na(sh_res), !is.na(sh_nat)) %>%
  filter(!(country %in% has_narrow) | !is.na(share_middle))

g2_data <- g2_years %>%
  group_by(country) %>%
  mutate(period = period_label(year), n_years = n()) %>%
  group_by(country, period, n_years) %>%
  summarise(`Tax, Broad` = mean(share_upper),
            `Tax, Narrow` = if (cur_group()$country %in% has_narrow) mean(share_middle) else NA_real_,
            Survey = mean(sh_raw), BFM_all = mean(sh_bfm), `Scaled DINA` = mean(sh_res), DINA = mean(sh_nat),
            broad_minus_dina_max = max(share_upper - sh_nat),
            broad_above_dina = sum(share_upper > sh_nat),
            .groups = "drop") %>%
  row_factor()

g2_long <- g2_data %>%
  pivot_longer(c(Survey, BFM_all, `Tax, Broad`, `Tax, Narrow`, `Scaled DINA`, DINA),
               names_to = "series", values_to = "share") %>%
  filter(!is.na(share)) %>%
  mutate(series = factor(series, levels = c("Survey", "BFM_all", "Tax, Broad", "Tax, Narrow",
                                            "Scaled DINA", "DINA")))

g2_span <- g2_long %>% group_by(row) %>% summarise(lo = min(share), hi = max(share), .groups = "drop")

g2 <- ggplot(g2_long, aes(x = share, y = row)) +
  geom_segment(data = g2_span, aes(x = lo, xend = hi, y = row, yend = row),
               inherit.aes = FALSE, colour = "grey80", linewidth = 0.6) +
  geom_point(aes(colour = series, shape = series), size = 3.6, stroke = 1.1, fill = "white") +
  scale_colour_manual(values = series_colors) +
  scale_shape_manual(values = series_shapes) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA),
                     breaks = seq(0, 0.35, 0.05), expand = expansion(mult = c(0, 0.03))) +
  labs(x = "Top 1% share", y = NULL) +
  guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
  theme_deck()

save_pres(g2, "pres_top1_share_ladder.pdf")

###############################################
# 5. G3 - DINA / OURS: TOP-1% INCOME VS DENOMINATOR
# Common years: our numerator, Broad and DINA. The share gap is the ratio of
# the two: (DINA share) / (our Broad share) = num ratio / denominator ratio.
###############################################

g3_data <- panel %>%
  filter(!is.na(share_upper), !is.na(num_nat), !is.na(avg_nat)) %>%
  mutate(num_ratio = num_nat / num_tax, den_ratio = avg_nat / den_pa_upper) %>%
  group_by(country) %>%
  mutate(period = period_label(year), n_years = n()) %>%
  group_by(country, period, n_years) %>%
  summarise(`Top-1% income` = mean(num_ratio), Denominator = mean(den_ratio),
            num_min = min(num_ratio), num_max = max(num_ratio),
            den_min = min(den_ratio), den_max = max(den_ratio), .groups = "drop") %>%
  row_factor()

g3_long <- g3_data %>%
  pivot_longer(c(`Top-1% income`, Denominator), names_to = "what", values_to = "ratio") %>%
  mutate(what = factor(what, levels = c("Denominator", "Top-1% income")))

g3 <- ggplot(g3_long, aes(x = ratio, y = row)) +
  geom_vline(xintercept = 1, colour = "grey55", linewidth = 0.6) +
  geom_segment(data = g3_data, aes(x = Denominator, xend = `Top-1% income`, y = row, yend = row),
               inherit.aes = FALSE, colour = "grey80", linewidth = 0.6) +
  geom_point(aes(shape = what), colour = series_colors[["DINA"]], fill = "white", size = 3.6, stroke = 1.2) +
  scale_shape_manual(values = c("Denominator" = 21, "Top-1% income" = 16),
                     labels = c("Denominator: DINA \u00f7 our Broad", "Top-1% income: DINA \u00f7 tax")) +
  ratio_axis() +
  labs(x = "DINA ÷ ours, per adult", y = NULL) +
  guides(shape = guide_legend(nrow = 2)) +
  theme_deck()

save_pres(g3, "pres_dina_gap.pdf")

###############################################
# 6. G4 - DINA'S TOP 1%: INCOME NO TAX RETURN HAS
# Composition of the top 1%'s income in DINA (step nat, group t1): the *_sh
# components sum to one. Undistributed profits, net taxes on production,
# imputed rent and the residual appear on no tax return. Same years as G3.
###############################################

comp_labels <- c("upr_sh"  = "Undistributed profits",
                 "indg_sh" = "Net taxes on production",
                 "imp_sh"  = "Imputed rent",
                 "lef_sh"  = "Residual")
comp_colors <- c("Undistributed profits"   = "#1F4E79",
                 "Net taxes on production" = "#3E8FC1",
                 "Imputed rent"            = "#9DC3E6",
                 "Residual"                = "#C9D9EA")

g3_years <- panel %>%
  filter(!is.na(share_upper), !is.na(num_nat), !is.na(avg_nat)) %>%
  select(country, year)

comp <- read_csv(PATH_DIST, show_col_types = FALSE) %>%
  filter(unit == "esn", step == "nat", group == "t1", country %in% COUNTRIES,
         variable %in% c("upr_sh", "indg_sh", "imp_sh", "lef_sh", "cap_sh", "mix_sh", "pen_sh", "wag_sh")) %>%
  select(country, year, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value, values_fill = 0) %>%
  inner_join(g3_years, by = c("country", "year"))

comp_check <- comp %>%
  mutate(total = upr_sh + indg_sh + imp_sh + lef_sh + cap_sh + mix_sh + pen_sh + wag_sh)
stopifnot(all(abs(comp_check$total - 1) < 1e-6))

g4_data <- comp %>%
  group_by(country) %>%
  mutate(period = period_label(year), n_years = n()) %>%
  group_by(country, period, n_years) %>%
  summarise(across(c(upr_sh, indg_sh, imp_sh, lef_sh), mean), .groups = "drop") %>%
  mutate(no_return = upr_sh + indg_sh + imp_sh + lef_sh) %>%
  row_factor()

g4_long <- g4_data %>%
  pivot_longer(c(upr_sh, indg_sh, imp_sh, lef_sh), names_to = "item", values_to = "sh") %>%
  mutate(item = factor(comp_labels[item], levels = rev(unname(comp_labels))))

g4 <- ggplot(g4_long, aes(x = sh, y = row, fill = item)) +
  geom_col(width = 0.62) +
  geom_text(data = g4_data, aes(x = no_return, y = row, label = percent(no_return, accuracy = 1)),
            inherit.aes = FALSE, family = "deck", size = 5, hjust = -0.2, colour = "grey20") +
  scale_fill_manual(values = comp_colors, breaks = unname(comp_labels)) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.85),
                     breaks = seq(0, 0.8, 0.2), expand = expansion(mult = c(0, 0))) +
  labs(x = "Share of DINA's top-1% income", y = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_deck() +
  theme(panel.grid.major.y = element_blank())

save_pres(g4, "pres_dina_notax.pdf")

###############################################
# 7. G5 (APPENDIX) - TAX / BFM_all TOP-1% INCOME
# Same denominator per adult (our BFM denominator is their bfm average), so
# this ratio is also our BFM-denominator share over BFM_all's share.
###############################################

g5_data <- panel %>%
  filter(!is.na(num_bfm)) %>%
  mutate(r = num_tax / num_bfm) %>%
  group_by(country) %>%
  mutate(period = period_label(year), n_years = n()) %>%
  group_by(country, period, n_years) %>%
  summarise(mean = mean(r), lo = min(r), hi = max(r), below_one = sum(r < 1), .groups = "drop") %>%
  row_factor()

g5 <- ggplot(g5_data, aes(y = row)) +
  geom_vline(xintercept = 1, colour = series_colors[["BFM_all"]], linewidth = 0.8) +
  geom_segment(aes(x = lo, xend = hi, yend = row), colour = "grey75", linewidth = 2.2, lineend = "round") +
  geom_point(aes(x = mean), colour = series_colors[["Tax"]], size = 3.6) +
  ratio_axis(breaks = c(0.5, 0.75, 1, 1.5, 2)) +
  labs(x = "Top-1% income per adult: tax ÷ BFM_all", y = NULL) +
  theme_deck() +
  theme(axis.title.x = element_text(hjust = 1))

save_pres(g5, "pres_tax_vs_bfm.pdf")

###############################################
# 8. APPENDIX - TOP-1% INCOME AND DENOMINATOR PER ADULT, BY COUNTRY
# Levels in 2020 USD per adult, converted as 03b section 3.3 and 03d section 1:
#   conv = (PI_2020 / PI_year) / ner_2020.
# All ten countries. Our BFM denominator per adult equals the BFM_all average
# (03d asserts it), so it is drawn once, as BFM_all.
###############################################

PATH_DEFLATE_NER <- file.path(ROOT, "input_data/deflate_and_ner")

ner <- readxl::read_excel(file.path(PATH_DEFLATE_NER, "ner_2020.xlsx")) %>%
  rename(xrate = lcu_per_usd_2020)
priceindex <- readxl::read_excel(file.path(PATH_DEFLATE_NER, "deflator_widpriceindex.xlsx"))

conv <- priceindex %>%
  inner_join(priceindex %>% filter(year == 2020) %>% select(country, pi_base = priceindex),
             by = "country") %>%
  mutate(deflator = pi_base / priceindex) %>%
  inner_join(ner, by = "country") %>%
  transmute(country, year, conv = deflator / xrate)

ours_all <- read_csv(PATH_TOP_INCOME_DF, show_col_types = FALSE) %>%
  filter(in_coverage, round(as.numeric(p), 6) == P_TARGET, country %in% COUNTRIES)

dist_all <- read_csv(PATH_DIST, show_col_types = FALSE) %>%
  filter(unit == "esn", step %in% names(BENCH_STEPS), country %in% COUNTRIES)

theirs_all <- inner_join(
    dist_all %>% filter(group == "tot", variable == "average") %>% select(country, year, step, avg = value),
    dist_all %>% filter(group == "t1", variable == "inc_sh")   %>% select(country, year, step, sh = value),
    by = c("country", "year", "step")) %>%
  mutate(series = unname(BENCH_STEPS[step]))

panel_labels <- c("Numerator: top-1% income per adult", "Denominator per adult")

levels_df <- bind_rows(
    ours_all %>%
      distinct(country, year, .keep_all = TRUE) %>%
      transmute(country, year, series = "Tax", value = topavg * (1 - P_TARGET), panel = panel_labels[1]),
    theirs_all %>%
      transmute(country, year, series, value = sh * avg, panel = panel_labels[1]),
    ours_all %>%
      filter(!is.na(denom_total), denom_concept %in% c("upper", "middle")) %>%
      distinct(country, year, denom_concept, .keep_all = TRUE) %>%
      transmute(country, year, series = c(upper = "Broad", middle = "Narrow")[denom_concept],
                value = denom_total / pop, panel = panel_labels[2]),
    theirs_all %>%
      transmute(country, year, series, value = avg, panel = panel_labels[2])) %>%
  inner_join(conv, by = c("country", "year")) %>%
  mutate(usd    = value * conv,
         panel  = factor(panel, levels = panel_labels),
         series = factor(series, levels = c("Tax", "Broad", "Narrow", "Survey", "BFM_all",
                                            "Scaled DINA", "DINA")))

lv_colors <- c(series_colors, "Broad" = "#C0392B", "Narrow" = "#C0392B")
lv_shapes <- c(series_shapes, "Broad" = 16, "Narrow" = 21)
lv_lines  <- c("Tax" = "solid", "Broad" = "solid", "Narrow" = "dashed", "Survey" = "dotted",
               "BFM_all" = "longdash", "Scaled DINA" = "dotdash", "DINA" = "dashed")

plot_levels <- function(iso3) {
  ggplot(levels_df %>% filter(country == iso3),
         aes(x = year, y = usd, colour = series, shape = series, linetype = series)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.2, fill = "white", stroke = 0.9) +
    facet_wrap(~ panel, scales = "free_y") +
    scale_colour_manual(values = lv_colors) +
    scale_shape_manual(values = lv_shapes) +
    scale_linetype_manual(values = lv_lines) +
    scale_y_continuous(labels = label_dollar(accuracy = 1, big.mark = ","), limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = function(x) { b <- breaks_pretty(5)(x); b[b == round(b)] }) +
    labs(x = NULL, y = "2020 USD") +
    guides(colour = guide_legend(nrow = 1)) +
    theme_deck(base_size = 16) +
    theme(strip.text = element_text(face = "bold", hjust = 0, colour = "grey20"),
          panel.spacing = unit(1.4, "lines"))
}

for (iso in COUNTRIES) save_pres(plot_levels(iso), paste0("pres_levels_", iso, ".pdf"),
                                     width = 10, height = 4.6)

###############################################
# 9. G6 - TRENDS: TOP 1% INCOME AGAINST SURVEY INCOME AND THE BFM DENOMINATOR
# 03e's index, per adult version (output/tables/index_top1_trends.csv, so run
# 03e first): the average income of a top-1% adult (tax) and of all adults
# (survey, BFM), in real terms, each country's base year = 100. Four countries
# on the slide, the other six in the appendix, and the four again with the
# years that have tax data marked.
###############################################

PATH_INDEX <- file.path(PATH_TABLES, "index_top1_trends.csv")
if (!file.exists(PATH_INDEX)) stop("Run 03e first: ", PATH_INDEX)

TRENDS_MAIN  <- c("BRA", "COL", "CHL", "URY")
TRENDS_OTHER <- setdiff(COUNTRIES, TRENDS_MAIN)

# 03e's three series, in the colours, line types and shapes of the other figures
tr_series <- c("Top 1% income (tax)", "Survey income", "BFM denominator")
tr_colors <- setNames(unname(series_colors[c("Tax", "Survey", "BFM_all")]), tr_series)
tr_shapes <- setNames(unname(series_shapes[c("Tax", "Survey", "BFM_all")]), tr_series)
tr_lines  <- setNames(c("solid", "dotted", "longdash"), tr_series)

trends <- read_csv(PATH_INDEX, show_col_types = FALSE) %>%
  filter(version == "per_adult") %>%
  mutate(series = factor(series, levels = tr_series))
stopifnot(setequal(unique(trends$country), COUNTRIES), !anyNA(trends$series),
          all(count(distinct(trends, country, base_year), country)$n == 1))

tr_base <- distinct(trends, country, base_year)
tr_strip <- setNames(sprintf("%s, %d = 100", country_labels[tr_base$country], tr_base$base_year),
                     tr_base$country)
tr_panel <- function(df, isos) {
  mutate(df, panel = factor(country, levels = isos, labels = unname(tr_strip[isos])))
}

# each run of consecutive tax years, as a band from half a year before its first year
# to half a year after its last
tr_bands <- trends %>%
  filter(series == tr_series[1]) %>%
  distinct(country, year) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(run = cumsum(c(1, diff(year) != 1))) %>%
  group_by(country, run) %>%
  summarise(xmin = min(year) - 0.5, xmax = max(year) + 0.5, .groups = "drop")

plot_trends <- function(isos, ncol, bands = FALSE, base_size = 18) {
  df <- trends %>% filter(country %in% isos) %>% tr_panel(isos)
  g <- ggplot(df, aes(x = year, y = index))
  if (bands) {
    g <- g +
      geom_rect(data = tr_bands %>% filter(country %in% isos) %>% tr_panel(isos),
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = "Years with tax data"),
                inherit.aes = FALSE) +
      scale_fill_manual(values = c("Years with tax data" = alpha(tr_colors[[1]], 0.13)))
  }
  g +
    geom_hline(yintercept = 100, colour = "grey55", linewidth = 0.4) +
    geom_vline(data = distinct(df, panel, base_year), aes(xintercept = base_year),
               colour = "grey60", linewidth = 0.4, linetype = "dashed") +
    geom_line(aes(colour = series, linetype = series), linewidth = 0.8) +
    geom_point(aes(colour = series, shape = series), size = 2) +
    facet_wrap(~ panel, ncol = ncol) +
    scale_colour_manual(values = tr_colors) +
    scale_shape_manual(values = tr_shapes) +
    scale_linetype_manual(values = tr_lines) +
    scale_x_continuous(breaks = seq(2000, 2020, 5)) +
    scale_y_continuous(breaks = seq(0, 300, 25)) +
    labs(x = NULL, y = "Real average income, index") +
    guides(colour = guide_legend(order = 1), shape = guide_legend(order = 1),
           linetype = guide_legend(order = 1), fill = guide_legend(order = 2)) +
    theme_deck(base_size = base_size) +
    theme(strip.text = element_text(face = "bold", hjust = 0, colour = "grey20"),
          panel.spacing = unit(1.2, "lines"),
          legend.box = "vertical", legend.box.just = "left",
          legend.spacing.y = unit(0.15, "lines"))
}

save_pres(plot_trends(TRENDS_MAIN, ncol = 2), "pres_trends_main.pdf", width = 9.6, height = 5.4)
save_pres(plot_trends(TRENDS_MAIN, ncol = 2, bands = TRUE), "pres_trends_years.pdf",
          width = 6.6, height = 5.6)
save_pres(plot_trends(TRENDS_OTHER, ncol = 3), "pres_trends_other.pdf", width = 10.4, height = 5.4)

# For the record: top 1% income index over the survey and BFM indices in the last
# year all three have, and the years each side covers
g6_data <- trends %>%
  select(country, year, series, index, base_year) %>%
  pivot_wider(names_from = series, values_from = index) %>%
  rename(tax = `Top 1% income (tax)`, survey = `Survey income`, bfm = `BFM denominator`) %>%
  group_by(country) %>%
  summarise(period = period_label(year[!is.na(tax)]), n_years = sum(!is.na(tax)),
            base_year = first(base_year),
            bfm_period = period_label(year[!is.na(bfm)]), bfm_years = sum(!is.na(bfm)),
            last_common = max(year[!is.na(tax) & !is.na(survey) & !is.na(bfm)]),
            vs_survey = (tax / survey)[year == last_common],
            vs_bfm = (tax / bfm)[year == last_common], .groups = "drop")

###############################################
# 10. NUMBERS FOR THE SLIDES
###############################################

summary_rows <- bind_rows(
  g1_data %>% transmute(figure = "G1", country, period, n_years,
                        tax = Tax, bfm = BFM_all, res = `Scaled DINA`, nat = DINA,
                        note = sprintf("tax below survey in %d years; min %.2f", tax_below_survey, tax_min)),
  g2_data %>% transmute(figure = "G2", country, period, n_years,
                        tax = `Tax, Broad`, narrow = `Tax, Narrow`, raw = Survey, bfm = BFM_all,
                        res = `Scaled DINA`, nat = DINA,
                        note = sprintf("Broad above DINA in %d of %d years", broad_above_dina, n_years)),
  g3_data %>% transmute(figure = "G3", country, period, n_years,
                        num_ratio = `Top-1% income`, den_ratio = Denominator,
                        note = sprintf("num %.2f-%.2f, den %.2f-%.2f", num_min, num_max, den_min, den_max)),
  g4_data %>% transmute(figure = "G4", country, period, n_years, no_return,
                        upr = upr_sh, indg = indg_sh, imp = imp_sh, lef = lef_sh),
  g5_data %>% transmute(figure = "G5", country, period, n_years, tax = mean,
                        note = sprintf("range %.2f-%.2f; below 1 in %d years", lo, hi, below_one)),
  g6_data %>% transmute(figure = "G6", country, period, n_years, raw = vs_survey, bfm = vs_bfm,
                        note = sprintf("%d = 100; raw, bfm: tax index over survey, BFM index in %d; BFM %s (%d years)",
                                       base_year, last_common, bfm_period, bfm_years))
)
write_csv(summary_rows, file.path(PATH_TABLES, "presentation_numbers.csv"))
message("Wrote: ", file.path(PATH_TABLES, "presentation_numbers.csv"))

options(width = 200)
print(as.data.frame(summary_rows %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))
