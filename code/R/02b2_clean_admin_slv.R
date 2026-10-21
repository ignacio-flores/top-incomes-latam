###############################################################################
# 02b2 - EL SALVADOR: TOP INCOMES FROM DGII'S TABULATIONS
#
# DGII's table (Tabulaciones_SLV.xls, one sheet per year) splits filers into
# two groups that do not overlap: pure wage earners (asalariados) and everyone
# with any other income (rentas diversas), whose wages are included.
#
#   income = diversas RENTA IMPONIBLE (F-11 box 255: receipts minus costs)
#          + wage earners' TOTAL RENTAS GRAVADAS (gross wages)
#
# Until 2026-10 the pipeline (SLV-wage-diverse-gperc.do, copied from
# DINA-latam) took the diversas TOTAL RENTAS GRAVADAS (box 145: receipts
# before costs) and left the wage earners out.
#
# DGII's brackets are cut on gross receipts, so net averages fall outside their
# own brackets and gpinter cannot take them. The 20 groups of both blocks are
# pooled and ranked by average, and the income of each top group is read off
# directly (floor convention: the group straddling p counts pro rata at its
# average). The result is written in the format of the other pre-built
# tabulations (CRI, MEX), which 02c loads without gpinter. thr is the average
# of the straddling group, an approximation that nothing downstream uses.
#
# Four DGII cells are patched; all four are inside DGII's printed totals:
#   2013 diversas bracket 2: renta imponible is 14 times its gross (the same
#        in DGII's own PDF) -> gross times the 2012/2014 bracket-2 ratio;
#   2009 diversas bracket 4: renta imponible exceeds its gross, and the tax
#        fits a lower base -> gross times the 2007/2010 bracket-4 ratio;
#   2006 and 2015 wage bracket 10: gross far above what DGII's own tax implies
#        (8 people with $196m, 1 person with $171m) -> its renta imponible.
#        Three smaller wage cells of the same kind (2000, 2005, 2007) move the
#        top 1% by under 1% and are left as they are.
# 2008 and 2011 stay out, as before: no denominator exists for them. 2018 is
# in the file but was not in the series before; adding it is a data update.
# Rows below the filers' reach (top groups larger than all filers) are not
# written, so 03a never treats a group of non-filers as observed.
#
# Decided with the coauthors on 2026-10-14; see results_v2.tex, last section.
###############################################################################

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(haven); library(writexl)
})

SLV_FILE  <- "input_data/admin_data/SLV/Tabulaciones_SLV.xls"
SLV_OUT   <- "input_data/admin_data/SLV/_clean"
SLV_YEARS <- setdiff(2000:2017, c(2008, 2011))

# must match p_grid in 02c_interpolate_admin_tabs.R
slv_p_grid <- c(seq(0.9, 0.99, by = 0.01),
                seq(0.991, 0.999, by = 0.001),
                seq(0.9991, 0.9999, by = 0.0001))

# ---- read one year, picking columns by their header text --------------------
read_slv_year <- function(y) {
  raw <- suppressMessages(read_excel(SLV_FILE, sheet = as.character(y), range = "A8:R19",
                                     col_names = FALSE, col_types = "text"))
  block <- as.character(unlist(raw[1, ]))   # sheet row 8: block names, merged cells
  for (j in seq_along(block)[-1]) if (is.na(block[j])) block[j] <- block[j - 1]
  head <- toupper(as.character(unlist(raw[2, ])))   # sheet row 9: column headers
  col_of <- function(blk, h) {
    k <- which(grepl(blk, toupper(block)) & grepl(h, head, fixed = TRUE))
    if (length(k) != 1) stop(sprintf("SLV %d: no unique column for %s / %s", y, blk, h))
    k
  }
  cols <- c(a_n     = col_of("ASALARIAD", "CONTRIB"),
            a_gross = col_of("ASALARIAD", "TOTAL RENTAS GRAVADAS"),
            a_net   = col_of("ASALARIAD", "RENTA IMPONIBLE"),
            d_n     = col_of("DIVERSAS",  "CONTRIB"),
            d_gross = col_of("DIVERSAS",  "TOTAL RENTAS GRAVADAS"),
            d_net   = col_of("DIVERSAS",  "RENTA IMPONIBLE"))
  body <- raw[3:12, ]
  out <- tibble(year = y, tramo = suppressWarnings(as.numeric(body[[2]])))
  for (nm in names(cols)) out[[nm]] <- suppressWarnings(as.numeric(body[[cols[[nm]]]]))
  out %>% filter(!is.na(tramo))
}

tab <- bind_rows(lapply(SLV_YEARS, read_slv_year)) %>%
  mutate(across(c(a_n, a_gross, a_net, d_n, d_gross, d_net), ~ coalesce(.x, 0)))

# ---- DGII typos ---------------------------------------------------------------
ratio <- function(tr, yrs) tab %>%
  filter(tramo == tr, year %in% yrs) %>%
  summarise(r = mean(d_net / d_gross)) %>%
  pull(r)
r2 <- ratio(2, c(2012, 2014))
r4 <- ratio(4, c(2007, 2010))
tab <- tab %>%
  mutate(d_net   = case_when(year == 2013 & tramo == 2 ~ d_gross * r2,
                             year == 2009 & tramo == 4 ~ d_gross * r4,
                             TRUE ~ d_net),
         a_gross = if_else(year %in% c(2006, 2015) & tramo == 10, a_net, a_gross))

# ---- pooled groups and top-group income ---------------------------------------
adults <- read_dta("input_data/wid_population/pops.dta") %>%
  filter(country == "SLV") %>%
  transmute(year = as.integer(year), adults = npopul_adults)

groups <- bind_rows(
    tab %>% transmute(year, n = d_n, inc = d_net),
    tab %>% transmute(year, n = a_n, inc = a_gross)
  ) %>%
  filter(n > 0) %>%
  mutate(avg = inc / n) %>%
  left_join(adults, by = "year")
stopifnot(!anyNA(groups$adults))

top_groups <- groups %>%
  crossing(p = slv_p_grid) %>%
  group_by(year, p) %>%
  arrange(desc(avg), .by_group = TRUE) %>%
  mutate(target = (1 - p) * adults,
         prev   = cumsum(n) - n,
         w      = pmin(pmax((target - prev) / n, 0), 1)) %>%
  summarise(topinc = sum(inc * w),
            thr    = { k <- which(prev < first(target) & prev + n >= first(target)); if (length(k)) avg[k[1]] else 0 },
            adults = first(adults), .groups = "drop") %>%
  arrange(year, p) %>%
  group_by(year) %>%
  mutate(topavg     = topinc / ((1 - p) * adults),
         bracketavg = if_else(is.na(lead(p)), topavg,
                              (topinc - lead(topinc)) / ((lead(p) - p) * adults))) %>%
  ungroup()

# coverage: filers as a share of adults; top groups larger than that are not observed
coverage <- groups %>%
  group_by(year) %>%
  summarise(filers = sum(n), adults = first(adults), .groups = "drop") %>%
  mutate(coverage = filers / adults)
top_groups <- top_groups %>%
  inner_join(coverage %>% select(year, coverage), by = "year") %>%
  filter(1 - p <= coverage + 1e-12)

if (!dir.exists(SLV_OUT)) dir.create(SLV_OUT, recursive = TRUE)
unlink(list.files(SLV_OUT, pattern = "^total-pre-SLV-[0-9]{4}\\.xlsx$", full.names = TRUE))
for (y in SLV_YEARS) {
  write_xlsx(top_groups %>% filter(year == y) %>% select(p, thr, bracketavg, topavg),
             file.path(SLV_OUT, paste0("total-pre-SLV-", y, ".xlsx")))
}
write.csv(coverage %>% select(year, coverage), file.path(SLV_OUT, "coverage-SLV.csv"), row.names = FALSE)
message("SLV: wrote ", length(SLV_YEARS), " years to ", SLV_OUT)
