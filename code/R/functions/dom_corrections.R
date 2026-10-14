###############################################################################
# DOMINICAN REPUBLIC: INCOME TO TAKE OUT OF EACH TOP GROUP
#
# Two corrections to DGII's "Monto total declarado", both in DGII's own data:
#
# 1. Lump sums. In DGII's top 1% of filers (rows 20-29 of the IR-1 table) the
#    wage column ("Salarios", note 2/: IR-4 columns F+H+J) is replaced by the
#    salary employers declared ("Ingresos del trabajo dependiente", note 18/).
#    The gap is 28-84 bn DOP in 2013-15 and 2019 and under 5 bn otherwise.
# 2. Cost of goods sold, 2012-2016. Before the 2017 form (DGII Aviso 54-17)
#    the table's costs ("gastos") leave out cost of sales. DGII's published
#    cost of sales (Boletin Estadistico 2019, Cuadro 4.01) is subtracted, each
#    row taking its share of all filers' business receipts ("Otros ingresos").
#
# Returns, by year and fractile p, the amount (DOP) to subtract from the income
# of the top (1 - p) of adults. Same floor convention as the other corrections:
# rows ranked by their average, the row straddling p counted pro rata. Rows are
# not re-ranked. 02c applies it to gpinter's top averages.
#
# Decided with the coauthors on 2026-10-14; see results_v2.tex, last section.
###############################################################################

dom_corrections <- function(p_grid,
                            file      = "input_data/admin_data/DOM/IR-1 consolidado (2012-2020).xlsx",
                            cogs_file = "input_data/admin_data/DOM/dgii_costo_ventas_2012_2016.csv",
                            pops_path = "input_data/wid_population/pops.dta",
                            years     = 2012:2020) {

  suppressPackageStartupMessages({
    library(readxl); library(dplyr); library(tidyr); library(readr); library(haven)
  })

  cogs <- read_csv(cogs_file, show_col_types = FALSE) %>%
    transmute(year, cogs = costo_ventas_millones_rd * 1e6)
  pops <- read_dta(pops_path) %>%
    filter(country == "DOM") %>%
    transmute(year = as.integer(year), adults = npopul_adults)

  # same cells as DOM-diverse.do (C4:X32), plus the row number in column B
  cols <- c("n", "declar", "thr", "totdec", "wage", "divs", "interest", "rent",
            "otherinc", "expens", "exemp", "taxdue", "sscont", "housasset",
            "agroasset", "stockasset", "cashasset", "liab", "men", "women",
            "capitalinc", "indepinc", "dependinc")

  rows <- bind_rows(lapply(years, function(y) {
    d <- suppressMessages(read_excel(file, sheet = paste0("Año ", y), range = "B4:X32",
                                     col_names = FALSE))
    names(d) <- cols
    d %>% mutate(across(everything(), as.numeric), year = y)
  })) %>%
    # row 19 is the sum of rows 20-29 (DGII's top 1% of filers), as in DOM-diverse.do
    filter(n != 19) %>%
    group_by(year) %>%
    mutate(lump = if_else(n >= 20, pmax(wage - dependinc, 0), 0),
           other_share = otherinc / sum(otherinc),
           avg = totdec / declar) %>%
    ungroup() %>%
    left_join(cogs, by = "year") %>%
    mutate(cogs_row = coalesce(cogs, 0) * other_share,
           remove = lump + cogs_row) %>%
    left_join(pops, by = "year")

  # the replacement only ever removes income: pmax() above, reported here
  neg <- rows %>% filter(n >= 20, wage < dependinc)
  if (nrow(neg)) message("DOM: ", nrow(neg), " top rows where employer-declared salary exceeds Salarios (left as is)")

  rows %>%
    select(year, avg, declar, remove, adults) %>%
    crossing(p = p_grid) %>%
    group_by(year, p) %>%
    arrange(desc(avg), .by_group = TRUE) %>%
    mutate(w = pmin(pmax(((1 - p) * adults - (cumsum(declar) - declar)) / declar, 0), 1)) %>%
    summarise(remove = sum(remove * w), adults = first(adults), .groups = "drop")
}
