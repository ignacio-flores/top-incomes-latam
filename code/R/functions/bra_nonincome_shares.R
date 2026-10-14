###############################################################################
# BRAZIL: SHARE OF TOP-GROUP INCOME THAT IS NOT INCOME
#
# Receita's total income (taxable + exclusively taxed + exempt - livro caixa)
# includes items that are not income: inheritances and gifts, capital gains
# (exclusively taxed and exempt), bonus shares, divorce settlements (meacao),
# insurance payouts, tax refunds and capital reductions. For each year and each
# fractile p this returns s1 = the share of the income of the top (1 - p) of
# adults that these items make up. 02c removes it (no re-ranking).
#
# Measured bracket by bracket where Receita publishes items by total-income
# bracket (AC2019, 2020, 2022, 2023). In the other years (2007-2018 and 2021,
# whose item table ranks by taxable income) each item's national total is
# multiplied by its top-p concentration measured in 2019, 2020 and 2022.
# Top-group income and items use the same floor convention: brackets ranked
# from the top, the bracket straddling p counted pro rata at its average.
# 2000, 2002 and 2006 come from a file without item detail; they take the
# 2008-2010 average.
#
# Decided with the coauthors on 2026-10-15; see results_v2.tex, last section.
###############################################################################

bra_nonincome_shares <- function(p_grid,
                                 fld       = "input_data/admin_data/BRA/",
                                 pops_path = "input_data/wid_population/pops.dta",
                                 early_years = c(2000, 2002, 2006),
                                 donor_years = 2008:2010) {

  suppressPackageStartupMessages({
    library(readxl); library(dplyr); library(tidyr); library(stringr); library(haven)
  })

  MEAS <- c(2019, 2020, 2022, 2023)   # items by total-income bracket
  REF  <- c(2019, 2020, 2022)         # years used for the concentrations
  NONINC <- c("inheritances", "capital gains", "bonus shares", "divorce",
              "insurance", "tax refunds", "capital reductions")

  # janitor-free header cleaner, same as the check against the pipeline reader
  clean_names_simple <- function(df) {
    x <- tolower(trimws(names(df)))
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("^_|_$", "", x)
    x <- ifelse(grepl("^[0-9]", x), paste0("x", x), x)
    names(df) <- make.unique(x, sep = "_")
    df
  }

  # ---- 1. bracket table: n and income per total-income bracket -------------
  getcomp <- function(t) {
    if (t <= 2013) { s <- "P14_P15_T9"; r <- "C11:U22" }
    if (t >= 2014) { s <- "T9_AC2014"; r <- "C11:U28" }
    if (t >= 2015) s <- "T9"
    if (t >= 2016) { s <- "faixa SM RTT+RTE+RTI"; r <- "C9:V26" }
    if (t >= 2017) r <- "B8:U26"
    if (t >= 2019) r <- "C11:V28"
    if (t == 2020) s <- "Tab9_Fx Rend Total"
    if (t >= 2021) { s <- "Tab8"; r <- "B7:BE959" }
    if (t >= 2022) r <- "A2:BD954"
    cc <- suppressMessages(read_excel(paste0(fld, "gn-irpf-ac", t, ".xlsx"),
                                      sheet = s, range = r, col_types = "text")) %>%
      clean_names_simple()
    if (t <= 2020) {
      cc <- cc %>% transmute(fx = x1, n = x2, A = x3, B = x4, C = x5, lc = livro_caixa)
    } else if (t == 2021) {
      cc <- cc %>% transmute(fx = x3, n = x4, A = x5, B = x13, C = x14, lc = livro_caixa)
    } else {
      cc <- cc %>% transmute(
        fx = faixa_de_rendim_tributavel_mais_trib_exclusiva_mais_isentos_em_sal_minimos,
        n = qtde_contribuintes, A = rendimento_tributavel_total,
        B = rend_sujeitos_a_tribut_exclusiva, C = rend_isentos_e_nao_tributaveis,
        lc = deducao_livro_caixa)
    }
    unit <- if (t <= 2021) 1e6 else 1
    cc %>%
      filter(!is.na(fx), fx != "Total") %>%
      mutate(fx = str_replace_all(fx, "1/2", "0.5"),
             fx = str_replace_all(fx, "Até 0.5", "0")) %>%
      separate(fx, into = c("lo", "resto"), sep = " a ", fill = "right") %>%
      mutate(lo = as.numeric(gsub("[^0-9.-]", "", lo)),
             # text cells such as "-" become NA and are skipped in the sums
             across(c(n, A, B, C, lc), ~ suppressWarnings(as.numeric(.x)))) %>%
      filter(!is.na(lo)) %>%
      group_by(lo) %>%
      summarise(n = sum(n, na.rm = TRUE),
                across(c(A, B, C, lc), ~ sum(.x, na.rm = TRUE) * unit),
                .groups = "drop") %>%
      mutate(year = t, inc = A + B + C - lc)
  }

  # ---- 2. classification of item labels ------------------------------------
  cat_item <- function(label) {
    s <- tolower(label)
    has <- function(k) any(vapply(k, function(z) grepl(z, s, fixed = TRUE), logical(1)))
    if (has(c("heran", "transf_patrimoniais"))) return("inheritances")
    if (grepl("mea", s, fixed = TRUE) && has(c("div", "dissolu"))) return("divorce")
    if (has(c("ganho cap", "ganhos cap", "gcap", "renda variável", "renda_variavel",
              "lucro na alienação", "alienacao", "recuperação de prejuízos",
              "recup_prejuizo", "ganhos líquidos", "ganhos líq", "ganho de capital")))
      return("capital gains")
    if (has("bonif")) return("bonus shares")
    if (has(c("apólice", "peculio", "pecúlio"))) return("insurance")
    if (has("restitui")) return("tax refunds")
    if (s == "rend_isento_reducao_capital") return("capital reductions")
    if (has(c("fgts", "indeniza"))) return("severance & FGTS")
    if (startsWith(s, "outros") || has("outros_rendim") || startsWith(s, "outros rend")) return("other")
    "income"
  }

  # whole sheet anchored at A1, cell types preserved
  read_raw <- function(y, sheet) {
    suppressMessages(read_excel(paste0(fld, "gn-irpf-ac", y, ".xlsx"), sheet = sheet,
                                col_names = FALSE, col_types = "list",
                                range = cellranger::cell_limits(c(1, 1), c(NA, NA))))
  }
  cell_str <- function(v) if (length(v) == 1 && is.character(v) && !is.na(v)) v else NA_character_
  cell_num <- function(v) if (length(v) == 1 && is.numeric(v) && !is.na(v)) v else NA_real_
  lo_of <- function(lbl) {
    l <- gsub("1/2", "0.5", lbl, fixed = TRUE)
    if (startsWith(trimws(l), "Até")) return(0)
    as.numeric(str_extract(l, "[0-9.]+"))
  }
  unit_of <- function(raw) {
    top <- raw[seq_len(min(12, nrow(raw))), ]
    hit <- any(vapply(unlist(top, recursive = FALSE),
                      function(v) { s <- cell_str(v); !is.na(s) && grepl("milh", tolower(s), fixed = TRUE) },
                      logical(1)))
    if (hit) 1e6 else 1
  }

  # ---- 3. items by bracket, AC2019-2023 ------------------------------------
  # AC2019-2020: one sheet per income type, brackets as columns from D
  read_split <- function(y, sheet, typ) {
    raw <- read_raw(y, sheet); u <- unit_of(raw); out <- list(); hdr <- NULL
    for (i in seq_len(nrow(raw))) {
      row <- raw[i, ]
      if (is.null(hdr)) {
        if (identical(cell_str(row[[4]][[1]]), "Até 1/2")) hdr <- row
        next
      }
      lab <- cell_str(row[[3]][[1]])
      if (is.na(lab) || startsWith(tolower(trimws(lab)), "total")) next
      for (j in 4:min(20, ncol(raw))) {
        h <- cell_str(hdr[[j]][[1]]); v <- cell_num(row[[j]][[1]])
        if (!is.na(h) && h != "Total" && !is.na(v))
          out[[length(out) + 1]] <- tibble(year = y, type = typ, item = trimws(lab),
                                           lo = lo_of(h), value = v * u)
      }
    }
    bind_rows(out)
  }
  # AC2021-2023: Tab19, income type and item in two columns, brackets after
  read_tab19 <- function(y) {
    raw <- read_raw(y, "Tab19"); u <- unit_of(raw); out <- list()
    hdr <- NULL; bcol <- NA; last <- NA_character_
    for (i in seq_len(nrow(raw))) {
      row <- raw[i, ]
      if (is.null(hdr)) {
        strs <- vapply(seq_len(ncol(raw)), function(j) cell_str(row[[j]][[1]]), character(1))
        k <- which(strs == "Tipo de Rendimento")
        if (length(k)) { hdr <- strs; bcol <- k[1] }
        next
      }
      tcell <- cell_str(row[[bcol]][[1]])
      if (!is.na(tcell)) last <- tcell
      typ <- if (!is.na(tcell)) tcell else last
      lab <- cell_str(row[[bcol + 1]][[1]])
      if (is.na(lab) || startsWith(tolower(trimws(lab)), "total")) next
      bj <- which(seq_along(hdr) >= bcol + 2 & !is.na(hdr) &
                  (startsWith(hdr, "Até") | startsWith(hdr, "De") | startsWith(hdr, "Mais")))
      labs <- hdr[bj]
      # 2022 Tab19 prints its values in alphabetical bracket order under numerically ordered headers
      if (y == 2022) labs <- sort(labs, method = "radix")
      for (q in seq_along(bj)) {
        v <- cell_num(row[[bj[q]]][[1]])
        if (!is.na(v))
          out[[length(out) + 1]] <- tibble(year = y, type = trimws(typ), item = trimws(lab),
                                           lo = lo_of(labs[q]), value = v * u)
      }
    }
    bind_rows(out)
  }
  norm_type <- function(t) {
    t <- tolower(t)
    ifelse(grepl("isent", t), "exempt", ifelse(grepl("exclus", t), "excl",
           ifelse(grepl("tribut", t), "taxable", "?")))
  }

  items <- bind_rows(
    read_split(2019, "faixa tributação exclusiva", "Exclusiva"),
    read_split(2019, "faixa isentos e não tributáveis", "Isentos"),
    read_split(2020, "Tab20A_Fx Rend Tribut Exclus", "Exclusiva"),
    read_split(2020, "Tab21A_Fx Rend Isentos", "Isentos"),
    read_tab19(2021), read_tab19(2022), read_tab19(2023)
  ) %>%
    mutate(tb = norm_type(type)) %>%
    filter(tb %in% c("excl", "exempt")) %>%
    mutate(category = vapply(item, cat_item, character(1)))

  # ---- 4. national item totals, AC2007-2018 (R$ bn) ------------------------
  nat_sheets <- function(y) {
    if (y <= 2013) {
      sh <- excel_sheets(paste0(fld, "gn-irpf-ac", y, ".xlsx"))
      return(c(excl = sh[startsWith(sh, "P48")][1], exempt = sh[startsWith(sh, "P49")][1]))
    }
    if (y == 2014) return(c(excl = "T19_AC2014", exempt = "T20_AC2014"))
    if (y == 2015) return(c(excl = "T19", exempt = "T20"))
    c(excl = "tributação exclusiva", exempt = "isentos e nâo tributáveis")
  }
  scan_nat <- function(y, sheet) {
    raw <- read_raw(y, sheet); unit <- NA; vcol <- NA; started <- FALSE; out <- list()
    for (i in seq_len(nrow(raw))) {
      row <- raw[i, ]
      strs <- vapply(seq_len(ncol(raw)), function(j) cell_str(row[[j]][[1]]), character(1))
      if (is.na(vcol)) {
        for (k in seq_along(strs)) {
          s <- tolower(strs[k])
          if (!is.na(s) && (grepl("r$ bilh", s, fixed = TRUE) || grepl("r$ milh", s, fixed = TRUE))) {
            unit <- if (grepl("bilh", s, fixed = TRUE)) 1 else 1e-3; vcol <- k
          }
        }
        next
      }
      j <- which(!is.na(strs[seq_len(vcol - 1)]) & nchar(trimws(strs[seq_len(vcol - 1)])) >= 3)
      if (!length(j)) next
      lab <- trimws(strs[j[1]]); l <- tolower(lab)
      if (startsWith(l, "total") || startsWith(l, "valor total")) {
        if (started) break
        next
      }
      a <- cell_num(row[[vcol]][[1]])
      if (is.na(a) || grepl("growth", l, fixed = TRUE) || l == "erro") next
      out[[length(out) + 1]] <- tibble(item = lab, value_bn = a * unit)
      started <- TRUE
    }
    bind_rows(out)
  }
  nat_early <- bind_rows(lapply(2007:2018, function(y) {
    s <- nat_sheets(y)
    bind_rows(scan_nat(y, s[["excl"]]) %>% mutate(tb = "excl"),
              scan_nat(y, s[["exempt"]]) %>% mutate(tb = "exempt")) %>%
      mutate(year = y)
  })) %>%
    mutate(category = vapply(item, cat_item, character(1)))

  nat_cat <- bind_rows(
      nat_early %>% select(year, category, value_bn),
      items %>% transmute(year, category, value_bn = value / 1e9)
    ) %>%
    group_by(year, category) %>%
    summarise(nat = sum(value_bn), .groups = "drop")

  # ---- 5. weights, top-group income and items above each p ------------------
  pops <- read_dta(pops_path) %>%
    filter(country == "BRA") %>%
    transmute(year = as.integer(year), adults = npopul_adults)
  comp <- bind_rows(lapply(2007:2023, getcomp)) %>%
    left_join(pops, by = "year")

  weights <- comp %>%
    select(year, lo, n, inc, adults) %>%
    crossing(p = p_grid) %>%
    group_by(year, p) %>%
    arrange(desc(lo), .by_group = TRUE) %>%
    mutate(w = pmin(pmax(((1 - p) * adults - (cumsum(n) - n)) / n, 0), 1)) %>%
    ungroup()

  top_inc <- weights %>%
    group_by(year, p) %>%
    summarise(T = sum(inc * w) / 1e9, .groups = "drop")

  top_items <- items %>%
    inner_join(weights %>% select(year, p, lo, w), by = c("year", "lo"),
               relationship = "many-to-many") %>%
    group_by(year, p, category) %>%
    summarise(top = sum(value * w) / 1e9, .groups = "drop")
  stopifnot(nrow(anti_join(items %>% distinct(year, lo),
                           weights %>% distinct(year, lo), by = c("year", "lo"))) == 0)

  # Concentration of each item in the top (1 - p), pooled over REF years and
  # measured on the imputed year's own bracket edges: REF brackets are merged
  # into those edges first. AC2007-2013 have 11 brackets (top one: 160+ minimum
  # wages, about 0.06% of adults) against 17 from AC2014 (top one: 320+). With
  # REF's finer brackets the items would be compared with a top income that
  # the coarse grid averages down, and the share would explode above p99.94.
  nat_ref <- nat_cat %>%
    filter(year %in% REF) %>%
    group_by(category) %>%
    summarise(nat = sum(nat), .groups = "drop")
  conc_on_grid <- function(edges) {
    to_edge <- function(lo) vapply(lo, function(x) max(edges[edges <= x + 1e-9]), numeric(1))
    ref_w <- comp %>%
      filter(year %in% REF) %>%
      mutate(lo = to_edge(lo)) %>%
      group_by(year, lo) %>%
      summarise(n = sum(n), adults = first(adults), .groups = "drop") %>%
      crossing(p = p_grid) %>%
      group_by(year, p) %>%
      arrange(desc(lo), .by_group = TRUE) %>%
      mutate(w = pmin(pmax(((1 - p) * adults - (cumsum(n) - n)) / n, 0), 1)) %>%
      ungroup()
    items %>%
      filter(year %in% REF) %>%
      mutate(lo = to_edge(lo)) %>%
      inner_join(ref_w %>% select(year, p, lo, w), by = c("year", "lo"),
                 relationship = "many-to-many") %>%
      group_by(p, category) %>%
      summarise(top = sum(value * w) / 1e9, .groups = "drop") %>%
      inner_join(nat_ref, by = "category") %>%
      filter(nat > 0) %>%
      transmute(p, category, conc = top / nat)
  }
  edges_of <- function(y) sort(unique(comp$lo[comp$year == y]))
  imp_years <- sort(setdiff(unique(nat_cat$year), MEAS))
  grid_key <- vapply(imp_years, function(y) paste(edges_of(y), collapse = "|"), character(1))
  conc <- bind_rows(lapply(unique(grid_key), function(k) {
    cg <- conc_on_grid(as.numeric(strsplit(k, "|", fixed = TRUE)[[1]]))
    crossing(year = imp_years[grid_key == k], cg)
  }))
  stopifnot(all(unlist(lapply(REF, edges_of)) %in% edges_of(REF[1])))

  measured <- top_items %>%
    filter(year %in% MEAS, category %in% NONINC) %>%
    group_by(year, p) %>%
    summarise(removed = sum(top), .groups = "drop")
  imputed <- nat_cat %>%
    filter(!year %in% MEAS, category %in% NONINC) %>%
    inner_join(conc, by = c("year", "category"), relationship = "many-to-many") %>%
    group_by(year, p) %>%
    summarise(removed = sum(conc * nat), .groups = "drop")

  s1 <- bind_rows(measured, imputed) %>%
    inner_join(top_inc, by = c("year", "p")) %>%
    transmute(year, p, s1 = removed / T, measured = year %in% MEAS)

  donor <- s1 %>%
    filter(year %in% donor_years) %>%
    group_by(p) %>%
    summarise(s1 = mean(s1), .groups = "drop")
  early <- crossing(year = early_years, donor) %>% mutate(measured = FALSE)

  bind_rows(s1, early) %>% arrange(year, p)
}
