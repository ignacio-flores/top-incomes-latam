collapse_and_clean <- function(d) {
  d <- d %>%
    arrange(p) %>%
    distinct(p, .keep_all = TRUE) %>%             # drop exact duplicate p's (if any)
    mutate(width = lead(p, default = 1) - p)
  
  # Build runs to MERGE forward until a row is "clean":
  # start a new run only if thr strictly increases AND bracketavg > thr
  d <- d %>%
    mutate(
      thr_prev = lag(thr),
      new_run  = case_when(
        row_number() == 1L ~ TRUE,
        (thr > thr_prev) & (bracketavg > thr) ~ TRUE,
        TRUE ~ FALSE
      ),
      run_id = cumsum(new_run)
    )
  
  # Merge each run: p = first (left boundary), thr = first thr of run,
  # bracketavg = width-weighted across merged span; 'average' = mean
  merged <- d %>%
    group_by(run_id) %>%
    summarise(
      p         = first(p),
      thr       = first(thr),
      bracketavg       = stats::weighted.mean(bracketavg, w = width, na.rm = TRUE),
      average   = mean(average, na.rm = TRUE),
      width_tot = sum(width, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    arrange(p)
  
  # Collapse any remaining equal thresholds (even if bracketavg differs slightly)
  merged2 <- merged %>%
    group_by(thr) %>%
    summarise(
      p       = min(p),
      bracketavg     = stats::weighted.mean(bracketavg, w = width_tot, na.rm = TRUE),
      average = mean(average, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(p)
  
  # Safety clamps post-averaging
  merged2 %>%
    mutate(
      thr = pmax(thr, 0),
      bracketavg = pmax(bracketavg, 0)
    )
}
