fit_and_tab <- function(d) {
  
  d <- collapse_and_clean(d)
  d <- enforce_avg_strictly_inside(d)
  
  all_thr_na <- all(is.na(d$thr))
  dist <- if (all_thr_na) {
    gpinter::tabulation_fit(
      p          = d$p,
      bracketavg = d$bracketavg,
      average    = d$average[1]
    )
  } else {
    gpinter::tabulation_fit(
      p          = d$p,
      threshold  = d$thr,
      bracketavg = d$bracketavg,
      average    = d$average[1]
    )
  }
  
  tab <- gpinter::generate_tabulation(
    dist,
    fractiles    = p_grid,
    threshold    = TRUE,
    bracketavg   = TRUE,
    topavg       = TRUE,
    invpareto    = TRUE
  )
  
  x <- tibble(
    p       = tab$fractile,
    thr     = tab$threshold,
    avg     = tab$bracket_average,
    topavg  = tab$top_average,
    b       = tab$invpareto
  )
  
  return(x)
}

