# rounding precision to collapse tiny float gaps
thr_digits <- 2
avg_digits <- 2

# tolerance used for “strictness” checks (scale-aware sanitizer uses its own eps too)
tol <- 0.5 * 10^(-max(thr_digits, avg_digits))

# Scale-aware epsilon for sanitizer
sanitize_eps <- function(x) {
  pmax(1e-6, 1e-9 * pmax(abs(x), 1))
}

strict_margin <- function(x) {
  pmax(sanitize_eps(x), tol)
}

enforce_avg_strictly_inside <- function(d) {
  d %>%
    arrange(p) %>%
    mutate(
      thr_lo = thr,
      thr_hi = lead(thr),
      g      = thr_hi - thr_lo,
      # base margins from scale + tol
      m_lo0  = strict_margin(thr_lo),
      m_hi0  = strict_margin(coalesce(thr_hi, thr_lo)),
      # gap-aware margin
      eps    = ifelse(is.finite(thr_hi),
                      pmax(0, pmin(m_lo0, m_hi0, g/3)),
                      m_lo0),
      bracketavg = ifelse(
        is.finite(thr_hi),
        pmin(pmax(bracketavg, thr_lo + eps), thr_hi - eps),
        pmax(bracketavg, thr_lo + eps)   # open top
      )
    ) %>%
    select(-thr_lo, -thr_hi, -g, -m_lo0, -m_hi0, -eps)
}