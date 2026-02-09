recompute_total_avg <- function(d) {
  
  #create a new row with p = 0 and avg = 0 
  new_row <- d[1, ]
  new_row[1, ] <- NA
  new_row$p <- 0
  new_row$bracketavg <- 0
  
  #recompute average 
  dx <- rbind(d, new_row) %>% arrange(p) %>% mutate(
    p_next = lead(p, default = 1),
    w = round(p_next - p, 6)
  )
  newa <- with(dx, weighted.mean(bracketavg, w, na.rm = TRUE))
  d <- d %>% mutate(average = newa) 
}