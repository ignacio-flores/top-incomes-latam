gpinterize_country <- function(c) {
  
  print(c)
  
  require(readxl)
  
  #read country-excel  
  file <- paste0("input_data/admin_data/", c, "/_clean/", "total-pre-", c, "-adults.xlsx")
  sheets <- excel_sheets(file)
  dl <- lapply(sheets, function(sheet) read_excel(file, sheet = sheet) )
  names(dl) <- sheets
  
  #drop first row(s) in selected cases
  if (c == "ARG") {
    yrs <- as.character(2013:2018)
    for (yr in yrs) {
      dl[[yr]] <- if (yr == "2018") dl[[yr]][-c(1,2), ] else dl[[yr]][-1, ]
    }
  }
  
  #define min p 
  df_minp <- dl %>%
    imap_dfr(~ mutate(.x, year = as.integer(.y))) %>%   
    group_by(year) %>%
    slice_min(order_by = p, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(country, year, p) %>% 
    rename(minp = `p`) 
  
  #apply gpinter to all years 
  all <- map_dfr(dl, fit_and_tab, .id = "year") %>% 
    mutate(country = c, year = as.integer(year)) %>% 
    select(country, year, p, thr, avg, topavg, b) 
  all <- left_join(all, df_minp, by = c("country", "year"))
  
  return(all)
}