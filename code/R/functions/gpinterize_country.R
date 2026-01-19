gpinterize_country <- function(c) {
  
  require(readxl)
  
  #read country-excel  
  file <- paste0("input_data/admin_data/", c, "/_clean/", "total-pre-", c, ".xlsx")
  sheets <- excel_sheets(file)
  dl <- lapply(sheets, function(sheet) read_excel(file, sheet = sheet))
  names(dl) <- sheets
  
  #apply gpinter to all years 
  all <- map_dfr(dl, fit_and_tab, .id = "year") %>% 
    mutate(country = c) %>%
    select(country, year, p, thr, avg, topavg, b)
  
  return(all)
}