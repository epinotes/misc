pct_change <- function(prop, n = 1){
  
  scales::percent((prop - dplyr::lag(prop, n = n))/dplyr::lag(prop, n = n))  
}
