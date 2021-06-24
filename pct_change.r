pct_change <- function(prop, n = 1, p = 100){
  
 p*(prop - dplyr::lag(prop, n = n))/dplyr::lag(prop, n = n)  
}
