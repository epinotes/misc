pct_change <- function(prop, n = 1){
  
 100*(prop - dplyr::lag(prop, n = n))/dplyr::lag(prop, n = n)  
}
