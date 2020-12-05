add_wilson_ci <- function(data, count, population, f = 10000, round = 2, level = 0.95, rate = "rate", lower = "lower_rate", upper = "upper_rate"){
  
  # confidence intervals with Wilson method 
  #simpler than the direct method 
  
  # Rothman, Kenneth J. 2012. Epidemiology: An Introduction. New York: Oxford University Press.

  requireNamespace("dplyr", quietly = T)
  
  ###
  a <- data %>% pull({{count}})
  N <- data %>% pull({{population}})
  p <- a/N
  z <- -qnorm((1-level)/2)
  moe <-((z/sqrt(N))*sqrt(p*(1-p) + z^2/(4*N)))/(1+z^2/N)
  center <-(p+z^2/(2*N))/(1+z^2/N)
  ll <- round((center-moe)*f, round)
  ul <- round((center+moe)*f, round)
  
  data %>% mutate({{rate}} := round(p*f, round),
                  {{lower}} := ll,
                  {{upper}} := ul)
  
  }
