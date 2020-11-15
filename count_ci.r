count_ci <- function(x, round = 0){
  LL = round(exactci::poisson.exact(x)$conf.int[[1]], round)
  UL = round(exactci::poisson.exact(x)$conf.int[[2]], round)
  
  glue::glue("{x} ({LL}, {UL})")
}
  
