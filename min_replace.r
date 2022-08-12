min_replace <- function(data, cond_var, min_value = 1, max_value = 9, replacement, ...) {

  # ... variables with values to replace with with 'replacement'
  # no quotations marks or vector of numeric indices
  
  sel <- c({{cond_var}}, quos(...))

  data %>%
    mutate(
      across(c(!!!sel),
             function(x) ifelse(between({{cond_var}}, min_value, max_value), replacement, x)))
}
