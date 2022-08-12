min_replace <- function(data, cond_var, min_value = 1, max_value = 9, replacement, ...) {

  # ... variables with values to replace with with 'replacement'
  # no quotations marks or vector of numeric indices
  
  sel <- c(quos(...))

  data %>%
    mutate(
      across(c(!!!sel, {{as.numeric(cond_var)}}),
             function(x) ifelse(between({{cond_var}}, min_value, max_value), replacement, x)))
}
