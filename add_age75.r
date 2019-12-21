add_age75 <- function(data, age){
  suppressWarnings(suppressMessages(require(classInt)))
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(forcats)))
  age <- data %>% pull({{age}}) %>% unlist()
  # <10    `10-17 18-24  25-34  35-44  45-54  55-64  65-74  75+
  age_max <- ifelse(max(age, na.rm = T) > 84, max(age, na.rm = T), 120)
  agecut75 <- c(0, 9, 17, 24, 34, 44, 54, 64, 74, age_max)
  int75 <- classIntervals(age, n = 9, style = "fixed", 
                          fixedBreaks = agecut75, intervalClosure = "right")
  agegrp75 <- as.factor(findCols(int75))
  data %>% mutate(agegrp75 = agegrp75, age75 = fct_recode(agegrp75, 
                                                          `<10` = "1", 
                                                          `10-17` = "2", 
                                                          `18-24` = "3", 
                                                          `25-34` = "4", 
                                                          `35-44` = "5",
                                                          `45-54` = "6", 
                                                          `55-64` = "7", 
                                                          `65-74` = "8",
                                                          `75+` = "9")) %>% 
    mutate(agegrp75 = as.double(as.character(agegrp75)))
}
