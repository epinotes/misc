add_age13 <- function(data, age){
  suppressWarnings(suppressMessages(require(classInt)))
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(forcats)))
  age <- data %>% pull({{age}}) %>% unlist()
  age_max <- ifelse(max(age, na.rm = T) > 84, max(age, na.rm = T), 120)
  agecut <- c(0, 0.99, 4, 9, 14, 19, 24, 34, 44, 54, 64, 74, 84, age_max)
  int <- classInt::classIntervals(age, n = 13, style = "fixed", 
                          fixedBreaks = agecut, intervalClosure = "right")
  agegrp <- classInt::findCols(int)
  data %>% add_column(agegrp13 = all_of(agegrp)) %>% mutate(age13 = fct_recode(as.factor(agegrp13), 
                                                          `<1` = "1", 
                                                          `01-04` = "2", 
                                                          `05-09` = "3", 
                                                          `10-14` = "4",
                                                          `15-19` = "5", 
                                                          `20-24` = "6",
                                                          `25-34` = "7", 
                                                          `35-44` = "8",
                                                          `45-54` = "9", 
                                                          `55-64` = "10", 
                                                          `65-74` = "11", 
                                                          `75-84` = "12",
                                                          `85+` = "13"))
}
