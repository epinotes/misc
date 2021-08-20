add_age11 <- function(data, age){
  suppressWarnings(suppressMessages(require(classInt)))
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(forcats)))
  age <- data %>% pull({{age}}) %>% unlist()
  age_max <- ifelse(max(age, na.rm = T) > 84, max(age, na.rm = T), 120)
  agecut <- c(0, 0.99, 4, 14, 24, 34, 44, 54, 64, 74, 84, age_max)
  int <- classInt::classIntervals(age, n = 11, style = "fixed", 
                          fixedBreaks = agecut, intervalClosure = "right")
  agegrp <- classInt::findCols(int)
  data %>% mutate(agegrp11 = all_of(agegrp), age11 = fct_recode(as.factor(agegrp11), 
                                                          `<1` = "1", 
                                                          `01-04` = "2", 
                                                          `05-14` = "3", 
                                                          `15-24` = "4", 
                                                          `25-34` = "5", 
                                                          `35-44` = "6",
                                                          `45-54` = "7", 
                                                          `55-64` = "8", 
                                                          `65-74` = "9", 
                                                          `75-84` = "10",
                                                          `85+` = "11"))
}
