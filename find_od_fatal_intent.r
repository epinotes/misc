find_od_fatal_intent <- function(data, underly) {
  requireNamespace("dplyr", quietly = T)
  requireNamespace("useicd10", quietly = T)

  create_indicator <- function(data, new_name, expr, colvec, ignore.case = T, perl = T) {
    data %>% mutate(`:=`({{ new_name }}, case_when(
      if_any({{ colvec }}, function(x) {
      grepl(expr, x, ignore.case = ignore.case, perl = perl)
    }) ~ 1, 
    TRUE ~ 0)))
  }
  
  # X40-X44: Accidental poisonings by drugs
  # X60-X64: Intentional self-poisoning by drugs
  # X85: Assault by drug poisoning
  # Y10-Y14: Drug poisoning of undetermined intent

  unintentional_od_ <- "X4[0-4]"

  self_od_ <- "X6[0-4]"

  assault_od_ <- "X85"

  undetermined_od_ <- "Y1[0-4]"

  data %>%
    create_indicator(
      new_name = "unintentional_fatal_od",
      expr = unintentional_od_,
      colvec = underly
    ) %>%
    create_indicator(
      new_name = "self_harm_fatal_od",
      expr = self_od_,
      colvec = underly
    ) %>%
    create_indicator(
      new_name = "assault_fatal_od",
      expr = assault_od_,
      colvec = underly
    ) %>%
    create_indicator(
      new_name = "undetermined_fatal_od",
      expr = undetermined_od_,
      colvec = underly
    )
}
