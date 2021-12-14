
svip_fatal_injury <- function(data, underly) {
  requireNamespace("dplyr", quietly = T)
  requireNamespace("useicd10cm", quietly = T)

# icd10 core svipp valid fatal injury subset ----------------------------------

# "V01–Y36, Y85–Y87, Y89, U01–U03 Injury and poisoning"

  icd10_valid_injury_ <- "V0[1-9]|W|X|Y[0-2]|Y3[0-6]|Y8[95-7]|U0[1-3]"

  data %>%
    icd_create_indicator(
      new_name = "svip_fatal_injury",
      expr = icd10_valid_injury_,
      colvec = underly
    )
}
