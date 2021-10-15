
svip_fatal_tbi <- function(data, mcause) {
  requireNamespace("dplyr", quietly = T)
  requireNamespace("useicd10cm", quietly = T)

  # icd10 core svipp valid fatal injury subset ----------------------------------

  # "V01–Y36, Y85–Y87, Y89, U01–U03 Injury and poisoning"

icd10_tbi_ <- "S01|S02[013789|S040|S06|S07[0189]|S09[7-9]|T90[124589]"

  data %>%
    icd_create_indicator(
      new_name = "svip_fatal_tbi",
      expr = icd10_tbi_,
      colvec = mcause
    )
}



