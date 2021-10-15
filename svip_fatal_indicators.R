
svip_fatal_indicators <- function(data, underly) {

  # icd10 core svipp fatal injury indicators except tbi -----

  requireNamespace("dplyr", quietly = T)
  requireNamespace("useicd10cm", quietly = T)

  icd10_drowning_ <- "W6[5-9]|W7[0-4]|V9[02]"

  icd10_unintentional_falls_ <- "W[01]"

  icd10_unintentional_fire_ <- "X0"

  icd10_firearm_ <- "W[3][2-4]|X7[2-4]|X9[3-5]|Y2[2-4]|Y350|U014"

  icd10_homicide_ <- "X8[5-9]|X9|Y0|Y871|U01|U02"

  icd10_unintentional_mvt_ <- "V0[2-4][19]|V092|V1[2-4][3-9]|V19[4-6]|V2[0-8][3-9]|V29[4-9]|V[3-7][0-9][4-9]|V80[3-5]|V8[12]1|V8[3-6][0-3]|V87[0-8]|V892"

  icd10_non_drug_poisoning_ <- "X[46][5-9]|X8[6-9]|X90|Y1[5-9]|Y352|U01[67]"

  icd10_suicide_ <- "X[67]|X8[0-4]|Y870|U03"

  data %>%

    icd_create_indicator(
      new_name = "svip_fatal_drowning",
      expr = icd10cm_drowning_,
      colvec = underly
    ) %>%
    icd_create_indicator(
      new_name = "svip_fatal_falls",
      expr = icd10_unintentional_falls_,
      colvec = underly
    ) %>%
    icd_create_indicator(
      new_name = "svip_fatal_fire",
      expr = icd10_unintentional_fire_,
      colvec = underly
    ) %>%
    icd_create_indicator(
      new_name = "svip_fatal_firearm",
      expr = icd10_firearm_,
      colvec = underly
    ) %>%
    icd_create_indicator(
      new_name = "svip_homicide",
      expr = icd10_homicide_,
      colvec = underly
    ) %>%
    icd_create_indicator(
      new_name = "svip_fatal_mvt",
      expr = icd10_unintentional_mvt_,
      colvec = underly
    ) %>%
    icd_create_indicator(
      new_name = "svip_fatal_non_drug_poisoning",
      expr = icd10_non_drug_poisoning_,
      colvec = underly
    ) %>%
    icd_create_indicator(
      new_name = "svip_suicide",
      expr = icd10_suicide_,
      colvec = underly
    )
}
