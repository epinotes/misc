add_synthetic_opioid <- function(data, diag_ecode_col) {
  
  requireNamespace("dplyr", quietly = T)

  fentanyl_icd10cm_ <- "T4041(A|$)"
  
  tramadol_icd10cm_ <- "T4042(A|$)"
  
  other_synth_opioid_icd10cm_ <- "T4049(A|$)"
  
  
  data %>%
    mutate(
      fentanyl = icd_new_diag(.,
                                expr = fentanyl_icd10cm_,
                                colvec = diag_ecode_col
      ),
      
      tramadol = icd_new_diag(.,
                                       expr = tramadol_icd10cm_ ,
                                       colvec = diag_ecode_col
      ),
      
      other_synth_opioid = icd_new_diag(.,
                            expr = other_synth_opioid_icd10cm_,
                            colvec = diag_ecode_col
      )
    )
      
      
}
