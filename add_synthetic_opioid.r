add_synthetic_opioid <- function(data, diag_ecode_col) {
  
  requireNamespace("dplyr", quietly = T)
  
  icd_new_diag <- function(data, expr, colvec, ignore.case = T, perl = T) {

  requireNamespace("dplyr", quietly = T)

 
  # assign '1' if the regular expression matched
  f1 <- function(x) grepl(expr, x, ignore.case = ignore.case, perl = perl)
  # any 1 in the diagnosis field suffices
  f2 <- function(x){
    sign(rowSums(x, na.rm = TRUE))
  }

  data %>% as_tibble() %>%
    select(all_of({{colvec}})) %>%
    mutate_all(as.character) %>%
    purrr::map_dfr(f1) %>%
    transmute(new_diag = f2(.)) %>%
    flatten_dbl()
}


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
