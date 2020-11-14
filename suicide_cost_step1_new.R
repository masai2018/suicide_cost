library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", "knitr",
               "dplyr", "lubridate", "tidyverse", "matrixStats")
need.packages(need_pkgs)
source("get_suicide_attempts.R")

cols <- c("MEDICAL_CLAIM_HEADER_ID", "INTERNAL_MEMBER_ID",
             "ICD_VERSION_IND", "DIAGNOSIS_CODE")
for(yr in 2013:2017){
  cat(paste0("begin year ", yr, " at ", Sys.time(), "\n"))
  mcpt <- fread(paste0("E:/CT_APCD/shared/intermediate_data/", 
                       "APCD_modified/medicare_medicare_ad_patients/", 
                       "fy", yr, "all_ages.csv"),
                colClasses = "character")
  # dgx.tmp <- fread(paste0(dgxdir, dgxname), select = dgxcols, colClasses = "character")
  dgx.tmp <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/", 
                          "cost_measure_intermediate_data/", 
                          "medical_fiscalyear_", yr, ".csv"), 
                   colClasses = "character",
                   select = cols)[
                     !INTERNAL_MEMBER_ID %in% mcpt$INTERNAL_MEMBER_ID]
  dgx.tmp[ICD_VERSION_IND == 0, 
          DIAGNOSIS_CODE := icd_map(DIAGNOSIS_CODE, 10, 9, 
                                    method = "both")]
  dgx.tmp2 <- dgx.tmp[, lapply(.SD, combine_words, and = ""), 
                      .SDcols = "DIAGNOSIS_CODE",
                      by = c("MEDICAL_CLAIM_HEADER_ID",
                             "INTERNAL_MEMBER_ID")]
  sc.tmp <- isSuicideAttempt_icd9(dgx.tmp2$DIAGNOSIS_CODE, need_split = TRUE)
  sc.tmp <- data.frame(t(sapply(sc.tmp, c)))
  sc.tmp2 <- rowSums(sc.tmp, na.rm = T)
  if(max(sc.tmp2, na.rm = T) > 0){
    inds <- which(sc.tmp2 > 0)
    dgx.tmp2 <- dgx.tmp2[inds, ]
    fwrite(dgx.tmp2, file = paste0(output_dir("output_new/"), 
                             "sc_step1_", yr, 
                             "_new.csv"))
  }
  cat(paste0("find ", sum(sc.tmp2 > 0, na.rm = T), " suicaid attemps cods in year ", yr, "\n"))
  cat(paste0("year ", yr, " done at ", Sys.time(), "\n"))
  rm(dgx.tmp, dgx.tmp2, sc.tmp, sc.tmp2)
  gc()
}
