if(Sys.info()[4] %in% c("LAZ-ID1",
                        "LAZ-ID2",
                        "LAZ-DEID1",
                        "LAZ-DEID2")){
  setwd("E:/CT_APCD/Sai/suicide_cost")
}
library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", "knitr",
               "dplyr", "lubridate", "tidyverse", "matrixStats")
need.packages(need_pkgs)
source("get_suicide_attempts.R")

cols <- c("first_service_dt", "INTERNAL_MEMBER_ID",
             # "ICD_VERSION_IND", "DIAGNOSIS_CODE", 
          "MEDICAL_CLAIM_SERVICE_LINE_ID")
dgxcols <- c("MEDICAL_CLAIM_SERVICE_LINE_ID",
             "ICD_VERSION_IND", "DIAGNOSIS_CODE")
elig_pt <- fread(paste0("E:/CT_APCD/shared/intermediate_data/", 
                        "APCD_modified/eligibility/By_Fiscal_Year/", 
                        "medical_elig_all.csv"),
                 colClasses = "character",
                 select = c("INTERNAL_MEMBER_ID",
                            "birth_dt",
                            "FiscalYR")) %>% unique(use.key = FALSE)
for(yr in 2013:2017){
  cat(paste0("begin ", yr, " at ", Sys.time(), "\n"))
  mcpt <- fread(paste0("E:/CT_APCD/shared/intermediate_data/", 
                       "APCD_modified/medicare_medicare_ad_patients/", 
                       "fy", yr, "all_ages.csv"),
                colClasses = "character")
  elig_pt1 <- elig_pt[FiscalYR == yr]
  mc <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/", 
                          "cost_measure_intermediate_data/", 
                          "medical_fiscalyear_", yr, ".csv"), 
                   colClasses = "character",
                   select = cols)[
                     !INTERNAL_MEMBER_ID %in% mcpt$INTERNAL_MEMBER_ID]
  mc <- mc[INTERNAL_MEMBER_ID %in% elig_pt1$INTERNAL_MEMBER_ID]
  for(i in list.files("E:/CT_APCD/Sai/intermediate_data/dgs_sub_files/")){
    cat(paste0("begin ",  yr, " ", i,  " at ", Sys.time(), "\n"))
    dgx <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/dgs_sub_files/", i),
                 colClasses = "character", encoding = "UTF-8",
                 select = dgxcols)[
                   MEDICAL_CLAIM_SERVICE_LINE_ID %in% 
                     mc$MEDICAL_CLAIM_SERVICE_LINE_ID]
    if(dim(dgx)[1] > 0) {
      dgx2 <- mc[dgx, on = "MEDICAL_CLAIM_SERVICE_LINE_ID"]
      rm(dgx)
      gc()
      dgx2[ICD_VERSION_IND == 0, 
           `:=`(DIAGNOSIS_CODE = icd_map(DIAGNOSIS_CODE, 10, 9, 
                                     method = "both"),
                ICD_VERSION_IND = 9)]
      fwrite(dgx2, 
             file = paste0("output/dgx_", yr, ".csv"), append = TRUE)
      rm(dgx2)
      gc()
    }
    cat(paste0(yr, " ", i,  " done at ", Sys.time(), "\n"))
  }
  # fwrite(mc, 
  #        file = paste0("output/mc_", yr, "_.csv"))
  rm(mcpt, elig_pt1, mc)
  cat(paste0(yr, " done at ", Sys.time(), "\n"))
}
