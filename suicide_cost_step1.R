library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats")
need.packages(need_pkgs)
source("get_suicide_attempts.R")

# dgxcols <- c("MEDICAL_CLAIM_SERVICE_LINE_ID",
#              "ICD_VERSION_IND", "DIAGNOSIS_CODE")
cols <- c("first_service_dt", "INTERNAL_MEMBER_ID",
          "ICD_VERSION_IND", "DIAGNOSIS_CODE",
          "MEDICAL_CLAIM_SERVICE_LINE_ID")
elig_pt <- fread(paste0("E:/CT_APCD/shared/intermediate_data/", 
                        "APCD_modified/eligibility/By_Fiscal_Year/", 
                        "medical_elig_all.csv"),
                 colClasses = "character",
                 select = c("INTERNAL_MEMBER_ID",
                            "birth_dt",
                            "FiscalYR")) %>% unique(use.key = FALSE)
for (yr in 2013:2017){
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
  mc[ICD_VERSION_IND == 0, 
     `:=`(DIAGNOSIS_CODE = icd_map(DIAGNOSIS_CODE, 10, 9, 
                                   method = "both"),
          ICD_VERSION_IND = 9)]
  dgx <- fread(paste0("output/dgx_", yr, ".csv"),
               colClasses = "character")
  mc_dgx <- rbind(dgx, mc) %>% unique(use.key = FALSE)
  fwrite(mc_dgx, file = paste0("output/mc_dgx_unique_",
                               yr, ".csv"))
  cat(paste0(yr, " done at ", Sys.time(), "\n"))
}