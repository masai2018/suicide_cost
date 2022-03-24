# all the claims for the SA and non-SA group 
# that meet the continuous enrollment and medical 
# claims requirements

if(Sys.info()[4] %in% c("LAZ-ID1",
                        "LAZ-ID2",
                        "LAZ-DEID1",
                        "LAZ-DEID2")){
  setwd("E:/CT_APCD/Sai/suicide_cost")
}
library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", "haven", "knitr",
               "dplyr", "lubridate", "tidyverse", "matrixStats", "berryFunctions")
need.packages(need_pkgs)
source("get_suicide_attempts.R")



pt1 <- fread('output/sc_15_no_sc_13_14.csv', colClasses = 'character',
            select = c("INTERNAL_MEMBER_ID", "birth_dt", 
                       "GENDER_CODE")) %>% unique()
pt2 <- fread(paste0("output/no_sc_2013_2015_pt.csv"), colClasses = "character") %>% 
  unique(use.key = FALSE)
pt <- rbind(pt1, pt2)
pt[, age := 2015 - as.integer(birth_dt)]
pt <- pt[age < 65]
pt[, `:=`(sc_15_no_sc_13_14 = 0,
          no_sc_13_14_15 = 0)]
pt[INTERNAL_MEMBER_ID %in% pt1$INTERNAL_MEMBER_ID, sc_15_no_sc_13_14 := 1]
pt[INTERNAL_MEMBER_ID %in% pt2$INTERNAL_MEMBER_ID, no_sc_13_14_15 := 1]
mc_dgx <- data.table()
for(yr in 2013:2017){
  tmp <- fread(paste0("output/mc_dgx_unique_", yr, ".csv"), 
               colClasses = "character")[
    INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID
  ]
  tmp2 <- tmp[, lapply(.SD, combine_words, sep = ",", and = ","), 
              .SDcols = "DIAGNOSIS_CODE",
              by = c("MEDICAL_CLAIM_SERVICE_LINE_ID")]
  tmp2[, DIAGNOSIS_CODE := gsub(",,", ",", DIAGNOSIS_CODE, fixed = TRUE)]
  setnames(tmp2, "DIAGNOSIS_CODE", "DIAGNOSIS_CODE_combined")
  tmp <- tmp2[tmp, on = "MEDICAL_CLAIM_SERVICE_LINE_ID"]
  rm(tmp2)
  gc()
  mc_allowed <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/", 
                     "cost_measure_intermediate_data/", 
                     "cost_files_by_year/total_", yr, "_all_ages.csv"), 
              colClasses = "character", 
              select = c("INTERNAL_MEMBER_ID",
                         "MEDICAL_CLAIM_HEADER_ID",
                         "ALLOWED_AMT"))[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  mc_allowed <- mc_allowed[, -"INTERNAL_MEMBER_ID"]
  mc <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/", 
                     "cost_measure_intermediate_data/", 
                     "medical_fiscalyear_", yr, ".csv"), 
              colClasses = "character", 
              select = c("INTERNAL_MEMBER_ID",
                         "MEDICAL_CLAIM_SERVICE_LINE_ID",
                         "MEDICAL_CLAIM_HEADER_ID"))[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  mc <- mc[, -"INTERNAL_MEMBER_ID"]
  mc_allowed <- mc[mc_allowed, on = "MEDICAL_CLAIM_HEADER_ID"]
  rm(mc)
  gc()
  tmp <- mc_allowed[tmp, on = "MEDICAL_CLAIM_SERVICE_LINE_ID"]
  rm(mc_allowed)
  gc()
  tmp <- tmp[pt, on = "INTERNAL_MEMBER_ID"]
  mc_dgx <- rbind(mc_dgx, tmp)
  rm(tmp)
  gc()
}
sum(!unique(pt$INTERNAL_MEMBER_ID) %in% mc_dgx$INTERNAL_MEMBER_ID)
sum(!unique(mc_dgx$INTERNAL_MEMBER_ID) %in% pt$INTERNAL_MEMBER_ID)
fwrite(mc_dgx, 
       file = paste0("output/all_claims_SA_non_SA_20220131.csv"))
# saveRDS(mc_dgx, 
#         file = paste0("output/all_claims_SA_non_SA_20220131.rds"))
write_sas(mc_dgx, "output/all_claims_SA_non_SA_20220131.sas7bdat")
