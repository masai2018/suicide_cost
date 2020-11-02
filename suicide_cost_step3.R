library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats")
need.packages(need_pkgs)
source("get_suicide_attempts.R")


elig_pt <- fread(paste0("E:/CT_APCD/shared/intermediate_data/", 
                        "APCD_modified/eligibility/By_Fiscal_Year/", 
                        "medical_elig_all.csv"),
                 colClasses = "character",
                 select = c("INTERNAL_MEMBER_ID",
                            "birth_dt",
                            "FiscalYR")) %>% unique(use.key = FALSE)
for(fyear in 2015:2013){
  mcpt <- fread(paste0("E:/CT_APCD/shared/intermediate_data/", 
                           "APCD_modified/medicare_medicare_ad_patients/", 
                           "fy", fyear, "all_ages.csv"),
                    colClasses = "character")
  pt <- elig_pt[FiscalYR == fyear][!mcpt, on = "INTERNAL_MEMBER_ID"]
  scpt <- fread(paste0("output/sc_", fyear, ".csv"), select = "INTERNAL_MEMBER_ID",
                    colClasses = "character") %>% unique()
  scpt <- scpt[pt, on = "INTERNAL_MEMBER_ID",
                       nomatch = 0]
  fname <- paste0("scpt", fyear)
  assign(fname, scpt)
}
pt <- scpt2015[!INTERNAL_MEMBER_ID %in% scpt2013$INTERNAL_MEMBER_ID |
                 !INTERNAL_MEMBER_ID %in% scpt2014$INTERNAL_MEMBER_ID
               ][, -"FiscalYR"]
## total medical claim
scmccost <- pt[, -"birth_dt"]
for(fyear in 2017:2013){
  mccost <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                         "cost_measure_intermediate_data/", 
                         "cost_files_by_year/", 
                         "total_", fyear, "_all_ages_com.csv"),
                  colClasses = "character")
  mccost <- mccost[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
                   ][ALLOWED_AMT >= 0 & 
                       INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  mccost <- unique(mccost, usekey = FALSE)
  mccost.smy <- mccost[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  setnames(mccost.smy, "ALLOWED_AMT", paste0(fyear, "cost"))
  scmccost <- mccost.smy[scmccost, on = "INTERNAL_MEMBER_ID"]
}
scmccost <- scmccost[pt, on = "INTERNAL_MEMBER_ID"]
fwrite(scmccost,
       file = paste0("output/total_medical_cost.csv"))