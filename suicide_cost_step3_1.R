if(Sys.info()[4] %in% c("LAZ-ID1",
                        "LAZ-ID2",
                        "LAZ-DEID1",
                        "LAZ-DEID2")){
  setwd("E:/CT_APCD/Sai/suicide_cost")
}
library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", "haven",
               "dplyr", "lubridate", "tidyverse", "matrixStats")
need.packages(need_pkgs)
source("get_suicide_attempts.R")


pt_all <- fread(paste0("output/sc_15pt.csv"), colClasses = "character")
pt_all <- fread(paste0("output/sc_15_no_sc_13_14_pt.csv"), colClasses = "character")
pt_no <- fread(paste0("output/no_sc_2013_2015_pt.csv"), colClasses = "character")

# elig_pt <- data.table(read_sas("E:/CT_APCD/Beth/data4/eligibility_4.sas7bdat"))
# elig_pt[, INTERNAL_MEMBER_ID := as.character(INTERNAL_MEMBER_ID)]
# elig_pt <- elig_pt[, c("INTERNAL_MEMBER_ID",
#                        "birth_dt")] %>% unique(use.key = FALSE)
medicare_ad_code <- c('HN', 'AB', 'MA', 'MD','12', '43', '14', '16', '15',
                      'CP', 'MB', 'MP', '41', '13', '47', '42', 'ZZ',
                      NULL)

elig_pt0 <- fread(paste0("E:/CT_APCD/shared/intermediate_data/",
                         "APCD_modified/eligibility/By_Fiscal_Year/",
                         "medical_elig.csv"),
                  colClasses = "character") %>% unique(use.key = FALSE)
elig_pt0[, `:=`(NumGapsLE45days = as.numeric(NumGapsLE45days),
                NumGapsGT45days = as.numeric(NumGapsGT45days),
                Elig_Sep30 = as.numeric(Elig_Sep30))]
# elig_pt2 <- fread(paste0("E:/CT_APCD/Beth/data4/Eligibility/",
#                          "By_Fiscal_Year/MEDICAL_ELIG_ALLRECS_FY_2012_17.csv"),
#                   colClasses = "character")
# elig_pt2[, `:=`(gapLE45days = as.numeric(gapLE45days),
#                 gapGT45days = as.numeric(gapGT45days),
#                 Elig_Sep30 = as.numeric(Elig_Sep30))]
elig_pt3 <- data.table(read_sas("E:/CT_APCD/Beth/data4/eligibility_4.sas7bdat"))
elig_pt3[, INTERNAL_MEMBER_ID := as.character(INTERNAL_MEMBER_ID)]
elig_pt3 <- elig_pt3[ymd(eligibility_start_dt) < ymd("2017-10-01") & 
                       ymd(eligibility_end_dt) > ymd("2012-09-30") & 
                       COVERAGE_CLASS == "MEDICAL"]
mcpt <- elig_pt3[SUBMITTER_ID == "15227" |
                   PRODUCT_CODE %in% medicare_ad_code,
                 .(INTERNAL_MEMBER_ID)] %>% unique()
elig_pt <- elig_pt0[!INTERNAL_MEMBER_ID %in% mcpt$INTERNAL_MEMBER_ID]
sum(!pt_all$INTERNAL_MEMBER_ID %in% unique(elig_pt$INTERNAL_MEMBER_ID))
pt_all <- pt_all[INTERNAL_MEMBER_ID %in% unique(elig_pt$INTERNAL_MEMBER_ID)]
pt_all <- unique(elig_pt3[, .(INTERNAL_MEMBER_ID, birth_dt,
                              GENDER_CODE)])[pt_all, on = "INTERNAL_MEMBER_ID"]
pt_all[, dob_gender := paste0(birth_dt, GENDER_CODE)]
pt_no <- pt_no[INTERNAL_MEMBER_ID %in% unique(elig_pt$INTERNAL_MEMBER_ID)]
pt_no <- unique(elig_pt3[, .(INTERNAL_MEMBER_ID, birth_dt,
                              GENDER_CODE)])[pt_no, on = "INTERNAL_MEMBER_ID"]
claim2015 <- fread("E:/CT_APCD/Sai/intermediate_data/cost_measure_intermediate_data/medical_fiscalyear_2015.csv",
                   colClasses = "character",
                   select = "INTERNAL_MEMBER_ID") %>% unique()
pt_no <- pt_no[INTERNAL_MEMBER_ID %in% claim2015$INTERNAL_MEMBER_ID]
pt_no <- pt_no[!INTERNAL_MEMBER_ID %in% pt_all$INTERNAL_MEMBER_ID]
pt_no[, dob_gender := paste0(birth_dt, GENDER_CODE)]
pt_no <- pt_no[dob_gender %in% unique(pt_all$dob_gender)]
pt_no <- pt_no[, -"dob_gender"]



# for(i in names(pt_all)[5:8]){
## total medical claim
pt <- pt_no
pt <- pt_all
scmccost <- pt
for(fyear in 2017:2013){
  mccost <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                         "cost_measure_intermediate_data/", 
                         "cost_files_by_year/", 
                         "total_", fyear, "_all_ages.csv"),
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
scmccost <- pt[scmccost[, -c("birth_dt",
                             "GENDER_CODE")], on = "INTERNAL_MEMBER_ID"]
names(scmccost)[4:8] <- paste0("fy", 2013:2017, "_total_medical_cost")
fwrite(scmccost,
       file = paste0("output/cost/compare_total_medical_cost_", ".csv"))


## total pharmacy cost
scphcost <- pt[, -c("birth_dt")]
for(fyear in 2017:2013){
  phcost <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                         "cost_measure_intermediate_data/", 
                         "cost_files_by_year/", 
                         "total_pharmacy_", fyear, "_all_ages.csv"),
                  colClasses = "character")
  phcost <- phcost[, total := as.numeric(total)
                   ][total >= 0 & 
                       INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  phcost <- unique(phcost, usekey = FALSE)
  phcost.smy <- phcost[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "total",
                       by = "INTERNAL_MEMBER_ID"]
  setnames(phcost.smy, "total", paste0(fyear, "cost"))
  scphcost <- phcost.smy[scphcost, on = "INTERNAL_MEMBER_ID"]
}
scphcost <- pt[scphcost[, -"GENDER_CODE"], on = "INTERNAL_MEMBER_ID"]
names(scphcost)[4:8] <- paste0("fy", 2013:2017, "_total_pharmacy_cost")

fwrite(scphcost,
       file = paste0("output/cost/compare_total_pharmacy_cost_", ".csv"))

## ip cost
scipcost <- pt[, -c("birth_dt", "GENDER_CODE")]
for(fyear in 2017:2013){
  ipcost <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                         "cost_measure_intermediate_data/", 
                         "value_sets_fyear/", 
                         "ip_raw_", fyear, "_all_ages.csv"),
                  colClasses = "character")
  names(ipcost) <- toupper(names(ipcost))
  ipcost <- ipcost[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
                   ][ALLOWED_AMT >= 0 & 
                       INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  ipcost <- unique(ipcost, usekey = FALSE)
  ipcost.smy <- ipcost[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  setnames(ipcost.smy, "ALLOWED_AMT", paste0(fyear, "cost"))
  scipcost <- ipcost.smy[scipcost, on = "INTERNAL_MEMBER_ID"]
}
scipcost <- pt[scipcost, on = "INTERNAL_MEMBER_ID"]
names(scipcost)[4:8] <- paste0("fy", 2013:2017, "_ip_cost")
fwrite(scipcost,
       file = paste0("output/cost/compare_ip_cost_", ".csv"))

## op cost
scopcost <- pt[, -c("birth_dt","GENDER_CODE" )]
for(fyear in 2017:2013){
  opcost <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                         "cost_measure_intermediate_data/", 
                         "value_sets_fyear/", 
                         "op_raw_", fyear, "_all_ages.csv"),
                  colClasses = "character")
  names(opcost) <- toupper(names(opcost))
  opcost <- opcost[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
                   ][ALLOWED_AMT >= 0 & 
                       INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  opcost <- unique(opcost, usekey = FALSE)
  opcost.smy <- opcost[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  setnames(opcost.smy, "ALLOWED_AMT", paste0(fyear, "cost"))
  scopcost <- opcost.smy[scopcost, on = "INTERNAL_MEMBER_ID"]
}
scopcost <- pt[scopcost, on = "INTERNAL_MEMBER_ID"]
names(scopcost)[4:8] <- paste0("fy", 2013:2017, "_op_cost")

fwrite(scopcost,
       file = paste0("output/cost/compare_op_cost_", ".csv"))


## pc cost
scpccost <- pt[, -c("birth_dt", "GENDER_CODE" )]
for(fyear in 2017:2013){
  pccost <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                         "cost_measure_intermediate_data/", 
                         "value_sets_fyear/", 
                         "pc_raw_", fyear, "_all_ages.csv"),
                  colClasses = "character")
  names(pccost) <- toupper(names(pccost))
  pccost <- pccost[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
                   ][ALLOWED_AMT >= 0 & 
                       INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  pccost <- unique(pccost, usekey = FALSE)
  pccost.smy <- pccost[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  setnames(pccost.smy, "ALLOWED_AMT", paste0(fyear, "cost"))
  scpccost <- pccost.smy[scpccost, on = "INTERNAL_MEMBER_ID"]
}
scpccost <- pt[scpccost, on = "INTERNAL_MEMBER_ID"]
names(scpccost)[4:8] <- paste0("fy", 2013:2017, "_pc_cost")
fwrite(scpccost,
       file = paste0("output/cost/compare_pc_cost_", ".csv"))

## combine them
rt <- pt
for(rtname in c("total_medical", "total_pharmacy", "ip", "op", "pc")){
  rt_tmp <- fread(paste0("output/cost/compare_", rtname, "_cost_", ".csv"), colClasses = "character")
  rt <- rt[rt_tmp[, -c("birth_dt", "GENDER_CODE" )], on = "INTERNAL_MEMBER_ID"]
}

fwrite(rt,
       file = paste0("output/cost/compare_all_cost_", ".csv"))

# }

