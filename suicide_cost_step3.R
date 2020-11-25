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
for(yr in 2015:2013){
  mcpt <- fread(paste0("E:/CT_APCD/shared/intermediate_data/", 
                           "APCD_modified/medicare_medicare_ad_patients/", 
                           "fy", yr, "all_ages.csv"),
                    colClasses = "character")
  tmp <- elig_pt[FiscalYR == yr][!mcpt, on = "INTERNAL_MEMBER_ID"]
  scpt <- fread(paste0("output/sc_pt_com_", yr, ".csv"), select = "INTERNAL_MEMBER_ID",
                    colClasses = "character") %>% unique()
  scpt <- scpt[tmp, on = "INTERNAL_MEMBER_ID",
                       nomatch = 0]
  fname <- paste0("scpt", yr)
  assign(fname, scpt)
}
pt <- scpt2015[!INTERNAL_MEMBER_ID %in% scpt2013$INTERNAL_MEMBER_ID &
                 !INTERNAL_MEMBER_ID %in% scpt2014$INTERNAL_MEMBER_ID
               ][, -"FiscalYR"]
## total medical claim
scmccost <- pt
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
scmccost <- pt[scmccost[, -"birth_dt"], on = "INTERNAL_MEMBER_ID"]
names(scmccost)[3:7] <- paste0(2013:2017, "_total_medical_cost")
fwrite(scmccost,
       file = paste0("output/total_medical_cost.csv"))


## total pharmacy cost
scphcost <- pt[, -"birth_dt"]
for(fyear in 2017:2013){
  phcost <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                         "cost_measure_intermediate_data/", 
                         "cost_files_by_year/", 
                         "total_pharmacy_", fyear, "_all_ages_com.csv"),
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
scphcost <- pt[scphcost, on = "INTERNAL_MEMBER_ID"]
names(scphcost)[3:7] <- paste0(2013:2017, "_total_pharmacy_cost")

fwrite(scphcost,
       file = paste0("output/total_pharmacy_cost.csv"))

## ip cost
scipcost <- pt[, -"birth_dt"]
for(fyear in 2017:2013){
  ipcost <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                         "cost_measure_intermediate_data/", 
                         "value_sets_fyear/", 
                         "ip_raw_", fyear, "_all_ages_com.csv"),
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
names(scipcost)[3:7] <- paste0(2013:2017, "_ip_cost")
fwrite(scipcost,
       file = paste0("output/ip_cost.csv"))

## op cost
scopcost <- pt[, -"birth_dt"]
for(fyear in 2017:2013){
  opcost <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                         "cost_measure_intermediate_data/", 
                         "value_sets_fyear/", 
                         "op_raw_", fyear, "_all_ages_com.csv"),
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
names(scopcost)[3:7] <- paste0(2013:2017, "_op_cost")

fwrite(scopcost,
       file = paste0("output/op_cost.csv"))


## pc cost
scpccost <- pt[, -"birth_dt"]
for(fyear in 2017:2013){
  pccost <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                         "cost_measure_intermediate_data/", 
                         "value_sets_fyear/", 
                         "pc_raw_", fyear, "_all_ages_com.csv"),
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
names(scpccost)[3:7] <- paste0(2013:2017, "_pc_cost")
fwrite(scpccost,
       file = paste0("output/pc_cost.csv"))

## combine them
rt <- pt
for(rtname in c("total_medical", "total_pharmacy", "ip", "op", "pc")){
  rt_tmp <- fread(paste0("output/", rtname, "_cost.csv"), colClasses = "character")
  rt <- rt[rt_tmp[, -"birth_dt"], on = "INTERNAL_MEMBER_ID"]
}

fwrite(rt,
       file = paste0("output/all_cost.csv"))
