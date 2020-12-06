if(Sys.info()[4] %in% c("LAZ-ID1",
                        "LAZ-ID2",
                        "LAZ-DEID1",
                        "LAZ-DEID2")){
  setwd("E:/CT_APCD/Sai/suicide_cost")
}
library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats")
need.packages(need_pkgs)
source("get_suicide_attempts.R")

pt_all <- fread("output/no_of_pt.csv", colClasses = "character")
setnames(pt_all, "internal_member_id", "INTERNAL_MEMBER_ID")
elig_pt <- fread(paste0("E:/CT_APCD/shared/intermediate_data/", 
                        "APCD_modified/eligibility/By_Fiscal_Year/", 
                        "medical_elig_all.csv"),
                 colClasses = "character",
                 select = c("INTERNAL_MEMBER_ID",
                            "birth_dt")) %>% unique(use.key = FALSE)
pt_all <- elig_pt[pt_all, on = "INTERNAL_MEMBER_ID"]

for(i in names(pt_all)[4:7]){
  ## total medical claim
  pt <- pt_all[get(i) == 1, 1:4]
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
  scmccost <- pt[scmccost[, -c("birth_dt",
                               "at_least_one_medical_claim",
                               "continuous_elig" )], on = "INTERNAL_MEMBER_ID"]
  names(scmccost)[5:9] <- paste0(2013:2017, "_total_medical_cost")
  fwrite(scmccost,
         file = paste0("output/new/total_medical_cost", "_i.csv"))
  
  
  ## total pharmacy cost
  scphcost <- pt[, -c("birth_dt", "at_least_one_medical_claim",
                      "continuous_elig" )]
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
  names(scphcost)[5:9] <- paste0(2013:2017, "_total_pharmacy_cost")
  
  fwrite(scphcost,
         file = paste0("output/new/total_pharmacy_cost", "_i.csv"))
  
  ## ip cost
  scipcost <- pt[, -c("birth_dt", "at_least_one_medical_claim",
                      "continuous_elig" )]
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
  names(scipcost)[5:9] <- paste0(2013:2017, "_ip_cost")
  fwrite(scipcost,
         file = paste0("output/new/ip_cost", "_i.csv"))
  
  ## op cost
  scopcost <- pt[, -c("birth_dt", "at_least_one_medical_claim",
                      "continuous_elig" )]
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
  names(scopcost)[5:9] <- paste0(2013:2017, "_op_cost")
  
  fwrite(scopcost,
         file = paste0("output/new/op_cost", "_i.csv"))
  
  
  ## pc cost
  scpccost <- pt[, -c("birth_dt", "at_least_one_medical_claim",
                      "continuous_elig" )]
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
  names(scpccost)[5:9] <- paste0(2013:2017, "_pc_cost")
  fwrite(scpccost,
         file = paste0("output/new/pc_cost", "_i.csv"))
  
  ## combine them
  rt <- pt
  for(rtname in c("total_medical", "total_pharmacy", "ip", "op", "pc")){
    rt_tmp <- fread(paste0("output/new/", rtname, "_cost", "_i.csv"), colClasses = "character")
    rt <- rt[rt_tmp[, -c("birth_dt", "at_least_one_medical_claim",
                         "continuous_elig" )], on = "INTERNAL_MEMBER_ID"]
  }
  
  fwrite(rt,
         file = paste0("output/new/all_cost", "_i.csv"))
  
}
