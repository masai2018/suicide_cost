#  Create a data file with all medical claims for SC and NOSC patients. 
# Include a variable that indicates SC v. NOSC.
# Create a variable that indicates whether the claims are PC, IP, OP,


if(Sys.info()[4] %in% c("LAZ-ID1",
                        "LAZ-ID2",
                        "LAZ-DEID1",
                        "LAZ-DEID2")){
  setwd("E:/CT_APCD/Sai/suicide_cost")
}
rm(list=ls())
library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats",
               "knitr", "plyr", "haven")
need.packages(need_pkgs)
source("E:/CT_APCD/Sai/2019spring/sim_healthcare_cost_measures/R/functions.R")

col_mc <- c("MEDICAL_CLAIM_SERVICE_LINE_ID",
            "SUBMITTER_ID",
            "PRODUCT_CODE",
            "INTERNAL_MEMBER_ID",
            "first_service_dt",
            "last_service_dt",
            "ADMISSION_DT",
            "discharge_dt",
            "PLACE_OF_SERVICE_CODE",
            "TYPE_OF_BILL_CODE",
            "CLAIM_TYPE_ID",
            "TYPE_OF_SETTING_ID",
            "PLACE_OF_SETTING_ID",
            "QUANTITY",
            "CHARGE_AMT",
            "PAID_AMT",
            "CLAIM_STATUS_CODE",
            "PROCEDURE_CODE",
            "REVENUE_CODE",
            "ICD_VERSION_IND",
            "DIAGNOSIS_CODE",
            "ICD_PROCEDURE_CODE",
            "ORPHANED_ADJUSTMENT_FLAG",
            "DENIED_CLAIM_FLAG",
            "EMERGENCY_ROOM_FLAG",
            "MEDICAL_CLAIM_HEADER_ID",
            "RENDERING_PROVIDER_ID",
            "BILLING_PROVIDER_ID",
            "ATTENDING_PROVIDER_ID",
            "REFERRING_PROVIDER_ID")


scpt <- unique(fread('output/sc_15_no_sc_13_14.csv', colClasses = 'character',
              select = c("INTERNAL_MEMBER_ID")))[, sc_flag := 1]
noscpt <- fread('output/no_sc_2013_2015_pt.csv', colClasses = 'character')[, sc_flag := 0]
pt <- rbind(scpt, noscpt)

## medical claim

for(yr in 2014:2017){
  cat(paste0("begin ", yr, " at ", Sys.time(), "\n"))
  mc <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/", 
                     "cost_measure_intermediate_data/", "medical_fiscalyear_", 
                     yr, ".csv"), 
              colClasses = "character",
              select = col_mc)[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  mc[, `:=`(sc_pt_flag = 0,
            pc_flag = 0,
            ip_flag = 0,
            op_flag = 0)]
  mc[INTERNAL_MEMBER_ID %in% scpt$INTERNAL_MEMBER_ID, sc_pt_flag := 1]
  pc <- rbindlist(lapply(yr:(yr - 1), function(x){
    # rt <- data.table(read_sas(paste0("E:/CT_APCD/Beth/data4/Commercial/new_intermediate_files/med",
    #                       sub('.*(?=.{2}$)', '',
    #                           x, perl = T),
    #                       "_com_primary_care.sas7bdat")))
    rt <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/", 
                       "cost_measure_intermediate_data/value_sets/pc_",
                       yr, ".csv"), 
                colClasses = "character",
                select = c("INTERNAL_MEMBER_ID", "MEDICAL_CLAIM_SERVICE_LINE_ID",
                            "first_service_dt"))[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
    return(rt)
  }))
  pc <- pc[ymd(first_service_dt) >= ymd(paste0(yr - 1, '-10-01')) &
           ymd(first_service_dt) <= ymd(paste0(yr, '-09-30'))]
  ip <- get_medical_vs(vs.list = c("Acute Inpatient",
                                   "Inpatient Stay",
                                   "Nonacute Inpatient",
                                   "Nonacute Inpatient Stay"),
                       year.list = (yr - 1):yr,
                       code.sys = c("revenue_code", "procedure_code"),
                       select = NULL,
                       dir.raw = paste0("E:/CT_APCD/shared/intermediate_data/", 
                                        "APCD_modified/value_set/", 
                                        "medical_claim/"),
                       takeUnique = TRUE, 
                       fill = TRUE,
                       tolower = FALSE)
  ip <- ip[, c("internal_member_id",
               "medical_claim_service_line_id",
               "first_service_dt")]
  setnames(ip, 
           c("internal_member_id",
             "medical_claim_service_line_id"),
           c("INTERNAL_MEMBER_ID",
             "MEDICAL_CLAIM_SERVICE_LINE_ID"))
  ip <- ip[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  ip[, first_service_dt := ymd(mdy(first_service_dt))]
  ip <- ip[ymd(first_service_dt) >= ymd(paste0(yr - 1, '-10-01')) &
             ymd(first_service_dt) <= ymd(paste0(yr, '-09-30'))]
  sum(!ip$MEDICAL_CLAIM_SERVICE_LINE_ID %in% mc$MEDICAL_CLAIM_SERVICE_LINE_ID)
  op <- get_medical_vs(vs.list = c("Outpatient"),
                       year.list = (yr - 1):yr,
                       code.sys = c("revenue_code", "procedure_code"),
                       select = NULL,
                       dir.raw = paste0("E:/CT_APCD/shared/intermediate_data/", 
                                        "APCD_modified/value_set/", 
                                        "medical_claim/"),
                       takeUnique = TRUE, 
                       fill = TRUE,
                       tolower = FALSE)
  op <- op[, c("internal_member_id",
               "medical_claim_service_line_id",
               "first_service_dt")]
  setnames(op, 
           c("internal_member_id",
             "medical_claim_service_line_id"),
           c("INTERNAL_MEMBER_ID",
             "MEDICAL_CLAIM_SERVICE_LINE_ID"))
  op <- op[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  op[, first_service_dt := ymd(mdy(first_service_dt))]
  op <- op[ymd(first_service_dt) >= ymd(paste0(yr - 1, '-10-01')) &
             ymd(first_service_dt) <= ymd(paste0(yr, '-09-30'))]
  sum(!op$MEDICAL_CLAIM_SERVICE_LINE_ID %in% mc$MEDICAL_CLAIM_SERVICE_LINE_ID)
  names(mc)
  mc[INTERNAL_MEMBER_ID %in% pc$MEDICAL_CLAIM_SERVICE_LINE_ID,
     pc_flag := 1]
  mc[INTERNAL_MEMBER_ID %in% ip$MEDICAL_CLAIM_SERVICE_LINE_ID,
     ip_flag := 1]
  mc[INTERNAL_MEMBER_ID %in% op$MEDICAL_CLAIM_SERVICE_LINE_ID,
     op_flag := 1]
  fwrite(mc, 
         file = paste0("output/files_2021_summer/mc_sc_vs_nosc_", yr, ".csv"))
  rm(mc, pc, ip, op)
  gc()
  cat(paste0(yr, " done at ", Sys.time(), "\n"))
}

## dgx

for(yr in 2013:2016){
  cat(paste0("begin ", yr, " at ", Sys.time(), "\n"))
  mc <- fread(paste0("output/files_2021_summer/mc_sc_vs_nosc_", yr, ".csv"),
              colClasses = "character",
              select = c("MEDICAL_CLAIM_SERVICE_LINE_ID",
                         "sc_pt_flag",
                         "pc_flag",
                         "ip_flag",
                         "op_flag" ))
  dgx <- fread(paste0("output/dgx_", yr, ".csv"),
               colClasses = "character")
  dgx <- dgx[mc, on = "MEDICAL_CLAIM_SERVICE_LINE_ID", nomatch = 0]
  fwrite(dgx, 
         file = paste0("output/files_2021_summer/dgx_sc_vs_nosc_", yr, ".csv"))
  rm(mc, dgx)
  gc()
  cat(paste0(yr, " done at ", Sys.time(), "\n"))
}

## icd procedure
icd_p <- fread("E:/CT_APCD/Uconn_extract_20180521_12312017/MEDICAL_CLAIM_ICD_PROCEDURE.txt",
               colClasses = "character")
for(yr in 2013:2017){
  cat(paste0("begin ", yr, " at ", Sys.time(), "\n"))
  mc <- fread(paste0("output/files_2021_summer/mc_sc_vs_nosc_", yr, ".csv"),
              colClasses = "character",
              select = c("MEDICAL_CLAIM_SERVICE_LINE_ID",
                         "sc_pt_flag",
                         "pc_flag",
                         "ip_flag",
                         "op_flag" ))
  icd_p2 <- icd_p[mc, on = "MEDICAL_CLAIM_SERVICE_LINE_ID", nomatch = 0]
  fwrite(icd_p2, 
         file = paste0("output/files_2021_summer/icd_procedure_sc_vs_nosc_", yr, ".csv"))
  rm(mc, icd_p2)
  gc()
  cat(paste0(yr, " done at ", Sys.time(), "\n"))
}
rm(icd_p)
gc()

## medical header

for(yr in 2013:2017){
  cat(paste0("begin ", yr, " at ", Sys.time(), "\n"))
  hd <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/", 
                     "cost_measure_intermediate_data/", 
                     "medical_header_fiscalyear_", yr, ".csv"), 
              colClasses = "character")
  mc <- fread(paste0("output/files_2021_summer/mc_sc_vs_nosc_", yr, ".csv"),
              colClasses = "character",
              select = c("MEDICAL_CLAIM_SERVICE_LINE_ID",
                         "MEDICAL_CLAIM_HEADER_ID",
                         "sc_pt_flag",
                         "pc_flag",
                         "ip_flag",
                         "op_flag" ))
  hd <- hd[mc, on = "MEDICAL_CLAIM_HEADER_ID", nomatch = 0]
  fwrite(hd, 
         file = paste0("output/files_2021_summer/medical_claim_header_sc_vs_nosc_", yr, ".csv"))
  rm(mc, hd)
  gc()
  cat(paste0(yr, " done at ", Sys.time(), "\n"))
}

 ## pharmacy
for(yr in 2013:2017){
  cat(paste0("begin ", yr, " at ", Sys.time(), "\n"))
  phc <- rbindlist(lapply(yr:(yr - 1), function(x){
    rt <- fread(paste0("E:/CT_APCD/Uconn_extract_20180521_12312017/PHARMACY_",
                       x, ".txt"), 
                colClasses = "character",
                select = c("pharmacy_claim_service_line_id",
                           "INTERNAL_MEMBER_ID",
                           "SUBMITTER_ID",
                           "PRODUCT_CODE",
                           "prescription_filled_dt", "NATIONAL_DRUG_CODE",
                           "DRUG_NAME", "REFILL_NUMBER", "QUANTITY",
                           "DAYS_SUPPLY", "THIRTY_DAY_EQUIVALENT",
                           "CHARGE_AMT", "PAID_AMT", "COPAY_AMT",
                           "CLAIM_STATUS_CODE", "DENIED_CLAIM_FLAG",
                           "GENERIC_DRUG_IND_CODE"))[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
    return(rt)
  }))
  phc <- phc[ymd(mdy(prescription_filled_dt)) >= ymd(paste0(yr - 1, '-10-01')) &
             ymd(mdy(prescription_filled_dt)) <= ymd(paste0(yr, '-09-30'))]
  mc <- fread(paste0("output/files_2021_summer/mc_sc_vs_nosc_", yr, ".csv"),
              colClasses = "character",
              select = c("MEDICAL_CLAIM_SERVICE_LINE_ID",
                         "INTERNAL_MEMBER_ID",
                         "sc_pt_flag",
                         "pc_flag",
                         "ip_flag",
                         "op_flag" ))
  phc <- phc[mc, on = "INTERNAL_MEMBER_ID", nomatch = 0, allow.cartesian=TRUE]
  fwrite(phc, 
         file = paste0("output/files_2021_summer/pharmacy_sc_vs_nosc_", yr, ".csv"))
  rm(mc, phc)
  gc()
  cat(paste0(yr, " done at ", Sys.time(), "\n"))
}



