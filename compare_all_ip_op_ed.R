rm(list = ls())
gc()
if(Sys.info()[4] %in% c("LAZ-ID1",
                        "LAZ-ID2",
                        "LAZ-DEID1",
                        "LAZ-DEID2")){
  setwd("E:/CT_APCD/Sai/suicide_cost")
}
library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", "haven",
               "dplyr", "lubridate", "tidyverse", "matrixStats", "berryFunctions")
need.packages(need_pkgs)
source("get_suicide_attempts.R")
source("E:/CT_APCD/Sai/2019spring/sim_healthcare_cost_measures/R/functions.R")



pt <- fread('output/sc_15_no_sc_13_14.csv', colClasses = 'character',
            select = c("INTERNAL_MEMBER_ID", "birth_dt", 
                       "GENDER_CODE")) %>% unique()
pt_no <- fread(paste0("output/no_sc_2013_2015_pt.csv"), colClasses = "character") %>% 
  unique(use.key = FALSE)

pt_all <- rbind(pt, pt_no)

# yr <- 2015
for (yr in 2013:2017){
  mhf <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                      "cost_measure_intermediate_data/value_sets_fyear/",
                      "header_and_amt_", yr, ".csv"),
               colClasses = 'character', encoding = 'UTF-8',
               select = c("ALLOWED_AMT",
                          "INTERNAL_MEMBER_ID",
                          "first_service_dt",
                          "MEDICAL_CLAIM_HEADER_ID",
                          "ORPHANED_HEADER_FLAG",
                          "DENIED_HEADER_FLAG",
                          NULL))[INTERNAL_MEMBER_ID %in% pt_all$INTERNAL_MEMBER_ID &
                                             ORPHANED_HEADER_FLAG == "N" & 
                                             DENIED_HEADER_FLAG == "N"][, -c("INTERNAL_MEMBER_ID",
                                                                             "ORPHANED_HEADER_FLAG",
                                                                             "DENIED_HEADER_FLAG")] %>%
    unique(by = c("ALLOWED_AMT", "MEDICAL_CLAIM_HEADER_ID"))
  setnames(mhf, "first_service_dt", "header_id_dt")
  mc <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/cost_measure_intermediate_data/medical_fiscalyear_", 
                     yr, ".csv"), select = c("INTERNAL_MEMBER_ID",
                                             "MEDICAL_CLAIM_HEADER_ID",
                                             "first_service_dt",
                                             "MEDICAL_CLAIM_SERVICE_LINE_ID",
                                             "ORPHANED_ADJUSTMENT_FLAG",
                                             "DENIED_CLAIM_FLAG"),
              colClasses = "character")[INTERNAL_MEMBER_ID %in% pt_all$INTERNAL_MEMBER_ID & 
                                          ORPHANED_ADJUSTMENT_FLAG == "N" &
                                          DENIED_CLAIM_FLAG == "N"][, -c("DENIED_CLAIM_FLAG",
                                                                         "ORPHANED_ADJUSTMENT_FLAG")]
  rt <- mhf[mc, on = "MEDICAL_CLAIM_HEADER_ID", nomatch = 0]
  rt[, sc_flag := 0]
  rt[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID, sc_flag := 1]
  setcolorder(rt, c("MEDICAL_CLAIM_HEADER_ID", "MEDICAL_CLAIM_SERVICE_LINE_ID", "INTERNAL_MEMBER_ID",
                    "ALLOWED_AMT", "first_service_dt", "header_id_dt",
                    # "ORPHANED_HEADER_FLAG", "DENIED_HEADER_FLAG",
                    # "ORPHANED_ADJUSTMENT_FLAG", "DENIED_CLAIM_FLAG",
                    "sc_flag"))
  ed <- get_medical_vs(vs.list = c("ED", "ED Procedure Code"),
                       year.list = (yr - 1):yr,
                       code.sys = c("revenue_code", "procedure_code"),
                       select = c("MEDICAL_CLAIM_SERVICE_LINE_ID", "INTERNAL_MEMBER_ID"),
                       dir.raw = "E:/CT_APCD/Sai/intermediate_data/cost_measure_intermediate_data/ED_Code/",
                       takeUnique = TRUE, 
                       fill = TRUE,
                       tolower = FALSE)[internal_member_id %in% pt_all$INTERNAL_MEMBER_ID &
                                          medical_claim_service_line_id %in% mc$MEDICAL_CLAIM_SERVICE_LINE_ID][, -"internal_member_id"]
  names(ed) <- toupper(names(ed))
  ip <- get_medical_vs(vs.list = c("Acute Inpatient",
                                   "Inpatient Stay",
                                   "Nonacute Inpatient",
                                   "Nonacute Inpatient Stay"),
                       year.list = (yr - 1):yr,
                       code.sys = c("revenue_code", "procedure_code"),
                       select = c("MEDICAL_CLAIM_SERVICE_LINE_ID", "INTERNAL_MEMBER_ID"),
                       dir.raw = "E:/CT_APCD/shared/intermediate_data/APCD_modified/value_set/medical_claim/",
                       takeUnique = TRUE, 
                       fill = TRUE,
                       tolower = FALSE)[internal_member_id %in% pt_all$INTERNAL_MEMBER_ID &
                                          medical_claim_service_line_id %in% mc$MEDICAL_CLAIM_SERVICE_LINE_ID][, -"internal_member_id"]
  names(ip) <- toupper(names(ip))
  op <- get_medical_vs(vs.list = c("Outpatient"),
                       year.list = (yr - 1):yr,
                       code.sys = c("revenue_code", "procedure_code"),
                       select = c("MEDICAL_CLAIM_SERVICE_LINE_ID", "INTERNAL_MEMBER_ID"),
                       dir.raw = "E:/CT_APCD/shared/intermediate_data/APCD_modified/value_set/medical_claim/",
                       takeUnique = TRUE, 
                       fill = TRUE,
                       tolower = FALSE)[internal_member_id %in% pt_all$INTERNAL_MEMBER_ID &
                                          medical_claim_service_line_id %in% mc$MEDICAL_CLAIM_SERVICE_LINE_ID][, -"internal_member_id"]
  names(op) <- toupper(names(op))
  rt[, `:=`(ed_flag = 0, ip_flag = 0, op_flag = 0)]
  
  rt[,  type_of_service_line_id := "not_ed_ip_op"]
  rt[MEDICAL_CLAIM_SERVICE_LINE_ID %in% ed$MEDICAL_CLAIM_SERVICE_LINE_ID, `:=`(ed_flag = 1, type_of_service_line_id = "ed")]
  rt[MEDICAL_CLAIM_SERVICE_LINE_ID %in% ip$MEDICAL_CLAIM_SERVICE_LINE_ID, `:=`(ip_flag = 1, type_of_service_line_id = "ip")]
  rt[MEDICAL_CLAIM_SERVICE_LINE_ID %in% op$MEDICAL_CLAIM_SERVICE_LINE_ID, `:=`(op_flag = 1, type_of_service_line_id = "op")]
  
  rt2 <- rt[, lapply(.SD, sum), .SDcols = c("ed_flag", "ip_flag", "op_flag"), by = "MEDICAL_CLAIM_HEADER_ID"]
  
  
  
  
  rt2[ed_flag >= 1 & ip_flag == 0 & op_flag == 0, type_of_header_id := "ed_only"]
  rt2[ed_flag == 0 & ip_flag >= 1 & op_flag == 0, type_of_header_id := "ip_only"]
  rt2[ed_flag == 0 & ip_flag == 0 & op_flag >= 1, type_of_header_id := "op_only"]
  rt2[ed_flag >= 1 & ip_flag >= 1 & op_flag == 0, type_of_header_id := "ed_ip_only"]
  rt2[ed_flag >= 1 & ip_flag == 0 & op_flag >= 1, type_of_header_id := "ed_op_only"]
  rt2[ed_flag == 0 & ip_flag >= 1 & op_flag >= 1, type_of_header_id := "ip_op_only"]
  rt2[ed_flag >= 1 & ip_flag >= 1 & op_flag >= 1, type_of_header_id := "ed_ip_op"]
  rt2[ed_flag == 0 & ip_flag == 0 & op_flag == 0, type_of_header_id := "not_ed_ip_op"]
  
  rt3 <- rt[, -c("ed_flag", "ip_flag", "op_flag")][rt2[, .(MEDICAL_CLAIM_HEADER_ID, type_of_header_id)], on = "MEDICAL_CLAIM_HEADER_ID"]
  # rt3[, `:=`(ed_only_cost = 0, ip_only_cost = 0, op_only_cost = 0,
  #            ed_ip_only_cost = 0, ed_op_only_cost = 0, ip_op_only_cost = 0,
  #            ed_ip_op_cost = 0, not_ed_ip_op_cost = 0)]
  rt3[type_of_header_id == "ed_only", ed_only_cost := ALLOWED_AMT]
  rt3[type_of_header_id == "ip_only", ip_only_cost := ALLOWED_AMT]
  rt3[type_of_header_id == "op_only", op_only_cost := ALLOWED_AMT]
  rt3[type_of_header_id == "ed_ip_only", ed_ip_only_cost := ALLOWED_AMT]
  rt3[type_of_header_id == "ed_op_only", ed_op_only_cost := ALLOWED_AMT]
  rt3[type_of_header_id == "ip_op_only", ip_op_only_cost := ALLOWED_AMT]
  rt3[type_of_header_id == "ed_ip_op", ed_ip_op_cost := ALLOWED_AMT]
  rt3[type_of_header_id == "not_ed_ip_op", not_ed_ip_op_cost := ALLOWED_AMT]
  fwrite(rt3, file = paste0("output/all_ed_ip_op_cost_", yr, ".csv"))
  rm(ed, ip, op, mc, rt, rt2, rt3)
  gc()
}
