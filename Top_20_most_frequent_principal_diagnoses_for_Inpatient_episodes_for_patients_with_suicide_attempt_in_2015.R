if(Sys.info()[4] %in% c("LAZ-ID1",
                        "LAZ-ID2",
                        "LAZ-DEID1",
                        "LAZ-DEID2")){
  setwd("E:/CT_APCD/Sai/suicide_cost")
}
library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", "haven",
               "dplyr", "lubridate", "tidyverse", "matrixStats", "touth")
need.packages(need_pkgs)
source("get_suicide_attempts.R")
# source('E:/CT_APCD/Sai/2019spring/data-organization/R/functions.R')
source("E:/CT_APCD/Sai/2019spring/measure/R/myfunction.R")


scpt2015 <- fread(paste0("output/demographics_for_suicide_cost_2015.csv"), 
                  colClasses = "character")[sc_flag == 1, .(INTERNAL_MEMBER_ID)]
col.select <- tolower(c("INTERNAL_MEMBER_ID",
                        "first_service_dt",
                        "last_service_dt",
                        "ADMISSION_DT",
                        "discharge_dt",
                        "DIAGNOSIS_CODE",
                        "MEDICAL_CLAIM_HEADER_ID",
                        "icd_version_ind",
                        NULL))
vsnames <- c("Acute Inpatient",
             "Inpatient Stay",
             "Nonacute Inpatient",
             "Nonacute Inpatient Stay", 
             NULL)

ip_vs1 <- get_medical_vs(
  vs.list = vsnames,
  year.list = 2012:2014,
  # select = col.select,
  fill = TRUE
)
ip_vs1 <- ip_vs1[, col.select, with = FALSE]
ip_vs1 <- ip_vs1[internal_member_id %in% scpt2015$INTERNAL_MEMBER_ID]
ip_vs1 <- ip_vs1[mdy(first_service_dt) >= ymd(paste0(2012, "-10-01")) &
                   mdy(first_service_dt) <= ymd(paste0(2014, "-09-30"))]
ip_vs1[is.na(icd_version_ind), icd_version_ind := 9]
ip_vs1[, len_stay := mdy(discharge_dt) - mdy(admission_dt)]
ip_vs1[is.na(len_stay), len_stay := mdy(last_service_dt) - mdy(first_service_dt)]

fwrite(ip_vs1, 
       file = paste0("output/scpt2015_ip_2013_2014.csv"))
ip_vs1 <- fread(paste0("output/scpt2015_ip_2013_2014.csv"), colClasses = "character")
ip_vs1[, `:=`(len_stay = as.numeric(len_stay))]
ip_vs1[, len_stay_1 := len_stay + 1]
ip_vs1[icd_version_ind == 0,`:=`(icd_version_ind = 9,
                                 diagnosis_code = icd_map(diagnosis_code, 10, 9))]


ip_vs2 <- get_medical_vs(
  vs.list = vsnames,
  year.list = 2015:2017,
  # select = col.select
  fill = TRUE
)
ip_vs2 <- ip_vs2[, col.select, with = FALSE]
ip_vs2 <- ip_vs2[internal_member_id %in% scpt2015$INTERNAL_MEMBER_ID]
ip_vs2 <- ip_vs2[mdy(first_service_dt) >= ymd(paste0(2015, "-10-01")) &
                   mdy(first_service_dt) <= ymd(paste0(2017, "-09-30"))]
ip_vs2[, len_stay := mdy(discharge_dt) - mdy(admission_dt)]
ip_vs2[is.na(len_stay), len_stay := mdy(last_service_dt) - mdy(first_service_dt)]
ip_vs2[is.na(icd_version_ind), icd_version_ind := 0]

fwrite(ip_vs2, 
       file = paste0("output/scpt2015_ip_2016_2017.csv"))
ip_vs2 <- fread(paste0("output/scpt2015_ip_2016_2017.csv"), colClasses = "character")
ip_vs2[, `:=`(len_stay = as.numeric(len_stay))]
ip_vs2[, len_stay_1 := len_stay + 1]
ip_vs2[icd_version_ind == 0,`:=`(icd_version_ind = 9,
                                 diagnosis_code1 = icd_map(diagnosis_code, 10, 9))]
ip_vs2[is.na(diagnosis_code1) | diagnosis_code1 == "", diagnosis_code1 := diagnosis_code]
ip_vs2[, diagnosis_code := diagnosis_code1]

mhf1 <- rbindlist(lapply(2012:2018, function(year){
  rt <- fread(paste0("E:/CT_APCD/Uconn_extract_20180521_12312017/medical_header_", 
                     year, ".txt"),
              colClasses = "character",
              select = c("ALLOWED_AMT",
                         # "INTERNAL_MEMBER_ID",
                         "first_service_dt",
                         "MEDICAL_CLAIM_HEADER_ID",
                         "ORPHANED_HEADER_FLAG",
                         "DENIED_HEADER_FLAG",
                         NULL))
  names(rt) <- tolower(names(rt))
  rt <- rt[mdy(first_service_dt) >= ymd(paste0(2012, "-10-01")) &
             mdy(first_service_dt) <= ymd(paste0(2014, "-09-30"))]
  return(rt)
}))
sum(!ip_vs1$medical_claim_header_id %in% mhf1$medical_claim_header_id)
mhf1 <- mhf1[medical_claim_header_id %in% ip_vs1$medical_claim_header_id]
mhf1 <- unique(mhf1, use.key = FALSE)
mhf1 <- mhf1[, -"first_service_dt"]
names(mhf1)
ip_vs1_1 <- mhf1[ip_vs1, on = "medical_claim_header_id"]
ip_vs1_1 <- unique(ip_vs1_1, use.key = FALSE)
# ip_vs1_1[icd_version_ind == 0, diagnosis_code := icd_map(diagnosis_code, 10, 9)]
ip_vs1_1[, diagnosis_code := insert_dot(diagnosis_code, 9)]
ip_vs1_1[, diagnosis_code := gsub("\\..*", "", diagnosis_code)]
ip_vs1_1 <- unique(ip_vs1_1, use.key = FALSE)
ip_vs1_1[, allowed_amt := as.numeric(allowed_amt)]
ip_vs1_1[allowed_amt < 0, allowed_amt := 0]
ip_vs1_1[is.na(allowed_amt), allowed_amt := 0]
smy1.1 <- ip_vs1_1[, .N, .(diagnosis_code)][order(-N)][1:20, ] %>% 
  mutate(percent = N/sum(N)) %>% data.table()
smy1.2 <- ip_vs1_1[diagnosis_code %in% 
                     smy1.1$diagnosis_code][, lapply(.SD, sum, na.rm = TRUE),
                                                .SDcols = "allowed_amt",
                                                by = "diagnosis_code"]
smy1.3 <- ip_vs1_1[diagnosis_code %in% 
                     smy1.1$diagnosis_code][, lapply(.SD, sum, na.rm = TRUE),
                                            .SDcols = "len_stay",
                                            by = "diagnosis_code"]
smy1.4 <- smy1.2[smy1.3, on = "diagnosis_code"]
smy1.5 <- smy1.4[, `Average cost per episode2` := allowed_amt/len_stay][, c("Average cost per episode2",
                                                                            "diagnosis_code")]
smy1.6 <- ip_vs1_1[diagnosis_code %in% 
                     smy1.1$diagnosis_code][, lapply(.SD, mean, na.rm = TRUE),
                                                .SDcols = "len_stay",
                                                by = "diagnosis_code"]
smy1 <- smy1.5[smy1.6, on = "diagnosis_code"]
smy1 <- smy1[smy1.1, on = "diagnosis_code"][order(-N)]
fwrite(smy1, file = paste0("output/top20_13_14.csv"))

mhf2 <- rbindlist(lapply(2012:2018, function(year){
  rt <- fread(paste0("E:/CT_APCD/Uconn_extract_20180521_12312017/medical_header_", 
                     year, ".txt"),
              colClasses = "character",
              select = c("ALLOWED_AMT",
                         # "INTERNAL_MEMBER_ID",
                         "first_service_dt",
                         "MEDICAL_CLAIM_HEADER_ID",
                         "ORPHANED_HEADER_FLAG",
                         "DENIED_HEADER_FLAG",
                         NULL))
  names(rt) <- tolower(names(rt))
  rt <- rt[mdy(first_service_dt) >= ymd(paste0(2015, "-10-01")) &
             mdy(first_service_dt) <= ymd(paste0(2017, "-09-30"))]
  return(rt)
}))
sum(!ip_vs2$medical_claim_header_id %in% mhf2$medical_claim_header_id)
mhf2 <- mhf2[medical_claim_header_id %in% ip_vs2$medical_claim_header_id]
mhf2 <- unique(mhf2, use.key = FALSE)
mhf2 <- mhf2[, -"first_service_dt"]
names(mhf2)
ip_vs2_1 <- mhf2[ip_vs2, on = "medical_claim_header_id"]
ip_vs2_1 <- unique(ip_vs2_1, use.key = FALSE)
# ip_vs2_1[icd_version_ind == 9, diagnosis_code := icd_map(diagnosis_code, 9, 10)]
ip_vs2_1[, diagnosis_code := insert_dot(diagnosis_code, 9)]
ip_vs2_1[, diagnosis_code := gsub("\\..*", "", diagnosis_code)]
ip_vs2_1 <- unique(ip_vs2_1, use.key = FALSE)
ip_vs2_1[, allowed_amt := as.numeric(allowed_amt)]
ip_vs2_1[, len_stay := as.numeric(len_stay)]
ip_vs2_1[allowed_amt < 0, allowed_amt := 0]
ip_vs2_1[is.na(allowed_amt), allowed_amt := 0]
smy2.1 <- ip_vs2_1[, .N, .(diagnosis_code)][order(-N)][1:20, ] %>% 
  mutate(percent = N/sum(N)) %>% data.table()
smy2.2 <- ip_vs2_1[diagnosis_code %in% 
                     smy2.1$diagnosis_code][, lapply(.SD, sum, na.rm = TRUE),
                                            .SDcols = "allowed_amt",
                                            by = "diagnosis_code"]
smy2.3 <- ip_vs2_1[diagnosis_code %in% 
                     smy2.1$diagnosis_code][, lapply(.SD, sum, na.rm = TRUE),
                                            .SDcols = "len_stay",
                                            by = "diagnosis_code"]
smy2.4 <- smy2.2[smy2.3, on = "diagnosis_code"]
smy2.5 <- smy2.4[, `Average cost per episode2` := allowed_amt/len_stay][, c("Average cost per episode2",
                                                                            "diagnosis_code")]
smy2.6 <- ip_vs2_1[diagnosis_code %in% 
                     smy2.1$diagnosis_code][, lapply(.SD, mean, na.rm = TRUE),
                                            .SDcols = "len_stay",
                                            by = "diagnosis_code"]
smy2 <- smy2.5[smy2.6, on = "diagnosis_code"]
smy2 <- smy2[smy2.1, on = "diagnosis_code"][order(-N)]
fwrite(smy2, file = paste0("output/top20_16_17.csv"))
