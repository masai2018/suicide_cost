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
need_pkgs <- c("data.table", "bit64", "tools", "touch", "haven", "BSDA","broom", "purrr",
               "dplyr", "lubridate", "tidyverse", "matrixStats", "berryFunctions")
need.packages(need_pkgs)
source("get_suicide_attempts.R")



pt <- fread('output/sc_15_no_sc_13_14.csv', colClasses = 'character',
            select = c("INTERNAL_MEMBER_ID", "birth_dt", 
                       "GENDER_CODE")) %>% unique()
pt[, age := 2015 - as.integer(birth_dt)]
pt <- pt[age < 65 & age > 9]
age_group <- list(c(10:64), c(10:24), c(25:44), c(45:64))
gender_group <- list(c("M", "F"), "M", "F")

sc <- rbindlist(lapply(9:10, function(x){
  fread(paste0("output/dgx_mc_2015_sc_icd", x, ".csv"), 
        colClasses = "character", 
        select = c("MEDICAL_CLAIM_SERVICE_LINE_ID",
                   "INTERNAL_MEMBER_ID",
                   "sc_flag"))[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID &
                                 sc_flag  == "1"]
}))
sc <- unique(sc, use.key = FALSE)

sc2 <- fread('output/all_claims_SA_non_SA_20220131.csv',  colClasses = 'character',
             select = c("MEDICAL_CLAIM_SERVICE_LINE_ID", 
                        "MEDICAL_CLAIM_HEADER_ID", "first_service_dt",
                        "sc_15_no_sc_13_14"))[
                          sc_15_no_sc_13_14 == "1" &
                            mdy(first_service_dt) > "2014-09-30" &
                            mdy(first_service_dt) < "2015-12-01"
                        ][, -c("sc_15_no_sc_13_14")]
sc2 <- unique(sc2, use.key = FALSE)
sc <- sc2[, - "MEDICAL_CLAIM_HEADER_ID"][sc, on = "MEDICAL_CLAIM_SERVICE_LINE_ID"]
sc[, first_service_dt := mdy(first_service_dt )]
sc_min <- sc[, lapply(.SD, min),
             .SDcols = "first_service_dt", by = "INTERNAL_MEMBER_ID"]
names(sc_min)[2] <- "min"
sc_max <- sc[, lapply(.SD, max),
             .SDcols = "first_service_dt", by = "INTERNAL_MEMBER_ID"]
names(sc_max)[2] <- "max"
sc_int <- sc_min[sc_max, on = "INTERNAL_MEMBER_ID"]
sc_int[, days := as.numeric(ymd(max) -ymd(min))]

sc_int_smy <- c(summary(sc_int$days), quantile(sc_int$days, 0.95),
                quantile(sc_int$days, 0.99), mean = mean(sc_int$days),
                sd = sd(sc_int$days))
# fwrite(data.table(t(sc_int_smy)),
#        file = "output/sc_days_smy.csv")
sc_min[, `:=`(day30 = min + 31,
              day60 = min + 61)] 

fl <-  rbind(fread(paste0("output/all_ed_ip_op_cost_", 2015, ".csv"),
                   colClasses = "character"),
             fread(paste0("output/all_ed_ip_op_cost_", 2016, ".csv"),
                   colClasses = "character"))[mdy(first_service_dt) > "2014-09-30" &
                                                mdy(first_service_dt) < "2015-12-01"]




for(i in c("all","ip", "op", "ed", "ed_only","ip_only", "op_only", "ed_ip_only","ip_op_only", "ed_op_only",
           "ed_ip_op", "not_ed_ip_op", "pharmacy")){
  if(i == "all"){
    mccost <- fl[, .(MEDICAL_CLAIM_HEADER_ID,
                     INTERNAL_MEMBER_ID,
                     ALLOWED_AMT,
                     first_service_dt)]
  } else if( i == "ip"){
    mccost <- fl[type_of_header_id %in% c("ip_only",  "ed_ip_only","ip_op_only", 
                                          "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                         INTERNAL_MEMBER_ID,
                                                         ALLOWED_AMT,
                                                         first_service_dt)]
  } else if (i == "op"){
    mccost <- fl[type_of_header_id %in% c("op_only","ip_op_only", "ed_op_only",
                                          "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                         INTERNAL_MEMBER_ID,
                                                         ALLOWED_AMT,
                                                         first_service_dt)]
  } else if (i == "ed"){
    mccost <- fl[type_of_header_id %in% c("ed_only","ed_ip_only", "ed_op_only",
                                          "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                         INTERNAL_MEMBER_ID,
                                                         ALLOWED_AMT,
                                                         first_service_dt)]
  } else if (i == "pharmacy") {
    mccost <- rbind(fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                                 "cost_measure_intermediate_data/", 
                                 "cost_files_by_year/",
                                 "total_pharmacy_", 2015, "_all_ages.csv"), colClasses = "character",
                          select = c("INTERNAL_MEMBER_ID", "prescription_filled_dt",
                                     "total", "pharmacy_claim_service_line_id"))[
                                       INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID)],
                    fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                                 "cost_measure_intermediate_data/", 
                                 "cost_files_by_year/",
                                 "total_pharmacy_", 2016, "_all_ages.csv"), colClasses = "character",
                          select = c("INTERNAL_MEMBER_ID", "prescription_filled_dt",
                                     "total", "pharmacy_claim_service_line_id"))[INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID)])
    setnames(mccost, c("total",  "prescription_filled_dt"), c("ALLOWED_AMT", "first_service_dt"))
    mccost <- mccost[mdy(first_service_dt) > "2014-09-30" &
                       mdy(first_service_dt) < "2015-12-01"]
  } else {
    mccost <- fl[type_of_header_id == i, .(MEDICAL_CLAIM_HEADER_ID,
                                           INTERNAL_MEMBER_ID,
                                           ALLOWED_AMT,
                                           first_service_dt)]
  }
  
  names(mccost) <- toupper(names(mccost))
  mccost[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)]
  mccost <- mccost[ALLOWED_AMT >= 0 & INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  mccost <- unique(mccost, use.key = FALSE)
  mccost2 <- sc_min[mccost, on = "INTERNAL_MEMBER_ID"]
  mccost2[, FIRST_SERVICE_DT := mdy(FIRST_SERVICE_DT)]
  
  if(i != "pharmacy"){
    mccost_30 <- mccost2[FIRST_SERVICE_DT >= min & FIRST_SERVICE_DT < day30, 
                         .(INTERNAL_MEMBER_ID, MEDICAL_CLAIM_HEADER_ID, ALLOWED_AMT)]
  } else {
    mccost_30 <- mccost2[FIRST_SERVICE_DT >= min & FIRST_SERVICE_DT < day30]
  }
  mccost_30 <- unique(mccost_30, use.key = FALSE)
  mccost_30_smy <- mccost_30[, lapply(.SD, sum), .SDcols = "ALLOWED_AMT",
                             by = "INTERNAL_MEMBER_ID"]
  names(mccost_30_smy)[2] <- "post_30"
  if(i != "pharmacy"){
    mccost_60 <- mccost2[FIRST_SERVICE_DT >= min & FIRST_SERVICE_DT < day60, 
                         .(INTERNAL_MEMBER_ID, MEDICAL_CLAIM_HEADER_ID, ALLOWED_AMT)]
  } else {
    mccost_60 <- mccost2[FIRST_SERVICE_DT >= min & FIRST_SERVICE_DT < day60]
  }
  mccost_60 <- unique(mccost_60, use.key = FALSE)
  mccost_60_smy <- mccost_60[, lapply(.SD, sum), .SDcols = "ALLOWED_AMT",
                             by = "INTERNAL_MEMBER_ID"]
  names(mccost_60_smy)[2] <- "post_60"
  smy <- mccost_30_smy[pt[, .(INTERNAL_MEMBER_ID)], on = "INTERNAL_MEMBER_ID"]
  smy <- mccost_60_smy[smy, on = "INTERNAL_MEMBER_ID"]
  smy[is.na(smy)] <- 0
  smy <- smy[pt, on = "INTERNAL_MEMBER_ID"]
  for(ages in age_group){
    tmp <- smy[age %in% ages]
    cat(paste0("post 30, cost type: ", i, ", age range: ", ages[1], "-", ages[length(ages)],
               ", compare mean cost of male and female:", "\n"))
    print(summary(aov(post_30 ~ GENDER_CODE, data = tmp)))
    tmp <- smy[age %in% ages]
    cat(paste0("post 60, cost type: ", i, ", age range: ", ages[1], "-", ages[length(ages)],
               ", compare mean cost of male and female:", "\n"))
    print(summary(aov(post_60 ~ GENDER_CODE, data = tmp)))
  }
  for(gender in gender_group){
    gender1 <- paste(gender, collapse = "-")
    tmp <- smy[GENDER_CODE %in% gender]
    tmp[, age := as.integer(age)]
    tmp[age >= 10 & age <= 24, age_group := "10-24"]
    tmp[age >= 25 & age <= 44, age_group := "25-44"]
    tmp[age >= 45 & age <= 64, age_group := "45-64"]
    cat(paste0("post 30, cost type: ", i, ", gender: ", gender1,
               ", compare mean cost of age groups:", "\n"))
    print(summary(aov(post_30 ~ age_group, data = tmp)))
    cat(paste0("post 60, cost type: ", i, ", gender: ", gender1,
               ", compare mean cost of age groups:", "\n"))
    print(summary(aov(post_60 ~ age_group, data = tmp)))
  }
}
