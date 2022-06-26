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



pt_m <- fread('output/sc_15_no_sc_13_14.csv', colClasses = 'character',
            select = c("INTERNAL_MEMBER_ID", "birth_dt", 
                       "GENDER_CODE")) %>% unique()
pt_no_m <- fread(paste0("output/no_sc_2013_2015_pt.csv"), colClasses = "character") %>% 
  unique(use.key = FALSE)
# medicare_ad_code <- c('HN', 'AB', 'MA', 'MD','12', '43', '14', '16', '15',
#                       'CP', 'MB', 'MP', '41', '13', '47', '42', 'ZZ',
#                       NULL)
# 
# elig_pt0 <- fread(paste0("E:/CT_APCD/shared/intermediate_data/",
#                          "APCD_modified/eligibility/By_Fiscal_Year/",
#                          "medical_elig.csv"),
#                   colClasses = "character") %>% unique(use.key = FALSE)
# elig_pt0[, `:=`(NumGapsLE45days = as.numeric(NumGapsLE45days),
#                 NumGapsGT45days = as.numeric(NumGapsGT45days),
#                 Elig_Sep30 = as.numeric(Elig_Sep30))]
# elig_pt3 <- data.table(read_sas("E:/CT_APCD/Beth/data4/eligibility_4.sas7bdat"))
# elig_pt3[, INTERNAL_MEMBER_ID := as.character(INTERNAL_MEMBER_ID)]
# elig_pt3 <- elig_pt3[ymd(eligibility_start_dt) < ymd("2017-10-01") & 
#                        ymd(eligibility_end_dt) > ymd("2012-09-30") & 
#                        COVERAGE_CLASS == "MEDICAL"]
# mcpt <- elig_pt3[SUBMITTER_ID == "15227" |
#                    PRODUCT_CODE %in% medicare_ad_code,
#                  .(INTERNAL_MEMBER_ID)] %>% unique()
# elig_pt <- elig_pt0[!INTERNAL_MEMBER_ID %in% mcpt$INTERNAL_MEMBER_ID]
# sum(!pt_all$INTERNAL_MEMBER_ID %in% unique(elig_pt$INTERNAL_MEMBER_ID))
# pt <- pt[INTERNAL_MEMBER_ID %in% unique(elig_pt$INTERNAL_MEMBER_ID)]
# pt <- unique(elig_pt3[, .(INTERNAL_MEMBER_ID, birth_dt,
#                               GENDER_CODE)])[pt, on = "INTERNAL_MEMBER_ID"]
pt_m[, age := 2015 - as.integer(birth_dt)]
pt_m <- pt_m[age < 65 & age > 9]
pt_no_m[, age := 2015 - as.integer(birth_dt)]
pt_no_m <- pt_no_m[age < 65 & age > 9]


pt_ph <- fread('output/sc_15_no_sc_13_14_ph.csv', colClasses = 'character',
            select = c("INTERNAL_MEMBER_ID", "birth_dt", 
                       "GENDER_CODE")) %>% unique()
pt_no_ph <- fread(paste0("output/no_sc_2013_2015_pt_ph.csv"), colClasses = "character") %>% 
  unique(use.key = FALSE)
pt_ph[, age := 2015 - as.integer(birth_dt)]
pt_ph <- pt_ph[age < 65 & age > 9]
pt_no_ph[, age := 2015 - as.integer(birth_dt)]
pt_no_ph <- pt_no_ph[age < 65 & age > 9]

age_group <- list(c(10:64), c(10:24), c(25:44), c(45:64))
gender_group <- list(c("M", "F"), "M", "F")


out_dir <- output_dir("output/annual_cost/")

for(i in c("ed_only","ip_only", "op_only", "ed_ip_only","ip_op_only", "ed_op_only",
           "ed_ip_op", "not_ed_ip_op", "pharmacy", "all", "ip", "op", "ed")){
  if(i == "pharmacy"){
    pt <- pt_ph
    pt_no <- pt_no_ph
  } else {
    pt <- pt_m
    pt_no <- pt_no_m
  }
  for(yr in 2013:2017) {
    fl1 <- fread(paste0("output/all_ed_ip_op_cost_", yr, ".csv"),
                 colClasses = "character")
    if(i == "all"){
      mccost1 <- fl1[, .(MEDICAL_CLAIM_HEADER_ID,
                         INTERNAL_MEMBER_ID,
                         ALLOWED_AMT)]
    } else if( i == "ip"){
      mccost1 <- fl1[type_of_header_id %in% c("ip_only",  "ed_ip_only","ip_op_only", 
                                              "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                             INTERNAL_MEMBER_ID,
                                                             ALLOWED_AMT)]
    } else if (i == "op"){
      mccost1 <- fl1[type_of_header_id %in% c("op_only","ip_op_only", "ed_op_only",
                                              "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                             INTERNAL_MEMBER_ID,
                                                             ALLOWED_AMT)]
    } else if (i == "ed"){
      mccost1 <- fl1[type_of_header_id %in% c("ed_only","ed_ip_only", "ed_op_only",
                                              "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                             INTERNAL_MEMBER_ID,
                                                             ALLOWED_AMT)]
    } else if (i == "pharmacy"){
      mccost1 <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                              "cost_measure_intermediate_data/", 
                              "cost_files_by_year/",
                              "total_pharmacy_", yr, "_all_ages.csv"), colClasses = "character",
                       select = c("INTERNAL_MEMBER_ID",
                                  "total", "pharmacy_claim_service_line_id"))[
                                    INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID, pt_no$INTERNAL_MEMBER_ID)]
      setnames(mccost1, "total", "ALLOWED_AMT")
      names(mccost1) <- toupper(names(mccost1))
      } else {
    mccost1 <- fl1[type_of_header_id == i, .(MEDICAL_CLAIM_HEADER_ID,
                                             INTERNAL_MEMBER_ID,
                                             ALLOWED_AMT)]
    }
    names(mccost1) <- toupper(names(mccost1))
    mc1.pt <- mccost1[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
    ][ALLOWED_AMT >= 0 & 
        INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
    mc1.no <- mccost1[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
    ][ALLOWED_AMT >= 0 & 
        INTERNAL_MEMBER_ID %in% pt_no$INTERNAL_MEMBER_ID]
    mc1.pt <- unique(mc1.pt, usekey = FALSE)
    mc1.no <- unique(mc1.no, usekey = FALSE)
    rm(mccost1, fl1)
    gc()
    mc1.pt.smy <- mc1.pt[, lapply(.SD, sum, na.rm = TRUE),
                         .SDcols = "ALLOWED_AMT",
                         by = "INTERNAL_MEMBER_ID"]
    mc1.pt.smy <- mc1.pt.smy[pt, on = "INTERNAL_MEMBER_ID"]
    mc1.no.smy <- mc1.no[, lapply(.SD, sum, na.rm = TRUE),
                         .SDcols = "ALLOWED_AMT",
                         by = "INTERNAL_MEMBER_ID"]
    mc1.no.smy <- mc1.no.smy[pt_no, on = "INTERNAL_MEMBER_ID"]
    mc1.pt.smy[is.na(mc1.pt.smy)] <- 0
    mc1.no.smy[is.na(mc1.no.smy)] <- 0
    out <- data.table()
    for (gender in gender_group){
      for(ages in age_group){
        tmp1 <- mc1.pt.smy[age %in% ages & GENDER_CODE %in% gender]
        tmp.out <- c(age = paste0(min(ages), "-", max(ages)),
                     gender = paste0(gender[1], "-", gender[length(gender)]),
                     summary(tmp1$ALLOWED_AMT)[4],  
                     sd = sd(tmp1$ALLOWED_AMT), 
                     summary(tmp1$ALLOWED_AMT)[1], 
                     summary(tmp1$ALLOWED_AMT)[6], 
                     summary(tmp1$ALLOWED_AMT)[2], 
                     summary(tmp1$ALLOWED_AMT)[3], 
                     summary(tmp1$ALLOWED_AMT)[5], 
                     quantile(tmp1$ALLOWED_AMT, 0.95), 
                     quantile(tmp1$ALLOWED_AMT, 0.99))
        out <- rbind(out, data.table(t(tmp.out)))
      }
    }
    outname_sc <- paste0("out_sc_", yr)
    assign(outname_sc, out)
    
    out <- data.table()
    for (gender in gender_group){
      for(ages in age_group){
        tmp1 <- mc1.no.smy[age %in% ages & GENDER_CODE %in% gender]
        tmp.out <- c(age = paste0(min(ages), "-", max(ages)),
                     gender = paste0(gender[1], "-", gender[length(gender)]),
                     summary(tmp1$ALLOWED_AMT)[4],  
                     sd1 = sd(tmp1$ALLOWED_AMT), 
                     summary(tmp1$ALLOWED_AMT)[1], 
                     summary(tmp1$ALLOWED_AMT)[6], 
                     summary(tmp1$ALLOWED_AMT)[2], 
                     summary(tmp1$ALLOWED_AMT)[3], 
                     summary(tmp1$ALLOWED_AMT)[5], 
                     quantile(tmp1$ALLOWED_AMT, 0.95), 
                     quantile(tmp1$ALLOWED_AMT, 0.99))
        out <- rbind(out, data.table(t(tmp.out)))
      }
    }
    outname_nosc <- paste0("out_nosc_", yr)
    assign(outname_nosc, out)
  }
  total_pt_cost <- cbind(out_sc_2013, out_sc_2014,
                         out_sc_2015, out_sc_2016,
                         out_sc_2017)
  total_no_pt_cost <-  cbind(out_nosc_2013, out_nosc_2014,
                             out_nosc_2015, out_nosc_2016,
                             out_nosc_2017)
  colorder <- rep(seq_len(ncol(out_sc_2013)), each = 5) +
    seq(0, 4*ncol(out_sc_2013), ncol(out_sc_2013))
  roworder <- c(1:5, 9, 6:8, 10:12)
  setcolorder(total_pt_cost, colorder)
  setorder(total_pt_cost[, .r := order(roworder)], .r)[, .r := NULL]
  setcolorder(total_no_pt_cost, colorder)
  setorder(total_no_pt_cost[, .r := order(roworder)], .r)[, .r := NULL]
  total_pt_cost <- insertRows(total_pt_cost, c(2, 6, 9, 13), NA)
  total_no_pt_cost <- insertRows(total_no_pt_cost, c(2, 6, 9, 13), NA)
  pt_header <- total_pt_cost[, c(1, 6)]
  total_pt_cost <- total_pt_cost[, -(1:10)]
  total_no_pt_cost <- total_no_pt_cost[, -(1:10)]
  colorder2 <- c(1:5, (1:5) + 45,
                 1:5 + 1*5, 1:5 + 45 + 1*5,
                 1:5 + 2*5, 1:5 + 45 + 2*5,
                 1:5 + 3*5, 1:5 + 45 + 3*5,
                 1:5 + 4*5, 1:5 + 45 + 4*5,
                 1:5 + 5*5, 1:5 + 45 + 5*5,
                 1:5 + 6*5, 1:5 + 45 + 6*5,
                 1:5 + 7*5, 1:5 + 45 + 7*5,
                 1:5 + 8*5, 1:5 + 45 + 8*5 )
  total_cost1 <- cbind(total_pt_cost, total_no_pt_cost)
  setcolorder(total_cost1, colorder2)
  total_cost <- cbind(pt_header, total_cost1)
  names(total_cost)[-(1:2)] <- paste0(c(rep("sc", 5), rep("nosc", 5)), "_", names(total_cost)[-(1:2)],
                                      "_", rep(2013:2017))
  fwrite(total_cost, file = paste0(out_dir, i, "_smy.csv"))
}
