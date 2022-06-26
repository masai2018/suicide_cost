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



pt <- fread('output/sc_15_no_sc_13_14.csv', colClasses = 'character',
            select = c("INTERNAL_MEMBER_ID", "birth_dt", 
                       "GENDER_CODE")) %>% unique()
pt_no <- fread(paste0("output/no_sc_2013_2015_pt.csv"), colClasses = "character") %>% 
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
pt[, age := 2015 - as.integer(birth_dt)]
pt <- pt[age < 65 & age > 9]
pt_no[, age := 2015 - as.integer(birth_dt)]
pt_no <- pt_no[age < 65 & age > 9]
age_group <- list(c(10:64), c(10:24), c(25:44), c(45:64))
gender_group <- list(c("M", "F"), "M", "F")
dem_smy_sc <- data.table()
for (gender in gender_group){
  for(ages in age_group){
    tmp <- pt[age %in% ages & GENDER_CODE %in% gender]
    dem_smy_tmp <- tmp[, .N]
    dem_smy_tmp <- cbind(ages = paste0(ages[1], "-", ages[length(ages)]), 
                         gender = paste0(gender[1], "-", gender[length(gender)]),
                         N = dem_smy_tmp, `% of total` = dem_smy_tmp/dim(pt)[1])
    dem_smy_sc <- rbind(dem_smy_sc, dem_smy_tmp)
    rm(dem_smy_tmp, tmp)
  }
}
dem_smy_sc <- rbind(dem_smy_sc[1:5, ], dem_smy_sc[9, ], 
                    dem_smy_sc[6:8, ], dem_smy_sc[10:12, ])
dem_smy_sc <- insertRows(dem_smy_sc, c(2, 6, 9, 13), NA)

dem_smy_no_sc <- data.table()
for (gender in gender_group){
  for(ages in age_group){
    tmp <- pt_no[age %in% ages & GENDER_CODE %in% gender]
    dem_smy_tmp <- tmp[, .N]
    dem_smy_tmp <- cbind(N = dem_smy_tmp, `% of total` = dem_smy_tmp/dim(pt_no)[1])
    dem_smy_no_sc <- rbind(dem_smy_no_sc, dem_smy_tmp)
    rm(dem_smy_tmp, tmp)
  }
}

dem_smy_no_sc <- rbind(dem_smy_no_sc[1:5, ], dem_smy_no_sc[9, ], 
                       dem_smy_no_sc[6:8, ], dem_smy_no_sc[10:12, ])
dem_smy_no_sc <- insertRows(dem_smy_no_sc, c(2, 6, 9, 13), NA)
dem_smy <- cbind(dem_smy_sc, dem_smy_no_sc)


fl1 <-  rbind(fread(paste0("output/all_ed_ip_op_cost_", 2013, ".csv"),
                    colClasses = "character"),
              fread(paste0("output/all_ed_ip_op_cost_", 2014, ".csv"),
                    colClasses = "character"))
fl3 <-  fread(paste0("output/all_ed_ip_op_cost_", 2015, ".csv"),
              colClasses = "character")
fl2 <-  rbind(fread(paste0("output/all_ed_ip_op_cost_", 2016, ".csv"),
                    colClasses = "character"),
              fread(paste0("output/all_ed_ip_op_cost_", 2017, ".csv"),
                    colClasses = "character"))



for(i in c("ed_only","ip_only", "op_only", "ed_ip_only","ip_op_only", "ed_op_only",
           "ed_ip_op", "not_ed_ip_op")){
  mccost1 <- fl1[type_of_header_id == i, .(MEDICAL_CLAIM_HEADER_ID,
                                           INTERNAL_MEMBER_ID,
                                           ALLOWED_AMT)]
  mccost2 <- fl2[type_of_header_id == i, .(MEDICAL_CLAIM_HEADER_ID,
                                           INTERNAL_MEMBER_ID,
                                           ALLOWED_AMT)]
  mccost3 <- fl3[type_of_header_id == i, .(MEDICAL_CLAIM_HEADER_ID,
                                           INTERNAL_MEMBER_ID,
                                           ALLOWED_AMT)]
  names(mccost1) <- toupper(names(mccost1))
  names(mccost2) <- toupper(names(mccost2))
  names(mccost3) <- toupper(names(mccost3))
  mc1.pt <- mccost1[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
  ][ALLOWED_AMT >= 0 & 
      INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  mc2.pt <- mccost2[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
  ][ALLOWED_AMT >= 0 & 
      INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  mc3.pt <- mccost3[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
  ][ALLOWED_AMT >= 0 & 
      INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  mc1.no <- mccost1[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
  ][ALLOWED_AMT >= 0 & 
      INTERNAL_MEMBER_ID %in% pt_no$INTERNAL_MEMBER_ID]
  mc2.no <- mccost2[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
  ][ALLOWED_AMT >= 0 & 
      INTERNAL_MEMBER_ID %in% pt_no$INTERNAL_MEMBER_ID]
  mc3.no <- mccost3[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
  ][ALLOWED_AMT >= 0 & 
      INTERNAL_MEMBER_ID %in% pt_no$INTERNAL_MEMBER_ID]
  mc1.pt <- unique(mc1.pt, usekey = FALSE)
  mc2.pt <- unique(mc2.pt, usekey = FALSE)
  mc3.pt <- unique(mc3.pt, usekey = FALSE)
  mc1.no <- unique(mc1.no, usekey = FALSE)
  mc2.no <- unique(mc2.no, usekey = FALSE)
  mc3.no <- unique(mc3.no, usekey = FALSE)
  rm(mccost1, mccost2, mccost3)
  gc()
  mc1.pt.smy <- mc1.pt[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  mc1.pt.smy <- mc1.pt.smy[pt, on = "INTERNAL_MEMBER_ID"]
  mc2.pt.smy <- mc2.pt[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  mc2.pt.smy <- mc2.pt.smy[pt, on = "INTERNAL_MEMBER_ID"]
  mc3.pt.smy <- mc3.pt[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  mc3.pt.smy <- mc3.pt.smy[pt, on = "INTERNAL_MEMBER_ID"]
  mc1.no.smy <- mc1.no[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  mc1.no.smy <- mc1.no.smy[pt_no, on = "INTERNAL_MEMBER_ID"]
  mc2.no.smy <- mc2.no[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  mc2.no.smy <- mc2.no.smy[pt_no, on = "INTERNAL_MEMBER_ID"]
  mc3.no.smy <- mc3.no[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  mc3.no.smy <- mc3.no.smy[pt_no, on = "INTERNAL_MEMBER_ID"]
  mc1.pt.smy[is.na(mc1.pt.smy)] <- 0
  mc2.pt.smy[is.na(mc2.pt.smy)] <- 0
  mc3.pt.smy[is.na(mc3.pt.smy)] <- 0
  mc1.no.smy[is.na(mc1.no.smy)] <- 0
  mc2.no.smy[is.na(mc2.no.smy)] <- 0
  mc3.no.smy[is.na(mc3.no.smy)] <- 0
  out <- data.table()
  for (gender in gender_group){
    for(ages in age_group){
      tmp1 <- mc1.pt.smy[age %in% ages & GENDER_CODE %in% gender]
      tmp2 <- mc2.pt.smy[age %in% ages & GENDER_CODE %in% gender]
      tmp3 <- mc3.pt.smy[age %in% ages & GENDER_CODE %in% gender]
      cat(paste0("age range ", ages[1], "-", ages[length(ages)], " and gender ", gender[1],
                 " ", gender[length(gender)],
                 " results are: ", "\n"))
      tmp.out <- c(age = paste0(min(ages), "-", max(ages)),
                   gender = paste0(gender[1], "-", gender[length(gender)]),
                   summary(tmp1$ALLOWED_AMT)[4], summary(tmp3$ALLOWED_AMT)[4], summary(tmp2$ALLOWED_AMT)[4], 
                   sd1 = sd(tmp1$ALLOWED_AMT), sd3 = sd(tmp3$ALLOWED_AMT), sd2 = sd(tmp2$ALLOWED_AMT),
                   summary(tmp1$ALLOWED_AMT)[1], summary(tmp3$ALLOWED_AMT)[1], summary(tmp2$ALLOWED_AMT)[1],
                   summary(tmp1$ALLOWED_AMT)[6], summary(tmp3$ALLOWED_AMT)[6], summary(tmp2$ALLOWED_AMT)[6],
                   summary(tmp1$ALLOWED_AMT)[2], summary(tmp3$ALLOWED_AMT)[2], summary(tmp2$ALLOWED_AMT)[2], 
                   summary(tmp1$ALLOWED_AMT)[3], summary(tmp3$ALLOWED_AMT)[3], summary(tmp2$ALLOWED_AMT)[3],
                   summary(tmp1$ALLOWED_AMT)[5], summary(tmp3$ALLOWED_AMT)[5], summary(tmp2$ALLOWED_AMT)[5], 
                   quantile(tmp1$ALLOWED_AMT, 0.95), quantile(tmp3$ALLOWED_AMT, 0.95), quantile(tmp2$ALLOWED_AMT, 0.95),
                   quantile(tmp1$ALLOWED_AMT, 0.99), quantile(tmp3$ALLOWED_AMT, 0.99), quantile(tmp2$ALLOWED_AMT, 0.99))
      out <- rbind(out, data.table(t(tmp.out)))
    }
  }
  fwrite(out, file = paste0("sc_", i, ".csv"))
  
  out <- data.table()
  for (gender in gender_group){
    for(ages in age_group){
      tmp1 <- mc1.no.smy[age %in% ages & GENDER_CODE %in% gender]
      tmp2 <- mc2.no.smy[age %in% ages & GENDER_CODE %in% gender]
      tmp3 <- mc3.no.smy[age %in% ages & GENDER_CODE %in% gender]
      cat(paste0("age range ", ages[1], "-", ages[length(ages)], " and gender ", gender[1],
                 " ", gender[length(gender)],
                 " results are: ", "\n"))
      tmp.out <- c(age = paste0(min(ages), "-", max(ages)),
                   gender = paste0(gender[1], "-", gender[length(gender)]),
                   summary(tmp1$ALLOWED_AMT)[4], summary(tmp3$ALLOWED_AMT)[4], summary(tmp2$ALLOWED_AMT)[4], 
                   sd1 = sd(tmp1$ALLOWED_AMT), sd3 = sd(tmp3$ALLOWED_AMT), sd2 = sd(tmp2$ALLOWED_AMT),
                   summary(tmp1$ALLOWED_AMT)[1], summary(tmp3$ALLOWED_AMT)[1], summary(tmp2$ALLOWED_AMT)[1],
                   summary(tmp1$ALLOWED_AMT)[6], summary(tmp3$ALLOWED_AMT)[6], summary(tmp2$ALLOWED_AMT)[6],
                   summary(tmp1$ALLOWED_AMT)[2], summary(tmp3$ALLOWED_AMT)[2], summary(tmp2$ALLOWED_AMT)[2], 
                   summary(tmp1$ALLOWED_AMT)[3], summary(tmp3$ALLOWED_AMT)[3], summary(tmp2$ALLOWED_AMT)[3],
                   summary(tmp1$ALLOWED_AMT)[5], summary(tmp3$ALLOWED_AMT)[5], summary(tmp2$ALLOWED_AMT)[5], 
                   quantile(tmp1$ALLOWED_AMT, 0.95), quantile(tmp3$ALLOWED_AMT, 0.95), quantile(tmp2$ALLOWED_AMT, 0.95),
                   quantile(tmp1$ALLOWED_AMT, 0.99), quantile(tmp3$ALLOWED_AMT, 0.99), quantile(tmp2$ALLOWED_AMT, 0.99))
      out <- rbind(out, data.table(t(tmp.out)))
    }
  }
  fwrite(out, file = paste0("no_sc_", i, ".csv"))
  
  total_pt_cost <- fread(paste0("sc_", i, ".csv"))
  total_pt_cost <- rbind(total_pt_cost[1:5, ], total_pt_cost[9, ], 
                         total_pt_cost[6:8, ], total_pt_cost[10:12, ])
  total_pt_cost <- insertRows(total_pt_cost, c(2, 6, 9, 13), NA)
  total_no_pt_cost <- fread(paste0("no_sc_", i, ".csv"))
  total_no_pt_cost <- rbind(total_no_pt_cost[1:5, ], total_no_pt_cost[9, ], 
                            total_no_pt_cost[6:8, ], total_no_pt_cost[10:12, ])
  total_no_pt_cost <- insertRows(total_no_pt_cost, c(2, 6, 9, 13), NA)
  total_cost <- cbind(total_pt_cost[, 1:2],
                      total_pt_cost[, 3:5], total_no_pt_cost[, 3:5],
                      total_pt_cost[, 6:8], total_no_pt_cost[, 6:8],
                      total_pt_cost[, 9:11], total_no_pt_cost[, 9:11],
                      total_pt_cost[, 12:14], total_no_pt_cost[, 12:14],
                      total_pt_cost[, 15:17], total_no_pt_cost[, 15:17],
                      total_pt_cost[, 18:20], total_no_pt_cost[, 18:20],
                      total_pt_cost[, 21:23], total_no_pt_cost[, 21:23],
                      total_pt_cost[, 24:26], total_no_pt_cost[, 24:26],
                      total_pt_cost[, 27:29], total_no_pt_cost[, 27:29])
  fwrite(total_cost, file = paste0(i, "_smy.csv"))
  
}


for(i in c("all","ip", "op", "ed", "pharmacy")){
  if(i == "all"){
    mccost1 <- fl1[, .(MEDICAL_CLAIM_HEADER_ID,
                       INTERNAL_MEMBER_ID,
                       ALLOWED_AMT)]
    mccost2 <- fl2[, .(MEDICAL_CLAIM_HEADER_ID,
                       INTERNAL_MEMBER_ID,
                       ALLOWED_AMT)]
    mccost3 <- fl3[, .(MEDICAL_CLAIM_HEADER_ID,
                       INTERNAL_MEMBER_ID,
                       ALLOWED_AMT)]
  } else if( i == "ip"){
    mccost1 <- fl1[type_of_header_id %in% c("ip_only",  "ed_ip_only","ip_op_only", 
                                            "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                           INTERNAL_MEMBER_ID,
                                                           ALLOWED_AMT)]
    mccost2 <- fl2[type_of_header_id %in% c("ip_only",  "ed_ip_only","ip_op_only", 
                                            "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                           INTERNAL_MEMBER_ID,
                                                           ALLOWED_AMT)]
    mccost3 <- fl3[type_of_header_id %in% c("ip_only",  "ed_ip_only","ip_op_only", 
                                            "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                           INTERNAL_MEMBER_ID,
                                                           ALLOWED_AMT)]
  } else if (i == "op"){
    mccost1 <- fl1[type_of_header_id %in% c("op_only","ip_op_only", "ed_op_only",
                                            "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                           INTERNAL_MEMBER_ID,
                                                           ALLOWED_AMT)]
    mccost2 <- fl2[type_of_header_id %in% c("op_only","ip_op_only", "ed_op_only",
                                            "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                           INTERNAL_MEMBER_ID,
                                                           ALLOWED_AMT)]
    mccost3 <- fl3[type_of_header_id %in% c("op_only","ip_op_only", "ed_op_only",
                                            "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                           INTERNAL_MEMBER_ID,
                                                           ALLOWED_AMT)]
  } else if (i == "ed"){
    mccost1 <- fl1[type_of_header_id %in% c("ed_only","ed_ip_only", "ed_op_only",
                                            "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                           INTERNAL_MEMBER_ID,
                                                           ALLOWED_AMT)]
    mccost2 <- fl2[type_of_header_id %in% c("ed_only","ed_ip_only", "ed_op_only",
                                            "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                           INTERNAL_MEMBER_ID,
                                                           ALLOWED_AMT)]
    mccost3 <- fl3[type_of_header_id %in% c("ed_only","ed_ip_only", "ed_op_only",
                                            "ed_ip_op"), .(MEDICAL_CLAIM_HEADER_ID,
                                                           INTERNAL_MEMBER_ID,
                                                           ALLOWED_AMT)]
  } else if( i == 'pharmacy'){
    mccost1 <- rbind(fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                                  "cost_measure_intermediate_data/", 
                                  "cost_files_by_year/",
                                  "total_pharmacy_", 2013, "_all_ages.csv"), colClasses = "character",
                           select = c("INTERNAL_MEMBER_ID",
                                      "total", "pharmacy_claim_service_line_id"))[
                                        INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID, pt_no$INTERNAL_MEMBER_ID)],
                     fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                                  "cost_measure_intermediate_data/", 
                                  "cost_files_by_year/",
                                  "total_pharmacy_", 2014, "_all_ages.csv"), colClasses = "character",
                           select = c("INTERNAL_MEMBER_ID",
                                      "total", "pharmacy_claim_service_line_id"))[INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID, pt_no$INTERNAL_MEMBER_ID)])
    mccost2 <- rbind(fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                                  "cost_measure_intermediate_data/", 
                                  "cost_files_by_year/",
                                  "total_pharmacy_", 2016, "_all_ages.csv"), colClasses = "character",
                           select = c("INTERNAL_MEMBER_ID",
                                      "total", "pharmacy_claim_service_line_id"))[
                                        INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID, pt_no$INTERNAL_MEMBER_ID)],
                     fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                                  "cost_measure_intermediate_data/", 
                                  "cost_files_by_year/",
                                  "total_pharmacy_", 2017, "_all_ages.csv"), colClasses = "character",
                           select = c("INTERNAL_MEMBER_ID",
                                      "total", "pharmacy_claim_service_line_id"))[INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID, pt_no$INTERNAL_MEMBER_ID)])
    mccost3 <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                            "cost_measure_intermediate_data/", 
                            "cost_files_by_year/",
                            "total_pharmacy_", 2015, "_all_ages.csv"), colClasses = "character",
                     select = c("INTERNAL_MEMBER_ID",
                                "total", "pharmacy_claim_service_line_id"))[
                                  INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID, pt_no$INTERNAL_MEMBER_ID)]
    setnames(mccost1, "total", "ALLOWED_AMT")
    setnames(mccost2, "total", "ALLOWED_AMT")
    setnames(mccost3, "total", "ALLOWED_AMT")
  }
  names(mccost1) <- toupper(names(mccost1))
  names(mccost2) <- toupper(names(mccost2))
  names(mccost3) <- toupper(names(mccost3))
  mc1.pt <- mccost1[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
  ][ALLOWED_AMT >= 0 & 
      INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  mc2.pt <- mccost2[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
  ][ALLOWED_AMT >= 0 & 
      INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  mc3.pt <- mccost3[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
  ][ALLOWED_AMT >= 0 & 
      INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
  mc1.no <- mccost1[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
  ][ALLOWED_AMT >= 0 & 
      INTERNAL_MEMBER_ID %in% pt_no$INTERNAL_MEMBER_ID]
  mc2.no <- mccost2[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
  ][ALLOWED_AMT >= 0 & 
      INTERNAL_MEMBER_ID %in% pt_no$INTERNAL_MEMBER_ID]
  mc3.no <- mccost3[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
  ][ALLOWED_AMT >= 0 & 
      INTERNAL_MEMBER_ID %in% pt_no$INTERNAL_MEMBER_ID]
  mc1.pt <- unique(mc1.pt, usekey = FALSE)
  mc2.pt <- unique(mc2.pt, usekey = FALSE)
  mc3.pt <- unique(mc3.pt, usekey = FALSE)
  mc1.no <- unique(mc1.no, usekey = FALSE)
  mc2.no <- unique(mc2.no, usekey = FALSE)
  mc3.no <- unique(mc3.no, usekey = FALSE)
  rm(mccost1, mccost2, mccost3)
  gc()
  mc1.pt.smy <- mc1.pt[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  mc1.pt.smy <- mc1.pt.smy[pt, on = "INTERNAL_MEMBER_ID"]
  mc2.pt.smy <- mc2.pt[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  mc2.pt.smy <- mc2.pt.smy[pt, on = "INTERNAL_MEMBER_ID"]
  mc3.pt.smy <- mc3.pt[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  mc3.pt.smy <- mc3.pt.smy[pt, on = "INTERNAL_MEMBER_ID"]
  mc1.no.smy <- mc1.no[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  mc1.no.smy <- mc1.no.smy[pt_no, on = "INTERNAL_MEMBER_ID"]
  mc2.no.smy <- mc2.no[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  mc2.no.smy <- mc2.no.smy[pt_no, on = "INTERNAL_MEMBER_ID"]
  mc3.no.smy <- mc3.no[, lapply(.SD, sum, na.rm = TRUE),
                       .SDcols = "ALLOWED_AMT",
                       by = "INTERNAL_MEMBER_ID"]
  mc3.no.smy <- mc3.no.smy[pt_no, on = "INTERNAL_MEMBER_ID"]
  mc1.pt.smy[is.na(mc1.pt.smy)] <- 0
  mc2.pt.smy[is.na(mc2.pt.smy)] <- 0
  mc3.pt.smy[is.na(mc3.pt.smy)] <- 0
  mc1.no.smy[is.na(mc1.no.smy)] <- 0
  mc2.no.smy[is.na(mc2.no.smy)] <- 0
  mc3.no.smy[is.na(mc3.no.smy)] <- 0
  out <- data.table()
  for (gender in gender_group){
    for(ages in age_group){
      tmp1 <- mc1.pt.smy[age >= min(ages) & age <= max(ages)& GENDER_CODE %in% gender]
      tmp2 <- mc2.pt.smy[age %in% ages & GENDER_CODE %in% gender]
      tmp3 <- mc3.pt.smy[age %in% ages & GENDER_CODE %in% gender]
      cat(paste0("age range ", ages[1], "-", ages[length(ages)], " and gender ", gender[1],
                 " ", gender[length(gender)],
                 " results are: ", "\n"))
      tmp.out <- c(age = paste0(min(ages), "-", max(ages)),
                   gender = paste0(gender[1], "-", gender[length(gender)]),
                   summary(tmp1$ALLOWED_AMT)[4], summary(tmp3$ALLOWED_AMT)[4], summary(tmp2$ALLOWED_AMT)[4], 
                   sd1 = sd(tmp1$ALLOWED_AMT), sd3 = sd(tmp3$ALLOWED_AMT), sd2 = sd(tmp2$ALLOWED_AMT),
                   summary(tmp1$ALLOWED_AMT)[1], summary(tmp3$ALLOWED_AMT)[1], summary(tmp2$ALLOWED_AMT)[1],
                   summary(tmp1$ALLOWED_AMT)[6], summary(tmp3$ALLOWED_AMT)[6], summary(tmp2$ALLOWED_AMT)[6],
                   summary(tmp1$ALLOWED_AMT)[2], summary(tmp3$ALLOWED_AMT)[2], summary(tmp2$ALLOWED_AMT)[2], 
                   summary(tmp1$ALLOWED_AMT)[3], summary(tmp3$ALLOWED_AMT)[3], summary(tmp2$ALLOWED_AMT)[3],
                   summary(tmp1$ALLOWED_AMT)[5], summary(tmp3$ALLOWED_AMT)[5], summary(tmp2$ALLOWED_AMT)[5], 
                   quantile(tmp1$ALLOWED_AMT, 0.95), quantile(tmp3$ALLOWED_AMT, 0.95), quantile(tmp2$ALLOWED_AMT, 0.95),
                   quantile(tmp1$ALLOWED_AMT, 0.99), quantile(tmp3$ALLOWED_AMT, 0.99), quantile(tmp2$ALLOWED_AMT, 0.99))
      out <- rbind(out, data.table(t(tmp.out)))
    }
  }
  fwrite(out, file = paste0("sc_", i, ".csv"))
  
  out <- data.table()
  for (gender in gender_group){
    for(ages in age_group){
      tmp1 <- mc1.no.smy[age %in% ages & GENDER_CODE %in% gender]
      tmp2 <- mc2.no.smy[age %in% ages & GENDER_CODE %in% gender]
      tmp3 <- mc3.no.smy[age %in% ages & GENDER_CODE %in% gender]
      cat(paste0("age range ", ages[1], "-", ages[length(ages)], " and gender ", gender[1],
                 " ", gender[length(gender)],
                 " results are: ", "\n"))
      tmp.out <- c(age = paste0(min(ages), "-", max(ages)),
                   gender = paste0(gender[1], "-", gender[length(gender)]),
                   summary(tmp1$ALLOWED_AMT)[4], summary(tmp3$ALLOWED_AMT)[4], summary(tmp2$ALLOWED_AMT)[4], 
                   sd1 = sd(tmp1$ALLOWED_AMT), sd3 = sd(tmp3$ALLOWED_AMT), sd2 = sd(tmp2$ALLOWED_AMT),
                   summary(tmp1$ALLOWED_AMT)[1], summary(tmp3$ALLOWED_AMT)[1], summary(tmp2$ALLOWED_AMT)[1],
                   summary(tmp1$ALLOWED_AMT)[6], summary(tmp3$ALLOWED_AMT)[6], summary(tmp2$ALLOWED_AMT)[6],
                   summary(tmp1$ALLOWED_AMT)[2], summary(tmp3$ALLOWED_AMT)[2], summary(tmp2$ALLOWED_AMT)[2], 
                   summary(tmp1$ALLOWED_AMT)[3], summary(tmp3$ALLOWED_AMT)[3], summary(tmp2$ALLOWED_AMT)[3],
                   summary(tmp1$ALLOWED_AMT)[5], summary(tmp3$ALLOWED_AMT)[5], summary(tmp2$ALLOWED_AMT)[5], 
                   quantile(tmp1$ALLOWED_AMT, 0.95), quantile(tmp3$ALLOWED_AMT, 0.95), quantile(tmp2$ALLOWED_AMT, 0.95),
                   quantile(tmp1$ALLOWED_AMT, 0.99), quantile(tmp3$ALLOWED_AMT, 0.99), quantile(tmp2$ALLOWED_AMT, 0.99))
      out <- rbind(out, data.table(t(tmp.out)))
    }
  }
  fwrite(out, file = paste0("no_sc_", i, ".csv"))
  
  total_pt_cost <- fread(paste0("sc_", i, ".csv"))
  total_pt_cost <- rbind(total_pt_cost[1:5, ], total_pt_cost[9, ], 
                         total_pt_cost[6:8, ], total_pt_cost[10:12, ])
  total_pt_cost <- insertRows(total_pt_cost, c(2, 6, 9, 13), NA)
  total_no_pt_cost <- fread(paste0("no_sc_", i, ".csv"))
  total_no_pt_cost <- rbind(total_no_pt_cost[1:5, ], total_no_pt_cost[9, ], 
                            total_no_pt_cost[6:8, ], total_no_pt_cost[10:12, ])
  total_no_pt_cost <- insertRows(total_no_pt_cost, c(2, 6, 9, 13), NA)
  total_cost <- cbind(total_pt_cost[, 1:2],
                      total_pt_cost[, 3:5], total_no_pt_cost[, 3:5],
                      total_pt_cost[, 6:8], total_no_pt_cost[, 6:8],
                      total_pt_cost[, 9:11], total_no_pt_cost[, 9:11],
                      total_pt_cost[, 12:14], total_no_pt_cost[, 12:14],
                      total_pt_cost[, 15:17], total_no_pt_cost[, 15:17],
                      total_pt_cost[, 18:20], total_no_pt_cost[, 18:20],
                      total_pt_cost[, 21:23], total_no_pt_cost[, 21:23],
                      total_pt_cost[, 24:26], total_no_pt_cost[, 24:26],
                      total_pt_cost[, 27:29], total_no_pt_cost[, 27:29])
  fwrite(total_cost, file = paste0(i, "_smy.csv"))
  
}



## pharmacy
pt <- fread('output/sc_15_no_sc_13_14_ph.csv', colClasses = 'character',
            select = c("INTERNAL_MEMBER_ID", "birth_dt", 
                       "GENDER_CODE")) %>% unique()
pt_no <- fread(paste0("output/no_sc_2013_2015_pt_ph.csv"), colClasses = "character") %>% 
  unique(use.key = FALSE)
pt[, age := 2015 - as.integer(birth_dt)]
pt <- pt[age < 65 & age > 9]
pt_no[, age := 2015 - as.integer(birth_dt)]
pt_no <- pt_no[age < 65 & age > 9]

age_group <- list(c(10:64), c(10:24), c(25:44), c(45:64))
gender_group <- list(c("M", "F"), "M", "F")

i <- 'pharmacy'
mccost1 <- rbind(fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                              "cost_measure_intermediate_data/", 
                              "cost_files_by_year/",
                              "total_pharmacy_", 2013, "_all_ages.csv"), colClasses = "character",
                       select = c("INTERNAL_MEMBER_ID",
                                  "total", "pharmacy_claim_service_line_id"))[
                                    INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID, pt_no$INTERNAL_MEMBER_ID)],
                 fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                              "cost_measure_intermediate_data/", 
                              "cost_files_by_year/",
                              "total_pharmacy_", 2014, "_all_ages.csv"), colClasses = "character",
                       select = c("INTERNAL_MEMBER_ID",
                                  "total", "pharmacy_claim_service_line_id"))[INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID, pt_no$INTERNAL_MEMBER_ID)])
mccost2 <- rbind(fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                              "cost_measure_intermediate_data/", 
                              "cost_files_by_year/",
                              "total_pharmacy_", 2016, "_all_ages.csv"), colClasses = "character",
                       select = c("INTERNAL_MEMBER_ID",
                                  "total", "pharmacy_claim_service_line_id"))[
                                    INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID, pt_no$INTERNAL_MEMBER_ID)],
                 fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                              "cost_measure_intermediate_data/", 
                              "cost_files_by_year/",
                              "total_pharmacy_", 2017, "_all_ages.csv"), colClasses = "character",
                       select = c("INTERNAL_MEMBER_ID",
                                  "total", "pharmacy_claim_service_line_id"))[INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID, pt_no$INTERNAL_MEMBER_ID)])
mccost3 <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                        "cost_measure_intermediate_data/", 
                        "cost_files_by_year/",
                        "total_pharmacy_", 2015, "_all_ages.csv"), colClasses = "character",
                 select = c("INTERNAL_MEMBER_ID",
                            "total", "pharmacy_claim_service_line_id"))[
                              INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID, pt_no$INTERNAL_MEMBER_ID)]
setnames(mccost1, "total", "ALLOWED_AMT")
setnames(mccost2, "total", "ALLOWED_AMT")
setnames(mccost3, "total", "ALLOWED_AMT")
names(mccost1) <- toupper(names(mccost1))
names(mccost2) <- toupper(names(mccost2))
names(mccost3) <- toupper(names(mccost3))
mc1.pt <- mccost1[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
][ALLOWED_AMT >= 0 & 
    INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
mc2.pt <- mccost2[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
][ALLOWED_AMT >= 0 & 
    INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
mc3.pt <- mccost3[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
][ALLOWED_AMT >= 0 & 
    INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
mc1.no <- mccost1[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
][ALLOWED_AMT >= 0 & 
    INTERNAL_MEMBER_ID %in% pt_no$INTERNAL_MEMBER_ID]
mc2.no <- mccost2[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
][ALLOWED_AMT >= 0 & 
    INTERNAL_MEMBER_ID %in% pt_no$INTERNAL_MEMBER_ID]
mc3.no <- mccost3[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)
][ALLOWED_AMT >= 0 & 
    INTERNAL_MEMBER_ID %in% pt_no$INTERNAL_MEMBER_ID]
mc1.pt <- unique(mc1.pt, usekey = FALSE)
mc2.pt <- unique(mc2.pt, usekey = FALSE)
mc3.pt <- unique(mc3.pt, usekey = FALSE)
mc1.no <- unique(mc1.no, usekey = FALSE)
mc2.no <- unique(mc2.no, usekey = FALSE)
mc3.no <- unique(mc3.no, usekey = FALSE)
rm(mccost1, mccost2, mccost3)
gc()
mc1.pt.smy <- mc1.pt[, lapply(.SD, sum, na.rm = TRUE),
                     .SDcols = "ALLOWED_AMT",
                     by = "INTERNAL_MEMBER_ID"]
mc1.pt.smy <- mc1.pt.smy[pt, on = "INTERNAL_MEMBER_ID"]
mc2.pt.smy <- mc2.pt[, lapply(.SD, sum, na.rm = TRUE),
                     .SDcols = "ALLOWED_AMT",
                     by = "INTERNAL_MEMBER_ID"]
mc2.pt.smy <- mc2.pt.smy[pt, on = "INTERNAL_MEMBER_ID"]
mc3.pt.smy <- mc3.pt[, lapply(.SD, sum, na.rm = TRUE),
                     .SDcols = "ALLOWED_AMT",
                     by = "INTERNAL_MEMBER_ID"]
mc3.pt.smy <- mc3.pt.smy[pt, on = "INTERNAL_MEMBER_ID"]
mc1.no.smy <- mc1.no[, lapply(.SD, sum, na.rm = TRUE),
                     .SDcols = "ALLOWED_AMT",
                     by = "INTERNAL_MEMBER_ID"]
mc1.no.smy <- mc1.no.smy[pt_no, on = "INTERNAL_MEMBER_ID"]
mc2.no.smy <- mc2.no[, lapply(.SD, sum, na.rm = TRUE),
                     .SDcols = "ALLOWED_AMT",
                     by = "INTERNAL_MEMBER_ID"]
mc2.no.smy <- mc2.no.smy[pt_no, on = "INTERNAL_MEMBER_ID"]
mc3.no.smy <- mc3.no[, lapply(.SD, sum, na.rm = TRUE),
                     .SDcols = "ALLOWED_AMT",
                     by = "INTERNAL_MEMBER_ID"]
mc3.no.smy <- mc3.no.smy[pt_no, on = "INTERNAL_MEMBER_ID"]
mc1.pt.smy[is.na(mc1.pt.smy)] <- 0
mc2.pt.smy[is.na(mc2.pt.smy)] <- 0
mc3.pt.smy[is.na(mc3.pt.smy)] <- 0
mc1.no.smy[is.na(mc1.no.smy)] <- 0
mc2.no.smy[is.na(mc2.no.smy)] <- 0
mc3.no.smy[is.na(mc3.no.smy)] <- 0
out <- data.table()
for (gender in gender_group){
  for(ages in age_group){
    tmp1 <- mc1.pt.smy[age >= min(ages) & age <= max(ages)& GENDER_CODE %in% gender]
    tmp2 <- mc2.pt.smy[age %in% ages & GENDER_CODE %in% gender]
    tmp3 <- mc3.pt.smy[age %in% ages & GENDER_CODE %in% gender]
    cat(paste0("age range ", ages[1], "-", ages[length(ages)], " and gender ", gender[1],
               " ", gender[length(gender)],
               " results are: ", "\n"))
    tmp.out <- c(age = paste0(min(ages), "-", max(ages)),
                 gender = paste0(gender[1], "-", gender[length(gender)]),
                 summary(tmp1$ALLOWED_AMT)[4], summary(tmp3$ALLOWED_AMT)[4], summary(tmp2$ALLOWED_AMT)[4], 
                 sd1 = sd(tmp1$ALLOWED_AMT), sd3 = sd(tmp3$ALLOWED_AMT), sd2 = sd(tmp2$ALLOWED_AMT),
                 summary(tmp1$ALLOWED_AMT)[1], summary(tmp3$ALLOWED_AMT)[1], summary(tmp2$ALLOWED_AMT)[1],
                 summary(tmp1$ALLOWED_AMT)[6], summary(tmp3$ALLOWED_AMT)[6], summary(tmp2$ALLOWED_AMT)[6],
                 summary(tmp1$ALLOWED_AMT)[2], summary(tmp3$ALLOWED_AMT)[2], summary(tmp2$ALLOWED_AMT)[2], 
                 summary(tmp1$ALLOWED_AMT)[3], summary(tmp3$ALLOWED_AMT)[3], summary(tmp2$ALLOWED_AMT)[3],
                 summary(tmp1$ALLOWED_AMT)[5], summary(tmp3$ALLOWED_AMT)[5], summary(tmp2$ALLOWED_AMT)[5], 
                 quantile(tmp1$ALLOWED_AMT, 0.95), quantile(tmp3$ALLOWED_AMT, 0.95), quantile(tmp2$ALLOWED_AMT, 0.95),
                 quantile(tmp1$ALLOWED_AMT, 0.99), quantile(tmp3$ALLOWED_AMT, 0.99), quantile(tmp2$ALLOWED_AMT, 0.99))
    out <- rbind(out, data.table(t(tmp.out)))
  }
}
fwrite(out, file = paste0("sc_", i, ".csv"))

out <- data.table()
for (gender in gender_group){
  for(ages in age_group){
    tmp1 <- mc1.no.smy[age %in% ages & GENDER_CODE %in% gender]
    tmp2 <- mc2.no.smy[age %in% ages & GENDER_CODE %in% gender]
    tmp3 <- mc3.no.smy[age %in% ages & GENDER_CODE %in% gender]
    cat(paste0("age range ", ages[1], "-", ages[length(ages)], " and gender ", gender[1],
               " ", gender[length(gender)],
               " results are: ", "\n"))
    tmp.out <- c(age = paste0(min(ages), "-", max(ages)),
                 gender = paste0(gender[1], "-", gender[length(gender)]),
                 summary(tmp1$ALLOWED_AMT)[4], summary(tmp3$ALLOWED_AMT)[4], summary(tmp2$ALLOWED_AMT)[4], 
                 sd1 = sd(tmp1$ALLOWED_AMT), sd3 = sd(tmp3$ALLOWED_AMT), sd2 = sd(tmp2$ALLOWED_AMT),
                 summary(tmp1$ALLOWED_AMT)[1], summary(tmp3$ALLOWED_AMT)[1], summary(tmp2$ALLOWED_AMT)[1],
                 summary(tmp1$ALLOWED_AMT)[6], summary(tmp3$ALLOWED_AMT)[6], summary(tmp2$ALLOWED_AMT)[6],
                 summary(tmp1$ALLOWED_AMT)[2], summary(tmp3$ALLOWED_AMT)[2], summary(tmp2$ALLOWED_AMT)[2], 
                 summary(tmp1$ALLOWED_AMT)[3], summary(tmp3$ALLOWED_AMT)[3], summary(tmp2$ALLOWED_AMT)[3],
                 summary(tmp1$ALLOWED_AMT)[5], summary(tmp3$ALLOWED_AMT)[5], summary(tmp2$ALLOWED_AMT)[5], 
                 quantile(tmp1$ALLOWED_AMT, 0.95), quantile(tmp3$ALLOWED_AMT, 0.95), quantile(tmp2$ALLOWED_AMT, 0.95),
                 quantile(tmp1$ALLOWED_AMT, 0.99), quantile(tmp3$ALLOWED_AMT, 0.99), quantile(tmp2$ALLOWED_AMT, 0.99))
    out <- rbind(out, data.table(t(tmp.out)))
  }
}
fwrite(out, file = paste0("no_sc_", i, ".csv"))

total_pt_cost <- fread(paste0("sc_", i, ".csv"))
total_pt_cost <- rbind(total_pt_cost[1:5, ], total_pt_cost[9, ], 
                       total_pt_cost[6:8, ], total_pt_cost[10:12, ])
total_pt_cost <- insertRows(total_pt_cost, c(2, 6, 9, 13), NA)
total_no_pt_cost <- fread(paste0("no_sc_", i, ".csv"))
total_no_pt_cost <- rbind(total_no_pt_cost[1:5, ], total_no_pt_cost[9, ], 
                          total_no_pt_cost[6:8, ], total_no_pt_cost[10:12, ])
total_no_pt_cost <- insertRows(total_no_pt_cost, c(2, 6, 9, 13), NA)
total_cost <- cbind(total_pt_cost[, 1:2],
                    total_pt_cost[, 3:5], total_no_pt_cost[, 3:5],
                    total_pt_cost[, 6:8], total_no_pt_cost[, 6:8],
                    total_pt_cost[, 9:11], total_no_pt_cost[, 9:11],
                    total_pt_cost[, 12:14], total_no_pt_cost[, 12:14],
                    total_pt_cost[, 15:17], total_no_pt_cost[, 15:17],
                    total_pt_cost[, 18:20], total_no_pt_cost[, 18:20],
                    total_pt_cost[, 21:23], total_no_pt_cost[, 21:23],
                    total_pt_cost[, 24:26], total_no_pt_cost[, 24:26],
                    total_pt_cost[, 27:29], total_no_pt_cost[, 27:29])
fwrite(total_cost, file = paste0(i, "_smy.csv"))
