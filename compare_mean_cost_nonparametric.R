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
need_pkgs <- c("data.table", "bit64", "tools", "touch", "haven", "BSDA",
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
  mc1.pt <- unique(mc1.pt, usekey = FALSE)
  mc2.pt <- unique(mc2.pt, usekey = FALSE)
  mc3.pt <- unique(mc3.pt, usekey = FALSE)
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
  mc1.pt.smy[is.na(mc1.pt.smy)] <- 0
  mc2.pt.smy[is.na(mc2.pt.smy)] <- 0
  mc3.pt.smy[is.na(mc3.pt.smy)] <- 0
  
  tmp1 <- mc1.pt.smy
  tmp2 <- mc2.pt.smy
  tmp3 <- mc3.pt.smy
  tmp1[, age := as.integer(age)]
  tmp1[age >= 10 & age <= 24, age_group := "10-24"]
  tmp1[age >= 25 & age <= 44, age_group := "25-44"]
  tmp1[age >= 45 & age <= 64, age_group := "45-64"]
  tmp1[, group := paste0(GENDER_CODE, "-", age_group )]
  tmp2[, age := as.integer(age)]
  tmp2[age >= 10 & age <= 24, age_group := "10-24"]
  tmp2[age >= 25 & age <= 44, age_group := "25-44"]
  tmp2[age >= 45 & age <= 64, age_group := "45-64"]
  tmp2[, group := paste0(GENDER_CODE, "-", age_group )]
  tmp3[, age := as.integer(age)]
  tmp3[age >= 10 & age <= 24, age_group := "10-24"]
  tmp3[age >= 25 & age <= 44, age_group := "25-44"]
  tmp3[age >= 45 & age <= 64, age_group := "45-64"]
  tmp3[, group := paste0(GENDER_CODE, "-", age_group )]
  cat(paste0("cost type: ", i, ", year: 2013-2014, ",
             ", compare mean cost of age and gender groups:", "\n"))
  print((kruskal.test(ALLOWED_AMT ~ group, data = tmp1)))
  cat(paste0("cost type: ", i, ", year: 2015, ",
             ", compare mean cost of age and gender groups:", "\n"))
  print((kruskal.test(ALLOWED_AMT ~ group, data = tmp3)))
  cat(paste0("cost type: ", i, ", year: 2016-2016, ",
             ", compare mean cost of age and gender groups:", "\n"))
  print((kruskal.test(ALLOWED_AMT ~ group, data = tmp2)))
  # for(ages in age_group){
  #   tmp1 <- mc1.pt.smy[age %in% ages]
  #   tmp2 <- mc2.pt.smy[age %in% ages]
  #   tmp3 <- mc3.pt.smy[age %in% ages]
  #   cat(paste0("cost type: ", i, ", year: 2013-2014, age range: ", ages[1], "-", ages[length(ages)],
  #              ", compare mean cost of male and female:", "\n"))
  #   print((kruskal.test(ALLOWED_AMT ~ GENDER_CODE, data = tmp1)))
  #   cat(paste0("cost type: ", i, ", year: 2015, age range: ", ages[1], "-", ages[length(ages)],
  #              ", compare mean cost of male and female:", "\n"))
  #   print((kruskal.test(ALLOWED_AMT ~ GENDER_CODE, data = tmp3)))
  #   cat(paste0("cost type: ", i, ", year: 2016-2017, age range: ", ages[1], "-", ages[length(ages)],
  #              ", compare mean cost of male and female:", "\n"))
  #   print((kruskal.test(ALLOWED_AMT ~ GENDER_CODE, data = tmp2)))
  # }
  # for(gender in gender_group){
  #   gender1 <- paste(gender, collapse = "-")
  #   tmp1 <- mc1.pt.smy[GENDER_CODE %in% gender]
  #   tmp1[, age := as.integer(age)]
  #   tmp1[age >= 10 & age <= 24, age_group := "10-24"]
  #   tmp1[age >= 25 & age <= 44, age_group := "25-44"]
  #   tmp1[age >= 45 & age <= 64, age_group := "45-64"]
  #   tmp2 <- mc2.pt.smy[GENDER_CODE %in% gender]
  #   tmp2[, age := as.integer(age)]
  #   tmp2[age >= 10 & age <= 24, age_group := "10-24"]
  #   tmp2[age >= 25 & age <= 44, age_group := "25-44"]
  #   tmp2[age >= 45 & age <= 64, age_group := "45-64"]
  #   tmp3 <- mc3.pt.smy[GENDER_CODE %in% gender]
  #   tmp3[, age := as.integer(age)]
  #   tmp3[age >= 10 & age <= 24, age_group := "10-24"]
  #   tmp3[age >= 25 & age <= 44, age_group := "25-44"]
  #   tmp3[age >= 45 & age <= 64, age_group := "45-64"]
  #   cat(paste0("cost type: ", i, ", year: 2013-2014, gender: ", gender1,
  #              ", compare mean cost of age groups:", "\n"))
  #   print((kruskal.test(ALLOWED_AMT ~ age_group, data = tmp1)))
  #   cat(paste0("cost type: ", i, ", year: 2015, gender: ",  gender1,
  #              ", compare mean cost of age groups:", "\n"))
  #   print((kruskal.test(ALLOWED_AMT ~ age_group, data = tmp3)))
  #   cat(paste0("cost type: ", i, ", year: 2016-2017, gender: ", gender1,
  #              ", compare mean cost of age groups:", "\n"))
  #   print((kruskal.test(ALLOWED_AMT ~ age_group, data = tmp2)))
  # }
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
                                        INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID)],
                     fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                                  "cost_measure_intermediate_data/", 
                                  "cost_files_by_year/",
                                  "total_pharmacy_", 2014, "_all_ages.csv"), colClasses = "character",
                           select = c("INTERNAL_MEMBER_ID",
                                      "total", "pharmacy_claim_service_line_id"))[INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID)])
    mccost2 <- rbind(fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                                  "cost_measure_intermediate_data/", 
                                  "cost_files_by_year/",
                                  "total_pharmacy_", 2016, "_all_ages.csv"), colClasses = "character",
                           select = c("INTERNAL_MEMBER_ID",
                                      "total", "pharmacy_claim_service_line_id"))[
                                        INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID)],
                     fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                                  "cost_measure_intermediate_data/", 
                                  "cost_files_by_year/",
                                  "total_pharmacy_", 2017, "_all_ages.csv"), colClasses = "character",
                           select = c("INTERNAL_MEMBER_ID",
                                      "total", "pharmacy_claim_service_line_id"))[INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID)])
    mccost3 <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                            "cost_measure_intermediate_data/", 
                            "cost_files_by_year/",
                            "total_pharmacy_", 2015, "_all_ages.csv"), colClasses = "character",
                     select = c("INTERNAL_MEMBER_ID",
                                "total", "pharmacy_claim_service_line_id"))[
                                  INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID)]
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
  mc1.pt <- unique(mc1.pt, usekey = FALSE)
  mc2.pt <- unique(mc2.pt, usekey = FALSE)
  mc3.pt <- unique(mc3.pt, usekey = FALSE)
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
  mc1.pt.smy[is.na(mc1.pt.smy)] <- 0
  mc2.pt.smy[is.na(mc2.pt.smy)] <- 0
  mc3.pt.smy[is.na(mc3.pt.smy)] <- 0
  
  
  tmp1 <- mc1.pt.smy
  tmp2 <- mc2.pt.smy
  tmp3 <- mc3.pt.smy
  tmp1[, age := as.integer(age)]
  tmp1[age >= 10 & age <= 24, age_group := "10-24"]
  tmp1[age >= 25 & age <= 44, age_group := "25-44"]
  tmp1[age >= 45 & age <= 64, age_group := "45-64"]
  tmp1[, group := paste0(GENDER_CODE, "-", age_group )]
  tmp2[, age := as.integer(age)]
  tmp2[age >= 10 & age <= 24, age_group := "10-24"]
  tmp2[age >= 25 & age <= 44, age_group := "25-44"]
  tmp2[age >= 45 & age <= 64, age_group := "45-64"]
  tmp2[, group := paste0(GENDER_CODE, "-", age_group )]
  tmp3[, age := as.integer(age)]
  tmp3[age >= 10 & age <= 24, age_group := "10-24"]
  tmp3[age >= 25 & age <= 44, age_group := "25-44"]
  tmp3[age >= 45 & age <= 64, age_group := "45-64"]
  tmp3[, group := paste0(GENDER_CODE, "-", age_group )]
  cat(paste0("cost type: ", i, ", year: 2013-2014, ",
             ", compare mean cost of age and gender groups:", "\n"))
  print((kruskal.test(ALLOWED_AMT ~ group, data = tmp1)))
  cat(paste0("cost type: ", i, ", year: 2015, ",
             ", compare mean cost of age and gender groups:", "\n"))
  print((kruskal.test(ALLOWED_AMT ~ group, data = tmp3)))
  cat(paste0("cost type: ", i, ", year: 2016-2016, ",
             ", compare mean cost of age and gender groups:", "\n"))
  print((kruskal.test(ALLOWED_AMT ~ group, data = tmp2)))
  
  # for(ages in age_group){
  #   tmp1 <- mc1.pt.smy[age %in% ages]
  #   tmp2 <- mc2.pt.smy[age %in% ages]
  #   tmp3 <- mc3.pt.smy[age %in% ages]
  #   cat(paste0("cost type: ", i, ", year: 2013-2014, age range: ", ages[1], "-", ages[length(ages)],
  #              ", compare mean cost of male and female:", "\n"))
  #   print((kruskal.test(ALLOWED_AMT ~ GENDER_CODE, data = tmp1)))
  #   cat(paste0("cost type: ", i, ", year: 2015, age range: ", ages[1], "-", ages[length(ages)],
  #              ", compare mean cost of male and female:", "\n"))
  #   print((kruskal.test(ALLOWED_AMT ~ GENDER_CODE, data = tmp3)))
  #   cat(paste0("cost type: ", i, ", year: 2016-2017, age range: ", ages[1], "-", ages[length(ages)],
  #              ", compare mean cost of male and female:", "\n"))
  #   print((kruskal.test(ALLOWED_AMT ~ GENDER_CODE, data = tmp2)))
  # }
  # for(gender in gender_group){
  #   gender1 <- paste(gender, collapse = "-")
  #   tmp1 <- mc1.pt.smy[GENDER_CODE %in% gender]
  #   tmp1[, age := as.integer(age)]
  #   tmp1[age >= 10 & age <= 24, age_group := "10-24"]
  #   tmp1[age >= 25 & age <= 44, age_group := "25-44"]
  #   tmp1[age >= 45 & age <= 64, age_group := "45-64"]
  #   tmp2 <- mc2.pt.smy[GENDER_CODE %in% gender]
  #   tmp2[, age := as.integer(age)]
  #   tmp2[age >= 10 & age <= 24, age_group := "10-24"]
  #   tmp2[age >= 25 & age <= 44, age_group := "25-44"]
  #   tmp2[age >= 45 & age <= 64, age_group := "45-64"]
  #   tmp3 <- mc3.pt.smy[GENDER_CODE %in% gender]
  #   tmp3[, age := as.integer(age)]
  #   tmp3[age >= 10 & age <= 24, age_group := "10-24"]
  #   tmp3[age >= 25 & age <= 44, age_group := "25-44"]
  #   tmp3[age >= 45 & age <= 64, age_group := "45-64"]
  #   cat(paste0("cost type: ", i, ", year: 2013-2014, gender: ", gender1,
  #              ", compare mean cost of age groups:", "\n"))
  #   print((kruskal.test(ALLOWED_AMT ~ age_group, data = tmp1)))
  #   cat(paste0("cost type: ", i, ", year: 2015, gender: ", gender1,
  #              ", compare mean cost of age groups:", "\n"))
  #   print((kruskal.test(ALLOWED_AMT ~ age_group, data = tmp3)))
  #   cat(paste0("cost type: ", i, ", year: 2016-2017, gender: ", gender1,
  #              ", compare mean cost of age groups:", "\n"))
  #   print((kruskal.test(ALLOWED_AMT ~ age_group, data = tmp2)))
  # }
  
}


## pharmacy
for(i in c("pharmacy")){
  pt <- fread('output/sc_15_no_sc_13_14_ph.csv', colClasses = 'character',
              select = c("INTERNAL_MEMBER_ID", "birth_dt", 
                         "GENDER_CODE")) %>% unique()
  
  pt[, age := 2015 - as.integer(birth_dt)]
  pt <- pt[age < 65 & age > 9]
  age_group <- list(c(10:64), c(10:24), c(25:44), c(45:64))
  gender_group <- list(c("M", "F"), "M", "F")
  
  mccost1 <- rbind(fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                                "cost_measure_intermediate_data/", 
                                "cost_files_by_year/",
                                "total_pharmacy_", 2013, "_all_ages.csv"), colClasses = "character",
                         select = c("INTERNAL_MEMBER_ID",
                                    "total", "pharmacy_claim_service_line_id"))[
                                      INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID)],
                   fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                                "cost_measure_intermediate_data/", 
                                "cost_files_by_year/",
                                "total_pharmacy_", 2014, "_all_ages.csv"), colClasses = "character",
                         select = c("INTERNAL_MEMBER_ID",
                                    "total", "pharmacy_claim_service_line_id"))[INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID)])
  mccost2 <- rbind(fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                                "cost_measure_intermediate_data/", 
                                "cost_files_by_year/",
                                "total_pharmacy_", 2016, "_all_ages.csv"), colClasses = "character",
                         select = c("INTERNAL_MEMBER_ID",
                                    "total", "pharmacy_claim_service_line_id"))[
                                      INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID)],
                   fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                                "cost_measure_intermediate_data/", 
                                "cost_files_by_year/",
                                "total_pharmacy_", 2017, "_all_ages.csv"), colClasses = "character",
                         select = c("INTERNAL_MEMBER_ID",
                                    "total", "pharmacy_claim_service_line_id"))[INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID)])
  mccost3 <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                          "cost_measure_intermediate_data/", 
                          "cost_files_by_year/",
                          "total_pharmacy_", 2015, "_all_ages.csv"), colClasses = "character",
                   select = c("INTERNAL_MEMBER_ID",
                              "total", "pharmacy_claim_service_line_id"))[
                                INTERNAL_MEMBER_ID %in% c(pt$INTERNAL_MEMBER_ID)]
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
  mc1.pt <- unique(mc1.pt, usekey = FALSE)
  mc2.pt <- unique(mc2.pt, usekey = FALSE)
  mc3.pt <- unique(mc3.pt, usekey = FALSE)
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
  mc1.pt.smy[is.na(mc1.pt.smy)] <- 0
  mc2.pt.smy[is.na(mc2.pt.smy)] <- 0
  mc3.pt.smy[is.na(mc3.pt.smy)] <- 0
  
  
  tmp1 <- mc1.pt.smy
  tmp2 <- mc2.pt.smy
  tmp3 <- mc3.pt.smy
  tmp1[, age := as.integer(age)]
  tmp1[age >= 10 & age <= 24, age_group := "10-24"]
  tmp1[age >= 25 & age <= 44, age_group := "25-44"]
  tmp1[age >= 45 & age <= 64, age_group := "45-64"]
  tmp1[, group := paste0(GENDER_CODE, "-", age_group )]
  tmp2[, age := as.integer(age)]
  tmp2[age >= 10 & age <= 24, age_group := "10-24"]
  tmp2[age >= 25 & age <= 44, age_group := "25-44"]
  tmp2[age >= 45 & age <= 64, age_group := "45-64"]
  tmp2[, group := paste0(GENDER_CODE, "-", age_group )]
  tmp3[, age := as.integer(age)]
  tmp3[age >= 10 & age <= 24, age_group := "10-24"]
  tmp3[age >= 25 & age <= 44, age_group := "25-44"]
  tmp3[age >= 45 & age <= 64, age_group := "45-64"]
  tmp3[, group := paste0(GENDER_CODE, "-", age_group )]
  cat(paste0("cost type: ", i, ", year: 2013-2014, ",
             ", compare mean cost of age and gender groups:", "\n"))
  print((kruskal.test(ALLOWED_AMT ~ group, data = tmp1)))
  cat(paste0("cost type: ", i, ", year: 2015, ",
             ", compare mean cost of age and gender groups:", "\n"))
  print((kruskal.test(ALLOWED_AMT ~ group, data = tmp3)))
  cat(paste0("cost type: ", i, ", year: 2016-2016, ",
             ", compare mean cost of age and gender groups:", "\n"))
  print((kruskal.test(ALLOWED_AMT ~ group, data = tmp2)))
  
  
  for(ages in age_group){
    tmp1 <- mc1.pt.smy[age %in% ages]
    tmp2 <- mc2.pt.smy[age %in% ages]
    tmp3 <- mc3.pt.smy[age %in% ages]
    cat(paste0("cost type: ", i, ", year: 2013-2014, age range: ", ages[1], "-", ages[length(ages)],
               ", compare mean cost of male and female:", "\n"))
    print((kruskal.test(ALLOWED_AMT ~ GENDER_CODE, data = tmp1)))
    cat(paste0("cost type: ", i, ", year: 2015, age range: ", ages[1], "-", ages[length(ages)],
               ", compare mean cost of male and female:", "\n"))
    print((kruskal.test(ALLOWED_AMT ~ GENDER_CODE, data = tmp3)))
    cat(paste0("cost type: ", i, ", year: 2016-2017, age range: ", ages[1], "-", ages[length(ages)],
               ", compare mean cost of male and female:", "\n"))
    print((kruskal.test(ALLOWED_AMT ~ GENDER_CODE, data = tmp2)))
  }
  for(gender in gender_group){
    gender1 <- paste(gender, collapse = "-")
    tmp1 <- mc1.pt.smy[GENDER_CODE %in% gender]
    tmp1[, age := as.integer(age)]
    tmp1[age >= 10 & age <= 24, age_group := "10-24"]
    tmp1[age >= 25 & age <= 44, age_group := "25-44"]
    tmp1[age >= 45 & age <= 64, age_group := "45-64"]
    tmp2 <- mc2.pt.smy[GENDER_CODE %in% gender]
    tmp2[, age := as.integer(age)]
    tmp2[age >= 10 & age <= 24, age_group := "10-24"]
    tmp2[age >= 25 & age <= 44, age_group := "25-44"]
    tmp2[age >= 45 & age <= 64, age_group := "45-64"]
    tmp3 <- mc3.pt.smy[GENDER_CODE %in% gender]
    tmp3[, age := as.integer(age)]
    tmp3[age >= 10 & age <= 24, age_group := "10-24"]
    tmp3[age >= 25 & age <= 44, age_group := "25-44"]
    tmp3[age >= 45 & age <= 64, age_group := "45-64"]
    cat(paste0("cost type: ", i, ", year: 2013-2014, gender: ", gender1,
               ", compare mean cost of age groups:", "\n"))
    print((kruskal.test(ALLOWED_AMT ~ age_group, data = tmp1)))
    cat(paste0("cost type: ", i, ", year: 2015, gender: ", gender1,
               ", compare mean cost of age groups:", "\n"))
    print((kruskal.test(ALLOWED_AMT ~ age_group, data = tmp3)))
    cat(paste0("cost type: ", i, ", year: 2016-2017, gender: ", gender1,
               ", compare mean cost of age groups:", "\n"))
    print((kruskal.test(ALLOWED_AMT ~ age_group, data = tmp2)))
  }
}
