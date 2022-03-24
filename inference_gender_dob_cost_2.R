if(Sys.info()[4] %in% c("LAZ-ID1",
                        "LAZ-ID2",
                        "LAZ-DEID1",
                        "LAZ-DEID2")){
  setwd("E:/CT_APCD/Sai/suicide_cost")
}
library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats",
               "knitr", "plyr")
need.packages(need_pkgs)

## total mc cost
rm(list=ls())
sc_mc <- fread("output/cost/sc_2015_total_medical_cost_.csv", colClasses = "character")
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc <- sc_mc[, -c("birth_dt", "GENDER_CODE")]
sc_mc[is.na(sc_mc)] <- 0
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                    by = dob_gender]
names(sc_mc_n)[-1] <- "N"
sc_mc_smy <- join_all(list(sc_mc_n, sc_mc), by = "dob_gender", type = "left")
sc_mc_smy[is.na(sc_mc_smy)] <- 0
fwrite(sc_mc_smy,
       file = "output/cost/inference/sc_2015_total_medical_cost_.csv")

nosc_mc <- fread("output/cost/compare_total_medical_cost_.csv", colClasses = "character")
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc <- nosc_mc[, -c("birth_dt", "GENDER_CODE")]
nosc_mc[is.na(nosc_mc)] <- 0
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(nosc_mc_n)[-1] <- "N"
nosc_mc_smy <- join_all(list(nosc_mc_n, nosc_mc), by = "dob_gender", type = "left")
nosc_mc_smy[is.na(nosc_mc_smy)] <- 0
fwrite(nosc_mc_smy,
       file = "output/cost/inference/nosc_2015_total_medical_cost_.csv")

## ip cost
rm(list=ls())
sc_mc <- fread("output/cost/sc_2015_ip_cost_.csv", colClasses = "character")
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc <- sc_mc[, -c("birth_dt", "GENDER_CODE")]
sc_mc[is.na(sc_mc)] <- 0
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(sc_mc_n)[-1] <- "N"
sc_mc_smy <- join_all(list(sc_mc_n, sc_mc), by = "dob_gender", type = "left")
sc_mc_smy[is.na(sc_mc_smy)] <- 0
fwrite(sc_mc_smy,
       file = "output/cost/inference/sc_2015_ip_cost_.csv")

nosc_mc <- fread("output/cost/compare_ip_cost_.csv", colClasses = "character")
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc <- nosc_mc[, -c("birth_dt", "GENDER_CODE")]
nosc_mc[is.na(nosc_mc)] <- 0
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                     by = dob_gender]
names(nosc_mc_n)[-1] <- "N"
nosc_mc_smy <- join_all(list(nosc_mc_n, nosc_mc), by = "dob_gender", type = "left")
nosc_mc_smy[is.na(nosc_mc_smy)] <- 0
fwrite(nosc_mc_smy,
       file = "output/cost/inference/nosc_2015_ip_cost_.csv")

## op cost
rm(list=ls())
sc_mc <- fread("output/cost/sc_2015_op_cost_.csv", colClasses = "character")
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc <- sc_mc[, -c("birth_dt", "GENDER_CODE")]
sc_mc[is.na(sc_mc)] <- 0
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(sc_mc_n)[-1] <- "N"
sc_mc_smy <- join_all(list(sc_mc_n, sc_mc), by = "dob_gender", type = "left")
sc_mc_smy[is.na(sc_mc_smy)] <- 0
fwrite(sc_mc_smy,
       file = "output/cost/inference/sc_2015_op_cost_.csv")

nosc_mc <- fread("output/cost/compare_op_cost_.csv", colClasses = "character")
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc <- nosc_mc[, -c("birth_dt", "GENDER_CODE")]
nosc_mc[is.na(nosc_mc)] <- 0
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                     by = dob_gender]
names(nosc_mc_n)[-1] <- "N"
nosc_mc_smy <- join_all(list(nosc_mc_n, nosc_mc), by = "dob_gender", type = "left")
nosc_mc_smy[is.na(nosc_mc_smy)] <- 0
fwrite(nosc_mc_smy,
       file = "output/cost/inference/nosc_2015_op_cost_.csv")

## pc cost
rm(list=ls())
sc_mc <- fread("output/cost/sc_2015_pc_cost_.csv", colClasses = "character")
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc <- sc_mc[, -c("birth_dt", "GENDER_CODE")]
sc_mc[is.na(sc_mc)] <- 0
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(sc_mc_n)[-1] <- "N"
sc_mc_smy <- join_all(list(sc_mc_n, sc_mc), by = "dob_gender", type = "left")
sc_mc_smy[is.na(sc_mc_smy)] <- 0
fwrite(sc_mc_smy,
       file = "output/cost/inference/sc_2015_pc_cost_.csv")

nosc_mc <- fread("output/cost/compare_pc_cost_.csv", colClasses = "character")
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc <- nosc_mc[, -c("birth_dt", "GENDER_CODE")]
nosc_mc[is.na(nosc_mc)] <- 0
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                     by = dob_gender]
names(nosc_mc_n)[-1] <- "N"
nosc_mc_smy <- join_all(list(nosc_mc_n, nosc_mc), by = "dob_gender", type = "left")
nosc_mc_smy[is.na(nosc_mc_smy)] <- 0
fwrite(nosc_mc_smy,
       file = "output/cost/inference/nosc_2015_pc_cost_.csv")

## total pharmacy cost
rm(list=ls())
sc_mc <- fread("output/cost/sc_2015_total_pharmacy_cost_.csv", colClasses = "character")
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc <- sc_mc[, -c("birth_dt", "GENDER_CODE")]
sc_mc[is.na(sc_mc)] <- 0
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(sc_mc_n)[-1] <- "N"
sc_mc_smy <- join_all(list(sc_mc_n, sc_mc), by = "dob_gender", type = "left")
sc_mc_smy[is.na(sc_mc_smy)] <- 0
fwrite(sc_mc_smy,
       file = "output/cost/inference/sc_2015_total_pharmacy_cost_.csv")

nosc_mc <- fread("output/cost/compare_total_pharmacy_cost_.csv", colClasses = "character")
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc <- nosc_mc[, -c("birth_dt", "GENDER_CODE")]
nosc_mc[is.na(nosc_mc)] <- 0
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                     by = dob_gender]
names(nosc_mc_n)[-1] <- "N"
nosc_mc_smy <- join_all(list(nosc_mc_n, nosc_mc), by = "dob_gender", type = "left")
nosc_mc_smy[is.na(nosc_mc_smy)] <- 0
fwrite(nosc_mc_smy,
       file = "output/cost/inference/nosc_2015_total_pharmacy_cost_.csv")
