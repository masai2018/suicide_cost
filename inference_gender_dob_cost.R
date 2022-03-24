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
mc_names <- paste0("fy", 2013:2017, "_total_medical_cost")
sc_mc <- fread("output/cost/sc_2015_total_medical_cost_.csv", colClasses = "character")
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc[sc_mc == ""] <- 0
sc_mc_o <- sc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                    by = dob_gender]
names(sc_mc_n)[-1] <- "N"
sc_mc <- sc_mc[, lapply(.SD, as.numeric), .SDcols = mc_names, by = "dob_gender"]
sc_mc_mean <- sc_mc[, lapply(.SD, mean), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_mean)[-1] <- paste0(names(sc_mc_mean)[-1], "_mean")
sc_mc_median <- sc_mc[, lapply(.SD, median), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_median)[-1] <- paste0(names(sc_mc_median)[-1], "_median")
sc_mc_sd <- sc_mc[, lapply(.SD, sd), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_sd)[-1] <- paste0(names(sc_mc_sd)[-1], "_sd")
sc_mc_max <- sc_mc[, lapply(.SD, max), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_max)[-1] <- paste0(names(sc_mc_max)[-1], "_max")
sc_mc_min <- sc_mc[, lapply(.SD, min), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_min)[-1] <- paste0(names(sc_mc_min)[-1], "_min")
sc_mc_smy <- join_all(list(sc_mc_o, sc_mc_n, sc_mc_mean, sc_mc_median, sc_mc_sd,
                           sc_mc_max, sc_mc_min), by = "dob_gender", type = "left")
sc_mc_smy[is.na(sc_mc_smy)] <- 0
fwrite(sc_mc_smy,
       file = "output/cost/inference/sc_pt_total_medical_cost_inference.csv")


nosc_mc <- fread("output/cost/compare_total_medical_cost_.csv", colClasses = "character")
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc[nosc_mc == ""] <- 0
nosc_mc_o <- nosc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(nosc_mc_n)[-1] <- "N"
nosc_mc <- nosc_mc[, lapply(.SD, as.numeric), .SDcols = mc_names, by = "dob_gender"]
nosc_mc_mean <- nosc_mc[, lapply(.SD, mean), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_mean)[-1] <- paste0(names(nosc_mc_mean)[-1], "_mean")
nosc_mc_median <- nosc_mc[, lapply(.SD, median), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_median)[-1] <- paste0(names(nosc_mc_median)[-1], "_median")
nosc_mc_sd <- nosc_mc[, lapply(.SD, sd), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_sd)[-1] <- paste0(names(nosc_mc_sd)[-1], "_sd")
nosc_mc_max <- nosc_mc[, lapply(.SD, max), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_max)[-1] <- paste0(names(nosc_mc_max)[-1], "_max")
nosc_mc_min <- nosc_mc[, lapply(.SD, min), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_min)[-1] <- paste0(names(nosc_mc_min)[-1], "_min")
nosc_mc_smy <- join_all(list(nosc_mc_n, nosc_mc_mean, nosc_mc_median, nosc_mc_sd,
                           nosc_mc_max, nosc_mc_min), by = "dob_gender", type = "left")
nosc_mc_smy[is.na(nosc_mc_smy)] <- 0
fwrite(nosc_mc_smy,
       file = "output/cost/inference/nosc_pt_total_medical_cost_inference.csv")

## inpatient
rm(list=ls())
mc_names <- paste0("fy", 2013:2017, "_ip_cost")
sc_mc <- fread("output/cost/sc_2015_ip_cost_.csv", colClasses = "character")
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc[sc_mc == ""] <- 0
sc_mc_o <- sc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(sc_mc_n)[-1] <- "N"
sc_mc <- sc_mc[, lapply(.SD, as.numeric), .SDcols = mc_names, by = "dob_gender"]
sc_mc_mean <- sc_mc[, lapply(.SD, mean), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_mean)[-1] <- paste0(names(sc_mc_mean)[-1], "_mean")
sc_mc_median <- sc_mc[, lapply(.SD, median), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_median)[-1] <- paste0(names(sc_mc_median)[-1], "_median")
sc_mc_sd <- sc_mc[, lapply(.SD, sd), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_sd)[-1] <- paste0(names(sc_mc_sd)[-1], "_sd")
sc_mc_max <- sc_mc[, lapply(.SD, max), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_max)[-1] <- paste0(names(sc_mc_max)[-1], "_max")
sc_mc_min <- sc_mc[, lapply(.SD, min), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_min)[-1] <- paste0(names(sc_mc_min)[-1], "_min")
sc_mc_smy <- join_all(list(sc_mc_o, sc_mc_n, sc_mc_mean, sc_mc_median, sc_mc_sd,
                           sc_mc_max, sc_mc_min), by = "dob_gender", type = "left")
sc_mc_smy[is.na(sc_mc_smy)] <- 0
fwrite(sc_mc_smy,
       file = "output/cost/inference/sc_pt_ip_cost_inference.csv")


nosc_mc <- fread("output/cost/compare_ip_cost_.csv", colClasses = "character")
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc[nosc_mc == ""] <- 0
nosc_mc_o <- nosc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                     by = dob_gender]
names(nosc_mc_n)[-1] <- "N"
nosc_mc <- nosc_mc[, lapply(.SD, as.numeric), .SDcols = mc_names, by = "dob_gender"]
nosc_mc_mean <- nosc_mc[, lapply(.SD, mean), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_mean)[-1] <- paste0(names(nosc_mc_mean)[-1], "_mean")
nosc_mc_median <- nosc_mc[, lapply(.SD, median), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_median)[-1] <- paste0(names(nosc_mc_median)[-1], "_median")
nosc_mc_sd <- nosc_mc[, lapply(.SD, sd), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_sd)[-1] <- paste0(names(nosc_mc_sd)[-1], "_sd")
nosc_mc_max <- nosc_mc[, lapply(.SD, max), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_max)[-1] <- paste0(names(nosc_mc_max)[-1], "_max")
nosc_mc_min <- nosc_mc[, lapply(.SD, min), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_min)[-1] <- paste0(names(nosc_mc_min)[-1], "_min")
nosc_mc_smy <- join_all(list(nosc_mc_n, nosc_mc_mean, nosc_mc_median, nosc_mc_sd,
                             nosc_mc_max, nosc_mc_min), by = "dob_gender", type = "left")
nosc_mc_smy[is.na(nosc_mc_smy)] <- 0
fwrite(nosc_mc_smy,
       file = "output/cost/inference/nosc_pt_ip_cost_inference.csv")

## outpatient
rm(list=ls())
mc_names <- paste0("fy", 2013:2017, "_op_cost")
sc_mc <- fread("output/cost/sc_2015_op_cost_.csv", colClasses = "character")
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc[sc_mc == ""] <- 0
sc_mc_o <- sc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(sc_mc_n)[-1] <- "N"
sc_mc <- sc_mc[, lapply(.SD, as.numeric), .SDcols = mc_names, by = "dob_gender"]
sc_mc_mean <- sc_mc[, lapply(.SD, mean), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_mean)[-1] <- paste0(names(sc_mc_mean)[-1], "_mean")
sc_mc_median <- sc_mc[, lapply(.SD, median), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_median)[-1] <- paste0(names(sc_mc_median)[-1], "_median")
sc_mc_sd <- sc_mc[, lapply(.SD, sd), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_sd)[-1] <- paste0(names(sc_mc_sd)[-1], "_sd")
sc_mc_max <- sc_mc[, lapply(.SD, max), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_max)[-1] <- paste0(names(sc_mc_max)[-1], "_max")
sc_mc_min <- sc_mc[, lapply(.SD, min), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_min)[-1] <- paste0(names(sc_mc_min)[-1], "_min")
sc_mc_smy <- join_all(list(sc_mc_o, sc_mc_n, sc_mc_mean, sc_mc_median, sc_mc_sd,
                           sc_mc_max, sc_mc_min), by = "dob_gender", type = "left")
sc_mc_smy[is.na(sc_mc_smy)] <- 0
fwrite(sc_mc_smy,
       file = "output/cost/inference/sc_pt_op_cost_inference.csv")


nosc_mc <- fread("output/cost/compare_op_cost_.csv", colClasses = "character")
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc[nosc_mc == ""] <- 0
nosc_mc_o <- nosc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                     by = dob_gender]
names(nosc_mc_n)[-1] <- "N"
nosc_mc <- nosc_mc[, lapply(.SD, as.numeric), .SDcols = mc_names, by = "dob_gender"]
nosc_mc_mean <- nosc_mc[, lapply(.SD, mean), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_mean)[-1] <- paste0(names(nosc_mc_mean)[-1], "_mean")
nosc_mc_median <- nosc_mc[, lapply(.SD, median), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_median)[-1] <- paste0(names(nosc_mc_median)[-1], "_median")
nosc_mc_sd <- nosc_mc[, lapply(.SD, sd), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_sd)[-1] <- paste0(names(nosc_mc_sd)[-1], "_sd")
nosc_mc_max <- nosc_mc[, lapply(.SD, max), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_max)[-1] <- paste0(names(nosc_mc_max)[-1], "_max")
nosc_mc_min <- nosc_mc[, lapply(.SD, min), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_min)[-1] <- paste0(names(nosc_mc_min)[-1], "_min")
nosc_mc_smy <- join_all(list(nosc_mc_n, nosc_mc_mean, nosc_mc_median, nosc_mc_sd,
                             nosc_mc_max, nosc_mc_min), by = "dob_gender", type = "left")
nosc_mc_smy[is.na(nosc_mc_smy)] <- 0
fwrite(nosc_mc_smy,
       file = "output/cost/inference/nosc_pt_op_cost_inference.csv")

## primary care 
rm(list=ls())
mc_names <- paste0("fy", 2013:2017, "_pc_cost")
sc_mc <- fread("output/cost/sc_2015_pc_cost_.csv", colClasses = "character")
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc[sc_mc == ""] <- 0
sc_mc_o <- sc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(sc_mc_n)[-1] <- "N"
sc_mc <- sc_mc[, lapply(.SD, as.numeric), .SDcols = mc_names, by = "dob_gender"]
sc_mc_mean <- sc_mc[, lapply(.SD, mean), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_mean)[-1] <- paste0(names(sc_mc_mean)[-1], "_mean")
sc_mc_median <- sc_mc[, lapply(.SD, median), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_median)[-1] <- paste0(names(sc_mc_median)[-1], "_median")
sc_mc_sd <- sc_mc[, lapply(.SD, sd), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_sd)[-1] <- paste0(names(sc_mc_sd)[-1], "_sd")
sc_mc_max <- sc_mc[, lapply(.SD, max), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_max)[-1] <- paste0(names(sc_mc_max)[-1], "_max")
sc_mc_min <- sc_mc[, lapply(.SD, min), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_min)[-1] <- paste0(names(sc_mc_min)[-1], "_min")
sc_mc_smy <- join_all(list(sc_mc_o, sc_mc_n, sc_mc_mean, sc_mc_median, sc_mc_sd,
                           sc_mc_max, sc_mc_min), by = "dob_gender", type = "left")
sc_mc_smy[is.na(sc_mc_smy)] <- 0
fwrite(sc_mc_smy,
       file = "output/cost/inference/sc_pt_pc_cost_inference.csv")


nosc_mc <- fread("output/cost/compare_pc_cost_.csv", colClasses = "character")
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc[nosc_mc == ""] <- 0
nosc_mc_o <- nosc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                     by = dob_gender]
names(nosc_mc_n)[-1] <- "N"
nosc_mc <- nosc_mc[, lapply(.SD, as.numeric), .SDcols = mc_names, by = "dob_gender"]
nosc_mc_mean <- nosc_mc[, lapply(.SD, mean), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_mean)[-1] <- paste0(names(nosc_mc_mean)[-1], "_mean")
nosc_mc_median <- nosc_mc[, lapply(.SD, median), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_median)[-1] <- paste0(names(nosc_mc_median)[-1], "_median")
nosc_mc_sd <- nosc_mc[, lapply(.SD, sd), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_sd)[-1] <- paste0(names(nosc_mc_sd)[-1], "_sd")
nosc_mc_max <- nosc_mc[, lapply(.SD, max), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_max)[-1] <- paste0(names(nosc_mc_max)[-1], "_max")
nosc_mc_min <- nosc_mc[, lapply(.SD, min), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_min)[-1] <- paste0(names(nosc_mc_min)[-1], "_min")
nosc_mc_smy <- join_all(list(nosc_mc_n, nosc_mc_mean, nosc_mc_median, nosc_mc_sd,
                             nosc_mc_max, nosc_mc_min), by = "dob_gender", type = "left")
nosc_mc_smy[is.na(nosc_mc_smy)] <- 0
fwrite(nosc_mc_smy,
       file = "output/cost/inference/nosc_pt_pc_cost_inference.csv")

## total pharmacy
rm(list=ls())
mc_names <- paste0("fy", 2013:2017, "_total_pharmacy_cost")
sc_mc <- fread("output/cost/sc_2015_total_pharmacy_cost_.csv", colClasses = "character")
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc[sc_mc == ""] <- 0
sc_mc_o <- sc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(sc_mc_n)[-1] <- "N"
sc_mc <- sc_mc[, lapply(.SD, as.numeric), .SDcols = mc_names, by = "dob_gender"]
sc_mc_mean <- sc_mc[, lapply(.SD, mean), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_mean)[-1] <- paste0(names(sc_mc_mean)[-1], "_mean")
sc_mc_median <- sc_mc[, lapply(.SD, median), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_median)[-1] <- paste0(names(sc_mc_median)[-1], "_median")
sc_mc_sd <- sc_mc[, lapply(.SD, sd), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_sd)[-1] <- paste0(names(sc_mc_sd)[-1], "_sd")
sc_mc_max <- sc_mc[, lapply(.SD, max), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_max)[-1] <- paste0(names(sc_mc_max)[-1], "_max")
sc_mc_min <- sc_mc[, lapply(.SD, min), .SDcols = mc_names, by = "dob_gender"]
names(sc_mc_min)[-1] <- paste0(names(sc_mc_min)[-1], "_min")
sc_mc_smy <- join_all(list(sc_mc_o, sc_mc_n, sc_mc_mean, sc_mc_median, sc_mc_sd,
                           sc_mc_max, sc_mc_min), by = "dob_gender", type = "left")
sc_mc_smy[is.na(sc_mc_smy)] <- 0
fwrite(sc_mc_smy,
       file = "output/cost/inference/sc_pt_total_pharmacy_cost_inference.csv")


nosc_mc <- fread("output/cost/compare_total_pharmacy_cost_.csv", colClasses = "character")
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc[nosc_mc == ""] <- 0
nosc_mc_o <- nosc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                     by = dob_gender]
names(nosc_mc_n)[-1] <- "N"
nosc_mc <- nosc_mc[, lapply(.SD, as.numeric), .SDcols = mc_names, by = "dob_gender"]
nosc_mc_mean <- nosc_mc[, lapply(.SD, mean), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_mean)[-1] <- paste0(names(nosc_mc_mean)[-1], "_mean")
nosc_mc_median <- nosc_mc[, lapply(.SD, median), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_median)[-1] <- paste0(names(nosc_mc_median)[-1], "_median")
nosc_mc_sd <- nosc_mc[, lapply(.SD, sd), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_sd)[-1] <- paste0(names(nosc_mc_sd)[-1], "_sd")
nosc_mc_max <- nosc_mc[, lapply(.SD, max), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_max)[-1] <- paste0(names(nosc_mc_max)[-1], "_max")
nosc_mc_min <- nosc_mc[, lapply(.SD, min), .SDcols = mc_names, by = "dob_gender"]
names(nosc_mc_min)[-1] <- paste0(names(nosc_mc_min)[-1], "_min")
nosc_mc_smy <- join_all(list(nosc_mc_n, nosc_mc_mean, nosc_mc_median, nosc_mc_sd,
                             nosc_mc_max, nosc_mc_min), by = "dob_gender", type = "left")
nosc_mc_smy[is.na(nosc_mc_smy)] <- 0
fwrite(nosc_mc_smy,
       file = "output/cost/inference/nosc_pt_total_pharmacy_cost_inference.csv")
