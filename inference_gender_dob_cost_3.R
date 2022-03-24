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
scmc_names <- paste0("sc_fy", 2013:2017, "_total_medical_cost")
sc_mc <- fread("output/cost/sc_2015_total_medical_cost_.csv", colClasses = "character")
names(sc_mc)[-(1:3)] <- paste0("sc_", names(sc_mc)[-(1:3)])
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc[sc_mc == ""] <- 0
sc_mc_o <- sc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                    by = dob_gender]
names(sc_mc_n)[-1] <- "sc_N"
sc_mc <- sc_mc[, lapply(.SD, as.numeric), .SDcols = scmc_names, by = "dob_gender"]
sc_mc_mean <- sc_mc[, lapply(.SD, mean), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_mean)[-1] <- paste0(names(sc_mc_mean)[-1], "_mean")
sc_mc_median <- sc_mc[, lapply(.SD, median), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_median)[-1] <- paste0(names(sc_mc_median)[-1], "_median")
sc_mc_sd <- sc_mc[, lapply(.SD, sd), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_sd)[-1] <- paste0(names(sc_mc_sd)[-1], "_sd")
sc_mc_max <- sc_mc[, lapply(.SD, max), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_max)[-1] <- paste0(names(sc_mc_max)[-1], "_max")
sc_mc_min <- sc_mc[, lapply(.SD, min), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_min)[-1] <- paste0(names(sc_mc_min)[-1], "_min")

noscmc_names <- paste0("nosc_fy", 2013:2017, "_total_medical_cost")
nosc_mc <- fread("output/cost/compare_total_medical_cost_.csv", colClasses = "character")
names(nosc_mc)[-(1:3)] <- paste0("nosc_", names(nosc_mc)[-(1:3)])
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc <- nosc_mc[, -c("birth_dt", "GENDER_CODE")]
nosc_mc[nosc_mc == ""] <- 0
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(nosc_mc_n)[-1] <- "nosc_N"
nosc_mc <- nosc_mc[, lapply(.SD, as.numeric), .SDcols = noscmc_names, by = "dob_gender"]
nosc_mc_mean <- nosc_mc[, lapply(.SD, mean), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_mean)[-1] <- paste0(names(nosc_mc_mean)[-1], "_mean")
nosc_mc_median <- nosc_mc[, lapply(.SD, median), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_median)[-1] <- paste0(names(nosc_mc_median)[-1], "_median")
nosc_mc_sd <- nosc_mc[, lapply(.SD, sd), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_sd)[-1] <- paste0(names(nosc_mc_sd)[-1], "_sd")
nosc_mc_max <- nosc_mc[, lapply(.SD, max), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_max)[-1] <- paste0(names(nosc_mc_max)[-1], "_max")
nosc_mc_min <- nosc_mc[, lapply(.SD, min), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_min)[-1] <- paste0(names(nosc_mc_min)[-1], "_min")

mc_smy <- join_all(list(sc_mc_o, sc_mc_n, nosc_mc_n, sc_mc_mean, nosc_mc_mean, 
                        sc_mc_median, nosc_mc_median, sc_mc_sd, nosc_mc_sd,
                           sc_mc_max, nosc_mc_max, sc_mc_min, nosc_mc_min), 
                   by = "dob_gender", type = "left")
mc_smy[is.na(mc_smy)] <- 0
finalnames <- c(names(mc_smy)[1:5],
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2013, "_total_medical_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2014, "_total_medical_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2015, "_total_medical_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2016, "_total_medical_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2017, "_total_medical_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)))
setcolorder(mc_smy, finalnames)
fwrite(mc_smy,
       file = "output/cost/inference/sc_nosc_pt_total_medical_cost_inference.csv")

## total ip cost
rm(list=ls())
scmc_names <- paste0("sc_fy", 2013:2017, "_ip_cost")
sc_mc <- fread("output/cost/sc_2015_ip_cost_.csv", colClasses = "character")
names(sc_mc)[-(1:3)] <- paste0("sc_", names(sc_mc)[-(1:3)])
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc[sc_mc == ""] <- 0
sc_mc_o <- sc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(sc_mc_n)[-1] <- "sc_N"
sc_mc <- sc_mc[, lapply(.SD, as.numeric), .SDcols = scmc_names, by = "dob_gender"]
sc_mc_mean <- sc_mc[, lapply(.SD, mean), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_mean)[-1] <- paste0(names(sc_mc_mean)[-1], "_mean")
sc_mc_median <- sc_mc[, lapply(.SD, median), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_median)[-1] <- paste0(names(sc_mc_median)[-1], "_median")
sc_mc_sd <- sc_mc[, lapply(.SD, sd), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_sd)[-1] <- paste0(names(sc_mc_sd)[-1], "_sd")
sc_mc_max <- sc_mc[, lapply(.SD, max), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_max)[-1] <- paste0(names(sc_mc_max)[-1], "_max")
sc_mc_min <- sc_mc[, lapply(.SD, min), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_min)[-1] <- paste0(names(sc_mc_min)[-1], "_min")

noscmc_names <- paste0("nosc_fy", 2013:2017, "_ip_cost")
nosc_mc <- fread("output/cost/compare_ip_cost_.csv", colClasses = "character")
names(nosc_mc)[-(1:3)] <- paste0("nosc_", names(nosc_mc)[-(1:3)])
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc <- nosc_mc[, -c("birth_dt", "GENDER_CODE")]
nosc_mc[nosc_mc == ""] <- 0
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                     by = dob_gender]
names(nosc_mc_n)[-1] <- "nosc_N"
nosc_mc <- nosc_mc[, lapply(.SD, as.numeric), .SDcols = noscmc_names, by = "dob_gender"]
nosc_mc_mean <- nosc_mc[, lapply(.SD, mean), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_mean)[-1] <- paste0(names(nosc_mc_mean)[-1], "_mean")
nosc_mc_median <- nosc_mc[, lapply(.SD, median), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_median)[-1] <- paste0(names(nosc_mc_median)[-1], "_median")
nosc_mc_sd <- nosc_mc[, lapply(.SD, sd), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_sd)[-1] <- paste0(names(nosc_mc_sd)[-1], "_sd")
nosc_mc_max <- nosc_mc[, lapply(.SD, max), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_max)[-1] <- paste0(names(nosc_mc_max)[-1], "_max")
nosc_mc_min <- nosc_mc[, lapply(.SD, min), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_min)[-1] <- paste0(names(nosc_mc_min)[-1], "_min")

mc_smy <- join_all(list(sc_mc_o, sc_mc_n, nosc_mc_n, sc_mc_mean, nosc_mc_mean, 
                        sc_mc_median, nosc_mc_median, sc_mc_sd, nosc_mc_sd,
                        sc_mc_max, nosc_mc_max, sc_mc_min, nosc_mc_min), 
                   by = "dob_gender", type = "left")
mc_smy[is.na(mc_smy)] <- 0
finalnames <- c(names(mc_smy)[1:5],
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2013, "_ip_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2014, "_ip_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2015, "_ip_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2016, "_ip_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2017, "_ip_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)))
setcolorder(mc_smy, finalnames)
fwrite(mc_smy,
       file = "output/cost/inference/sc_nosc_pt_ip_cost_inference.csv")

## total pharmacy cost
rm(list=ls())
scmc_names <- paste0("sc_fy", 2013:2017, "_total_pharmacy_cost")
sc_mc <- fread("output/cost/sc_2015_total_pharmacy_cost_.csv", colClasses = "character")
names(sc_mc)[-(1:3)] <- paste0("sc_", names(sc_mc)[-(1:3)])
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc[sc_mc == ""] <- 0
sc_mc_o <- sc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(sc_mc_n)[-1] <- "sc_N"
sc_mc <- sc_mc[, lapply(.SD, as.numeric), .SDcols = scmc_names, by = "dob_gender"]
sc_mc_mean <- sc_mc[, lapply(.SD, mean), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_mean)[-1] <- paste0(names(sc_mc_mean)[-1], "_mean")
sc_mc_median <- sc_mc[, lapply(.SD, median), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_median)[-1] <- paste0(names(sc_mc_median)[-1], "_median")
sc_mc_sd <- sc_mc[, lapply(.SD, sd), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_sd)[-1] <- paste0(names(sc_mc_sd)[-1], "_sd")
sc_mc_max <- sc_mc[, lapply(.SD, max), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_max)[-1] <- paste0(names(sc_mc_max)[-1], "_max")
sc_mc_min <- sc_mc[, lapply(.SD, min), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_min)[-1] <- paste0(names(sc_mc_min)[-1], "_min")

noscmc_names <- paste0("nosc_fy", 2013:2017, "_total_pharmacy_cost")
nosc_mc <- fread("output/cost/compare_total_pharmacy_cost_.csv", colClasses = "character")
names(nosc_mc)[-(1:3)] <- paste0("nosc_", names(nosc_mc)[-(1:3)])
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc <- nosc_mc[, -c("birth_dt", "GENDER_CODE")]
nosc_mc[nosc_mc == ""] <- 0
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                     by = dob_gender]
names(nosc_mc_n)[-1] <- "nosc_N"
nosc_mc <- nosc_mc[, lapply(.SD, as.numeric), .SDcols = noscmc_names, by = "dob_gender"]
nosc_mc_mean <- nosc_mc[, lapply(.SD, mean), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_mean)[-1] <- paste0(names(nosc_mc_mean)[-1], "_mean")
nosc_mc_median <- nosc_mc[, lapply(.SD, median), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_median)[-1] <- paste0(names(nosc_mc_median)[-1], "_median")
nosc_mc_sd <- nosc_mc[, lapply(.SD, sd), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_sd)[-1] <- paste0(names(nosc_mc_sd)[-1], "_sd")
nosc_mc_max <- nosc_mc[, lapply(.SD, max), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_max)[-1] <- paste0(names(nosc_mc_max)[-1], "_max")
nosc_mc_min <- nosc_mc[, lapply(.SD, min), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_min)[-1] <- paste0(names(nosc_mc_min)[-1], "_min")

mc_smy <- join_all(list(sc_mc_o, sc_mc_n, nosc_mc_n, sc_mc_mean, nosc_mc_mean, 
                        sc_mc_median, nosc_mc_median, sc_mc_sd, nosc_mc_sd,
                        sc_mc_max, nosc_mc_max, sc_mc_min, nosc_mc_min), 
                   by = "dob_gender", type = "left")
mc_smy[is.na(mc_smy)] <- 0
finalnames <- c(names(mc_smy)[1:5],
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2013, "_total_pharmacy_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2014, "_total_pharmacy_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2015, "_total_pharmacy_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2016, "_total_pharmacy_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2017, "_total_pharmacy_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)))
setcolorder(mc_smy, finalnames)
fwrite(mc_smy,
       file = "output/cost/inference/sc_nosc_pt_total_pharmacy_cost_inference.csv")

## total op cost
rm(list=ls())
scmc_names <- paste0("sc_fy", 2013:2017, "_op_cost")
sc_mc <- fread("output/cost/sc_2015_op_cost_.csv", colClasses = "character")
names(sc_mc)[-(1:3)] <- paste0("sc_", names(sc_mc)[-(1:3)])
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc[sc_mc == ""] <- 0
sc_mc_o <- sc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(sc_mc_n)[-1] <- "sc_N"
sc_mc <- sc_mc[, lapply(.SD, as.numeric), .SDcols = scmc_names, by = "dob_gender"]
sc_mc_mean <- sc_mc[, lapply(.SD, mean), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_mean)[-1] <- paste0(names(sc_mc_mean)[-1], "_mean")
sc_mc_median <- sc_mc[, lapply(.SD, median), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_median)[-1] <- paste0(names(sc_mc_median)[-1], "_median")
sc_mc_sd <- sc_mc[, lapply(.SD, sd), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_sd)[-1] <- paste0(names(sc_mc_sd)[-1], "_sd")
sc_mc_max <- sc_mc[, lapply(.SD, max), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_max)[-1] <- paste0(names(sc_mc_max)[-1], "_max")
sc_mc_min <- sc_mc[, lapply(.SD, min), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_min)[-1] <- paste0(names(sc_mc_min)[-1], "_min")

noscmc_names <- paste0("nosc_fy", 2013:2017, "_op_cost")
nosc_mc <- fread("output/cost/compare_op_cost_.csv", colClasses = "character")
names(nosc_mc)[-(1:3)] <- paste0("nosc_", names(nosc_mc)[-(1:3)])
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc <- nosc_mc[, -c("birth_dt", "GENDER_CODE")]
nosc_mc[nosc_mc == ""] <- 0
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                     by = dob_gender]
names(nosc_mc_n)[-1] <- "nosc_N"
nosc_mc <- nosc_mc[, lapply(.SD, as.numeric), .SDcols = noscmc_names, by = "dob_gender"]
nosc_mc_mean <- nosc_mc[, lapply(.SD, mean), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_mean)[-1] <- paste0(names(nosc_mc_mean)[-1], "_mean")
nosc_mc_median <- nosc_mc[, lapply(.SD, median), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_median)[-1] <- paste0(names(nosc_mc_median)[-1], "_median")
nosc_mc_sd <- nosc_mc[, lapply(.SD, sd), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_sd)[-1] <- paste0(names(nosc_mc_sd)[-1], "_sd")
nosc_mc_max <- nosc_mc[, lapply(.SD, max), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_max)[-1] <- paste0(names(nosc_mc_max)[-1], "_max")
nosc_mc_min <- nosc_mc[, lapply(.SD, min), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_min)[-1] <- paste0(names(nosc_mc_min)[-1], "_min")

mc_smy <- join_all(list(sc_mc_o, sc_mc_n, nosc_mc_n, sc_mc_mean, nosc_mc_mean, 
                        sc_mc_median, nosc_mc_median, sc_mc_sd, nosc_mc_sd,
                        sc_mc_max, nosc_mc_max, sc_mc_min, nosc_mc_min), 
                   by = "dob_gender", type = "left")
mc_smy[is.na(mc_smy)] <- 0
finalnames <- c(names(mc_smy)[1:5],
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2013, "_op_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2014, "_op_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2015, "_op_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2016, "_op_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2017, "_op_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)))
setcolorder(mc_smy, finalnames)
fwrite(mc_smy,
       file = "output/cost/inference/sc_nosc_pt_op_cost_inference.csv")

## total pc cost
rm(list=ls())
scmc_names <- paste0("sc_fy", 2013:2017, "_pc_cost")
sc_mc <- fread("output/cost/sc_2015_pc_cost_.csv", colClasses = "character")
names(sc_mc)[-(1:3)] <- paste0("sc_", names(sc_mc)[-(1:3)])
sc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
sc_mc[sc_mc == ""] <- 0
sc_mc_o <- sc_mc[, .(dob_gender, birth_dt, GENDER_CODE)] %>% unique()
sc_mc_n <- sc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                 by = dob_gender]
names(sc_mc_n)[-1] <- "sc_N"
sc_mc <- sc_mc[, lapply(.SD, as.numeric), .SDcols = scmc_names, by = "dob_gender"]
sc_mc_mean <- sc_mc[, lapply(.SD, mean), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_mean)[-1] <- paste0(names(sc_mc_mean)[-1], "_mean")
sc_mc_median <- sc_mc[, lapply(.SD, median), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_median)[-1] <- paste0(names(sc_mc_median)[-1], "_median")
sc_mc_sd <- sc_mc[, lapply(.SD, sd), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_sd)[-1] <- paste0(names(sc_mc_sd)[-1], "_sd")
sc_mc_max <- sc_mc[, lapply(.SD, max), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_max)[-1] <- paste0(names(sc_mc_max)[-1], "_max")
sc_mc_min <- sc_mc[, lapply(.SD, min), .SDcols = scmc_names, by = "dob_gender"]
names(sc_mc_min)[-1] <- paste0(names(sc_mc_min)[-1], "_min")

noscmc_names <- paste0("nosc_fy", 2013:2017, "_pc_cost")
nosc_mc <- fread("output/cost/compare_pc_cost_.csv", colClasses = "character")
names(nosc_mc)[-(1:3)] <- paste0("nosc_", names(nosc_mc)[-(1:3)])
nosc_mc[, dob_gender := paste0(birth_dt, GENDER_CODE)]
nosc_mc <- nosc_mc[, -c("birth_dt", "GENDER_CODE")]
nosc_mc[nosc_mc == ""] <- 0
nosc_mc_n <- nosc_mc[, lapply(.SD, uniqueN), .SDcols = "INTERNAL_MEMBER_ID",
                     by = dob_gender]
names(nosc_mc_n)[-1] <- "nosc_N"
nosc_mc <- nosc_mc[, lapply(.SD, as.numeric), .SDcols = noscmc_names, by = "dob_gender"]
nosc_mc_mean <- nosc_mc[, lapply(.SD, mean), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_mean)[-1] <- paste0(names(nosc_mc_mean)[-1], "_mean")
nosc_mc_median <- nosc_mc[, lapply(.SD, median), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_median)[-1] <- paste0(names(nosc_mc_median)[-1], "_median")
nosc_mc_sd <- nosc_mc[, lapply(.SD, sd), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_sd)[-1] <- paste0(names(nosc_mc_sd)[-1], "_sd")
nosc_mc_max <- nosc_mc[, lapply(.SD, max), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_max)[-1] <- paste0(names(nosc_mc_max)[-1], "_max")
nosc_mc_min <- nosc_mc[, lapply(.SD, min), .SDcols = noscmc_names, by = "dob_gender"]
names(nosc_mc_min)[-1] <- paste0(names(nosc_mc_min)[-1], "_min")

mc_smy <- join_all(list(sc_mc_o, sc_mc_n, nosc_mc_n, sc_mc_mean, nosc_mc_mean, 
                        sc_mc_median, nosc_mc_median, sc_mc_sd, nosc_mc_sd,
                        sc_mc_max, nosc_mc_max, sc_mc_min, nosc_mc_min), 
                   by = "dob_gender", type = "left")
mc_smy[is.na(mc_smy)] <- 0
finalnames <- c(names(mc_smy)[1:5],
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2013, "_pc_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2014, "_pc_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2015, "_pc_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2016, "_pc_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)),
                paste0(rep(c("sc_fy", "nosc_fy"), 1), 2017, "_pc_cost", 
                       rep(c("_mean", "_median", "_sd", "_max", "_min"), each = 2)))
setcolorder(mc_smy, finalnames)
fwrite(mc_smy,
       file = "output/cost/inference/sc_nosc_pt_pc_cost_inference.csv")