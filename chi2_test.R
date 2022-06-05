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
pt[age >= 10 & age <= 24, age_group := "10-24"]
pt[age >= 25 & age <= 44, age_group := "25-44"]
pt[age >= 45 & age <= 64, age_group := "45-64"]
pt_no[, age := 2015 - as.integer(birth_dt)]
pt_no <- pt_no[age < 65 & age > 9]
pt_no[age >= 10 & age <= 24, age_group := "10-24"]
pt_no[age >= 25 & age <= 44, age_group := "25-44"]
pt_no[age >= 45 & age <= 64, age_group := "45-64"]

pt_smy1 <- pt[, .N, by = "age_group"]
setnames(pt_smy1, "N", "sc")
pt_no_smy1 <- pt_no[, .N, by = "age_group"]
setnames(pt_no_smy1, "N", "nosc")
tb1 <- t(cbind(pt_smy1, pt_no_smy1[, 2])[, 2:3])
chisq.test(as.matrix(tb1))
# Pearson's Chi-squared test
# 
# data:  as.matrix(tb1)
# X-squared = 249.3, df = 2, p-value < 2.2e-16

pt_smy2 <- pt[, .N, by = "GENDER_CODE"]
setnames(pt_smy2, "N", "sc")
pt_no_smy2 <- pt_no[, .N, by = "GENDER_CODE"]
setnames(pt_no_smy2, "N", "nosc")
tb2 <- t(cbind(pt_smy2, pt_no_smy2[, 2])[, 2:3])
chisq.test(as.matrix(tb2))
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  as.matrix(tb2)
# X-squared = 85.972, df = 1, p-value < 2.2e-16

pt_smy3 <- pt[GENDER_CODE == "F", .N, by = "age_group"]
setnames(pt_smy3, "N", "sc")
pt_no_smy3 <- pt_no[GENDER_CODE == "F", .N, by = "age_group"]
setnames(pt_no_smy3, "N", "nosc")
tb3 <- t(cbind(pt_smy3, pt_no_smy3[, 2])[, 2:3])
chisq.test(as.matrix(tb3))
# Pearson's Chi-squared test
# 
# data:  as.matrix(tb3)
# X-squared = 160.72, df = 2, p-value < 2.2e-16

pt_smy4 <- pt[GENDER_CODE == "M", .N, by = "age_group"]
setnames(pt_smy4, "N", "sc")
pt_no_smy4 <- pt_no[GENDER_CODE == "M", .N, by = "age_group"]
setnames(pt_no_smy4, "N", "nosc")
tb4 <- t(cbind(pt_smy4, pt_no_smy4[, 2])[, 2:3])
chisq.test(as.matrix(tb4))
# Pearson's Chi-squared test
# 
# data:  as.matrix(tb4)
# X-squared = 50.612, df = 2, p-value = 1.023e-11


