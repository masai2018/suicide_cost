if(Sys.info()[4] %in% c("LAZ-ID1",
                        "LAZ-ID2",
                        "LAZ-DEID1",
                        "LAZ-DEID2")){
  setwd("E:/CT_APCD/Sai/suicide_cost")
}
library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", "haven",
               "dplyr", "lubridate", "tidyverse", "matrixStats")
need.packages(need_pkgs)
medicare_ad_code <- c('HN', 'AB', 'MA', 'MD','12', '43', '14', '16', '15',
                      'CP', 'MB', 'MP', '41', '13', '47', '42', 'ZZ',
                      NULL)

elig_pt0 <- fread(paste0("E:/CT_APCD/shared/intermediate_data/",
                        "APCD_modified/eligibility/By_Fiscal_Year/",
                        "medical_elig.csv"),
                 colClasses = "character") %>% unique(use.key = FALSE)
elig_pt0[, `:=`(NumGapsLE45days = as.numeric(NumGapsLE45days),
                NumGapsGT45days = as.numeric(NumGapsGT45days),
                Elig_Sep30 = as.numeric(Elig_Sep30))]
# elig_pt2 <- fread(paste0("E:/CT_APCD/Beth/data4/Eligibility/",
#                          "By_Fiscal_Year/MEDICAL_ELIG_ALLRECS_FY_2012_17.csv"),
#                   colClasses = "character")
# elig_pt2[, `:=`(gapLE45days = as.numeric(gapLE45days),
#                 gapGT45days = as.numeric(gapGT45days),
#                 Elig_Sep30 = as.numeric(Elig_Sep30))]
elig_pt3 <- data.table(read_sas("E:/CT_APCD/Beth/data4/eligibility_4.sas7bdat"))
elig_pt3[, INTERNAL_MEMBER_ID := as.character(INTERNAL_MEMBER_ID)]
elig_pt3 <- elig_pt3[ymd(eligibility_start_dt) < ymd("2017-10-01") & 
                       ymd(eligibility_end_dt) > ymd("2012-09-30") & 
                       COVERAGE_CLASS == "MEDICAL"]

# mcpt <- elig_pt2[SUBMITTER_ID == "15227" |
#                    PRODUCT_CODE %in% medicare_ad_code,
#                  .(INTERNAL_MEMBER_ID)] %>% unique()
mcpt <- elig_pt3[SUBMITTER_ID == "15227" |
                   PRODUCT_CODE %in% medicare_ad_code,
                 .(INTERNAL_MEMBER_ID)] %>% unique()

# mcpt2 <- rbindlist(lapply(2013:2017, function(yr){
#   fread(paste0("E:/CT_APCD/shared/intermediate_data/",
#                "APCD_modified/medicare_medicare_ad_patients/",
#                "fy", yr, "all_ages.csv"),
#         colClasses = "character")
# })) %>% unique()
elig_pt <- elig_pt0[!INTERNAL_MEMBER_ID %in% mcpt$INTERNAL_MEMBER_ID]


for(yr in 2013:2017){
  cat(paste0("begin ", yr, " at ", Sys.time(), "\n"))
  eligname <- paste0("elig", yr)
  mcname <- paste0("mc", yr)
  tmp0 <- elig_pt[FiscalYR == yr]
  tmp1 <- tmp0[NumGapsGT45days == 0 & 
                 NumGapsLE45days <= 1 &
                 Elig_Sep30 == 1, 
               .(INTERNAL_MEMBER_ID)] %>% unique()
  # tmp1 <- tmp0[!tmp, on = "INTERNAL_MEMBER_ID"]
  # tmp11 <- tmp0[!INTERNAL_MEMBER_ID %in% tmp$INTERNAL_MEMBER_ID]
  tmp1 <- tmp1[, unique(INTERNAL_MEMBER_ID)]
  tmp2 <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
                       "cost_measure_intermediate_data/",
                       "medical_fiscalyear_", yr, ".csv"),
                colClasses = "character",
                select = "INTERNAL_MEMBER_ID")[
                  !INTERNAL_MEMBER_ID %in% mcpt$INTERNAL_MEMBER_ID
                  ][, unique(INTERNAL_MEMBER_ID)]
  assign(eligname, tmp1)
  assign(mcname, tmp2)
  rm(tmp0, tmp1, tmp2)
  gc()
  cat(paste0(yr, " done at ", Sys.time(), "\n"))
}


pt <- data.table(INTERNAL_MEMBER_ID = elig2013)
elig_pt4 <- unique(elig_pt3[, .(INTERNAL_MEMBER_ID, GENDER_CODE, birth_dt)])
pt2 <- elig_pt4[pt, on = "INTERNAL_MEMBER_ID"]
# fwrite(pt2, file = paste0("output/pt_all.csv"))

for(yr in 2015:2013){
  scpt9 <- fread(paste0("output/dgx_mc_", yr, "_sc_icd9.csv"),
                 select = c("INTERNAL_MEMBER_ID", "sc_flag"),
                 colClasses = "character")[sc_flag > 0][, -"sc_flag"] %>% unique()
  scpt10 <- fread(paste0("output/dgx_mc_", yr, "_sc_icd10.csv"),
                 select = c("INTERNAL_MEMBER_ID", "sc_flag"),
                 colClasses = "character")[sc_flag > 0][, -"sc_flag"] %>% unique()
  scpt <- rbind(scpt9, scpt10) %>% unique()
  rm(scpt9, scpt10)
  gc()
  fname <- paste0("scpt", yr)
  assign(fname, scpt)
}
scpt13_15 <- rbind(scpt2013, scpt2014, scpt2015) %>% unique()
noscpt <- pt[!INTERNAL_MEMBER_ID %in% scpt13_15$INTERNAL_MEMBER_ID]
fwrite(noscpt,
       file = paste0("output/no_sc_2013_2015_pt.csv"))

for(yr in 2015:2013){
  scpt9 <- fread(paste0("output/dgx_mc_", yr, "_sc_icd9.csv"),
                 colClasses = "character")[sc_flag > 0][, -c("sc_flag",
                                                             "DIAGNOSIS_CODE")] %>% unique()
  scpt10 <- fread(paste0("output/dgx_mc_", yr, "_sc_icd10.csv"),
                  colClasses = "character")[sc_flag > 0][, -c("sc_flag",
                                                              "DIAGNOSIS_CODE")] %>% unique()
  setnames(scpt9, "DIAGNOSIS_CODE_9_combined", "DIAGNOSIS_CODE_combined")
  setnames(scpt10, "DIAGNOSIS_CODE_10_combined", "DIAGNOSIS_CODE_combined")
  scpt <- rbind(scpt9, scpt10) %>% unique()
  rm(scpt9, scpt10)
  gc()
  fname <- paste0("scpt", yr)
  assign(fname, scpt)
}
sc_15_no_sc_13_14 <- scpt2015[!INTERNAL_MEMBER_ID %in% unique(c(scpt2013$INTERNAL_MEMBER_ID,
                                                                scpt2014$INTERNAL_MEMBER_ID))]
sc_15_no_sc_13_14 <- sc_15_no_sc_13_14[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
sc_15_no_sc_13_14 <- unique(elig_pt3[, .(INTERNAL_MEMBER_ID,
                                         birth_dt,
                                         GENDER_CODE)])[sc_15_no_sc_13_14,
                                                        on = "INTERNAL_MEMBER_ID"]
fwrite(sc_15_no_sc_13_14,
       file = paste0("output/sc_15_no_sc_13_14.csv"))
scpt2015 <- scpt2015[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID
                     ][, .(INTERNAL_MEMBER_ID)] %>% unique()
fwrite(scpt2015,
       file = paste0("output/sc_15pt.csv"))

# 
# 
# # 1) Persons who have at least one medical claim in each year 
# # from 2013 to 2017 and have 1 or more suicide attempts 
# # in 2015 (and not in 2013 or 2014). Also, note that it’s “medical” 
# # claim and not “medicare” claim. Also note that this is just one 
# # number for 2013-2017.
# 
# for(yr in 2015:2013){
#   scpt9 <- fread(paste0("output/dgx_mc_", yr, "_sc_icd9_small.csv"), 
#                  select = c("INTERNAL_MEMBER_ID", "sc_flag"),
#                  colClasses = "character")[sc_flag > 0][, -"sc_flag"] %>% unique()
#   scpt10 <- fread(paste0("output/dgx_mc_", yr, "_sc_icd10_small.csv"), 
#                  select = c("INTERNAL_MEMBER_ID", "sc_flag"),
#                  colClasses = "character")[sc_flag > 0][, -"sc_flag"] %>% unique()
#   scpt <- rbind(scpt9, scpt10) %>% unique()
#   rm(scpt9, scpt10)
#   gc()
#   scpt <- scpt[INTERNAL_MEMBER_ID %in% mc_ce]
#   fname <- paste0("scpt", yr)
#   assign(fname, scpt)
# }
# pt1 <- scpt2015[!INTERNAL_MEMBER_ID %in% scpt2013$INTERNAL_MEMBER_ID &
#                  !INTERNAL_MEMBER_ID %in% scpt2014$INTERNAL_MEMBER_ID
#                ]
# uniqueN(pt1$INTERNAL_MEMBER_ID)   # 839
# 
# # 2) Persons with medical data who have continuous eligibility 
# # for medical benefits from 2013 to 2017 and have 1 or more suicide 
# # attempts in 2015 (and not in 2013 or 2014). Also, note that it’s
# # “medical” claim and not “medicare” claim. Also note that this
# # is just one number for 2013-2017.
# 
# # for(yr in 2015:2013){
# #   scpt <- fread(paste0("output/sc_pt_", yr, ".csv"), select = "INTERNAL_MEMBER_ID",
# #                 colClasses = "character") %>% unique()
# #   scpt <- scpt[INTERNAL_MEMBER_ID %in% elig_ce]
# #   fname <- paste0("scpt", yr)
# #   assign(fname, scpt)
# # }
# scpt2015_10 <- fread("output/dgx_mc_2015_sc_gem_icd10.csv", 
#                    colClasses = "character", 
#                    select = "INTERNAL_MEMBER_ID")
# scpt2015_9 <- fread("output/dgx_mc_2015_sc_gem_icd9.csv", 
#                     colClasses = "character",
#                     select = "INTERNAL_MEMBER_ID")
# scpt2015 <- unique(c(scpt2015_10$INTERNAL_MEMBER_ID,
#                      scpt2015_9$INTERNAL_MEMBER_ID))
# pt2 <- scpt2015[!INTERNAL_MEMBER_ID %in% scpt2013$INTERNAL_MEMBER_ID &
#                   !INTERNAL_MEMBER_ID %in% scpt2014$INTERNAL_MEMBER_ID]
# uniqueN(pt2$INTERNAL_MEMBER_ID)   # 676
# 
# # 3) Persons who have at least one medical claim in each year from 
# # 2013 to 2017 and have no suicide attempts in 2015, 2016 or 2017. 
# # Also, note that it’s “medical” claim and not “medicare” claim.
# # Also note that this is just one number for 2013-2017.
# 
# for(yr in 2015:2013){
#   scpt <- fread(paste0("output/sc_pt_", yr, ".csv"), select = "INTERNAL_MEMBER_ID",
#                 colClasses = "character") %>% unique()
#   fname <- paste0("scpt", yr)
#   assign(fname, scpt)
# }
# pt3 <- mc_ce[!mc_ce %in% unique(rbind(scpt2015,
#                                       scpt2014,
#                                       scpt2013)$INTERNAL_MEMBER_ID)]
# uniqueN(pt3)       # 422675
# 
# # 4) Persons with medical data who have continuous eligibility for medical 
# # benefits from 2013 to 2017 and have no suicide attempts in 2015, 2016 or 
# # 2017. Also, note that it’s “medical” claim and not “medicare” claim. 
# # Also note that this is just one number for 2013-2017.
# 
# for(yr in 2015:2013){
#   scpt <- fread(paste0("output/sc_pt_", yr, ".csv"), select = "INTERNAL_MEMBER_ID",
#                 colClasses = "character") %>% unique()
#   fname <- paste0("scpt", yr)
#   assign(fname, scpt)
# }
# pt4 <- elig_ce[!elig_ce %in% unique(rbind(scpt2015,
#                                       scpt2014,
#                                       scpt2013)$INTERNAL_MEMBER_ID)]
# uniqueN(pt4)       # 395578
# 
# ## intermidiate file
# mc_pt <- data.table(internal_member_id = unique(c(mc_ce, elig_ce)),
#                     at_least_one_medical_claim = 0,
#                     continuous_elig = 0,
#                     suicide_1 = 0,
#                     suicide_2 = 0,
#                     no_suicide_1 = 0,
#                     no_suicide_2 = 0)
# mc_pt[internal_member_id %in% mc_ce, at_least_one_medical_claim := 1]
# mc_pt[internal_member_id %in% elig_ce, continuous_elig := 1]
# mc_pt[internal_member_id %in% pt1$INTERNAL_MEMBER_ID, suicide_1 := 1]
# mc_pt[internal_member_id %in% pt2$INTERNAL_MEMBER_ID, suicide_2 := 1]
# mc_pt[internal_member_id %in% pt3, no_suicide_1 := 1]
# mc_pt[internal_member_id %in% pt4, no_suicide_2 := 1]
# table(mc_pt$at_least_one_medical_claim)
# table(mc_pt$continuous_elig)
# table(mc_pt$suicide_1)
# table(mc_pt$suicide_2)
# table(mc_pt$no_suicide_1)
# table(mc_pt$no_suicide_2)
# fwrite(mc_pt, file = "output/no_of_pt.csv")
