if(Sys.info()[4] %in% c("LAZ-ID1",
                        "LAZ-ID2",
                        "LAZ-DEID1",
                        "LAZ-DEID2")){
  setwd("E:/CT_APCD/Sai/suicide_cost")
}
library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats")
need.packages(need_pkgs)

elig_pt <- fread(paste0("E:/CT_APCD/shared/intermediate_data/", 
                        "APCD_modified/eligibility/By_Fiscal_Year/", 
                        "medical_elig.csv"),
                 colClasses = "character") %>% unique(use.key = FALSE)
elig_pt[, NumGapsLE45days := as.numeric(NumGapsLE45days)]
mcpt <- rbindlist(lapply(2013:2017, function(yr){
  fread(paste0("E:/CT_APCD/shared/intermediate_data/", 
               "APCD_modified/medicare_medicare_ad_patients/", 
               "fy", yr, "all_ages.csv"),
        colClasses = "character")
})) %>% unique()
elig_pt <- elig_pt[!mcpt, on = "INTERNAL_MEMBER_ID"]


for(yr in 2013:2017){
  cat(paste0("begin ", yr, " at ", Sys.time(), "\n"))
  eligname <- paste0("elig", yr)
  mcname <- paste0("mc", yr)
  tmp1 <- elig_pt[FiscalYR == yr &
                   NumGapsGT45days == "0" & 
                   NumGapsLE45days <= 1 &
                   Elig_Sep30 == "1", 
                 unique(INTERNAL_MEMBER_ID)]
  tmp2 <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/", 
                       "cost_measure_intermediate_data/", 
                       "medical_fiscalyear_", yr, ".csv"), 
                colClasses = "character",
                select = "INTERNAL_MEMBER_ID")[
                  !INTERNAL_MEMBER_ID %in% mcpt$INTERNAL_MEMBER_ID
                  ][, unique(INTERNAL_MEMBER_ID)]
  assign(eligname, tmp1)
  assign(mcname, tmp2)
  rm(tmp1, tmp2)
  gc()
  cat(paste0(yr, " done at ", Sys.time(), "\n"))
}
elig_ce <- Reduce(intersect, list(elig2013, elig2014, elig2015,
                                  elig2016, elig2017))
mc_ce <- Reduce(intersect, list(mc2013, mc2014, mc2015,
                                mc2016, mc2017))


# 1) Persons who have at least one medical claim in each year 
# from 2013 to 2017 and have 1 or more suicide attempts 
# in 2015 (and not in 2013 or 2014). Also, note that it’s “medical” 
# claim and not “medicare” claim. Also note that this is just one 
# number for 2013-2017.

for(yr in 2015:2013){
scpt <- fread(paste0("output/sc_pt_com_", yr, ".csv"), select = "INTERNAL_MEMBER_ID",
                colClasses = "character") %>% unique()
  scpt <- scpt[INTERNAL_MEMBER_ID %in% mc_ce]
  fname <- paste0("scpt", yr)
  assign(fname, scpt)
}
pt1 <- scpt2015[!INTERNAL_MEMBER_ID %in% scpt2013$INTERNAL_MEMBER_ID &
                 !INTERNAL_MEMBER_ID %in% scpt2014$INTERNAL_MEMBER_ID
               ]
uniqueN(pt1$INTERNAL_MEMBER_ID)   # 866

# 2) Persons with medical data who have continuous eligibility 
# for medical benefits from 2013 to 2017 and have 1 or more suicide 
# attempts in 2015 (and not in 2013 or 2014). Also, note that it’s
# “medical” claim and not “medicare” claim. Also note that this
# is just one number for 2013-2017.

for(yr in 2015:2013){
  scpt <- fread(paste0("output/sc_pt_com_", yr, ".csv"), select = "INTERNAL_MEMBER_ID",
                colClasses = "character") %>% unique()
  scpt <- scpt[INTERNAL_MEMBER_ID %in% elig_ce]
  fname <- paste0("scpt", yr)
  assign(fname, scpt)
}
pt2 <- scpt2015[!INTERNAL_MEMBER_ID %in% scpt2013$INTERNAL_MEMBER_ID &
                  !INTERNAL_MEMBER_ID %in% scpt2014$INTERNAL_MEMBER_ID]
uniqueN(pt2$INTERNAL_MEMBER_ID)   # 702

# 3) Persons who have at least one medical claim in each year from 
# 2013 to 2017 and have no suicide attempts in 2015, 2016 or 2017. 
# Also, note that it’s “medical” claim and not “medicare” claim.
# Also note that this is just one number for 2013-2017.

for(yr in 2015:2013){
  scpt <- fread(paste0("output/sc_pt_com_", yr, ".csv"), select = "INTERNAL_MEMBER_ID",
                colClasses = "character") %>% unique()
  fname <- paste0("scpt", yr)
  assign(fname, scpt)
}
pt3 <- mc_ce[!mc_ce %in% unique(rbind(scpt2015,
                                      scpt2014,
                                      scpt2013)$INTERNAL_MEMBER_ID)]
uniqueN(pt3)       # 437508

# 4) Persons with medical data who have continuous eligibility for medical 
# benefits from 2013 to 2017 and have no suicide attempts in 2015, 2016 or 
# 2017. Also, note that it’s “medical” claim and not “medicare” claim. 
# Also note that this is just one number for 2013-2017.

for(yr in 2015:2013){
  scpt <- fread(paste0("output/sc_pt_com_", yr, ".csv"), select = "INTERNAL_MEMBER_ID",
                colClasses = "character") %>% unique()
  fname <- paste0("scpt", yr)
  assign(fname, scpt)
}
pt4 <- elig_ce[!elig_ce %in% unique(rbind(scpt2015,
                                      scpt2014,
                                      scpt2013)$INTERNAL_MEMBER_ID)]
uniqueN(pt4)       # 428275

## intermidiate file
mc_pt <- data.table(internal_member_id = unique(c(mc_ce, elig_ce)),
                    at_least_one_medical_claim = 0,
                    continuous_elig = 0,
                    suicide_1 = 0,
                    suicide_2 = 0,
                    no_suicide_1 = 0,
                    no_suicide_2 = 0)
mc_pt[internal_member_id %in% mc_ce, at_least_one_medical_claim := 1]
mc_pt[internal_member_id %in% elig_ce, continuous_elig := 1]
mc_pt[internal_member_id %in% pt1$INTERNAL_MEMBER_ID, suicide_1 := 1]
mc_pt[internal_member_id %in% pt2$INTERNAL_MEMBER_ID, suicide_2 := 1]
mc_pt[internal_member_id %in% pt3, no_suicide_1 := 1]
mc_pt[internal_member_id %in% pt4, no_suicide_2 := 1]
table(mc_pt$at_least_one_medical_claim)
table(mc_pt$continuous_elig)
table(mc_pt$suicide_1)
table(mc_pt$suicide_2)
table(mc_pt$no_suicide_1)
table(mc_pt$no_suicide_2)
fwrite(mc_pt, file = "output/no_of_pt.csv")
