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

for(yr in 2015:2017){
  scpt <- fread(paste0("output/sc_pt_com_", yr, ".csv"), select = "INTERNAL_MEMBER_ID",
                colClasses = "character") %>% unique()
  fname <- paste0("scpt", yr)
  assign(fname, scpt)
}
pt3 <- mc_ce[!mc_ce %in% unique(rbind(scpt2015,
                                      scpt2016,
                                      scpt2017)$INTERNAL_MEMBER_ID)]
uniqueN(pt3)       # 429844

# 4) Persons with medical data who have continuous eligibility for medical 
# benefits from 2013 to 2017 and have no suicide attempts in 2015, 2016 or 
# 2017. Also, note that it’s “medical” claim and not “medicare” claim. 
# Also note that this is just one number for 2013-2017.

for(yr in 2015:2017){
  scpt <- fread(paste0("output/sc_pt_com_", yr, ".csv"), select = "INTERNAL_MEMBER_ID",
                colClasses = "character") %>% unique()
  fname <- paste0("scpt", yr)
  assign(fname, scpt)
}
pt4 <- elig_ce[!elig_ce %in% unique(rbind(scpt2015,
                                      scpt2016,
                                      scpt2017)$INTERNAL_MEMBER_ID)]
uniqueN(pt4)       # 421953
