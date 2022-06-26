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
source("get_suicide_attempts.R")

# 
# pt_all <- fread(paste0("output/sc_15pt.csv"), colClasses = "character")
# pt_all <- fread(paste0("output/sc_15_no_sc_13_14_pt.csv"), colClasses = "character")
# pt_no <- fread(paste0("output/no_sc_2013_2015_pt.csv"), colClasses = "character")

# elig_pt <- data.table(read_sas("E:/CT_APCD/Beth/data4/eligibility_4.sas7bdat"))
# elig_pt[, INTERNAL_MEMBER_ID := as.character(INTERNAL_MEMBER_ID)]
# elig_pt <- elig_pt[, c("INTERNAL_MEMBER_ID",
#                        "birth_dt")] %>% unique(use.key = FALSE)
medicare_ad_code <- c('HN', 'AB', 'MA', 'MD','12', '43', '14', '16', '15',
                      'CP', 'MB', 'MP', '41', '13', '47', '42', 'ZZ',
                      NULL)

elig_pt0 <- fread(paste0("E:/CT_APCD/shared/intermediate_data/",
                         "APCD_modified/eligibility/By_Fiscal_Year/",
                         "PHARMACY_ELIG_GapsByFY_2013_17.csv"),
                  colClasses = "character") %>% unique(use.key = FALSE)
elig_pt0[, `:=`(NumGapsLE45days = as.numeric(NumGapsLE45days),
                NumGapsGT45days = as.numeric(NumGapsGT45days),
                Elig_Sep30 = as.numeric(Elig_Sep30))]

elig_pt3 <- data.table(read_sas("E:/CT_APCD/Beth/data4/eligibility_4.sas7bdat"))
elig_pt3[, INTERNAL_MEMBER_ID := as.character(INTERNAL_MEMBER_ID)]
elig_pt3 <- elig_pt3[ymd(eligibility_start_dt) < ymd("2017-10-01") & 
                       ymd(eligibility_end_dt) > ymd("2012-09-30") & 
                       COVERAGE_CLASS == "PHARMACY"]

mcpt <- elig_pt3[SUBMITTER_ID == "15227" |
                   PRODUCT_CODE %in% medicare_ad_code,
                 .(INTERNAL_MEMBER_ID)] %>% unique()


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

elig_ce <- Reduce(base::intersect, list(elig2013, elig2014, elig2015,
                                        elig2016, elig2017))
mc_ce <- Reduce(base::intersect, list(mc2013, mc2014, mc2015,
                                      mc2016, mc2017))
pt <- Reduce(base::intersect, list(elig_ce, mc_ce)) %>% unique()
pt <- data.table(INTERNAL_MEMBER_ID = pt)
elig_pt4 <- unique(elig_pt3[, .(INTERNAL_MEMBER_ID, GENDER_CODE, birth_dt)])
pt2 <- elig_pt4[pt, on = "INTERNAL_MEMBER_ID"]
pt2[, age := 2015 - as.integer(birth_dt)]
pt3 <- pt2[age < 65 & age > 9]
uniqueN(pt3)
fwrite(pt3, file = paste0("output/pharmacy_pt_all.csv"))


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
noscpt <- pt3[!INTERNAL_MEMBER_ID %in% scpt13_15$INTERNAL_MEMBER_ID]
noscpt <- unique(elig_pt3[, .(INTERNAL_MEMBER_ID,
                    birth_dt,
                    GENDER_CODE)])[noscpt,
                                   on = "INTERNAL_MEMBER_ID"]
noscpt2 <- noscpt[, .(INTERNAL_MEMBER_ID,
           birth_dt,
           GENDER_CODE)]
noscpt2[, age := 2015 - as.integer(birth_dt)]
fwrite(noscpt2,
       file = paste0("output/no_sc_2013_2015_pt_ph.csv"))

sc_15_no_sc_13_14 <- scpt2015[!INTERNAL_MEMBER_ID %in% unique(c(scpt2013$INTERNAL_MEMBER_ID,
                                                                scpt2014$INTERNAL_MEMBER_ID))]
sc_15_no_sc_13_14 <- sc_15_no_sc_13_14[INTERNAL_MEMBER_ID %in% pt3$INTERNAL_MEMBER_ID]
sc_15_no_sc_13_14 <- unique(elig_pt3[, .(INTERNAL_MEMBER_ID,
                                         birth_dt,
                                         GENDER_CODE)])[sc_15_no_sc_13_14,
                                                        on = "INTERNAL_MEMBER_ID"]



sc_15_no_sc_13_14[, age := 2015 - as.integer(birth_dt)]
fwrite(sc_15_no_sc_13_14,
       file = paste0("output/sc_15_no_sc_13_14_ph.csv"))



pt <- unique(fread(paste0("output/sc_15_no_sc_13_14_ph.csv"), colClasses  = "character")[, c("INTERNAL_MEMBER_ID",
                                                                                             "GENDER_CODE", "age")])


pt_no <- unique(fread("output/no_sc_2013_2015_pt_ph.csv", colClasses = "character"))

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
fwrite(dem_smy, file = "output/dem_pharmacy.csv")

