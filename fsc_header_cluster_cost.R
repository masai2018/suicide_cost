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
need_pkgs <- c("data.table", "bit64", "tools", "touch", "haven", "xlsx",
               "dplyr", "lubridate", "tidyverse", "matrixStats", "berryFunctions")
need.packages(need_pkgs)
source("get_suicide_attempts.R")

fsc <- fread("E:/CT_APCD/Abby/suicide/costs/explore_header_ids/fsc_header_cluster.csv",
             colClasses  = "character")
pt <- fread('output/sc_15_no_sc_13_14.csv', colClasses = 'character',
            select = c("INTERNAL_MEMBER_ID", "birth_dt", 
                       "GENDER_CODE")) %>% unique()
pt[, age := 2015 - as.integer(birth_dt)]
pt <- pt[age < 65 & age > 9]
age_group <- list(c(10:64), c(10:24), c(25:44), c(45:64))
gender_group <- list(c("M", "F"), "M", "F")
pt <- pt[, -"birth_dt"]

sc <- rbindlist(lapply(9:10, function(x){
  dat <- fread(paste0("output/dgx_mc_2015_sc_icd", x, ".csv"), 
        colClasses = "character", 
        select = c("MEDICAL_CLAIM_SERVICE_LINE_ID",
                   # "INTERNAL_MEMBER_ID",
                   # "sc_flag",
                   "ICD_VERSION_IND",
                   "DIAGNOSIS_CODE_10_combined",
                   "DIAGNOSIS_CODE_9_combined"))
  names(dat)[3] <- "DIAGNOSIS_CODE_combined"
  return(dat)
}))
sc <- unique(sc, use.key = FALSE)
fsc <- pt[fsc, on = "INTERNAL_MEMBER_ID"]
fsc <- sc[fsc, on = "MEDICAL_CLAIM_SERVICE_LINE_ID"]



i <- "total"
indir <- paste0("E:/CT_APCD/Sai/intermediate_data/",
                "cost_measure_intermediate_data/", 
                "cost_files_by_year/")
mccost <- rbind(fread(paste0(indir, i,
                             "_", 2016, "_all_ages.csv"),
                      colClasses = "character"),
                fread(paste0(indir, i,
                             "_", 2017, "_all_ages.csv"),
                      colClasses = "character"),
                fread(paste0(indir, i,
                             "_", 2015, "_all_ages.csv"),
                      colClasses = "character"))[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]

names(mccost) <- tolower(names(mccost))

mccost <- mccost[, c("internal_member_id",
                     "first_service_dt",
                     "medical_claim_header_id","allowed_amt")]
names(mccost) <- toupper(names(mccost))
fsc2 <- unique(mccost[, .(MEDICAL_CLAIM_HEADER_ID, ALLOWED_AMT)])[fsc, on = "MEDICAL_CLAIM_HEADER_ID"]
fwrite(fsc2, 
       file = paste0("output/fsc_header_cluster_Sai.csv"))
mccost[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)]
mccost <- mccost[ALLOWED_AMT >= 0]
mccost <- unique(mccost, use.key = FALSE)

mccost <- mccost[fsc, on = "MEDICAL_CLAIM_HEADER_ID"]
mccost <- mccost[, -"i.INTERNAL_MEMBER_ID"]
mccost <- unique(mccost, use.key = FALSE)
uniqueN(mccost$INTERNAL_MEMBER_ID)
mccost <- mccost[, .(INTERNAL_MEMBER_ID,
                      MEDICAL_CLAIM_HEADER_ID,
                      ALLOWED_AMT,
                      GENDER_CODE,
                      age, sc_code, header_type)]
mccost <- unique(mccost, use.key = FALSE)
mccost1 <- mccost[sc_code == TRUE & header_type %in% c("0", "3", "4")] %>% unique(use.key = FALSE)
mccost2 <- mccost[header_type %in% c("0", "3", "4")] %>% unique(use.key = FALSE)
mccost3 <- mccost[sc_code == TRUE] %>% unique(use.key = FALSE)


smy1 <- mccost1[, lapply(.SD, sum), .SDcol = "ALLOWED_AMT", by = "INTERNAL_MEMBER_ID"]
smy2 <- mccost2[, lapply(.SD, sum), .SDcol = "ALLOWED_AMT", by = "INTERNAL_MEMBER_ID"]
smy3 <- mccost3[, lapply(.SD, sum), .SDcol = "ALLOWED_AMT", by = "INTERNAL_MEMBER_ID"]

smy <- smy3[pt, on = "INTERNAL_MEMBER_ID"]
smy[is.na(smy)] <- 0
out <- data.table()
for (gender in gender_group){
  for(ages in age_group){
    tmp1 <- smy[age %in% ages & GENDER_CODE %in% gender]
    tmp.out <- c(age = paste0(min(ages), "-", max(ages)),
                 gender = paste0(gender[1], "-", gender[length(gender)]),
                 summary(tmp1$ALLOWED_AMT)[4],
                 sd = sd(tmp1$ALLOWED_AMT), 
                 summary(tmp1$ALLOWED_AMT)[1], 
                 summary(tmp1$ALLOWED_AMT)[6], 
                 summary(tmp1$ALLOWED_AMT)[2],  
                 summary(tmp1$ALLOWED_AMT)[3], 
                 summary(tmp1$ALLOWED_AMT)[5], 
                 quantile(tmp1$ALLOWED_AMT, 0.95), 
                 quantile(tmp1$ALLOWED_AMT, 0.99))
    out <- rbind(out, data.table(t(tmp.out)))
  }
}
out <- rbind(out[1:5, ], out[9, ], 
             out[6:8, ], out[10:12, ])
out <- insertRows(out, c(2, 6, 9, 13), NA)
fwrite(out, file = paste0("fsc_header_cluster_3", ".csv"))





