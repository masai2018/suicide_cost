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



pt <- fread('output/sc_15_no_sc_13_14.csv', colClasses = 'character',
            select = c("INTERNAL_MEMBER_ID", "birth_dt", 
                       "GENDER_CODE")) %>% unique()
pt[, age := 2015 - as.integer(birth_dt)]
pt <- pt[age < 65 & age > 9]
age_group <- list(c(10:64), c(10:24), c(25:44), c(45:64))
gender_group <- list(c("M", "F"), "M", "F")

sc <- rbindlist(lapply(9:10, function(x){
  fread(paste0("output/dgx_mc_2015_sc_icd", x, ".csv"), 
        colClasses = "character", 
        select = c("MEDICAL_CLAIM_SERVICE_LINE_ID",
                   "INTERNAL_MEMBER_ID",
                   "sc_flag"))[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID &
                                 sc_flag  == "1"]
}))
sc <- unique(sc, use.key = FALSE)

sc2 <- fread('output/all_claims_SA_non_SA_20220131.csv',  colClasses = 'character',
             select = c("MEDICAL_CLAIM_SERVICE_LINE_ID", 
                        "MEDICAL_CLAIM_HEADER_ID", "first_service_dt",
                        "sc_15_no_sc_13_14"))[
                          sc_15_no_sc_13_14 == "1"][, -c("sc_15_no_sc_13_14")]
sc2 <- unique(sc2, use.key = FALSE)
sc <- sc2[, - "MEDICAL_CLAIM_HEADER_ID"][sc, on = "MEDICAL_CLAIM_SERVICE_LINE_ID"]
sc[, first_service_dt := mdy(first_service_dt )]

hd_ol <- fread("E:/CT_APCD/Abby/suicide/costs/explore_header_ids/fsc_header_cluster.csv",
               colClasses = "character", 
               select = c("MEDICAL_CLAIM_SERVICE_LINE_ID", "MEDICAL_CLAIM_HEADER_ID",
                          "first_service_dt"))
sc2o <- rbind(sc2, hd_ol) %>% unique(use.key = FALSE)

i <- "total"
indir <- paste0("E:/CT_APCD/Sai/intermediate_data/",
                "cost_measure_intermediate_data/", 
                "cost_files_by_year/")
## no overlap
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
mccost[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)]
mccost <- mccost[ALLOWED_AMT >= 0]
mccost <- unique(mccost, use.key = FALSE)
mccost <- sc2[, -"first_service_dt"][mccost, on = "MEDICAL_CLAIM_HEADER_ID"]
sc3 <- mccost[, lapply(.SD, min), .SDcol = "FIRST_SERVICE_DT", by = "INTERNAL_MEMBER_ID"]
mccost <- mccost[sc3, on = c("INTERNAL_MEMBER_ID", "FIRST_SERVICE_DT")]
mccost <- mccost[, -"MEDICAL_CLAIM_SERVICE_LINE_ID"]
mccost <- unique(mccost, use.key = FALSE)
uniqueN(mccost$INTERNAL_MEMBER_ID)
smy <- mccost[, lapply(.SD, sum), .SDcol = "ALLOWED_AMT", by = "INTERNAL_MEMBER_ID"]

smy <- smy[pt, on = "INTERNAL_MEMBER_ID"]
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
fwrite(out, file = paste0("sc_current_header_id_cost", ".csv"))


## overlap
mccost_o <- rbind(fread(paste0(indir, i,
                             "_", 2016, "_all_ages.csv"),
                      colClasses = "character"),
                fread(paste0(indir, i,
                             "_", 2017, "_all_ages.csv"),
                      colClasses = "character"),
                fread(paste0(indir, i,
                             "_", 2015, "_all_ages.csv"),
                      colClasses = "character"))[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]
lsd <- rbindlist(lapply(2015:2017, function(yr){
  unique(fread(paste0("E:/CT_APCD/Sai/intermediate_data/cost_measure_intermediate_data/medical_fiscalyear_", yr, ".csv"),
        select = c("INTERNAL_MEMBER_ID", "first_service_dt", "last_service_dt",
                   MEDICAL_CLAIM_HEADER_ID))[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID])
}))
  
  
  
  rbind(fread(paste0(indir, i,
                               "_", 2016, "_all_ages.csv"),
                        colClasses = "character"),
                  fread(paste0(indir, i,
                               "_", 2017, "_all_ages.csv"),
                        colClasses = "character"),
                  fread(paste0(indir, i,
                               "_", 2015, "_all_ages.csv"),
                        colClasses = "character"))[INTERNAL_MEMBER_ID %in% pt$INTERNAL_MEMBER_ID]




names(mccost_o) <- tolower(names(mccost_o))

mccost <- mccost_o[, c("internal_member_id",
                     "first_service_dt",
                     "medical_claim_header_id","allowed_amt")]
names(mccost) <- toupper(names(mccost))
mccost[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)]
mccost <- mccost[ALLOWED_AMT >= 0]
mccost <- unique(mccost, use.key = FALSE)
mccost <- sc2o[, -"first_service_dt"][mccost, on = "MEDICAL_CLAIM_HEADER_ID"]
sc3o1 <- mccost[, lapply(.SD, min), .SDcol = "FIRST_SERVICE_DT", by = "INTERNAL_MEMBER_ID"]
mccost2 <- mccost[sc3o1, on = c("INTERNAL_MEMBER_ID", "FIRST_SERVICE_DT")]
mccost3 <- mccost[MEDICAL_CLAIM_HEADER_ID %in% hd_ol$MEDICAL_CLAIM_HEADER_ID]
mccost4 <- rbind(mccost2, mccost3) %>% unique(use.key = FALSE)
mccost4 <- mccost4[, -"MEDICAL_CLAIM_SERVICE_LINE_ID"]
mccost4 <- unique(mccost4, use.key = FALSE)
uniqueN(mccost4$INTERNAL_MEMBER_ID)
smy <- mccost4[, lapply(.SD, sum), .SDcol = "ALLOWED_AMT", by = "INTERNAL_MEMBER_ID"]

smy <- smy[pt, on = "INTERNAL_MEMBER_ID"]
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
fwrite(out, file = paste0("sc_current_header_id_cost_overlap", ".csv"))
