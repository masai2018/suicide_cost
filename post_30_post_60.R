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
                          sc_15_no_sc_13_14 == "1" &
                            mdy(first_service_dt) > "2014-09-30" &
                            mdy(first_service_dt) < "2015-12-01"
                        ][, -c("sc_15_no_sc_13_14")]
sc2 <- unique(sc2, use.key = FALSE)
sc <- sc2[, - "MEDICAL_CLAIM_HEADER_ID"][sc, on = "MEDICAL_CLAIM_SERVICE_LINE_ID"]
sc[, first_service_dt := mdy(first_service_dt )]
sc_min <- sc[, lapply(.SD, min),
             .SDcols = "first_service_dt", by = "INTERNAL_MEMBER_ID"]
names(sc_min)[2] <- "min"
sc_max <- sc[, lapply(.SD, max),
             .SDcols = "first_service_dt", by = "INTERNAL_MEMBER_ID"]
names(sc_max)[2] <- "max"
sc_int <- sc_min[sc_max, on = "INTERNAL_MEMBER_ID"]
sc_int[, days := as.numeric(ymd(max) -ymd(min))]
pdf(file = "hist_plot.pdf", width = 6,
    height = 4, onefile = TRUE, family = "Times", paper = "special")
hist(sc_int$days[sc_int$days > 0], xlab = "Range", main = "")
dev.off()
sc_int_smy <- c(summary(sc_int$days), quantile(sc_int$days, 0.95),
                quantile(sc_int$days, 0.99), mean = mean(sc_int$days),
                sd = sd(sc_int$days))
# fwrite(data.table(t(sc_int_smy)),
#        file = "output/sc_days_smy.csv")
sc_min[, `:=`(day30 = min + 31,
              day60 = min + 61)] 

for(i in c("total", 
           "total_pharmacy",
           "ed_raw", "ip_raw", "op_raw", "pc_raw")){
  indir <- paste0("E:/CT_APCD/Sai/intermediate_data/",
                  "cost_measure_intermediate_data/", 
                  "cost_files_by_year/")
  if(i %in% c("ip_raw", "op_raw", "pc_raw", "ed_raw")){
    indir <- paste0("E:/CT_APCD/Sai/intermediate_data/",
                    "cost_measure_intermediate_data/", 
                    "value_sets_fyear/")
  }
  mccost <- rbind(fread(paste0(indir, i,
                               "_", 2016, "_all_ages.csv"),
                        colClasses = "character"),
                  fread(paste0(indir, i,
                               "_", 2015, "_all_ages.csv"),
                        colClasses = "character"))
  if(i == "total_pharmacy"){
    mccost[, ALLOWED_AMT := total]
    mccost[, first_service_dt := prescription_filled_dt]
    mccost[, MEDICAL_CLAIM_SERVICE_LINE_ID := pharmacy_claim_service_line_id]
  }
  names(mccost) <- tolower(names(mccost))
  if(i == "pc_raw"){
    mccost <- mccost[ymd(first_service_dt) > "2014-09-30" &
                       ymd(first_service_dt) < "2015-12-01" &
                       internal_member_id %in% pt$INTERNAL_MEMBER_ID]
  } else{
    mccost <- mccost[mdy(first_service_dt) > "2014-09-30" &
                       mdy(first_service_dt) < "2015-12-01" &
                       internal_member_id %in% pt$INTERNAL_MEMBER_ID]
  }
  if(i == "total_pharmacy"){
    mccost <- mccost[, c("internal_member_id",
                         "first_service_dt", "allowed_amt")]
  } else {
    mccost <- mccost[, c("internal_member_id",
                         "first_service_dt",
                         "medical_claim_header_id","allowed_amt")]
  }
  names(mccost) <- toupper(names(mccost))
  mccost[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)]
  mccost <- mccost[ALLOWED_AMT >= 0]
  if(i != "total_pharmacy"){
    mccost <- sc2[, -"first_service_dt"][mccost, on = "MEDICAL_CLAIM_HEADER_ID"]
  }
  mccost <- mccost[, -"MEDICAL_CLAIM_SERVICE_LINE_ID"]
  mccost <- unique(mccost, use.key = FALSE)
  mccost2 <- sc_min[mccost, on = "INTERNAL_MEMBER_ID"]
  if(i == "pc_raw"){
    mccost2[, FIRST_SERVICE_DT := ymd(FIRST_SERVICE_DT)]
  } else {
    mccost2[, FIRST_SERVICE_DT := mdy(FIRST_SERVICE_DT)]
  }
  mccost_30 <- mccost2[FIRST_SERVICE_DT >= min & FIRST_SERVICE_DT < day30]
  mccost_30_smy <- mccost_30[, lapply(.SD, sum), .SDcols = "ALLOWED_AMT",
                             by = "INTERNAL_MEMBER_ID"]
  names(mccost_30_smy)[2] <- "post_30"
  mccost_60 <- mccost2[FIRST_SERVICE_DT >= min & FIRST_SERVICE_DT < day60]
  mccost_60_smy <- mccost_60[, lapply(.SD, sum), .SDcols = "ALLOWED_AMT",
                             by = "INTERNAL_MEMBER_ID"]
  names(mccost_60_smy)[2] <- "post_60"
  smy <- mccost_30_smy[pt[, .(INTERNAL_MEMBER_ID)], on = "INTERNAL_MEMBER_ID"]
  smy <- mccost_60_smy[smy, on = "INTERNAL_MEMBER_ID"]
  smy[is.na(smy)] <- 0
  sum((smy$post_60 - smy$post_30)<0 )
  # smy <- data.table()
  # for(p in  pt$INTERNAL_MEMBER_ID){
  #   tmp1 <- sc[INTERNAL_MEMBER_ID == p]
  #   tmp2 <- mccost[INTERNAL_MEMBER_ID == p]
  #   cost30 <- cost60 <- data.table()
  #   for(dt in unique(tmp1$first_service_dt)){
  #     if(i == "total_pharmacy"){
  #       tmp3 <- tmp2[mdy(FIRST_SERVICE_DT) < (mdy(dt) + 31) & 
  #                      mdy(FIRST_SERVICE_DT) > mdy(dt)][, .(ALLOWED_AMT)]
  #       tmp3 <- unique(tmp3, use.key = FALSE)
  #       cost30 <- rbind(cost30, tmp3)
  #       tmp4 <- tmp2[mdy(FIRST_SERVICE_DT) < mdy(dt) + 61 &
  #                      mdy(FIRST_SERVICE_DT) > mdy(dt)][, .(ALLOWED_AMT)]
  #       tmp4 <- unique(tmp4, use.key = FALSE)
  #     } else if (i == "pc_raw"){
  #       tmp3 <- tmp2[ymd(FIRST_SERVICE_DT) < (mdy(dt) + 31) & 
  #                      ymd(FIRST_SERVICE_DT) > mdy(dt)][, .(MEDICAL_CLAIM_HEADER_ID ,
  #                                                           ALLOWED_AMT)]
  #       tmp3 <- unique(tmp3, use.key = FALSE)
  #       cost30 <- rbind(cost30, tmp3)
  #       tmp4 <- tmp2[ymd(FIRST_SERVICE_DT) < mdy(dt) + 61 &
  #                      ymd(FIRST_SERVICE_DT) > mdy(dt)][, .(MEDICAL_CLAIM_HEADER_ID ,
  #                                                           ALLOWED_AMT)]
  #       tmp4 <- unique(tmp4, use.key = FALSE)
  #     } else {
  #       tmp3 <- tmp2[mdy(FIRST_SERVICE_DT) < (mdy(dt) + 31) & 
  #                      mdy(FIRST_SERVICE_DT) > mdy(dt)][, .(MEDICAL_CLAIM_HEADER_ID ,
  #                                                           ALLOWED_AMT)]
  #       tmp3 <- unique(tmp3, use.key = FALSE)
  #       cost30 <- rbind(cost30, tmp3)
  #       tmp4 <- tmp2[mdy(FIRST_SERVICE_DT) < mdy(dt) + 61 &
  #                      mdy(FIRST_SERVICE_DT) > mdy(dt)][, .(MEDICAL_CLAIM_HEADER_ID ,
  #                                                           ALLOWED_AMT)]
  #       tmp4 <- unique(tmp4, use.key = FALSE)
  #     }
  #     cost60 <- rbind(cost60, tmp4)
  #   }
  #   smy <- rbind(smy, data.table(INTERNAL_MEMBER_ID = p, post30 = cost30$ALLOWED_AMT,
  #                                post60 = cost60$ALLOWED_AMT, 
  #                                post30_mean = mean(cost30$ALLOWED_AMT, na.rm = T),
  #                                post60_mean = mean(cost60$ALLOWED_AMT, na.rm = T)))
  # }
  smy <- unique(smy, use.key = F)
  setkey(smy, INTERNAL_MEMBER_ID)
  fwrite(smy,
         file = paste0("output/post_30_60_", i, ".csv"))
  smy_30 <- smy[, lapply(.SD, sum), .SDcols = "post_30", by = "INTERNAL_MEMBER_ID"]
  smy_60 <- smy[, lapply(.SD, sum), .SDcols = "post_60", by = "INTERNAL_MEMBER_ID"]
  smy_30 <- pt[smy_30, on = "INTERNAL_MEMBER_ID"]
  smy_60 <- pt[smy_60, on = "INTERNAL_MEMBER_ID"]
  smy_30[, ALLOWED_AMT := post_30]
  smy_60[, ALLOWED_AMT := post_60]
  out <- data.table()
  for (gender in gender_group){
    for(ages in age_group){
      tmp1 <- smy_30[age %in% ages & GENDER_CODE %in% gender]
      tmp2 <- smy_60[age %in% ages & GENDER_CODE %in% gender]
      tmp.out <- c(age = paste0(min(ages), "-", max(ages)),
                   gender = paste0(gender[1], "-", gender[length(gender)]),
                   summary(tmp1$ALLOWED_AMT)[4], summary(tmp2$ALLOWED_AMT)[4], 
                   sd1 = sd(tmp1$ALLOWED_AMT), sd2 = sd(tmp2$ALLOWED_AMT),
                   summary(tmp1$ALLOWED_AMT)[1], summary(tmp2$ALLOWED_AMT)[1],
                   summary(tmp1$ALLOWED_AMT)[6], summary(tmp2$ALLOWED_AMT)[6],
                   summary(tmp1$ALLOWED_AMT)[2], summary(tmp2$ALLOWED_AMT)[2], 
                   summary(tmp1$ALLOWED_AMT)[3], summary(tmp2$ALLOWED_AMT)[3],
                   summary(tmp1$ALLOWED_AMT)[5], summary(tmp2$ALLOWED_AMT)[5], 
                   quantile(tmp1$ALLOWED_AMT, 0.95), quantile(tmp2$ALLOWED_AMT, 0.95),
                   quantile(tmp1$ALLOWED_AMT, 0.99), quantile(tmp2$ALLOWED_AMT, 0.99))
      out <- rbind(out, data.table(t(tmp.out)))
    }
  }
  out <- rbind(out[1:5, ], out[9, ], 
               out[6:8, ], out[10:12, ])
  out <- insertRows(out, c(2, 6, 9, 13), NA)
  fwrite(out, file = paste0("post_30_60_smy_", i, ".csv"))
}

# total medical + pharmacy

indir <- paste0("E:/CT_APCD/Sai/intermediate_data/",
                "cost_measure_intermediate_data/", 
                "cost_files_by_year/")
mccosta <- rbind(fread(paste0(indir, "total_pharmacy", 
                               "_", 2015, "_all_ages.csv"),
                        colClasses = "character"),
                  fread(paste0(indir, "total_pharmacy",
                               "_", 2016, "_all_ages.csv"),
                        colClasses = "character"))
mccostb <- rbind(fread(paste0(indir, "total", 
                               "_", 2015, "_all_ages.csv"),
                        colClasses = "character"),
                  fread(paste0(indir, "total",
                               "_", 2016, "_all_ages.csv"),
                        colClasses = "character"))

mccosta[, ALLOWED_AMT := total]
# mccost1a <- unique(mccost1a)
mccostb <- unique(mccostb)

names(mccosta) <- toupper(names(mccosta))

names(mccostb) <- toupper(names(mccostb))

setnames(mccosta,"PRESCRIPTION_FILLED_DT", "FIRST_SERVICE_DT")
mccost <- rbind(mccosta[, c("INTERNAL_MEMBER_ID", "ALLOWED_AMT","FIRST_SERVICE_DT")], 
                 mccostb[, c("INTERNAL_MEMBER_ID", "ALLOWED_AMT","FIRST_SERVICE_DT")])




# mccost <- mccost[, -"MEDICAL_CLAIM_SERVICE_LINE_ID"]
# mccost <- unique(mccost, use.key = FALSE)
mccost2 <- sc_min[mccost, on = "INTERNAL_MEMBER_ID"]
mccost2[, FIRST_SERVICE_DT := mdy(FIRST_SERVICE_DT)]
mccost_30 <- mccost2[FIRST_SERVICE_DT >= min & FIRST_SERVICE_DT < day30]
mccost_30[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)]
mccost_30_smy <- mccost_30[, lapply(.SD, sum), .SDcols = "ALLOWED_AMT",
                           by = "INTERNAL_MEMBER_ID"]
names(mccost_30_smy)[2] <- "post_30"
mccost_60 <- mccost2[FIRST_SERVICE_DT >= min & FIRST_SERVICE_DT < day60]
mccost_60[, ALLOWED_AMT := as.numeric(ALLOWED_AMT)]
mccost_60_smy <- mccost_60[, lapply(.SD, sum), .SDcols = "ALLOWED_AMT",
                           by = "INTERNAL_MEMBER_ID"]
names(mccost_60_smy)[2] <- "post_60"
smy <- mccost_30_smy[pt[, .(INTERNAL_MEMBER_ID)], on = "INTERNAL_MEMBER_ID"]
smy <- mccost_60_smy[smy, on = "INTERNAL_MEMBER_ID"]
smy[is.na(smy)] <- 0
sum((smy$post_60 - smy$post_30)<0 )
# smy <- data.table()
# for(p in  pt$INTERNAL_MEMBER_ID){
#   tmp1 <- sc[INTERNAL_MEMBER_ID == p]
#   tmp2 <- mccost[INTERNAL_MEMBER_ID == p]
#   cost30 <- cost60 <- data.table()
#   for(dt in unique(tmp1$first_service_dt)){
#     if(i == "total_pharmacy"){
#       tmp3 <- tmp2[mdy(FIRST_SERVICE_DT) < (mdy(dt) + 31) & 
#                      mdy(FIRST_SERVICE_DT) > mdy(dt)][, .(ALLOWED_AMT)]
#       tmp3 <- unique(tmp3, use.key = FALSE)
#       cost30 <- rbind(cost30, tmp3)
#       tmp4 <- tmp2[mdy(FIRST_SERVICE_DT) < mdy(dt) + 61 &
#                      mdy(FIRST_SERVICE_DT) > mdy(dt)][, .(ALLOWED_AMT)]
#       tmp4 <- unique(tmp4, use.key = FALSE)
#     } else if (i == "pc_raw"){
#       tmp3 <- tmp2[ymd(FIRST_SERVICE_DT) < (mdy(dt) + 31) & 
#                      ymd(FIRST_SERVICE_DT) > mdy(dt)][, .(MEDICAL_CLAIM_HEADER_ID ,
#                                                           ALLOWED_AMT)]
#       tmp3 <- unique(tmp3, use.key = FALSE)
#       cost30 <- rbind(cost30, tmp3)
#       tmp4 <- tmp2[ymd(FIRST_SERVICE_DT) < mdy(dt) + 61 &
#                      ymd(FIRST_SERVICE_DT) > mdy(dt)][, .(MEDICAL_CLAIM_HEADER_ID ,
#                                                           ALLOWED_AMT)]
#       tmp4 <- unique(tmp4, use.key = FALSE)
#     } else {
#       tmp3 <- tmp2[mdy(FIRST_SERVICE_DT) < (mdy(dt) + 31) & 
#                      mdy(FIRST_SERVICE_DT) > mdy(dt)][, .(MEDICAL_CLAIM_HEADER_ID ,
#                                                           ALLOWED_AMT)]
#       tmp3 <- unique(tmp3, use.key = FALSE)
#       cost30 <- rbind(cost30, tmp3)
#       tmp4 <- tmp2[mdy(FIRST_SERVICE_DT) < mdy(dt) + 61 &
#                      mdy(FIRST_SERVICE_DT) > mdy(dt)][, .(MEDICAL_CLAIM_HEADER_ID ,
#                                                           ALLOWED_AMT)]
#       tmp4 <- unique(tmp4, use.key = FALSE)
#     }
#     cost60 <- rbind(cost60, tmp4)
#   }
#   smy <- rbind(smy, data.table(INTERNAL_MEMBER_ID = p, post30 = cost30$ALLOWED_AMT,
#                                post60 = cost60$ALLOWED_AMT, 
#                                post30_mean = mean(cost30$ALLOWED_AMT, na.rm = T),
#                                post60_mean = mean(cost60$ALLOWED_AMT, na.rm = T)))
# }
smy <- unique(smy, use.key = F)
setkey(smy, INTERNAL_MEMBER_ID)
# fwrite(smy,
#        file = paste0("output/post_30_60_", i, ".csv"))
smy_30 <- smy[, lapply(.SD, sum), .SDcols = "post_30", by = "INTERNAL_MEMBER_ID"]
smy_60 <- smy[, lapply(.SD, sum), .SDcols = "post_60", by = "INTERNAL_MEMBER_ID"]
smy_30 <- pt[smy_30, on = "INTERNAL_MEMBER_ID"]
smy_60 <- pt[smy_60, on = "INTERNAL_MEMBER_ID"]
smy_30[, ALLOWED_AMT := post_30]
smy_60[, ALLOWED_AMT := post_60]
out <- data.table()
for (gender in gender_group){
  for(ages in age_group){
    tmp1 <- smy_30[age %in% ages & GENDER_CODE %in% gender]
    tmp2 <- smy_60[age %in% ages & GENDER_CODE %in% gender]
    tmp.out <- c(age = paste0(min(ages), "-", max(ages)),
                 gender = paste0(gender[1], "-", gender[length(gender)]),
                 summary(tmp1$ALLOWED_AMT)[4], summary(tmp2$ALLOWED_AMT)[4], 
                 sd1 = sd(tmp1$ALLOWED_AMT), sd2 = sd(tmp2$ALLOWED_AMT),
                 summary(tmp1$ALLOWED_AMT)[1], summary(tmp2$ALLOWED_AMT)[1],
                 summary(tmp1$ALLOWED_AMT)[6], summary(tmp2$ALLOWED_AMT)[6],
                 summary(tmp1$ALLOWED_AMT)[2], summary(tmp2$ALLOWED_AMT)[2], 
                 summary(tmp1$ALLOWED_AMT)[3], summary(tmp2$ALLOWED_AMT)[3],
                 summary(tmp1$ALLOWED_AMT)[5], summary(tmp2$ALLOWED_AMT)[5], 
                 quantile(tmp1$ALLOWED_AMT, 0.95), quantile(tmp2$ALLOWED_AMT, 0.95),
                 quantile(tmp1$ALLOWED_AMT, 0.99), quantile(tmp2$ALLOWED_AMT, 0.99))
    out <- rbind(out, data.table(t(tmp.out)))
  }
}
out <- rbind(out[1:5, ], out[9, ], 
             out[6:8, ], out[10:12, ])
out <- insertRows(out, c(2, 6, 9, 13), NA)
fwrite(out, file = paste0("post_30_60_smy_total_medica_pharmacy", ".csv"))
