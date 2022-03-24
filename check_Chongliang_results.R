library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats",
               "knitr")
need.packages(need_pkgs)
source("get_suicide_attempts.R")
source("create_icd10_rule.R")

rt11 <- data.table(readRDS("E:/CT_APCD/Chongliang/Suicide-Opioid/R/data/SA_medical_2014.RDS"))
rt12 <- data.table(readRDS("E:/CT_APCD/Chongliang/Suicide-Opioid/R/data/SA_medical_2015.RDS"))
rt11[, INTERNAL_MEMBER_ID := as.character(INTERNAL_MEMBER_ID)]
rt12[, INTERNAL_MEMBER_ID := as.character(INTERNAL_MEMBER_ID)]
rt1 <- rbind(rt11, rt12)
rt1[, first_service_dt := mdy(first_service_dt)]
rt1 <- rt1[first_service_dt >= "2014-10-01" & first_service_dt <= "2015-09-30"]
fwrite(rt1, file = paste0("output/SA_medical_2015_Chongliang.csv"))
pt3 <- fread(paste0("output/pt_all.csv"), colClasses = "character")
rt1 <- rt1[INTERNAL_MEMBER_ID %in% pt3$INTERNAL_MEMBER_ID]
uniqueN(rt1$INTERNAL_MEMBER_ID)

rt2 <- fread(paste0("output/sc_15pt.csv"), colClasses = "character")
rt2 <- rt2[INTERNAL_MEMBER_ID %in% pt3$INTERNAL_MEMBER_ID]

in2_not_in1 <- rt1[!INTERNAL_MEMBER_ID %in% rt2$INTERNAL_MEMBER_ID]
uniqueN(in2_not_in1$INTERNAL_MEMBER_ID)
sum(unique(in2_not_in1$INTERNAL_MEMBER_ID) %in% pt3$INTERNAL_MEMBER_ID)
in2_not_in1 <- in2_not_in1[in2_not_in1$INTERNAL_MEMBER_ID %in% pt3$INTERNAL_MEMBER_ID]
in2_not_in1 <- pt3[in2_not_in1, on = "INTERNAL_MEMBER_ID"]
table(in2_not_in1$age)
in2_not_in1[INTERNAL_MEMBER_ID %in% c("1608494946", "1609233496"), 
            .(ICD_VERSION_IND,
              MEDICAL_CLAIM_HEADER_ID, 
              DIAGNOSIS_CODE, first_service_dt)]
# rt_all <- rbindlist(lapply(2:7, function(x){
#   aaa <- readRDS(paste0("E:/CT_APCD/Chongliang/Suicide-Opioid/R/data/SA_medical_201", 
#                         x, ".RDS"))
#   aaa[, INTERNAL_MEMBER_ID := as.character(INTERNAL_MEMBER_ID)]
#   return(data.table(aaa))
# }))
# rt_all[INTERNAL_MEMBER_ID %in% c("1608494946", "1609233496"), 
#        .(ICD_VERSION_IND,
#          MEDICAL_CLAIM_HEADER_ID, 
#          DIAGNOSIS_CODE, first_service_dt)]
scpt_all <- rbind(rt1[, .(INTERNAL_MEMBER_ID)], rt2) %>% unique()
mc_dgx_2015_9 <- fread("output/dgx_mc_2015_sc_icd9.csv", 
                       colClasses = "character")[INTERNAL_MEMBER_ID %in% scpt_all$INTERNAL_MEMBER_ID]
mc_dgx_2015_10 <- fread("output/dgx_mc_2015_sc_icd10.csv", 
                       colClasses = "character")[INTERNAL_MEMBER_ID %in% scpt_all$INTERNAL_MEMBER_ID]
# mc_dgx_2015[INTERNAL_MEMBER_ID %in% c("1609233496"), 
#             .(ICD_VERSION_IND,
#               DIAGNOSIS_CODE, first_service_dt)]
# unique(mc_dgx_2015[INTERNAL_MEMBER_ID %in% c("1609233496")]$DIAGNOSIS_CODE)
# fwrite(in2_not_in1, file = paste0("output/extra_sc_pt_Chongliang.csv"))
# scpt_rule2 <- fread("E:/CT_APCD/Chongliang/Suicide-Opioid/R/data/MEDICAL_CLAIM_DIAGNOSIS#DIAGNOSIS_CODE#SA-2.csv",
#                     colClasses = "character")
# in2_not_in1[, rule2 := 0]
# in2_not_in1[MEDICAL_CLAIM_SERVICE_LINE_ID %in% scpt_rule2$MEDICAL_CLAIM_SERVICE_LINE_ID,
#             rule2 := 1]
# uniqueN(in2_not_in1[rule2 == 1, ]$INTERNAL_MEMBER_ID)
# fwrite(in2_not_in1[rule2 == 0, ], file = paste0("output/extra_sc_pt_not_rule2_Chongliang.csv"))
# 
# in_mine_not_in_chongliang <- rt2[!INTERNAL_MEMBER_ID %in% rt1$INTERNAL_MEMBER_ID]
# fwrite(in_mine_not_in_chongliang, file = paste0("output/sc_pt_in_ours_not_in_Chongliang.csv"))
setnames(mc_dgx_2015_10, "DIAGNOSIS_CODE_10_combined", "DIAGNOSIS_CODE_combined")
setnames(mc_dgx_2015_9, "DIAGNOSIS_CODE_9_combined", "DIAGNOSIS_CODE_combined")
mc_dgx_2015 <- rbind(mc_dgx_2015_10, mc_dgx_2015_9)
sum(!rt1$INTERNAL_MEMBER_ID %in% mc_dgx_2015$INTERNAL_MEMBER_ID)
mc_dgx_2015 <- mc_dgx_2015[, -"DIAGNOSIS_CODE"] %>% unique(use.key = FALSE)
setnames(mc_dgx_2015, "sc_flag", "Sai_SA")
setnames(mc_dgx_2015, "Sai_SA", "Sai_SA_service_line_id")
mc_dgx_2015[, `:=`(Chongliang_SA_pt = 0,
                   Sai_SA_pt = 0,
                   Chongliang_SA_service_line_id = 0)]
mc_dgx_2015[INTERNAL_MEMBER_ID %in% rt1$INTERNAL_MEMBER_ID, Chongliang_SA_pt := 1]
mc_dgx_2015[MEDICAL_CLAIM_SERVICE_LINE_ID %in% rt1$MEDICAL_CLAIM_SERVICE_LINE_ID, 
            Chongliang_SA_service_line_id := 1]
mc_dgx_2015[INTERNAL_MEMBER_ID %in% rt2$INTERNAL_MEMBER_ID, Sai_SA_pt := 1]
mc_dgx_2015 <- pt3[mc_dgx_2015, on = "INTERNAL_MEMBER_ID"]
mc2015 <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/", 
                       "cost_measure_intermediate_data/", 
                       "medical_fiscalyear_", 2015, ".csv"), 
                colClasses = "character",
                select = c("MEDICAL_CLAIM_SERVICE_LINE_ID", 
                           "MEDICAL_CLAIM_HEADER_ID",
                           "last_service_dt",
                           "DIAGNOSIS_CODE"))
rt1[, `:=`(Chongliang_SA_pt = 1,
           Chongliang_SA_service_line_id = 1,
           Sai_SA_pt = 0,
           Sai_SA_service_line_id = 0,
           rule1 = 0,
           rule3 = 0,
           rule4 = 0)]
rt1_tmp <- rt1[, lapply(.SD, combine_words, sep = ",", and = ","), 
               .SDcols = "DIAGNOSIS_CODE",
               by = c("MEDICAL_CLAIM_SERVICE_LINE_ID")]
setnames(rt1_tmp, "DIAGNOSIS_CODE", "DIAGNOSIS_CODE_combined")
rt1 <- rt1_tmp[rt1, on = "MEDICAL_CLAIM_SERVICE_LINE_ID"]
rt1 <- pt3[rt1, on = "INTERNAL_MEMBER_ID"]
rt1[INTERNAL_MEMBER_ID %in% rt2$INTERNAL_MEMBER_ID, Sai_SA_pt := 1]
sum(!mc_dgx_2015$MEDICAL_CLAIM_SERVICE_LINE_ID %in% mc2015$MEDICAL_CLAIM_SERVICE_LINE_ID)
mc_dgx_2015 <- mc2015[mc_dgx_2015, on = "MEDICAL_CLAIM_SERVICE_LINE_ID"]
mc_dgx_2015 <- mc_dgx_2015[Chongliang_SA_pt == 1 | Sai_SA_pt == 1]
rt1 <- rt1[, names(mc_dgx_2015), with = FALSE]
rt1[, DIAGNOSIS_CODE_combined := as.character(DIAGNOSIS_CODE_combined)]
rt1_tmp2 <- rt1[!MEDICAL_CLAIM_SERVICE_LINE_ID %in% mc_dgx_2015$MEDICAL_CLAIM_SERVICE_LINE_ID]
dim(rt1_tmp2)
mc_dgx_2015[, first_service_dt := mdy(first_service_dt)]
mc_dgx_2015 <- rbind(mc_dgx_2015, rt1_tmp2)
uniqueN(mc_dgx_2015[Chongliang_SA_pt == 1]$INTERNAL_MEMBER_ID)
uniqueN(mc_dgx_2015[Sai_SA_pt == 1]$INTERNAL_MEMBER_ID)
uniqueN(mc_dgx_2015[Chongliang_SA_service_line_id == 1]$INTERNAL_MEMBER_ID)
uniqueN(mc_dgx_2015[Sai_SA_service_line_id == 1]$INTERNAL_MEMBER_ID)
fwrite(mc_dgx_2015, 
       file = paste0("output/compare_SA_Chongliang_Sai_detail_new.csv"))
mc_dgx_2015[, rule_2_Chongliang := 0]
scpt_rule2 <- fread("E:/CT_APCD/Chongliang/Suicide-Opioid/R/data/MEDICAL_CLAIM_DIAGNOSIS#DIAGNOSIS_CODE#SA-2.csv",
                    colClasses = "character")
scpt_rule3 <- fread("E:/CT_APCD/Chongliang/Suicide-Opioid/R/data/MEDICAL_CLAIM_DIAGNOSIS#DIAGNOSIS_CODE#SA-3.csv",
                    colClasses = "character")
scpt_rule4 <- fread("E:/CT_APCD/Chongliang/Suicide-Opioid/R/data/MEDICAL_CLAIM_DIAGNOSIS#DIAGNOSIS_CODE#SA-4.csv",
                    colClasses = "character")
scpt_rule34 <- rbind(scpt_rule3, scpt_rule4)[, .(MEDICAL_CLAIM_SERVICE_LINE_ID)]
mc_dgx_2015[MEDICAL_CLAIM_SERVICE_LINE_ID %in% scpt_rule2$MEDICAL_CLAIM_SERVICE_LINE_ID,
            rule_2_Chongliang := 1]
table(mc_dgx_2015$rule_2_Chongliang)
fwrite(mc_dgx_2015, 
       file = paste0("output/compare_SA_Chongliang_Sai_detail_new_1123.csv"))
mc_dgx_2015 <- fread(paste0("output/compare_SA_Chongliang_Sai_detail_new_1123.csv"),
                     colClasses = "character")
mc_dgx_2015_2 <- rbind(mc_dgx_2015_10, mc_dgx_2015_9)
mc_dgx_2015[, in_our_data := 0]
mc_dgx_2015[MEDICAL_CLAIM_SERVICE_LINE_ID %in% mc_dgx_2015_2$MEDICAL_CLAIM_SERVICE_LINE_ID, in_our_data := 1]
table(mc_dgx_2015$in_our_data)
fwrite(mc_dgx_2015, 
       file = paste0("output/compare_SA_Chongliang_Sai_detail_new_1216.csv"))
mc_dgx_2015_2 <- mc_dgx_2015[rule_2_Chongliang == 0]
uniqueN(mc_dgx_2015_2[ICD_VERSION_IND == 0 &  Sai_SA_pt == 1 & Chongliang_SA_pt == 0]$INTERNAL_MEMBER_ID)
#29
uniqueN(mc_dgx_2015[rule_2_Chongliang == 1 & Sai_SA_pt == 0]$INTERNAL_MEMBER_ID)
#238
fwrite(mc_dgx_2015_2, 
       file = paste0("output/compare_SA_Chongliang_Sai_detail_new_1216_no_rule_2.csv"))
mc_dgx_2015_2[, rule34 := 0]
mc_dgx_2015_2[MEDICAL_CLAIM_SERVICE_LINE_ID %in% scpt_rule34$MEDICAL_CLAIM_SERVICE_LINE_ID, rule34 := 1]
uniqueN(mc_dgx_2015_2[rule34 == 1 & rule_2_Chongliang == 0 & Sai_SA_pt == 0 &
                      ICD_VERSION_IND == 9 & Chongliang_SA_pt == 1 &
                      !MEDICAL_CLAIM_SERVICE_LINE_ID %in% rt1_tmp2$MEDICAL_CLAIM_SERVICE_LINE_ID]$INTERNAL_MEMBER_ID)

