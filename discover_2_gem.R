library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats",
               "readxl")
need.packages(need_pkgs)

dat2015 <- fread("E:/CT_APCD/Sai/intermediate_data/cost_measure_intermediate_data/medical_fiscalyear_2015.csv",
                 colClasses = "character", 
                 select = c("ICD_VERSION_IND",
                 "INTERNAL_MEMBER_ID",
                 "MEDICAL_CLAIM_SERVICE_LINE_ID",
                 "first_service_dt",
                 "DIAGNOSIS_CODE"))
dat2015[, .N, .(ICD_VERSION_IND)]
dat2015[, DIAGNOSIS_CODE_9_gem := DIAGNOSIS_CODE]
dat2015[!ICD_VERSION_IND == 9, `:=`(DIAGNOSIS_CODE_9_gem = icd_map(DIAGNOSIS_CODE, 
                                                            from = 10, to = 9, 
                                                            method = "gem"))]
dat2015[, plus_flag := as.numeric(grepl("+", DIAGNOSIS_CODE_9_gem, fixed = TRUE))]
dat2015[, comma_flag := as.numeric(grepl(",", DIAGNOSIS_CODE_9_gem, fixed = TRUE))]
dat2015[, .N, .(plus_flag)]
dat2015[, .N, .(comma_flag)]
dat2015_comma <- dat2015[comma_flag == 1, strsplit(DIAGNOSIS_CODE_9_gem, ",", fixed = T), 
                     by = eval(names(dat2015)[!names(dat2015) == "DIAGNOSIS_CODE_9_gem"])]
setnames(dat2015_comma, "V1", "DIAGNOSIS_CODE_9_gem")
sum(as.numeric(grepl(",", dat2015_comma$DIAGNOSIS_CODE_9_gem, fixed = TRUE)))
sum(as.numeric(grepl(",", dat2015[comma_flag == 0, ]$DIAGNOSIS_CODE_9_gem, fixed = TRUE)))
dat2015 <- rbind(dat2015_comma, dat2015[comma_flag == 0, ])
dat2015[plus_flag == 1, DIAGNOSIS_CODE_9_gem :=  gsub("+", ",", 
                                                      DIAGNOSIS_CODE_9_gem, 
                                                      fixed = TRUE)]
sum(as.numeric(grepl("+", dat2015$DIAGNOSIS_CODE_9_gem, fixed = TRUE)))
dat2015 <- dat2015[, -c("plus_flag", "comma_flag")]
rm(dat2015_comma)
gc()
source("get_suicide_attempts.R")
# sc.tmp <- isSuicideAttempt_icd9(dat2015$DIAGNOSIS_CODE_9_gem,
#                                 need_split = TRUE)
# fwrite(sc.tmp, file = paste0("output/scAt2015_gem.csv"))
sc.tmp <- data.table::transpose(fread(paste0("output/scAt2015_gem.csv")))
names(sc.tmp) <- c("rule1", "rule3", "rule4", "rule2")
dim(sc.tmp)
dim(dat2015)
sc.tmp2 <- rowSums(sc.tmp[, -"rule2"])
length(sc.tmp2)
dat2015[,  `:=`(sc_flag = sc.tmp2,
               rule1 = as.numeric(sc.tmp$rule1),
               rule3 = as.numeric(sc.tmp$rule3),
               rule4 = as.numeric(sc.tmp$rule4))]
fwrite(dat2015, file = paste0("output/dat_2015_sc_gem.csv"))
sc2015_claim_gem <- dat2015[sc_flag == 1, ]
fwrite(sc2015_claim_gem, file = paste0("output/sc_2015_gem.csv"))
