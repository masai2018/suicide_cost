library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats",
               "readxl")
need.packages(need_pkgs)

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
dat2015[, `:=`(DIAGNOSIS_CODE_9_gem = DIAGNOSIS_CODE,
               DIAGNOSIS_CODE_9_both = DIAGNOSIS_CODE)]
dat2015[!ICD_VERSION_IND == 9, `:=`(DIAGNOSIS_CODE_9_gem = icd_map(DIAGNOSIS_CODE, 
                                                                   from = 10, to = 9, 
                                                                   method = "gem"),
                                    DIAGNOSIS_CODE_9_both = icd_map(DIAGNOSIS_CODE, 
                                                                   from = 10, to = 9, 
                                                                   method = "both"))]
sc2015_gem <- fread(paste0("output/sc_2015_gem.csv"), colClasses = "character")
sc2015_both <- fread(paste0("output/sc_2015_both.csv"), colClasses = "character")
dat2015[, `:=`(sc_flag_gem = 0, sc_flag_both = 0,
               rule1_gem = 0, rule3_gem = 0, rule4_gem = 0,
               rule1_both = 0, rule3_both = 0, rule4_both = 0)]
dat2015[MEDICAL_CLAIM_SERVICE_LINE_ID %in% sc2015_gem$MEDICAL_CLAIM_SERVICE_LINE_ID,
        sc_flag_gem := 1]
dat2015[MEDICAL_CLAIM_SERVICE_LINE_ID %in% sc2015_both$MEDICAL_CLAIM_SERVICE_LINE_ID,
        sc_flag_both := 1]
dat2015[MEDICAL_CLAIM_SERVICE_LINE_ID %in% sc2015_gem[rule1 == 1]$MEDICAL_CLAIM_SERVICE_LINE_ID,
        rule1_gem := 1]
dat2015[MEDICAL_CLAIM_SERVICE_LINE_ID %in% sc2015_gem[rule3 == 1]$MEDICAL_CLAIM_SERVICE_LINE_ID,
        rule3_gem := 1]
dat2015[MEDICAL_CLAIM_SERVICE_LINE_ID %in% sc2015_gem[rule4 == 1]$MEDICAL_CLAIM_SERVICE_LINE_ID,
        rule4_gem := 1]
dat2015[MEDICAL_CLAIM_SERVICE_LINE_ID %in% sc2015_both[rule1 == 1]$MEDICAL_CLAIM_SERVICE_LINE_ID,
        rule1_both := 1]
dat2015[MEDICAL_CLAIM_SERVICE_LINE_ID %in% sc2015_both[rule3 == 1]$MEDICAL_CLAIM_SERVICE_LINE_ID,
        rule3_both := 1]
dat2015[MEDICAL_CLAIM_SERVICE_LINE_ID %in% sc2015_both[rule4 == 1]$MEDICAL_CLAIM_SERVICE_LINE_ID,
        rule4_both := 1]
fwrite(dat2015, file = paste0("output/compare_sc_2015_gem_both.csv"))
