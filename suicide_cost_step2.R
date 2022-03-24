if(Sys.info()[4] %in% c("LAZ-ID1",
                        "LAZ-ID2",
                        "LAZ-DEID1",
                        "LAZ-DEID2")){
  setwd("E:/CT_APCD/Sai/suicide_cost")
}
library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats",
               "knitr")
need.packages(need_pkgs)
source("get_suicide_attempts.R")
source("create_icd10_rule.R")

for(yr in 2013:2015){
  cat(paste0("begin year ", yr, " at ", Sys.time(), "\n"))
  dgx_mc <- fread(paste0("output/mc_dgx_unique_", yr, ".csv"),
                  colClasses = "character")
  # dgx_mc[, .N, .(ICD_VERSION_IND)]
  dgx_mc_9 <- dgx_mc[ICD_VERSION_IND == 9]
  dgx_mc_10 <- dgx_mc[ICD_VERSION_IND == 0]
  rm(dgx_mc)
  gc()
  
  dgx_mc_9_2 <- dgx_mc_9[, lapply(.SD, combine_words, sep = ",", and = ","), 
                         .SDcols = "DIAGNOSIS_CODE",
                         by = c("MEDICAL_CLAIM_SERVICE_LINE_ID")]
  dgx_mc_9_2[, DIAGNOSIS_CODE := gsub(",,", ",", DIAGNOSIS_CODE, fixed = TRUE)]
  
  nn <- dim(dgx_mc_9_2)[1]
  jj <- floor(nn/100)
  for(i in 0:100){
    cat(paste0("begin ",
               (i*jj + 1), " to ", min(nn, ((i + 1)*jj)), 
               " at ", Sys.time(),  "\n"))
    sc.tmp <- data.table(t(isSuicideAttempt_icd9(dgx_mc_9_2$DIAGNOSIS_CODE[(i*jj + 1):min(nn, ((i + 1)*jj))],
                                                 need_split = TRUE)))
    fwrite(sc.tmp, 
           file = paste0("output/dgx_mc_scAt", yr, "_9.csv"), append = TRUE)
    rm(sc.tmp)
    gc()
  }
  sc.tmp <- fread(paste0("output/dgx_mc_scAt", yr, "_9.csv"))
  names(sc.tmp) <- c("rule1", "rule3", "rule4", "rule2")
  dim(sc.tmp)
  dim(dgx_mc_9_2)
  sc.tmp2 <- as.numeric(rowSums(sc.tmp[, -"rule2"]) > 0)
  length(sc.tmp2)
  dgx_mc_9_2[,  `:=`(sc_flag = sc.tmp2,
                     rule1 = as.numeric(sc.tmp$rule1),
                     rule3 = as.numeric(sc.tmp$rule3),
                     rule4 = as.numeric(sc.tmp$rule4))]
  rm(sc.tmp)
  rm(sc.tmp2)
  gc()
  setnames(dgx_mc_9_2, "DIAGNOSIS_CODE", "DIAGNOSIS_CODE_9_combined")
  dgx_mc_9 <- dgx_mc_9_2[dgx_mc_9, on = "MEDICAL_CLAIM_SERVICE_LINE_ID"]
  fwrite(dgx_mc_9_2, file = paste0("output/dgx_mc_", yr, "_sc_icd9_small.csv"))
  # dgx_mc_9_2 <- fread(paste0("output/dgx_mc_", yr, "_sc_icd9_small.csv"), colClasses = "character")
  rm(dgx_mc_9_2)
  gc()
  fwrite(dgx_mc_9, file = paste0("output/dgx_mc_", yr, "_sc_icd9.csv"))
  # dgx_mc_9 <- fread(paste0("output/dgx_mc_", yr, "_sc_icd9.csv"), colClasses = "character")
  rm(dgx_mc_9)
  gc()
  
  # dgx_mc_10_2 <- dgx_mc_10[, lapply(.SD, combine_words, sep = ",", and = ","), 
  #                          .SDcols = "DIAGNOSIS_CODE",
  #                          by = c("MEDICAL_CLAIM_SERVICE_LINE_ID")]
  # dgx_mc_10_2[, DIAGNOSIS_CODE := gsub(",,", ",", DIAGNOSIS_CODE, fixed = TRUE)]
  # nn <- dim(dgx_mc_10_2)[1]
  # jj <- floor(nn/100)
  # for(i in 0:100){
  #   cat(paste0("begin ",
  #              (i*jj + 1), " to ", min(nn, ((i + 1)*jj)), 
  #              " at ", Sys.time(),  "\n"))
  #   sc.tmp <- data.table(t(sapply(dgx_mc_10_2$DIAGNOSIS_CODE[(i*jj + 1):min(nn, ((i + 1)*jj))], isSuicideAttempt_icd10)))
  #   fwrite(sc.tmp, 
  #          file = paste0("output/dgx_mc_scAt", yr, "_10.csv"), append = TRUE)
  #   rm(sc.tmp)
  #   gc()
  # }
  # sc.tmp_10 <- fread(paste0("output/dgx_mc_scAt", yr, "_10.csv"))
  # names(sc_tmp_10) <- c("rule1", "rule2", "rule3", "rule4")
  # sc_tmp_10 <- sc_tmp_10[, -"rule2"]
  # sc.tmp_10_2 <- rowSums(sc_tmp_10)
  # sc_tmp_10[, `:=`(sc_flag = as.numeric(sc.tmp_10_2 > 0))]
  # dgx_mc_10_2 <- cbind(dgx_mc_10_2, sc_tmp_10)
  # setnames(dgx_mc_10_2, "DIAGNOSIS_CODE", "DIAGNOSIS_CODE_10_combined")
  # dgx_mc_10 <- dgx_mc_10_2[dgx_mc_10, on = "MEDICAL_CLAIM_SERVICE_LINE_ID"]
  # fwrite(dgx_mc_10_2, file = paste0("output/dgx_mc_", yr, "_sc_icd10_small.csv"))
  # rm(dgx_mc_10_2)
  # gc()
  # fwrite(dgx_mc_10, file = paste0("output/dgx_mc_", yr, "_sc_icd10.csv"))
  # rm(dgx_mc_10)
  # gc()
}




# yr <- 2015
# cat(paste0("begin year ", yr, " at ", Sys.time(), "\n"))
# dgx_mc <- fread(paste0("output/mc_dgx_unique_", yr, ".csv"),
#                 colClasses = "character")
# dgx_mc[, .N, .(ICD_VERSION_IND)]
# dgx_mc[, DIAGNOSIS_CODE_9_gem := DIAGNOSIS_CODE]
# # dgx_mc <- dgx_mc[, lapply(.SD, combine_words, and = ""), 
# #                  .SDcols = "DIAGNOSIS_CODE",
# #                  by = c("INTERNAL_MEMBER_ID",
# #                         "first_service_dt")]
# dgx_mc[!ICD_VERSION_IND == 9, `:=`(DIAGNOSIS_CODE_9_gem = icd_map(DIAGNOSIS_CODE, 
#                                                                    from = 10, to = 9, 
#                                                                    method = "gem"))]
# dgx_mc[, plus_flag := as.numeric(grepl("+", DIAGNOSIS_CODE_9_gem, fixed = TRUE))]
# dgx_mc[, comma_flag := as.numeric(grepl(",", DIAGNOSIS_CODE_9_gem, fixed = TRUE))]
# dgx_mc[, .N, .(plus_flag)]
# dgx_mc[, .N, .(comma_flag)]
# dgx_mc_comma <- dgx_mc[comma_flag == 1, strsplit(DIAGNOSIS_CODE_9_gem, ",", fixed = T), 
#                          by = eval(names(dgx_mc)[!names(dgx_mc) == "DIAGNOSIS_CODE_9_gem"])]
# setnames(dgx_mc_comma, "V1", "DIAGNOSIS_CODE_9_gem")
# sum(as.numeric(grepl(",", dgx_mc_comma$DIAGNOSIS_CODE_9_gem, fixed = TRUE)))
# sum(as.numeric(grepl(",", dgx_mc[comma_flag == 0, ]$DIAGNOSIS_CODE_9_gem, fixed = TRUE)))
# dgx_mc <- rbind(dgx_mc_comma, dgx_mc[comma_flag == 0, ])
# dgx_mc[plus_flag == 1, DIAGNOSIS_CODE_9_gem :=  gsub("+", ",", 
#                                                       DIAGNOSIS_CODE_9_gem, 
#                                                       fixed = TRUE)]
# sum(as.numeric(grepl("+", dgx_mc$DIAGNOSIS_CODE_9_gem, fixed = TRUE)))
# dgx_mc <- dgx_mc[, -c("plus_flag", "comma_flag")]
# rm(dgx_mc_comma)
# gc()
# source("get_suicide_attempts.R")
# 
# dgx_mc_2 <- dgx_mc[, lapply(.SD, combine_words, sep = ",", and = ","), 
#            .SDcols = "DIAGNOSIS_CODE_9_gem",
#            by = c("MEDICAL_CLAIM_SERVICE_LINE_ID")]
# dgx_mc_2[, DIAGNOSIS_CODE_9_gem := gsub(",,", ",", DIAGNOSIS_CODE_9_gem, fixed = TRUE)]
# nn <- dim(dgx_mc_2)[1]
# jj <- floor(nn/100)
# for(i in 0:100){
#   cat(paste0("begin ",
#              (i*jj + 1), " to ", min(nn, ((i + 1)*jj)), 
#              " at ", Sys.time(),  "\n"))
#   sc.tmp <- data.table(t(isSuicideAttempt_icd9(dgx_mc_2$DIAGNOSIS_CODE_9_gem[(i*jj + 1):min(nn, ((i + 1)*jj))],
#                                                need_split = TRUE)))
#   fwrite(sc.tmp, 
#          file = paste0("output/dgx_mc_scAt2015_gem_2.csv"), append = TRUE)
#   rm(sc.tmp)
#   gc()
# }
# sc.tmp <- fread(paste0("output/dgx_mc_scAt2015_gem_2.csv"))
# names(sc.tmp) <- c("rule1", "rule3", "rule4", "rule2")
# dim(sc.tmp)
# dim(dgx_mc_2)
# sc.tmp2 <- rowSums(sc.tmp[, -"rule2"])
# length(sc.tmp2)
# dgx_mc_2[,  `:=`(sc_flag = sc.tmp2,
#                rule1 = as.numeric(sc.tmp$rule1),
#                rule3 = as.numeric(sc.tmp$rule3),
#                rule4 = as.numeric(sc.tmp$rule4))]
# rm(sc.tmp)
# gc()
# setnames(dgx_mc_2, "DIAGNOSIS_CODE_9_gem", "DIAGNOSIS_CODE_9_gem_combined")
# fwrite(dgx_mc_2, file = paste0("output/dgx_mc_2015_sc_gem_2.csv"))
# dgx_mc_new <- dgx_mc_2[dgx_mc, on = "MEDICAL_CLAIM_SERVICE_LINE_ID"]
# rm(dgx_mc)
# rm(dgx_mc_2)
# gc()
# dgx_mc_new[sc_flag != 0, sc_flag := 1]
# dgx_mc_new[, `:=`(sc_from_icd9 = 0,
#               sc_from_icd10 = 0)]
# dgx_mc_new[sc_flag == 1 & ICD_VERSION_IND == 9, sc_from_icd9 := 1]
# dgx_mc_new[sc_flag == 1 & ICD_VERSION_IND == 0, sc_from_icd10 := 1]
# uniqueN(dgx_mc_new[sc_from_icd9 == 1]$MEDICAL_CLAIM_SERVICE_LINE_ID)
# # 39102
# uniqueN(dgx_mc_new[sc_from_icd10 == 1]$MEDICAL_CLAIM_SERVICE_LINE_ID)
# # 2476
# uniqueN(dgx_mc_new[sc_from_icd9 == 1]$INTERNAL_MEMBER_ID)
# # 2409
# uniqueN(dgx_mc_new[sc_from_icd10 == 1]$INTERNAL_MEMBER_ID)
# # 166
# fwrite(dgx_mc_new, file = paste0("output/dgx_mc_2015_sc_gem.csv"))
# 
# dgx_mc_new <- fread("output/dgx_mc_2015_sc_gem.csv", colClasses = "character",
#                     select = c("MEDICAL_CLAIM_SERVICE_LINE_ID",
#                                "first_service_dt",
#                                "INTERNAL_MEMBER_ID",
#                                "ICD_VERSION_IND",
#                                "DIAGNOSIS_CODE",
#                                # "DIAGNOSIS_CODE_9_gem_combined",
#                                "sc_flag",
#                                "rule1",
#                                "rule3",
#                                "rule4"))
# dgx_mc_9 <- dgx_mc_new[ICD_VERSION_IND == 9]
# fwrite(dgx_mc_9, file = "output/dgx_mc_2015_sc_gem_icd9.csv")
# rm(dgx_mc_9); gc()
# dgx_mc_10 <- dgx_mc_new[ICD_VERSION_IND == 0]
# fwrite(dgx_mc_10, file = "output/dgx_mc_2015_sc_gem_icd10.csv")
# dgx_mc_10 <- fread("output/dgx_mc_2015_sc_gem_icd10.csv", 
#                    colClasses = "character")
# dgx_mc_10_2 <- dgx_mc_10[, lapply(.SD, combine_words, sep = ",", and = ","), 
#                    .SDcols = "DIAGNOSIS_CODE",
#                    by = c("MEDICAL_CLAIM_SERVICE_LINE_ID")]
# sc_tmp_10 <- data.table(t(sapply(dgx_mc_10_2$DIAGNOSIS_CODE, isSuicideAttempt_icd10)))
# names(sc_tmp_10) <- c("rule1", "rule2", "rule3", "rule4")
# sc_tmp_10 <- sc_tmp_10[, -"rule2"]
# sc.tmp_10_2 <- rowSums(sc_tmp_10)
# sc_tmp_10[, `:=`(sc_flag = as.numeric(sc.tmp_10_2 > 0))]
# dgx_mc_10_2 <- cbind(dgx_mc_10_2, sc_tmp_10)
# setnames(dgx_mc_10_2, "DIAGNOSIS_CODE", "DIAGNOSIS_CODE_10_combined")
# dgx_mc_10_new <- dgx_mc_10_2[dgx_mc_10, on = "MEDICAL_CLAIM_SERVICE_LINE_ID"]
# fwrite(dgx_mc_10_new, file = "output/dgx_mc_2015_sc_icd10.csv")
# 
# 
# aaa <- unique(dgx_mc_10_new[sc_flag == 1]$INTERNAL_MEMBER_ID)
# bbb <- unique(dgx_mc_10[sc_flag == 1]$INTERNAL_MEMBER_ID)
