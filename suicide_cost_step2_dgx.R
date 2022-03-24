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

yr <- 2015
cat(paste0("begin year ", yr, " at ", Sys.time(), "\n"))
dgx <- fread(paste0("output/dgx_", yr, ".csv"),
                colClasses = "character")
dgx[, .N, .(ICD_VERSION_IND)]
dgx[, DIAGNOSIS_CODE_9_gem := DIAGNOSIS_CODE]
# dgx <- dgx[, lapply(.SD, combine_words, and = ""), 
#                  .SDcols = "DIAGNOSIS_CODE",
#                  by = c("INTERNAL_MEMBER_ID",
#                         "first_service_dt")]
dgx[!ICD_VERSION_IND == 9, `:=`(DIAGNOSIS_CODE_9_gem = icd_map(DIAGNOSIS_CODE, 
                                                                   from = 10, to = 9, 
                                                                   method = "gem"))]
dgx[, plus_flag := as.numeric(grepl("+", DIAGNOSIS_CODE_9_gem, fixed = TRUE))]
dgx[, comma_flag := as.numeric(grepl(",", DIAGNOSIS_CODE_9_gem, fixed = TRUE))]
dgx[, .N, .(plus_flag)]
dgx[, .N, .(comma_flag)]
dgx_comma <- dgx[comma_flag == 1, strsplit(DIAGNOSIS_CODE_9_gem, ",", fixed = T), 
                         by = eval(names(dgx)[!names(dgx) == "DIAGNOSIS_CODE_9_gem"])]
setnames(dgx_comma, "V1", "DIAGNOSIS_CODE_9_gem")
sum(as.numeric(grepl(",", dgx_comma$DIAGNOSIS_CODE_9_gem, fixed = TRUE)))
sum(as.numeric(grepl(",", dgx[comma_flag == 0, ]$DIAGNOSIS_CODE_9_gem, fixed = TRUE)))
dgx <- rbind(dgx_comma, dgx[comma_flag == 0, ])
dgx[plus_flag == 1, DIAGNOSIS_CODE_9_gem :=  gsub("+", ",", 
                                                      DIAGNOSIS_CODE_9_gem, 
                                                      fixed = TRUE)]
sum(as.numeric(grepl("+", dgx$DIAGNOSIS_CODE_9_gem, fixed = TRUE)))
dgx <- dgx[, -c("plus_flag", "comma_flag")]
rm(dgx_comma)
gc()
source("get_suicide_attempts.R")
nn <- dim(dgx)[1]
jj <- floor(nn/100)
for(i in 0:100){
  cat(paste0("begin ",
             (i*jj + 1), " to ", min(nn, ((i + 1)*jj)), 
             " at ", Sys.time(),  "\n"))
  sc.tmp <- data.table(t(isSuicideAttempt_icd9(dgx$DIAGNOSIS_CODE_9_gem[(i*jj + 1):min(nn, ((i + 1)*jj))],
                                        need_split = TRUE)))
  fwrite(sc.tmp, 
         file = paste0("output/dgx_scAt2015_gem.csv"), append = TRUE)
  rm(sc.tmp)
  gc()
}
# sc.tmp <- isSuicideAttempt_icd9(dgx$DIAGNOSIS_CODE_9_gem,
#                                 need_split = TRUE)
sc.tmp <- fread(paste0("output/dgx_scAt2015_gem.csv"))
names(sc.tmp) <- c("rule1", "rule3", "rule4", "rule2")
dim(sc.tmp)
dim(dgx)
sc.tmp2 <- rowSums(sc.tmp[, -"rule2"])
length(sc.tmp2)
dgx[,  `:=`(sc_flag = sc.tmp2,
               rule1 = as.numeric(sc.tmp$rule1),
               rule3 = as.numeric(sc.tmp$rule3),
               rule4 = as.numeric(sc.tmp$rule4))]
rm(sc.tmp)
gc()
dgx <- unique(dgx, use.key = FALSE)
fwrite(dgx, file = paste0("output/dgx_2015_sc_gem.csv"))
