library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats",
               "knitr")
need.packages(need_pkgs)
source("get_suicide_attempts.R")

for(yr in 2013:2017){
  cat(paste0("begin year ", yr, " at ", Sys.time(), "\n"))
  dgx_mc <- fread(paste0("output/mc_dgx_unique_", yr, ".csv"),
                  colClasses = "character")[ICD_VERSION_IND == 9]
  dgx_mc <- dgx_mc[, lapply(.SD, combine_words, and = ""), 
                      .SDcols = "DIAGNOSIS_CODE",
                      by = c("INTERNAL_MEMBER_ID",
                             "first_service_dt")]
  sc.tmp <- isSuicideAttempt_icd9(dgx_mc$DIAGNOSIS_CODE, need_split = TRUE)
  sc.tmp <- data.frame(t(sapply(sc.tmp, c)))[, 1:4]
  sc.tmp2 <- rowSums(sc.tmp, na.rm = T)
  inds <- which(sc.tmp2 > 0)
  dgx_mc <- dgx_mc[inds, ]
  fwrite(dgx_mc, file = paste0(output_dir("output/"), 
                                 "sc_pt_com_", yr, 
                                 ".csv"))
  cat(paste0("find ", sum(sc.tmp2 > 0, na.rm = T), " suicaid attemps cods in year ", yr, "\n"))
  cat(paste0("year ", yr, " done at ", Sys.time(), "\n"))
  rm(dgx_mc, sc.tmp, sc.tmp2)
  gc()
}
