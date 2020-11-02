library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats")
need.packages(need_pkgs)
source("get_suicide_attempts.R")

dgxcols <- c("MEDICAL_CLAIM_SERVICE_LINE_ID",
             "ICD_VERSION_IND", "DIAGNOSIS_CODE")
# dgxdir <- input_dir(paste0("E:/CT_APCD/Sai/intermediate_data/dgs_sub_files/"))
# dgxnames <- list.files(dgxdir)
sc <- data.table()
for(i in 1:200){
  cat(paste0("begin chunk ", i, " at ", Sys.time(), "\n"))
  # dgx.tmp <- fread(paste0(dgxdir, dgxname), select = dgxcols, colClasses = "character")
  dgx.tmp <- fread(paste0("E:/CT_APCD/Uconn_extract_20180521_12312017/MEDICAL_CLAIM_DIAGNOSIS.txt"), 
                   colClasses = "character", nrow = 1e7, 
                   select = c(1, 5, 6), skip = 1e7*(i - 1) + 1)
  names(dgx.tmp) <- dgxcols
  dgx.tmp[ICD_VERSION_IND == 0, 
          DIAGNOSIS_CODE := icd_map(DIAGNOSIS_CODE, 10, 9)]
  need_split <- FALSE
  if(sum(grepl(",", dgx.tmp$DIAGNOSIS_CODE)) > 0){
    need_split <- TRUE
  }
  sc.tmp <- isSuicideAttempt_icd9(dgx.tmp$DIAGNOSIS_CODE, need_split = need_split)
  sc.tmp <- data.frame(t(sapply(sc.tmp, c)))
  sc.tmp2 <- rowSums(sc.tmp, na.rm = T)
  if(max(sc.tmp2, na.rm = T) > 0){
    inds <- which(sc.tmp2 > 0)
    dgx.tmp <- dgx.tmp[inds, ]
    sc <- rbind(sc, dgx.tmp)
    fwrite(sc, file = paste0(output_dir("output/"), 
                             "sc_step1_do_not_open.csv"), append = TRUE)
  }
  cat(paste0("find ", sum(sc.tmp2 > 0, na.rm = T), " suicaid attemps cods in chunck ", i, "\n"))
  cat(paste0("chunk ", i, " done at ", Sys.time(), "\n"))
  rm(dgx.tmp, sc.tmp, sc.tmp2)
  gc()
}
