library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats")
need.packages(need_pkgs)
source("get_suicide_attempts.R")

mccols <- c("MEDICAL_CLAIM_SERVICE_LINE_ID", 
            "INTERNAL_MEMBER_ID", "first_service_dt")
mcdir <- input_dir(paste0("E:/CT_APCD/Sai/intermediate_data/cost_measure_intermediate_data/"))
sc <- fread("output/sc_step1.csv", colClasses = "character")
for (fyear in 2017:2013){
  cat(paste0("begin ", fyear, " at ", Sys.time(), "\n"))
  mc <- fread(paste0(mcdir, "medical_fiscalyear_", fyear, ".csv"),
              colClasses = "character", select = mccols)
  sc2 <- mc[sc, on = "MEDICAL_CLAIM_SERVICE_LINE_ID", nomatch = 0]
  fwrite(sc2, file = paste0(output_dir("output/"), 
                            "sc_", fyear, ".csv"))
  cat(paste0("find ", dim(sc2)[1], " suicaid attemps ", fyear, "\n"))
  cat(paste0(fyear, " done at ", Sys.time(), "\n"))
  rm(mc, sc2)
  gc()
}

elig <- fread(paste0("E:/CT_APCD/shared/intermediate_data/",
                     "APCD_modified/eligibility/", 
                     "By_Fiscal_Year/medical_elig_all.csv"),
              select = c("INTERNAL_MEMBER_ID", "FiscalYR",
                         "birth_dt", "GENDER_CODE"),
              colClasses = "character")[
                , birth_dt := as.integer(birth_dt)] %>% 
  unique(use.key = FALSE)
for (fyear in 2017:2013) {
  cat(paste0("begin ", fyear, " at ", Sys.time(), "\n"))
  mc.pt <- fread(paste0("E:/CT_APCD/shared/intermediate_data/",
                        "APCD_modified/medicare_medicare_ad_patients/fy",
                        fyear, "all_ages.csv"),
                 colClasses = "character")
  elig1 <- elig[FiscalYR == fyear][, -"FiscalYR"]
  elig1 <- elig1[!mc.pt, on = "INTERNAL_MEMBER_ID"]
  sc.pt <- fread(paste0("output/sc_", fyear, ".csv"),
                 colClasses = "character")
  sc.pt <- elig[sc.pt, on = "INTERNAL_MEMBER_ID", nomatch = 0]
  sc.pt[, age := fyear - as.integer(birth_dt)]
  sc.pt[, .N, .(age)]
  fwrite(sc.pt, file = paste0(output_dir("output/"), 
                            "sc_com_", fyear, ".csv"))
  cat(paste0(fyear, " done at ", Sys.time(), "\n"))
  rm(mc.pt, sc.pt, elig1)
  gc()
}
