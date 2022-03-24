if(Sys.info()[4] %in% c("LAZ-ID1",
                        "LAZ-ID2",
                        "LAZ-DEID1",
                        "LAZ-DEID2")){
  setwd("E:/CT_APCD/Sai/suicide_cost")
}
library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", "haven",
               "dplyr", "lubridate", "tidyverse", "matrixStats")
need.packages(need_pkgs)
source("get_suicide_attempts.R")

mc_dgx_2015 <- fread(paste0("output/mc_dgx_unique_2015.csv"),
                     header = TRUE, colClasses = "character",
                     # nrows = 1,
                     select = "INTERNAL_MEMBER_ID",
                     encoding = "UTF-8") %>% unique()
sc_2015 <- fread(paste0("output/sc_pt_2015.csv"),
                 header = TRUE, colClasses = "character",
                 # nrows = 1,
                 select = "INTERNAL_MEMBER_ID",
                 encoding = "UTF-8") %>% unique()
mc_dgx_2015[, sc_flag := 0]
mc_dgx_2015[INTERNAL_MEMBER_ID %in% sc_2015$INTERNAL_MEMBER_ID,
            sc_flag := 1]
elig_pt0 <- fread(paste0("E:/CT_APCD/shared/intermediate_data/",
                         "APCD_modified/eligibility/By_Fiscal_Year/",
                         "medical_elig.csv"),
                  colClasses = "character") %>% unique(use.key = FALSE)
elig_pt0[, `:=`(NumGapsLE45days = as.numeric(NumGapsLE45days),
                NumGapsGT45days = as.numeric(NumGapsGT45days),
                Elig_Sep30 = as.numeric(Elig_Sep30))]
elig_pt3 <- data.table(read_sas("E:/CT_APCD/Beth/data4/eligibility_4.sas7bdat"))
elig_pt3[, INTERNAL_MEMBER_ID := as.character(INTERNAL_MEMBER_ID)]
elig_pt3 <- elig_pt3[ymd(eligibility_start_dt) < ymd("2017-10-01") & 
                       ymd(eligibility_end_dt) > ymd("2012-09-30") & 
                       COVERAGE_CLASS == "MEDICAL"]
medicare_ad_code <- c('HN', 'AB', 'MA', 'MD','12', '43', '14', '16', '15',
                      'CP', 'MB', 'MP', '41', '13', '47', '42', 'ZZ',
                      NULL)
mcpt <- elig_pt3[SUBMITTER_ID == "15227" |
                   PRODUCT_CODE %in% medicare_ad_code,
                 .(INTERNAL_MEMBER_ID)] %>% unique()
elig_pt <- elig_pt0[!INTERNAL_MEMBER_ID %in% mcpt$INTERNAL_MEMBER_ID]

for(yr in 2013:2017){
  cat(paste0("begin ", yr, " at ", Sys.time(), "\n"))
  eligname <- paste0("elig", yr)
  mcname <- paste0("mc", yr)
  tmp0 <- elig_pt[FiscalYR == yr]
  tmp1 <- tmp0[NumGapsGT45days == 0 & 
                 NumGapsLE45days <= 1 &
                 Elig_Sep30 == 1, 
               .(INTERNAL_MEMBER_ID)] %>% unique()
  # tmp1 <- tmp0[!tmp, on = "INTERNAL_MEMBER_ID"]
  # tmp11 <- tmp0[!INTERNAL_MEMBER_ID %in% tmp$INTERNAL_MEMBER_ID]
  tmp1 <- tmp1[, unique(INTERNAL_MEMBER_ID)]
  # tmp2 <- fread(paste0("E:/CT_APCD/Sai/intermediate_data/",
  #                      "cost_measure_intermediate_data/",
  #                      "medical_fiscalyear_", yr, ".csv"),
  #               colClasses = "character",
  #               select = "INTERNAL_MEMBER_ID")[
  #                 !INTERNAL_MEMBER_ID %in% mcpt$INTERNAL_MEMBER_ID
  #                 ][, unique(INTERNAL_MEMBER_ID)]
  assign(eligname, tmp1)
  # assign(mcname, 
         # tmp2,
         # NULL)
  rm(tmp0, tmp1)
  gc()
  cat(paste0(yr, " done at ", Sys.time(), "\n"))
}
elig_ce <- Reduce(intersect, list(elig2013, elig2014, elig2015,
                                  elig2016, elig2017))
# mc_ce <- Reduce(intersect, list(mc2013, mc2014, mc2015,
#                                 mc2016, mc2017))



mc_dgx_2015 <- mc_dgx_2015[INTERNAL_MEMBER_ID %in% elig_ce]
elig_pt4 <- elig_pt3[, .(INTERNAL_MEMBER_ID,
                         birth_dt,
                         GENDER_CODE)] %>% unique(use.key = FALSE)
mc_dgx_2015 <- elig_pt4[mc_dgx_2015, on = "INTERNAL_MEMBER_ID"]
mc_dgx_2015[, birth_dt := as.integer(birth_dt)]
mc_dgx_2015[, age := 2015 - birth_dt]
mc_dgx_2015[, .N, .(sc_flag)][, prop := 100*N/sum(N)] %>% print
mc_dgx_2015[age %in% 0:17, group := "0-17"]
mc_dgx_2015[age %in% 18:29, group := "18-29"]
mc_dgx_2015[age %in% 30:44, group := "30-44"]
mc_dgx_2015[age %in% 45:59, group := "45-59"]
mc_dgx_2015[age >= 60, group := "60+"]
mc_dgx_2015[sc_flag == 1, .N, .(group)][, prop := 100*N/sum(N)] %>% print
mc_dgx_2015[sc_flag == 0, .N, .(group)][, prop := 100*N/sum(N)] %>% print
mc_dgx_2015[sc_flag == 1, .N, .(GENDER_CODE)][, prop := 100*N/sum(N)] %>% print
mc_dgx_2015[sc_flag == 0, .N, .(GENDER_CODE)][, prop := 100*N/sum(N)] %>% print
fwrite(mc_dgx_2015, 
       file = paste0("output/demographics_for_suicide_cost_2015.csv"))
