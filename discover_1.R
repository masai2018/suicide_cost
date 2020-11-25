library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats",
               "readxl")
need.packages(need_pkgs)

med2015 <- data.table(read_excel("E:\\CT_APCD\\Sai\\suicide_cost\\output\\from_Beth\\med2015_1604942739.xlsx", 
                       col_types = c("text", "numeric", "numeric", 
                                     "text", "numeric", "text", "numeric", 
                                     "numeric", "numeric", "date", "numeric", 
                                     "date", "text", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "text", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "text", "numeric", "numeric", "numeric", 
                                     "text", "text", "text", "numeric", 
                                     "numeric", "numeric", "text", "text"), 
                       skip = 1))
med2015_fy <- med2015[ymd(first_service_dt) < ymd("2015-10-01")]

diag1 <- data.table(read_excel("E:\\CT_APCD\\Sai\\suicide_cost\\output\\from_Beth\\diag1_1604942739.xlsx", 
                               col_types = c("text", "numeric", "numeric", 
                                             "numeric", "numeric", "text", "numeric"), 
                               skip = 1))

diag2 <- diag1[MEDICAL_CLAIM_SERVICE_LINE_ID %in% med2015_fy$MEDICAL_CLAIM_SERVICE_LINE_ID]

sc <- fread("output/mc_dgx_unique_2015.csv", colClasses = "character")
sc1 <- sc[INTERNAL_MEMBER_ID == "1604942739"]
sum(med2015_fy$MEDICAL_CLAIM_SERVICE_LINE_ID %in% sc1$MEDICAL_CLAIM_SERVICE_LINE_ID)
sum(diag2$MEDICAL_CLAIM_SERVICE_LINE_ID %in% sc1$MEDICAL_CLAIM_SERVICE_LINE_ID)
