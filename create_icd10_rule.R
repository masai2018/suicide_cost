library(methods)
source("utils.R")
need_pkgs <- c("data.table", "bit64", "tools", "touch", 
               "dplyr", "lubridate", "tidyverse", "matrixStats",
               "knitr", "readxl")
need.packages(need_pkgs)
source("get_suicide_attempts.R")

icd10_r1 <- read_excel("suicide_attempt_icd10_codes__draft1.xlsx", 
                       sheet = "Rule1 icd10 codes")[-1, ]$...2 %>% as.character() %>% unique()
icd10_r1 <- gsub("\\.", "", icd10_r1)
icd10_r1 <- icd10_r1[!is.na(icd10_r1)]
icd10_r1 <- icd10_r1[icd10_r1 != ""]
head(icd10_r1)

icd10_r2 <- read_excel("suicide_attempt_icd10_codes__draft1.xlsx", 
                       sheet = "Rule 2 icd10 codes")[-1, ]$...2  %>% as.character() %>% unique()
icd10_r2 <- gsub("\\.", "", icd10_r2)
icd10_r2 <- icd10_r2[!is.na(icd10_r2)]
icd10_r2 <- icd10_r2[icd10_r2 != ""]
head(icd10_r2)

icd10_r3 <- read_excel("suicide_attempt_icd10_codes__draft1.xlsx", 
                       sheet = "Rule 3icd10  codes")[-1, ]
icd10_r3_1 <- icd10_r3$...2 %>% unique() %>% as.character()
icd10_r3_1 <- gsub("\\.", "", icd10_r3_1)
icd10_r3_1 <- icd10_r3_1[!is.na(icd10_r3_1)]
icd10_r3_1 <- icd10_r3_1[icd10_r3_1 != ""]
head(icd10_r3_1)

icd10_r3_2 <- icd10_r3$...5 %>% unique() %>% as.character()
icd10_r3_2 <- gsub("\\.", "", icd10_r3_2)
icd10_r3_2 <- icd10_r3_2[!is.na(icd10_r3_2)]
icd10_r3_2 <- icd10_r3_2[icd10_r3_2 != ""]
head(icd10_r3_2)

icd10_r4 <- read_excel("suicide_attempt_icd10_codes__draft1.xlsx", 
                       sheet = "Rule 4 icd10 codes")[-(1:2), ]
icd10_r4_1 <- icd10_r4$...2 %>% unique() %>% as.character()
icd10_r4_1 <- gsub("\\.", "", icd10_r4_1)
icd10_r4_1 <- icd10_r4_1[!is.na(icd10_r4_1)]
icd10_r4_1 <- icd10_r4_1[icd10_r4_1 != ""]
head(icd10_r4_1)

icd10_r4_2 <- icd10_r4$...5 %>% unique() %>% as.character()
icd10_r4_2 <- gsub("\\.", "", icd10_r4_2)
icd10_r4_2 <- icd10_r4_2[!is.na(icd10_r4_2)]
icd10_r4_2 <- icd10_r4_2[icd10_r4_2 != ""]
head(icd10_r4_2)

isSuicideAttempt_icd10 <- function(dx_codes){
  dx_codes <- gsub(" ", "", dx_codes)
  dx_codes <- unlist(strsplit(dx_codes, ","))
  dx_codes <- dx_codes[dx_codes != ""]
  is_sa <- function(x) {
    idx1 <- sum(x %in% icd10_r1) > 0
    idx2 <- sum(x %in% icd10_r2) > 0
    idx3 <- (sum(x %in% icd10_r3_1) > 0) & (sum(x %in% icd10_r3_2) > 0)
    idx4 <- (sum(x %in% icd10_r4_1) > 0) & (sum(x %in% icd10_r4_2) > 0)
    return(c(idx1, idx2, idx3, idx4))
  }
  # sapply(dx_codes, is_sa)
  is_sa(dx_codes)
}
