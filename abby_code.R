library(data.table)
library(lubridate)
sai_dir <- "E:/CT_APCD/Sai/suicide_cost/output/"


date0 <- as.Date("2012-10-01")
date1 <- as.Date("2017-09-30")

total_cost <- lapply(paste0(sai_dir, 
                            list.files(sai_dir,
                                       pattern = "all_ed_ip_op_cost_201[3-7]")),
                     fread, colClasses = "character")

## added by Sai, start
pt <- unique(fread('output/sc_15_no_sc_13_14.csv', colClasses = 'character'))
pt_no <- unique(fread(paste0("output/no_sc_2013_2015_pt.csv"), colClasses = "character"))
pt[, age := 2015 - as.integer(birth_dt)]
pt <- pt[age < 65 & age > 9]
pt_no[, age := 2015 - as.integer(birth_dt)]
pt_no <- pt_no[age < 65 & age > 9]
## added by Sai, end

total_cost <- rbindlist(total_cost)
total_cost <- unique(total_cost)
total_cost[, first_service_dt := as.Date(first_service_dt, format = "%m/%d/%Y"), ]
mfile <- unique(total_cost[first_service_dt >= date0 & first_service_dt <= date1, ])
mfile[, year := cut(first_service_dt, breaks = c(date0, as.Date("2014-10-01"),
                                                 as.Date("2015-10-01"),
                                                 as.Date("2017-10-01")),
                    labels = c("2013-14", "2015", "2016-17"),
                    right = FALSE,
                    include.lowest = TRUE), ]

convcols <- c("ed_only_cost", "ip_only_cost",
              "op_only_cost", "ed_ip_only_cost",
              "ed_op_only_cost", "ip_op_only_cost",
              "ed_ip_op_cost", "not_ed_ip_op_cost")

mfile1 <- mfile[, (convcols) := lapply(.SD, as.numeric), .SDcols = convcols]
select_cols <- c(convcols, "sc_flag", "year", "INTERNAL_MEMBER_ID",
                 "MEDICAL_CLAIM_HEADER_ID",  # added by Sai
                 "type_of_header_id")
mfile1 <- unique(mfile1[, ..select_cols])

mfile1 <- mfile1[, cost := rowSums(.SD, na.rm = T), .SDcols = convcols]
mfile1 <- mfile1[INTERNAL_MEMBER_ID %in% unique(c(pt$INTERNAL_MEMBER_ID, pt_no$INTERNAL_MEMBER_ID))] # added by Sai
mfile1[, .(sum_cost = sum(cost),
           N = length(unique(INTERNAL_MEMBER_ID))), by = .(sc_flag, year)]


# sc_flag    year   sum_cost      N
# 1:       0 2013-14 2265632608 273162
# 2:       1 2013-14    4940061    251
# 3:       0    2015 1261091721 270872
# 4:       1    2015    6952830    250
# 5:       0 2016-17 2952150302 273137
# 6:       1 2016-17    7176325    251



