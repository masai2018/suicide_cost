

## total mc cost

pt <- fread('output/sc_15_no_sc_13_14.csv', colClasses = 'character',
            select = c("INTERNAL_MEMBER_ID", "birth_dt", 
                       "GENDER_CODE")) %>% unique()
uniqueN(pt$INTERNAL_MEMBER_ID)

head(pt)
pt[, age := 2015 - as.integer(birth_dt)]
pt[age <= 17, age_range := "0-17"]
pt[age >= 18 & age <= 29, age_range := "18-29"]
pt[age >= 30 & age <= 44, age_range := "30-44"]
pt[age >= 45 & age <= 59, age_range := "45-59"]
pt[age >= 60, age_range := "60+"]
smy_age <- pt[, .N, .(age_range)]
smy_age[, per := N/uniqueN(pt$INTERNAL_MEMBER_ID)]
smy_gender <- pt[, .N, .(GENDER_CODE)]
smy_gender[, per := N/uniqueN(pt$INTERNAL_MEMBER_ID)]
smy_gender_M_age <- pt[GENDER_CODE == "M", .N, .(age_range)]
smy_gender_M_age[, per := N/uniqueN(pt[GENDER_CODE == "M"]$INTERNAL_MEMBER_ID)]

smy_gender_F_age <- pt[GENDER_CODE == "F", .N, .(age_range)]
smy_gender_F_age[, per := N/uniqueN(pt[GENDER_CODE == "F"]$INTERNAL_MEMBER_ID)]



pt2 <- fread('output/no_sc_2013_2015_pt.csv', colClasses = 'character') %>% unique()
head(pt2)
elig_pt3 <- data.table(read_sas("E:/CT_APCD/Beth/data4/eligibility_4.sas7bdat"))
elig_pt3[, INTERNAL_MEMBER_ID := as.character(INTERNAL_MEMBER_ID)]
elig_pt3 <- elig_pt3[ymd(eligibility_start_dt) < ymd("2017-10-01") & 
                       ymd(eligibility_end_dt) > ymd("2012-09-30") & 
                       COVERAGE_CLASS == "MEDICAL"]
elig_pt3 <- unique(elig_pt3[, .(INTERNAL_MEMBER_ID, GENDER_CODE, birth_dt)])
pt2 <- elig_pt3[pt2, on = "INTERNAL_MEMBER_ID"]
fwrite(pt2, file = 'output/no_sc_2013_2015_pt_new.csv')
pt2[, age := 2015 - as.integer(birth_dt)]
pt2[age <= 17, age_range := "0-17"]
pt2[age >= 18 & age <= 29, age_range := "18-29"]
pt2[age >= 30 & age <= 44, age_range := "30-44"]
pt2[age >= 45 & age <= 59, age_range := "45-59"]
pt2[age >= 60, age_range := "60+"]
smy2_age <- pt2[, .N, .(age_range)]
smy2_age[, per := N/uniqueN(pt2$INTERNAL_MEMBER_ID)]
smy2_gender <- pt2[, .N, .(GENDER_CODE)]
smy2_gender[, per := N/uniqueN(pt2$INTERNAL_MEMBER_ID)]

smy2_gender_M_age <- pt2[GENDER_CODE == "M", .N, .(age_range)]
smy2_gender_M_age[, per := N/uniqueN(pt2[GENDER_CODE == "M"]$INTERNAL_MEMBER_ID)]

smy2_gender_F_age <- pt2[GENDER_CODE == "F", .N, .(age_range)]
smy2_gender_F_age[, per := N/uniqueN(pt2[GENDER_CODE == "F"]$INTERNAL_MEMBER_ID)]
