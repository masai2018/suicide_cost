##' Identify Case Cohort Based on ICD-9 Codes
##'
##' @param dx_codes a vector of icd-9 diagnosis code strings possibly
##'     seperated by commas
##' @param need_split whether need to split codes by commas.
##' @param return_raw if \code{TURE}, return identification results from alla
##'     sub-conditions. The default is \code{FALSE}.
##'
##' @return a list of different suicide attempt indicators
isSuicideAttempt_icd9 <- function(dx_codes,
                                  need_split = TRUE,
                                  return_raw = FALSE)
{
    ## helpers
    rep0 <- function(a) {
        sapply(a, function(b) paste0(rep("0", max(0, b)), collapse = ""))
    }
    inside <- function(x, lower, upper) {
        x >= lower & x <= upper
    }
    anyInside <- function(x, lower, upper) {
        any(inside(x, lower, upper))
    }
    ## engine function
    is_sa <- function(x) {
        ## reformat the codes to five characters with trailling zero filled in
        x <- paste0(toupper(x), rep0(5 - nchar(x)))
        ## replace E with 11 and V with 12
        x <- sub("E", "11", x, fixed = TRUE)
        x <- sub("V", "12", x, fixed = TRUE)
        ## it must be wrong if it has other letters, replace them all
        x <- gsub("[A-Z]", "99999", x)
        ## convert x to numeric values
        x <- as.numeric(x)

        ## rule 1: E950-E958
        idx1 <- anyInside(x, 119500, 119589)

        ## rule 2: V6284 and (870--899 or 960--989)
        idx2_1 <- any(x == 126284)
        idx2_2 <- anyInside(x, 87000, 89999) || anyInside(x, 96000, 98999)
        idx2 <- idx2_1 && idx2_2

        ## rule 3: 881, 960--979, 980--989, 994.7 and
        ## ( 293.83, 296.20-296.36, 296.82, 296.90, 298.0, 300.4,
        ## 309.0-309.1, 311, 296.00-296.06, 296.1-296.14, 296.40-296.89,
        ## 296.99, 301.13, 301, 290.8-290.9, 295, 297, 298.1-298.9, 299,
        ## 301.20-301.22, 780.1, 309.2-309.9 )
        idx3_1 <- (any(x == 88100) || anyInside(x, 96000, 98999) ||
                   any(x == 99470))
        idx3_2 <- any(x %in% c(29383, 29682, 29800, 30040, 31100, 29699,
                               30113, 30100, 29500, 29700, 29900, 78010)) ||
            anyInside(x, 29620, 29636) || anyInside(x, 30900, 30910) ||
            anyInside(x, 29600, 29606) || anyInside(x, 29600, 29606) ||
            anyInside(x, 29610, 29614) || anyInside(x, 29640, 29689) ||
            anyInside(x, 29080, 29099) || anyInside(x, 29810, 29899) ||
            anyInside(x, 30120, 30122) || anyInside(x, 30920, 30999)
        idx3 <- idx3_1 && idx3_2

        ## rule 4: E980--E988
        idx4 <- anyInside(x, 119800, 119889)

        ## rule 5: Barak-Corren et al
        ## rule 6: Walsh et al
        idx6 <- anyInside(x, 119500, 119599)
        idx5 <- idx6 ||
            anyInside(x, 96500, 96599) || anyInside(x, 96700, 96799) ||
            anyInside(x, 96900, 96999) || anyInside(x, 88100, 88199)

        ## return
        if (return_raw) {
            setNames(c(idx1, idx2_1, idx2_2, idx3_1, idx3_2, idx4, idx5, idx6),
                     c("idx1", "idx2_1", "idx2_2", "idx3_1",
                       "idx3_2", "idx4", "idx5", "idx6"))
        } else {
            setNames(c(idx1, idx2, idx3, idx4, idx5, idx6),
                     paste0("idx", seq_len(6)))
        }
    }
    dx_codes_list <- if (need_split) {
                         strsplit(dx_codes, split = ",", fixed = TRUE)
                     } else {
                         dx_codes
                     }
    ## return
    lapply(dx_codes_list, is_sa)
}


##' Identify Case Cohorts Based on ICD-10 Codes
##'
##' @param dx_codes a vector of icd-10 diagnosis code strings possibly
##'     seperated by commas
##' @param need_split whether need to split codes by commas.
##' @param return_raw if \code{TURE}, return identification results from alla
##'     sub-conditions. The default is \code{FALSE}.
##'
##' @return a list of different suicide attempt indicators
isSuicideAttempt_icd10 <- function(dx_codes,
                                   need_split = TRUE,
                                   return_raw = FALSE)
{
    ## convert icd-10 codes back to icd-9 codes by forward and backward GEM
    dx_codes_icd9 <- touch::icd_map(dx_codes, from = 10, to = 9,
                                    method = "both")
    isSuicideAttempt_icd9(dx_codes_icd9,
                          need_split = need_split,
                          return_raw = return_raw)
}
