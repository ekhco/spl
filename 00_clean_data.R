# ------------------------------------------------------------------------------
# Risk of second PLC
#
# Eric Chow, 10-3-2018
# This script reads in the csv file, formats dates and outputs a dta file with
# meta data saved (ie: dates etc). Surgery/radiation treatment are also ascertained
#
# ------------------------------------------------------------------------------

rm(list=ls()); gc() # clean up
# load libraries
library(stringr)
library(foreign)
library(haven)
library(plyr)



# ------------------------------------------------------------------------------
# read in Summer's file. The SPL diagnosis date differs from splc.dta in a few cases
# do a few clean up steps, like fixing dates, renaming vars to replace . and turning
# variables back into factors or numeric.
#
# 2nd primary lung cancer if histology result differs from index tumor, or if it
# is the same, if dgn > 2yrs after
ss <- read.delim("/Volumes/QSU/Datasets/SEER_medicare/data/SEER.medicare_271892.csv", sep=",", colClasses = "character")

# for all date variables, turn back to date
date_var_vector <- !is.na(str_locate(names(ss), "dt")[,1]) # get array of vars that have dt
for (var in names(ss)[date_var_vector]) { # for each date var, turn it back into a date
    ss[,var] <- as.Date(ss[,var], format="%Y-%m-%d")
}

# replace . in var names to _ for file format compatibility
names(ss) <- gsub("[.]", "_", names(ss))

# turn variables back into factors
factor_vars <- c("s_sex", "race", "histbeh_IPL", "histrec_IPL", "cod10v_IPL", "site_IPL",
 "his2_IPL", "histbeh_SPL", "histrec_SPL", "cod10v_SPL", "site_SPL", "his2_SPL")
for (var in factor_vars) {
    ss[,var] <- as.factor(ss[,var])
}

# turn variables back into numeric
char_vars <- c("patient_id")
num_vars <- names(ss)[!(names(ss) %in% c(names(ss[date_var_vector]), factor_vars, char_vars))]
for (var in num_vars) {
    ss[,var] <- type.convert(ss[,var])
}


# ------------------------------------------------------------------------------
# create a histology category variable
ss$hist_cat_IPL <- ""
ss[(ss$hist_IPL %in% c(8140, 8141, 8143)), "hist_cat_IPL"] <- "AD"
ss[(ss$hist_IPL %in% c(8070, 8076)), "hist_cat_IPL"] <- "SQ"
ss[(ss$hist_IPL %in% c(8041, 8045)), "hist_cat_IPL"] <- "SC"
ss[(ss$hist_IPL %in% c(8012, 8014)), "hist_cat_IPL"] <- "LC"
ss[(ss$hist_IPL %in% c(8250, 8255)), "hist_cat_IPL"] <- "BAC"
ss[(ss$hist_IPL %in% c(8240)), "hist_cat_IPL"] <- "CARCINOID"
ss[(ss$hist_IPL %in% c(8010, 8011, 8015)), "hist_cat_IPL"] <- "CARCINOMA, NOS"
ss[(ss$hist_IPL %in% c("")), "hist_cat_IPL"] <- "OTH"

ss$hist_cat_IPL <- as.factor(ss$hist_cat_IPL)

# ------------------------------------------------------------------------------
# define AJ3SR
aj3sr <- c(0, 10, 11, 12, 13, 19, 20, 21, 22, 23, 29, 30, 31, 32, 33, 39, 40, 41, 42, 49, 90, 98)
aj3sr_lab <- c("in situ", "I", "IA", "IB", "IC", "I, NOS", "II", "IIA", "IIB", "IIC", "II, NOS", "III", "IIIA", "IIIB", "IIIC", "III, NOS", "IV", "IVA", "IVB", "IV, NOS", "unstaged", "na")
aj3sr_dict <- data.frame(aj3sr_IPL = aj3sr, aj3sr_lab_IPL = aj3sr_lab)
summary(aj3sr_dict)

ss <- join(ss, aj3sr_dict, by="aj3sr_IPL", type="left", match="first")
summary(ss$aj3sr_lab_IPL)

# ------------------------------------------------------------------------------
# define grade
label define grade

grade <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
grade_lab <- c("grade I", "grade II", "grade III", "grade IV", "T-cell", "B-cell", "Null cell", "NK cell", "cell type not determined")
grade_dict <- data.frame(grade_IPL = grade, grade_lab_IPL = grade_lab)
summary(grade_dict)

ss <- join(ss, grade_dict, by="grade_IPL", type="left", match="first")
summary(ss$grade_lab_IPL)

# ------------------------------------------------------------------------------
# define laterality

lat <- c(0, 1, 2, 3, 4, 5, 9)
lat_lab <- c("not paired site", "right", "left", "only one side involved, R/L unspecified", "bilateral involvement", "paired site, midline", "paired site, but no laterality info")
lat_dict <- data.frame(lat_IPL = lat, lat_lab_IPL = lat_lab)
summary(lat_dict)

ss <- join(ss, lat_dict, by="lat_IPL", type="left", match="first")
summary(ss$lat_lab_IPL)



# ------------------------------------------------------------------
# merge in the chronic conditions now, by patient ID and year of dgn
# COMORBIDITIES: (Dutkowska, Antczak 2016)
# COPD, Pneumonia, Silicosis, Residual TB
# hypertension, coronary artery disease,
# peripheral vascular disease, arrhythmia,
# abdominal aortic aneurysm,
# ischemic heart disease, cerebrovascular disease
# chronic obstructive pulmonary disease (COPD)
# diabetes mellitus (DM)

cc <- read_dta("/Volumes/QSU/Datasets/SEER_medicare/CCflag/CCflag_small.dta")

ss$bene_enrollmt_ref_yr <- ss$yrdx__IPL
ss  <- join(ss, cc[ ,c("ami", "asthma", "atrial_fib", "chf", "copd", "diabetes", "hypert", "ischemicheart", "stroke_tia")],
            by=c("patient_id", "bene_enrollmt_ref_yr"), type="left", match="first")
# drop bene_enrollmt_ref_yr


# ------------------------------------------------------------------
# Define treatment:
#
# radsurg1 "for patients who had BOTH surgery and radiation, the order of surgery/radiation"
label define radsurg 0 "No radiation and/or surgery as defined above" 2 "Radiation before surgery" 3 "Radiation after surgery" 4 "Radiation both before and after surgery" 5 "Intraoperative radiation therapy" 6 "Intraoperative radiation with other radiation given before or after surgery" 9 "Sequence unknown, but both surgery and radiation were given"
label values radsurg1 radsurg
# 0 No radiation and/or surgery as defined above
# 2 Radiation before surgery
# 3 Radiation after surgery
# 4 Radiation both before and after surgery
# 5 Intraoperative radiation therapy
# 6 Intraoperative radiation with other radiation given before or after surgery
# 9 Sequence unknown, but both surgery and radiation were given

* label no surg
label var nosrg1 "reason that surgery not performed on primary site"
label define surg 0 "Surgery performed" 1 "Surgery not recommended" 2 "Contraindicated due to other conditions; Autopsy Only case" 5 "Patient died before recommended surgery" 6 "Unknown reason for no surgery" 7 "Patient or patient's guardian refused" 8 "Recommended, unknown if done" 9 "Unknown if surgery performed; Death Certificate Only case"
label values nosrg1 surg
# 0 Surgery performed
# 1 Surgery not recommended
# 2 Contraindicated due to other conditions; Autopsy Only case
# 5 Patient died before recommended surgery
# 6 Unknown reason for no surgery
# 7 Patient or patient's guardian refused
# 8 Recommended, unknown if done
# 9 Unknown if surgery performed; Death Certificate Only case

* label rad
label var rad1 "method of radiation therapy performed as part of the first course of treatment"
label define rad 0 "None; diagnosed at autopsy" 1 "Beam radiation" 2 "Radioactive implants" 3 "Radioisotopes" 4 "Combination of 1 with 2 or 3" 5 "Radiation, NOS – method or source not specified" 6 "Other radiation (1973-1987 cases only)" 7 "Patient or patient’s guardian refused radiation therapy" 8 "Radiation recommended, unknown if administered" 9 "Unknown if radiation administered"
label values rad1 rad
# 0 None; diagnosed at autopsy
# 1 Beam radiation
# 2 Radioactive implants
# 3 Radioisotopes
# 4 Combination of 1 with 2 or 3
# 5 Radiation, NOS – method or source not specified
# 6 Other radiation (1973-1987 cases only)
# 7 Patient or patient’s guardian refused radiation therapy
# 8 Radiation recommended, unknown if administered
# 9 Unknown if radiation administered

* ----------------------------------------------
* assign treatment categories
* ----------------------------------------------
capture drop init_tx1
gen init_tx1 = "n/a"
label var init_tx1 "composite of nosrg, rad, and radsurg"

replace init_tx1 = "surg only" if nosrg1 == 0 & inlist(rad1,0,7)
replace init_tx1 = "rad only"  if inlist(rad1,1,2,3,4,5,6) & !(inlist(nosrg1,  0,8,9))
replace init_tx1 = "surg, then rad"  if radsurg1 == 3
replace init_tx1 = "other surg+rad comb" if inlist(radsurg1, 2, 4, 5, 6)
replace init_tx1 = "no surg or rad" if inlist(nosrg1, 1,2,5,6,7) & inlist(rad1, 0, 7) #& init_tx1 == "n/a"
replace init_tx1 = "pt refused rad or surg" if nosrg1 == 7 & rad1 == 7
replace init_tx1 = "unknown if surg or rad done" if inlist(nosrg1, 8, 9) & inlist(rad1, 8, 9)
* look at crosstab
tab init_tx1

* look at cases yet unclassified
tab nosrg1 rad1 if init_tx1 == "n/a"
* fix odd cases
replace init_tx1 = "other surg+rad comb" if inlist(nosrg1, 0) & inlist(rad1, 1,2,3,4,5) & init_tx1 == "n/a"
* assume unknown if unknown surgery, and no rad
replace init_tx1 = "unknown if surg or rad done" if inlist(nosrg1,8,9) & rad1 == 0 & init_tx1 == "n/a"
* assume unknown if unknown rad, and no surg
replace init_tx1 = "unknown if surg or rad done" if inlist(rad1,8,9) & inlist(nosrg1,1,2,5,6) & init_tx1 == "n/a"
* assume rad only, if known rad, but unknown surg
replace init_tx1 = "rad only"  if inlist(rad1,1,2,3,4,5,6) & inlist(nosrg1, 8,9) & init_tx1 == "n/a"
* assume surg only, if known surg, but unknown rad
replace init_tx1 = "surg only" if inlist(nosrg1, 0) & inlist(rad1, 8,9) & init_tx1 == "n/a"
* assume refusal if surg or rad refused, and rad or surg unknown
replace init_tx1 = "pt refused rad or surg" if inlist(nosrg1, 7) & inlist(rad1, 8,9)  & init_tx1 == "n/a"
replace init_tx1 = "pt refused rad or surg" if inlist(nosrg1, 8,9) & inlist(rad1, 7)  & init_tx1 == "n/a"

* look at crosstab
tab init_tx1
encode init_tx1, gen(init_tx1_)
drop init_tx1
rename init_tx1_ init_tx1






# ------------------------------------------------------------------
# merge in FIPS codes of state
fp <- read_dta("/Users/echow/QSU/SEER-medicare/data/state_fips.dta")
ss$fips <- ss$state
join(ss, fp, by=c("fips"), type="left", match="first")

uhhhh state needs to be renamed First
ss$state <- factor(ss$state)



# ------------------------------------------------------------------------------
# the initial PLC is defined by diag_dt.IPL
# the second  PLC is defined by diag_dt.SPL (and was ascertained by Summer Han)
# ascertain the outcome: 0 - uknown (ie: censored at end of study)
#                        1 - SPL    (outcome of interest)
#                        2 - death before SPL (competing risk)

# These are important variables

    agedx1 = "Age at index LC"
    frstprm1 = "First primary malignant"
    hist_cat1 = "histology at index LC"
    hist_cat2 = "histology at 2nd LC"
    hist_cat3 = "histology at 3rd LC"
    hist_cat4 = "histology at 4th LC"
    histrec1 = "histology"
    lat1 = "laterality"
    yrdx_1 = "year of diagnosis"
    aj3sr1 = "SEER modified AJCC stage 3ed"
    grade1 = "Grade"
    e10sz1 = "Tumor size"
    e10ex1 = "Extension"
    e10pe1 = "Extension prostate path"
    e10nd1 = "Lymph node involvement"
    e10ne1 = "Regional nodes examined"
    e10pn1 = "Regional nodes positive"


# ------------------------------------------------------------------------------
# save out to a cleaned up data
write_dta(ss, "/Volumes/QSU/Datasets/SEER_medicare/data/SEER.medicare_271892.dta")
# compress in Stata afterwards
