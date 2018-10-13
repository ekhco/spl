# ------------------------------------------------------------------------------
# Risk of second PLC
#
# Eric Chow, 10-3-2018
# This script reads in the csv file, formats dates and outputs a dta file with
# meta data saved (ie: dates etc). Surgery/radiation treatment are also ascertained
# and an analytical file is written.
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
# variables back into factors or numeric. 2nd primary lung cancer if histology result
# differs from index tumor, or if it is the same, if dgn > 2yrs after

if (1==0) {ss <- read.delim("/Volumes/QSU/Datasets/SEER_medicare/data/SEER.medicare_271892.csv", sep=",", colClasses = "character")}
ss <- read.delim("~/DATA_splc/SEER.medicare_271892.csv", sep=",", colClasses = "character")
ss_raw <- ss

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

# ------------------------------------------------------------------------------
# define LC-related COD
summary(cod10v_IPL)
# TODO


# ------------------------------------------------------------------
# merge in the chronic conditions now, by patient ID and year of dgn
# COMORBIDITIES: (Dutkowska, Antczak 2016)
# COPD, Pneumonia, Silicosis, Residual TB, hypertension, coronary artery disease,
# peripheral vascular disease, arrhythmia, abdominal aortic aneurysm,
# ischemic heart disease, cerebrovascular disease, chronic obstructive pulmonary disease (COPD)
# diabetes mellitus (DM)

# CC is too big for R, so make a subset of CC for 271,892
if (1==0) {pt_id <- read.delim("/Volumes/QSU/Datasets/SEER_medicare/data/patient_ids.csv", sep=",", colClasses=c("character","numeric"))}
pt_id <- read.delim("~/DATA_splc/patient_ids.csv", sep=",", colClasses=c("character","numeric"))
names(pt_id) <- c("patient_id", "indic")
if (1==0) {write.dta(pt_id, "/Volumes/QSU/Datasets/SEER_medicare/data/patient_ids.dta")}
write.dta(pt_id, "~/DATA_splc/patient_ids.dta")

#     ... run some Stata code b/c CC is too big for R ...

# cc <- read_dta("/Volumes/QSU/Datasets/SEER_medicare/CCflag/CCflag_small.dta")}
# cc <- read_dta("/Volumes/QSU/Datasets/SEER_medicare/CCflag/CCflag_small.dta")}
if (1==0) {cc <- data.frame(read_dta("/Volumes/QSU/Datasets/SEER_medicare/CCflag/CCflag_small_271892.dta"))}
cc <- data.frame(read_dta("~/DATA_splc/CCflag_small_271892.dta"))

# merge in chronic conditions by patient_id and the benefit year
ss$bene_enrollmt_ref_yr <- ss$yrdx__IPL
ss  <- join(ss, cc[ ,c("patient_id", "bene_enrollmt_ref_yr", "ami", "asthma", "atrial_fib", "chf", "copd", "diabetes", "hypert", "ischemicheart", "stroke_tia")],
            by=c("patient_id", "bene_enrollmt_ref_yr"), type="left", match="first")
ss$bene_enrollmt_ref_yr <- NULL  # drop bene_enrollmt_ref_yr
head(ss)


# ------------------------------------------------------------------
# Define treatment:
# radsurg1 "for patients who had BOTH surgery and radiation, the order of surgery/radiation"
table(ss$radsurg_IPL)
# 0 No radiation and/or surgery as defined above
# 2 Radiation before surgery
# 3 Radiation after surgery
# 4 Radiation both before and after surgery
# 5 Intraoperative radiation therapy
# 6 Intraoperative radiation with other radiation given before or after surgery
# 9 Sequence unknown, but both surgery and radiation were given

# label var nosrg1 "reason that surgery not performed on primary site"
table(ss$nosrg_IPL)
# 0 Surgery performed
# 1 Surgery not recommended
# 2 Contraindicated due to other conditions; Autopsy Only case
# 5 Patient died before recommended surgery
# 6 Unknown reason for no surgery
# 7 Patient or patients guardian refused
# 8 Recommended, unknown if done
# 9 Unknown if surgery performed; Death Certificate Only case

# label var rad1 "method of radiation therapy performed as part of the first course of treatment"
table(ss$rad_IPL)
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

# ------------------------------------------------------------------------------
# assign treatment categories
# init_tx_IPL "composite of nosrg, rad, and radsurg"
# ------------------------------------------------------------------------------
ss[ ,"init_tx_IPL"]                                                                 <- "NA"
ss[(ss$nosrg_IPL == 0) & (ss$rad_IPL %in% c(0,7)) , "init_tx_IPL"]                  <- "surg only"
ss[(ss$rad_IPL %in% c(1,2,3,4,5,6) & !(ss$nosrg_IPL %in% c(0,8,9))) ,"init_tx_IPL"] <- "rad only"
ss[(ss$radsurg_IPL == 3) ,"init_tx_IPL"]                                            <- "surg, then rad"
ss[(ss$radsurg_IPL %in% c(2,4,5,6)) ,"init_tx_IPL"]                                 <- "other surg+rad comb"
ss[(ss$nosrg_IPL %in% c(1,2,5,6,7) & ss$rad_IPL %in% c(0,7)) , "init_tx_IPL"]       <- "no surg or rad"
ss[(ss$nosrg_IPL == 7 & ss$rad_IPL  == 7) , "init_tx_IPL"]                          <- "pt refused rad or surg"
ss[(ss$nosrg_IPL %in% c(8,9) & ss$rad_IPL %in% c(8,9)) , "init_tx_IPL"]             <- "unknown if surg or rad done"

# ----------- Stata was:
# replace init_tx1 = "surg only"                  if nosrg1 == 0 & inlist(rad1,0,7)
# replace init_tx1 = "rad only"                   if inlist(rad1,1,2,3,4,5,6) & !(inlist(nosrg1,  0,8,9))
# replace init_tx1 = "surg, then rad"             if radsurg1 == 3
# replace init_tx1 = "other surg+rad comb"        if inlist(radsurg1, 2, 4, 5, 6)
# replace init_tx1 = "no surg or rad"             if inlist(nosrg1, 1,2,5,6,7) & inlist(rad1, 0, 7) #& init_tx1 == "n/a"
# replace init_tx1 = "pt refused rad or surg"     if nosrg1 == 7 & rad1 == 7
# replace init_tx1 = "unknown                     if surg or rad done" if inlist(nosrg1, 8, 9) & inlist(rad1, 8, 9)

table(ss$init_tx_IPL)

# look at cases not yet classified
table(ss[ss$init_tx_IPL == "NA", "nosrg_IPL"],  ss[ss$init_tx_IPL == "NA", "rad_IPL"])

# fix odd cases
ss[(ss$init_tx_IPL == "NA" & ss$nosrg_IPL == 0 & ss$rad_IPL %in% c(1,2,3,4,5)) ,"init_tx_IPL"]           <- "other surg+rad comb"
# assume unknown if unknown surgery, and no rad
ss[(ss$init_tx_IPL == "NA" & ss$nosrg_IPL %in% c(8,9) & ss$rad_IPL == 0) ,"init_tx_IPL"]                 <- "unknown if surg or rad done"
# assume unknown if unknown rad, and no surg
ss[(ss$init_tx_IPL == "NA" & ss$nosrg_IPL %in% c(1,2,5,6) & ss$rad_IPL %in% c(8,9)) ,"init_tx_IPL"]      <- "unknown if surg or rad done"
# assume rad only, if known rad, but unknown surg
ss[(ss$init_tx_IPL == "NA" & ss$nosrg_IPL %in% c(8,9) & ss$rad_IPL %in% c(1,2,3,4,5,6)) ,"init_tx_IPL"]  <- "rad only"
# assume surg only, if known surg, but unknown rad
ss[(ss$init_tx_IPL == "NA" & ss$nosrg_IPL == 0 & ss$rad_IPL %in% c(8,9)) ,"init_tx_IPL"]                 <- "surg only"
# assume refusal if surg or rad refused, and rad or surg unknown
ss[(ss$init_tx_IPL == "NA" & ss$nosrg_IPL %in% c(7) & ss$rad_IPL %in% c(8,9)) ,"init_tx_IPL"]            <- "pt refused rad or surg"
ss[(ss$init_tx_IPL == "NA" & ss$nosrg_IPL %in% c(8,9) & ss$rad_IPL %in% c(7)) ,"init_tx_IPL"]            <- "pt refused rad or surg"



# ------------ Stata was:
# replace init_tx1 = "other surg+rad comb" if inlist(nosrg1, 0) & inlist(rad1, 1,2,3,4,5) & init_tx1 == "n/a"
# replace init_tx1 = "unknown if surg or rad done" if inlist(nosrg1,8,9) & rad1 == 0 & init_tx1 == "n/a"
# replace init_tx1 = "unknown if surg or rad done" if inlist(rad1,8,9) & inlist(nosrg1,1,2,5,6) & init_tx1 == "n/a"
# replace init_tx1 = "rad only"  if inlist(rad1,1,2,3,4,5,6) & inlist(nosrg1, 8,9) & init_tx1 == "n/a"
# replace init_tx1 = "surg only" if inlist(nosrg1, 0) & inlist(rad1, 8,9) & init_tx1 == "n/a"
# replace init_tx1 = "pt refused rad or surg" if inlist(nosrg1, 7) & inlist(rad1, 8,9)  & init_tx1 == "n/a"
# replace init_tx1 = "pt refused rad or surg" if inlist(nosrg1, 8,9) & inlist(rad1, 7)  & init_tx1 == "n/a"

# look at crosstab
ss$init_tx_IPL <- as.factor(ss$init_tx_IPL)
table(ss$init_tx_IPL)/nrow(ss)


# ------------------------------------------------------------------------------
# merge in FIPS codes of state
fp <- read.dta("/Users/echow8/QSU/SEER-medicare/data/state_fips.dta")
ss$fips <- ss$state_IPL
ss <- join(ss, fp, by=c("fips"), type="left", match="first")
ss$state_cd_IPL   <- factor(ss$state)
ss$state_desc_IPL <- factor(ss$state_desc)
ss$state <- ss$state_desc <- NULL
names(ss)


# ------------------------------------------------------------------------------
# save out to a cleaned up data
if (1==0) {write_dta(ss, "/Volumes/QSU/Datasets/SEER_medicare/data/SEER.medicare_271892.dta")}
write_dta(ss, "~/DATA_splc/SEER.medicare_271892.dta")

        # compress in Stata afterwards
        # compress
        # saveold "~/DATA_splc/SEER.medicare_271892.dta", v(12) replace

#      ~ fin ~
