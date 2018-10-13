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
library(cmprsk)
library(mstate)

# ------------------------------------------------------------------------------
# read in cleaned dataset
setwd("~/QSU/SEER-medicare/splc")
ss <- data.frame(read.dta("~/DATA_splc/SEER.medicare_271892.dta"))





# ------------------------------------------------------------------------------
# the initial PLC is defined by diag_dt.IPL
# the second  PLC is defined by diag_dt.SPL (and was ascertained by Summer Han)

summary(ss$diag_dt_IPL)
summary(ss$diag_dt_SPL)
summary(ss$med_death_dt)
ss$censor_dt <- as.Date("01-25-2016", format="%m-%d-%Y") # censored date at max death date

# ascertain the first event - min date of SPL diagnosis, death, or censorship
ss$event_dt <- as.Date(apply(ss[,c("diag_dt_SPL", "med_death_dt", "censor_dt")] , 1, function(x) min(x, na.rm=TRUE)))
summary(ss$event_dt)

# calculate time to event in years
ss$t_event <- as.numeric(ss$event_dt - ss$diag_dt_IPL)/365.24

# some are negative... why? 2.9% died before IPL
neg_ss <- ss[ss$t_event < 0, ]
nrow(neg_ss) # 8000
head(neg_ss[ , c("diag_dt_IPL","diag_dt_SPL", "med_death_dt", "censor_dt", "t_event", "init_tx_IPL")],10)

# make t == 0
ss[ss$t_event <= 0, "t_event" ] <- 0

# Need to reconsider population! Should we select people who had IPL, but were treated
# and survived to be at risk for developing SPLC?



# ------------------------------------------------------------------------------
# ascertain the outcome: 4 - uknown (ie: censored at end of study)
#                        1 - SPL    (outcome of interest)
#                        2 - death before SPL (competing risk)
#                        3 - death/SPL same date

ss[ , "event"] <- NA
ss[(ss$event_dt == ss$diag_dt_SPL) & !is.na(ss$diag_dt_SPL), "event"] <- 1  # SPLC
ss[(ss$event_dt == ss$med_death_dt) & !is.na(ss$med_death_dt), "event"] <- 2  # death first
ss[(ss$event_dt == ss$med_death_dt) & (ss$event_dt == ss$diag_dt_SPL) & !is.na(ss$diag_dt_SPL) & !is.na(ss$med_death_dt), "event"] <- 3  # death and SPL at same time
ss[(ss$event_dt == ss$censor_dt) & is.na(ss$med_death_dt) & is.na(ss$diag_dt_SPL), "event"] <- 0 # censored

# there are 2 cases where the death date and SPL are thh same
ss[ ss$event == 3 ,c("diag_dt_IPL","event_dt","diag_dt_SPL", "med_death_dt", "censor_dt")]
# treat them as SPL? ********* CHECK W SUMMER **********
ss[ ss$event == 3, "event"] <- 2

# how many died on the same day as IPL?
sum(as.numeric(ss$diag_dt_IPL == ss$med_death_dt), na.rm=TRUE) # 885
head(ss[ss$diag_dt_IPL == ss$med_death_dt & !is.na(ss$med_death_dt), c("patient_id","diag_dt_IPL", "med_death_dt", "event", "init_tx_IPL", "rad_IPL", "nosrg_IPL")], 10)

# diagnosed w SPL before or on same date as IPL
head(ss[(ss$diag_dt_SPL <= ss$diag_dt_IPL & !is.na(ss$diag_dt_SPL)), c("diag_dt_IPL","diag_dt_SPL", "med_death_dt", "censor_dt","event", "t_event", "init_tx_IPL")], 10)

# how many where IPL diagnosed at Autopsy or Death certificate?
nrow(ss[ss$rad_IP == 0 | ss$nosrg_IPL %in% c(2,9), ]) #182,695

# time to event of splc
summary(ss[ss$event==1, "t_event"]) # SPLC
summary(ss[ss$event==2, "t_event"]) # Died
summary(ss[ss$event==0, "t_event"]) # censored

# table of events
table(ss$event)

# ------------------------------------------------------------------------------
# Cumulative incidence plot - mortality & SPLC
# ------------------------------------------------------------------------------
plot <- FALSE
if (plot == TRUE) {	# do cumulative incidence
	splc_mort <- Cuminc(time = ss$t_event, status = ss$event, failcodes=c(1,2))
	head(splc_mort)

	# get each probability
	wo <- summary(splc_mort)

	# plot variables
	t    <- wo$time
	splc <- wo$pstate[,1] # SPLC
	died <- wo$pstate[,2] # death

	# -----------------------------------------------------
	# Overall competing risk figure
	# -----------------------------------------------------

	pdf(file="figs/overall_cif.pdf", width=7, height=6)
		# plot SPLC CIF
		plot(splc ~ t, type="S", xlim=c(0,10), ylim=c(0,1), yaxt="n",xaxt="n",
	      lty=1, col="tomato", lwd=1,
	      main="Outcomes after IPL", xlab="time since IPL (years)", ylab="cumulative incidence")

		# plot "stacked" death CIF on top of SPLC CIF
		lines((splc+died)~t, type="S", col="black", lty=2, lwd=1)

		axis(side=1,at=0:10, labels=0:10, cex.axis=0.85)
		axis(side=2,at=c(0,1:10)/10, labels=c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"), las=1, cex.axis=0.75)
		legend(0,1,legend=c("SPLC","Died before SPLC"), lty=c(1,2), lwd=c(1,1) , col=c("tomato", "black"), bty="n", cex=0.8)

	dev.off()
}

# ------------------------------------------------------------------------------
# Competing risk regression
# ------------------------------------------------------------------------------

ransamp <- sample(1:nrow(ss), size=10000, replace=FALSE)
ss_ <- ss[ransamp, ]
table(ss_$event)
write.dta(ss_, "~/DATA_splc/ss_.dta")

# competing risk regression (Fine & Gray)
z <- crr(
	ftime = ss_$t_event, fstatus = ss_$event,
	failcode = 1 , cencode = 0,
	cov1 = ss_[ ,"agedx_IPL"]
); summary(z)
#
# Competing Risks Regression
#
# Call:
# crr(ftime = ss_$t_event, fstatus = ss_$event, cov1 = ss_[, "agedx_IPL"],
#     failcode = 1, cencode = 0)
#
#                       coef exp(coef) se(coef)     z p-value
# ss_[, "agedx_IPL"]1 -0.084     0.919   0.0114 -7.39 1.5e-13
#
#                     exp(coef) exp(-coef)  2.5% 97.5%
# ss_[, "agedx_IPL"]1     0.919       1.09 0.899  0.94
#
# Num. cases = 10000
# Pseudo Log-likelihood = -1577
# Pseudo likelihood ratio test = 48.5  on 1 df,

z <- crr(
	ftime = ss_$t_event, fstatus = ss_$event,
	failcode = 1 , cencode = 0,
	cov1 = as.numeric(ss_[ ,"s_sex"])
); summary(z)

# these are some important variables

"demographics"
		agedx1 = "Age at index LC"
		s_sex
		srace_IPL
		frstprm1 = "First primary malignant"
		hist_cat1 = "histology at index LC"
		histrec1 = "histology"
		lat1 = "laterality"
		yrdx_1 = "year of diagnosis"
		aj3sr1 = "SEER modified AJCC stage 3ed"
		grade1 = "Grade"
		e10sz1 = "Tumor size"
		e10ex1 = "Extension"
		e10nd1 = "Lymph node involvement"
		e10ne1 = "Regional nodes examined"
		e10pn1 = "Regional nodes positive"
		e10pe1 = "Extension prostate path"
"family history of lung cancer"
"personal history of cancer"
"comorbidity (e.g. COPD)"
"geographic factors"
