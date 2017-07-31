# --------------------------------------------------------
#                  ERROR PROPAGATION
#  Author: Daniele Ferraretto
#  started on 26th July, 2017
#  updated:                        last update: 31/07/2017
# --------------------------------------------------------
# --------------------------------------------------------
# clear the memory
rm(list=ls())

.libPaths("C:/Workspace/R") # adjust the issues with my desktop demmerda
### set working dir for pc:
#setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
#setwd("M:/My PhD/R/PhD-local_repo")
setwd("C:/Users/s1373890/Daniele_Repo")

##########          THROUGHFALL PROPAGATION ERROR          #############

# prima prova. 1) SD e mean per ogni sampling date, 
# 2) poi valore giornaliero come errore prova/ numero giorni per giorni utili
# 3) Addition: semplice radice quadrata dei due valori di errore di cui da 2

##########              ERROR IN TF DEPTH                  #############
library(RSQLite)

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

tf  = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' AND vals>=0 ORDER BY date")

NO3data = dbGetQuery(db, "SELECT * FROM labdata WHERE VALS >= 0 AND variable = 'NO3.N' ORDER BY date")
NH4data = dbGetQuery(db, "SELECT * FROM labdata WHERE VALS >= 0 AND variable = 'NH4.N' ORDER BY date")


# 1: mean depth value by sampling date 

tf.depth.mean=aggregate(vals ~ date, data = tf, FUN = mean, na.rm = TRUE )
names(tf.depth.mean) = c("date", "TF.depth.mean")

# 2: SD of depth value by sampling date

tf.depth.SD=aggregate(vals ~ date, data = tf, FUN = sd, na.rm = TRUE )
names(tf.depth.SD) = c("date", "TF.depth.SD")


##########              ERROR IN TF N LAB VALS                  #############

TF.coll = c("C10T1", "C10T2", "C10T3", "C11T1", "C11T2", "C11T3", "C12T1", "C12T2", "C12T3", "T10T1", 
           "T10T2", "T10T3", "T11T1", "T11T2", "T11T3", "T12T1", "T12T2", "T12T3")

library(data.table)

NO3.TF = NO3data[NO3data$sample %in% TF.coll, ]
NH4.TF = NH4data[NH4data$sample %in% TF.coll, ]

# 3: mean of lab values by sampling date

NO3.TF.mean = aggregate(vals ~ date, data = NO3.TF, FUN = mean, na.rm = TRUE )
names(NO3.TF.mean) = c("date", "TF.NO3.mean")
NH4.TF.mean = aggregate(vals ~ date, data = NH4.TF, FUN = mean, na.rm = TRUE )
names(NH4.TF.mean) = c("date", "TF.NH4.mean")


#4: SD of lab values by sampling date

NO3.TF.SD = aggregate(vals ~ date, data = NO3.TF, FUN = sd, na.rm = TRUE )
names(NO3.TF.SD) = c("date", "TF.NO3.SD")

NH4.TF.SD = aggregate(vals ~ date, data = NH4.TF, FUN = sd, na.rm = TRUE )
names(NH4.TF.SD) = c("date", "TF.NH4.SD")

# 5. Calculate the error per each sampling date: MULTIMERGE

dTF.samplingdate = merge(merge(merge(merge(merge(NO3.TF.mean, NO3.TF.SD, by='date', all=T), 
                               NH4.TF.mean, by='date', all=T), NH4.TF.SD, by='date', all=T),
                               tf.depth.mean, by='date', all=T),tf.depth.SD, by='date', all=T)

# 5a: Error propagation on NO3.N in TF by sampling date:
dTF.samplingdate$dTF.NO3 = dTF.samplingdate$TF.depth.mean * dTF.samplingdate$TF.NO3.mean *
  ((dTF.samplingdate$TF.NO3.SD/dTF.samplingdate$TF.NO3.mean)^2+(dTF.samplingdate$TF.depth.SD/dTF.samplingdate$TF.depth.mean)^2)^0.5


# 5b: Error propagation on NH4.N in TF by sampling date:
dTF.samplingdate$dTF.NH4 = dTF.samplingdate$TF.depth.mean * dTF.samplingdate$TF.NH4.mean *
  ((dTF.samplingdate$TF.NH4.SD/dTF.samplingdate$TF.NH4.mean)^2+(dTF.samplingdate$TF.depth.SD/dTF.samplingdate$TF.depth.mean)^2)^0.5


#####################################################################################
#########   REFINING ERROR PROPAGATION in TF: ERROR PROPAGATION BY MONTH   ##########

# Rationale: working on partial errors weighed by the number of days of each month
# covered by the n sampling date needs to 1) calculate how many days of each sampling date fall in a month;
# 2. calculate the weighed error; 3. propagation error as sum of the weighed errors

# 1. Calculating how many days from previous sampling:

dates=as.Date(unique(dTF.samplingdate$date))

date.end.month <- seq(as.Date("2011-11-01"),length=66,by="months")-1

dates2 = c(dates,date.end.month)
dates2= as.data.frame.Date(dates2)
library(dplyr)
dates = dates2 %>% distinct(dates2) # this is to select unique values from dates2
dates = dates[order(dates$dates2, decreasing = FALSE ),]


days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
dd.dates=cbind(dates,days)
dd.dates = as.data.frame(dd.dates)
dd.dates$dates = as.Date(dd.dates$dates, origin="1970-01-01") # I ignore why I need to put this date as origin. Calculated via excel.
dTF.samplingdate$date = as.Date(dTF.samplingdate$date)
dTF.samplingdate1 = merge(dTF.samplingdate,dd.dates,by.x="date",by.y="dates",na.rm=FALSE, all = T)

# filling the NA to make calculations
library(zoo)
dTF.samplingdate1 = na.locf(dTF.samplingdate1,fromLast = TRUE)
dTF.samplingdate1$Ym = strftime(dTF.samplingdate1$date, "%Y%m")
# Calculate the number of days of a month
# install.packages("Hmisc")
library(Hmisc)
dTF.samplingdate1$daysxmonth = monthDays(as.Date(dTF.samplingdate1$date))

# remove the last row
dTF.samplingdate1 = head(dTF.samplingdate1, -1)

dTF.samplingdate1$days = as.numeric(dTF.samplingdate1$days)
cols.num <- c("TF.NO3.mean","TF.NO3.SD","TF.NH4.mean", "TF.NH4.SD","TF.depth.mean", "TF.depth.SD", "days")
dTF.samplingdate1[cols.num] <- sapply(dTF.samplingdate1[cols.num],as.numeric)
summary(dTF.samplingdate1)

# 2a: Error propagation on NO3.N in TF:
dTF.samplingdate1$dTF.NO3 = dTF.samplingdate1$TF.depth.mean * dTF.samplingdate1$TF.NO3.mean *
  ((dTF.samplingdate1$TF.NO3.SD/dTF.samplingdate1$TF.NO3.mean)^2+(dTF.samplingdate1$TF.depth.SD/dTF.samplingdate1$TF.depth.mean)^2)^0.5 *
  dTF.samplingdate1$days/dTF.samplingdate1$daysxmonth


# 2b: Error propagation on NH4.N in TF:
dTF.samplingdate1$dTF.NH4 = dTF.samplingdate1$TF.depth.mean * dTF.samplingdate1$TF.NH4.mean *
  ((dTF.samplingdate1$TF.NH4.SD/dTF.samplingdate1$TF.NH4.mean)^2+(dTF.samplingdate1$TF.depth.SD/dTF.samplingdate1$TF.depth.mean)^2)^0.5 *
  dTF.samplingdate1$days/dTF.samplingdate1$daysxmonth

# 3. PROPAGATION ERROR AS SUM OF ERRORS FROM DIFFERENT SAMPLING DATES
# wide to long
library(reshape2)
TF.err.propag = melt(dTF.samplingdate1, id.vars = c("date", "Ym"), 
                     measure.vars = c("dTF.NH4", "dTF.NO3"), variable.name = "error_propagation_var")
# Calculate dn^2
TF.err.propag$value = (TF.err.propag$value)^2

dTF.err = aggregate(TF.err.propag$value ~  TF.err.propag$Ym + TF.err.propag$error_propagation_var, FUN = sum)
names(dTF.err) = c("Ym", "variable", "value")
dTF.err$value= (dTF.err$value)^0.5

#housekeeping
rm(dates2, dd.dates, dTF.samplingdate, dTF.samplingdate1, NH4.TF, NH4.TF.mean, NH4.TF.SD, NO3.TF, NO3.TF.mean, NO3.TF.SD, tf, tf.depth.mean,
   tf.depth.SD, TF.err.propag, cols.num, date.end.month, dates, days, diffdays, TF.coll)
##############################################################################

############            STEMFLOW PROPAGATION ERROR             ###############

# prima prova. 1) SD e mean per ogni sampling date, 
# 2) poi valore giornaliero come errore prova/ numero giorni per giorni utili
# 3) Addition: semplice radice quadrata dei due valori di errore di cui da 2

##########                    ERROR IN SF vol                    #############


library(RSQLite)

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

sf  = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'stem vol' AND vals>=0 ORDER BY date")


# 1: mean vol value by sampling date 

sf.vol.mean=aggregate(vals ~ date, data = sf, FUN = mean, na.rm = TRUE )
names(sf.vol.mean) = c("date", "SF.depth.mean")

# 2: SD of depth value by sampling date

sf.vol.SD=aggregate(vals ~ date, data = sf, FUN = sd, na.rm = TRUE )
names(sf.vol.SD) = c("date", "SF.depth.SD")


##########              ERROR IN TF N LAB VALS                  #############

SF.coll = c("C10S1", "C10S2", "C10S3", "C11S1", "C11S2", "C11S3", "C12S1", "C12S2", "C12S3", "T10S1", 
            "T10S2", "T10S3", "T11S1", "T11S2", "T11S3", "T11S4", "T11S5", "T11S6", "T11S7", "T12S1", "T12S2", "T12S3")

library(data.table)

NO3.SF = NO3data[NO3data$sample %in% SF.coll, ]
NH4.SF = NH4data[NH4data$sample %in% SF.coll, ]

# 3: mean of lab values by sampling date

NO3.SF.mean = aggregate(vals ~ date, data = NO3.SF, FUN = mean, na.rm = TRUE )
names(NO3.SF.mean) = c("date", "SF.NO3.mean")
NH4.SF.mean = aggregate(vals ~ date, data = NH4.SF, FUN = mean, na.rm = TRUE )
names(NH4.SF.mean) = c("date", "SF.NH4.mean")


#4: SD of lab values by sampling date

NO3.SF.SD = aggregate(vals ~ date, data = NO3.SF, FUN = sd, na.rm = TRUE )
names(NO3.SF.SD) = c("date", "SF.NO3.SD")

NH4.SF.SD = aggregate(vals ~ date, data = NH4.SF, FUN = sd, na.rm = TRUE )
names(NH4.SF.SD) = c("date", "SF.NH4.SD")

# 5. Calculate the error per each sampling date: MULTIMERGE

dSF.samplingdate = merge(merge(merge(merge(merge(NO3.SF.mean, NO3.SF.SD, by='date', all=T), 
                                           NH4.SF.mean, by='date', all=T), NH4.SF.SD, by='date', all=T),
                               sf.vol.mean, by='date', all=T),sf.vol.SD, by='date', all=T)

# 5a: Error propagation on NO3.N in TF by sampling date:
dSF.samplingdate$dSF.NO3 = dSF.samplingdate$SF.depth.mean * dSF.samplingdate$SF.NO3.mean *
  ((dSF.samplingdate$SF.NO3.SD/dSF.samplingdate$SF.NO3.mean)^2+(dSF.samplingdate$SF.depth.SD/dSF.samplingdate$SF.depth.mean)^2)^0.5


# 5b: Error propagation on NH4.N in SF by sampling date:
dSF.samplingdate$dSF.NH4 = dSF.samplingdate$SF.depth.mean * dSF.samplingdate$SF.NH4.mean *
  ((dSF.samplingdate$SF.NH4.SD/dSF.samplingdate$SF.NH4.mean)^2+(dSF.samplingdate$SF.depth.SD/dSF.samplingdate$SF.depth.mean)^2)^0.5

# correct NaN to 0 (after a check to lab values, all = 0)
# dSF.samplingdate[19, "dSF.NO3"] = 0 # per ora no, non mi piace come soluzione, forse meglio ignorare l'errore proprio

###############################################################################
#########   REFINING ERROR PROPAGATION: ERROR PROPAGATION BY MONTH   ##########
# Rationale: working on partial errors weighed by the number of days of each month
# covered by the n sampling date needs to 1) calculate how many days of each sampling date fall in a month;
# 2. calculate the weighed error; 3. propagation error as sum of the weighed errors

# 1. Calculating how many days from previous sampling:

dates=as.Date(unique(dSF.samplingdate$date))

date.end.month <- seq(as.Date("2011-11-01"),length=66,by="months")-1

dates2 = c(dates,date.end.month)
dates2= as.data.frame.Date(dates2)
library(dplyr)
dates = dates2 %>% distinct(dates2) # this is to select unique values from dates2
dates = dates[order(dates$dates2, decreasing = FALSE ),]


days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
dd.dates=cbind(dates,days)
dd.dates = as.data.frame(dd.dates)
dd.dates$dates = as.Date(dd.dates$dates, origin="1970-01-01") # I ignore why I need to put this date as origin. Calculated via excel.
dSF.samplingdate$date = as.Date(dSF.samplingdate$date)
dSF.samplingdate1 = merge(dSF.samplingdate,dd.dates,by.x="date",by.y="dates",na.rm=FALSE, all = T)

# filling the NA to make calculations
library(zoo)
dSF.samplingdate1 = na.locf(dSF.samplingdate1,fromLast = TRUE)
dSF.samplingdate1$Ym = strftime(dSF.samplingdate1$date, "%Y%m")
# Calculate the number of days of a month
# install.packages("Hmisc")
library(Hmisc)
dSF.samplingdate1$daysxmonth = monthDays(as.Date(dSF.samplingdate1$date))

# remove the last row
dSF.samplingdate1 = head(dSF.samplingdate1, -1)

dSF.samplingdate1$days = as.numeric(dSF.samplingdate1$days)
cols.num <- c("SF.NO3.mean","SF.NO3.SD","SF.NH4.mean", "SF.NH4.SD","SF.depth.mean", "SF.depth.SD", "days")
dSF.samplingdate1[cols.num] <- sapply(dSF.samplingdate1[cols.num],as.numeric)

# 2a: Error propagation on NO3.N in SF:
dSF.samplingdate1$dSF.NO3 = dSF.samplingdate1$SF.depth.mean * dSF.samplingdate1$SF.NO3.mean *
  ((dSF.samplingdate1$SF.NO3.SD/dSF.samplingdate1$SF.NO3.mean)^2+(dSF.samplingdate1$SF.depth.SD/dSF.samplingdate1$SF.depth.mean)^2)^0.5 *
  dSF.samplingdate1$days/dSF.samplingdate1$daysxmonth


# 2b: Error propagation on NH4.N in SF:
dSF.samplingdate1$dSF.NH4 = dSF.samplingdate1$SF.depth.mean * dSF.samplingdate1$SF.NH4.mean *
  ((dSF.samplingdate1$SF.NH4.SD/dSF.samplingdate1$SF.NH4.mean)^2+(dSF.samplingdate1$SF.depth.SD/dSF.samplingdate1$SF.depth.mean)^2)^0.5 *
  dSF.samplingdate1$days/dSF.samplingdate1$daysxmonth

# 3. PROPAGATION ERROR AS SUM OF ERRORS FROM DIFFERENT SAMPLING DATES
# wide to long
library(reshape2)
SF.err.propag = melt(dSF.samplingdate1, id.vars = c("date", "Ym"), 
                     measure.vars = c("dSF.NH4", "dSF.NO3"), variable.name = "error_propagation_var")
# Calculate dn^2
SF.err.propag$value = (SF.err.propag$value)^2

dSF.err = aggregate(SF.err.propag$value ~  SF.err.propag$Ym + SF.err.propag$error_propagation_var, FUN = sum)
names(dSF.err) = c("Ym", "variable", "value")
dSF.err$value= (dSF.err$value)^0.5

#housekeeping
rm(dates2, dd.dates, dSF.samplingdate, dSF.samplingdate1, NH4.SF, NH4.SF.mean, NH4.SF.SD, NO3.SF, NO3.SF.mean, NO3.SF.SD, sf, sf.vol.mean,
   sf.vol.SD, SF.err.propag, cols.num, date.end.month, dates, days, diffdays, SF.coll)

##########################################################################################################

##########                        PRECIPITATION PROPAGATION ERROR                            #############

##########################################################################################################

library(RSQLite)

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

rf  = dbGetQuery(db, "SELECT * FROM fielddata WHERE sample = 'C30D1' or sample = 'C31D1' ORDER BY date")

# 1: mean vol value by sampling date 

rf.depth.mean=aggregate(vals ~ date, data = rf, FUN = mean, na.rm = TRUE )
names(rf.depth.mean) = c("date", "RF.depth.mean")

# 2: SD of depth value by sampling date

rf.depth.SD=aggregate(vals ~ date, data = rf, FUN = sd, na.rm = TRUE )
names(rf.depth.SD) = c("date", "RF.depth.SD")


##########              ERROR IN RF N LAB VALS                  #############

RF.coll = c("C30D1", "C31D1")

library(data.table)

NO3.RF = NO3data[NO3data$sample %in% RF.coll, ]
NH4.RF = NH4data[NH4data$sample %in% RF.coll, ]

# 3: mean of lab values by sampling date

NO3.RF.mean = aggregate(vals ~ date, data = NO3.RF, FUN = mean, na.rm = TRUE )
names(NO3.RF.mean) = c("date", "RF.NO3.mean")
NH4.RF.mean = aggregate(vals ~ date, data = NH4.RF, FUN = mean, na.rm = TRUE )
names(NH4.RF.mean) = c("date", "RF.NH4.mean")


#4: SD of lab values by sampling date

NO3.RF.SD = aggregate(vals ~ date, data = NO3.RF, FUN = sd, na.rm = TRUE )
names(NO3.RF.SD) = c("date", "RF.NO3.SD")

NH4.RF.SD = aggregate(vals ~ date, data = NH4.RF, FUN = sd, na.rm = TRUE )
names(NH4.RF.SD) = c("date", "RF.NH4.SD")

# 5. Calculate the error per each sampling date: MULTIMERGE

dRF.samplingdate = merge(merge(merge(merge(merge(NO3.RF.mean, NO3.RF.SD, by='date', all=T), 
                                           NH4.RF.mean, by='date', all=T), NH4.RF.SD, by='date', all=T),
                               rf.depth.mean, by='date', all=T),rf.depth.SD, by='date', all=T)

# 5a: Error propagation on NO3.N in RF by sampling date:
dRF.samplingdate$dRF.NO3 = dRF.samplingdate$RF.depth.mean * dRF.samplingdate$RF.NO3.mean *
  ((dRF.samplingdate$RF.NO3.SD/dRF.samplingdate$RF.NO3.mean)^2+(dRF.samplingdate$RF.depth.SD/dRF.samplingdate$RF.depth.mean)^2)^0.5


# 5b: Error propagation on NH4.N in RF by sampling date:
dRF.samplingdate$dRF.NH4 = dRF.samplingdate$RF.depth.mean * dRF.samplingdate$RF.NH4.mean *
  ((dRF.samplingdate$RF.NH4.SD/dRF.samplingdate$RF.NH4.mean)^2+(dRF.samplingdate$RF.depth.SD/dRF.samplingdate$RF.depth.mean)^2)^0.5

# correct NaN to 0 (after a check to lab values, all = 0)
# dRF.samplingdate[19, "dRF.NO3"] = 0 # per ora no, non mi piace come soluzione, forse meglio ignorare l'errore proprio

###############################################################################
#########   REFINING ERROR PROPAGATION: ERROR PROPAGATION BY MONTH   ##########
# Rationale: working on partial errors weighed by the number of days of each month
# covered by the n sampling date needs to 1) calculate how many days of each sampling date fall in a month;
# 2. calculate the weighed error; 3. propagation error as sum of the weighed errors

# 1. Calculating how many days from previous sampling:

dates=as.Date(unique(dRF.samplingdate$date))

date.end.month <- seq(as.Date("2011-11-01"),length=66,by="months")-1

dates2 = c(dates,date.end.month)
dates2= as.data.frame.Date(dates2)
library(dplyr)
dates = dates2 %>% distinct(dates2) # this is to select unique values from dates2
dates = dates[order(dates$dates2, decreasing = FALSE ),]


days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
dd.dates=cbind(dates,days)
dd.dates = as.data.frame(dd.dates)
dd.dates$dates = as.Date(dd.dates$dates, origin="1970-01-01") # I ignore why I need to put this date as origin. Calculated via excel.
dRF.samplingdate$date = as.Date(dRF.samplingdate$date)
dRF.samplingdate1 = merge(dRF.samplingdate,dd.dates,by.x="date",by.y="dates",na.rm=FALSE, all = T)

# filling the NA to make calculations
library(zoo)
dRF.samplingdate1 = na.locf(dRF.samplingdate1,fromLast = TRUE)
dRF.samplingdate1$Ym = strftime(dRF.samplingdate1$date, "%Y%m")
# Calculate the number of days of a month
# install.packages("Hmisc")
library(Hmisc)
dRF.samplingdate1$daysxmonth = monthDays(as.Date(dRF.samplingdate1$date))

# remove the last row
dRF.samplingdate1 = head(dRF.samplingdate1, -1)

dRF.samplingdate1$days = as.numeric(dRF.samplingdate1$days)
cols.num <- c("RF.NO3.mean","RF.NO3.SD","RF.NH4.mean", "RF.NH4.SD","RF.depth.mean", "RF.depth.SD", "days")
dRF.samplingdate1[cols.num] <- sapply(dRF.samplingdate1[cols.num],as.numeric)

# 2a: Error propagation on NO3.N in RF:
dRF.samplingdate1$dRF.NO3 = dRF.samplingdate1$RF.depth.mean * dRF.samplingdate1$RF.NO3.mean *
  ((dRF.samplingdate1$RF.NO3.SD/dRF.samplingdate1$RF.NO3.mean)^2+(dRF.samplingdate1$RF.depth.SD/dRF.samplingdate1$RF.depth.mean)^2)^0.5 *
  dRF.samplingdate1$days/dRF.samplingdate1$daysxmonth


# 2b: Error propagation on NH4.N in RF:
dRF.samplingdate1$dRF.NH4 = dRF.samplingdate1$RF.depth.mean * dRF.samplingdate1$RF.NH4.mean *
  ((dRF.samplingdate1$RF.NH4.SD/dRF.samplingdate1$RF.NH4.mean)^2+(dRF.samplingdate1$RF.depth.SD/dRF.samplingdate1$RF.depth.mean)^2)^0.5 *
  dRF.samplingdate1$days/dRF.samplingdate1$daysxmonth

# 3. PROPAGATION ERROR AS SUM OF ERRORS FROM DIFFERENT SAMPLING DATES
# wide to long
library(reshape2)
RF.err.propag = melt(dRF.samplingdate1, id.vars = c("date", "Ym"), 
                     measure.vars = c("dRF.NH4", "dRF.NO3"), variable.name = "error_propagation_var")
# Calculate dn^2
RF.err.propag$value = (RF.err.propag$value)^2 # a)

dRF.err = aggregate(RF.err.propag$value ~  RF.err.propag$Ym + RF.err.propag$error_propagation_var, FUN = sum) # sum of squares b)
names(dRF.err) = c("Ym", "variable", "value")
dRF.err$value= (dRF.err$value)^0.5 # square root to calculate the error propagation of sums c)

#housekeeping
rm(dates2, dd.dates, dRF.samplingdate, dRF.samplingdate1, NH4.RF, NH4.RF.mean, NH4.RF.SD, NO3.RF, NO3.RF.mean, NO3.RF.SD, rf, rf.depth.mean,
   rf.depth.SD, RF.err.propag, cols.num, date.end.month, dates, days, diffdays, RF.coll)
