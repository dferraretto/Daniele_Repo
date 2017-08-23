############## --------------------------------------------------------
##############                  ERROR PROPAGATION
##############  Author: Daniele Ferraretto
##############  started on 26th July, 2017
##############  updated:                        last update: 31/07/2017
############## --------------------------------------------------------
############## --------------------------------------------------------

#  ------>>>  NOTE: ALL ERRORS ARE IN mg N / m2. You need to divide them by 100 to compare them with the monthly data,
#             expressed in Kg N / ha


# clear the memory
 rm(list=ls())


.libPaths("C:/Workspace/R") # adjust the issues with my desktop demmerda
### set working dir for pc:
#setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
#setwd("M:/My PhD/R/PhD-local_repo")
setwd("C:/Users/s1373890/Daniele_Repo")

##########          THROUGHFALL PROPAGATION ERROR          #############
#                      by  SAMPLING DATE (1/2)

# prima prova. 1) SD e mean per ogni sampling date, 
# 2) poi valore giornaliero come errore prova/ numero giorni per giorni utili
# 3) Addition: semplice radice quadrata dei due valori di errore di cui da 2

##########              ERROR IN TF DEPTH                  #############
library(RSQLite)

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

TF  = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' AND vals>=0 ORDER BY date")

NO3data = dbGetQuery(db, "SELECT * FROM labdata WHERE VALS >= 0 AND variable = 'NO3.N' ORDER BY date")
NH4data = dbGetQuery(db, "SELECT * FROM labdata WHERE VALS >= 0 AND variable = 'NH4.N' ORDER BY date")


# A1: MEAN depth value by sampling date 

TF.depth.mean=aggregate(vals ~ date, data = TF, FUN = mean, na.rm = TRUE )
names(TF.depth.mean) = c("date", "TF.depth.mean")

# A2: SD of depth value by sampling date

TF.depth.SD=aggregate(vals ~ date, data = TF, FUN = sd, na.rm = TRUE )

# A3: counting samples 

TF.N = as.data.frame(table(TF$date))

# A4. SE = SD/(N)^0.5. SE (95%) = 1.96*SE
TF.SE.95 =  as.data.frame(1.96*TF.depth.SD$vals/(TF.N$Freq)^0.5)

TF.SE.95 = cbind(TF.N$Var1, TF.SE.95)
names(TF.SE.95) = c("date", "depth.SE.95")

##########              ERROR IN TF N LAB VALS                  #############

TF.coll = c("C10T1", "C10T2", "C10T3", "C11T1", "C11T2", "C11T3", "C12T1", "C12T2", "C12T3", "T10T1", 
           "T10T2", "T10T3", "T11T1", "T11T2", "T11T3", "T12T1", "T12T2", "T12T3")

library(data.table)

NO3.TF = NO3data[NO3data$sample %in% TF.coll, ]
NH4.TF = NH4data[NH4data$sample %in% TF.coll, ]

# B1: mean of lab values by sampling date

NO3.TF.mean = aggregate(vals ~ date, data = NO3.TF, FUN = mean, na.rm = TRUE )
names(NO3.TF.mean) = c("date", "TF.NO3.mean")
NH4.TF.mean = aggregate(vals ~ date, data = NH4.TF, FUN = mean, na.rm = TRUE )
names(NH4.TF.mean) = c("date", "TF.NH4.mean")


# B2: SD of lab values by sampling date

NO3.TF.SD = aggregate(vals ~ date, data = NO3.TF, FUN = sd, na.rm = TRUE )
names(NO3.TF.SD) = c("date", "TF.NO3.SD")

NH4.TF.SD = aggregate(vals ~ date, data = NH4.TF, FUN = sd, na.rm = TRUE )
names(NH4.TF.SD) = c("date", "TF.NH4.SD")

# 22/08/2017: problema con i dati di luglio 2017 di NH4: non esistono. verifica origine problema. se nec rerun scripts!!!
# B3: counting samples: 
(library(plyr))

NO3.TF.N = as.data.frame(table(NO3.TF$date))

NH4.TF.N = as.data.frame(table(NH4.TF$date))


# B4. SE = SD/(N)^0.5. SE (95%) = 1.96*SE
TFNO3.SE.95 =  as.data.frame(1.96*NO3.TF.SD$TF.NO3.SD/(NO3.TF.N$Freq)^0.5)
TFNH4.SE.95 =  as.data.frame(1.96*NH4.TF.SD$TF.NH4.SD/(NH4.TF.N$Freq)^0.5)


TFLAB.SE.95 = cbind(NO3.TF.N$Var1, TFNO3.SE.95, TFNH4.SE.95)
names(TFLAB.SE.95) = c("date", "SE.95.NO3", "SE.95.NH4")


# B5. Calculate the error per each sampling date: MULTIMERGE

dTF.samplingdate = merge(merge(merge(merge(NO3.TF.mean, TF.SE.95, by='date', all=T), 
                               NH4.TF.mean, by='date', all=T), TF.depth.mean, by='date', all=T), TFLAB.SE.95, by='date', all=T)

# 5a: Error propagation on NO3.N in TF by sampling date:
dTF.samplingdate$dTF.NO3 = dTF.samplingdate$TF.depth.mean * dTF.samplingdate$TF.NO3.mean *
  ((dTF.samplingdate$SE.95.NO3/dTF.samplingdate$TF.NO3.mean)^2+(dTF.samplingdate$depth.SE.95/dTF.samplingdate$TF.depth.mean)^2)^0.5


# 5b: Error propagation on NH4.N in TF by sampling date:
dTF.samplingdate$dTF.NH4 = dTF.samplingdate$TF.depth.mean * dTF.samplingdate$TF.NH4.mean *
  ((dTF.samplingdate$SE.95.NH4/dTF.samplingdate$TF.NH4.mean)^2+(dTF.samplingdate$depth.SE.95/dTF.samplingdate$TF.depth.mean)^2)^0.5


#####################################################################################
#########   REFINING ERROR PROPAGATION in TF: ERROR PROPAGATION BY MONTH   ##########
#                                      (2/2)
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

dTF.samplingdate1$days = as.numeric(dTF.samplingdate1$days)
cols.num <- c("TF.NO3.mean","SE.95.NO3","TF.NH4.mean", "SE.95.NH4","TF.depth.mean", "depth.SE.95", "days")
dTF.samplingdate1[cols.num] <- sapply(dTF.samplingdate1[cols.num],as.numeric)
summary(dTF.samplingdate1)

# 2A: Error propagation on NO3.N in TF (error weighed by days/month):
dTF.samplingdate1$dTF.NO3 = dTF.samplingdate1$TF.depth.mean * dTF.samplingdate1$TF.NO3.mean *
  ((dTF.samplingdate1$SE.95.NO3/dTF.samplingdate1$TF.NO3.mean)^2+(dTF.samplingdate1$depth.SE.95/dTF.samplingdate1$TF.depth.mean)^2)^0.5 *
  dTF.samplingdate1$days/dTF.samplingdate1$daysxmonth


# 2B: Error propagation on NH4.N in TF (error weighed by days/month)::
dTF.samplingdate1$dTF.NH4 = dTF.samplingdate1$TF.depth.mean * dTF.samplingdate1$TF.NH4.mean *
  ((dTF.samplingdate1$SE.95.NH4/dTF.samplingdate1$TF.NH4.mean)^2+(dTF.samplingdate1$depth.SE.95/dTF.samplingdate1$TF.depth.mean)^2)^0.5 *
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
rm(NH4.TF.N, NO3.TF.N, TF.N, TF.SE.95, TFLAB.SE.95, TFNH4.SE.95, TFNO3.SE.95, dates2, dd.dates, dTF.samplingdate, dTF.samplingdate1, NH4.TF, NH4.TF.mean, NH4.TF.SD, NO3.TF, NO3.TF.mean, NO3.TF.SD, TF, 
   TF.depth.mean, TF.depth.SD, TF.err.propag, cols.num, date.end.month, dates, days, diffdays, TF.coll)

##############################################################################

############            STEMFLOW PROPAGATION ERROR             ###############

# prima prova. 1) SD e mean per ogni sampling date, 
# 2) poi valore giornaliero come errore prova/ numero giorni per giorni utili
# 3) Addition: semplice radice quadrata dei due valori di errore di cui da 2

##########                    ERROR IN SF vol                    #############


SF  = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'stem vol' AND vals>=0 ORDER BY date")


# A1: MEAN vol value by sampling date 

SF.vol.mean=aggregate(vals ~ date, data = SF, FUN = mean, na.rm = TRUE )
names(SF.vol.mean) = c("date", "SF.vol.mean")

# A2: SD of vol value by sampling date

SF.vol.SD=aggregate(vals ~ date, data = SF, FUN = sd, na.rm = TRUE )

# A3: counting samples 

SF.N = as.data.frame(table(SF$date))

# A4. SE = SD/(N)^0.5. SE (95%) = 1.96*SE
SF.SE.95 =  as.data.frame(1.96*SF.vol.SD$vals/(SF.N$Freq)^0.5)

SF.SE.95 = cbind(SF.N$Var1, SF.SE.95)
names(SF.SE.95) = c("date", "vol.SE.95")


##########              ERROR IN SF N LAB VALS                  #############

SF.coll = c("C10S1", "C10S2", "C10S3", "C11S1", "C11S2", "C11S3", "C12S1", "C12S2", "C12S3", "T10S1", 
            "T10S2", "T10S3", "T11S1", "T11S2", "T11S3", "T11S4", "T11S5", "T11S6", "T11S7", "T12S1", "T12S2", "T12S3")

NO3.SF = NO3data[NO3data$sample %in% SF.coll, ]
NH4.SF = NH4data[NH4data$sample %in% SF.coll, ]

# B1: mean of lab values by sampling date

NO3.SF.mean = aggregate(vals ~ date, data = NO3.SF, FUN = mean, na.rm = TRUE )
names(NO3.SF.mean) = c("date", "SF.NO3.mean")
NH4.SF.mean = aggregate(vals ~ date, data = NH4.SF, FUN = mean, na.rm = TRUE )
names(NH4.SF.mean) = c("date", "SF.NH4.mean")


# B2: SD of lab values by sampling date

NO3.SF.SD = aggregate(vals ~ date, data = NO3.SF, FUN = sd, na.rm = TRUE )
names(NO3.SF.SD) = c("date", "SF.NO3.SD")

NH4.SF.SD = aggregate(vals ~ date, data = NH4.SF, FUN = sd, na.rm = TRUE )
names(NH4.SF.SD) = c("date", "SF.NH4.SD")

# B3: counting samples: 

NO3.SF.N = as.data.frame(table(NO3.SF$date))

NH4.SF.N = as.data.frame(table(NH4.SF$date))


# B4. SE = SD/(N)^0.5. SE (95%) = 1.96*SE
SFNO3.SE.95 =  as.data.frame(1.96*NO3.SF.SD$SF.NO3.SD/(NO3.SF.N$Freq)^0.5)
SFNH4.SE.95 =  as.data.frame(1.96*NH4.SF.SD$SF.NH4.SD/(NH4.SF.N$Freq)^0.5)


SFLAB.SE.95 = cbind(NO3.SF.N$Var1, SFNO3.SE.95, SFNH4.SE.95)
names(SFLAB.SE.95) = c("date", "SE.95.NO3", "SE.95.NH4")


# B5. Calculate the error per each sampling date: MULTIMERGE

dSF.samplingdate = merge(merge(merge(merge(SF.vol.mean, SF.SE.95, by='date', all=T), 
                                     NH4.SF.mean, by='date', all=T), NO3.SF.mean, by='date', all=T), 
                         SFLAB.SE.95, by='date', all=T) # this last is the sum of the two Nx lab.SE

# B5a: Error propagation on NO3.N in TF by sampling date:
dSF.samplingdate$dSF.NO3 = dSF.samplingdate$SF.vol.mean * dSF.samplingdate$SF.NO3.mean *
  ((dSF.samplingdate$SE.95.NO3/dSF.samplingdate$SF.NO3.mean)^2+(dSF.samplingdate$vol.SE.95/dSF.samplingdate$SF.vol.mean)^2)^0.5


# B5b: Error propagation on NH4.N in SF by sampling date:
dSF.samplingdate$dSF.NH4 = dSF.samplingdate$SF.vol.mean * dSF.samplingdate$SF.NH4.mean *
  ((dSF.samplingdate$SE.95.NH4/dSF.samplingdate$SF.NH4.mean)^2+(dSF.samplingdate$vol.SE.95/dSF.samplingdate$SF.vol.mean)^2)^0.5

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

dSF.samplingdate1 = na.locf(dSF.samplingdate1,fromLast = TRUE)
dSF.samplingdate1$Ym = strftime(dSF.samplingdate1$date, "%Y%m")
# Calculate the number of days of a month
# install.packages("Hmisc")

dSF.samplingdate1$daysxmonth = monthDays(as.Date(dSF.samplingdate1$date))

dSF.samplingdate1$days = as.numeric(dSF.samplingdate1$days)
cols.num <- c("SF.NO3.mean","SE.95.NO3","SF.NH4.mean", "SE.95.NH4","SF.vol.mean", "vol.SE.95", "days")
dSF.samplingdate1[cols.num] <- sapply(dSF.samplingdate1[cols.num],as.numeric)


# 2a: Error propagation on NO3.N in SF (error weighed by days/month):
dSF.samplingdate1$dSF.NO3 = dSF.samplingdate1$SF.vol.mean * dSF.samplingdate1$SF.NO3.mean *
  ((dSF.samplingdate1$SE.95.NO3/dSF.samplingdate1$SF.NO3.mean)^2+(dSF.samplingdate1$vol.SE.95/dSF.samplingdate1$SF.vol.mean)^2)^0.5 *
  dSF.samplingdate1$days/dSF.samplingdate1$daysxmonth


# 2b: Error propagation on NH4.N in SF (error weighed by days/month):
dSF.samplingdate1$dSF.NH4 = dSF.samplingdate1$SF.vol.mean * dSF.samplingdate1$SF.NH4.mean *
  ((dSF.samplingdate1$SE.95.NH4/dSF.samplingdate1$SF.NH4.mean)^2+(dSF.samplingdate1$vol.SE.95/dSF.samplingdate1$SF.vol.mean)^2)^0.5 *
  dSF.samplingdate1$days/dSF.samplingdate1$daysxmonth

# 3. PROPAGATION ERROR AS SUM OF ERRORS FROM DIFFERENT SAMPLING DATES
# wide to long

SF.err.propag = melt(dSF.samplingdate1, id.vars = c("date", "Ym"), 
                     measure.vars = c("dSF.NH4", "dSF.NO3"), variable.name = "error_propagation_var")
# Calculate dn^2
SF.err.propag$value = (SF.err.propag$value)^2

dSF.err = aggregate(SF.err.propag$value ~  SF.err.propag$Ym + SF.err.propag$error_propagation_var, FUN = sum)
names(dSF.err) = c("Ym", "variable", "value")
dSF.err$value= (dSF.err$value)^0.5

#housekeeping
rm(dates2, dd.dates, dSF.samplingdate, dSF.samplingdate1, NH4.SF, NH4.SF.mean, NH4.SF.SD, NO3.SF, NO3.SF.mean, NO3.SF.SD,
   SF.err.propag, cols.num, date.end.month, dates, days, diffdays, SF.coll, NH4.SF.N, NO3.SF.N, SF, SF.N, SF.SE.95,
   SF.vol.mean, SFLAB.SE.95, SFNH4.SE.95, SFNO3.SE.95, SF.vol.SD)

##########################################################################################################

##########                        PRECIPITATION PROPAGATION ERROR                            #############

##########################################################################################################

RF  = dbGetQuery(db, "SELECT * FROM fielddata WHERE sample = 'C30D1' or sample = 'C31D1' ORDER BY date")

# A1: MEAN depth by sampling date 

RF.depth.mean=aggregate(vals ~ date, data = RF, FUN = mean, na.rm = TRUE )
names(RF.depth.mean) = c("date", "RF.depth.mean")

# A2: SD of depth value by sampling date

RF.depth.SD=aggregate(vals ~ date, data = RF, FUN = sd, na.rm = TRUE )

# A3: counting samples 

RF.N = as.data.frame(table(RF$date))

# A4. SE = SD/(N)^0.5. SE (95%) = 1.96*SE
RF.SE.95 =  as.data.frame(1.96*RF.depth.SD$vals/(RF.N$Freq)^0.5)

RF.SE.95 = cbind(RF.N$Var1, RF.SE.95)
names(RF.SE.95) = c("date", "depth.SE.95")



##########              ERROR IN RF N LAB VALS                  #############

RF.coll = c("C30D1", "C31D1")


NO3.RF = NO3data[NO3data$sample %in% RF.coll, ]
NH4.RF = NH4data[NH4data$sample %in% RF.coll, ]

# B1: MEAN of LAB values by sampling date

NO3.RF.mean = aggregate(vals ~ date, data = NO3.RF, FUN = mean, na.rm = TRUE )
names(NO3.RF.mean) = c("date", "RF.NO3.mean")
NH4.RF.mean = aggregate(vals ~ date, data = NH4.RF, FUN = mean, na.rm = TRUE )
names(NH4.RF.mean) = c("date", "RF.NH4.mean")

#4: SD of lab values by sampling date

NO3.RF.SD = aggregate(vals ~ date, data = NO3.RF, FUN = sd, na.rm = TRUE )
names(NO3.RF.SD) = c("date", "RF.NO3.SD")

NH4.RF.SD = aggregate(vals ~ date, data = NH4.RF, FUN = sd, na.rm = TRUE )
names(NH4.RF.SD) = c("date", "RF.NH4.SD")

# B3: counting samples: 
NO3.RF.N = as.data.frame(table(NO3.RF$date))

NH4.RF.N = as.data.frame(table(NH4.RF$date))

# B4. SE = SD/(N)^0.5. SE (95%) = 1.96*SE
RFNO3.SE.95 =  as.data.frame(1.96*NO3.RF.SD$RF.NO3.SD/(NO3.RF.N$Freq)^0.5)
RFNH4.SE.95 =  as.data.frame(1.96*NH4.RF.SD$RF.NH4.SD/(NH4.RF.N$Freq)^0.5)



RFLAB.SE.95 = cbind(NO3.RF.N$Var1, RFNO3.SE.95, RFNH4.SE.95)
names(RFLAB.SE.95) = c("date", "SE.95.NO3", "SE.95.NH4")


# B5. Calculate the error per each sampling date: MULTIMERGE

dRF.samplingdate = merge(merge(merge(merge(NO3.RF.mean, RF.SE.95, by='date', all=T), 
                                     NH4.RF.mean, by='date', all=T), RF.depth.mean, by='date', all=T), RFLAB.SE.95, by='date', all=T)

# 5a: Error propagation on NO3.N in RF by sampling date:
dRF.samplingdate$dRF.NO3 = dRF.samplingdate$RF.depth.mean * dRF.samplingdate$RF.NO3.mean *
  ((dRF.samplingdate$SE.95.NO3/dRF.samplingdate$RF.NO3.mean)^2+(dRF.samplingdate$depth.SE.95/dRF.samplingdate$RF.depth.mean)^2)^0.5


# 5b: Error propagation on NH4.N in RF by sampling date:
dRF.samplingdate$dRF.NH4 = dRF.samplingdate$RF.depth.mean * dRF.samplingdate$RF.NH4.mean *
  ((dRF.samplingdate$SE.95.NH4/dRF.samplingdate$RF.NH4.mean)^2+(dRF.samplingdate$depth.SE.95/dRF.samplingdate$RF.depth.mean)^2)^0.5


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

dRF.samplingdate1 = na.locf(dRF.samplingdate1,fromLast = TRUE)
dRF.samplingdate1$Ym = strftime(dRF.samplingdate1$date, "%Y%m")
# Calculate the number of days of a month

dRF.samplingdate1$daysxmonth = monthDays(as.Date(dRF.samplingdate1$date))

# converting columns from character to numeric at once:
dRF.samplingdate1$days = as.numeric(dRF.samplingdate1$days)
cols.num <- c("RF.NO3.mean","SE.95.NO3","RF.NH4.mean", "SE.95.NH4","RF.depth.mean", "depth.SE.95", "days")
dRF.samplingdate1[cols.num] <- sapply(dRF.samplingdate1[cols.num],as.numeric)

# 2a: Error propagation on NO3.N in RF:
dRF.samplingdate1$dRF.NO3 = dRF.samplingdate1$RF.depth.mean * dRF.samplingdate1$RF.NO3.mean *
  ((dRF.samplingdate1$SE.95.NO3/dRF.samplingdate1$RF.NO3.mean)^2+(dRF.samplingdate1$depth.SE.95/dRF.samplingdate1$RF.depth.mean)^2)^0.5 *
  dRF.samplingdate1$days/dRF.samplingdate1$daysxmonth


# 2b: Error propagation on NH4.N in RF:
dRF.samplingdate1$dRF.NH4 = dRF.samplingdate1$RF.depth.mean * dRF.samplingdate1$RF.NH4.mean *
  ((dRF.samplingdate1$SE.95.NH4/dRF.samplingdate1$RF.NH4.mean)^2+(dRF.samplingdate1$depth.SE.95/dRF.samplingdate1$RF.depth.mean)^2)^0.5 *
  dRF.samplingdate1$days/dRF.samplingdate1$daysxmonth

# 3. PROPAGATION ERROR AS SUM OF ERRORS FROM DIFFERENT SAMPLING DATES
# wide to long
RF.err.propag = melt(dRF.samplingdate1, id.vars = c("date", "Ym"), 
                     measure.vars = c("dRF.NH4", "dRF.NO3"), variable.name = "error_propagation_var")
# Calculate dn^2
RF.err.propag$value = (RF.err.propag$value)^2 # a)

dRF.err = aggregate(RF.err.propag$value ~  RF.err.propag$Ym + RF.err.propag$error_propagation_var, FUN = sum) # sum of squares b)
names(dRF.err) = c("Ym", "variable", "value")
dRF.err$value= (dRF.err$value)^0.5 # square root to calculate the error propagation of sums c)

#housekeeping
rm(dates2, dd.dates, dRF.samplingdate, dRF.samplingdate1, NH4.RF, NH4.RF.mean, NH4.RF.SD, NO3.RF, NO3.RF.mean,
   NO3.RF.SD, RF, RF.depth.mean, NH4.RF.N, NO3.RF.N, RF.SE.95, RFLAB.SE.95, RFNH4.SE.95, RFNO3.SE.95, RF.N,
   RF.depth.SD, RF.err.propag, cols.num, date.end.month, dates, days, diffdays, RF.coll, NO3data, NH4data)