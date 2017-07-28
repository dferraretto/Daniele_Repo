# --------------------------------------------------------
#                  ERROR PROPAGATION
#  Author: Daniele Ferraretto
#  started on 20th July, 2015
#  updated: 09/10/2015          last update: 15/10/2015
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

# 5a: Error propagation on NO3.N in TF:
dTF.samplingdate$dTF.NO3 = dTF.samplingdate$TF.depth.mean * dTF.samplingdate$TF.NO3.mean *
  ((dTF.samplingdate$TF.NO3.SD/dTF.samplingdate$TF.NO3.mean)^2+(dTF.samplingdate$TF.depth.SD/dTF.samplingdate$TF.depth.mean)^2)^0.5


# 5b: Error propagation on NH4.N in TF:
dTF.samplingdate$dTF.NH4 = dTF.samplingdate$TF.depth.mean * dTF.samplingdate$TF.NH4.mean *
  ((dTF.samplingdate$TF.NH4.SD/dTF.samplingdate$TF.NH4.mean)^2+(dTF.samplingdate$TF.depth.SD/dTF.samplingdate$TF.depth.mean)^2)^0.5

# Calculating how many days from previous sampling:

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
dd.dates$dates = as.Date(dd.dates$dates)


fielddata=merge(x,dd.dates,by.x="date",by.y="dates",na.rm=FALSE)
fielddata$days=as.numeric(levels(fielddata$days))[fielddata$days] #this is to "read" days as a number = to the level (weird things happens otherwise...)

dates2 = merge(dates, date.end.month, all = T)
merge(distinct_paper_year_data,author_data,by="author_id", all=T)
