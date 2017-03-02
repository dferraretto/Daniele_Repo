# -------------------------------------------------------- #
# -------------------------------------------------------- #
#                                                          #
#     CREATING A DF FROM THE ORIGINAL FIELDWORK DATA       #
#     TO PLOT A CORRELATION BETWEEN tf, sf, rf & FOG       #
#     VOLUMES                    created: 09/12/2016       #
# -------------------------------------------------------- #
# -------------------------------------------------------- #
#
#
#        PART ONE: ALL DATA (OF included)
# clear the memory
rm(list=ls())
.libPaths("C:/Workspace/R")


library(RSQLite)
library(zoo)
library(ggplot2)
library(reshape2)

### set working dir for pc:
#setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
setwd("M:/My PhD/R/PhD-local_repo")

# remove the outliers from the Griffin SQlite: at the moment only manually by using Outliers_aug2016 to identify them and Outliers to remove them
#source("mikerspencer/Outliers.R")


db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")
# -------------------------------------------------------------
# 1.    STEMFLOW
# -------------------------------------------------------------
# cREATE A WEIGHED "DAILY" VALUE (BY DBH CLASS) BY SAMPLING DATE
library (plyr)

# select SF volumes data from SQL db
SF = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'stem vol' AND vals>=0 ORDER BY date")

# 1a. Obtain a "daily mean" value by calculating how many days from the previous sampling

dates=unique(SF$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
days.dates=cbind(dates,days)
SFdepth=merge(SF,days.dates,by.x="date",by.y="dates",na.rm=FALSE)
SFdepth$days = as.numeric(as.character(SFdepth$days))
SFdepth$dailyvals = SFdepth$vals/SFdepth$days # this creates a "daily value" to make different periods comparable
SFdepth = SFdepth[ , c(1,3,4,11)]

# 1b. importing dbh classes and merge with sf
stemdbh <- read.csv("mikerspencer/stemdbh.csv", header=TRUE)
dbh.class=stemdbh[,c(1,5)]
SF.class=merge(SFdepth,dbh.class, by = "sample")
SF.class=arrange(SF.class,date,dbh.class)


# STEP 1c: MEAN  vals by dbh.class+DATE:
meandbhSF <- aggregate(dailyvals ~ dbh.class+date, data = SF.class, FUN = mean,  na.rm = TRUE) 

# STEP2b: MEAN vals by date:
SFdepth <- aggregate(dailyvals ~ date, data = meandbhSF, FUN = mean,  na.rm = TRUE) 
SFdepth$variable = "SF"
colnames(SFdepth)[2] = "vals"

#housekeeping
rm(SF, dates, days, diffdays, stemdbh, dbh.class, SF.class, meandbhSF, days.dates)
# -------------------------------------------------------------
# 2.    THROUGHFALL
# -------------------------------------------------------------
# CREATE A MEAN TF "DAILY" VALUE  BY DATE
TF = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' ORDER BY date")

# How many days from last sampling?
dates=unique(TF$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
days.dates=cbind(dates,days)

TFdepth=merge(TF,days.dates,by.x="date",by.y="dates",na.rm=FALSE)
TFdepth$days = as.numeric(as.character(TFdepth$days))
TFdepth$dailyvals = TFdepth$vals/TFdepth$days # this creates a "daily value" to make different periods comparable
TFdepth = TFdepth[ , c(1,3,4,11)]
TFdepth = aggregate(dailyvals ~ date, data = TFdepth, FUN = mean, na.rm = TRUE )
TFdepth$variable = "TF"
colnames(TFdepth)[2] = "vals"

#housekeeping
rm(TF, dates, days, diffdays, days.dates)

# -------------------------------------------------------------
# 3.    RAINFALL
# -------------------------------------------------------------
RF = dbGetQuery(db, "SELECT * FROM fielddata WHERE (sample = 'C30D1' or sample = 'C31D1') ORDER BY date")
RFdepth = aggregate(vals ~ date, data = RF, FUN = mean, na.rm = TRUE )
RFdepth = RFdepth[ , c(1,3)]
RFdepth$variable = "RF"

# housekeeping
rm(RF)

# -------------------------------------------------------------
# 4.    FOG COLLECTOR
# -------------------------------------------------------------
fog = dbGetQuery(db, "SELECT * FROM fielddata WHERE sample = 'C30D2' ORDER BY date")
fog = fog[ , c(1,6)]
fog$variable = "fog collector"

# long df
x = rbind(RFdepth, SFdepth, TFdepth, fog)
# long to wide
xwide = dcast(x, date ~ variable, value.var = "vals")

library(psych) 
pairs.panels(xwide[2:5]) # select columns 2:5

# -------------------------------------------------------------------
#
#
#                    PART TWO: excluding OF data
#
#
# -------------------------------------------------------------------

# clear the memory
rm(list=ls())
.libPaths("C:/Workspace/R")


library(RSQLite)
library(zoo)
library(ggplot2)
library(reshape2)

### set working dir for pc:
#setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
setwd("M:/My PhD/R/PhD-local_repo")

# remove the outliers from the Griffin SQlite: at the moment only manually by using Outliers_aug2016 to identify them and Outliers to remove them
#source("mikerspencer/Outliers.R")


db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")
# -------------------------------------------------------------
# 1.    STEMFLOW
# -------------------------------------------------------------
# cREATE A WEIGHED "DAILY" VALUE (BY DBH CLASS) BY SAMPLING DATE
library (plyr)

# select SF volumes data from SQL db
SF = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'stem vol' AND vals>=0 ORDER BY date")
SF = SF[SF$overflowing!="1", ]

# 1a. Obtain a "daily mean" value by calculating how many days from the previous sampling

dates=unique(SF$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
days.dates=cbind(dates,days)
SFdepth=merge(SF,days.dates,by.x="date",by.y="dates",na.rm=FALSE)
SFdepth$days = as.numeric(as.character(SFdepth$days))
SFdepth$dailyvals = SFdepth$vals/SFdepth$days # this creates a "daily value" to make different periods comparable
SFdepth = SFdepth[ , c(1,3,4,11)]

# 1b. importing dbh classes and merge with sf
stemdbh <- read.csv("mikerspencer/stemdbh.csv", header=TRUE)
dbh.class=stemdbh[,c(1,5)]
SF.class=merge(SFdepth,dbh.class, by = "sample")
SF.class=arrange(SF.class,date,dbh.class)


# STEP 1c: MEAN  vals by dbh.class+DATE:
meandbhSF <- aggregate(dailyvals ~ dbh.class+date, data = SF.class, FUN = mean,  na.rm = TRUE) 

# STEP2b: MEAN vals by date:
SFdepth <- aggregate(dailyvals ~ date, data = meandbhSF, FUN = mean,  na.rm = TRUE) 
SFdepth$variable = "SF"
colnames(SFdepth)[2] = "vals"

#housekeeping
rm(SF, dates, days, diffdays, stemdbh, dbh.class, SF.class, meandbhSF, days.dates)
# -------------------------------------------------------------
# 2.    THROUGHFALL
# -------------------------------------------------------------
# CREATE A MEAN TF "DAILY" VALUE  BY DATE
TF = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' ORDER BY date")
TF = TF[TF$overflowing!="1", ]

# How many days from last sampling?
dates=unique(TF$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
days.dates=cbind(dates,days)

TFdepth=merge(TF,days.dates,by.x="date",by.y="dates",na.rm=FALSE)
TFdepth$days = as.numeric(as.character(TFdepth$days))
TFdepth$dailyvals = TFdepth$vals/TFdepth$days # this creates a "daily value" to make different periods comparable
TFdepth = TFdepth[ , c(1,3,4,11)]
TFdepth = aggregate(dailyvals ~ date, data = TFdepth, FUN = mean, na.rm = TRUE )
TFdepth$variable = "TF"
colnames(TFdepth)[2] = "vals"

#housekeeping
rm(TF, dates, days, diffdays, days.dates)

# -------------------------------------------------------------
# 3.    RAINFALL
# -------------------------------------------------------------
RF = dbGetQuery(db, "SELECT * FROM fielddata WHERE (sample = 'C30D1' or sample = 'C31D1') ORDER BY date")
RF = RF[RF$overflowing!="1", ]

RFdepth = aggregate(vals ~ date, data = RF, FUN = mean, na.rm = TRUE )
RFdepth = RFdepth[ , c(1,3)]
RFdepth$variable = "RF"

# housekeeping
rm(RF)

# -------------------------------------------------------------
# 4.    FOG COLLECTOR
# -------------------------------------------------------------

fog = dbGetQuery(db, "SELECT * FROM fielddata WHERE sample = 'C30D2' ORDER BY date")
fog = fog[fog$overflowing!="1", ]

fog = fog[ , c(1,6)]
fog$variable = "fog collector"

# long df
x = rbind(RFdepth, SFdepth, TFdepth, fog)
# long to wide
xwide = dcast(x, date ~ variable, value.var = "vals")

library(psych) 
pairs.panels(xwide[2:5]) # select columns 2:5


# -------------------------------------------------------- #
# -------------------------------------------------------- #
#                                                          #
#     CREATING A DF FROM THE ORIGINAL FIELDWORK DATA       #
#     TO PLOT A CORRELATION BETWEEN tf, sf, rf & FOG       #
#     N MASSES                    created: 15/12/2016      #
# -------------------------------------------------------- #
# -------------------------------------------------------- #

# PART ONE (and only, likely): ALL DATA (OF included)
# clear the memory
rm(list=ls())
.libPaths("C:/Workspace/R")


library(RSQLite)
library(zoo)
library(ggplot2)
library(reshape2)

### set working dir for pc:
#setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
setwd("M:/My PhD/R/PhD-local_repo")

# remove the outliers from the Griffin SQlite: at the moment only manually by using Outliers_aug2016 to identify them and Outliers to remove them
#source("mikerspencer/Outliers.R")


db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")
# -------------------------------------------------------------
# 1.    STEMFLOW
# -------------------------------------------------------------
# cREATE A WEIGHED "DAILY" VALUE (BY DBH CLASS) BY SAMPLING DATE
library (plyr)

# select SF volumes data from SQL db
SF = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'stem vol' AND vals>=0 ORDER BY date")
# vector containing all SF samples names
SFsamples= unique(SF$sample)
# extract lab values by sample, name and N form separately
labNO3 = dbGetQuery(db, "SELECT * FROM labdata WHERE variable = 'NO3.N'  ORDER BY date")
SFNO3=labNO3[labNO3$sample%in%SFsamples,]
labNH4 = dbGetQuery(db, "SELECT * FROM labdata WHERE variable = 'NH4.N'  ORDER BY date")
SFNH4=labNH4[labNH4$sample%in%SFsamples,]
# merge SF lab vals
SFlab = merge(SFNO3, SFNH4, by.x = c("date", "sample"), by.y = c("date", "sample"), all = TRUE)
SFlab= SFlab[, c(1,2,5,8)]
colnames(SFlab)= c("date", "sample", "NO3.N", "NH4.N")

# merge  SF field vals (volumes) and lab vals (concentrations)
SF = merge(SF, SFlab, by.x = c("date", "sample"), by.y = c("date", "sample"), all = TRUE)

# calculate the N mass per sample per date per N form
SF$NO3.N=SF$vals*SF$NO3.N
SF$NH4.N=SF$vals*SF$NH4.N

# cleaning up the df
SF = SF[ , c(1,2,10,11)]




# 1a. Obtain a "daily mean" value by calculating how many days from the previous sampling

dates=unique(SF$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
days.dates=cbind(dates,days)
SF.N.mass=merge(SF,days.dates,by.x="date",by.y="dates",na.rm=FALSE)
SF.N.mass$days = as.numeric(as.character(SF.N.mass$days))
SF.N.mass$dailyNO3 = SF.N.mass$NO3.N/SF.N.mass$days # this creates a NO3.N "daily value" to make different periods comparable
SF.N.mass$dailyNH4 = SF.N.mass$NH4.N/SF.N.mass$days # this creates a NH4.N "daily value" to make different periods comparable

SF.N.mass = SF.N.mass[ , c(1,2,6,7)]

# 1b. importing dbh classes and merge with sf
stemdbh <- read.csv("mikerspencer/stemdbh.csv", header=TRUE)
dbh.class=stemdbh[,c(1,5)]
SF.N.mass=merge(SF.N.mass,dbh.class, by = "sample")
SF.N.mass=arrange(SF.N.mass,date,dbh.class)
SF.N.mass = SF.N.mass[  , c(2:5)]
SF.N.mass$date = as.Date(SF.N.mass$date)

# STEP 1c: MEAN  vals by dbh.class+DATE:
SF.N.mass.melted = melt(SF.N.mass, id = c("date","dbh.class"))
meandbhSF <- dcast(SF.N.mass.melted,  date + dbh.class ~ variable, mean, na.rm = TRUE) 

# STEP2b: MEAN vals by date:
SF.N.mass = melt(meandbhSF, id = c("date","dbh.class"))
SF.N.mass <- dcast(SF.N.mass,  date ~ variable, mean, na.rm = TRUE) 

colnames(SF.N.mass) = c("date","SF.NO3-N","SF.NH4.N")


#housekeeping
rm(SF, dates, days, diffdays, stemdbh, dbh.class, meandbhSF, days.dates, SF.N.mass.melted, SFlab, SFNO3, SFNH4, SFsamples)

# -------------------------------------------------------------
# 2.    THROUGHFALL
# -------------------------------------------------------------
# CREATE A MEAN TF "DAILY" VALUE  BY DATE
TF = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' ORDER BY date")

# vector containing all SF samples names
TFsamples= unique(TF$sample)
# extract lab values by sample, name and N form separately
TFNO3=labNO3[labNO3$sample%in%TFsamples,]
TFNH4=labNH4[labNH4$sample%in%TFsamples,]

# merge TF lab vals
TFlab = merge(TFNO3, TFNH4, by.x = c("date", "sample"), by.y = c("date", "sample"), all = TRUE)
TFlab= TFlab[, c(1,2,5,8)]
colnames(TFlab)= c("date", "sample", "NO3.N", "NH4.N")

# merge  TF field vals (volumes) and lab vals (concentrations)
TF = merge(TF, TFlab, by.x = c("date", "sample"), by.y = c("date", "sample"), all = TRUE)

# calculate the N mass per sample per date per N form
TF$NO3.N=TF$vals*TF$NO3.N
TF$NH4.N=TF$vals*TF$NH4.N

# cleaning up the df
TF = TF[ , c(1,2,10,11)]

# How many days from last sampling?
dates=unique(TF$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
days.dates=cbind(dates,days)

TF.N.mass=merge(TF,days.dates,by.x="date",by.y="dates",na.rm=FALSE)
TF.N.mass$days = as.numeric(as.character(TF.N.mass$days))

TF.N.mass$dailyNO3 = TF.N.mass$NO3.N/TF.N.mass$days # this creates a NO3.N "daily value" to make different periods comparable
TF.N.mass$dailyNH4 = TF.N.mass$NH4.N/TF.N.mass$days # this creates a NH4.N "daily value" to make different periods comparable

TF.N.mass = TF.N.mass[ , c(1,2,6,7)]

TF.depth = melt(TF.N.mass, id = c("date","sample"))
TF.N.mass <- dcast(TF.depth,  date ~ variable, mean, na.rm = TRUE) 

colnames(TF.N.mass) = c("date","TF.NO3-N","TF.NH4.N")

#housekeeping
rm(TF, dates, days, diffdays, days.dates, TF.depth, TFlab, TFNH4, TFNO3, TFsamples)

# -------------------------------------------------------------
# 3.    RAINFALL
# -------------------------------------------------------------
RF = dbGetQuery(db, "SELECT * FROM fielddata WHERE (sample = 'C30D1' or sample = 'C31D1') ORDER BY date")

# vector containing all SF samples names
RFsamples= c("C30D1", "C31D1")

# extract lab values by sample, name and N form separately
RFNO3=labNO3[labNO3$sample%in%RFsamples,]
RFNH4=labNH4[labNH4$sample%in%RFsamples,]

# merge RF lab vals
RFlab = merge(RFNO3, RFNH4, by.x = c("date", "sample"), by.y = c("date", "sample"), all = TRUE)
RFlab= RFlab[, c(1,2,5,8)]
colnames(RFlab)= c("date", "sample", "NO3.N", "NH4.N")

# merge  RF field vals (volumes) and lab vals (concentrations)
RF = merge(RF, RFlab, by.x = c("date", "sample"), by.y = c("date", "sample"), all = TRUE)

# calculate the N mass per sample per date per N form
RF$NO3.N=RF$vals*RF$NO3.N
RF$NH4.N=RF$vals*RF$NH4.N

# cleaning up the df
RF = RF[ , c(1,2,10,11)]
########################################################################################
# How many days from last sampling?
dates=unique(RF$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
days.dates=cbind(dates,days)

RF.N.mass = melt(RF, id = c("date","sample"))
RF.N.mass <- dcast(RF.N.mass,  date ~ variable, mean, na.rm = TRUE) 

colnames(RF.N.mass) = c("date","RF.NO3-N","RF.NH4.N")



# housekeeping
rm(RF, RFlab, RFNH4, RFNO3, RFsamples)

# -------------------------------------------------------------
# 4.    FOG COLLECTOR
# -------------------------------------------------------------
fog = dbGetQuery(db, "SELECT * FROM fielddata WHERE sample = 'C30D2' ORDER BY date")

# extract lab values by sample, name and N form separately
fogNO3=labNO3[labNO3$sample=="C30D2",]
fogNH4=labNH4[labNH4$sample=="C30D2",]

# merge fog lab vals
foglab = merge(fogNO3, fogNH4, by.x = c("date", "sample"), by.y = c("date", "sample"), all = TRUE)
foglab= foglab[, c(1,5,8)]
colnames(foglab)= c("date", "NO3.N", "NH4.N")

# merge  fog field vals (volumes) and lab vals (concentrations)
fog = merge(fog, foglab, by = "date", all = TRUE)

# calculate the N mass per sample per date per N form
fog$NO3.N = fog$vals*fog$NO3.N
fog$NH4.N = fog$vals*fog$NH4.N

# cleaning up the df
fog.N.mass = fog[ , c(1,10,11)]

# How many days from last sampling?
dates=unique(fog$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
days.dates=cbind(dates,days)

fog.N.mass=merge(fog.N.mass,days.dates,by.x="date",by.y="dates",na.rm=FALSE)
fog.N.mass$days = as.numeric(as.character(fog.N.mass$days))

fog.N.mass$dailyNO3 = fog.N.mass$NO3.N/fog.N.mass$days # this creates a NO3.N "daily value" to make different periods comparable
fog.N.mass$dailyNH4 = fog.N.mass$NH4.N/fog.N.mass$days # this creates a NH4.N "daily value" to make different periods comparable

fog.N.mass = fog.N.mass[ , c(1,5,6)]

colnames(fog.N.mass) = c("date","fog.NO3-N","fog.NH4.N")



# housekeeping
rm(fog, foglab, fogNH4, fogNO3, labNH4, labNO3, days.dates, dates, days, db, diffdays)

# cbind the dataframes
x = cbind(RF.N.mass, fog.N.mass, TF.N.mass, SF.N.mass)

# delete duplicated columns
x <- x[, !duplicated(colnames(x))]


library(psych) 
pairs.panels(x[c(2,4,6,8)]) # select NO3s
pairs.panels(x[c(3,5,7,9)]) # select NH4s

