# --------------------------------------------------------
#  Daniele Ferraretto, creating a daily file from Griffin
#  data in SQLite so to be able to aggregrate per month.
#  started on 3rd of November, 2015
#  updated: 06/11/2015             last update: 10/11/2015
# --------------------------------------------------------
# --------------------------------------------------------

# clear the memory
rm(list=ls())

library(RSQLite)
library(zoo)

### set working dir for pc: 
setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo/Daniele_Repo")
### setwd per desktop
setwd("M:/My PhD/R/PhD-local_repo")

# remove the outliers from the Griffin SQlite: at the moment only manually by using Outliers_aug2016 to identify them and Outliers to remove them
#source("mikerspencer/Outliers.R")


db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

#####################################################
#               Part 1: lab data
#####################################################

# Query SQLite on labdata table not by DESCending date

labdata = dbGetQuery(db, "SELECT rowid, date, sample, site, variable, vals FROM labdata ORDER BY date")
#get rid of NA before proceeding with na.locf:
labdata[is.na(labdata)] = 99999

NO3lab=subset(labdata, variable =="NO3.N")
NH4lab=subset(labdata, variable =="NH4.N")

enddate = as.Date(labdata[nrow(labdata),"date"]) #extracts the date from the last row

dd.Griffin=rep(seq(as.Date("2011-10-11"), enddate, by = "day"), each = 52) # Creates a vector of daily dates 
# from 11thOctober 2011 to the last added date, each date repeated 52 times (1 per sample)

sample = unique(labdata$sample) # creates sample set

#--------- working on Nform subsets to build up my new daily dataframe

NH4.N = "NH4.N" # 3rd column of my "subkey" matrix: NH4.N
key1=data.frame(cbind(as.Date(dd.Griffin), sample, NH4.N)) # creates a df of 3 columns:date,sample,N form
key1$V1=as.Date(as.numeric(key1[,1]), origin = "2011-10-10")
dd.labNH4 = merge(key1, NH4lab, by.x = c("V1","sample", "NH4.N"), by.y = c("date","sample", "variable"), all.x= TRUE, na.rm=FALSE)
dd.labNH4 = dd.labNH4[with(dd.labNH4, order(sample)), ] # reorder to use na.locf
colnames(dd.labNH4)[3] = "Nform"

NO3.N = "NO3.N"# 3rd column of my "subkey" matrix: NO3.N
key2=data.frame(cbind(as.Date(dd.Griffin), sample, NO3.N))
key2$V1=as.Date(as.numeric(key2[,1]), origin = "2011-10-10")
dd.labNO3 = merge(key2, NO3lab, by.x = c("V1","sample", "NO3.N"), by.y = c("date","sample", "variable"), all.x= TRUE, na.rm=FALSE)
dd.labNO3 = dd.labNO3[with(dd.labNO3, order(sample)), ] # reorder to use na.locf
colnames(dd.labNO3)[3] = "Nform"

dd.lab=rbind(dd.labNH4, dd.labNO3) # creates labdata by day. NB: litter samples are missing here, same with POC
colnames(dd.lab)[1] = "dates"

# LABDATA ARE NOW READY TO BE NA.LOCF

dd.NX = na.locf(dd.lab,fromLast = TRUE) # daily vlaues
#dd.NX$dates=as.Date(dd.NX$dates)
dd.NX$vals=as.numeric(dd.NX$vals)
dd.NX[dd.NX == 99999] <- NA # replaced 99999, used to properly apply na.locf, with NA

dd.NX=drop(dd.NX[,c("dates","sample","Nform","site","vals")])
colnames(dd.NX)=c("date","sample","variable","site","vals")
dd.NX=dd.NX[c(1,2,4,3,5)]
# Housekeeping:
rm(dd.lab, dd.labNO3,dd.labNH4,key1,key2,labdata,NH4.N,NH4lab,NO3.N,NO3lab,dd.Griffin,enddate,sample)# Query SQLite on labdata table not by DESCending date

#####################################################
#               Part 2: field data
#####################################################

x = dbGetQuery(db, "SELECT date, sample, site, variable, vals, overflowing, QC FROM fielddata ORDER BY date")

enddate = as.Date(x[nrow(x),"date"]) #extracts the date from the last row

# NOW I NEED TO CUSTOMISE THIS PER EACH VARIABLE LEVEL: 

# how many days from last sampling?
dates=unique(x$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
dd.dates=cbind(dates,days)
fielddata=merge(x,dd.dates,by.x="date",by.y="dates",na.rm=FALSE)
fielddata$days=as.numeric(levels(fielddata$days))[fielddata$days] #this is to "read" days as a number = to the level (weird things happens otherwise...)

#  THE FOLLOWING LINE TURNS ALL CUMULATED RAW VALUES INTO DAILY VALUES:
fielddata$d.vals=fielddata$vals/fielddata$days
# "fill" the NA here, not to mess them with the new data NA later
fielddata[is.na(fielddata)] = 99999

# preparing each level of variables for the dd.fielddata:
#subsetting and adding days since last sample

#-------------      STEMFLOW      ----------------
dd.sf=subset(fielddata, variable == "stem vol")
stemvol = "stem vol" # 3rd column of my "subkey" matrix
sample = c("C10S1", "C10S2", "C10S3", "C11S1", "C11S2", "C11S3", "C11S4", "C11S5", "C11S6", "C11S7", "C12S1", "C12S2", "C12S3", "T10S1", "T10S2", "T10S3", "T11S1", "T11S2", "T11S3", "T12S1", "T12S2", "T12S3")
dd.Griffin=rep(seq(as.Date("2011-10-11"), enddate, by = "day"), each = 22) # Creates a vector of daily dates 
key1=data.frame(cbind(as.Date(dd.Griffin), sample, stemvol)) # creates a df of 3 columns:date,sample,N form
key1$V1=as.Date(as.numeric(key1[,1]), origin = "2011-10-10")
dd.stemvol = merge(key1, dd.sf, by.x = c("V1","sample","stemvol"), by.y = c("date","sample","variable"), all = TRUE, na.rm=FALSE)
dd.stemvol = dd.stemvol[with(dd.stemvol, order(sample)), ] # reorder to use na.locf
colnames(dd.stemvol)[3] = "variable"
# note: trying with three keys without specifying each set of sample levels didnt work :(

#-------------      THROUGHFALL   VOLUMES   ----------------
dd.tf=subset(fielddata, variable == "through vol")
throughvol = "through vol" # 3rd column of my "subkey" matrix
sample = c("C10T1", "C10T2", "C10T3", "C11T1", "C11T2", "C11T3", "C12T1", "C12T2", "C12T3", "T10T1", "T10T2", "T10T3", "T11T1", "T11T2", "T11T3", "T12T1", "T12T2", "T12T3")
dd.Griffin=rep(seq(as.Date("2011-10-11"), enddate, by = "day"), each = 18) # Creates a vector of daily dates 
key2=data.frame(cbind(as.Date(dd.Griffin), sample, throughvol)) # creates a df of 3 columns:date,sample,N form
key2$V1=as.Date(as.numeric(key2[,1]), origin = "2011-10-10")
dd.throughvol = merge(key2, dd.tf, by.x = c("V1","sample","throughvol"), by.y = c("date","sample","variable"), all= TRUE, na.rm=FALSE)
dd.throughvol = dd.throughvol[with(dd.throughvol, order(sample)), ] # reorder to use na.locf
colnames(dd.throughvol)[3] = "variable"

#-------------      THROUGHFALL DEPTH    ----------------
dd.tfd=subset(fielddata, variable == "through depth")
throughdepth = "through depth" # 3rd column of my "subkey" matrix
sample = c("C10T1", "C10T2", "C10T3", "C11T1", "C11T2", "C11T3", "C12T1", "C12T2", "C12T3", "T10T1", "T10T2", "T10T3", "T11T1", "T11T2", "T11T3", "T12T1", "T12T2", "T12T3")
dd.Griffin=rep(seq(as.Date("2011-10-11"), enddate, by = "day"), each = 18) # Creates a vector of daily dates 
key7=data.frame(cbind(as.Date(dd.Griffin), sample, throughdepth)) # creates a df of 3 columns:date,sample,N form
key7$V1=as.Date(as.numeric(key7[,1]), origin = "2011-10-10")
dd.throughdepth = merge(key7, dd.tfd, by.x = c("V1","sample","throughdepth"), by.y = c("date","sample","variable"), all= TRUE, na.rm=FALSE)
dd.throughdepth = dd.throughdepth[with(dd.throughdepth, order(sample)), ] # reorder to use na.locf
colnames(dd.throughdepth)[3] = "variable"
# ---------------------------------------------------------------------------

#-------------      PRECIPITATION      --------------------------------------
dd.precip=subset(fielddata, variable =="precip depth")
prec = "precip depth" # 3rd column of my "subkey" matrix: NH4.N
sample = c("C30D1", "C30D2", "C31D1")
dd.Griffin=rep(seq(as.Date("2011-10-11"), enddate, by = "day"), each = 3) # Creates a vector of daily dates 
key6=data.frame(cbind(as.Date(dd.Griffin), sample, prec)) # creates a df of 3 columns:date,sample,N form
key6$V1=as.Date(as.numeric(key6[,1]), origin = "2011-10-10")
dd.prec = merge(key6, dd.precip, by.x = c("V1","sample", "prec"), by.y = c("date","sample", "variable"), all= TRUE, na.rm=FALSE)
dd.prec = dd.prec[with(dd.prec, order(sample)), ] # reorder to use na.locf
colnames(dd.prec)[3] = "variable" # note: precip/prec may lead to confusion when reading this in the future
colnames(dd.prec)[5] = "d.vals" #  as prec values originarily come in daily value, I need to play this "trick"
colnames(dd.prec)[9] = "vals" #  and exchange vals/d.vals names so tu use the real daily value instead than the obtained d.vals
# ---------------------------------------------------------------------------

#-------------      STREAM FLOW      ----------------------------------------
dd.vnotch=subset(fielddata, variable =="v-notch flow")
# turn it into the original value of l/s and turn it into a daily value (l/d); useless to turn them into m3 here as i need this data in liters to multiply it by concentration
dd.vnotch$d.vals=dd.vnotch$d.vals*dd.vnotch$days*24*3600
vnotch = "v-notch flow" # 3rd column of my "subkey" matrix: NH4.N
sample = c("C20SW1", "C21SW1", "T20SW1", "T21SW1")
dd.Griffin=rep(seq(as.Date("2011-10-11"), enddate, by = "day"), each = 4) # Creates a vector of daily dates 
key5=data.frame(cbind(as.Date(dd.Griffin), sample, vnotch)) # creates a df of 3 columns:date,sample,N form
key5$V1=as.Date(as.numeric(key5[,1]), origin = "2011-10-10")
dd.vntch = merge(key5, dd.vnotch, by.x = c("V1","sample", "vnotch"), by.y = c("date","sample", "variable"), all= TRUE, na.rm=FALSE)
dd.vntch = dd.vntch[with(dd.vntch, order(sample)), ] # reorder to use na.locf
colnames(dd.vntch)[3] = "variable"


dd.stageboard=subset(fielddata, variable =="stageboard cm")
# back to the field reading:
dd.stageboard$d.vals=dd.stageboard$d.vals*dd.stageboard$days
stageboard = "stageboard cm" # 3rd column of my "subkey" matrix: NH4.N
sample = c("C20SW1", "C21SW1", "T20SW1", "T21SW1")
dd.Griffin=rep(seq(as.Date("2011-10-11"), enddate, by = "day"), each = 4) # Creates a vector of daily dates 
key4=data.frame(cbind(as.Date(dd.Griffin), sample, stageboard)) # creates a df of 3 columns:date,sample,N form
key4$V1=as.Date(as.numeric(key4[,1]), origin = "2011-10-10")
dd.stg = merge(key4, dd.stageboard, by.x = c("V1","sample", "stageboard"), by.y = c("date","sample", "variable"), all= TRUE, na.rm=FALSE)
dd.stg = dd.stg[with(dd.stg, order(sample)), ] # reorder to use na.locf
colnames(dd.stg)[3] = "variable"


dd.gauged=subset(fielddata, variable =="gauged flow")
# turn it into the original value of l/s and turn it into a daily value (l/d)
dd.gauged$d.vals=dd.gauged$d.vals*dd.gauged$days*24*3600
gauged = "gauged flow" # 3rd column of my "subkey" matrix: NH4.N
sample = c("C20SW1", "C21SW1", "T20SW1", "T21SW1")
dd.Griffin=rep(seq(as.Date("2011-10-11"), enddate, by = "day"), each = 4) # Creates a vector of daily dates 
key3=data.frame(cbind(as.Date(dd.Griffin), sample, gauged)) # creates a df of 3 columns:date,sample,N form
key3$V1=as.Date(as.numeric(key3[,1]), origin = "2011-10-10")
dd.gf = merge(key3, dd.gauged, by.x = c("V1","sample", "gauged"), by.y = c("date","sample", "variable"), all= TRUE, na.rm=FALSE)
dd.gf = dd.gf[with(dd.gf, order(sample)), ] # reorder to use na.locf
colnames(dd.gf)[3] = "variable"
# ---------------------------------------------------------------------------

#  Building up my daily df: stavolta ho solo date e sample da ordinare, dovrei riuscire...
#manca una riga di variable da poter "attaccare" alla chiave

dd.field = rbind(dd.stemvol, dd.throughvol, dd.throughdepth, dd.prec, dd.vntch, dd.stg, dd.gf)

dd.field = dd.field[with(dd.field, order(sample)), ] # reorder to use na.locf

# FIELDDATA ARE NOW READY TO BE NA.LOCF

dd.Field = na.locf(dd.field,fromLast = TRUE) # daily values
dd.Field$d.vals=as.numeric(dd.Field$d.vals)
dd.Field[dd.Field == 99999] <- NA # replaced 99999, used to properly apply na.locf, with NA

colnames(dd.Field)[1] <- "date"
dd.field=dd.Field[,c(1,2,4,3,9,6,7)]# reorder columns for SQLite db. NB: if I need the "days" column here is where I should keep it (and change the "Create_daily_db.R" fielddata structure)

#disconnecting from Griffin db
dbDisconnect(db)

# Now I have two dataframes: 1) dd.Field for all the field data on a daily base and
# 2) dd.NX with all the lab data related to N forms on the same daily base.

# creating a new db in SQLite where to append daily values:

source("mikerspencer/Create_daily_db.R")

# appending fieldata and part of labdata (Nform) 

dbWriteTable(conn=db, name="labdata", dd.NX, append=T, row.names=F) #NX data added to the db

dbWriteTable(conn=db, name="fielddata", dd.field, append=T, row.names=F)

dbDisconnect(db)

# Daily_Griffin.SQLite is my new db with daily values. Now I can proceed to query per month!

rm(db, dd.field, dd.NX, dd.Griffin, x, dd.dates, dates, days, diffdays, enddate, sample, throughvol, throughdepth, fielddata, dd.throughvol, dd.throughdepth, dd.Field, dd.stemvol, dd.prec, dd.vntch, dd.stg, dd.gf, gauged, stageboard, vnotch, prec, stemvol, key1, key2, key3, key4, key5, key6, key7, dd.sf, dd.tf, dd.tfd, dd.gauged, dd.stageboard, dd.vnotch, dd.precip)
#housekeeping


