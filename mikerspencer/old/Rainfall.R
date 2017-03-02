# --------------------------------------------------------
#         3. RAINFALL NO3-N and NH4-N (mg/mq?)
# --------------------------------------------------------
#NB RF è una massa per giorno!!!!

# select SF data from SQL db

rf = dbGetQuery(db, "SELECT date, sample, site, variable, vals FROM fielddata WHERE variable = 'precip depth'  ORDER BY date")

# how many days from last sampling?

dates = dbGetQuery(db, "SELECT DISTINCT(date) FROM fielddata ORDER BY date")
days = as.POSIXlt(dates$date)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
dd.dates=cbind(dates,days) #merge days from last sampling with the respective sampling date

# --------------------------------------------------------
#         PRECHECK. RAINFALL AND FOG VOLUMES (mm/sampling date)
# --------------------------------------------------------

rf2 = subset(rf, sample=="C30D1"|sample=="C31D1")
d=unique(rf2$date) #create a column to bind with the mean
RF.mean = tapply(rf2$vals, rf2$date, mean, na.rm = TRUE)
RF.mean=as.numeric(RF.mean)
RF.vol.d = cbind(d,RF.mean)
RF.vol = merge(RF.vol.d, dd.dates, by.x="d", by.y="date")
class(RF.vol)
RF.vol[,"days"] = as.numeric(RF.vol$days,units="days")
RF.vol[,"RF.mean"] = as.numeric(paste(RF.vol$RF.mean))
RF.vol$RF.vol=RF.vol$RF.mean*RF.vol$days
RF.mmeq=RF.vol[ , c(1,4)]

# FOG VOLUMES
fog = subset(rf, sample=="C30D1"|sample=="C30D2")
d=unique(fog$date) #create a column to bind with the difference
fog = tapply(fog$vals, fog$date, FUN = diff, na.rm = TRUE)
fog=as.numeric(fog)
fog.vol.d = cbind(d, fog)

#---------------------------------------------------------

rf1= subset(rf, sample=="C30D1"|sample=="C30D2"|sample=="C31D1")

#### 3a. RF AND FOG N-NO3 in g/ha ####------------------------------------------

# calculate RF.NO3 merging fielddata, labdata, multiplying per period of sampling (* days)
RF.NO3=merge(rf1,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))
rf.NO3=merge(RF.NO3,dd.dates, by.x="date", by.y="date")
rf.NO3$NO3.N=rf.NO3$vals.x*rf.NO3$vals.y*rf.NO3$days*10000/1000

#1/12/15 only has data from D30D2. To be decided what to do with data
# add these 9 days to the following campaign?)

# housekeeping; in case I need q/Q controls here is where to operate
rf.NO3=rf.NO3[,c("date","sample","variable.y","NO3.N")] 
colnames(rf.NO3)=c("date","sample","N.form","value")

# MEAN RF NO3-N:
rf.NO3.sub=subset(rf.NO3, sample=="C30D1"|sample=="C31D1")
#rf.NO3.sub$rf.NO3.mean=tapply(rf.NO3.sub$value,rf.NO3.sub$date,mean)
#rf.NO3.sub$rf.NO3.mean=NULL
rf.NO3.MEAN= tapply(rf.NO3.sub$value,rf.NO3.sub$date,mean)

# FOG NO3-N:
FOG.NO3.sub=subset(rf.NO3, sample=="C30D1"|sample=="C30D2")
#rf.NO3.sub$rf.NO3.mean=tapply(rf.NO3.sub$value,rf.NO3.sub$date,mean)
#rf.NO3.sub$rf.NO3.mean=NULL
FOG.NO3= tapply(FOG.NO3.sub$value,FOG.NO3.sub$date, diff, na.rm=TRUE)


#### 3b. RF N-NH4 in g/ha ####-------------------------------------------
RF.NH4=merge(rf1,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
rf.NH4=merge(RF.NH4,dd.dates, by.x="date", by.y="date")
rf.NH4$NH4.N=rf.NH4$vals.x*rf.NH4$vals.y*rf.NH4$days*10000/1000

# housekeeping; in case I need q/Q controls here is where to operate
rf.NH4=rf.NH4[,c("date","sample","variable.y","NH4.N")] 
colnames(rf.NH4)=c("date","sample","N.form","value")

# MEAN RF NH4-N:
rf.NH4.sub=subset(rf.NH4, sample=="C30D1"|sample=="C31D1")
rf.NH4.MEAN= tapply(rf.NH4.sub$value,rf.NH4.sub$date,mean, na.rm=TRUE)

# FOG NH4-N:
FOG.NH4.sub=subset(rf.NH4, sample=="C30D1"|sample=="C30D2")
FOG.NH4= tapply(FOG.NH4.sub$value,FOG.NH4.sub$date,diff, na.rm=TRUE)
#####   create rf.MEAN as a data frame ######
rf.FOG.MEAN = data.frame(rf.NO3.MEAN, rf.NH4.MEAN, FOG.NO3, FOG.NH4)



#housekeeping:
rm(FOG.NO3.sub, FOG.NO3, FOG.NH4.sub, FOG.NH4, NH4data, NO3data, RF, RF.C30D1, RF.C30D2, RF.FOG, RF.fog, RF.NH4 ,RF.NO3 ,RF.sub ,RF1 ,RF.NO3.C30D1,RF.NO3.C30D2,RF.NO3.FOG,RF.NO3.fog,RF.NO3.sub,RF.NH4.C30D1,RF.NH4.C30D2,RF.NH4.FOG,RF.NH4.fog,RF.NH4.sub)

#RF.NO3.MEAN=merge(dd.dates,rf.NO3.MEAN, by.x="date",by.y="row.names")
#RF.NH4.MEAN=merge(dd.dates,rf.NH4.MEAN, by.x="date",by.y="row.names")


#RF.TF.SF= data.frame(RF.NO3.MEAN,RF.NH4.MEAN,SF.MEAN.ha,TF.MEAN.ha)
#class(RF.TF.SF)

