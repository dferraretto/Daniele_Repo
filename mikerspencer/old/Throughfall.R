# --------------------------------------------------------
#  Daniele Ferraretto, working on data in Griffin,SQLite
#  started on 15th October, 2015
#  updated: 15/10/2015          last update: 15/10/2015
# --------------------------------------------------------
# --------------------------------------------------------

# select TF data from db
tf = dbGetQuery(db, "SELECT date, sample, site, variable, vals FROM fielddata WHERE variable = 'through vol' ORDER BY date")

# how many days from last sampling?
dates=unique(tf$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
dd.dates=cbind(dates,days) #merge days from last sampling with the respective sampling date
# --------------------------------------------------------
#         PRECHECK. THROUGHFALL VOLUMES (l/sampling date)
# --------------------------------------------------------

# OVERALL MEAN TF VOL

TF.vol = tapply(tf$vals,tf$date,mean,na.rm=TRUE) # TF vol to be compared with RF and SF
TF.mmeq= cbind(dd.dates,TF.vol)

# --------------------------------------------------------
#         1. THROUGHFALL NO3-N and NH4-N (mg/mq)
# --------------------------------------------------------

TF.NO3=merge(tf,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))
TF.NO3$NO3.N=TF.NO3$vals.x*TF.NO3$vals.y

# in case I need q/Q controls here is where to operate
TF.NO3=TF.NO3[,c("date","sample","site.x","variable.x","NO3.N")] 
colnames(TF.NO3)=c("date","sample","plot","var","NO3.N")

TF.NH4=merge(tf,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
TF.NH4$NH4.N=TF.NH4$vals.x*TF.NH4$vals.y
# in case I need q/Q controls here is where to operate
TF.NH4=TF.NH4[,c("date","sample","site.x","variable.x","NH4.N")] 
colnames(TF.NH4)=c("date","sample","plot","var","NH4.N")
#View(TF.NH4)

#####   MEAN TF Nxx-N ( mg per mq?)  ###### 

#####  OVERALL MEAN TF NO3-N
TF.NO3.mean = tapply(TF.NO3$NO3.N,TF.NO3$date,mean,na.rm=TRUE)

#####  OVERALL MEAN TF NH4-N
TF.NH4.mean = tapply(TF.NH4$NH4.N,TF.NH4$date,mean,na.rm=TRUE)

# MEAN TF NO3-N  PER T/C PLOT
TFC.NO3=subset(TF.NO3,plot=="Control")
TFT.NO3=subset(TF.NO3,plot=="Treatment")
# Control mean TF NO3-N
TFC.NO3.mean = tapply(TFC.NO3$NO3.N,TFC.NO3$date,mean,na.rm=TRUE)
# Treatment mean TF NO3-N
TFT.NO3.mean = tapply(TFT.NO3$NO3.N,TFT.NO3$date,mean,na.rm=TRUE)



# MEAN TF NH4-N  PER T/C PLOT
TFC.NH4=subset(TF.NH4,plot=="Control")
TFT.NH4=subset(TF.NH4,plot=="Treatment")
# Control mean TF NH4-N
TFC.NH4.mean = tapply(TFC.NH4$NH4.N,TFC.NH4$date,mean,na.rm=TRUE)
# Treatment mean TF NH4-N
TFT.NH4.mean = tapply(TFT.NH4$NH4.N,TFT.NH4$date,mean,na.rm=TRUE)

### building up TF.MEAN matrix
TF.MEAN=cbind(TF.NO3.mean,TF.NH4.mean,TFC.NO3.mean,TFC.NH4.mean,TFT.NO3.mean,TFT.NH4.mean)

## TF.MEAN in g/ha.
TF.MEAN.ha=TF.MEAN*10000/1000

# housekeeping
rm(TF.NO3,TF.NH4,TFC.NO3,TFT.NO3,TFC.NO3.mean,TFT.NO3.mean,TFC.NH4,TFT.NH4,TF.NH4.mean,TF.MEAN, TF.NO3.mean,TFC.NH4.mean,TFT.NH4.mean)
