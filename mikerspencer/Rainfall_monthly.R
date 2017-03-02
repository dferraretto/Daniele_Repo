# Created: 28th November 2015. Updated (for the long to wide format): 21/04/2016

# --------------------------------------------------------
#         3. RAINFALL NO3-N and NH4-N (mg/mq?)
# --------------------------------------------------------
# note: this script only consiers the rainfall gauges and not the fog gauge.
# I can build up a script to check rain vs fog values and take decisions on how to consider
# the potentially extra N collected as fog (partly being dry dep?). This assumption is,
# however, reductive in terme of N input and hence more cautious.

rf = dbGetQuery(db, "SELECT * FROM fielddata WHERE sample = 'C30D1' or sample = 'C31D1'   ORDER BY date")

###                   RF NO3                         ###
# STEP 0: calculate the monthly precipitation

RF.mmeq=aggregate(vals ~ date, data = rf, FUN = mean, na.rm = TRUE )
RF.mmeq$mY=strftime(RF.mmeq$date,"%Y%m") # creates month-Year column
RF.m.vol = aggregate (vals ~ mY, data = RF.mmeq, FUN = sum) 

# STEP 1: merge depth and concentration, clean the merged file
RF.NO3=merge(rf,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))
RF.NO3$NO3.N=RF.NO3$vals.x*RF.NO3$vals.y*10000/1000000 #turns mg/m2 into kg/ha
# in case I need q/Q controls here is where to operate
RF.NO3=RF.NO3[,c("date","sample","site.x","variable.x","NO3.N")] 
colnames(RF.NO3)=c("date","sample","plot","var","NO3.N.RF")

# STEP 2: MEAN  vals by DATE:
meanddNO3RF <- aggregate(NO3.N.RF ~ date, data = RF.NO3, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# STEP 3: aggregate by month 
meanddNO3RF$mY=strftime(meanddNO3RF$date,"%Y%m") # creates month-Year column

monthlyNO3.RF = aggregate(NO3.N.RF ~ mY,  data = meanddNO3RF, FUN = sum)



# STEP 4: prepare the df to be bound to RF, SF (and/or plot values)

NO3.rf = "RF.NO3"
both = "both"

mNO3RF=data.frame(cbind(monthlyNO3.RF, both, NO3.rf))
colnames(mNO3RF)=c("mY", "vals", "site", "var")

###                   RF NH4                         ###

# STEP 1: merge depth and concentration, clean the merged file
RF.NH4=merge(rf,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
RF.NH4$NH4.N=RF.NH4$vals.x*RF.NH4$vals.y*10000/1000000 #turns mg/m2 into kg/ha
# in case I need q/Q controls here is where to operate
RF.NH4=RF.NH4[,c("date","sample","site.x","variable.x","NH4.N")] 
colnames(RF.NH4)=c("date","sample","plot","var","NH4.N.RF")

# STEP 2: MEAN  vals by DATE:
meanddNH4RF <- aggregate(NH4.N.RF ~ date, data = RF.NH4, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# STEP 3: aggregate by month 
meanddNH4RF$mY=strftime(meanddNH4RF$date,"%Y%m") # creates month-Year column

monthlyNH4.RF = aggregate(NH4.N.RF ~ mY,  data = meanddNH4RF, FUN = sum)

# STEP 4: prepare the df to be bound to RF, SF (and/or plot values)

NH4.rf = "RF.NH4"
both = "both"

mNH4RF=data.frame(cbind(monthlyNH4.RF, both, NH4.rf))
colnames(mNH4RF)=c("mY", "vals", "site", "var")

#housekeeping
rm(RF.mmeq, meanddNH4RF, both, meanddNO3RF, NH4data, NO3data, rf, RF.NO3, RF.NH4, NH4.rf, NO3.rf)

