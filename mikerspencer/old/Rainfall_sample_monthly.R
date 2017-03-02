# Created: 11/05/2016, taken from the rainfall_monthly to consider samples instead than mean value

# --------------------------------------------------------
#         3. RAINFALL NO3-N and NH4-N (kg/ha/month/sample)
# --------------------------------------------------------


rf = dbGetQuery(db, "SELECT * FROM fielddata WHERE sample = 'C30D1' or sample = 'C31D1'   ORDER BY date")

###                   RF NO3                         ###
# STEP 0: calculate the monthly precipitation



# STEP 1: merge depth and concentration, clean the merged file
rf$mY=strftime(rf$date,"%Y%m") # creates month-Year column
RF.NO3=merge(rf,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))
RF.NO3$value=RF.NO3$vals.x*RF.NO3$vals.y*10000/1000000 #turns mg/m2 into kg/ha
# in case I need q/Q controls here is where to operate
RF.NO3=RF.NO3[,c("mY","sample","site.x","variable.x","value")] 
colnames(RF.NO3)=c("mY","sample","plot","var","value")
RF.NO3$var="NO3N.mass.RF"

# STEP 2: aggregate by month 

sampleNO3.RF = aggregate(value ~ mY+sample,  data = RF.NO3, FUN = sum)



# STEP 3: prepare the df to be bound to RF, SF (and/or plot values)

sampleNO3.RF$var = "NO3N.mass.RF"



###                   RF NH4                         ###


# STEP 1: merge depth and concentration, clean the merged file
rf$mY=strftime(rf$date,"%Y%m") # creates month-Year column
RF.NH4=merge(rf,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
RF.NH4$value=RF.NH4$vals.x*RF.NH4$vals.y*10000/1000000 #turns mg/m2 into kg/ha
# in case I need q/Q controls here is where to operate
RF.NH4=RF.NH4[,c("mY","sample","site.x","variable.x","value")] 
colnames(RF.NH4)=c("mY","sample","plot","var","value")
RF.NH4$var="NH4N.mass.RF"

# STEP 2: aggregate by month 

sampleNH4.RF = aggregate(value ~ mY+sample,  data = RF.NH4, FUN = sum)



# STEP 3: prepare the df to be bound to RF, SF (and/or plot values)

sampleNH4.RF$var = "NH4N.mass.RF"


#housekeeping
rm(rf, RF.NO3, RF.NH4)

