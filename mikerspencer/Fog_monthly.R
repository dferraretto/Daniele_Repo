## Created: 01st March 2016
# --------------------------------------------------------
#         3. FOG NO3-N and NH4-N (g/ha/day)
# --------------------------------------------------------
# 
# This script checks (rain vs) fog values and take decisions on how to consider
# the potentially extra N collected as fog (partly being dry dep?). This assumption is,
# however, reductive in terme of N input and hence more cautious.

fog = dbGetQuery(db, "SELECT * FROM fielddata WHERE sample = 'C30D1' or sample = 'C30D2'   ORDER BY date")

###                   FOG NO3                         ###
# STEP 0: calculate the monthly fog depth

fog.mmeq=aggregate(vals ~ date, data = fog, FUN = diff, na.rm = TRUE ) # calcola i mm eq per day di fog come diff tra D2 e D1 [1]
fog.mmeq$mY=strftime(fog.mmeq$date,"%Y%m")                             # creates month-Year column
fog.m.vol = aggregate (vals ~ mY, data = fog.mmeq, FUN = sum)          # aggrega i mm giornalieri in un valore mensile [2]  

###      NOTES ON NEGATIVE VALUES        and                           suggested actions:

# May 2016: pipe out on fog bottle.                                    give an estimated volume by  VOL regression (average rain/fog)
# April 16: OK
# March 16: OK
# Feb   16: funnel on ground, intact. same with D1.                    should have likely been FULL -> 
# Jan   16: vols app. OK, but fog catcher down+pipe out                  ???
# Dec.  16: fog catcher down, vols almost OK, should have been full.    ???
#  nov. 16: pipe out?                                                   volume by regression?
# ...   OK
# May 2012: hard to explain. C31D1 higher vol, than both D2 and C30D1. No notes about it.

# Insert any punctual substitution of volume values in the Griffin SQLite file before splitting data into daily vals. Until then I will substitute neg vals with
# a "fake" abs val

#fog.m.vol$vals = abs(fog.m.vol$vals) # to delete once single vals are amended by regression

###                   fog NO3                         ###  

# STEP 1: merge depth and concentration, clean the merged file
fog.NO3=merge(fog,NO3data,by.x=c("date","sample"), by.y=c("date","sample")) # ora ho sulla stessa riga depth e concentrazione di fog e RF
fog.NO3$NO3.N=fog.NO3$vals.x*fog.NO3$vals.y*10000/1000000*(0.06/0.29) # calculates the NO3 daily mass of D2 e D1, turning mg/m2 into kg/ha 
#                                                                    and applying the collector/forest collection efficency
# in case I need q/Q controls here is where to operate
fog.NO3=fog.NO3[,c("date","sample","site.x","variable.x","NO3.N")] 
colnames(fog.NO3)=c("date","sample","plot","var","NO3.N.fog")

# STEP 2: MEAN  vals by DATE:
#meanddNO3fog <- aggregate(NO3.N ~ date, data = fog.NO3, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# STEP 3: aggregate by month 
fog.NO3$mY=strftime(fog.NO3$date,"%Y%m") # creates month-Year column

monthlyNO3.sample.fog = aggregate(NO3.N.fog ~ mY+sample,  data = fog.NO3, FUN = sum) # aggregate by date AND sample

monthlyNO3.fog = aggregate(NO3.N.fog ~ mY,  data = monthlyNO3.sample.fog, FUN = diff) # aggregate by date AND sample


# STEP 4: prepare the df to be bound to TF, SF (and/or plot values)

NO3.fog = "NO3.fog"
both = "both"

mNO3fog=data.frame(cbind(monthlyNO3.fog, both, NO3.fog))
colnames(mNO3fog)=c("mY", "vals", "site", "var")

###                   fog NH4                         ###

# STEP 1: merge depth and concentration, clean the merged file
fog.NH4=merge(fog,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
fog.NH4$NH4.N=fog.NH4$vals.x*fog.NH4$vals.y*10000/1000000*(0.06/0.29) # turns mg/m2 into kg/ha and 
                                                                      # applying the collector/forest collection efficency
# in case I need q/Q controls here is where to operate
fog.NH4=fog.NH4[,c("date","sample","site.x","variable.x","NH4.N")] 
colnames(fog.NH4)=c("date","sample","plot","var","NH4.N.fog")

# STEP 2: MEAN  vals by DATE:
#meanddNH4fog <- aggregate(NH4.N ~ date, data = fog.NH4, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# STEP 3: aggregate by month 
fog.NH4$mY=strftime(fog.NH4$date,"%Y%m") # creates month-Year column

monthlyNH4.sample.fog = aggregate(NH4.N.fog ~ mY+sample,  data = fog.NH4, FUN = sum)

monthlyNH4.fog = aggregate(NH4.N.fog ~ mY,  data = monthlyNH4.sample.fog, FUN = diff) # aggregate by date AND sample

# STEP 4: prepare the df to be bound to TF, SF (and/or plot values)

NH4.fog = "NH4.fog"
both = "both"

mNH4fog=data.frame(cbind(monthlyNH4.fog, both, NH4.fog))
colnames(mNH4fog)=c("mY", "vals", "site", "var")

#housekeeping
rm(fog.mmeq, both, fog, fog.NO3, fog.NH4, NH4.fog, NO3.fog, monthlyNO3.sample.fog, monthlyNH4.sample.fog)

