## Created: 11/05/2016
# --------------------------------------------------------
#         3. FOG NO3-N and NH4-N (kg/ha//month/sample)
# --------------------------------------------------------
# note: this script only consiers the rainfall gauges and not the fog gauge.
# I can build up a script to check rain vs fog values and take decisions on how to consider
# the potentially extra N collected as fog (partly being dry dep?). This assumption is,
# however, reductive in terme of N input and hence more cautious.

fog = dbGetQuery(db, "SELECT * FROM fielddata WHERE sample = 'C30D1' or sample = 'C30D2'   ORDER BY date")

###                   FOG NO3                         ###
# STEP 0: calculate the monthly fog depth
# mi viene il dubbio che valga la pena comparare graficamente le depth: fog, rf e TF. Pensaci quando ti sei svegliato...
#fog.mmeq=aggregate(vals ~ date, data = fog, FUN = diff, na.rm = TRUE )
#fog.mmeq$mY=strftime(fog.mmeq$date,"%Y%m") # creates month-Year column
#fog.m.vol = aggregate (vals ~ mY, data = fog.mmeq, FUN = sum) 

# STEP 1: merge depth and concentration, clean the merged file
fog.NO3=merge(fog,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))
fog.NO3$value=fog.NO3$vals.x*fog.NO3$vals.y*10000/1000000 #turns mg/m2 into kg/ha
# in case I need q/Q controls here is where to operate
fog.NO3=fog.NO3[,c("date","sample","site.x","variable.x","value")] 
colnames(fog.NO3)=c("date","sample","plot","var","value")
fog.NO3$var = "NO3N.mass.fog"

# STEP 2: aggregate by month 
fog.NO3$mY=strftime(fog.NO3$date,"%Y%m") # creates month-Year column

sampleNO3.fog = aggregate(value ~ mY+sample,  data = fog.NO3, FUN = sum)

sampleNO3.fog$var= "NO3N.mass.fog"



###                   fog NH4                         ###

# STEP 0: calculate the monthly fog depth
# mi viene il dubbio che valga la pena comparare graficamente le depth: fog, rf e TF. Pensaci quando ti sei svegliato...
#fog.mmeq=aggregate(vals ~ date, data = fog, FUN = diff, na.rm = TRUE )
#fog.mmeq$mY=strftime(fog.mmeq$date,"%Y%m") # creates month-Year column
#fog.m.vol = aggregate (vals ~ mY, data = fog.mmeq, FUN = sum) 

# STEP 1: merge depth and concentration, clean the merged file
fog.NH4=merge(fog,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
fog.NH4$value=fog.NH4$vals.x*fog.NH4$vals.y*10000/1000000 #turns mg/m2 into kg/ha
# in case I need q/Q controls here is where to operate
fog.NH4=fog.NH4[,c("date","sample","site.x","variable.x","value")] 
colnames(fog.NH4)=c("date","sample","plot","var","value")
fog.NH4$var = "NH4N.mass.fog"

# STEP 2: aggregate by month 
fog.NH4$mY=strftime(fog.NH4$date,"%Y%m") # creates month-Year column

sampleNH4.fog = aggregate(value ~ mY+sample,  data = fog.NH4, FUN = sum)

sampleNH4.fog$var= "NH4N.mass.fog"


#housekeeping

rm(fog, fog.NO3, fog.NH4, NH4data, NO3data)
