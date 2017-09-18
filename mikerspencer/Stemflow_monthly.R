# --------------------------------------------------------
#         2. STEMFLOW NO3-N and NH4-N (mg/mq?)
# --------------------------------------------------------
# Differently from TF this calculates a mean per each 
# dbh class, so that the resulting matrix (values per average tree) is multiplied per 
# average number of trees per ha. This number proceeds as a calculation from 2770trees/ha 
# reduced of 5th row and 3rd tree) whereas 1439 trees/ha per C would be the real numbers, 
# including a further reduction due to attrition. This second number could be used to differentiate 
# T and C plots, but at the moment, considering the values obtained, it doesnt seem necessary.
# NUMBER OF TREES PER HECTAR = 1883 trees/ha
# last update: 21/04/2016 (to create wide to long NX file)

library (plyr)

# select SF data from SQL db
sf = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'stem vol' AND vals>=0 ORDER BY date")


# importing dbh classes and merge with sf
stemdbh <- read.csv("mikerspencer/stemdbh.csv", header=TRUE)
dbh.class=stemdbh[,c(1,5)]
SF.class=merge(sf,dbh.class, by = "sample")
SF.class=arrange(SF.class,date,dbh.class)

###                  NO3.N in SF                    ###

# STEP 1: merge vol and concentration, clean the merged file
SF.class.NO3=merge(SF.class,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))
SF.class.NO3$NO3.N=SF.class.NO3$vals.x*SF.class.NO3$vals.y

# in case I need q/Q controls here is where to operate
SF.class.NO3=SF.class.NO3[,c("date","sample","site.x","variable.x","dbh.class", "NO3.N")] 
colnames(SF.class.NO3)=c("date","sample","plot","var","dbh.class","NO3.N.SF")

# STEP 2: MEAN  vals by dbh.class+DATE:
meanddNO3SF <- aggregate(NO3.N.SF ~ dbh.class+date, data = SF.class.NO3, FUN = mean,  na.rm = TRUE) 

# STEP2b: MEAN vals by date:
meanddNO3SF <- aggregate(NO3.N.SF ~ date, data = meanddNO3SF, FUN = mean,  na.rm = TRUE) 

# STEP 3: aggregate by month, obtaining a "single model tree" value in mg

meanddNO3SF$mY=strftime(meanddNO3SF$date,"%Y%m") # creates month-Year column

monthlyNO3.SF = aggregate(NO3.N.SF ~ mY,  data = meanddNO3SF, FUN = sum)

# converting into kg/ha:
monthlyNO3.SF$NO3.N.SF = monthlyNO3.SF$NO3.N.SF*1883/1000000

# STEP 4: prepare the df to be bound to RF (and/or plot values)

NO3.sf = "SF.NO3"
both = "both"

mNO3SF=data.frame(cbind(monthlyNO3.SF, both, NO3.sf))
colnames(mNO3SF)=c("mY", "vals", "site", "var")



###                  NH4.N in SF                    ###

# STEP 1: merge depth and concentration, clean the merged file
SF.class.NH4=merge(SF.class,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
SF.class.NH4$NH4.N=SF.class.NH4$vals.x*SF.class.NH4$vals.y

# in case I need q/Q controls here is where to operate
SF.class.NH4=SF.class.NH4[,c("date","sample","site.x","variable.x","dbh.class", "NH4.N")] 
colnames(SF.class.NH4)=c("date","sample","plot","var","dbh.class","NH4.N.SF")

# STEP 2: MEAN  vals by dbh.class+DATE:
meanddNH4SF <- aggregate(NH4.N.SF ~ dbh.class+date, data = SF.class.NH4, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# STEP2b: MEAN vals by date:
meanddNH4SF <- aggregate(NH4.N.SF ~ date, data = meanddNH4SF, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# STEP 3: aggregate by month, obtaining a "single model tree" value in mg

meanddNH4SF$mY=strftime(meanddNH4SF$date,"%Y%m") # creates month-Year column

monthlyNH4.SF = aggregate(NH4.N.SF ~ mY,  data = meanddNH4SF, FUN = sum)

# converting into g/ha:
monthlyNH4.SF$NH4.N.SF = monthlyNH4.SF$NH4.N.SF*1883/1000000

# STEP 4: prepare the df to be bound to RF (and/or plot values)

NH4.sf = "SF.NH4"
both = "both"

mNH4SF=data.frame(cbind(monthlyNH4.SF, both, NH4.sf))
colnames(mNH4SF)=c("mY", "vals", "site", "var")


#housekeeping
rm(sf, SF.class, SF.class.NH4, SF.class.NO3, stemdbh, dbh.class, meanddNH4SF, meanddNO3SF, both, NH4.sf, NO3.sf)

