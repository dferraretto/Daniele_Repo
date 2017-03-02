# --------------------------------------------------------
#  Daniele Ferraretto, working on data in Griffin,SQLite
#  started on 15th October, 2015
#  updated: 15/10/2015          last update: 21/04/2016
# --------------------------------------------------------
# --------------------------------------------------------

tf  = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' AND vals>=0 ORDER BY date")

# --------------------------------------------------------
#         0. THROUGHFALL DEPTH (MONTHLY)
# --------------------------------------------------------

# 1: mean daily value
tf.vol.dd=aggregate(vals ~ date, data = tf, FUN = mean, na.rm = TRUE )

tf.vol.dd$mY=strftime(tf.vol.dd$date,"%Y%m") # creates month-Year column
# 2: aggregate per month
tf.m.vol = aggregate (vals ~ mY, data = tf.vol.dd, FUN = sum) 

# --------------------------------------------------------
#         1. THROUGHFALL NO3-N and NH4-N (mg/mq)
# --------------------------------------------------------

###                  NO3.N in TF                    ###

# STEP 1: merge depth and concentration, clean the merged file
TF.NO3=merge(tf,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))
TF.NO3$NO3.N=TF.NO3$vals.x*TF.NO3$vals.y*10000/1000000 # turns to a g/ha value

# in case I need q/Q controls here is where to operate
TF.NO3=TF.NO3[,c("date","sample","site.x","variable.x","NO3.N")] 
colnames(TF.NO3)=c("date","sample","plot","var","NO3.N")

# STEP 2: MEAN  vals by DATE:
meanddNO3TF <- aggregate(NO3.N ~ date, data = TF.NO3, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# STEP 3: aggregate by month 
meanddNO3TF$mY=strftime(meanddNO3TF$date,"%Y%m") # creates month-Year column

monthlyNO3.TF = aggregate(NO3.N ~ mY,  data = meanddNO3TF, FUN = sum)

colnames(monthlyNO3.TF) = c("mY", "NO3.N.TF") # to be used to creat a long (melt) format NX file

# STEP 4: prepare the df to be bound to RF (and/or plot values)
NO3.tf = "TF.NO3"
both = "both"

mNO3TF=data.frame(cbind(monthlyNO3.TF, both, NO3.tf))
colnames(mNO3TF)=c("mY", "vals", "site", "var")



###                  NH4.N in TF                    ###

# STEP 1: merge depth and concentration, clean the merged file
TF.NH4=merge(tf,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
TF.NH4$NH4.N=TF.NH4$vals.x*TF.NH4$vals.y*10000/1000000 # turns from mg/m2 into kg/ha
# in case I need q/Q controls here is where to operate
TF.NH4=TF.NH4[,c("date","sample","site.x","variable.x","NH4.N")] 
colnames(TF.NH4)=c("date","sample","plot","var","NH4.N")

# STEP 2: MEAN  vals by DATE:
meanddNH4TF <- aggregate(NH4.N ~ date, data = TF.NH4, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# STEP 3: aggregate by month 
meanddNH4TF$mY=strftime(meanddNH4TF$date,"%Y%m") # creates month-Year column

# STEP 4: prepare the df to be bound to RF, SF (and/or plot values)
monthlyNH4.TF = aggregate(NH4.N ~ mY,  data = meanddNH4TF, FUN = sum)

colnames(monthlyNH4.TF) = c("mY", "NH4.N.TF") # to be used to creat a long (melt) format NX file


NH4.tf = "TF.NH4"
both = "both"

mNH4TF=data.frame(cbind(monthlyNH4.TF, both, NH4.tf))
colnames(mNH4TF)=c("mY", "vals", "site", "var")

# Housekeeping:
rm(tf.vol.dd, meanddNH4TF, meanddNO3TF, tf, TF.NH4, TF.NO3, both, NH4.tf, NO3.tf)

### remember remember: all data are expressed in mg/m2 so far. To express them in g/ha I need to 
# multiply them per 10000/1000 = 10 (I cann add this to the post merge operation)