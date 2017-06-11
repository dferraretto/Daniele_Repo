# --------------------------------------------------------
#  Daniele Ferraretto, working on data in Griffin,SQLite
#  started on 15th October, 2015
#  updated: 15/10/2015          last update: 21/04/2016
# --------------------------------------------------------
# --------------------------------------------------------
rm(list=ls())

.libPaths("C:/Workspace/R") # adjust the issues with my desktop demmerda

library(RSQLite)

db = dbConnect(SQLite(), dbname="field_lab/Daily_Griffin.SQLite")

# Select labdata from SQLite db (next step is two queries to avoid subset)
NO3data = dbGetQuery(db, "SELECT * FROM labdata WHERE VALS >= 0 AND variable = 'NO3.N' ORDER BY date")
NH4data = dbGetQuery(db, "SELECT * FROM labdata WHERE VALS >= 0 AND variable = 'NH4.N' ORDER BY date")

tf  = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' AND vals>=0 ORDER BY date AND sample")

treatment = c("T10T1", "T10T2", "T10T3", "T11T1", "T11T2", "T11T3", "T12T1", "T12T2", "T12T3")
control = c("C10T1", "C10T2", "C10T3", "C11T1", "C11T2", "C11T3", "C12T1", "C12T2", "C12T3")
tft  = subset(tf, sample == treatment)
tfc  = subset(tf, sample == control)

# --------------------------------------------------------
#         1. THROUGHFALL NO3-N and NH4-N (mg/mq)
# --------------------------------------------------------

###                1a  NO3.N in TFT                    ###

# STEP 1: merge depth and concentration, clean the merged file
TFT.NO3=merge(tft,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))
TFT.NO3$NO3.N=TFT.NO3$vals.x*TFT.NO3$vals.y*10000/1000000 # turns to a Kg/ha value

# in case I need q/Q controls here is where to operate
TFT.NO3=TFT.NO3[,c("date","sample","site.x","variable.x","NO3.N")] 
colnames(TFT.NO3)=c("date","sample","plot","var","NO3.N")

# STEP 2: MEAN  vals by DATE:
meanddNO3TFT <- aggregate(NO3.N ~ date, data = TFT.NO3, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# STEP 3: aggregate by month 
meanddNO3TFT$mY=strftime(meanddNO3TFT$date,"%Y%m") # creates month-Year column

monthlyNO3.TFT = aggregate(NO3.N ~ mY,  data = meanddNO3TFT, FUN = sum)

colnames(monthlyNO3.TFT) = c("mY", "NO3.N.TFT") # to be used to creat a long (melt) format NX file

# STEP 4: prepare the df to be bound to RF (and/or plot values)
NO3.tft = "TFT.NO3"
site = "treatment"

mNO3TFT=data.frame(cbind(monthlyNO3.TFT, site, NO3.tft))
colnames(mNO3TFT)=c("mY", "vals", "site", "var")

###                1b  NO3.N in TFC                    ###

# STEP 1: merge depth and concentration, clean the merged file
TFC.NO3=merge(tfc,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))
TFC.NO3$NO3.N=TFC.NO3$vals.x*TFC.NO3$vals.y*10000/1000000 # turns to a Kg/ha value

# in case I need q/Q controls here is where to operate
TFC.NO3=TFC.NO3[,c("date","sample","site.x","variable.x","NO3.N")] 
colnames(TFC.NO3)=c("date","sample","plot","var","NO3.N")

# STEP 2: MEAN  vals by DATE:
meanddNO3TFC <- aggregate(NO3.N ~ date, data = TFC.NO3, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# STEP 3: aggregate by month 
meanddNO3TFC$mY=strftime(meanddNO3TFC$date,"%Y%m") # creates month-Year column

monthlyNO3.TFC = aggregate(NO3.N ~ mY,  data = meanddNO3TFC, FUN = sum)

colnames(monthlyNO3.TFC) = c("mY", "NO3.N.TFC") # to be used to creat a long (melt) format NX file

# STEP 4: prepare the df to be bound to RF (and/or plot values)
NO3.tfc = "TFC.NO3"
site = "control"

mNO3TFC=data.frame(cbind(monthlyNO3.TFC, site, NO3.tfc))
colnames(mNO3TFT)=c("mY", "vals", "site", "var")


###                2a  NH4.N in TFT                    ###

# STEP 1: merge depth and concentration, clean the merged file
TFT.NH4=merge(tft,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
TFT.NH4$NH4.N=TFT.NH4$vals.x*TFT.NH4$vals.y*10000/1000000 # turns to a Kg/ha value

# in case I need q/Q controls here is where to operate
TFT.NH4=TFT.NH4[,c("date","sample","site.x","variable.x","NH4.N")] 
colnames(TFT.NH4)=c("date","sample","plot","var","NH4.N")

# STEP 2: MEAN  vals by DATE:
meanddNH4TFT <- aggregate(NH4.N ~ date, data = TFT.NH4, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# STEP 3: aggregate by month 
meanddNH4TFT$mY=strftime(meanddNH4TFT$date,"%Y%m") # creates month-Year column

monthlyNH4.TFT = aggregate(NH4.N ~ mY,  data = meanddNH4TFT, FUN = sum)

colnames(monthlyNH4.TFT) = c("mY", "NH4.N.TFT") # to be used to creat a long (melt) format NX file

# STEP 4: prepare the df to be bound to RF (and/or plot values)
NH4.tft = "TFT.NH4"
site = "treatment"

mNH4TFT=data.frame(cbind(monthlyNH4.TFT, site, NH4.tft))
colnames(mNH4TFT)=c("mY", "vals", "site", "var")

###                2b  NH4.N in TFC                    ###

# STEP 1: merge depth and concentration, clean the merged file
TFC.NH4=merge(tfc,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
TFC.NH4$NH4.N=TFC.NH4$vals.x*TFC.NH4$vals.y*10000/1000000 # turns to a Kg/ha value

# in case I need q/Q controls here is where to operate
TFC.NH4=TFC.NH4[,c("date","sample","site.x","variable.x","NH4.N")] 
colnames(TFC.NH4)=c("date","sample","plot","var","NH4.N")

# STEP 2: MEAN  vals by DATE:
meanddNH4TFC <- aggregate(NH4.N ~ date, data = TFC.NH4, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# STEP 3: aggregate by month 
meanddNH4TFC$mY=strftime(meanddNH4TFC$date,"%Y%m") # creates month-Year column

monthlyNH4.TFC = aggregate(NH4.N ~ mY,  data = meanddNH4TFC, FUN = sum)

colnames(monthlyNH4.TFC) = c("mY", "NH4.N.TFC") # to be used to creat a long (melt) format NX file

# STEP 4: prepare the df to be bound to RF (and/or plot values)
NH4.tfc = "TFC.NH4"
site = "control"

mNH4TFC=data.frame(cbind(monthlyNH4.TFC, site, NH4.tfc))
colnames(mNH4TFT)=c("mY", "vals", "site", "var")
# Housekeeping:
rm(tf.vol.dd, meanddNH4TFT, meanddNO3TFT, meanddNH4TFC, meanddNO3TFC, tf, tfc, tft, NH4data, NO3data,
   TFC.NH4, TFC.NO3, TFT.NH4, TFT.NO3, site, NH4.tfc, NO3.tfc, NH4.tft, NO3.tft, control, treatment)

### remember remember: all data are expressed in mg/m2 so far. To express them in kg/ha I need to 
# multiply them per 10000/1000000 (I can add this to the post merge operation)

# TFC and TFT TABLE AND PLOTS
TF1.by.plot = merge(monthlyNH4.TFT, monthlyNO3.TFT, by = "mY")# "labelled" vol but it's depth!!!
TF2.by.plot = merge(monthlyNH4.TFC, monthlyNO3.TFC, by = "mY")# "labelled" vol but it's depth!!!
TF.by.plot = merge(TF1.by.plot, TF2.by.plot, by = "mY")
TF.by.plot = transform(TF.by.plot, mY = as.yearmon(as.character(mY), "%Y%m"))
names(TF.by.plot) = c("date", "Plot T TF NH4-N", "Plot T TF NO3-N", "Plot C TF NH4-N", "Plot C TF NO3-N")
