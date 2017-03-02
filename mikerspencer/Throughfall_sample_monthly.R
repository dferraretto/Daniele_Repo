# --------------------------------------------------------
#  Daniele Ferraretto, working on data in Griffin,SQLite
#  started on 15th October, 2015
#  updated: 15/10/2015          last update: 21/04/2016
# --------------------------------------------------------
# --------------------------------------------------------

tf  = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' AND vals>=0 ORDER BY date")


# --------------------------------------------------------
#         1. THROUGHFALL NO3-N and NH4-N (mg/mq)
# --------------------------------------------------------

###                  NO3.N in TF                    ###

# STEP 1: merge depth and concentration, clean the merged file
TF.NO3=merge(tf,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))
TF.NO3$NO3.N=TF.NO3$vals.x*TF.NO3$vals.y*10000/1000000 # turns to a g/ha value

# in case I need q/Q controls here is where to operate
TF.NO3=TF.NO3[,c("date","sample","site.x","variable.x","NO3.N")] 
colnames(TF.NO3)=c("date","sample","plot","var","value")

# STEP 2: aggregate by month NOT BY SAMPLE

TF.NO3$mY=strftime(TF.NO3$date,"%Y%m") # creates month-Year column

sampleNO3.TF = aggregate(value ~ mY+sample,  data = TF.NO3, FUN = sum)


# STEP 3: prepare the df to be bound to RF (and/or plot values)
sampleNO3.TF$var = "NO3N.mass.TF"





###                  NH4.N in TF                    ###

# STEP 1: merge depth and concentration, clean the merged file
TF.NH4=merge(tf,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
TF.NH4$NH4.N=TF.NH4$vals.x*TF.NH4$vals.y*10000/1000000 # turns to a g/ha value

# in case I need q/Q controls here is where to operate
TF.NH4=TF.NH4[,c("date","sample","site.x","variable.x","NH4.N")] 
colnames(TF.NH4)=c("date","sample","plot","var","value")

# STEP 2: aggregate by month NOT BY SAMPLE

TF.NH4$mY=strftime(TF.NH4$date,"%Y%m") # creates month-Year column

sampleNH4.TF = aggregate(value ~ mY+sample,  data = TF.NH4, FUN = sum)


# STEP 3: prepare the df to be bound to RF (and/or plot values)
sampleNH4.TF$var = "NH4N.mass.TF"


# Housekeeping:
rm(tf, TF.NH4, TF.NO3)

### remember remember: all data are expressed in mg/m2 so far. To express them in g/ha I need to 
# multiply them per 10000/1000 = 10 (I cann add this to the post merge operation)