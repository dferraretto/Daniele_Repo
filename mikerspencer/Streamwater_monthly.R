# --------------------------------------------------------
#  Daniele Ferraretto, working on data in Griffin,SQLite
#  started on 15th February, 2016
#  updated: 15/02/2016         last update: 15/02/2016 
# --------------------------------------------------------
# --------------------------------------------------------

# --------------------------------------------------------
#         0. SW DISCHARGE PER DAY (L/DAY)
# --------------------------------------------------------

# 1. HO RIPORTATO I DATI DI CAMPO DA VALORE ISTANTANEO A GIORNALIERO MOLTIPLICANDOLI PER 24*3600 
# - DONE in Griffin_rts_SQLite (RINOMINATO RFTSW_monthly)
# 2. POI DI SOTTO CALCOLATO IL CONTRIBUTO PER BACINO. in T l'output di N per ettaro sara':
# (daily mass T20 - daily mass T21)/ (area T20 - area T21)
# C21 and C 20 drain two different areas, nonE of them likely containing C10, C11 and C12 (more likely these are in the middle):
# hence I would go with a mean value of (C20 daily mass/ C20 subcatchment area + C21 daily mass/ C21 subcatchment area)
# C20: 26620*25/10000
# C21: (10776+631+589)*25/10000
# T20: 63333*25/10000
# T21: 31893*25/10000

sw = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'v-notch flow' ORDER BY date")

###                   Streamwater DAILY PER ORA                         ###



#####                  1. NO3 IN SW


sw.NO3=merge(sw,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))
sw.NO3$NO3.N.SW=sw.NO3$vals.x*sw.NO3$vals.y/1000 #turns mg into g/area
# in case I need q/Q controls here is where to operate
sw.NO3=sw.NO3[,c("date","sample","site.x","variable.y","NO3.N.SW")] 
colnames(sw.NO3)=c("date","sample","plot","var","NO3.N.SW")

#NH4.sw = "NH4.SW"
#NO3.sw = "NO3.SW"
#Treatment = "Treatment"
#Control = "Control"

#####            STEP 1a: CONTROL AREA:


Csw.NO3 = sw.NO3[which (sw.NO3$plot=='Control'),] # subset control samples

# long to wide

library(reshape2)

Csw.NO3 = dcast(Csw.NO3, date ~ sample, value.var = "NO3.N.SW")

# C20SW1 per surface

Csw.NO3$C20SW1 = Csw.NO3$C20SW1*10000/(26620*25)

# C20SW1 per surface

Csw.NO3$C21SW1 = Csw.NO3$C21SW1*10000/((10776+631+589)*25)

# melt and calculating the mean value per hectare (g/ha/d)

Csw.NO3 = melt(Csw.NO3, id = "date")

Csw.NO3 = aggregate(value ~ date, data = Csw.NO3, mean, na.rm = TRUE) # verifica se na.rm non rimuove date e poi mi da problemi nel successivo melt

colnames(Csw.NO3) = c("date", "CSW.NO3.N")

# Aggregate by month 

Csw.NO3$mY=strftime(Csw.NO3$date,"%Y%m") # creates month-Year column

monthlyCSW.NO3 = aggregate(CSW.NO3.N ~ mY,  data = Csw.NO3, FUN = sum)

# the following 2 lines are only needed in case of building a table

# monthlyCsw.NO3 = data.frame(cbind(monthlyCsw.NO3, Control, NO3.sw))

# colnames(monthlyCsw.NO3)=c("mY", "vals", "site", "var")

#####            STEP 1b: TREATMENT AREA:


Tsw.NO3 = sw.NO3[which (sw.NO3$plot=='Treatment'),] # subset Treatment samples

# long to wide

Tsw.NO3 = dcast(Tsw.NO3, date ~ sample, value.var = "NO3.N.SW")

# C20SW1 per surface

Tsw.NO3$T20SW1 = Tsw.NO3$T20SW1*10000/(63333*25)

# C20SW1 per surface

Tsw.NO3$T21SW1 = Tsw.NO3$T21SW1*10000/(31893*25)

# Calculate the NO3 output per ha per day, as (daily mass T20 - daily mass T21)/ (area T20 - area T21)

Tsw.NO3$TNO3.N = (Tsw.NO3$T20SW1-Tsw.NO3$T21SW1)/(63333*25-31893*25)

# Aggregate by month 

Tsw.NO3$mY=strftime(Tsw.NO3$date,"%Y%m") # creates month-Year column

monthlyTSW.NO3 = aggregate(TNO3.N ~ mY,  data = Tsw.NO3, FUN = sum)

colnames(monthlyTSW.NO3) = c("mY", "TSW.NO3.N")


# see above - only needed to create a table
# monthlyTsw.NO3 = data.frame(cbind(monthlyTsw.NO3, Treatment, NO3.sw))
# colnames(monthlyTsw.NO3)=c("mY", "vals", "site", "var")

#####                  2. NH4 IN SW


sw.NH4=merge(sw,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
sw.NH4$NH4.N=sw.NH4$vals.x*sw.NH4$vals.y/1000 #turns mg into g/area
# in case I need q/Q controls here is where to operate
sw.NH4=sw.NH4[,c("date","sample","site.x","variable.y","NH4.N")] 
colnames(sw.NH4)=c("date","sample","plot","var","NH4.N.SW")

#####            STEP 2a: CONTROL AREA:

Csw.NH4 = sw.NH4[which (sw.NH4$plot=='Control'),] # subset

Csw.NH4 = Csw.NH4[ , c("date", "sample", "NH4.N.SW")]

# long to wide

Csw.NH4 = dcast(Csw.NH4, date ~ sample, value.var = "NH4.N.SW")

# C20SW1 per surface

Csw.NH4$C20SW1 = Csw.NH4$C20SW1*10000/(26620*25)

# C20SW1 per surface

Csw.NH4$C21SW1 = Csw.NH4$C21SW1*10000/((10776+631+589)*25)

# melt and calculating the mean value per hectare (g/ha/d)

Csw.NH4 = melt(Csw.NH4, id = "date")

Csw.NH4 = aggregate(value ~ date, data = Csw.NH4, mean, na.rm = TRUE) # verifica se na.rm non rimuove date e poi mi da problemi nel successivo melt

colnames(Csw.NH4) = c("date", "CSW.NH4.N")

# Aggregate by month 

Csw.NH4$mY=strftime(Csw.NH4$date,"%Y%m") # creates month-Year column

monthlyCSW.NH4 = aggregate(CSW.NH4.N ~ mY,  data = Csw.NH4, FUN = sum)

# monthlyCsw.NH4 = data.frame(cbind(monthlyCsw.NH4, Control, NH4.sw))

# colnames(monthlyCsw.NH4)=c("mY", "vals", "site", "var")

#####            STEP 1b: TREATMENT AREA:


Tsw.NH4 = sw.NH4[which (sw.NH4$plot=='Treatment'),] # subset

Tsw.NH4 = Tsw.NH4[ , c("date", "sample", "NH4.N.SW")]

# long to wide

Tsw.NH4 = dcast(Tsw.NH4, date ~ sample, value.var = "NH4.N.SW")

# T20SW1 per surface

Tsw.NH4$T20SW1 = Tsw.NH4$T20SW1*10000/(63333*25)

# T21SW1 per surface

Tsw.NH4$T21SW1 = Tsw.NH4$T21SW1*10000/(31893*25)

# Calculate the NH4 output per ha per day, as (daily mass T20 - daily mass T21)/ (area T20 - area T21)

Tsw.NH4$TSW.NH4.N = (Tsw.NH4$T20SW1-Tsw.NH4$T21SW1)/(63333*25-31893*25)

# Aggregate by month 

Tsw.NH4$mY=strftime(Tsw.NH4$date,"%Y%m") # creates month-Year column

monthlyTSW.NH4 = aggregate(TSW.NH4.N ~ mY,  data = Tsw.NH4, FUN = sum)

# monthlyTsw.NH4 = data.frame(cbind(monthlyTsw.NH4, Treatment, NH4.sw))

# colnames(monthlyTsw.NH4)=c("mY", "vals", "site", "var")

# per domani? Completare con la colonna variabile tipo TF e preparare il file SW per la tabulizzazzione
# Housekeeping:
rm(sw, sw.NO3, sw.NH4, Csw.NH4, Csw.NO3, Tsw.NH4, Tsw.NO3)
