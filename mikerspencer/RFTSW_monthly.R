# --------------------------------------------------------
#  Daniele Ferraretto, working on data in Griffin,SQLite
#  started on 20th July, 2015
#  updated: 09/10/2015          last update: 15/10/2015
# --------------------------------------------------------
# --------------------------------------------------------
# clear the memory
rm(list=ls())

.libPaths("C:/Workspace/R") # adjust the issues with my desktop demmerda
### set working dir for pc:
#setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
setwd("M:/My PhD/R/PhD-local_repo")

# --------------------------------------------------------
# Connect to SQL db
# --------------------------------------------------------
library(RSQLite)
library(ggplot2)
library(plotly)
library(zoo)

db = dbConnect(SQLite(), dbname="field_lab/Daily_Griffin.SQLite")

# Select labdata from SQLite db (next step is two queries to avoid subset)
NO3data = dbGetQuery(db, "SELECT * FROM labdata WHERE VALS >= 0 AND variable = 'NO3.N' ORDER BY date")
NH4data = dbGetQuery(db, "SELECT * FROM labdata WHERE VALS >= 0 AND variable = 'NH4.N' ORDER BY date")

source("mikerspencer/Throughfall_monthly.R")
source("mikerspencer/Stemflow_monthly.R")
source("mikerspencer/Fog_monthly.R")
source("mikerspencer/Streamwater_monthly.R")
source("mikerspencer/Rainfall_monthly.R")


############################################################################
#######          CREATING TABLE AND WIDE/LONG MONTHLY df            ########
############################################################################

# TF and RF DEPTH TABLE AND PLOTS
TF.RF.depth = merge(RF.m.vol, tf.m.vol, by = "mY")# "labelled" vol but it's depth!!!
TF.RF.fog.depth = merge(TF.RF.depth, fog.m.vol, by = "mY")
names(TF.RF.fog.depth) = c("date", "RF", "TF", "fog")

# PLOTTING TF, RF and fog depths

long.TF.RF.fog.depth = melt(TF.RF.fog.depth, id.vars = "date") 


long.TF.RF.fog.depth = transform(long.TF.RF.fog.depth, date = as.yearmon(as.character(date), "%Y%m"))
long.TF.RF.fog.depth$month = format(long.TF.RF.fog.depth$date, "%m")
long.TF.RF.fog.depth$year = format(long.TF.RF.fog.depth$date, "%Y")


#################################################
#       CREATE A NX TABLE FOR ALL FLUXES:
#################################################

table.m.NX=cbind(monthlyNH4.RF, monthlyNO3.RF, monthlyNH4.fog, monthlyNO3.fog, monthlyNH4.TF, monthlyNO3.TF, monthlyNH4.SF, monthlyNO3.SF) # monthlyCsw.NH4, monthlyCsw.NO3, monthlyTsw.NH4, monthlyTsw.NO3, monthlyCSW.NH4, monthlyCSW.NO3
# monthlyTSW.NH4, monthlyTSW.NO3 temporarily removed due to missing value in October 2016 leading to different number of rows
table.m.NX <- table.m.NX[, !duplicated(colnames(table.m.NX))]
table.m.NX = transform(table.m.NX, mY = as.yearmon(as.character(mY), "%Y%m"))

# Additional columns on demand:
table.m.NX$NH4.N.input = table.m.NX$NH4.N.RF + table.m.NX$NH4.N.fog
table.m.NX$NO3.N.input = table.m.NX$NO3.N.RF + table.m.NX$NO3.N.fog
table.m.NX$NH4.output = table.m.NX$NH4.N.TF + table.m.NX$NH4.N.SF
table.m.NX$NO3.output = table.m.NX$NO3.N.TF + table.m.NX$NO3.N.SF

# DO NOT RUN the following 2 lines if you need the long.N.RFTSW df, go straight to WIDE TO LONG below

# table.m.NX$month = format(table.m.NX$mY, "%m")
# table.m.NX$year = format(table.m.NX$mY, "%Y")

#########         WIDE TO LONG NX TABLE:

long.N.RFTSW = melt(table.m.NX, id.vars = "mY") 

# turn character into month+year, month (numeric) and year(numeric):

long.N.RFTSW$month = format(long.N.RFTSW$mY, "%m")
long.N.RFTSW$year = format(long.N.RFTSW$mY, "%Y")

# long.N.RFTSW = long.N.RFTSW[long.N.RFTSW$variable %like% ".N", ] # to get rid of non numeric arguments coming from the melt command, had to switch it off as it doesnt recognise the command %like%
# long.N.RFTSW$value= as.numeric(long.N.RFTSW$value) # because, sometimes, getting rid of non numeric lookalaike arguments is not enough...


# **************************************************************************************************
# all time sum (by column, needed for the "N fluxes in Griffin Forest, years 2011-2016")
alltime.NX = cbind(monthlyNH4.RF, monthlyNO3.RF, monthlyNH4.fog, monthlyNO3.fog, monthlyNH4.TF, monthlyNO3.TF, monthlyNH4.SF, monthlyNO3.SF)
alltime.NX = alltime.NX[ , -which(names(alltime.NX) %in% "mY")]
sum = colSums(alltime.NX)
# per KAte avevo fatto una somma "Manuale", ma ora che ho creato il vettore sum posso sommare le voci qui sotto, se servira'

# housekeeping:
rm(monthlyNH4.RF, monthlyNH4.fog, monthlyNH4.SF, monthlyNH4.TF, monthlyNO3.fog, monthlyNO3.RF, monthlyNO3.TF,
   monthlyNO3.SF, mNH4fog, mNH4RF, mNH4SF, mNH4TF, mNO3fog, mNO3RF, mNO3SF, mNO3TF, monthlyCSW.NH4, monthlyCSW.NO3, monthlyTSW.NH4, monthlyTSW.NO3)


#
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PLOTTING the annual fluxes (DC) for Funds request

x = ggplot(data = mNX.rfsftf, aes (mY, vals, fill = var))

plottone = x + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("royal blue", "Sky Blue", "Dark Green", "Yellow Green", "Saddle Brown", "Burlywood"), name = "N flux \n and form", labels = c(expression(RF~NH[4]*-N), expression(RF~NO[3]*-N), expression(TF~NH[4]*-N), expression(TF~NO[3]*-N), expression(SF~NH[4]*-N), expression(SF~NO[3]*-N))) +
  labs( x = "YEAR", y = expression(N~flux~~"(kg N"~~ha^"-1"~y^"-1"*")")) 

ggsave("M:/My PhD/R/PhD-local_repo/output_tables_plots/N_fluxes_Griffin.png", width = 6, height = 4, dpi = 100, plottone)

ggsave(filename, plot = last_plot(), device = NULL, path = NULL, scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE, ...)
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # adapting the annual fluxes (DC) for Funds request to the new long data
  
  x = ggplot(data = long.N.RFTSW, aes (month, value, fill = variable))

plottone = x + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("royal blue", "Sky Blue", "Dark Green", "Yellow Green", "Saddle Brown", "Burlywood", "grey20", "grey30", "grey40", "grey50", "grey60", "grey70"), name = "N flux \n and form", labels = c(expression(RF~NH[4]*-N), expression(RF~NO[3]*-N), expression(TF~NH[4]*-N), expression(TF~NO[3]*-N), expression(SF~NH[4]*-N), expression(SF~NO[3]*-N), expression(RF~NO[3]*-N), expression(RF~NO[3]*-N), expression(RF~NO[3]*-N), expression(RF~NO[3]*-N))) +
  facet_grid(year ~ .)
  labs( x = "YEAR", y = expression(N~flux~~"(kg N"~~ha^"-1"~y^"-1"*")")) 

 # + facet_grid(mY ~ ., scales = "free") #, breaks = "var", labels = c("RF NH4-N", "RF NO3-N","TF NH4-N", "TF NO3-N","SF NH4-N", "SF NO3-N")))

# aiutino per labels con subscripts e special fonts: * e' uno spazio non spazio (x es. per staccare un pedice dal testo successivo), ~ e' uno spazio fisico
# plot(1,1, xlab=expression(N~flux~~"(kg N"~ha^"-1"~y^"-1"*")"))


