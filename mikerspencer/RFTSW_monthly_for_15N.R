# --------------------------------------------------------
#  Daniele Ferraretto, working on data in Griffin,SQLite
#  started on 20th July, 2015
#  updated: 09/10/2015          last update: 15/10/2015
# --------------------------------------------------------
# --------------------------------------------------------
# clear the memory
# rm(list=ls())

#.libPaths("C:/Workspace/R") # adjust the issues with my desktop demmerda
### set working dir for pc:
#setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
#setwd("M:/My PhD/R/PhD-local_repo")
#setwd("C:/Users/s1373890/Daniele_Repo")

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


source("mikerspencer/Throughfall_monthly_by_PLOT.R") # this line will be used to produce any plots to be compared with the 15N results
source("mikerspencer/Stemflow_monthly.R")
source("mikerspencer/Fog_monthly.R")
source("mikerspencer/Streamwater_monthly.R")
source("mikerspencer/Rainfall_monthly.R")


############################################################################
#######          CREATING TABLE AND WIDE/LONG MONTHLY df            ########
############################################################################


#################################################
#       CREATE A NX TABLE FOR ALL FLUXES:
#################################################

# add the missing row to TFTs:
uselessrow = c("201110", NA)
names(uselessrow) = c("mY", "NH4.N.TFT")
monthlyNH4.TFT = rbind (uselessrow, monthlyNH4.TFT)
names(uselessrow) = c("mY", "NO3.N.TFT")
monthlyNO3.TFT = rbind (uselessrow, monthlyNO3.TFT)
# THE FOLLOWING IS THE TABLE.m.nx CONTAINING ONLY THE TREATMENT DATA, USED TO WORK ON THE 15N DATA
table.m.NX=cbind(monthlyNH4.RF, monthlyNO3.RF, monthlyNH4.fog, monthlyNO3.fog, monthlyNH4.TFT, 
  monthlyNO3.TFT, monthlyNH4.SF, monthlyNO3.SF) # monthlyCsw.NH4, monthlyCsw.NO3, monthlyTsw.NH4, monthlyTsw.NO3, monthlyCSW.NH4, monthlyCSW.NO3
###################################################


# monthlyTSW.NH4, monthlyTSW.NO3 temporarily removed due to missing value in October 2016 leading to different number of rows
table.m.NX <- table.m.NX[, !duplicated(colnames(table.m.NX))]
table.m.NX = transform(table.m.NX, mY = as.yearmon(as.character(mY), "%Y%m"))
table.m.NX$NH4.N.TFT = as.numeric(table.m.NX$NH4.N.TFT)
table.m.NX$NO3.N.TFT = as.numeric(table.m.NX$NO3.N.TFT)

# Additional columns on demand:
table.m.NX$NH4.N.input = table.m.NX$NH4.N.RF + table.m.NX$NH4.N.fog
table.m.NX$NO3.N.input = table.m.NX$NO3.N.RF + table.m.NX$NO3.N.fog
table.m.NX$NH4.output = table.m.NX$NH4.N.TFT + table.m.NX$NH4.N.SF
table.m.NX$NO3.output = table.m.NX$NO3.N.TFT + table.m.NX$NO3.N.SF


# table.m.NX$month = format(table.m.NX$mY, "%m")
# table.m.NX$year = format(table.m.NX$mY, "%Y")

#########         WIDE TO LONG NX TABLE:

long.N.RFTSW = melt(table.m.NX, id.vars = "mY") 

# turn character into month+year, month (numeric) and year(numeric):

long.N.RFTSW$month = format(long.N.RFTSW$mY, "%m")
long.N.RFTSW$year = format(long.N.RFTSW$mY, "%Y")

# long.N.RFTSW = long.N.RFTSW[long.N.RFTSW$variable %like% ".N", ] # to get rid of non numeric arguments coming from the melt command, had to switch it off as it doesnt recognise the command %like%
# long.N.RFTSW$value= as.numeric(long.N.RFTSW$value) # because, sometimes, getting rid of non numeric lookalaike arguments is not enough...


# housekeeping:
rm(monthlyNH4.RF, monthlyNH4.fog, monthlyNH4.SF, monthlyNH4.TFT, monthlyNO3.fog, monthlyNO3.RF, monthlyNO3.TFT, 
   monthlyNH4.TFC, monthlyNO3.TFC, monthlyNO3.SF, mNH4fog, mNH4RF, mNH4SF, mNH4TFT, mNH4TFC, mNO3fog, mNO3RF,
   mNO3SF, mNO3TFT, mNO3TFC, monthlyCSW.NH4, monthlyCSW.NO3, monthlyTSW.NH4, monthlyTSW.NO3, uselessrow, TF.by.plot,
   TF1.by.plot, TF2.by.plot)





