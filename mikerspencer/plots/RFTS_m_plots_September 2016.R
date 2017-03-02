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

db = dbConnect(SQLite(), dbname="field_lab/Daily_Griffin.SQLite")

# Select labdata from SQLite db (next step is two queries to avoid subset)
NO3data = dbGetQuery(db, "SELECT * FROM labdata WHERE VALS >= 0 AND variable = 'NO3.N' ORDER BY date")
NH4data = dbGetQuery(db, "SELECT * FROM labdata WHERE VALS >= 0 AND variable = 'NH4.N' ORDER BY date")

source("mikerspencer/Throughfall_monthly.R")
source("mikerspencer/Stemflow_monthly.R")
source("mikerspencer/Fog_monthly.R")
source("mikerspencer/Streamwater_monthly.R")
source("mikerspencer/Rainfall_monthly.R")


######################################################################
#########           TF, SF, RF, FOG & SW PLOTS            ############
######################################################################

# TABLES: COMPARING RF AND TF DEPTHS PER MONTH
TF.RF.depth = merge(RF.m.vol, tf.m.vol, by = "mY")# "labelled" vol but it's depth!!!
names(TF.RF.depth) = c("date", "RF", "TF")

#adding fog
TF.RF.fog.depth = merge(TF.RF.depth, fog.m.vol, by.x = "date", by.y="mY")
names(TF.RF.fog.depth) = c("date", "RF", "TF", "fog")

# prepare to export as xls file:
TF.RF.fog.depth$date = transform(TF.RF.fog.depth, date = as.yearmon(as.character(date), "%Y%m"))
TF.RF.fog.depth = TF.RF.fog.depth[ , -c(2,3,4)]


library(WriteXLS)
# export depths as .xls file
WriteXLS(TF.RF.fog.depth, "output_tables_plots/RF_TF_FOG_depths.xlsx") 

# PLOT (less useful than others) for a comparison of RF, TF and fog depths:


TF.RF.fog.depth = TF.RF.fog.depth[, c("date", "RF", "TF", "fog")]

long.TF.RF.fog.depth = melt(TF.RF.fog.depth, id.vars = "date") 

long.TF.RF.fog.depth$m = format(long.TF.RF.fog.depth$date, "%m")
long.TF.RF.fog.depth$year = format(long.TF.RF.fog.depth$date, "%Y")



ggplot(data = long.TF.RF.fog.depth,
       mapping = aes(x = m, y = value, colour = variable, group = variable)) +
  geom_line() + 
  facet_grid(facets = year ~ .) +  labs( x = "month", y = expression(Flux~ depth~"(mm"~~month^"-1"*")")) +
  ggtitle ("Rainfall, fog and throughfall \n depth in Griffin") 

# NB: GROUP!!! in aes, it allows to create different lines from different variable levels at once!!!


# 1. PLOTTING TF, RF AND highlight RF overflows (OF)
library(plotly)
library(zoo)


dbDisconnect(db) # in order to extract RF from Griffin.SQLite and add the overflowing information

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

rf = dbGetQuery(db, "SELECT * FROM fielddata WHERE sample = 'C30D1' or sample = 'C31D1'   ORDER BY date")

rf$overflowing = replace(rf$overflowing, c(7,8,18,40,56,63,70,79,100,101), 1) # this is to "correct the rf information on OF. Needs to be added to the Griffin.SQlite straight

rf.OF= rf[, c("date","overflowing")]

rf.OF$date = as.Date(rf.OF$date) # this is a date-Of db to be merged with rf and TF

# rf.OF=rf.OF[-c(26,27),] # linea temp fino al prossimo giro di db!

rf.OF$overflowing = as.numeric(rf.OF$overflowing)
rf.OF$overflowing = rf.OF$overflowing*80 # set a higher value to appear in the center of the graphic
rf.OF[rf.OF == 0] <- NA # turn 0 into NA

rf.OF = rf.OF[complete.cases(rf.OF),] # removing NA

rf.OF$date = format(rf.OF$date, "%Y%m")
rf.OF = transform(rf.OF, date = as.yearmon(as.character(date), "%Y%m"))
rf.OF$m = format(rf.OF$date, "%m")
rf.OF$year = format(rf.OF$date, "%Y")

long.TF.RF.depth =  melt(TF.RF.depth, id.vars = "date") 
long.TF.RF.depth = transform(long.TF.RF.depth, date = as.yearmon(as.character(date), "%Y%m"))
long.TF.RF.depth$m = format(long.TF.RF.depth$date, "%m")
long.TF.RF.depth$year = format(long.TF.RF.depth$date, "%Y")


#1. PLOT: RF, TF and partial values


ggplot() + geom_line(data=long.TF.RF.depth, aes(x=m, y= value,  ymin=1, ymax=100, colour = variable, group = variable)) +
  geom_text(data=rf.OF, aes(x=m, y=overflowing, stat="identity"), size=3, label = "OF") + facet_grid(facets = year ~ .) +
  labs( x = "month", y = expression(flux~ depth~"(mm"~~month^"-1"*")")) +ggtitle("Comparison between RF and TF depths \n with overflows (OF) events")

#2. Plotting RF+FOG and TF

TF.RF.fog.depth$RFfog = TF.RF.fog.depth$RF + TF.RF.fog.depth$fog
TF.RFfog.mm = TF.RF.fog.depth[,c("date", "TF", "RFfog")]

long.TF.RFfog.mm =  melt(TF.RFfog.mm, id.vars = "date") 

long.TF.RFfog.mm$m = format(long.TF.RFfog.mm$date, "%m")
long.TF.RFfog.mm$year = format(long.TF.RFfog.mm$date, "%Y")

ggplot(data = long.TF.RFfog.mm,
       mapping = aes(x = m, y = value, colour = variable, group = variable)) +
  geom_line() +  scale_color_manual(values=c("Saddle Brown", "Sky Blue"), name="Fluxes", breaks=c("RFfog","TF"), labels=c("RF+FOG", "TF")) +
  facet_grid(facets = year ~ .) +  labs( x = "month", y = expression(flux~ depth~"(mm"~~month^"-1"*")")) + 
  ggtitle ("Comparison between \n RF+fog depth and TF depth")

# This one shows that there is still huge space to increase RF, but let's see it by calculating the value TF/RF and then plot it!
# 2a: CALCULATING TF/RF AND PLOTTING IT

TF.RF.depth$ratio=(1-TF.RF.depth$TF/TF.RF.depth$RF)*100

TFRF.ratio = TF.RF.depth[, c("date","ratio")]

# add cutoffs: (literature gives interception around 30-60%. Being interception = 1 - RF/TF, my cutoffs are)
TFRF.ratio$cutoff40 = 30
TFRF.ratio$cutoff70 = 60

# wide to long:
long.TFRF.ratio = melt(TFRF.ratio, id.vars = "date")

long.TFRF.ratio = transform(long.TFRF.ratio, date = as.yearmon(as.character(date), "%Y%m"))
long.TFRF.ratio$m = format(long.TFRF.ratio$date, "%m")
long.TFRF.ratio$year = format(long.TFRF.ratio$date, "%Y")

# PLOT RATIO WITH MIN AND MAX - this was huuuuge work, solved the following problems:
# 1 .double legend 2. different lynetipes 3. horizontal cutoff previously added before melting :)

  ggplot(data = long.TFRF.ratio, mapping = aes(x = m, y = value, group = variable, colour = variable)) +
    geom_line(aes(linetype=variable), size=0.5) +  scale_color_manual(values=c("#000000","#CC0000", "#CC0000"), labels=c("Interception", "interception 30%", "interception 60%")) +
    scale_linetype_manual(values = c(1,3,2), labels=c("Interception", "interception 30%", "interception 60%"))  +
    facet_grid(facets = year ~ .) +  labs( x = "month", y = "TF/RF %") + ylim(-35,100) +
    ggtitle ("Canopy interception rate in Griffin")
  




#########################################
# CREATE A NX yearly TABLE FOR RF and fog (Kate, comparing to literature):
table.m.NX.RFfog=cbind(monthlyNH4.RF, monthlyNO3.RF, monthlyNH4.fog, monthlyNO3.fog) # monthlyCsw.NH4, monthlyCsw.NO3, monthlyTsw.NH4, monthlyTsw.NO3
table.m.NX.RFfog <- table.m.NX.RFfog[, !duplicated(colnames(table.m.NX.RFfog))]
table.m.NX.RFfog = transform(table.m.NX.RFfog, mY = as.yearmon(as.character(mY), "%Y%m"))
table.m.NX.RFfog$year = format(table.m.NX.RFfog$mY, "%Y")

# export Nx in RF and FOG as .xls file
WriteXLS(table.m.NX.RFfog, "output_tables_plots/monthly.NX.RF_FOG.xlsx") 

# wide to long for ggplot
year.table.NX.RFfog = melt(table.m.NX.RFfog, id.vars = "year")
year.table.NX.RFfog = aggregate(value ~ year + variable, data = year.table.NX.RFfog, sum)
selected = c("NH4.N.RF", "NO3.N.RF", "NH4.N.fog", "NO3.N.fog")
yearly.NX.RFfog = year.table.NX.RFfog[year.table.NX.RFfog$variable %in% selected,]

#  PLOTTING YEARLY N MASSES IN FOG AND RF

x = ggplot(data = yearly.NX.RFfog, aes (year, value, fill = variable))

plottone = x + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("royal blue", "Sky Blue", "grey40", "grey70"), name = "N flux \n and form", labels = c(expression(RF~NH[4]*-N), expression(RF~NO[3]*-N), expression(fog~NH[4]*-N), expression(fog~NO[3]*-N))) +
  labs( x = "year", y = expression(N~flux~~"(kg N"~~ha^"-1"~y^"-1"*")")) + ggtitle("Yearly fluxes in Griffin \n in rainfall (RF) and fog")


# CREATE A TABLE WITH ALL FLUXES TO THEN SUM RF AND FOG, TF AND SF, AND PLOTT'EM

# CREATE A NX TABLE FOR ALL FLUXES:
table.m.NX.RFTS=cbind(monthlyNH4.RF, monthlyNO3.RF, monthlyNH4.fog, monthlyNO3.fog, monthlyNH4.TF, monthlyNO3.TF, monthlyNH4.SF, monthlyNO3.SF) # monthlyCsw.NH4, monthlyCsw.NO3, monthlyTsw.NH4, monthlyTsw.NO3
table.m.NX.RFTS <- table.m.NX.RFTS[, !duplicated(colnames(table.m.NX.RFTS))]
table.m.NX.RFTS = transform(table.m.NX.RFTS, mY = as.yearmon(as.character(mY), "%Y%m"))


# export THE Nx ALL FLUXES TABLEas .xls file
WriteXLS(table.m.NX.RFTS, "output_tables_plots/monthly.NX_all_fluxes.xlsx") 

# Create the overall fluxes over and under canopy:
table.m.NX.RFTS$RFOG.NH4 = table.m.NX.RFTS$NH4.N.RF+table.m.NX.RFTS$NH4.N.fog
table.m.NX.RFTS$RFOG.NO3 = table.m.NX.RFTS$NO3.N.RF+table.m.NX.RFTS$NO3.N.fog
table.m.NX.RFTS$TFSF.NH4 = table.m.NX.RFTS$NH4.N.TF + table.m.NX.RFTS$NH4.N.SF
table.m.NX.RFTS$TFSF.NO3 = table.m.NX.RFTS$NO3.N.TF + table.m.NX.RFTS$NO3.N.SF
# remove unwanted columns and change:
over.under.canopy = table.m.NX.RFTS[,-c(2:9)]

# long format:
long.over.under.canopy = melt(over.under.canopy, id.vars = "mY")
# add month and year column
long.over.under.canopy = transform(long.over.under.canopy, mY = as.yearmon(as.character(mY), "%Y%m"))
long.over.under.canopy$month = format(long.over.under.canopy$mY, "%m")
long.over.under.canopy$year = format(long.over.under.canopy$mY, "%Y")
long.over.under.canopy$value=long.over.under.canopy$value/1000 # turn all values from g to kg

# PLOTT'EM!
# NH4N and NO3N over and under canopy

x = ggplot(data = long.over.under.canopy, aes (month, value, fill = variable))


plottonzo = x + geom_bar(stat = "identity", position = "dodge") + ylim(0, 2.1) +
  scale_fill_manual(values = c("#0033CC", "#6699FF", "#666600", "#CCCC99"), name = "N flux \n and form", labels = c(expression(RF+FOG~NH[4]*-N), expression(RF+FOG~NO[3]*-N), expression(TF+SF~NH[4]*-N), expression(TF+SF~NO[3]*-N))) +
  facet_grid(year ~ .) + labs( x = "month", y = expression(N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) + 
  ggtitle("N fluxes overcanopy and \n undercanopy") 

# tried to turn it into a log scale but Stack Overflows rightly says that geom_bar and scale_y_log10 don't work well together (0 vals are impossible, vals <1 are negative)


###############   RF and TF MONTHLY N flux overall (NO3+NH4), faceted by year 

over.under.canopy$over=over.under.canopy$RFOG.NH4+over.under.canopy$RFOG.NO3
over.under.canopy$under=over.under.canopy$TFSF.NH4+over.under.canopy$TFSF.NO3
Over.Under.N = over.under.canopy[, c("mY", "over", "under")]
  
long.Over.Under.N = melt(Over.Under.N, id.vars = "mY")

# add month and year column
long.Over.Under.N = transform(long.Over.Under.N, mY = as.yearmon(as.character(mY), "%Y%m"))
long.Over.Under.N$month = format(long.Over.Under.N$mY, "%m")
long.Over.Under.N$year = format(long.Over.Under.N$mY, "%Y")
long.Over.Under.N$value=long.Over.Under.N$value/1000 # turn all values from g to kg

y = ggplot(data = long.Over.Under.N, aes (month, value, fill = variable))

plottino = y + geom_bar(stat = "identity", position = "dodge") + ylim(0, 3) +
  scale_fill_manual(values = c("#3366FF", "#663300"), name = "N flux", labels = c("RF + fog Nx", "TF + SF Nx")) +
  facet_grid(year ~ .) +
  labs( x = "month", y = expression(N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) +
  ggtitle("Total N overcanopy and \n undercanopy") 


