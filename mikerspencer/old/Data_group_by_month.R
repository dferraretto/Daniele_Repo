# --------------------------------------------------------
#  Daniele Ferraretto, from the daily SQLite db now 
#  creating a monthly new dataframe to be used for RTS
#  started on 3rd of November, 2015
#  updated: 06/11/2015             last update: 10/11/2015
# --------------------------------------------------------
# --------------------------------------------------------

# clear the memory
rm(list=ls())

library(RSQLite)


### set working dir for pc:
setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
setwd("M:/My PhD/R/PhD-local_repo")

#####################################################
#               Part 1: lab data
#####################################################

db = dbConnect(SQLite(), dbname="field_lab/Daily_Griffin.SQLite")

labdata = dbGetQuery(db, "SELECT rowid, date, sample, site, variable, vals, QC FROM labdata ORDER BY date")

  
fielddata = dbGetQuery(db, "SELECT date, sample, site, variable, vals, overflowing, QC FROM fielddata ORDER BY sample")

##########   Aggregate data by month   ##############

# Create a column "shorttime", with month and year:
labdata$shortdate <- strftime(labdata$date, format="%Y/%m")
fielddata$shortdate <- strftime(fielddata$date, format="%Y/%m")

# Aggregate by month:

mo.labdata=aggregate(vals ~ sample + shortdate, fielddata, sum)
mo.fielddata=aggregate(vals ~ sample + shortdate, fielddata, sum)

mo.fd=merge(mo.fielddata, fielddata, by.x= c("sample","shortdate"), by.y=c("sample","shortdate"), all=FALSE)

# OPPURE:
mo.labdata = by(data = labdata, INDICES = list(labdata$sample, labdata$shortdate), function(x) x[sum(x$vals), ])
mo.labdata=do.call(rbind, mo.labdata)

##########   Aggregate data by year   ##############

# Create a column "shorttime", with year only:
labdata$sd <- strftime(labdata$date, format="%Y")
fielddata$sd <- strftime(fielddata$date, format="%Y")

# ecc... NB: lavoro bloccato perché non posso lavorare su valori di laboratorio accumulati mensilmente!!! 
# cioè aV*c+bW*d ne (aV+bW)*(ac+bd)!!!!a numero giorni del mese m, b numero giorni del mese n, 
# V,c=volume giornaliero e conc. nel mese m, W, d volume giornaliero e conc. nel mese n. Sega a vapore e ciao

