# --------------------------------------------------------
#  Daniele Ferraretto, working on data in Griffin,SQLite
#  started on 25th August, 2015
#  updated: 27/08/2015          last update: 29/09/2015
# --------------------------------------------------------
# --------------------------------------------------------

# --------------------------------------------------------
# clear the memory
rm(list=ls())

### set working dir for pc:
setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
setwd("M:/My PhD/R/PhD-local_repo")
# This is gonna be the only difference between windows pc and desktop,
# as I will keep the same relative position for the scripts and files within
# PhD-local_repo

# --------------------------------------------------------
# Carico i dati fielddata e labdata in due db distinti

fielddata <- read.csv("mikerspencer/fielddata.csv", header=FALSE)

#View(fielddata)

labdata <- read.csv("mikerspencer/labdata.csv", header=FALSE)

# --------------------------------------------------------
# --------------------------------------------------------
# PART 1. PRECIPITATION
# --------------------------------------------------------
# --------------------------------------------------------

# Subsetto labdata and labdata for precipitation

prec.data=subset(fielddata, V5=="precip depth")
colnames(prec.data)=c("Date","time","sample","plot","V5","values", "V7", "V8","V9")
prec.data$Date = as.Date(prec.data$Date)
#View(prec.data)
#summary(prec.data$Date)
library(ggplot2)
library(plyr)
# NB: problems with plotting were caused: not by a lack of sorting by date, not by "weird" factor values, boh!
# anyway I couldn't solve sorting by date with the function "arrange" in R; I can't solve this :(
newprecdata <- prec.data[ which(prec.data$sample=='C30D1'| prec.data$sample=='C30D2'| prec.data$sample=='C31D1'),c("Date", "sample", "values") ]
#View(newprecdata)
#---------------------------------------------------------
# lab NO3 and NH4
labNxx=subset(labdata, V2=="C30D1"|V2=="C30D2"|V2=="C31D1")
labNxx=subset(labNxx, V4=="NO3.N"|V4=="NH4.N")
colnames(labNxx)=c("date","sample","plot","Nspec","value", "V6", "V7")
labNxx=arrange(labNxx,date)
#View(labNxx) #dati ora in ordine cronologico
#---------------------------------------------------------


# --------------------------------------------------------
# 1b. General plots on precipitation
# --------------------------------------------------------

# Mike's prec plot:
ggplot(prec.data, aes(Date, values)) + geom_point() + facet_grid(sample ~ .) + xlab("Date") + ylab("Precipitation (mm)")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(newprecdata, aes(Date, values)) + geom_point() + facet_grid(sample ~ .) + xlab("Date") + ylab("Precipitation (mm)")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# NB Default date format here and in SQL il yyyy mm dd, Excel put it into dd mm yyyy.
# Qui vorrei tanto aggiungere dei "segmenti di media per ogni anno per ogni smaple, ma non sono sicuro di riuscirci, see stackoverflow "adding partial horizontal lines" Q.
#---------------------------------------------------------------------
# PREC GRAPH WITH TIME SERIES FACETTED PER SAMPLE + MEAN
#---------------------------------------------------------------------
P0=ggplot(newprecdata, aes(Date, values)) + geom_point() + facet_grid(sample ~ .) + xlab("Date") + ylab("precipitation (mm d-1)")#+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#adding a mean line to the graph:
meanprec.sample=tapply(newprecdata$values, newprecdata$sample, mean, na.rm = TRUE) # calculated samples media

hline.precmean=data.frame(z=meanprec.sample,sample=c("C30D1","C30D2","C31D1")) # created a data.frame with media and sample
# extra exercise quando ho tempo: vedere se e cosa cambia cambiando sample =c("C30D1","C30D2","C31D1") con sample = c(1:3) o anche solo (c(1:3))

P0+geom_hline(aes(yintercept = z,linetype="dashed", color = "red"), hline.precmean) 
# NOTA: blue returns red, linetype non risponde ne' al codice 0-6 
# ne' alla stringa (solid, dotted, dashed, dotdash, longdash, twodash)
#----------------------------------------------------------------------
#----------------------------------------------------------
# --------------------------------------------------------
# 1c. NO3-N and NH4-N in precipitaiton
# --------------------------------------------------------

prec.N=merge(newprecdata,labNxx,by.x=c("Date","sample"), by.y=c("date","sample"))

# adding the column NO3.N (the total quantity, not a concentration anymore):
prec.N$Nxn.N=prec.N$values*prec.N$value 
#View(prec.N)

#subset values
prec.NO3=subset(prec.N,Nspec=="NO3.N")
prec.NH4=subset(prec.N,Nspec=="NH4.N")
#View(prec.NO3)
#ok, boy, hai ottenuto la quantita' di N nelle sue due forme nella precipitazione. Ora plotta a scassucazzu!!!

# --------------------------------------------------------
# 1d. NO3-N and NH4-N in precipitaiton - PLOTS
# --------------------------------------------------------
library(ggplot2)
summary(prec.NO3)
qplot(Date, Nxn.N, data= prec.NO3, colour = factor(sample))+ labs(y="NO3 (mg/m2/day)") #NO3 time series for all samples
qplot(Date, Nxn.N, data= prec.NH4, colour = factor(sample))+ labs(y="NH4 (mg/m2/day)") #NH4 time series for all samples
# ATTENZIONE AI VALORI QUI! HO MOLTIPLICATO mm/d*[N(mg/l)], vuol dire mg/m2/day
#---------------------------------------------------------------------------------
# 1e. NO3-N and NH4 GRAPH WITH TIME SERIES FACETTED PER SAMPLE, EACH WITH THEIR OWN MEAN
#---------------------------------------------------------------------------------
P1=ggplot(prec.NO3, aes(Date, Nxn.N)) + geom_point() + facet_grid(sample ~ .) + xlab("Date") + ylab("NO3-N (mg m-2 d-1)")#+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#adding a mean line to the graph:
meanNO3.sample=tapply(prec.NO3$Nxn.N, prec.NO3$sample, mean, na.rm = TRUE) # calculated samples media

hline.NO3mean=data.frame(z=meanNO3.sample,sample=c("C30D1","C30D2","C31D1")) # created a data.frame with media and sample
# extra exercise quando ho tempo: vedere se e cosa cambia cambiando sample =c("C30D1","C30D2","C31D1") con sample = c(1:3) o anche solo (c(1:3))

P1+geom_hline(aes(yintercept = z,linetype="dashed", color = "red"), hline.NO3mean) 
# NOTA: blue returns red, linetype non risponde ne' al codice 0-6 
# ne' alla stringa (solid, dotted, dashed, dotdash, longdash, twodash)
#----------------------------------------------------------------------

P2=ggplot(prec.NH4, aes(Date, Nxn.N)) + geom_point() + facet_grid(sample ~ .) + xlab("Date") + ylab("NH4-N (mg m-2 d-1)")#+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#adding a mean line to the graph:
meanNH4.sample=tapply(prec.NH4$Nxn.N, prec.NH4$sample, mean, na.rm = TRUE) # calculated samples media

hline.NH4mean=data.frame(z=meanNH4.sample,sample=c("C30D1","C30D2","C31D1")) # created a data.frame with media and sample
# extra exercise quando ho tempo: vedere se e cosa cambia cambiando sample =c("C30D1","C30D2","C31D1") con sample = c(1:3) o anche solo (c(1:3))

P2+geom_hline(aes(yintercept = z,linetype="dashed", colour = "blue"), hline.NH4mean) 
# NOTA: blue or any other color returns red, linetype non risponde ne' al codice 0-6 
# ne' alla stringa (solid, dotted, dashed, dotdash, longdash, twodash)


ggplot(prec.NH4, aes(Date, Nxn.N)) + geom_point() + facet_grid(sample ~ .) + xlab("Date") + ylab("NH4 (mg -2 d-1)")


# --------------------------------------------------------
# --------------------------------------------------------
# PART 2. STREAMWATER
# --------------------------------------------------------
# --------------------------------------------------------

# Subsetto labdata and labdata for  streamflow
selected.sw=c("C20SW1","C21SW1","T20SW1","T21SW1")
stream.data=fielddata[fielddata$V3 %in% selected.sw, ]
colnames(stream.data)=c("Date","time","sample","plot","V5","values", "V7", "V8","V9")
stream.data$Date = as.Date(stream.data$Date)
stream.data=arrange(stream.data,Date)
View(stream.data)
summary(prec.data$Date)
library(ggplot2)
library(plyr)
# NB: problems with plotting were caused: not by a lack of sorting by date, not by "weird" factor values or levels, boh!
# --------------------------------------------------------
# PART 2a. STREAMWATER Discharge plots
# --------------------------------------------------------
# The following plot shows gauged flow, v-notch flow and w-height at the stageboard per each sample
ggplot(stream.data, aes(x=Date, y=values,colour=V5)) + geom_point() + facet_grid(sample ~ .) + xlab("Date") + ylab("Discharge (l/s)")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(stream.data, aes(x=Date, y=values,colour=V5)) + geom_point() + facet_grid(sample ~ V5) + geom_smooth(method=loess, aes(colour=V5)) + coord_cartesian(ylim=c(-1,40)) + xlab("Date") + ylab("Discharge (l/s)")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# now only gauge vs. v-notch
stream.flows=subset(stream.data, V5=="gauged flow"|V5=="v-notch flow")
ggplot(stream.flows, aes(x=Date, y=values,colour=V5)) + geom_point() + facet_grid(sample ~ .) + xlab("Date") + ylab("Discharge (l/s)")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#---------------------------------------------------------
# lab NO3 and NH4
labN.SW=subset(labdata, V2=="C20SW1"|V2=="C21SW1"|V2=="T20SW1"|V2=="T21SW1")
labNxx=subset(labNxx, V4=="NO3.N"|V4=="NH4.N")
colnames(labN.SW)=c("date","sample","plot","Nspec","value", "V6", "V7")
labN.SW=arrange(labN.SW,date)
#View(labN.SW) #dati ora in ordine cronologico
#---------------------------------------------------------

