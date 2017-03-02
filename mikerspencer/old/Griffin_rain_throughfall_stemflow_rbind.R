# --------------------------------------------------------
#  Daniele Ferraretto, working on data in Griffin,SQLite
#  started on 20th July, 2015
#  updated: 27/09/2015          last update: 28/09/2015
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

#View(labdata)

#NB ERRORS: missing lab values on labdata on 2015-03-25, Repeated dates in December

# --------------------------------------------------------
#         RAINFALL vs. THROUGHFALL vs. STEMFLOW
# --------------------------------------------------------
library(ggplot2)
library(plyr) # contains arrange function

# select only prec, stem and throughfall data
rts=subset(fielddata, V5=="precip depth"|V5=="stem vol"|V5=="through depth")
#rename columns
colnames(rts)=c("date","time","sample","plot","dg","value", "Overflow", "QC","notes")
colnames(labdata)=c("date","sample","plot","Nx","value", "Overflow", "QC")

#order by date
rts=arrange(rts,date)
#ora ho la throughfall depth, devo calcolarmi l'apporto stem per classe di alberi e poi posso
# comparare tutto
TF=subset(rts, dg=="through depth")
SF=subset(rts, dg=="stem vol")
RF=subset(rts, dg=="precip depth")
#subset labdata
NO3data=subset(labdata, Nx=="NO3.N")
NH4data=subset(labdata, Nx=="NH4.N")
# --------------------------------------------------------
#         1. THROUGHFALL NO3-N and NH4-N (mg/mq)
# --------------------------------------------------------
# The logic behind this verbose script is treating N quantities
# from throughfall as a mean value of Griffin as a whole, and of T/C plots

TF.NO3=merge(TF,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))
TF.NO3$NO3.N=TF.NO3$value.x*TF.NO3$value.y

# in case I need q/Q controls here is where to operate
TF.NO3=TF.NO3[,c("date","sample","plot.x","dg","NO3.N")] 
colnames(TF.NO3)=c("date","sample","plot","dg","NO3.N")
#View(TF.NO3)

# altra nota. Finora ho fatto direttamente i conti con la massa di N, ignorando i volumi di H20 da soli.


TF.NH4=merge(TF,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
TF.NH4$NH4.N=TF.NH4$value.x*TF.NH4$value.y
# in case I need q/Q controls here is where to operate
TF.NH4=TF.NH4[,c("date","sample","plot.x","dg","NH4.N")] 
colnames(TF.NH4)=c("date","sample","plot","dg","NH4.N")
#View(TF.NH4)

#####   MEAN TF Nxx-N ( mg per mq?)  ###### 

# OVERALL TF NO3-N, OVERALL
TF.NO3.mean = tapply(TF.NO3$NO3.N,TF.NO3$date,mean,na.rm=TRUE)

# mean TF NO3-N  per plot

TFC.NO3=subset(TF.NO3,plot=="Control")
TFT.NO3=subset(TF.NO3,plot=="Treatment")
# Control mean TF NO3-N
TFC.NO3.mean = tapply(TFC.NO3$NO3.N,TFC.NO3$date,mean,na.rm=TRUE)
# Treatment mean TF NO3-N
TFT.NO3.mean = tapply(TFT.NO3$NO3.N,TFT.NO3$date,mean,na.rm=TRUE)

# OVERALL TF NH4-N, OVERALL
TF.NH4.mean = tapply(TF.NH4$NH4.N,TF.NH4$date,mean,na.rm=TRUE)

# mean TF NH4-N  per plot

TFC.NH4=subset(TF.NH4,plot=="Control")
TFT.NH4=subset(TF.NH4,plot=="Treatment")
# Control mean TF NH4-N
TFC.NH4.mean = tapply(TFC.NH4$NH4.N,TFC.NH4$date,mean,na.rm=TRUE)
View(TFC.NH4.mean)
# Treatment mean TF NH4-N
TFT.NH4.mean = tapply(TFT.NH4$NH4.N,TFT.NH4$date,mean,na.rm=TRUE)

### building up TF.MEAN matrix
TF.MEAN=cbind(TF.NO3.mean,TF.NH4.mean,TFC.NO3.mean,TFC.NH4.mean,TFT.NO3.mean,TFT.NH4.mean)

## TF.MEAN in g/ha.
TF.MEAN.ha=TF.MEAN*10000/1000

#housekeeping:
rm(fielddata,labdata,rts,TF,TF.NO3,TF.NH4,TFC.NO3,TFT.NO3,TFC.NO3.mean,TFT.NO3.mean,TFC.NH4,TFT.NH4,TF.NH4.mean, TF.NO3.mean,TFC.NH4.mean,TFT.NH4.mean)

# --------------------------------------------------------
#         2. STEMFLOW NO3-N and NH4-N (mg/mq?)
# --------------------------------------------------------
# The logic behind this verbose script is treating N quantities
# from SF as a mean value of Griffin as a whole, and of T/C plots. 
# Differently from TF it has been necessary to calculate a mean per each 
# dbh class, so that the resulting matrix (values per average tree) are multiplied per 
# average number of trees per ha.
# Per il NUMBER OF TREES PER HECTAR usero' 1883 trees/ha su T (2770trees/ha reduced of 5th row and 3rd tree)
# whereas 1439 trees/ha per C would be the real numbers, including a further reduction due to attrition

# importo dbh classes e merge con SF
stemdbh <- read.csv("mikerspencer/stemdbh.csv", header=TRUE)
dbh.class=stemdbh[,c(1,5)]
# merge SF and dbh.class
SF.class=merge(SF,dbh.class,by.x=c("sample"), by.y=c("X"))
SF.class=arrange(SF.class,date)


SF.NO3=merge(SF.class,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))
SF.NO3$NO3.N=SF.NO3$value.x*SF.NO3$value.y
# in case I need q/Q controls here is where to operate
SF.NO3=SF.NO3[,c("date","sample","plot.x","dg","dbh.class","NO3.N")] 
colnames(SF.NO3)=c("date","sample","plot","dg","dbh.class","NO3.N")
#View(SF.NO3)

SF.NH4=merge(SF.class,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
SF.NH4$NH4.N=SF.NH4$value.x*SF.NH4$value.y
# in case I need q/Q controls here is where to operate
SF.NH4=SF.NH4[,c("date","sample","plot.x","dg","dbh.class","NH4.N")] 
colnames(SF.NH4)=c("date","sample","plot","dg","dbh.class","NH4.N")
#View(SF.NH4)

#####   MEAN SF Nxx-N ( mg per mq?)  ###### 

# OVERALL SF NO3-N, OVERALL trying with two variables: 
# the result SHOULD be the mean by dbh class AND date (one column per each class, one row per date)

SF.NO3.mean1 = tapply(SF.NO3$NO3.N,list(SF.NO3$date,SF.NO3$dbh.class),mean,na.rm=TRUE)
SF.NO3.mean=rowMeans(SF.NO3.mean1, na.rm = TRUE, dims = 1)
# SF.NO3.mean seems correct! Ora anzi dopo va tutto moltiplicato per il numero di alberi per ettaro!!!

#### mean SF NO3-N  per plot ####

SFC.NO3=subset(SF.NO3,plot=="Control")
SFT.NO3=subset(SF.NO3,plot=="Treatment")
# Control mean SF NO3-N
SFC.NO3.mean = tapply(SFC.NO3$NO3.N,SFC.NO3$date,mean,na.rm=TRUE)
SFC.NO3.mean1 = tapply(SFC.NO3$NO3.N,list(SFC.NO3$date,SFC.NO3$dbh.class),mean,na.rm=TRUE)
SFC.NO3.mean=rowMeans(SFC.NO3.mean1, na.rm = TRUE, dims = 1)
# Treatment mean SF NO3-N
SFT.NO3.mean = tapply(SFT.NO3$NO3.N,SFT.NO3$date,mean,na.rm=TRUE)
SFT.NO3.mean1 = tapply(SFT.NO3$NO3.N,list(SFT.NO3$date,SFT.NO3$dbh.class),mean,na.rm=TRUE)
SFT.NO3.mean=rowMeans(SFT.NO3.mean1, na.rm = TRUE, dims = 1)

# OVERALL SF NH4-N, OVERALL

SF.NH4.mean1 = tapply(SF.NH4$NH4.N,list(SF.NH4$date,SF.NH4$dbh.class),mean,na.rm=TRUE)
SF.NH4.mean=rowMeans(SF.NH4.mean1, na.rm = TRUE, dims = 1)
# SF.NH4.mean seems correct! Ora anzi dopo va tutto moltiplicato per il numero di alberi per ettaro!!!

#### mean SF NH4-N  per plot ####

SFC.NH4=subset(SF.NH4,plot=="Control")
SFT.NH4=subset(SF.NH4,plot=="Treatment")
# Control mean SF NH4-N
SFC.NH4.mean = tapply(SFC.NH4$NH4.N,SFC.NH4$date,mean,na.rm=TRUE)
SFC.NH4.mean1 = tapply(SFC.NH4$NH4.N,list(SFC.NH4$date,SFC.NH4$dbh.class),mean,na.rm=TRUE)
SFC.NH4.mean=rowMeans(SFC.NH4.mean1, na.rm = TRUE, dims = 1)
# Treatment mean SF NH4-N
SFT.NH4.mean = tapply(SFT.NH4$NH4.N,SFT.NH4$date,mean,na.rm=TRUE)
SFT.NH4.mean1 = tapply(SFT.NH4$NH4.N,list(SFT.NH4$date,SFT.NH4$dbh.class),mean,na.rm=TRUE)
SFT.NH4.mean=rowMeans(SFT.NH4.mean1, na.rm = TRUE, dims = 1)

### building up SF.MEAN matrix ("average tree")
SF.MEAN=cbind(SF.NO3.mean,SF.NH4.mean,SFC.NO3.mean,SFC.NH4.mean,SFT.NO3.mean,SFT.NH4.mean)

# SF values in g per hectare

SF.MEAN.ha=SF.MEAN*1883/1000
#View(SF.MEAN.ha)
#housekeeping:
rm(dbh.class,stemdbh,SF,SF.NH4.mean1,SF.class,SF.NO3.mean1,SFT.NH4.mean1,SFT.NO3.mean1,SFC.NH4.mean1,SFC.NO3.mean1,SF.NO3,SF.NH4,SFC.NO3,SFT.NO3,SFC.NO3.mean,SFT.NO3.mean,SFC.NH4,SFT.NH4,SF.NH4.mean,SF.NO3.mean,SFC.NH4.mean,SFT.NH4.mean)

# --------------------------------------------------------
#         3. RAINFALL NO3-N and NH4-N (mg/mq?)
# --------------------------------------------------------
#NB RF è una massa per giorno!!!!

RF1= subset(RF, sample=="C30D1"|sample=="C30D2"|sample=="C31D1")

# merge RF1 and lab data
RF.NO3=merge(RF1,NO3data,by.x=c("date","sample"), by.y=c("date","sample"))

RF.NO3$NO3.N=RF.NO3$value.x*RF.NO3$value.y

# housekeeping; in case I need q/Q controls here is where to operate
RF.NO3=RF.NO3[,c("date","sample","dg","NO3.N")] 
colnames(RF.NO3)=c("date","sample","dg","NO3.N")


RF.NH4=merge(RF1,NH4data,by.x=c("date","sample"), by.y=c("date","sample"))
RF.NH4$NH4.N=RF.NH4$value.x*RF.NH4$value.y
# in case I need q/Q controls here is where to operate
RF.NH4=RF.NH4[,c("date","sample","dg","NH4.N")] 
colnames(RF.NH4)=c("date","sample","dg","NH4.N")
# View(RF.NH4)

#####   MEAN RF Nxx-N and "FOG" RF Nxx-N ###### 
# NO3 in RF: subset and mean the subset
RF.NO3.sub=subset(RF.NO3, sample=="C30D1"|sample=="C31D1")
RF.NO3.MEAN= tapply(RF.NO3.sub$NO3.N,RF.NO3.sub$date,mean)
#View(RF.NO3.MEAN)
# ora lo risolvo con una porcata vergognosa, ma devo chiedere a Mike come darne fuori onorevolmente!
# DA CASA: subsetta per sample, merge e fai la differenza colonne!
RF.NO3.C30D1=subset(RF.NO3, sample=="C30D1")
RF.NO3.C30D2=subset(RF.NO3, sample=="C30D2")
RF.NO3.fog=merge(RF.NO3.C30D1,RF.NO3.C30D2, by.x="date", by.y="date")
RF.NO3.fog$RF.NO3.fog=RF.NO3.fog$NO3.N.y-RF.NO3.fog$NO3.N.x
RF.NO3.FOG=RF.NO3.fog[,c("date", "RF.NO3.fog")]
#View(RF.NO3.FOG)

#NH4:
RF.NH4.sub=subset(RF.NH4, sample=="C30D1"|sample=="C31D1")
RF.NH4.MEAN= tapply(RF.NH4.sub$NH4.N,RF.NH4.sub$date,mean)
#View(RF.NH4.MEAN)
# ora lo risolvo con una porcata vergognosa, ma devo chiedere a Mike come darne fuori onorevolmente!
# DA CASA: subsetta per sample, merge e fai la differenza colonne!
RF.NH4.C30D1=subset(RF.NH4, sample=="C30D1")
RF.NH4.C30D2=subset(RF.NH4, sample=="C30D2")
RF.NH4.fog=merge(RF.NH4.C30D1,RF.NH4.C30D2, by.x="date", by.y="date")
RF.NH4.fog$RF.NH4.fog=RF.NH4.fog$NH4.N.y-RF.NH4.fog$NH4.N.x
RF.NH4.FOG=RF.NH4.fog[,c("date", "RF.NH4.fog")]
#View(RF.NH4.FOG)
# esercizio di stile obbrobrioso riuscito. Resta un problema sulla FOG: ho dimenticato che
# è "schermata" e che quindi per capire quanta dell'acqua sia acqua di fog-na non so bene che fare:
# O va bene cosi? booooh
#altro dubbio è sui periodi. Credo e spero k Mike abbia fatto le cose bene e devo trovare
# dove si tiene conto dei giorni passati tra sampling e sampling



#housekeeping:
rm(NH4data, NO3data,RF,RF.C30D1,RF.C30D2,RF.FOG,RF.fog,RF.NH4,RF.NO3,RF.sub,RF1,RF.NO3.C30D1,RF.NO3.C30D2,RF.NO3.FOG,RF.NO3.fog,RF.NO3.sub,RF.NH4.C30D1,RF.NH4.C30D2,RF.NH4.FOG,RF.NH4.fog,RF.NH4.sub)

RF.TF.SF= cbind(RF.NO3.MEAN,RF.NH4.MEAN,SF.MEAN.ha,TF.MEAN.ha)
class(RF.TF.SF)

# file PROVVISORIO per poter importare i giorni tra un sampling e l'altro'
#d.days <- read.csv("mikerspencer/date-days.csv", header=TRUE) 
#tempRF.TF.SF= cbind(d.days,RF.TF.SF)
#rm(d.days)

##########################################################
#######           TF, SF. RF PLOTS            ############
##########################################################

x=c(26,20,22,9,14,28,35,28,42,29,27,29,33,35,22,28,28,35,34,36,21,35,28,38,25,42,41,72,34,28,28,27,30,34,28,33,38,27,18,52,18,27,18)
y=read.csv("mikerspencer/dates.csv", header=TRUE)
RF.TF.SF1=RF.TF.SF[c(-18:-19,-26,-45),]

RFw.TF.SF=cbind(y,x,RF.TF.SF1)
df=as.data.frame(RFw.TF.SF)

#write the table, as all plots have shown to be a disaster. It's the df structure
# to be absolutely wrong. I need to restart, but before restarting I want to build up the whole db
# using Mike's files. Then I will go to Mike and check my mess.


df$RF.NO3.MEAN=df$RF.NO3.MEAN*df$x*10000/1000
df$RF.NH4.MEAN=df$RF.NH4.MEAN*df$x*10000/1000
write.table(df, "C:/Users/Daniele Ferraretto/Documents/PhD-local_repo/RF_TF_SF1.txt", sep="\t") 

#df has cumulative RF-N values (mg/m2) per each sampling period; multiplying *10000/1000
# I get g/ha, as TF and SF (in g/ha)

ggplot(df, aes(dates)) + 
  geom_line(aes(y = RF.NO3.MEAN, colour = "blue")) + 
  geom_line(aes(y = SF.NO3.mean, colour = "green")) +
  geom_line(aes(y = TF.NO3.mean, colour = "red"))
#epic fail
  
ggplot(df, aes(dates, RF.NO3.MEAN)) + geom_point()  + xlab("Date") + ylab("N-NO3 (g/ha)")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(df, aes(dates, TF.NO3.mean)) + geom_point()  + xlab("Date") + ylab("N-NO3 (g/ha)")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(df, aes(dates, SF.NO3.mean)) + geom_point()  + xlab("Date") + ylab("N-NO3 (g/ha)")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
