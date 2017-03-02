# --------------------------------------------------------
#  Daniele Ferraretto, working on data in Griffin,SQLite
#  started on 20th July, 2015
#  updated: 19/08/2015          last update: 20/08/2015
# --------------------------------------------------------
# --------------------------------------------------------
# Set up Griffin database
# --------------------------------------------------------
# --------------------------------------------------------
# needed libraries:
library(RSQLite)
# ----------------------------------------------
# Connect to SQLite db
# ----------------------------------------------
getwd()
# ----------------------------------------------
# Connect to SQLite db
# ----------------------------------------------
db = dbConnect(SQLite(), dbname="data/Griffin.SQLite")

##### siccome non riesco a far girare stoca, mercoledi' installo l'addon su Firefox, 
##### mi esporto i .csv e lavoro su quelli perche' enough is enough...
# --------------------------------------------------------
# clear the memory
rm(list=ls())
# --------------------------------------------------------
# Carico i dati fielddata e labdata in due db distinti

fielddata <- read.csv("~/My PhD/R/mikerspencer-griffin-e2070d91e30c/mikerspencer-griffin-e2070d91e30c/fielddata.csv", header=FALSE)
View(fielddata)

labdata <- read.csv("~/My PhD/R/mikerspencer-griffin-e2070d91e30c/mikerspencer-griffin-e2070d91e30c/labdata.csv", header=FALSE)
View(labdata)
# --------------------------------------------------------
# --------------------------------------------------------
# Provo a lavorare tra i due db con tapply(x,by,function) dopo aver subsettato magari il NO3 da labdata?
# subsetto labdata per NO3.N

NO3data=subset(labdata, V4=="NO3.N")
colnames(NO3data)=c("Date","sample","plot","N form","value", "V6", "V7")
View(NO3data)

# --------------------------------------------------------
# estraggo STEMFLOW senza NO OVERFLOW per il momento. Per il NUMBER OF TREES PER HECTAR usero':
# 1883 trees/ha su T (2770trees/ha reduced of 5th row and 3rd tree), 1439 trees/ha per C (further reduction after attrition)#####
# --------------------------------------------------------

stemflow=subset(fielddata, V5=="stem vol", select=c(V1, V3, V4, V6))
colnames(stemflow)=c("Date","sample","plot","value") # cambio il nome alle colonne, piu' intuitivo
View(stemflow)

# --------------------------------------------------------
# merge STEMFLOW & LAB-NO3 by date and CODE

stemflow.NO3=merge(stemflow,NO3data,by.x=c("Date","sample"), by.y=c("Date","sample"))
View(stemflow.NO3)

# added the column stemflowNO3.N (the total quantity, not a concentration anymore):
stemflow.NO3$stemflowNO3.N=stemflow.NO3$value.x*stemflow.NO3$value.y 

# elimino la colonna ripetuta/inutili plot.y, V6 e V7 e rinomino le colonne
stemflow.NO31=stemflow.NO3[,c("Date","sample","plot.x","N form", "stemflowNO3.N")]
stemflow.NO3=stemflow.NO31
colnames(stemflow.NO3)=c("Date","sample","plot","N form","NO3.N")
View(stemflow.NO3)
# --------------------------------------------------------
# PLOTS with ggplot2
# --------------------------------------------------------
library(ggplot2)
# scatter plot with fit curve line

ggplot(stemflow.NO3, aes(Date, stemflowNO3.N) ) + 
      geom_point() + geom_smooth(method=lm)

ggplot(stemflow.NO3, aes(Date, stemflowNO3.N) ) +
  geom_smooth( aes(linetype = plot.x), method = "lm") +
  geom_point( aes(shape = plot.x) )

ggplot(stemflow.NO3, aes(Date, stemflowNO3.N) ) +
  geom_smooth(method = "lm")+
  geom_point()+ facet_grid(plot.x ~ .)

qplot(Date, NO3.N, data= stemflow.NO3, colour = factor(plot)) + geom_smooth(data=plot, method=lm) #bellino ma non riesco ad aggiungere fitlines DP
#PER CASA: boxplot e fitlines!!!

#risultato parziale: non mi escono le fit line, e soprattutto serve a poco.
# provo quindi con le bar plot che forse mi danno un miglior risultato



# --------------------------------------------------------
# divido i dati per plot C/T
stemflow.NO3.C = subset(stemflow.NO3, plot.x == "Control")
View(stemflow.NO3.C)
stemflow.NO3.T = subset(stemflow.NO3, plot.x == "Treatment")
View(stemflow.NO3.T)
# --------------------------------------------------------
# max min e altre stats potrebbero essere utili per descirvere i dati, da fare (forse) piu tardi
# --------------------------------------------------------
# calcolo MEAN e SD
# le tre prove sono servite a capire come mi labellizzava i risultati.
# il problema e' che la formula con due index per subsettare per data e plot funziona ma perdo la label del plot. Quindi devo:
# a) predividere i miei dati in due subset per ogni plot. b) calcolare la media per T e C con tapply. c) calcolare la media delle medie? unire i risultati e plottare!
# mean.stemflow.NO3.N0=  tapply(stemflow.NO3$stemflowNO3.N,stemflow.NO3$plot.x, mean,na.rm=TRUE)
# mean.stemflow.NO3.N00= tapply(stemflow.NO3$stemflowNO3.N,stemflow.NO3$Date,mean,na.rm=TRUE)
# mean.stemflow.NO3.C=tapply(stemflow.NO3$stemflowNO3.N,c(stemflow.NO3$Date,stemflow.NO3$plot), mean,na.rm=TRUE) #I can't get the columns with the aggregators (date+plot)!

mean.stemflow.NO3.C= tapply(stemflow.NO3.C$stemflowNO3.N,stemflow.NO3.C$Date,mean,na.rm=TRUE)
View(mean.stemflow.NO3.C)
mean.stemflow.NO3.T= tapply(stemflow.NO3.T$stemflowNO3.N,stemflow.NO3.T$Date,mean,na.rm=TRUE)
View(mean.stemflow.NO3.T)

# calcolo SD

sd.stemflow.NO3.C= tapply(stemflow.NO3.C$stemflowNO3.N,stemflow.NO3.C$Date,sd,na.rm=TRUE)
View(sd.stemflow.NO3.C)
sd.stemflow.NO3.T= tapply(stemflow.NO3.T$stemflowNO3.N,stemflow.NO3.T$Date,sd,na.rm=TRUE)
View(sd.stemflow.NO3.T)

# Calcolo length n
n.stemflow.NO3.C= tapply(stemflow.NO3.C$stemflowNO3.N,stemflow.NO3.C$Date,length)
View(n.stemflow.NO3.C)
n.stemflow.NO3.T= tapply(stemflow.NO3.T$stemflowNO3.N,stemflow.NO3.T$Date,length)
View(n.stemflow.NO3.T)
# ho dovuto eliminare il na.rm=TRUE perche' mi dava l'errore "2 arguments passed to 'length' which requires 1". 
# Ma cosi' n tiene conto anche dei NA, non ha senso




barplot(mean.stemflow.NO3.C)



# per plottare mi calcolo la media, la sd e la madre que los pario', cerca su libro


