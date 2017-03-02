# --------------------------------------------------------
#  Daniele Ferraretto, working on data in Griffin,SQLite
#  started on 20th July, 2015
#  updated: 19/08/2015          last update: 21/09/2015
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
# ----------------------------------------------
# Connect to SQLite db
# ----------------------------------------------
db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

##### ho finalmente ottenuto di connettermi al db SQLite!
##### however prima di spaccarsi la testa mi esporto i .csv e lavoro su quelli
##### per fare una comparazione quantitativa precipitation throughfall stemflow. 
##### Sulla base di quella poi decido se inistere con stemflow o se sciacuarmene le palle

# --------------------------------------------------------
# clear the memory
rm(list=ls())
# --------------------------------------------------------
# Carico i dati fielddata e labdata in due db distinti

fielddata <- read.csv("~/My PhD/R/mikerspencer-griffin-e2070d91e30c/mikerspencer-griffin-e2070d91e30c/fielddata.csv", header=FALSE)
#View(fielddata)

labdata <- read.csv("~/My PhD/R/mikerspencer-griffin-e2070d91e30c/mikerspencer-griffin-e2070d91e30c/labdata.csv", header=FALSE)
#View(labdata)
# --------------------------------------------------------
# PART 1. THROUGHFALL AND NO3
# --------------------------------------------------------

# Subsetto labdata per NO3.N

NO3data=subset(labdata, V4=="NO3.N")
colnames(NO3data)=c("Date","sample","plot","N form","value", "V6", "V7")
#View(NO3data)

# --------------------------------------------------------
# estraggo throughfall senza NO OVERFLOW per il momento. Per il NUMBER OF TREES PER HECTAR usero':
# 1883 trees/ha su T (2770trees/ha reduced of 5th row and 3rd tree), 1439 trees/ha per C (further reduction after attrition)#####
# --------------------------------------------------------

throughfall=subset(fielddata, V5=="through vol", select=c(V1, V3, V4, V6))
colnames(throughfall)=c("Date","sample","plot","value") # cambio il nome alle colonne, piu' intuitivo
#View(throughfall)

# --------------------------------------------------------
# merge throughfall & LAB-NO3 by date and CODE

throughfall.NO3=merge(throughfall,NO3data,by.x=c("Date","sample"), by.y=c("Date","sample"))
#View(throughfall.NO3)

# added the column NO3.N (the total quantity, not a concentration anymore):
throughfall.NO3$NO3.N=throughfall.NO3$value.x*throughfall.NO3$value.y 

# elimino la colonna ripetuta/inutili plot.y, V6 e V7 e rinomino le colonne
throughfall.NO31=throughfall.NO3[,c("Date","sample","plot.x","N form", "NO3.N")]
throughfall.NO3=throughfall.NO31
colnames(throughfall.NO3)=c("Date","sample","plot","N form","NO3.N")
rm(throughfall.NO31) #housekeeping (removing temp fil)
View(throughfall.NO3)
# --------------------------------------------------------
# PLOTTING  throughfall.NO3 with ggplot2
# --------------------------------------------------------
library(ggplot2)

# scatter plot with fit curve line

# ggplot(throughfall.NO3, aes(Date, NO3.N) ) + 
#      geom_point() + geom_smooth(method=lm)

#ggplot(throughfall.NO3, aes(Date, NO3.N) ) +
#  geom_smooth( aes(linetype = plot.x), method = "lm") +
#  geom_point( aes(shape = plot.x) )

#ggplot(throughfall.NO3, aes(Date, NO3.N) ) +
#  geom_smooth(method = "lm")+
#  geom_point()+ facet_grid(plot.x ~ .)

#qplot(Date, NO3.N, data = throughfall.NO3, geom = c("point", "smooth"))

qplot(Date, NO3.N, data= throughfall.NO3, colour = factor(plot)) #bellino ma non riesco ad aggiungere fitlines DP

# esempi di grafici: in realta' siccome per ogni data ho molti valori non riesco a ottenere una fit line. Quindi ora procedero' con i fitbox,
# poi mi calcolero' la media per ogni data e da quella provero' a costruire una fitline
# --------------------------------------------------------------
# MULTIPLE PLOT FUNCTION: HOW TO CREATE OBJECTS WITH MULTIPLE PLOTS
# --------------------------------------------------------------
#
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# -------------------------------------------------------------------
# Create a multiplot for NO3 volumes per plot per year
# -------------------------------------------------------------------

TNO3.1 = ggplot(throughfall.NO3[1:306,],  aes(x=Date, y=NO3.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2011
TNO3.2 = ggplot(throughfall.NO3[307:468,],aes(x=Date, y=NO3.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2012
TNO3.3 = ggplot(throughfall.NO3[469:684,],aes(x=Date, y=NO3.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2013
TNO3.4 = ggplot(throughfall.NO3[-(1:684),],aes(x=Date, y=NO3.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2014

#rm(px)
#px

multiplot (TNO3.1+ ggtitle("NO3-N \nin throughfall 2012")+ theme(plot.title = element_text(lineheight=.8, face="bold")), 
           TNO3.2+ ggtitle("NO3-N \nin throughfall 2013")+ theme(plot.title = element_text(lineheight=.8, face="bold")), 
           TNO3.3+ ggtitle("NO3-N \nin throughfall 2014")+ theme(plot.title = element_text(lineheight=.8, face="bold")), 
           TNO3.4+ ggtitle("NO3-N \nin throughfall 2015")+ theme(plot.title = element_text(lineheight=.8, face="bold")), cols=2)

# --------------------------------------------------------
# PART 2. THROUGHFALL and NH4
# --------------------------------------------------------

# subsetto labdata per NH4

NH4data=subset(labdata, V4=="NH4.N")
colnames(NH4data)=c("Date","sample","plot","N form","value", "V6", "V7")
#View(NH4data)

# --------------------------------------------------------
# merge throughfall & LAB-NH4 by date and CODE

throughfall.NH4=merge(throughfall,NH4data,by.x=c("Date","sample"), by.y=c("Date","sample"))
#View(throughfall.NH4)

# added the column NO3.N (the total quantity, not a concentration anymore):
throughfall.NH4$throughfallNH4.N=throughfall.NH4$value.x*throughfall.NH4$value.y 

# elimino la colonna ripetuta/inutili plot.y, V6 e V7 e rinomino le colonne
throughfall.NH41=throughfall.NH4[,c("Date","sample","plot.x","N form", "throughfallNH4.N")]
throughfall.NH4=throughfall.NH41
colnames(throughfall.NH4)=c("Date","sample","plot","N form","NH4.N")
rm(throughfall.NH41) #housekeeping (removing temp fil)
View(throughfall.NH4)

# --------------------------------------------------------
# PLOTTING  throughfall.NH4 with ggplot2
# --------------------------------------------------------

qplot(Date, NH4.N, data= throughfall.NH4, colour = factor(plot))

TNH4.1 = ggplot(throughfall.NH4[1:306,],  aes(x=Date, y=NH4.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2011
TNH4.2 = ggplot(throughfall.NH4[307:468,],aes(x=Date, y=NH4.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2012
TNH4.3 = ggplot(throughfall.NH4[469:684,],aes(x=Date, y=NH4.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2013
TNH4.4 = ggplot(throughfall.NH4[-(1:684),],aes(x=Date, y=NH4.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2014

# rm(pNH44) # se ho bisogno di rimuovere gli oggetti sopra creati per qualche ragione
multiplot (TNH4.1+ ggtitle("NH4-N \nin throughfall 2012")+ theme(plot.title = element_text(lineheight=.8, face="bold")), 
           TNH4.2+ ggtitle("NH4-N \nin throughfall 2013")+ theme(plot.title = element_text(lineheight=.8, face="bold")), 
           TNH4.3+ ggtitle("NH4-N \nin throughfall 2014")+ theme(plot.title = element_text(lineheight=.8, face="bold")), 
           TNH4.4+ ggtitle("NH4-N \nin throughfall 2015")+ theme(plot.title = element_text(lineheight=.8, face="bold")), cols=2)

#NOTE: a) sarebbe meglio fissare una grid y comune; 
#      b) c'e' un valore in 2013 e uno in 2014 che sballano il grafico, sarebbe il caso di fare una versione di plot eliminando il valore

# --------------------------------------------------------
# HOUSEKEEPING: QUANTO SOTTO RIPORTATO E' AL MOMENTO INUTILE ####
# --------------------------------------------------------

# --------------------------------------------------------
# max min e altre stats potrebbero essere utili per descirvere i dati, da fare (forse) piu tardi
# --------------------------------------------------------
# calcolo MEAN e SD
# le tre prove sono servite a capire come mi labellizzava i risultati.
# il problema e' che la formula con due index per subsettare per data e plot funziona ma perdo la label del plot. Quindi devo:
# a) predividere i miei dati in due subset per ogni plot. b) calcolare la media per T e C con tapply. c) calcolare la media delle medie? unire i risultati e plottare!
# mean.throughfall.NO3.N0=  tapply(throughfall.NO3$NO3.N,throughfall.NO3$plot.x, mean,na.rm=TRUE)
# mean.throughfall.NO3.N00= tapply(throughfall.NO3$NO3.N,throughfall.NO3$Date,mean,na.rm=TRUE)
# mean.throughfall.NO3.C=tapply(throughfall.NO3$NO3.N,c(throughfall.NO3$Date,throughfall.NO3$plot), mean,na.rm=TRUE) #I can't get the columns with the aggregators (date+plot)!

mean.throughfall.NO3.C= tapply(throughfall.NO3.C$NO3.N,throughfall.NO3.C$Date,mean,na.rm=TRUE)
View(mean.throughfall.NO3.C)
mean.throughfall.NO3.T= tapply(throughfall.NO3.T$NO3.N,throughfall.NO3.T$Date,mean,na.rm=TRUE)
View(mean.throughfall.NO3.T)

# calcolo SD

sd.throughfall.NO3.C= tapply(throughfall.NO3.C$NO3.N,throughfall.NO3.C$Date,sd,na.rm=TRUE)
View(sd.throughfall.NO3.C)
sd.throughfall.NO3.T= tapply(throughfall.NO3.T$NO3.N,throughfall.NO3.T$Date,sd,na.rm=TRUE)
View(sd.throughfall.NO3.T)

# Calcolo length n
n.throughfall.NO3.C= tapply(throughfall.NO3.C$NO3.N,throughfall.NO3.C$Date,length)
View(n.throughfall.NO3.C)
n.throughfall.NO3.T= tapply(throughfall.NO3.T$NO3.N,throughfall.NO3.T$Date,length)
View(n.throughfall.NO3.T)
# ho dovuto eliminare il na.rm=TRUE perche' mi dava l'errore "2 arguments passed to 'length' which requires 1". 
# Ma cosi' n tiene conto anche dei NA, non ha senso




barplot(mean.throughfall.NO3.C)



# per plottare mi calcolo la media, la sd e la madre que los pario', cerca su libro


