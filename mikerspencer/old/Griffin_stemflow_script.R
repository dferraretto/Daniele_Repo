# --------------------------------------------------------
#  Daniele Ferraretto, working on data in Griffin,SQLite
#  started on 20th July, 2015
#  updated: 19/08/2015          last update: 21/09/2015
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

stemdbh = read.csv("mikerspencer/stemdbh.csv", header=FALSE)

View(stemdbh)
# --------------------------------------------------------
# PART 1. STEMFLOW AND NO3
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
#View(stemflow)

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
rm(stemflow.NO31) #housekeeping (removing temp fil)
View(stemflow.NO3)
# --------------------------------------------------------
# PLOTTING  STEMFLOW.NO3 with ggplot2
# --------------------------------------------------------
library(ggplot2)

qplot(Date, NO3.N, data= stemflow.NO3, colour = plot) + facet_grid(plot ~ .)
# esempi di grafici: in realta' siccome per ogni data ho molti valori non riesco a ottenere una fit line. Quindi ora procedero' con i fitbox,
# poi mi calcolero' la media per ogni data e da quella provero' a costruire una fitline

library("grid")
# Multiple plot function
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
p1 = ggplot(stemflow.NO3[1:352,], aes(x=Date, y=NO3.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2011
p2 = ggplot(stemflow.NO3[352:550,], aes(x=Date, y=NO3.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2012
p3 = ggplot(stemflow.NO3[551:814,], aes(x=Date, y=NO3.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2013
p4 = ggplot(stemflow.NO3[815:924,], aes(x=Date, y=NO3.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2014

multiplot (p1+ ggtitle("NO3-N in stemflow 2012"), p2+ ggtitle("NO3-N in stemflow 2013"), p3+ ggtitle("NO3-N in stemflow 2014"), p4+ ggtitle("NO3-N in stemflow 2015"), cols=2)

# --------------------------------------------------------
# PART 2. STEMFLOW AND NH4
# --------------------------------------------------------
# Provo a lavorare tra i due db con tapply(x,by,function) dopo aver subsettato magari il NO3 da labdata?
# subsetto labdata per NO3.N

NH4data=subset(labdata, V4=="NH4.N")
colnames(NH4data)=c("Date","sample","plot","N form","value", "V6", "V7")
View(NH4data)

# --------------------------------------------------------
# merge STEMFLOW & LAB-NH4 by date and CODE

stemflow.NH4=merge(stemflow,NH4data,by.x=c("Date","sample"), by.y=c("Date","sample"))
View(stemflow.NH4)

# added the column stemflowNO3.N (the total quantity, not a concentration anymore):
stemflow.NH4$stemflowNH4.N=stemflow.NH4$value.x*stemflow.NH4$value.y 

# elimino la colonna ripetuta/inutili plot.y, V6 e V7 e rinomino le colonne
stemflow.NH41=stemflow.NH4[,c("Date","sample","plot.x","N form", "stemflowNH4.N")]
stemflow.NH4=stemflow.NH41
colnames(stemflow.NH4)=c("Date","sample","plot","N form","NH4.N")
rm(stemflow.NH41) #housekeeping (removing temp fil)
View(stemflow.NH4)

# --------------------------------------------------------
# PLOTTING  STEMFLOW.NH4 with ggplot2
# --------------------------------------------------------

qplot(Date, NH4.N, data= stemflow.NH4, colour = factor(plot))
qplot(Date, NH4.N, data= stemflow.NH4, colour = factor(plot)) + coord_cartesian(ylim=c(-10,60))
pNH41 = ggplot(stemflow.NH4[1:352,], aes(x=Date, y=NH4.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2011
pNH42 = ggplot(stemflow.NH4[352:550,], aes(x=Date, y=NH4.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2012
pNH43 = ggplot(stemflow.NH4[551:814,], aes(x=Date, y=NH4.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2013
pNH44 = ggplot(stemflow.NH4[815:924,], aes(x=Date, y=NH4.N, fill=plot)) + geom_boxplot() + facet_grid(plot ~ .) #year 2014

multiplot (pNH41+ ggtitle("NH4-N \nin stemflow 2012"), pNH42+ ggtitle("NH4-N \nin stemflow 2013"), pNH43+ ggtitle("NH4-N \nin stemflow 2014"), pNH44+ ggtitle("NH4-N \nin stemflow 2015"), cols=2)
# the following line can be paste anywhere to check the actual usage of RAM by R
cat('point 1 mem',memory.size(),memory.size(max=TRUE),'\n')

#NOTE: a) sarebbe meglio fissare una grid y comune; 
#      b) c'e' un valore in 2013 e uno in 2014 che sballano il grafico, sarebbe il caso di fare una versione di plot eliminando il valore

# --------------------------------------------------------
# ######           Relation N -  tree DBH           ######
# --------------------------------------------------------
###da qui si riparte - da cancellare
#install.packages("dplyr")
library(dplyr) # inutile qui

View(stemdbh)
### DBH MEDIO
stemdbh$mean.dbh=(stemdbh$V2 + stemdbh$V3)/2
# merge stem.dbh and labdata
stemdbh.NO3=merge(stemdbh,stemflow.NO3,by.x="V1", by.y="sample")
View(stemdbh.NO3)
# order by meandbh 
stem.NO3.1=arrange(stemdbh.NO3,mean.dbh)
#calculate the average value per volume, concentration and total NO3 per sample
mean.volSF=  tapply(stem.NO3.1$value.x,stem.NO3.1$V1,mean, na.rm=TRUE)
mean.concNO3=tapply(stem.NO3.1$value.y,stem.NO3.1$V1,mean, na.rm=TRUE)
mean.NO3=    tapply(stem.NO3.1$stemflowNO3.N,stem.NO3.1$V1,mean, na.rm=TRUE)
# build up a matrix with these values to plot dbh vs. Vol,conc,NO3-N
mean.dbh=    tapply(stem.NO3.1$mean.dbh,stem.NO3.1$V1,mean, na.rm=TRUE)                     
MEANdbh.SF= cbind(mean.dbh,mean.volSF,mean.concNO3,mean.NO3)

############# PLOT dbh vs stemflow ##################
library(ggplot2)
View(MEANdbh.SF)
MEANdbhSF1=MEANdbh.SF[order(mean.dbh), ]
# converto matrix a dataframe per vedere se è quello il problema di ggplot
plot.dbh.df= as.data.frame(MEANdbhSF1,row.names = NULL)
View(plot.dbh.df)
# lm regression per VOL, conc, NO3

ggplot(plot.dbh.df, aes(mean.dbh, mean.volSF)) + geom_point() + geom_smooth(method=lm) + xlab("dbh (cm)") + ylab("stemflow vol (l)")#+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(plot.dbh.df, aes(mean.dbh, mean.concNO3)) + geom_point() + geom_smooth(method=lm) + xlab("dbh (cm)") + ylab("SF NO3-N conc (mg/l)")#+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(plot.dbh.df, aes(mean.dbh, mean.NO3)) + geom_point() + geom_smooth(method=lm) + xlab("dbh (cm)") + ylab("SF NO3-N (mg)")#+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# NB: NA are not considered. 2) mean has been applied tothe whole period of time. Maybe splitting into years would help "isolate" weird values
