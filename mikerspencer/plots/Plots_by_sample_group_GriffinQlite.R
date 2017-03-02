# -------------------------------------------------------- #
# -------------------------------------------------------- #
#                                                          #
#   PLOTS ON TF AND SF BY SAMPLE FROM GRIFFIN.SQLITE       #
#   created: 08/12/2016
#                                                          #
# -------------------------------------------------------- #
# -------------------------------------------------------- #

# clear the memory
rm(list=ls())
.libPaths("C:/Workspace/R")


library(RSQLite)
library(zoo)
library(ggplot2)
library(reshape2)

### set working dir for pc:
#setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
setwd("M:/My PhD/R/PhD-local_repo")

# remove the outliers from the Griffin SQlite: at the moment only manually by using Outliers_aug2016 to identify them and Outliers to remove them
#source("mikerspencer/Outliers.R")


db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

# ----------------------------------------------------------
#                    STEMFLOW by dbh
#-----------------------------------------------------------

Stemvol = dbGetQuery(db, "SELECT date, sample, site, vals, overflowing FROM fielddata  WHERE variable = 'stem vol' ORDER BY date")

SFNH4 = dbGetQuery(db, "SELECT date, sample, vals FROM labdata  WHERE variable = 'NH4.N' ORDER BY date")

SFNO3 = dbGetQuery(db, "SELECT date, sample, vals FROM labdata  WHERE variable = 'NO3.N' ORDER BY date")

# note provvisorie: mo devo mettere insieme vol, 3. Nx, 2.dbh per ogni sample e 1. numero giorni dall'ultima volta.

# 1. how many days from last sampling?
dates=unique(Stemvol$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
days.dates=cbind(dates,days)
SFvol=merge(Stemvol,days.dates,by.x="date",by.y="dates",na.rm=FALSE)

# 2. merge with dbh
# importing dbh classes and merge with sf
stemdbh <- read.csv("mikerspencer/stemdbh.csv", header=TRUE)
dbh.class=stemdbh[,c(1,4)]
SF.class=merge(SFvol,dbh.class, by ="sample")

# 3a SFNH4
SF.NH4 = merge(SF.class, SFNH4, by = c("sample", "date"))
SF.NH4$days = as.numeric(SF.NH4$days)
SF.NH4$vals = SF.NH4$vals.x * SF.NH4$vals.y / SF.NH4$days
SF.NH4 = SF.NH4[ , -c(4,5,7)]
SF.NH4[SF.NH4 < 0] <- 0 # turns the negative values (very low vols) into 0

# 3b SFNO3
SF.NO3 = merge(SF.class, SFNO3, by = c("sample", "date"))
SF.NO3$days = as.numeric(SF.NO3$days)
SF.NO3$vals = SF.NO3$vals.x * SF.NO3$vals.y / SF.NO3$days
SF.NO3 = SF.NO3[ , -c(4,5,7)]
SF.NO3[SF.NO3 < 0] <- 0 # turns the negative values (very low vols) into 0

#3c. SF vols
SF.class$days = as.numeric(SF.class$days)
SF.class$vals = SF.class$vals/SF.class$days
# remove OF
SF.class$overflowing[SF.class$overflowing == TRUE] = 1
SF.class.clean = SF.class[SF.class$overflowing!=1, c(1,2,3,4,7)]
# turn diameter into dbh
SF.class.clean$dbh = (SF.class.clean$mean.d/2)^2*pi
# Housekeeping
rm(days.dates, stemdbh, dbh.class, Stemvol, dates, days, diffdays, SFNH4, SFNO3, SFvol)

# ------------------------------------------------------------
#
#               STEMFLOW VOLUMES vs dbh  - PLOT
#
# ------------------------------------------------------------

# long to wide

SF.class.wide = dcast(SF.class, sample ~ date, value.var = "vals")
SF.vols.wide  = merge(SF.class.wide, dbh.class, by = "sample")

# correlation with all vol data including overflows ~= 0.007-0.0014 a seconda del metodo
# correlation without OF ~0.0006-0.018 cioe' na ciofeca!!!
cor(SF.class.clean[SF.class$site=="Treatment" , c(4,6)], use="complete.obs", method="kendall") 
cor(SF.class.clean[SF.class$site=="Control" , c(4,6)], use="complete.obs", method="kendall") # correlazioni bassissime anche con dbh e non diametro

cor(SF.class.clean[ , c(4,5)], use="complete.obs", method="pearson") 
cor(SF.class.clean[ , c(4,5)], use="complete.obs", method="spearman") 

# proviamo anche le polychoric correlations (per provare correlazione tra sample e volumi, sempre date by date pero')
library(polycor)
# cor(numeric-ordinal)
SF.class.clean$sample = as.factor(SF.class.clean$sample)
hetcor(SF.class.clean[ , c(1,4)]) 
# regression SF vols ~ dbh (without OF): horror
SF.lm = lm(vals ~ dbh, data = SF.class.clean) # r-squared = 0.00037, praticamente se vomitavo sugli assi R era piu' alto


ggplot(SF.class) + geom_point(aes(x = mean.d, y = vals, group = date, colour = date)) + geom_line(mean.d, vals, group = date, colour = date)
ggplot(data = SF.class, mapping = aes(x = mean.d, y = vals, group = date, colour = date)) + 
  geom_smooth(data = SF.class, mapping = aes(x = mean.d, y = vals, group = date, colour = date))  

# sono lungi da capire, ma i grafici e la correlazione mi fanno pensare che sto ragionando su un'accozzaglia di dati. 
# Dovrei perlomeno filtrare gli OF e portare a NA gli zeri. Spiegazioni? Bosco fitto, dipende dall'area? QC (pipe degradation?)
# ci ho pure provato con le polycorrelazioni, praticamente cercando una relazione sample - volume. Come fosse Antani
# Last try: lavorare su volumi cumulati annuali. Ma a questo punto devo tornare a Griffin daily.
#--------------------------------------------------------------
#
#                 STEMFLOW VOLUMES BY YEAR
#
# -------------------------------------------------------------

db = dbConnect(SQLite(), dbname="field_lab/Daily_Griffin.SQLite")

# select SF data from SQL db
sf = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'stem vol' AND vals>=0 ORDER BY date")

# 1. Group vols by year
library(plyr)

sf$Y=strftime(sf$date,"%Y") # creates month-Year column
sf$sample = as.factor(sf$sample)

yearly.SF = aggregate(vals ~ Y + sample,  data = sf, FUN = sum)

yearly.SF = yearly.SF[yearly.SF$Y!="2011", ]
colnames(yearly.SF)[1] = "Year"
# 2. Merge with dbh
# importing dbh classes and merge with sf
stemdbh <- read.csv("mikerspencer/stemdbh.csv", header=TRUE)
dbh.class=stemdbh[,c(1,4)]
SF.class=merge(yearly.SF,dbh.class, by ="sample")

# 3. Turning dbh into basal area BA(cm2)
SF.class$BA = (SF.class$mean.d/2)^2*pi

# Plot
ggplot(SF.class) + geom_point(aes(x = mean.d, y = vals, group = Year, colour = Year)) + 
  geom_smooth(aes(x = mean.d, y = vals, group = Year, colour = Year)) + facet_grid(Year ~ .) + labs( x = "dbh", y = expression(SF~vol~~"(l"~y^"-1"*")")) +# ylim(-10,760) +
  ggtitle ("SF volumes by dbh per year") + theme(plot.title = element_text(lineheight=.8, face="bold"))# comments! OK, la correlazione 
# fa schifo per non dir peggio, ma negli anni i volumi si confermano: gli alberi grossi (>500cm2) raccolgono meno acqua di quelli medi!

# correlations
cor(SF.class[ , c(3,4)], use="complete.obs", method="pearson") # -0.12, record of the day, ma non un bel record...
cor(SF.class[ , c(3,4)], use="complete.obs", method="spearman") 
cor(SF.class[ , c(3,4)], use="complete.obs", method="kendall") # ugly
# regression
# regression SF vols ~ dbh (without OF): horror
SF.class.lm = lm(vals ~ mean.d, data = SF.class) # r-squared = 0.00037, praticamente se vomitavo sugli assi R era piu' alto
summary(SF.class.lm) # R-squared = 0.008249

# ----------------------------------------------------------------------------- #
#
#                            THROUGHFALL by sample                              #
#
# ----------------------------------------------------------------------------- #
rm(list=ls())

# 1st round: work on originary data (cumulated data)
db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

# select SF data from SQL db
TF = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' ORDER BY date")

# 1. how many days from last sampling?
dates=unique(TF$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=26 #set first value as 26 (first sampling in 2011)
days.dates=cbind(dates,days)
TFdepth=merge(TF,days.dates,by.x="date",by.y="dates",na.rm=FALSE)
TFdepth$days = as.numeric(as.character(TFdepth$days))
TFdepth$dailyvals = TFdepth$vals/TFdepth$days # this creates a "daily value" to make different periods comparable
TFdepth = TFdepth[ , c(1,3,4,11)]

# a. plots and statistics with all values
# long to wide by C/T site
library(zoo)
TF.boxplot <- TFdepth[order(as.yearmon(TFdepth$date, '%b-%y')),]
#adding column year to facet grid
TF.boxplot$year = substring(TFdepth$date,1,4)
TF.boxplot$month = substring(TFdepth$date,6,7)

# PLOT TF VOL DATA (ALL VALUES, OF INCLUDED)
ggplot(TF.boxplot) + geom_boxplot(aes(x = month, y = dailyvals)) + facet_grid(year ~ .) + 
  labs(x = "Month", y = expression(TF~ depth~"(mm"~~sampling~period^"-1"*")")) + 
  ggtitle("Throughfall variability among samples by sampling date")



# b. plots&stats without OF values
# al prossimo giro ci staranno pure gli OF antichi

# VAGONATA DI WASTE OF TIME. DEVO COSTRUIRE UN DF INTERESSANTE PER DELLE CORRELAZIONI, CHE SO:
# DATE OF SAMPLING, MEAN TF, MEAN SF, MEAN RF, FOG COLLECTOR. A QUESTI APPLICO:

