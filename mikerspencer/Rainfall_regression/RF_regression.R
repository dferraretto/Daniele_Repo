######################################################
#
#      Script to prepare Robert Clement data to be   #
#      comparable with my rainfall data in order to  #
#      create a regression and correct the OF data.  #
#                                                    #
######################################################

# clear the memory and set the path for libraries
rm(list=ls())
.libPaths("C:/Workspace/R")

library(RSQLite) # likely useless, to be deleted
library(zoo)

### set working dir for pc:
#setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
setwd("M:/My PhD/R/PhD-local_repo")

# Creating a vector with all my sampling dates
db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite") # let's see the format, likely to be a .csv file

sampling.dates = dbGetQuery(db, "SELECT date FROM fielddata WHERE (date BETWEEN '2012-05-24' AND '2014-09-24') ORDER BY date") 
# IN REALTA' IN QUERY DOVREI FARE QUALCOSA TIPO "AND C30D1/D2 >0, SE NO POI SOTTO CON LE DATE SBALLO DI BRUTTO
sampling.dates = unique(sampling.dates$date)


#How many days between samplings:
sd = as.POSIXlt(sampling.dates)
sd=rev(sd) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( sd[1:length(sd-1)] , sd[2:length(sd)], units = "days" )
diffdays= round(diffdays, digits=0) #correct values to the integer
diffdays=rev(diffdays) #back to the increasing order of dates
diffdays[[1]] = NA




# ho creato un vettore con le sampling dates, ora mi serve calcolarmi in qualche modo la somma delle precipitazioni 
# da una sampling date all'altra, cioe' devo mergiare questo vettore al db che mi da rob, e sommare i daily values.
# a quel punto sono pronto per la regressione.

Gfert_precip_long <- read.csv("~/My PhD/R/PhD-local_repo/Rob RF data/Gfert_precip_long.csv") # precip is on a 30 min base

Gfert_precip_long$Date.Time = as.Date(Gfert_precip_long$Date.Time, format = "%d/%m/%Y") # preparing date-time to be aggregated

daily.Gfert_precip_long = aggregate(precip ~ Date.Time + Plot, data = Gfert_precip_long, FUN=sum, na.rm=TRUE) # aggregate as daily precip


#1. MERGE GRIFFIN DATES AND ROB DAILY DATAFRAME (to later aggregate rob's data by Griffin's)
s.d = data.frame(sampling.dates)
s.d$sampling.dates=as.Date(s.d$sampling.dates) # non serve format, perche' , "%Y/%m/%d" e' il formato di default
s.d$key=s.d$sampling.dates # creating the key i.e. the sampling date to be compared with Rob data

# Creating Rob subfiles C and sampling.dates.C
control= daily.Gfert_precip_long[daily.Gfert_precip_long$Plot=="control" ,]
sampling.dates.C=s.d[s.d$sampling.dates<"2014-11-13",]

x = merge(control, sampling.dates.C, by.x="Date.Time", by.y="sampling.dates", all=TRUE, na.rm=FALSE)
# na.locf C plot
x$key = na.locf(x$key,fromLast = TRUE)
# now resta da pulire il file ed e' pronto per aggregate by key

# Creating Rob subfiles T and sampling.dates.T
treatment= daily.Gfert_precip_long[daily.Gfert_precip_long$Plot=="treatment",]

sampling.dates.T=s.d[s.d$sampling.dates>"2012-08-22",]

y = merge(treatment, sampling.dates.T, by.x="Date.Time", by.y="sampling.dates", all=TRUE, na.rm=FALSE)
# na.locf C plot
y$key = na.locf(y$key,fromLast = TRUE)

z=rbind(x,y)

Rob.RF=aggregate(precip ~ Plot+key, data = z, FUN = sum, na.rm = TRUE)

# housekeeping
rm(control, daily.Gfert_precip_long, Gfert_precip_long, s.d, sampling.dates.C, sampling.dates.T, treatment, x, y, z, sd)

############## import Griffin RF data and preparing for binding

Griffin.RF = dbGetQuery(db, "SELECT date, sample, vals FROM fielddata WHERE (sample = 'C30D1' or sample = 'C31D1') AND (date BETWEEN '2012-05-26' AND '2014-10-31')  ORDER BY date")

colnames(Griffin.RF) = c("key","Plot","precip")

sampling.dates = data.frame(sampling.dates,diffdays) # this will let to get the originary cumulative precip value
sampling.dates = sampling.dates[ -1, ]

G.RF = merge(Griffin.RF, sampling.dates, by.x = "key", by.y = "sampling.dates", all=T, na.rm=F)

G.RF$precip=G.RF$precip*G.RF$diffdays # calculates the sampling cumulated RF

G.RF$key=as.Date(G.RF$key)
G.RF= G.RF[, -4]

dbDisconnect(db)

RF.b4.regression = rbind(G.RF, Rob.RF)
RF.b4.regression$key=as.Date(RF.b4.regression$key)
RF.b4.regression$precip = as.numeric(RF.b4.regression$precip)
summary(RF.b4.regression)

RF.b4.regression = na.omit(RF.b4.regression)

rm(db, diffdays,  G.RF, Griffin.RF, Rob.RF, sampling.dates)


# plot (as in Checking RF data, solo che dovro' adattarlo al format dei vari dati, anzi no, key e' gia' formato "Date")
library(ggplot2)


ggplot(data = RF.b4.regression, aes(x=key, y = precip, colour = Plot, group = Plot),  alpha = 0.4) + geom_line() +
  labs(x = "Date", y = expression(precip~ depth~"(mm"~~sampling~period^"-1"*")")) + theme(
    panel.grid.major = element_line(colour = "grey80"),
    #panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_color_manual(aes(colour = Plot), values = c("control" = "blue", "treatment" = "cornflowerblue", "C30D1" = "chocolate4", "C31D1" = "chocolate1")) +
  ggtitle("Precipitation data from the Treatment and \n Control towers compared to my data")

ggplot(yt.views, aes(Date, Views)) + geom_line()

# plot OK, ora devo aggiungere gli OF per capire dove ho bisogno di correggere

##############################################################################
#######                                                                #######
#######                          REGRESSION                            #######
#######                                                                #######

# see http://www.r-tutor.com/elementary-statistics/simple-linear-regression/estimated-simple-regression-equation

# RF.b4.regression is a long format df containing Rob data (Control and Treatment) and my data (C30 and C31)
# which regressions? all values, values by area, other? ...


library(reshape2)


# Extracting OF values and add them to my long df

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

rf = dbGetQuery(db, "SELECT * FROM fielddata WHERE sample = 'C30D1' or sample = 'C31D1'   ORDER BY date")

rf$overflowing = replace(rf$overflowing, c(7,8,18,40,56,63,70,79,100,101), 1) # this is to "correct the rf information on OF. Needs to be added to the Griffin.SQlite straight

rf.OF= rf[, c("date", "sample", "overflowing")]

rf.OF$date = as.Date(rf.OF$date) # this is a date-Of db to be merged with rf and TF

rf.OF$overflowing = as.numeric(rf.OF$overflowing)

names(rf.OF) = c("key", "Plot", "overflowing")
# replace 0 with NA: (http://stackoverflow.com/questions/11036989/replace-all-0-values-to-na)
rf.OF[rf.OF == 0] <- NA


# 1. Rob values & C31 vs. C30
# long file: RF.b4.regression

# http://stackoverflow.com/questions/22754760/merging-in-r-keeping-all-rows-of-a-data-set

Rf.long = merge(RF.b4.regression, rf.OF, by = c("key", "Plot"), all = T)

# SUBSET OF: it has a range 2011-2016. Here I need a range May 2012 - Sep 2014
# http://stackoverflow.com/questions/14471640/r-subset-by-date : senza as.Date non se ne dava fuori!!!

Rf.long <- subset(Rf.long, key > as.Date("2012-05-25") & key <= as.Date("2014-09-23"))

# fino a qui OK, Rf.long di 89 rows e 4  variabili



# Remove all rows where OF = 1: 
#http://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r
Rf.long$overflowing[is.na(Rf.long$overflowing)] <- 3 # this bcoz NA cause crezinezz

Rf.long <- Rf.long[!(Rf.long$overflowing=="1"), c("key", "Plot", "precip")]



## Long to wide (http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/)

wide.RF <- dcast(Rf.long, key ~ Plot, value.var="precip")

#  1. C + T + C30 ~ C31

RF.lm1 = lm(control + treatment + C30D1 ~  C31D1, data = wide.RF)

summary(RF.lm1)$r.squared # 0.7308 ON 5/12 AND on 13/12!, on Griffin SQLite WITHOUT outliers BEFORE any adjustment through other regressions!
summary(RF.lm1) # good!, credo: p = 0.003431!, cioe' 0.01. BUT the point is that it is not thanks to Rob's data! In fact
# C30D1 ~  C31D1 gives an R-squared value of 0.7406!, better than adding Rob's data!

# confidence interval for predicted values:
newdata = data.frame(var.to.predict=80) 
predict(allRF.lm, newdata, interval="confidence") 

#  2. C + T + C31 ~ C30

RF.lm2 = lm(control + treatment + C31D1 ~  C30D1, data = wide.RF)

summary(RF.lm2)$r.squared # 0.57 confirmed on 13/12

summary(RF.lm2) # ** 0.01

# confidence interval for predicted values:
newdata = data.frame(var.to.predict=80) 
predict(allRF.lm, newdata, interval="confidence") 

#  3.  T ~ C30

RF.lm3 = lm(treatment  ~  C30D1, data = wide.RF)

summary(RF.lm3)$r.squared # 0.3226 suicidal

summary(RF.lm3) # * p = 0.02808, i.e. 0.05

# confidence interval for predicted values:
newdata = data.frame(var.to.predict=80) 
predict(allRF.lm, newdata, interval="confidence") 

#  4.  c ~ C31

RF.lm4 = lm(control  ~  C31D1, data = wide.RF)

summary(RF.lm4)$r.squared # 0.455 

summary(RF.lm4) # 2nd best: p = 0.006935, cioe' 0.01

# confidence interval for predicted values:
newdata = data.frame(var.to.predict=80) 
predict(allRF.lm, newdata, interval="confidence") 

#  5. C+T ~ C30+C31

RF.lm5 = lm(control + treatment  ~  C30D1 + C31D1, data = wide.RF)

summary(RF.lm5)$r.squared # 0.5791, boh... I mean, why picking this if I have a good 0.74 from C30D1 ~  C31D1?

summary(RF.lm5) # 2nd best: p = 0.02, cioe' 0.01

#  6. C30 ~ C31 (to compare to the above values)

RF.lm6 = lm(C30D1 ~ C31D1, data = wide.RF)

summary(RF.lm6)$r.squared # 0.5791, boh... I mean, why picking this if I have a good 0.74 from C30D1 ~  C31D1?

summary(RF.lm6) # 2nd best: p = 0.02, cioe' 0.01

########################################################################
##### Regressione coi dati completi catafottendosene dei OF, per capire:

wide.RF.tot = dcast(RF.b4.regression, key ~ Plot, value.var="precip")

#  1. C + T + C30 ~ C31

RF.lm1 = lm(control + treatment + C30D1 ~  C31D1, data = wide.RF.tot)

summary(RF.lm1)$r.squared # 0.41

summary(RF.lm1) 

# confidence interval for predicted values:
newdata = data.frame(var.to.predict=80) 
predict(allRF.lm, newdata, interval="confidence") 

#  2. C + T + C31 ~ C30

RF.lm2 = lm(control + treatment + C31D1 ~  C30D1, data = wide.RF.tot)

summary(RF.lm2)$r.squared # 0.45, vs. 07079 if only C31D1 ~  C30D1 is considered

summary(RF.lm2) 


#  3.  T ~ C30

RF.lm3 = lm(treatment  ~  C30D1, data = wide.RF.tot)

summary(RF.lm3)$r.squared # 0.2497
summary(RF.lm3) 


#  4.  c ~ C31

RF.lm4 = lm(control  ~  C31D1, data = wide.RF.tot)

summary(RF.lm4)$r.squared # 0.1445 AHAHAHAHAHAHAH

summary(RF.lm4) 

#  5. C+T ~ C30+C31

RF.lm5 = lm(control + treatment  ~  C30D1 + C31D1, data = wide.RF.tot)

summary(RF.lm5)$r.squared # 0.2107 Ciaone
summary(RF.lm5) 

# NOTE: it is likely that the change of R values in different dates was due to the Griffin.SQLite version I was using
# (raw, without otliers, fully "treated" after the other regressions)