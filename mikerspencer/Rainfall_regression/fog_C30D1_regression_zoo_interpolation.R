###########################################################################
#                                                                         #
#      This script creates a regression between fog data and C30D1 to     #
#      predict fog values where either they were negative or there were   #
#      OF/QC (partial values).                                            #
# By: Daniele                               Last update: 20/11/2016       #
###########################################################################

# clear the memory and set the path for libraries
rm(list=ls())
.libPaths("C:/Workspace/R")

library(RSQLite) # likely useless, to be deleted
library(zoo)
library(reshape2)
### set working dir for pc:
#setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
setwd("M:/My PhD/R/PhD-local_repo")
############################################################################
#
#                              FIELDDATA
#
############################################################################
# Creating a vector with all my sampling dates
db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite") # let's see the format, likely to be a .csv file
# syntax available in Mike file:
fog.C30D1 = dbGetQuery(db, "SELECT date, sample, vals, overflowing, QC, comments FROM fielddata WHERE sample = 'C30D1' or sample = 'C30D2' ORDER BY date")


# Remove all rows where OF = 1: 
#http://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r

fog.C30D1.semicleaned <- fog.C30D1[!(fog.C30D1$QC=="1"), ]
fog.C30D1.cleaned <- fog.C30D1.semicleaned[!(fog.C30D1.semicleaned$overflowing=="1"), ] 


## Long to wide (http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/)

wide.RFfog.cleaned <- dcast(fog.C30D1.cleaned, date ~ sample, value.var="vals") # ready for regression

wide.RFfog <- dcast(fog.C30D1, date ~ sample, value.var="vals") # ready to select values for prediction


##############################################################################
#######                                                                #######
#######                          REGRESSION                            #######
#######                                                                #######


# see http://www.r-tutor.com/elementary-statistics/simple-linear-regression/estimated-simple-regression-equation

# RF.b4.regression is a long format df containing Rob data (Control and Treatment) and my data (C30 and C31)
# which regressions? all values, values by area, other? ...


# PRIMA DI QUELLO SOTTO AVEVO ANCHE CALCOLATO UN LM CLEARED of OF ONLY

########              1 . C30D2 ~ C30D1, cleared of OF + QC + (D2-D1<0)

# elimino i fog<RF to be ready for lm:
wide.RFfog.cleaned$diff= wide.RFfog.cleaned$C30D2 - wide.RFfog.cleaned$C30D1 # this checks if there were any (fog vol < RF) left
wide.RFfog.lm <- wide.RFfog.cleaned[(wide.RFfog.cleaned$diff>"0"), ]

# extracting (the first 3) negative values to be predicted
wide.fog.neg = wide.RFfog.cleaned[(wide.RFfog.cleaned$diff<"0"), ]
wide.fog.neg = na.omit(wide.fog.neg) 



RFfog.lm = lm( C30D2 ~  C30D1, data = wide.RFfog.lm)

summary(RFfog.lm)$r.squared # reactivate when rerunning this script with new data - R squared on November 2016 = 0.8101, on 6/12: 0.7962. mah.
summary(RFfog.lm) #  p on November 2016 = ***, cioe' OVER THE TOP


# NOW PREDICT!    1. WHICH C30D2 VALUES? SELECT OF+QCs (it should work with any values within the min max C30D1 values)
                # 1a RF.fog completo


fog.all = dbGetQuery(db, "SELECT date, sample, vals, overflowing, QC, comments FROM fielddata WHERE sample = 'C30D2' ORDER BY date")

# selecting OF and QC to be predicted
fog.not =  fog.all[ which(fog.all$overflowing =='1' | fog.all$QC =='1'), ]

# selecting vector dates from OF, QC & neg values (wide.fog.neg)
fog.not = fog.not[, "date"]
wide.fog.neg = wide.fog.neg[ , "date"]
# new try (1/12): di prediction dates prendo solo quelle in cui C30D1>C30D2
prediction.dates = c(fog.not, wide.fog.neg) # one vector from OF+QC & "negatives" vector
prediction.dates = unique(prediction.dates) # this removes duplicates, if any (not at November 2016)

# extracting C30D1 values to be used to predict fog values
TBP <- wide.RFfog[wide.RFfog$date %in% prediction.dates, ]

TBP= na.omit(TBP) # remove NA rows that shouldn't be there


# predict C30D2 on a 95% prediction interval


# confidence interval for predicted values:


newdata = as.data.frame(TBP[ , "C30D1"])
colnames(newdata) = "C30D1"

predicted.fog.int = as.data.frame(predict(RFfog.lm, newdata, interval="confidence")) # SIIIIIIIIIIIII confidence level: 95%

# predict.fog = as.data.frame(predict(RFfog.lm2, data.frame(TBP$C30D1))) # questo mi restituisce il solo vettore di predictions, senza intervallo di confidenza :)

predicted.fog = cbind( TBP, predicted.fog.int) # add the predicted figures to the TBP db

# Some of the calculated values are lower than the real values! To be rejected:
predicted.fog$checkfit =  predicted.fog$fit- predicted.fog$C30D1
predicted.fog$checklwr = predicted.fog$lwr- predicted.fog$C30D1
predicted.fog$checkrealval = predicted.fog$C30D2- predicted.fog$C30D1

# adjust 07/2016 by picking the upr value
#predicted.fog[["44","fit"]] = predicted.fog[["44","upr"]] # 6/12/16: all lwr


rm(fog.C30D1, fog.C30D1.semicleaned, fog.C30D1.cleaned, newdata, predicted.fog.int, TBP, 
   wide.RFfog.cleaned, wide.RFfog.lm, fog.all, db, fog.not, prediction.dates, RFfog.lm, wide.fog.neg, wide.RFfog)

# Now substitute the predicted accepted values to my Griffin.Sqlite db:

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

fielddata = dbGetQuery(db, "SELECT date, time, sample, site, variable, vals, overflowing, QC, comments FROM fielddata ORDER BY date")

# Prepare the dataframe for the substitution
predicted.fog = predicted.fog[ , c("date", "lwr")] # values to be substituted in Griffin.SQlite!!
predicted.fog$sample = "C30D2"

y = merge(fielddata, predicted.fog, by.x = c("date", "sample"), by.y = c("date", "sample"), all.x = TRUE, all.y = TRUE)

# http://stackoverflow.com/questions/25711530/replace-row-values-conditional-on-is-na-in-another-column:
y$vals[!is.na(y$lwr)] <- y$lwr[!is.na(y$lwr)]

# adjusting y to overwrite fielddata:

fielddata = y[ , c(1, 3, 2, 4, 5, 6, 7, 8, 9)]

# overwrite the fielddata table after substituting the predicted values of fog:
dbWriteTable(conn=db, name="fielddata", fielddata, overwrite = TRUE, append=F, row.names=F) #NX data added to the db
dbDisconnect(db)

rm(fielddata, predicted.fog, y)

############################################################################
#
#                              LABDATA
#
############################################################################

# Creating a vector with all my sampling dates - syntax available in Mike file:
db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite") # let's see the format, likely to be a .csv file

fog.C30D1 = dbGetQuery(db, "SELECT date, sample, variable, vals FROM labdata WHERE sample = 'C30D1' or sample = 'C30D2' ORDER BY date")


# Remove all rows where OF = 1: 
#http://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r

fog.C30D1.cleaned <- fog.C30D1[!(fog.C30D1$variable=="acidity"), ]
fog.C30D1.NO3 <- fog.C30D1[fog.C30D1$variable=="NO3.N", ]
fog.C30D1.NH4 <- fog.C30D1[fog.C30D1$variable=="NH4.N", ]


## Long to wide (http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/)

wide.prec.NO3 <- dcast(fog.C30D1.NO3, date ~ sample, value.var="vals") # ready for regression
wide.prec.NH4 <- dcast(fog.C30D1.NH4, date ~ sample, value.var="vals")



##############################################################################
#######                                                                #######
#######                    LABDATA    REGRESSION                       #######
#######                                                                #######


# see http://www.r-tutor.com/elementary-statistics/simple-linear-regression/estimated-simple-regression-equation



########           1. NO3. C30D2 ~ C30D1, precleared of "long term" outliers

# elimino i fog<RF to be ready for lm:
wide.prec.NO3$diff= wide.prec.NO3$C30D2 - wide.prec.NO3$C30D1
wide.prNO3.lm <- na.omit(wide.prec.NO3[wide.prec.NO3$diff>"0", ])

# extracting fog values to be predicted
predictNO3 =  na.omit(wide.prec.NO3[wide.prec.NO3$diff<"0", ])



RFfog.lm = lm( C30D2 ~  C30D1, data = wide.prNO3.lm)

summary(RFfog.lm)$r.squared # reactivate when rerunning this script with new data - R squared on November 2016 = 0.8331646, better than fielddata :)

summary(RFfog.lm) #  p on November 2016 = ***, cioe' OVER THE TOP

# extracting C30D1 values to be used to predict fog values


newdata = as.data.frame(predictNO3[ , "C30D1"])
colnames(newdata) = "C30D1"

predictedNO3.fog.int = as.data.frame(predict(RFfog.lm, newdata, interval="confidence")) # SIIIIIIIIIIIII confidence level: 95%

# predict.fog = as.data.frame(predict(RFfog.lm2, data.frame(TBP$C30D1))) # questo mi restituisce il solo vettore di predictions, senza intervallo di confidenza :)

predictedNO3.fog = cbind(predictNO3, predictedNO3.fog.int) # add the predicted figures to the TBP db

# Check if lwr value is enough to make differences to turn positive:
predictedNO3.fog$check =  predictedNO3.fog$lwr- predictedNO3.fog$C30D1
# adjust 17/12/2015 by picking the fit value
predictedNO3.fog[["51","lwr"]] = predictedNO3.fog[["51","fit"]]
# Prepare the dataframe for the substitution
predictedNO3.fog = predictedNO3.fog[ , c("date", "lwr")] # values to be substituted in Griffin.SQlite!!
predictedNO3.fog$sample = "C30D2"
predictedNO3.fog$variable = "NO3.N"


rm(fog.C30D1, fog.C30D1.cleaned, fog.C30D1.NO3, newdata, predictedNO3.fog.int, predictNO3, 
   wide.prec.NO3, wide.prNO3.lm, db, RFfog.lm)

########           1. NH4. C30D2 ~ C30D1, precleared of "long term" outliers

# elimino i fog<RF to be ready for lm:
wide.prec.NH4$diff= wide.prec.NH4$C30D2 - wide.prec.NH4$C30D1
wide.prNH4.lm <- na.omit(wide.prec.NH4[wide.prec.NH4$diff>"0", ])

# extracting fog values to be predicted
predictNH4 =  na.omit(wide.prec.NH4[wide.prec.NH4$diff<"0", ])



RFfog.lm = lm( C30D2 ~  C30D1, data = wide.prNH4.lm)

summary(RFfog.lm)$r.squared # reactivate when rerunning this script with new data - R squared on November 2016 = 0.8331646, better than fielddata :)

summary(RFfog.lm) #  p on November 2016 = ***, cioe' OVER THE TOP

# extracting C30D1 values to be used to predict fog values


newdata = as.data.frame(predictNH4[ , "C30D1"])
colnames(newdata) = "C30D1"

predictedNH4.fog.int = as.data.frame(predict(RFfog.lm, newdata, interval="confidence")) # SIIIIIIIIIIIII confidence level: 95%

# predict.fog = as.data.frame(predict(RFfog.lm2, data.frame(TBP$C30D1))) # questo mi restituisce il solo vettore di predictions, senza intervallo di confidenza :)

predictedNH4.fog = cbind(predictNH4, predictedNH4.fog.int) # add the predicted figures to the TBP db

# Check if lwr value is enough to make differences to turn positive:
predictedNH4.fog$check =  predictedNH4.fog$lwr- predictedNH4.fog$C30D1
# adjust 17/12/2015, 15/11/2012 and 19/08/2016 by picking the fit value (try to turn this into an automatic check!)
predictedNH4.fog[c("51","59","16"),"lwr"] = predictedNH4.fog[c("51","59","16"),"fit"]
# Prepare the dataframe for the substitution
predictedNH4.fog = predictedNH4.fog[ , c("date", "lwr")] # values to be substituted in Griffin.SQlite!!
predictedNH4.fog$sample = "C30D2"
predictedNH4.fog$variable = "NH4.N"


rm(fog.C30D1.NH4, newdata, predictedNH4.fog.int, predictNH4, 
   wide.prec.NH4, wide.prNH4.lm, RFfog.lm)


# Now substitute the predicted accepted values to my Griffin.Sqlite db:

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

labdata = dbGetQuery(db, "SELECT * FROM labdata ORDER BY date")

predicted.fog = rbind(predictedNO3.fog, predictedNH4.fog)

w = merge(labdata, predicted.fog, by.x = c("date", "sample", "variable"), by.y = c("date", "sample", "variable"), all.x = TRUE, all.y = TRUE)

# http://stackoverflow.com/questions/25711530/replace-row-values-conditional-on-is-na-in-another-column:
w$vals[!is.na(w$lwr)] <- w$lwr[!is.na(w$lwr)]

# adjusting y to overwrite fielddata:

labdata = w[ , c(1, 4, 2, 3, 5)]

# overwrite the labdata table after substituting the predicted labdata values of fog:
dbWriteTable(conn=db, name="labdata", labdata, overwrite = TRUE, append=F, row.names=F) #NX data added to the db
dbDisconnect(db)

rm(db, labdata, predicted.fog, predictedNH4.fog, predictedNO3.fog, w)

#-------------------------------------------------------------------------------
##############     interpolation for missing labdata      ################
# Note: this will be applied to NO3 on RF and fog only for the date 2015-09-??
#-------------------------------------------------------------------------------

# Creating a df with all my sampling dates
db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite") # let's see the format, likely to be a .csv file
# syntax available in Mike file:
labdata = dbGetQuery(db, "SELECT * FROM labdata WHERE sample = 'C30D1' or sample = 'C30D2' or sample = 'C31D1' ORDER BY date")

labdata = labdata[labdata$variable!="acidity", ]

wide.labdata =  dcast(labdata, date ~ sample + variable, value.var="vals")

dates=unique(wide.labdata$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=NA #set first value as NA (che chissefrega)
widelabdata.days=cbind(wide.labdata,days)
wide.labdata = widelabdata.days[46:50, c(1,3,5,7,8) ]

wide.labdata["48", c(2:4)] = (wide.labdata["47", c(2:4)]*wide.labdata[["47","days"]] + wide.labdata["49", c(2:4)]*wide.labdata[["49","days"]]) / (wide.labdata[["47","days"]] +wide.labdata[["49","days"]])

wide.labdata = wide.labdata["48", 1:4]
interpol.labdata = melt(wide.labdata, id.vars = "date")
levels(interpol.labdata$variable) = c("C30D1", "C30D2", "C31D1")
interpol.labdata$sample = "NO3.N"
colnames(interpol.labdata) = c("date", "sample", "value", "variable")

labdata = dbGetQuery(db, "SELECT * FROM labdata  ORDER BY date")

w = merge(labdata, interpol.labdata, by.x = c("date", "sample", "variable"), by.y = c("date", "sample", "variable"), all.x = TRUE, all.y = TRUE)

# http://stackoverflow.com/questions/25711530/replace-row-values-conditional-on-is-na-in-another-column:
w$vals[!is.na(w$value)] <- w$value[!is.na(w$value)]

labdata = w[ , c(1, 4, 2, 3, 5)]

# overwrite the labdata table after substituting the predicted labdata values of fog:
dbWriteTable(conn=db, name="labdata", labdata, overwrite = TRUE, append=F, row.names=F) #NX data added to the db
dbDisconnect(db)

rm(db, labdata, dates, days, diffdays, interpol.labdata, w, wide.labdata, widedata.days, widelabdata.days)

