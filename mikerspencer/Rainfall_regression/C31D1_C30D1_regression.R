###########################################################################
#                                                                         #
#      This script creates a regression between C31D1 and C30D1 to        #
#      predict fog values where either they were negative or there were   #
#      OF/QC (partial values).                                            #
# By: Daniele                               Last update: 06/12/2016       #
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
C31D1.C30D1.field = dbGetQuery(db, "SELECT date, sample, vals, overflowing, QC, comments FROM fielddata WHERE sample = 'C30D1' or sample = 'C31D1' ORDER BY date")


# Remove all rows where OF = 1: 
#http://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r

C31D1.C30D1.semicleaned <- C31D1.C30D1.field[!(C31D1.C30D1.field$QC=="1"), ]
C31D1.C30D1.cleaned <- C31D1.C30D1.semicleaned[!(C31D1.C30D1.semicleaned$overflowing=="1"), ] 


## Long to wide (http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/)

wide.RF1RF2.cleaned <- dcast(C31D1.C30D1.cleaned, date ~ sample, value.var="vals") # ready for regression

wide.RF1RF2 <- dcast(C31D1.C30D1.field, date ~ sample, value.var="vals") # ready to select values for prediction, 
# but also to check the alldata regression

##############################################################################
#######                                                                #######
#######                          REGRESSION                            #######
#######                                                                #######


# see http://www.r-tutor.com/elementary-statistics/simple-linear-regression/estimated-simple-regression-equation

# RF.b4.regression is a long format df containing Rob data (Control and Treatment) and my data (C30 and C31)


# PRIMA DI QUELLO SOTTO AVEVO ANCHE CALCOLATO UN LM CLEARED of OF ONLY

########              1 . C31D1 ~ C30D1, cleared of OF + QC + NA

# elimino i NA to be ready for lm:
wide.RF1RF2.lm= wide.RF1RF2.cleaned[complete.cases(wide.RF1RF2.cleaned),]
wide.RF1RF2.NA = wide.RF1RF2.cleaned[!complete.cases(wide.RF1RF2.cleaned),] # NA to be potentially predicted


# check best r-squared: 
RFall.lm = lm(C31D1 ~  C30D1, data = wide.RF1RF2)
summary(RFall.lm)$r.squared # 0.5718 OK, una mezza schifezza ma allora perche' quando ho fatto la prova sul file della 
# regressione coi dati di Rob mi esce un 0.72, mannaggiachitammuort????
summary(RFall.lm) #  p on 13 Dec 2016 = ***

RF.lm = lm(C31D1 ~  C30D1, data = wide.RF1RF2.lm)

summary(RF.lm)$r.squared # 0.6751 bene ma non benissimo
summary(RF.lm) #  p on November 2016 = ***, cioe' OVER THE TOP

# benone, questa mi serve per ricostruire il missing value di C31D1 il 2011-12-15

# extracting C30D1 values to be used to predict C31D1 values


newdata = as.data.frame(wide.RF1RF2.NA["4",  "C30D1"])
colnames(newdata) = "C30D1"


predicted.RFvol.int = as.data.frame(predict(RF.lm, newdata, interval="confidence"))
#    fit      lwr      upr
#  2.147388 1.880382 2.414393 I will take the upr value here, as the original outlier was 10.89, and C30D1 is 2.61, still higher than upr.

# predict.fog = as.data.frame(predict(RFfog.lm2, data.frame(TBP$C30D1))) # questo mi restituisce il solo vettore di predictions, senza intervallo di confidenza :)

predictedvol.C31D1 = cbind(as.data.frame(wide.RF1RF2.NA["4", ]), predicted.RFvol.int) # add the predicted figures to the TBP db


predictedvol.C31D1= predictedvol.C31D1[ , c("date", "upr")] # values to be substituted in Griffin.SQlite!!
predictedvol.C31D1$sample = "C31D1"

# Substitute the predicted value into the db:

fielddata = dbGetQuery(db, "SELECT date, time, sample, site, variable, vals, overflowing, QC, comments FROM fielddata ORDER BY date")

y = merge(fielddata, predictedvol.C31D1, by.x = c("date", "sample"), by.y = c("date", "sample"), all.x = TRUE, all.y = TRUE)

# http://stackoverflow.com/questions/25711530/replace-row-values-conditional-on-is-na-in-another-column:
y$vals[!is.na(y$upr)] <- y$upr[!is.na(y$upr)]
# add informations to the "new" line
y["326" , c("site", "variable", "comments")] <- c("Both", "precip depth", "predicted through C31D1_C30D1 regression")

# adjusting y to overwrite fielddata:
fielddata = y[ , c(1, 3, 2, 4, 5, 6, 7, 8, 9)]

# overwrite the fielddata table after substituting the predicted values of fog:
dbWriteTable(conn=db, name="fielddata", fielddata, overwrite = TRUE, append=F, row.names=F) #NX data added to the db
dbDisconnect(db)

  rm(C31D1.C30D1.field, C31D1.C30D1.semicleaned, C31D1.C30D1.cleaned, newdata, wide.RF1RF2,
     wide.RF1RF2.cleaned, wide.RF1RF2.lm, wide.RF1RF2.NA, RF.lm, y, fielddata, predicted.RFvol.int, predictedvol.C31D1)

############################################################################
#
#                              LABDATA
#
############################################################################

# Creating a vector with all my sampling dates - syntax available in Mike file:
db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite") # let's see the format, likely to be a .csv file

C31D1.C30D1 = dbGetQuery(db, "SELECT date, sample, variable, vals FROM labdata WHERE sample = 'C30D1' or sample = 'C31D1' ORDER BY date")


# Remove all rows where OF = 1: 
#http://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r

C31D1.C30D1.cleaned <- C31D1.C30D1[!(C31D1.C30D1$variable=="acidity"), ]
# C31D1.C30D1.NO3 <- C31D1.C30D1[C31D1.C30D1$variable=="NO3.N", ]
C31D1.C30D1.NH4 <- C31D1.C30D1[C31D1.C30D1$variable=="NH4.N", ]


## Long to wide (http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/)

# wide.prec.NO3 <- dcast(C31D1.C30D1.NO3, date ~ sample, value.var="vals") # ready for regression
wide.prec.NH4 <- dcast(C31D1.C30D1.NH4, date ~ sample, value.var="vals")



##############################################################################
#######                                                                #######
#######                    LABDATA    REGRESSION                       #######
#######                                                                #######


# see http://www.r-tutor.com/elementary-statistics/simple-linear-regression/estimated-simple-regression-equation



# No NO3 necessary as on December 2016, coz the R squared value of lm is generally too low to accept it.

########     2. NH4. C31D1 ~ C30D1, pre-cleared of "long term" outliers + fog regression
# Despite of the low R squared value this regression is gonna be used to substitute a very high C30D1 NH4 value 
# on 25-07-2016

# delete NA to be ready for lm:
wide.prec.NH4.lm = wide.prec.NH4[complete.cases(wide.prec.NH4),]
wide.prec.NH4.lm = wide.prec.NH4.lm[which(wide.prec.NH4.lm$date != "2016-07-25"), ]


predictableoes = wide.prec.NH4[which(wide.prec.NH4$date == "2016-07-25"), ]

wide.prec.NH4.lm = lm(C30D1 ~  C31D1, data = wide.prec.NH4.lm)

summary(wide.prec.NH4.lm)$r.squared # 0.348 rifiuto e vado avanti!

summary(wide.prec.NH4.lm) #  mi fermo qui, non so se vale la pena incasinarsi la vita per un valore inculato

# extracting C30D1 values to be used to predict fog values


newdata = as.data.frame(predictableoes[ , "C31D1"])
colnames(newdata) = "C31D1"

predictedNH4.RF.int = as.data.frame(predict(wide.prec.NH4.lm, newdata, interval="confidence")) # SIIIIIIIIIIIII confidence level: 95%

# predict.fog = as.data.frame(predict(RFfog.lm2, data.frame(TBP$C30D1))) # questo mi restituisce il solo vettore di predictions, senza intervallo di confidenza :)

predictedNH4.C30D1 = cbind(predictableoes, predictedNH4.RF.int) # add the predicted figures to the TBP db


predictedNH4.C30D1= predictedNH4.C30D1[ , c("date", "upr")] # I am choosing the upr value of 0.26 as the rejected lab value was 0.899
predictedNH4.C30D1$sample = "C30D1"
predictedNH4.C30D1$variable = "NH4.N"


rm(C31D1.C30D1, C31D1.C30D1.cleaned, C31D1.C30D1.NH4, newdata, predictableoes, 
   predictedNH4.RF.int, wide.prec.NH4, wide.prec.NH4.lm)

# Now substitute the predicted accepted values to my Griffin.Sqlite db:

labdata = dbGetQuery(db, "SELECT * FROM labdata ORDER BY date")

w = merge(labdata,predictedNH4.C30D1, by.x = c("date", "sample", "variable"), by.y = c("date", "sample", "variable"), all.x = TRUE, all.y = TRUE)

# http://stackoverflow.com/questions/25711530/replace-row-values-conditional-on-is-na-in-another-column:
w$vals[!is.na(w$upr)] <- w$upr[!is.na(w$upr)]

# adjusting y to overwrite fielddata:

labdata = w[ , c(1, 4, 2, 3, 5)]

# overwrite the labdata table after substituting the predicted labdata values of fog:
dbWriteTable(conn=db, name="labdata", labdata, overwrite = TRUE, append=F, row.names=F) #NX data added to the db

dbDisconnect(db)

rm(labdata, predictedNH4.C30D1, w, db)

# CONCLUSIONS: AS TO 09/12/2016 THIS REGRESSION ONLY WORKS TO CHANGE ONE VALUE ON FIELDATA AND ONE VALUE ON LABDATA.

