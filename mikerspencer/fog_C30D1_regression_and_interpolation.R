###########################################################################
#                                                                         #
#      This script creates a regression between fog data and C30D1 to     #
#      predict fog values where either they were negative or there were   #
#      OF/QC (partial values) AND interpolates missing values on 2 dates  #
#                                                                         #
#     By: Daniele F.                        Last update: 10/04/2017       #
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
setwd("C:/Users/s1373890/Daniele_Repo")
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


########              1 . C30D2 ~ C30D1, cleared of OF + QC + (D2-D1<0)

# cropping fog<RF to be ready for lm:
wide.RFfog.cleaned$diff= wide.RFfog.cleaned$C30D2 - wide.RFfog.cleaned$C30D1 # this checks if there were any (fog vol < RF) left - 5 on 04/03/17
wide.RFfog.lm <- wide.RFfog.cleaned[(wide.RFfog.cleaned$diff>"0"), ]


RFfog.lm = lm( C30D2 ~  C30D1, data = wide.RFfog.lm)

summary(RFfog.lm)$r.squared # reactivate when rerunning this script with new data - R squared on November 2016 = 0.8101, on 03/04/17: 0.7931. mah.
summary(RFfog.lm) #  p on November 2016 = ***, cioe' OVER THE TOP


# NOW PREDICT!    1. WHICH C30D2 VALUES? SELECT OF+QCs (it should work with any values within the min max C30D1 values)
                # 1a RF.fog completo


# selecting negative values to be predicted

wide.RFfog$diff= wide.RFfog$C30D2 - wide.RFfog$C30D1 # this show when (fog vol < RF) - 9 on 04/03/17
fog.neg <- wide.RFfog[(wide.RFfog$diff<"0"), ]
fog.neg = na.omit(fog.neg) # omitting NA (didn't work)

# extracting dates vector from fog.neg

prediction.dates = fog.neg[, "date"]

# extracting C30D1 values to be used to predict fog values
TBP <- wide.RFfog[wide.RFfog$date %in% prediction.dates, ]

TBP= na.omit(TBP) # remove NA rows that shouldn't be there


newdata = as.data.frame(TBP[ , "C30D1"])
colnames(newdata) = "C30D1"

predicted.fog.int = as.data.frame(predict(RFfog.lm, newdata, interval="confidence")) # confidence level: 95%

predicted.fog = cbind( TBP, predicted.fog.int) # add the predicted figures to the TBP db

#None of the calculated values are lower than the real values! Anyway, good to check how lwr-calc-upr vals fit the substitution
# adjust 07/2016 by picking the upr value
#predicted.fog[["44","fit"]] = predicted.fog[["44","upr"]] 
# 04/03/17: ALL LWR


rm(fog.C30D1, fog.C30D1.semicleaned, fog.C30D1.cleaned, fog.neg, newdata, predicted.fog.int, TBP, 
   wide.RFfog.cleaned, wide.RFfog.lm, db, prediction.dates, RFfog.lm, wide.RFfog)

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

rm(fielddata, predicted.fog, y, db)

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

fog.C30D1.cleaned <- fog.C30D1[!(fog.C30D1$variable =="acidity"), ]
fog.C30D1.NO3 <- fog.C30D1[fog.C30D1$variable =="NO3.N", ]
fog.C30D1.NH4 <- fog.C30D1[fog.C30D1$variable =="NH4.N", ]


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
#NOTE: if the difference between fog and C30 prec is >0.1 I am not adjusting those slight differences that might depend on
# (lab/minimal contaminations) systematic errors. They need to be verified, or I would create a false input peak.
# 17/12/2015: C30D1>>C31D1>C30D2. I will substitute C30D1 with C31D1 and then proceed to calculate the new fit value.
# 2014-08-21: C30D1>>C31D1>C30D2. However, I will not push this substitution thing, as the difference C31-C30 is reasonable compared to the historical 
# 24/07/2014: 
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

summary(RFfog.lm)$r.squared # reactivate when rerunning this script with new data - R squared on April 2017 = 0.8117, always very good

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

####### ORA PROVO A FARLA GIRARE, SOVRASCRIVERE E VEDERE COSA SUCCEDE CON LE DATE, SE POSSO LAVORARE SU C30D1 IN UN SECONDO MOMENTO

# rm(fog.C30D1.NH4, newdata, predictedNH4.fog.int, predictNH4, 
   #wide.prec.NH4, wide.prNH4.lm, RFfog.lm)


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

# NOTE on 10/03/2017: THIS HAS BEEN RERUN AFTER THE OUTLIERS CHECK WHERE C30D1-C31D1 DIFFERED OF OVER 0.75. I HAVE A LAST 
# REASONABLE DOUBT FOR THE DATE OF APRIL 2014 WHERE THE FOG VALUE IS MUCH SMALLER THAN A HIGH C30 VALUE, BUT IT IS LIKELY TO BE DUE TO 
# A HIGH DEP EVENT WHERE C30 AND C31 VALS WERE QUITE CLOSE.

###########################################################################
#                                                                         #
#      This script creates a regression between fog data and C30D1 to     #
#      predict C30D1 WHERE C30D1 IS NOT AVAILABLE, JULY 2017 ESPECIALLY.  #
#                                                                         #
#     By: Daniele F.                        Last update: 10/04/2017       #
###########################################################################

# Creating a vector with all my sampling dates - syntax available in Mike file:
db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite") # let's see the format, likely to be a .csv file

fog.C30D1 = dbGetQuery(db, "SELECT date, sample, variable, vals FROM labdata WHERE sample = 'C30D1' or sample = 'C30D2' ORDER BY date")


# Remove all rows where OF = 1: 
#http://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r

fog.C30D1.cleaned <- fog.C30D1[!(fog.C30D1$variable =="acidity"), ]

fog.C30D1.NH4 <- fog.C30D1[fog.C30D1$variable =="NH4.N", ]


## Long to wide (http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/)

wide.prec.NH4 <- dcast(fog.C30D1.NH4, date ~ sample, value.var="vals")  # ready for regression




########           1. NH4. C30D1 ~ C30D2, precleared of "long term" outliers

# elimino i fog<RF to be ready for lm:
wide.prec.NH4$diff= wide.prec.NH4$C30D2 - wide.prec.NH4$C30D1
#wide.prNH4.lm <- na.omit(wide.prec.NH4[wide.prec.NH4$diff>"0", ]) # non voglio questo perche' cazzo mi sega proprio luglio 2017

# extracting fog values to be predicted
#predictNH4 =  na.omit(wide.prec.NH4[wide.prec.NH4$diff<"0", ])


RFfog.lm = lm(C30D1 ~  C30D2, data = wide.prec.NH4)

summary(RFfog.lm)$r.squared # reactivate when rerunning this script with new data - R squared on August 2017 = 0.8696, always very good

summary(RFfog.lm) #  p on November 2016 = ***, cioe' OVER THE TOP

# extracting fog values to be used to predict C30D1 values


newdata = as.data.frame(wide.prec.NH4[ , "C30D2"]) #QUESTO AL MOM NON FUNZIONA MA ME NE INCULO E POI MI TIRO FUORI IL VAL PUNTUALE DI LUGLIO17 SE FUNZIA
colnames(newdata) = "C30D2"

predictedNH4.c30D1.int = as.data.frame(predict(RFfog.lm, newdata, interval="confidence")) # SIIIIIIIIIIIII confidence level: 95%

# predict.fog = as.data.frame(predict(RFfog.lm2, data.frame(TBP$C30D1))) # questo mi restituisce il solo vettore di predictions, senza intervallo di confidenza :)

predictedNH4.C30D1 = cbind(wide.prec.NH4, predictedNH4.c30D1.int) # add the predicted figures to the TBP db

# Check if lwr value is enough to make differences to turn positive:
predictedNH4.C30D1$check =  predictedNH4.C30D1$C30D2 - predictedNH4.C30D1$lwr # YO!

# ARRIVATO QUI E VA CAPITO COSA HO FATTO SOTTO - io non mi farei troppe pippe, si piglia l'unico valore ottenuto utile e lo si sostituise nel db. ciao.


# Prepare the dataframe for the substitution
predictedNH4.C30D1= predictedNH4.C30D1[ , c("date", "lwr")] # values to be substituted in Griffin.SQlite!!
predictedNH4.C30D1$sample = "C30D1"
predictedNH4.C30D1$variable = "NH4.N"



# Now substitute the predicted accepted values to my Griffin.Sqlite db:

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

labdata = dbGetQuery(db, "SELECT * FROM labdata ORDER BY date")


# OCCHISSIMO QUI ALLA STRUTTURA DI 
w = merge(labdata, predictedNH4.C30D1, by.x = c("date", "sample", "variable"), by.y = c("date", "sample", "variable"), all.x = TRUE, all.y = TRUE)

# https://stackoverflow.com/questions/15629885/replace-na-in-column-with-value-in-adjacent-column
w$vals[is.na(w$vals)] <- w$lwr[is.na(w$vals)]

# adjusting y to overwrite fielddata:

labdata = w[ , c(1, 4, 2, 3, 5)]

# overwrite the labdata table after substituting the predicted labdata values of fog:
dbWriteTable(conn=db, name="labdata", labdata, overwrite = TRUE, append=F, row.names=F) #NX data added to the db
dbDisconnect(db)

rm(list = ls())
#-------------------------------------------------------------------------------
#
###                 INTERPOLATION   for missing labdata               ########
#      Note: this will be applied to NO3 on RF and fog only for 
#      the dateS 2015-09-24 and February 2015
#-------------------------------------------------------------------------------

# Creating a df with all my sampling dates
db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite") # let's see the format, likely to be a .csv file
# syntax available in Mike file:
labdata = dbGetQuery(db, "SELECT * FROM labdata WHERE sample = 'C30D1' or sample = 'C30D2' or sample = 'C31D1' ORDER BY date")

labdata = labdata[labdata$variable!="acidity", ]

wide.labdata =  dcast(labdata, date ~ sample + variable, value.var="vals") # wide format

dates=unique(wide.labdata$date)
days = as.POSIXlt(dates)
days=rev(days) # reverse the order of dates to obtain positive values from differences
diffdays = difftime( days[1:length(days-1)] , days[2:length(days)] )
diffdays= round(diffdays, digits=0) #correct values to the integer
days=rev(diffdays) #back to the increasing order of dates
days[[1]]=NA #set first value as NA (che chissefrega)
widelabdata.days=cbind(wide.labdata,days)
wide.labdata = widelabdata.days[c(39:43, 46:50), c(1:5,8) ]
# interpolating JUST the NO3 vals for 2015-09-24:
wide.labdata["48", c(3,5)] = (wide.labdata["47", c(3,5)]*wide.labdata[["47","days"]] + wide.labdata["49", c(3,5)]*wide.labdata[["49","days"]]) / (wide.labdata[["47","days"]] + wide.labdata[["49","days"]])
# interpolating all vals for 2015-02-23:
wide.labdata["41", c(2:5)] = (wide.labdata["40", c(2:5)]*wide.labdata[["40","days"]] + wide.labdata["42", c(2:5)]*wide.labdata[["42","days"]]) / (wide.labdata[["40","days"]] + wide.labdata[["42","days"]])
# cropping the unwanted lines:
wide.labdata = wide.labdata[c("41","48"), 1:5]

interpol.labdata = melt(wide.labdata, id.vars = "date")
#levels(interpol.labdata$variable) = c("C30D1", "C30D2", "C31D1")

# function extract in tidyr: see manual and 
# http://stackoverflow.com/questions/26489276/separate-name-into-firstname-and-lastname-columns-of-data-frame
library(tidyr)
interpol.labdata = extract(interpol.labdata, variable, c("sample", "variable"), "([[:alnum:]]+)_([[:alnum:]]+)")
# turn character into factor, then change level names to fit labdata levels:

interpol.labdata$variable= as.factor(interpol.labdata$variable)
levels(interpol.labdata$variable) = c("NH4.N", "NO3.N")

labdata = dbGetQuery(db, "SELECT * FROM labdata  ORDER BY date")

w = merge(labdata, interpol.labdata, by.x = c("date", "sample", "variable"), by.y = c("date", "sample", "variable"), all.x = TRUE, all.y = TRUE)

# http://stackoverflow.com/questions/25711530/replace-row-values-conditional-on-is-na-in-another-column:
w$vals[!is.na(w$value)] <- w$value[!is.na(w$value)]

labdata = w[ , c(1, 4, 2, 3, 5)]

# overwrite the labdata table after substituting the predicted labdata values of fog:
dbWriteTable(conn=db, name="labdata", labdata, overwrite = TRUE, append=F, row.names=F) #NX data added to the db
dbDisconnect(db)

rm(db, labdata, dates, days, diffdays, interpol.labdata, w, wide.labdata, widelabdata.days)


