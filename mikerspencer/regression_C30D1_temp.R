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
