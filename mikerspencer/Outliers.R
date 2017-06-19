#####################################################################################
#                                                                                   #
#                       OUTLIERS CORRECTIONS                                        #
# 02/09/2016                        Authour: Daniele Ferraretto                 
# Last update and use: 2017-04-03
#------------------------------------------------------------------------------------

# THIS FILE AIMS TO MANUALLY CORRECT SOME OF THE OUTLIERS, AFTER RUNNING THE oUTLIERS_aUG2016
# AND EVENTUALLY CHECKING THE FIELD/LAB DIARY.
# THE IDEA (TO BE CHECKED) IS NOT TO CORRECT THE ORIGINAL DATA ON .xls BUT THE .SQL db
# Corrections MUST be run AFTER the creation of Griffin.SQLite and BEFORE the creation
# of the DAILY DB
# UPDATE: on March 2017 I found that the use of the number of the rows does not guarantee to delete the proper lines.
# Consequently, I reran the outliers check, prepared a list of outliers (much shorter than before), removed 2 
# fieldwork outliers directly with the above mentioned script and now need to change keys to recognise the outliers and modify them
# -----------------------------------------------------------------------------------
# OUTLIERS REMOVED THROUGH OUTLIERS.Aug2016:
# 2011-12-01 C30D2 (field)
# 2011-12-15 C31D1 (field)

# Outliers to be removed through this script:
  
# 2015-10-19 T11T2 NO3.N
  
# 2013-08-22 T11T2 NH4.N
# 2015-04-21 T12T1 NH4.N
# 2014-04-24 T12T1 NH4.N
  
# 2014-07-24 T10S2 NO3.N

# 2014-07-24 T10S2 NH4.N
# 2014-06-20 T12S2 NH4.N
# 2015-02-23 T10S1 NH4.N
# 2013-07-28 T10S2 NH4.N
# 2013-08-22 C10S1 NH4.N
# 2015-02-23 T10S2 NH4.N
# 2012-04-26 T10S2 NH4.N

# 2015-06-17 C31D1 NH4.N
# 2015-07-21 C31D1 NH4.N
# 2013-10-03 C31D1 NH4.N

rm(list=ls())

### setwd 4 desktop
setwd("C:/Users/s1373890/Daniele_Repo")

library(RSQLite)

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

# NB: Part 1 and Part 2 are meant to decide which outliers to drop or change. 
# Jump to line 150 to apply the changes and save them in Griffin.SQLite


#####################################################################################
#                              Part 1: field data
#####################################################################################

# Query SQLite on labdata table not by DESCending date

fielddata = dbGetQuery(db, "SELECT date, time, sample, site, variable, vals, overflowing, QC, comments FROM fielddata ORDER BY date")

# 2 Outliers to remove: #  2 outliers: C30D2 on 2011-12-01 and C31D1 on 2011-12-15, 
# problems on 10/04/2017: stommerda di 1/12/2011 gives error, but problem is that there is no rainfall for this date!!!
# cosa faccio? regressione al contrario? C30D1 da C30D2? Si, toh!

#NB: non c'e' ragione di rimuovere il valore fog del 1/12. Fino a che non ci capisco meglio non rimuovo proprio nulla!

fielddata[(fielddata$date == '2011-12-01' & fielddata$sample == 'C30D2'),] = NA
fielddata$vals[fielddata$date == '2011-12-15' & fielddata$sample == 'C31D1'] = NA

#### FOG correction NB: I will use a capture efficiency coefficient of *0.06/0.29 instead than 0.05 but this correction will applied to the 
#### fog_monthly db (or any other future script aimed to use the single sample values separately)

# ----------------------------------------------------------------------------------
#                         Coding OF = 1 from the old data
# ----------------------------------------------------------------------------------

fielddata$overflowing[fielddata$comments == "F"] = 1
fielddata$overflowing[fielddata$overflowing == "TRUE"] = 1

# appending fieldata

dbWriteTable(conn=db, name="fielddata", fielddata, overwrite = TRUE, append=F, row.names=F) #NX data added to the db

#####################################################################################
#                              Part 2: lab data
#####################################################################################

# Query SQLite on labdata table not by DESCending date

labdata = dbGetQuery(db, "SELECT * FROM labdata ORDER BY date")

# The following is to identify the rows I need to amend/delete. These files are only intended to indentify the lines, NOT to be overwritten to my db!!!


# REMOVING OUTLIERS FROM LABDATA:
# OLD REMOVALS:
#  labdata = labdata[-c(6452, 3862, 1031, 1443, 2169, 2481, 3426, 3595, 5454, 8327, 1064, 1090, 2318, 3646, 3886, 5336, 6012,
   #                  6083, 7871, 8322, 863, 2363, 2403, 3758, 3885, 5065, 5058, 2559, 5738, 5907), ] # Last check: 02/12/2016. 30 outliers deleted.
# All the rows to be deleted in labdata are removed HERE
# removed before november 2016: 3886, 5010, 5336, 6012, 6181, 7881, 1031, 2369, 2481, 5454, 2363, 2403, 3758, 3885, 5065, 5068, 2559, 5738, 5907, 7766, 7929

#### LABDATA CORRECTIONS (can be updated): 
labdata$vals[labdata$date == '2015-10-19' & labdata$sample == 'T11T2' & labdata$variable == 'NO3.N'] = NA
labdata$vals[labdata$date == '2013-08-22' & labdata$sample == 'T11T2' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2015-04-21' & labdata$sample == 'T12T1' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2014-04-24' & labdata$sample == 'T12T1' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2014-07-24' & labdata$sample == 'T10S2' & labdata$variable == 'NO3.N'] = NA
labdata$vals[labdata$date == '2014-07-24' & labdata$sample == 'T10S2' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2014-06-20' & labdata$sample == 'T12S2' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2015-02-23' & labdata$sample == 'T10S1' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2013-07-28' & labdata$sample == 'T10S2' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2013-08-22' & labdata$sample == 'C10S1' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2015-02-23' & labdata$sample == 'T10S2' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2012-04-26' & labdata$sample == 'T10S2' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2015-06-17' & labdata$sample == 'C31D1' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2015-07-21' & labdata$sample == 'C31D1' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2013-10-03' & labdata$sample == 'C31D1' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2016-06-22' & labdata$sample == 'C31D1' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2016-07-25' & labdata$sample == 'C30D1' & labdata$variable == 'NH4.N'] = NA
labdata$vals[labdata$date == '2016-07-25' & labdata$sample == 'C31D1' & labdata$variable == 'NH4.N'] = NA
# TEMP! LATER TO BE OBTAINED BY INVERSE REGRESSION FROM C30D2

# THIS WAS A THRESHOLD OUTLIER,
# TAKING INTO CONSIDERATION THAT THE THRESHOLD WAS SET AT 0.75 (COMPARISON BETWEEN NO3 AND NH4 OUTLIERS) AND THIS WAS A DIFFERENCE
# BETWEEN C30D1 AND C31D1 OF 0.77. HOWEVER, THE LOW VAL OF C30D2 CONFIRMED THE ANOMALY IN THE HIGH VALUE OF C30D1. LAST TWO REJECTIONS
# WERE ADDED ON 11/04/2017 AFTER CALCULATING THE (C30D1-C31D1) OUTLIERS


# appending fieldata and part of labdata (Nform) 

dbWriteTable(conn=db, name="labdata", labdata, overwrite = TRUE, append=F, row.names=F) #NX data added to the db

dbDisconnect(db)

rm(fielddata, labdata, db)






