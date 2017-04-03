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


# on 25/11/2016 I got this weird "Error in plot.new() : figure margins too large". To solve it check:
par("mar") # if the result was 5.1 4.1 4.1 2.1 then:
par(mar=c(1,1,1,1))

#####################################################################################
#                              Part 1: field data
#####################################################################################

# Query SQLite on labdata table not by DESCending date

fielddata = dbGetQuery(db, "SELECT date, time, sample, site, variable, vals, overflowing, QC, comments FROM fielddata ORDER BY date")

# 2 Outliers removed through the Outliers.Aug2016.R

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

#  labdata = labdata[-c(6452, 3862, 1031, 1443, 2169, 2481, 3426, 3595, 5454, 8327, 1064, 1090, 2318, 3646, 3886, 5336, 6012,
   #                  6083, 7871, 8322, 863, 2363, 2403, 3758, 3885, 5065, 5058, 2559, 5738, 5907), ] # Last check: 02/12/2016. 30 outliers deleted.
# All the rows to be deleted in labdata are removed HERE

# removed before november 2016: 3886, 5010, 5336, 6012, 6181, 7881, 1031, 2369, 2481, 5454, 2363, 2403, 3758, 3885, 5065, 5068, 2559, 5738, 5907, 7766, 7929

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



#### LABDATA CORRECTIONS (can be updated): 

# appending fieldata and part of labdata (Nform) 

dbWriteTable(conn=db, name="labdata", labdata, overwrite = TRUE, append=F, row.names=F) #NX data added to the db

dbDisconnect(db)








