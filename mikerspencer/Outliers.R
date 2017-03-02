#####################################################################################
#                                                                                   #
#                       OUTLIERS CORRECTIONS                                        #
# 02/09/2016                        Authour: Daniele Ferraretto                 
# Last update and use: 2016-11-25
#------------------------------------------------------------------------------------

# THIS FILE AIMS TO MANUALLY CORRECT SOME OF THE OUTLIERS, AFTER RUNNING THE oUTLIERS_aUG2016
# AND EVENTUALLY CHECKING THE FIELD/LAB DIARY.
# THE IDEA (TO BE CHECKED) IS NOT TO CORRECT THE ORIGINAL DATA ON .xls BUT THE .SQL db
# Corrections MUST be run AFTER the creation of Griffin.SQLite and BEFORE the creation
# of the DAILY DB
# -----------------------------------------------------------------------------------

rm(list=ls())

### setwd 4 desktop
setwd("M:/My PhD/R/PhD-local_repo")

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

#### FIELDDATA CORRECTIONS (can be updated)
fielddata = fielddata[-c(254, 327), ]  # deleting the biggest C30D2 outlier (16.48232) and C31D1

#### FOG correction NB: I will use a capture efficiency coeeficient of *0.06/0.29 instead than 0.05 but this correction will applied to the 
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

labdata = labdata[-c(6452, 3862, 1031, 1443, 2169, 2481, 3426, 3595, 5454, 8327, 1064, 1090, 2318, 3646, 3886, 5336, 6012,
                     6083, 7871, 8322, 863, 2363, 2403, 3758, 3885, 5065, 5058, 2559, 5738, 5907), ] # Last check: 02/12/2016. 30 outliers deleted.
# All the rows to be deleted in labdata are removed HERE

# removed before november 2016: 3886, 5010, 5336, 6012, 6181, 7881, 1031, 2369, 2481, 5454, 2363, 2403, 3758, 3885, 5065, 5068, 2559, 5738, 5907, 7766, 7929



#### LABDATA CORRECTIONS (can be updated): 

# appending fieldata and part of labdata (Nform) 

dbWriteTable(conn=db, name="labdata", labdata, overwrite = TRUE, append=F, row.names=F) #NX data added to the db

dbDisconnect(db)





##############      NITRATE LAB DATA        ##################
# IMPORTANT TO READ!!!! on 25/11/2016 I decided to manually check all data, report the deleteable rows on it and hence I will skip all this part and go
# straight to the deleting row below. Note that the method below is pretty fuzzy. I believe I should check outliers more automatically
# by analizing data by date and eventually by sample. 

labNO3 = labdata[which(labdata$variable=='NO3.N'),]

# Now filtering the outliers by checking the graphs printed in Outlier Griffindata_1.docx (MyPhD/Meeting with supervisors Meeting 20160830)

# THROUGHFALL

TF = c("T10T1", "T10T2", "T10T3", "T11T1", "T11T2", "T11T3", "T12T1", "T12T2", "T12T3", "C10T1", "C10T2", "C10T3", "C11T1", "C11T2", "C11T3", "C12T1", "C12T2", "C12T3")

TFNO3outliers = labNO3[labNO3$sample %in% TF,] #& labNO3$vals > 2.1
TFNO3outliers[complete.cases(TFNO3outliers),]
# 4 removed rows suggested by the function, yet 3 belong to 2015-02-23 where most of values are above 1.5: 

# Deleted row(s): 6452

# Notes: I found out a very weird sampling series, dated February 2015, when Mike collected samples show very high levels of NO3 (1to10 mg/l!).
# There was few water in the barrels, which makes me wonder if anything happened in those barrels at all... 


# STEMFLOW:

SF = c("T10S1", "T10S2", "T10S3", "T11S1", "T11S2", "T11S3", "T12S1", "T12S2", "T12S3", "C10S2", "C11S1", "C11S2", "C11S3", "C11S4", "C11S5", "C10S1", "C10S3", "C11S6","C11S7", "C12S1", "C12S2", "C12S3")

SFNO3outliers = labNO3[labNO3$sample %in% SF & labNO3$vals > 3.85,] #& labNO3$vals > 3.85
SFNO3outliers[complete.cases(SFNO3outliers),]

# Deleted SFNO3 rows: the function identifies 7 outliers. All accepted (vals > 3.85 mg/l) including two values registered on 23/02/2015 as they are 
# outliers even within the sampling date.
# Deleted row(s): 3886, 5010, 5336, 6012, 6181, 7881, 8322

# RAINFALL:

RF = c("C30D1", "C31D1")


# IMPORTANT: RF has a specific issue to be solved. Outliers as expression of C30 AND C31 are not explaining the reality. In fact in many cases old data show 
# unlikely differences between concentrations in C30 and C31. I should calculate a difference of conc values per each date, then calculating outliers of those
# and correct the difference as the minimum value (considering the highest as a result of contaminated sample)

# HENCE:
RFNO3outliers = labNO3[labNO3$sample %in% RF, ] # select all available RFNO3 values
RFNO3outliers = aggregate(data = RFNO3outliers, vals ~ date, diff, na.exclude = T) # calculate the difference of RFNO3 values by date
RFNO3outliers$vals = as.numeric(RFNO3outliers$vals)
RFNO3outliers$vals = abs(RFNO3outliers$vals)
outlierKD(RFNO3outliers, vals) # this tells me I have 8 possible outliers, with values above 0.5. let's check'em!
# Quali sono i miei outliers?
RFNO3outliers = RFNO3outliers[RFNO3outliers$vals>0.2,]
RFNO3outliers = labNO3[labNO3$sample %in% RF & labNO3$vals > 0.8,] 
RFNO3outliers[complete.cases(RFNO3outliers),]
# OUTLIERS: THE HIGHEST VALUE IS 28-03-2013, but I will accept it as the fog value is 3 times higher than the D1, note says funnel not changed by Rob.
# 3 more significant outliers: 20/06/2013 (0.34), 25/03/2015 (0.289), 19/10/2015 (0.274).
# RFNO3 deleted/changed rows: NONE of the above. 2015-06-17 would need to be substituted due to bird poo but the difference is not relevant


#############      AMMONIA LAB DATA             #############

labNH4 = labdata[which(labdata$variable=='NH4.N'),] # in questo modo mantengo "memoria" del numero della riga per poi identificare gli outliers

# THROUGHFALL

TFNH4outliers = labNH4[labNH4$sample %in% TF ,] # & labNH4$vals > 3.1

TFNH4outliers[complete.cases(TFNH4outliers),]

# removed 4 TFNH4 rows: stitching to the function yet EXCLUDING the date 24/07/2014: 4 vals in T plot are around 4 mg/l!! 
# Deleted row(s): 1031, 2369, 2481, 5454, 3426

# STEMFLOW

SFNH4outliers = labNH4[labNH4$sample %in% SF & labNH4$vals > 10,] 

SFNH4outliers[complete.cases(SFNH4outliers),]

# Deleted rows were above 10.5. I will stitch to the OUTLIERS function!
# Deleted row(s): 863, 2363, 2403, 3758, 3885, 5065, 5068


# RAINFALL:

# IMPORTANT: RF has a specific issue to be solved. Outliers as expression of C30 AND C31 are not explaining the reality. In fact in many cases old data show 
# unlikely differences between concentrations in C30 and C31. I should calculate a difference of conc values per each date, then calculating outliers of those
# and correct the difference as the minimum value (considering the highest as a result of contaminated sample)

# HENCE:
RFNH4outliers = labNH4[labNH4$sample %in% RF, ] # select all available RFNH4 values
RFNH4outliers = aggregate(data = RFNH4outliers, vals ~ date, diff, na.exclude = T) # calculate the difference of RFNH4 values by date
RFNH4outliers$vals = as.numeric(RFNH4outliers$vals)
RFNH4outliers$vals = abs(RFNH4outliers$vals)
outlierKD(RFNH4outliers, vals) # this tells me I have 7 possible outliers, with values above 0.7. let's check'em!
# Quali sono i miei outliers?
RFNH4outliers = RFNH4outliers[RFNH4outliers$vals>0.4,]
RFNH4outliers = labNH4[labNH4$sample %in% RF & labNH4$vals > 0.8,] 
RFNH4outliers[complete.cases(RFNH4outliers),]

# identified outliers: 7. (ma parlo di 6 sotto... boh, checkin procrastination, qui facevo grafici per il documento outliers per kate)
# Substituting outliers (2013-03-28 (0.701) NO, HIGH VALUES EVERYWHERE RISK TO OVER-SUBESTIMATE, 2013-10-03 YES C31D1 MUCH HIGHER THAN C30 IN BOTH Nx,
# IN NH4 EVEN HIGHER THAN FOG. 2015-06-17 BOTH TO BE SUBSTITUTED DUE TO BIRD POO; 21-07-2015 crazy value for C31D1 to be substituted (deleted) 
# 2016-06-22 NOEXPLANATIONS BUT C31D1 DOUBLE THAN FOG, TO BE deleted, 2016-07-25 DA CAMBIARE, PIU ALTO DI D2 DI BESTIA) with the lowest measured value:
# C31D1 to be deleted. Rows to be deleted: 2559, 5738, 5907, 7766, 7929


