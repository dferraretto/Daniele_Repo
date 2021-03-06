#
#
#####            OUTLIERS            ##########
+++++++++   Last check: 31/03/2017  +++++++++++
# see: https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/

# Outlier removal by the Tukey rules on quartiles +/- 1.5 IQR
# 2016 Klodian Dhana


rm(list=ls())


.libPaths("C:/Workspace/R")

### setwd 4 desktop
setwd("C:/Users/s1373890/Daniele_Repo")

# Funzione per trovare gli outliers:
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title( main = deparse(substitute(dt)), outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "\n")
  cat("Proportion (%) of outliers:", round((na2 - na1) / tot*100, 1), "\n")
  cat("Mean of the outliers:", round(mo, 2), "\n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "\n")
  cat("Mean if we remove outliers:", round(m2, 2), "\n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "\n")
    return(invisible(var_name))
}
}


# on 25/11/2016 I got this weird "Error in plot.new() : figure margins too large". To solve it check:
par("mar") # if the result was 5.1 4.1 4.1 2.1 then:
par(mar=c(1,1,1,1))


# Connect to SQL db

library(RSQLite)

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

throughvol = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through vol' ORDER BY date")

outlierKD(throughvol, vals)

#3 outliers, but acceptable, as the highest is a rare TF OF

#------------------------------

stemvol = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'stem vol' ORDER BY date")

smallSF = c("T10S1", "T10S2", "T10S3", "T11S1", "T11S2", "T11S3", "T12S1", "T12S2", "T12S3", "C10S2", "C11S1", "C11S2", "C11S3", "C11S4", "C11S5")

littleSF <-stemvol[which(stemvol$sample %in% smallSF),]

outlierKD(littleSF, vals)

bigSF = c( "C10S1", "C10S3", "C11S6","C11S7", "C12S1", "C12S2", "C12S3")
hugeSF <-stemvol[which(stemvol$sample %in% bigSF),]
outlierKD(hugeSF, vals)

# sF DOes NOT HAVE OUTLIERS!

#------------------------------

rainfall = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'precip depth' ORDER BY date")
RF = c("C30D1", "C31D1")
fog = "C30D2"
RF.outl =  rainfall[which(rainfall$sample %in% RF),]
fog.outl =  rainfall[which(rainfall$sample %in% fog),]

outlierKD(RF.outl, vals)
outlierKD(fog.outl, vals)

#  2 outliers: C30D2 on 2011-12-01 and C31D1 on 2011-12-15, NOT removed from here

#------------------------------

throughdepth = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' ORDER BY date")

outlierKD(throughdepth, vals)

# outliers: 3, accepted: NONE

#############################################################################
# ----------------------------   LAB data   ---------------------------------
####################           OLD EDITION            #######################

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

labdata = dbGetQuery(db, "SELECT * FROM labdata ORDER BY date")
labNO3 = labdata[which(labdata$variable %in% 'NO3.N'),]
labNH4 = labdata[which(labdata$variable %in% 'NH4.N'),] # in questo modo mantengo "memoria" del numero della riga per poi identificare gli outliers

# lab throughfalls:

throughfall = c("T10T1", "T10T2", "T10T3", "T11T1", "T11T2", "T11T3", "T12T1", "T12T2", "T12T3", "C10T1", "C10T2", "C10T3", "C11T1", "C11T2", "C11T3", "C12T1", "C12T2", "C12T3")

TFNO3<-labNO3[which(labNO3$sample %in% throughfall),]
TFNH4<-labNH4[which(labNH4$sample %in% throughfall),]

outlierKD(TFNO3, vals)
# outliers identified on 03/04/2017: 82.
# select potential outliers row and decide:
TFNO3 = TFNO3[order(-TFNO3[,5]), ]
potentialOUTLIERS = head(TFNO3, n=82)
potentialOUTLIERS = potentialOUTLIERS[order(potentialOUTLIERS  [,5]), ] 
# removed: 1 (T11T2 on 19/10/2015)
# 2015-10-19 T11T2 NO3.N


outlierKD(TFNH4, vals)
# outliers identified on 03/04/2017:132
# select potential outliers row and decide:
TFNH4 = TFNH4[order(-TFNH4[,5]), ]
potentialOUTLIERS = head(TFNH4, n=132)
potentialOUTLIERS = potentialOUTLIERS[order(potentialOUTLIERS  [,1]), ] 
# removed 3 TFNH4 rows: the 4th and 5th highest values are not much higher than other values in the same date (high deposition event)
  # 2013-08-22 T11T2 NH4.N
  # 2015-04-21 T12T1 NH4.N
  # 2014-04-24 T12T1 NH4.N


# lab stemflows
stemflow = c("T10S1", "T10S2", "T10S3", "T11S1", "T11S2", "T11S3", "T12S1", "T12S2", "T12S3", "C10S1", "C10S2", "C10S3", "C11S1", "C11S2", "C11S3", "C11S4", "C11S5", "C11S6","C11S7", "C12S1", "C12S2", "C12S3")
 

SFNO3<-labNO3[which(labNO3$sample %in% stemflow),]
SFNH4<-labNH4[which(labNH4$sample %in% stemflow),]

outlierKD(SFNO3, vals)
# outliers identified on 31/03/2017: 186
# select potential outliers row and decide:
SFNO3 = SFNO3[order(-SFNO3[,5]), ]
potentialOUTLIERS = head(SFNO3, n=186)
# Anche in questo caso i numeri alti sono tanti, ma preferisco eliminarne uno solo pur sapendo che i valori molto elevati vengono da quasi 0 volumes,
# good to give an explanation in the data quality sub-chapter
# 2014-07-24 T10S2 NO3.N

outlierKD(SFNH4, vals)
# outliers identified on 03/04/2017: 156 
# select potential outliers row and decide:
SFNH4 = SFNH4[order(-SFNH4[,5]), ]
potentialOUTLIERS = head(SFNH4, n=156)
potentialOUTLIERS = potentialOUTLIERS[order(potentialOUTLIERS  [,1]), ]
# 7 accepted, all those > 10:
# 2014-07-24 T10S2 NH4.N
# 2014-06-20 T12S2 NH4.N
# 2015-02-23 T10S1 NH4.N
# 2013-07-28 T10S2 NH4.N
# 2013-08-22 C10S1 NH4.N
# 2015-02-23 T10S2 NH4.N
# 2012-04-26 T10S2 NH4.N


# LAB RAINFALL AND FOG

RFNO3 =  labNO3[which(labNO3$sample %in% RF),]
RFNH4 =  labNH4[which(labNH4$sample %in% RF),]

fogNO3 = labNO3[which(labNO3$sample %in% fog),]
fogNH4 = labNH4[which(labNH4$sample %in% fog),]

# check for anomalies i.e. huge differences in conc. values between C30 and C31:
wide.RFNO3 = dcast(RFNO3, date ~ sample, value.var="vals")
wide.RFNH4 = dcast(RFNH4, date ~ sample, value.var="vals")
# check the difference between samples:
wide.RFNO3$diff = wide.RFNO3$C30D1 - wide.RFNO3$C31D1
wide.RFNH4$diff = wide.RFNH4$C30D1 - wide.RFNH4$C31D1

outlierKD(wide.RFNO3, diff) # 8 identified, now check and keep a precautionary approach:
# select potential RF-NO3 outliers rows and decide:
wide.RFNO3$diff = abs(wide.RFNO3$diff)
wide.RFNO3 = wide.RFNO3[order(-wide.RFNO3[,4]), ]
potentialOUTLIERS = head(wide.RFNO3, n=7) # none accepted

outlierKD(wide.RFNH4, diff)
# select potential outliers row and decide:
wide.RFNH4$diff = abs(wide.RFNH4$diff)
wide.RFNH4 = wide.RFNH4[order(-wide.RFNH4[,4]), ]
potentialOUTLIERS = head(wide.RFNH4, n=14) # 5 accepted and deleted in outliers (see note there)


# outliers identified on 03/04/2017: 9 
# select potential outliers row and decide:
RFNO3 = RFNO3[order(-RFNO3[,5]), ]
potentialOUTLIERS = head(RFNO3, n=9)
potentialOUTLIERS = potentialOUTLIERS[order(potentialOUTLIERS  [,1]), ] 
# NONE accepted, all make sense as more or less coupled per date
outlierKD(RFNH4, vals)
# outliers identified on 03/04/2017: 9 
# select potential outliers row and decide:
RFNH4 = RFNH4[order(-RFNH4[,5]), ]
potentialOUTLIERS = head(RFNH4, n=9)
potentialOUTLIERS = potentialOUTLIERS[order(potentialOUTLIERS  [,1]), ] # 3/8 accepted, as much higher than the other sample in the same date:
  # 2015-06-17 C31D1 NH4.N
  # 2015-07-21 C31D1 NH4.N
  # 2013-10-03 C31D1 NH4.N

outlierKD(fogNO3, vals)
# outliers identified on 03/04/2017: 4 
# select potential outliers row and decide:
fogNO3 = fogNO3[order(-fogNO3[,5]), ]
potentialOUTLIERS = head(fogNO3, n=4)
# NONE ACCEPTED as all belonging to high deposition dates!

outlierKD(fogNH4, vals)
# outliers identified on 25/11/2016: 4 
# select potential outliers row and decide:
fogNH4 = fogNH4[order(-fogNH4[,5]), ]
potentialOUTLIERS = head(fogNH4, n=4)
potentialOUTLIERS = potentialOUTLIERS[order(potentialOUTLIERS  [,1]), ] 
# NONE ACCEPTED as all belonging to high deposition dates!

#------------------------

