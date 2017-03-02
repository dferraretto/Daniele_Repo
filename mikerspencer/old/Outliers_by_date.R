#############################################################################
# -----------------------   LAB data OUTLIERS   -----------------------------
####################             BY DATE              #######################
##  CREATED: 29/11/2016
#############################################################################

rm(list=ls())

.libPaths("C:/Workspace/R")

### setwd 4 desktop
setwd("M:/My PhD/R/PhD-local_repo")

# OUTLIERS FUNCITON
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



library(RSQLite)

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

labNO3 = dbGetQuery(db, "SELECT * FROM labdata WHERE variable = 'NO3.N' ORDER BY date")

labNH4 = dbGetQuery(db, "SELECT * FROM labdata WHERE variable = 'NH4.N' ORDER BY date")

# split labNO3 by date

NO3 = split(labNO3, as.Date(labNO3$date)) #access to the nth group by [[ ]]
dates = unique(labNO3$date)

par(mar=c(1,1,1,1))

# PROVA - FUNZIONA
outlierKD(NO3[['2011-10-11']], vals) # reject all outliers?  (highest being C30D2, whilst C30D1 and C31D1 are 0.1-0.2)
outlierKD(NO3[['2011-10-31']], vals) # reject outlier C30D2 as about 3 times the RF (same order of magnitude OOM)
outlierKD(NO3[['2011-11-22']], vals) # reject all 5 (4 RF e fog), same OOM
outlierKD(NO3[['2011-12-01']], vals) # 
outlierKD(NO3[['2011-12-15']], vals)
outlierKD(NO3[['2012-01-12']], vals)
outlierKD(NO3[['2012-02-16']], vals)
outlierKD(NO3[['2012-03-15']], vals)
outlierKD(NO3[['2012-04-26']], vals)
outlierKD(NO3[['2012-05-25']], vals)
outlierKD(NO3[['2012-06-21']], vals)
outlierKD(NO3[['2012-07-20']], vals)
outlierKD(NO3[['2012-08-22']], vals)
outlierKD(NO3[['2012-09-26']], vals)
outlierKD(NO3[['2012-10-18']], vals)
outlierKD(NO3[['2012-11-15']], vals)
outlierKD(NO3[['2012-12-10']], vals)
outlierKD(NO3[['2013-01-17']], vals)
outlierKD(NO3[['2013-02-20']], vals)
outlierKD(NO3[['2013-03-28']], vals)
outlierKD(NO3[['2013-04-18']], vals)
outlierKD(NO3[['2013-05-23']], vals)
outlierKD(NO3[['2013-06-20']], vals)
outlierKD(NO3[['2013-07-28']], vals)
outlierKD(NO3[['2013-08-22']], vals)
outlierKD(NO3[['2013-10-03']], vals)
outlierKD(NO3[['2013-11-13']], vals)
outlierKD(NO3[['2014-01-24']], vals)
outlierKD(NO3[['2014-02-27']], vals)
outlierKD(NO3[['2014-03-27']], vals)
outlierKD(NO3[['2014-04-24']], vals)
outlierKD(NO3[['2014-05-21']], vals)
outlierKD(NO3[['2014-06-20']], vals)
outlierKD(NO3[['2014-07-24']], vals)
outlierKD(NO3[['2014-08-21']], vals)
outlierKD(NO3[['2014-09-23']], vals)
outlierKD(NO3[['2014-10-31']], vals)
outlierKD(NO3[['2014-11-27']], vals)
outlierKD(NO3[['2014-12-15']], vals)
outlierKD(NO3[['2015-02-05']], vals)
outlierKD(NO3[['2015-02-23']], vals)
outlierKD(NO3[['2015-03-25']], vals)
outlierKD(NO3[['2015-04-21']], vals)
outlierKD(NO3[['2015-05-19']], vals)
outlierKD(NO3[['2015-06-17']], vals)
outlierKD(NO3[['2015-07-21']], vals)
outlierKD(NO3[['2015-08-23']], vals)
outlierKD(NO3[['2015-09-24']], vals)
outlierKD(NO3[['2015-10-19']], vals)
outlierKD(NO3[['2015-11-20']], vals)
outlierKD(NO3[['2015-12-17']], vals)
outlierKD(NO3[['2016-01-19']], vals)
outlierKD(NO3[['2016-02-20']], vals)
outlierKD(NO3[['2016-03-18']], vals)
outlierKD(NO3[['2016-04-24']], vals)
outlierKD(NO3[['2016-05-24']], vals)
outlierKD(NO3[['2016-06-22']], vals)
outlierKD(NO3[['2016-07-25']], vals)
outlierKD(NO3[['2016-08-19']], vals)
outlierKD(NO3[['2016-09-20']], vals)
outlierKD(NO3[['2016-10-12']], vals)
outlierKD(NO3[['2016-11-22']], vals)

