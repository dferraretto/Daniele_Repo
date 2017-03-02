#
#
#
#####            OUTLIERS            ##########

# see: https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/

# Outlier removal by the Tukey rules on quartiles +/- 1.5 IQR
# 2016 Klodian Dhana

.libPaths("C:/Workspace/R")


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
# esempio

setwd("M:/My PhD/R/PhD-local_repo")

# --------------------------------------------------------
# Connect to SQL db
# --------------------------------------------------------

library(RSQLite)

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

throughvol = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through vol' ORDER BY date")

outlierKD(throughvol, vals)

#------------------------------

stemvol = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'stem vol' ORDER BY date")

outlierKD(stemvol, vals)

#------------------------------

rainfall = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'precip depth' ORDER BY date")

outlierKD(rainfall, vals)

#------------------------------

throughdepth = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' ORDER BY date")

outlierKD(throughdepth, vals)

# The analysis of the outliers in the field data RF, TF, SF only shows considerable outliers within the SF data. TF depth shows no outliers,
# RF shows a single outlier. Let's see what happens with the lab + field data
# Per me: I should first "clean" my db from outliers, and then use the "cleaned db" to produce a "cleaned" daily db

source("mikerspencer/RFTSW_monthly.R")

outlierKD(mNH4fog, vals)
outlierKD(mNO3fog, vals)
outlierKD(mNH4RF, vals)
outlierKD(mNO3RF, vals)
outlierKD(mNH4SF, vals)
outlierKD(mNO3SF, vals)
outlierKD(mNH4TF, vals)
outlierKD(mNO3TF, vals)

# COMMENTS: outliers applied to the lab+field transformed data is not a wise option. some of the outliers could make sense but they do not
# recognise outliers from high acceptable values.


