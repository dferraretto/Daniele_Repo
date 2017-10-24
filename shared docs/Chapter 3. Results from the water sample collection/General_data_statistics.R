#######################################################################
#
#        Chapter 3. Data analysis - extractions for tables            #
# 23/10/2017
#######################################################################

rm(list=ls())
.libPaths("C:/Workspace/R")

setwd("C:/Users/s1373890/Daniele_Repo")

# --------------------------------------------------------
# Connect to SQL db
# --------------------------------------------------------
      library(RSQLite)
      library(ggplot2)
      library(plotly)
      library(zoo)
      
db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")
      
# Select field data from SQLite db (next step is two queries to avoid subset)
fielddata = dbGetQuery(db, "SELECT * FROM fielddata WHERE VALS >= 0 ORDER BY date")

# no. of lines:
summary(fielddata)
cols = c("date", "time", "sample", "site", "variable", "overflowing", "QC")
fielddata[cols] <- lapply(fielddata[cols], factor)


nrow(fielddata) # 4236
levels(fielddata$variable)

library(dplyr)

table.variables.freq = fielddata %>% 
                       group_by(variable) %>%
                       summarise(no_rows = length(variable))


# Select lab data from SQLite db (next step is two queries to avoid subset)
labdata = dbGetQuery(db, "SELECT * FROM labdata WHERE VALS >= 0 ORDER BY date")
# no. of lines:
summary(labdata)
cols = c("date", "sample", "site", "variable")
labdata[cols] <- lapply(labdata[cols], factor)



table.variables.freq = fielddata %>% 
  group_by(variable) %>%
  summarise(no_rows = length(variable))