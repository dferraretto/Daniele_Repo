#######################################################################
#
#        Chapter 3. Data analysis - extractions for tables            #
# 23/10/2017
#######################################################################

rm(list=ls())
.libPaths("C:/Workspace/R")
setwd("C:/Users/Daniele Ferraretto/Documents/Daniele_Repo")
#or
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
<<<<<<< HEAD
fielddata = dbGetQuery(db, "SELECT * FROM fielddata WHERE VALS >= 0 ORDER BY date")
=======
fielddata = dbGetQuery(db, "SELECT * FROM fielddata  WHERE date between '2012-01-01' AND '2016-12-12' ORDER BY date")# no. of lines:

>>>>>>> b4538688d70244ac5a14883be563df35c2e7cebc

summary(fielddata)
cols = c("date", "sample", "site", "variable", "overflowing", "QC")
fielddata[cols] <- lapply(fielddata[cols], factor)


nrow(fielddata) # 4236
levels(fielddata$variable)

library(dplyr)

table.variables.freq = fielddata %>% 
                       group_by(variable) %>%
                       summarise(no_rows = length(variable))

<<<<<<< HEAD
nlevels(fielddata$date)
head(fielddata)
tail(fielddata)
=======
>>>>>>> b4538688d70244ac5a14883be563df35c2e7cebc

# Select lab data from SQLite db (next step is two queries to avoid subset)
labdata = dbGetQuery(db, "SELECT * FROM labdata WHERE VALS >= 0 ORDER BY date")
# no. of lines:
summary(labdata)
cols = c("date", "sample", "site", "variable")
labdata[cols] <- lapply(labdata[cols], factor)



<<<<<<< HEAD
table.variables.freq = labdata %>% 
  group_by(variable) %>%
  summarise(no_rows = length(variable))
=======
table.variables.freq = fielddata %>% 
  group_by(variable) %>%
  summarise(no_rows = length(variable))

###########################################################################
#                PRECIPITATION  and Throughfall PLOT
#
###########################################################################

db = dbConnect(SQLite(), dbname="field_lab/Griffin_daily.SQLite")

source("mikerspencer/RFTSW_monthly.R")

long.TF.RF.fog.depth = long.TF.RF.fog.depth[long.TF.RF.fog.depth$date>"Dec 2011" & long.TF.RF.fog.depth$date <"Jan 2017", ]
long.TF.RF.fog.depth = long.TF.RF.fog.depth[long.TF.RF.fog.depth$variable != "fog", ]

ggplot() + 
  geom_area(data = long.TF.RF.fog.depth, aes (month, value, fill = variable, group = variable), position = "identity") + 
  geom_line(data = long.TF.RF.fog.depth, aes (month, value, group = variable)) +
  geom_point(data = long.TF.RF.fog.depth, aes (month, value, group = variable)) +
  facet_grid(year ~ .) +
  ggtitle("Precipitation and throughfall") +
  labs(x = "month", y = "monthly depth (mm)") + theme_bw(base_size = 11) +
  scale_fill_manual(values = c('RF' = "dodgerblue1",'TF' = "palegreen4"),
                    labels = c('RF' = "precipitation",'TF' = "throughfall")) +
  scale_colour_manual(values = c('RF' = "dodgerblue1",'TF' = "palegreen4"),
                       labels = c('RF' = "precipitation",'TF' = "throughfall")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.background = element_rect(fill = "transparent",colour = NA)) 

#############################################################################
#
#                           RF  OVERFLOWS
#
#############################################################################
db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

RF_OF = dbGetQuery(db, "SELECT * FROM fielddata  WHERE variable = 'precip depth' AND overflowing = 1 ORDER BY date")
RF_OF = RF_OF[RF_OF$sample != "C30D2",]

#############################################################################
#
#                 SF contribution to the hydrological balance
#
#############################################################################

# PART 1: calculate SF 

db = dbConnect(SQLite(), dbname="field_lab/Daily_Griffin.SQLite")

# select SF data from SQL db
sf = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'stem vol' AND vals>=0 ORDER BY date")

cols = c( "sample", "date", "site", "variable", "overflowing", "QC")
sf[cols] <- lapply(sf[cols], factor)

# importing dbh classes and merge with sf
stemdbh <- read.csv("mikerspencer/stemdbh.csv", header=TRUE)
dbh.class=stemdbh[,c(1,5)]
SF.class=merge(sf,dbh.class, by = "sample")
SF.class=arrange(SF.class,date,dbh.class)
# STEP 1: MEAN  vals by dbh.class+DATE:
classSFvol= aggregate(vals ~ date+dbh.class, data = SF.class, FUN = mean,  na.rm = TRUE) # mean SF volume by day by class

# STEP2: MEAN vals by date:
SFmeanvol <- aggregate(vals ~ date, data = classSFvol, FUN = mean,  na.rm = TRUE) 

# STEP 3: aggregate by month, obtaining a "single model tree" vol
SFmeanvol$mY=strftime(SFmeanvol$date,"%Y%m") # creates month-Year column

monthly.SFvol = aggregate(vals ~ mY,  data = SFmeanvol, FUN = sum)

# STEP 4: scale to the hectare (1750 trees
monthly.SFvol$vals = monthly.SFvol$vals*1750/10000

#################################################################################
#
#                                SF ~ DBH2
#
#################################################################################

rm(list = ls())

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")
sf = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'stem vol' AND vals > 0 ORDER BY date")

# importing dbh classes and merge with sf
stemdbh <- read.csv("mikerspencer/stemdbh.csv", header=TRUE)
dbh.class=stemdbh[,c(1,4)]
SF.class=merge(sf,dbh.class, by = "sample")
SF.class=arrange(SF.class,date,mean_d)
SF.class$DBH2 = SF.class$mean_d^2

# Calculating multiple regression
SF.DBH2.lm <- lm(vals ~ DBH2 + date, data=SF.class)

summary(SF.DBH2.lm)


# PLOTTING SF ~ DBH2
ggplot() + 
  geom_point(data = SF.class, aes (DBH2, vals)) + 
  facet_grid(date ~ ., scales = "free_x")
  
>>>>>>> b4538688d70244ac5a14883be563df35c2e7cebc
