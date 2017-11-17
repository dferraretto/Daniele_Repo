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

fielddata = dbGetQuery(db, "SELECT * FROM fielddata WHERE VALS >= 0 ORDER BY date")

fielddata = dbGetQuery(db, "SELECT * FROM fielddata  WHERE date between '2012-01-01' AND '2016-12-12' ORDER BY date")# no. of lines:



summary(fielddata)
cols = c("date", "sample", "site", "variable", "overflowing", "QC")
fielddata[cols] <- lapply(fielddata[cols], factor)


nrow(fielddata) # 4236
levels(fielddata$variable)

library(dplyr)

table.variables.freq = fielddata %>% 
                       group_by(variable) %>%
                       summarise(no_rows = length(variable))


nlevels(fielddata$date)
head(fielddata)
tail(fielddata)



# Select lab data from SQLite db (next step is two queries to avoid subset)
labdata = dbGetQuery(db, "SELECT * FROM labdata WHERE VALS >= 0 ORDER BY date")
# no. of lines:
summary(labdata)
cols = c("date", "sample", "site", "variable")
labdata[cols] <- lapply(labdata[cols], factor)



table.variables.freq = labdata %>% 
  group_by(variable) %>%
  summarise(no_rows = length(variable))

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
library(dplyr)
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

# SELECT 5 (10) DATES WITH THE HIGHEST (LOWEST) RAINFALL EVENTS

rm(list = ls())
db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")
RF = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'precip depth' AND sample != 'C30D2' ORDER BY date") # DOUBLE COSTRAINT WITH !=
RF=arrange(RF,-vals) #needs dplyr
Hrf = RF[c(1:30),]
Hrf = unique(Hrf$date)
Lrf = unique(tail(RF, n = 10))
Lrf = Lrf[Lrf$date!="2011-12-15",]
Lrf = unique(Lrf$date)

# Extract SF values for wettest and driest dates

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")
sf = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'stem vol' AND vals > 0 ORDER BY date")
cols = c( "sample", "date", "site", "variable", "overflowing", "QC")
sf[cols] <- lapply(sf[cols], factor)
sf = sf[sf$vals<35, ]
# importing dbh classes and merge with sf
stemdbh <- read.csv("mikerspencer/stemdbh.csv", header=TRUE)
dbh.class=stemdbh[,c(1,4)]
SF.class=merge(sf,dbh.class, by = "sample")
SF.class=arrange(SF.class,date,mean_d)
SF.class$DBH2 = SF.class$mean_d^2

# SELECT WETTEST DATES
SF.class.wet = SF.class[SF.class$date %in% Hrf,]

# SELECT driest DATES
SF.class.dry = SF.class[SF.class$date %in% Lrf,]

# SELECT intermediate DATES https://stackoverflow.com/questions/17427916/r-selecting-all-rows-from-a-data-frame-that-dont-appear-in-another
SF.class.intermediate <- rbind(SF.class, SF.class.dry, SF.class.wet)

SF.class.intermediate = SF.class.intermediate[! duplicated(SF.class.intermediate, fromLast=TRUE) & seq(nrow(SF.class.intermediate)) <= nrow(SF.class), ]

# Calculating multiple regression for:
#DRY
SF.dry.lm <- lm(vals ~ DBH2 + date, data=SF.class.dry)

summary(SF.dry.lm)

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(SF.dry.lm)
#WET
SF.wet.lm <- lm(vals ~ DBH2 + date, data=SF.class.wet)

summary(SF.wet.lm)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(SF.wet.lm)
#INTERMEDIATE
SF.intermediate.lm <- lm(vals ~ DBH2 + date, data=SF.class.intermediate)

summary(SF.intermediate.lm)

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(SF.intermediate.lm)

# PLOTTING SF ~ DBH2
library(ggplot2)

ggplot() + 
  geom_point(data = SF.dry.lm, aes (DBH2, vals)) + 
  facet_grid(date ~ ., scales = "free_x")


ggplot() + 
  geom_point(data = SF.wet.lm, aes (DBH2, vals)) + 
  facet_grid(date ~ ., scales = "free_x")

ggplot() + 
  geom_point(data = SF.intermediate.lm, aes (DBH2, vals)) + 
  facet_grid(date ~ ., scales = "free_x")
# COMMENT: WHATEVER SUBSET OF MULTIPLE DATES, THE RESULTS IN TERM OF REGRESSION ARE PRETTY MISERABLE. LET SEE SINGLE DATES
SF0312 = SF.class[SF.class$date == "2012-03-15",] #21 samplers, buono dai
SF0312.lm <- lm(vals ~ DBH2, data=SF0312)
summary(SF0312.lm) #Adjusted R-squared:  -0.05195 orrore vero
SF0513 = SF.class[SF.class$date == "2013-05-23",] #21 samplers, buono dai
SF0513.lm <- lm(vals ~ DBH2, data=SF0513)
summary(SF0513.lm) # altrettanto horrible!!!

###########################################################################
#                     PLOT DBH2~SF FOR CHAPTER 3
###########################################################################

SFdry2013 = SF.class[SF.class$date== "2013-06-20",]
SFwet2013 = SF.class[SF.class$date== "2015-05-19",]
SFint2013 = SF.class[SF.class$date== "2014-09-23",] 
SF.DBH = rbind(SFdry2013,SFwet2013,SFint2013)
SF.DBH = droplevels(SF.DBH)

# create the regression equations: https://stackoverflow.com/questions/7549694/adding-regression-line-equation-and-r2-on-graph
lm_eqn <- function(SF.DBH){
  m <- lm(vals ~ DBH2, SF.DBH);
  eq <- substitute(italic(vol) == a + b %.% italic(paste("DBH"^"2"))*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

library(plyr)

split_regr = ddply(SF.DBH, c("date"), lm_eqn)


ggplot(data = SF.DBH, aes(x = DBH2, y = vals)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(date ~ .) +
  geom_text(data=split_regr, aes(x=c(220,225,225), y=c(32,32,32), label=V1), 
            parse = TRUE, inherit.aes = FALSE,show.legend=F) +
  theme(panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  labs(x = expression(paste("DBH"^"2"~"(cm"^"2"*")")), y = " stemflow volume (L)") + # good expression example for cm^2
  theme_bw(base_size = 14) +
  theme(plot.title=element_text(size = 16, hjust = 0.5))



