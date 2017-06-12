--------------------------------------------------------
  #  2 plot in croce per EGU
  #  started on April 13th 2017
  #  updated: 
  # --------------------------------------------------------
# --------------------------------------------------------
# clear the memory
rm(list=ls())

.libPaths("C:/Workspace/R") # adjust the issues with my desktop demmerda
### set working dir for pc:
#setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
#setwd("M:/My PhD/R/PhD-local_repo")
setwd("C:/Users/s1373890/Daniele_Repo")

library(readr)
N_Ndep_15N_simplified <- read_csv("C:/Users/s1373890/Daniele_Repo/15N_experiment/15N_fieldlab/15N_Ndep_simplified.csv")

# Trees canopy surface (m2) as from Excel calculations - "STAR" model simplified
T1.area = 11.8
T2.area = 11.5
T3.area = 13.8
T1.TF.coll = 0.9472
T2.TF.coll = 0.9457
T3.TF.coll = 0.9390


# select the replicate for the T1 samples:
# by creating a df with the rows to be removed
TF.2.remove <- rbind(TFSF15.N[ which(TFSF15.N$Date =='2016-08-10'
                         & TFSF15.N$Sample == "T1"), ],
                     TFSF15.N[ which(TFSF15.N$Date =='2016-08-10'
                                     & TFSF15.N$Sample == "T2"), ],
                     TFSF15.N[ which(TFSF15.N$Date =='2016-08-10'
                                     & TFSF15.N$Sample == "T3"), ])

TF.2.remove = TF.2.remove[is.na(TF.2.remove$Replicate), ]

# remove rows by anti_join (deletes rows on 1 db based on a 2nd db with one or MORE columns in common)
library(dplyr)
N_Ndep_15N_simplified = anti_join(N_Ndep_15N_simplified, TF.2.remove, by = c("Sample", "Date", "Replicate"))

# calculate N masses

N_Ndep_15N_simplified$NH4.N = N_Ndep_15N_simplified$NH4*N_Ndep_15N_simplified$Volume*14/18

N_Ndep_15N_simplified$NO3.N = N_Ndep_15N_simplified$NO3*N_Ndep_15N_simplified$Volume*14/62

# crop all samples but T13Tn, TF and SF

TFSF.15N = N_Ndep_15N_simplified[which(N_Ndep_15N_simplified$Sample == "T1" | N_Ndep_15N_simplified$Sample == "T2" | N_Ndep_15N_simplified$Sample == "T3"
                                       | N_Ndep_15N_simplified$Sample == "S1"| N_Ndep_15N_simplified$Sample == "S2"| N_Ndep_15N_simplified$Sample == "S3"
                                       | N_Ndep_15N_simplified$Sample == "T13T1" | N_Ndep_15N_simplified$Sample == "T13T2" | N_Ndep_15N_simplified$Sample == "T13T3"),]

# drop all but masses
TFSF.15N = TFSF.15N[ , c(1,2,16,17)]


# sum N masses by tree
library(reshape2)
TFSF.15N.long = melt(TFSF.15N, id.vars = c("Date", "Sample"))
TFSF.15 = dcast(TFSF.15N.long, Date ~ Sample + variable , value.var = "value")
# get rid and quick
TFSF15.N = TFSF15.N[, c(1,2,5,12,13,14,15,16,17)]

d15N.db = TFSF15.N[, c(1,2,6,7)]

library(reshape2)

long.d15N = melt(d15N.db, id = c("Date", "Sample"))
summary(long.d15N)
long.d15N$Date = as.Date(long.d15N$Date)
# reorder dates
dates <- unique(sort(long.d15N$Date))
long.d15N$Date <- factor(long.d15N$Date, labels = dates,  ordered = T)
long.d15N$Date=as.Date(long.d15N$Date)
long.d15N$Sample = as.factor(long.d15N$Sample)
library(ggplot2)

##########################     PLOT d15N


plot.d15N = ggplot(data = long.d15N, aes (Date, value, colour = Sample)) + geom_point(size = 2)

#orange.bold.text <- element_text(face = "bold", color = "orange") # per bold AND italic: bold.italic
#orange.text <- element_text(face = "plain", color = "orange")
Edired.text <- element_text(face = "plain", color = "#DF0057")
# cols <- c("TF1" = "red4", "TF2" = "red3", "TF3" = "red", "SF1" = "grey20", "SF2" = "grey40", "SF3" = "grey75")
plot.d15N +  facet_grid(variable ~ .) +
  scale_colour_manual(values = c("red4", "red3", "red", "grey20","grey40", "grey75"),
                      labels = c('TF1', 'TF2','TF3', 'SF1', 'SF2', 'SF3')) +
  ggtitle(expression(paste(delta, {15},"N in throughfall and stemflow after the first application"))) +
  labs( x = "date", y = expression(paste(delta, {15},"N (\u2030)"))) + theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.1, size = 18, colour = '#DF0057'),
        plot.background = element_rect(fill = "transparent",colour = NA)) 
  







