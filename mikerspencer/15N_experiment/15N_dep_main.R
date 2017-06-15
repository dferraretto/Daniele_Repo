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

#### NOTE: all NH4 and NO3 have been cleaned by blanks in the .csv file  ########

# Trees canopy surface (m2) as from Excel calculations - "STAR" model simplified
T1.area = 11.8
T2.area = 11.5
T3.area = 13.8
T1.TF.coll = 0.9472
T2.TF.coll = 0.9457
T3.TF.coll = 0.9390


# select the replicate for the T1 samples:
# by creating a df with the rows to be removed
TF.2.remove <- rbind(N_Ndep_15N_simplified[ which(N_Ndep_15N_simplified$Date =='2016-08-10'
                         & N_Ndep_15N_simplified$Sample == "T1"), ],
                     N_Ndep_15N_simplified[ which(N_Ndep_15N_simplified$Date =='2016-08-10'
                                     & N_Ndep_15N_simplified$Sample == "T2"), ],
                     N_Ndep_15N_simplified[ which(N_Ndep_15N_simplified$Date =='2016-08-10'
                                     & N_Ndep_15N_simplified$Sample == "T3"), ])

TF.2.remove = TF.2.remove[is.na(TF.2.remove$Replicate), ]

# remove rows by anti_join (deletes rows on 1 db based on a 2nd db with one or MORE columns in common)
library(dplyr)
N_Ndep_15N_simplified = anti_join(N_Ndep_15N_simplified, TF.2.remove, by = c("Sample", "Date", "Replicate"))

# calculate N masses

N_Ndep_15N_simplified$NH4.N = N_Ndep_15N_simplified$NH4*N_Ndep_15N_simplified$Volume*14/18

N_Ndep_15N_simplified$NO3.N = N_Ndep_15N_simplified$NO3*N_Ndep_15N_simplified$Volume*14/62

# crop the NAs (to get rid of ghost NAs)
N_Ndep_15N_simplified = N_Ndep_15N_simplified[complete.cases(N_Ndep_15N_simplified[, 4:5]),] #this detects those rows where NH4 and NO3 = NA

# crop all samples but T13Tn, TF and SF

TFSF.15N = N_Ndep_15N_simplified[which(N_Ndep_15N_simplified$Sample == "T1" | N_Ndep_15N_simplified$Sample == "T2" | N_Ndep_15N_simplified$Sample == "T3"
                                       | N_Ndep_15N_simplified$Sample == "S1"| N_Ndep_15N_simplified$Sample == "S2"| N_Ndep_15N_simplified$Sample == "S3"),]
# | N_Ndep_15N_simplified$Sample == "T13T1" | N_Ndep_15N_simplified$Sample == "T13T2" | N_Ndep_15N_simplified$Sample == "T13T3"

# drop all but masses
TFSF.15N = TFSF.15N[ , c(1,2,18,19)]

#substring Samples to the number of trees
TFSF.15N$tree = substr(TFSF.15N$Sample, start=2, stop=2)


# sum N masses by tree
library(reshape2)
TFSF.15N.long = melt(TFSF.15N, id.vars = c("Date", "Sample", "tree"))
TF15 = TFSF.15N.long[which(TFSF.15N.long$Sample== "T1" | TFSF.15N.long$Sample== "T2" | TFSF.15N.long$Sample== "T3"), ]
SF15 = TFSF.15N.long[which(TFSF.15N.long$Sample== "S1" | TFSF.15N.long$Sample== "S2" | TFSF.15N.long$Sample== "S3"), ]


# order the 2 subsets by date and sample
TF15 = TF15[with(TF15, order(Date, Sample, tree)), ]
SF15 = SF15[with(SF15, order(Date, Sample, tree)), ]

# merge TF and SF
TFSF.Nx = merge(TF15, SF15, by = c("Date", "variable", "tree"), all = FALSE)

# Scaling the masses to each tree canopy (https://stackoverflow.com/questions/29709248/multiplying-column-value-by-another-value-depending-on-value-in-certain-column-r)
TFSF.Nx = transform(TFSF.Nx, newvalue.x=ifelse(tree==1, value.x*T1.area / T1.TF.coll, 
                              ifelse(tree==2, value.x*T2.area / T2.TF.coll,
                              ifelse(tree==3, value.x*T3.area / T3.TF.coll, NA))))
TFSF.Nx = transform(TFSF.Nx, newvalue.y=ifelse(tree==1, value.y*T1.area / T1.TF.coll, 
                              ifelse(tree==2, value.y*T2.area / T2.TF.coll,
                              ifelse(tree==3, value.y*T3.area / T3.TF.coll, NA))))


TFSF.Nx = TFSF.Nx[ , c(1:3,8,9)]
names(TFSF.Nx) = c("date","N_form", "tree", "TF_N","SF_N")

# overall N (TF+SF) by form
TFSF.Nx$Nmass = TFSF.Nx$TF_N + TFSF.Nx$SF_N

# Define season
TFSF.Nx$season <- "summer"
TFSF.Nx$season[TFSF.Nx$date > "2016-12-31"] <- "winter"

summer.Nx = TFSF.Nx[TFSF.Nx$season == "summer",]
winter.Nx = TFSF.Nx[TFSF.Nx$season == "winter",]

# Create a cumulative Nmass column - to be calculated after splitting the two seasonal sets
# OR to be checked as a whole by summing the two treatments N masses

require(data.table)
summer.Nx <- data.table(summer.Nx)
summer.Nx = summer.Nx[, Cum.Sum := cumsum(Nmass),by=list(N_form, tree)]
winter.Nx <- data.table(winter.Nx)
winter.Nx = winter.Nx[, Cum.Sum := cumsum(Nmass), by=list(N_form, tree)]

rm(SF15, TF15, TF.2.remove, TFSF.15N.long, TFSF.15N, TFSF.Nx)

##########################     PLOT N mass
library(ggplot2)

#extract the application N data

N_app = N_Ndep_15N_simplified[grep("T13T", N_Ndep_15N_simplified$Sample),c(1,2,4,5)]
N_app$NH4.N = N_app$NH4_lab*14/18
N_app$NO3.N = N_app$NO3_lab*14/62
N_app =N_app[ , c(1,2,5,6)]

library(reshape2)
N_app_long <- melt(N_app, id.vars = c("Date", "Sample"))
N_app_sum = N_app_long[N_app_long$Date == "2016-08-05", ]


# cumulative N per application
dummy1 <- data.frame(X = c("NH4.N", "NO3.N"), Z = c(1335.444, 1332.710))
dummy2 <- data.frame(X = c("NH4.N", "NO3.N"), Z = c(1494.889, 1491.903))


ggplot(data = summer.Nx, aes (date, Cum.Sum)) + geom_smooth(color = 'red4') + facet_grid(N_form ~ .) +
    ggtitle("Cumulative N mass collected in throughfall \nand stemflow after the winter application") +
    labs( x = "date", y = "cumulative N mass (mg/tree)") + theme_bw(base_size = 12) +
    geom_hline(data = dummy1, aes(yintercept = Z), color = "red3") +
      theme(plot.title = element_text(hjust = 0.1, size = 18, colour = 'red4'),
          plot.background = element_rect(fill = "transparent",colour = NA)) 


ggplot(data = winter.Nx, aes (date, Cum.Sum)) + geom_smooth(color = 'red4') + facet_grid(N_form ~ .) +
    ggtitle("Cumulative N mass collected in throughfall \nand stemflow after the winter application") +
  labs( x = "date", y = "cumulative N mass (mg/tree)") + theme_bw(base_size = 12) +
  geom_hline(data = dummy2, aes(yintercept = Z), color = "blue", linetype = 3, size = 1) +
  theme(plot.title = element_text(hjust = 0.1, size = 18, colour = 'red4'),
        plot.background = element_rect(fill = "transparent",colour = NA)) 



# OK ho messo una linea che poi e' il valore spruzzato su T1. Come ci metto anche T2 e T3 come se fossero alto e basso?
# OPPURE: Creo un cumulativo di spraying iniziale piu' valore di RF! mmmm, complicato pero' ne ho pochissimi e 
# non ho la profondita' quindi il vol/m.
# Ne esco meglio mostrando dove control e treatment si equivalgono e quindi ci metto una linea verticale























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
  







