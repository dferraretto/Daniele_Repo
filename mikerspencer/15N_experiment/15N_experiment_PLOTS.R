--------------------------------------------------------
  #  PLOTS for both winter and summer application
  #  started on April 13th 2017
  #  updated: 04/08/2017
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
N_Ndep_15N_simplified <- read_csv("C:/Users/s1373890/Daniele_Repo/15N_experiment/15N_fieldlab/15N_Ndep_simplified_15N_complete.csv")

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

# crop all samples but TF and SF

TFSF.15N = N_Ndep_15N_simplified[which(N_Ndep_15N_simplified$Sample == "T1" | N_Ndep_15N_simplified$Sample == "T2" | N_Ndep_15N_simplified$Sample == "T3"
                                       | N_Ndep_15N_simplified$Sample == "S1"| N_Ndep_15N_simplified$Sample == "S2"| N_Ndep_15N_simplified$Sample == "S3"),]
# | N_Ndep_15N_simplified$Sample == "T13T1" | N_Ndep_15N_simplified$Sample == "T13T2" | N_Ndep_15N_simplified$Sample == "T13T3"

# drop all but masses
TFSF.15N = TFSF.15N[ , c(1,2,16,17)]

#substring Samples to the code (1-2-3) of trees
TFSF.15N$tree = substr(TFSF.15N$Sample, start=2, stop=2)


# sum N masses by tree:

library(reshape2)
TFSF.15N.long = melt(TFSF.15N, id.vars = c("Date", "Sample", "tree"))
TF15 = TFSF.15N.long[which(TFSF.15N.long$Sample== "T1" | TFSF.15N.long$Sample== "T2" | TFSF.15N.long$Sample== "T3"), ]
SF15 = TFSF.15N.long[which(TFSF.15N.long$Sample== "S1" | TFSF.15N.long$Sample== "S2" | TFSF.15N.long$Sample== "S3"), ]


# order the 2 subsets by date and sample
TF15 = TF15[with(TF15, order(Date, Sample, tree)), ]
SF15 = SF15[with(SF15, order(Date, Sample, tree)), ]

# merge TF and SF
TFSF.Nx = merge(TF15, SF15, by = c("Date", "variable", "tree"), all = FALSE)

# SUBTRACTING CONTROLS IN ORDER TO TAKE INTO ACCOUNT?


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

winter.Nx = aggregate(Nmass ~ date + N_form + tree,  data = winter.Nx, FUN = mean)
winter.Nx <- data.table(winter.Nx) # NOTA: Cum.Sum si comporta stranamente. Non si aggiorna nella View ed e' sensibile a quando viene lanciato (basta che data.table venga applicato prima di aggregate e Cum.Sum non viene eseguito, da errore)
winter.Nx = winter.Nx[, Cum.Sum := cumsum(Nmass),by=list(N_form, tree)]

rm(SF15, TF15, TF.2.remove, TFSF.15N.long, TFSF.15N, TFSF.Nx)

###############################################################
#                     Griffin data                            #
#               for the summer application                    #
# rerun RFTSW_monthly to then extract Aug-Dec 2016 data       #

source("mikerspencer/RFTSW_monthly_for_15N.R")

long.N.RFTSW$mY = as.Date(long.N.RFTSW$mY)
long.N.RFTSW = long.N.RFTSW[long.N.RFTSW$mY> "2016-07-01" & long.N.RFTSW$mY<"2017-01-01",]

# %like requires data.table package
library(data.table)

N.in.summer <- long.N.RFTSW[long.N.RFTSW$variable %like% "input",]

N.in.summer <- subset(N.in.summer, variable != "total.Ninput")
N.in.summer = droplevels(N.in.summer)
N.in.summer$key = substr(N.in.summer$variable, start=1, stop=3)

# install.packages("timeDate")
library(timeDate)

N.in.summer$date= timeLastDayInMonth(N.in.summer$mY)
N.in.summer = N.in.summer[, c("date","key", "value")]

# repeating rows and add tree column

N.in.summer = N.in.summer[rep(seq_len(nrow(N.in.summer)), each=3),]
N.in.summer$tree = c(1,2,3)
N.in.summer$value = as.numeric(N.in.summer$value)
N.in.summer$date = as.Date(N.in.summer$date)
# Scaling the masses to each tree canopy (scaling from ha to m2 and from kg to mg -> /10000*1000000)
# (https://stackoverflow.com/questions/29709248/multiplying-column-value-by-another-value-depending-on-value-in-certain-column-r)
N.in.summer = transform(N.in.summer, value=ifelse(tree==1, value/10000*1000000*T1.area, 
                                    ifelse(tree==2, value/10000*1000000*T2.area,
                                           ifelse(tree==3, value/10000*1000000*T3.area, NA))))


#  Creating a data frame with Griffin input vals and the N application for the SUMMER application

summer_application = N_Ndep_15N_simplified[N_Ndep_15N_simplified$Date == "2016-08-05", ]
summer_application = summer_application [ , c(1,2,4,5)]

summer_application$Sample <- factor(summer_application$Sample, labels = c(1,2,3))

## Prepare application df to be added to N.in:
#  from Nx to Nx-N
summer_application$NH4 = summer_application$NH4*14/18
summer_application$NO3 = summer_application$NO3*14/62
#wide to long
library(reshape2)
summer_application.long = melt(summer_application, id.vars = c("Date", "Sample"))
#rename cols
names(summer_application.long) = c("date", "tree",  "key","value")

# adding application.long to the N.in db
N.input.summer = rbind(N.in.summer, summer_application.long)
N.input.summer$date = as.Date(N.input.summer$date)
N.input.summer$key = as.factor(N.input.summer$key)
N.input.summer$tree = as.factor(N.input.summer$tree)
N.input.summer = N.input.summer[with(N.input.summer, order(date,tree,key)), ]

# creating the cumulative column:

N.input.summer <- data.table(N.input.summer)
N.input.summer = N.input.summer[, Cum.Sum := cumsum(value),by=list(tree,key)]

###############################################################
#                     Griffin data                            #
#               for the winter application                    #
# rerun RFTSW_monthly to then extract Aug-Dec 2016 data       #

source("mikerspencer/RFTSW_monthly_for_15N.R")
# pausa finche non carico i dati di aprile diobo
long.N.RFTSW$mY = as.Date(long.N.RFTSW$mY)
long.N.RFTSW = long.N.RFTSW[long.N.RFTSW$mY> "2017-02-27" & long.N.RFTSW$mY<"2017-04-07",]

# %like requires data.table package
library(data.table)

N.in.win <- long.N.RFTSW[long.N.RFTSW$variable %like% "input",]

N.in.win <- subset(N.in.win, variable != "total.Ninput")
N.in.win = droplevels(N.in.win)
N.in.win$key = substr(N.in.win$variable, start=1, stop=3)

# install.packages("timeDate")
library(timeDate)

N.in.win$date= timeLastDayInMonth(N.in.win$mY)
N.in.win = N.in.win[, c("date","key", "value")]

# repeating rows and add tree column

N.in.win = N.in.win[rep(seq_len(nrow(N.in.win)), each=3),]
N.in.win$tree = c(1,2,3)
N.in.win$value = as.numeric(N.in.win$value)
N.in.win$date = as.Date(N.in.win$date)
# Scaling the masses to each tree canopy (scaling from ha to m2 and from kg to mg -> /10000*1000000)
# (https://stackoverflow.com/questions/29709248/multiplying-column-value-by-another-value-depending-on-value-in-certain-column-r)
N.in.win = transform(N.in.win, value=ifelse(tree==1, value/10000*1000000*T1.area, 
                                    ifelse(tree==2, value/10000*1000000*T2.area,
                                           ifelse(tree==3, value/10000*1000000*T3.area, NA))))


#  Creating a data frame with Griffin input vals and the N application

winter_application = N_Ndep_15N_simplified[N_Ndep_15N_simplified$Date == "2017-02-28", ]
winter_application = winter_application [ , c(1,2,4,5)]

winter_application$Sample <- factor(winter_application$Sample, labels = c(1,2,3))

## Prepare application df to be added to N.in:
#  from Nx to Nx-N
winter_application$NH4 = winter_application$NH4*14/18
winter_application$NO3 = winter_application$NO3*14/62
#wide to long
library(reshape2)
winter_application.long = melt(winter_application, id.vars = c("Date", "Sample"))
#rename cols
names(winter_application.long) = c("date", "tree",  "key","value")

# adding application.long to the N.in db
N.input.winter = rbind(N.in.win, winter_application.long)
N.input.winter$date = as.Date(N.input.winter$date)
N.input.winter$key = as.factor(N.input.winter$key)
N.input.winter$tree = as.factor(N.input.winter$tree)
N.input.winter = N.input.winter[with(N.input.winter, order(date,tree,key)), ]

# creating the cumulative column:

N.input.winter <- data.table(N.input.winter)
N.input.winter = N.input.winter[, Cum.Sum := cumsum(value),by=list(tree,key)]


########################    PLOTS    #############################

##################################################################
# PLOT comparison between the Griffin data and the 15N-plot N data
##################################################################


# prepare the faceting factor for a parsed labelling
N.inout <- long.N.RFTSW[c(long.N.RFTSW$variable %like% "input" |long.N.RFTSW$variable %like% "output") ,]

N.inout <- subset(N.inout, variable != "total.Ninput")
N.inout = droplevels(N.inout)
N.inout$key = substr(N.inout$variable, start=1, stop=3)


N.inout$key <- factor(N.inout$key, labels = c("NH[4]-N","NO[3]-N"))

  ctrl1 = ggplot(data = N.inout, aes (month, value, fill = variable)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values = c("cadetblue4", "cadetblue2", "Dark Green", "Yellow Green"), name = "N flux \nand form", 
                      labels = c(expression(~N[dep]~NH[4]*-N), expression(~N[dep]~NO[3]*-N), 
                                 expression(~TF+SF~NH[4]*-N), expression(~TF+SF~NO[3]*-N))) +
    facet_grid(key ~ ., labeller = label_parsed) + ggtitle(expression(~N[dep]*~"and N flux under the canopy")) +
    labs( x = "Month", y = expression(N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) + theme_bw(base_size = 11)+
    theme(panel.border = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA), 
          plot.title = element_text(hjust = 0.5, size = 16, colour = 'red4')) 
  
  # Calculate a Control cumulative value to compare my new data
  
  N.inout$csum <- ave(N.inout$value, N.inout$variable, FUN=cumsum)
  
  # Plot'em!

  ctrl2 = ggplot(data = N.inout, aes (month, csum, fill = variable)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values = c("cadetblue4", "cadetblue2", "Dark Green", "Yellow Green"), name = "N flux \nand form", 
                      labels = c(expression(~N[dep]~NH[4]*-N), expression(~N[dep]~NO[3]*-N), 
                                 expression(~TF+SF~NH[4]*-N), expression(~TF+SF~NO[3]*-N))) +
    facet_grid(key ~ ., labeller = label_parsed) + ggtitle(expression("Cumulative"~N[dep]*~"and N content under the canopy")) +
    labs( x = "Month", y = expression(Cumulative~N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) + theme_bw(base_size = 11) +
    theme(panel.border = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA), 
          plot.title = element_text(hjust = 0.5, size = 16, colour = 'red4')) 
  

  ################################################################
  #   PLOT CUMULATED N MASS by APPLICATION AND collection date:
  ################################################################
  
#extract the application N data

N_app = N_Ndep_15N_simplified[grep("T13T", N_Ndep_15N_simplified$Sample),c(1,2,4,5)]
N_app$NH4.N = N_app$NH4*14/18
N_app$NO3.N = N_app$NO3*14/62
N_app =N_app[ , c(1,2,5,6)]

library(reshape2)
N_app_long <- melt(N_app, id.vars = c("Date", "Sample"))
N_app_sum = N_app_long[N_app_long$Date == "2016-08-05", ]
N_app_win = N_app_long[N_app_long$Date == "2017-02-28", ]

##########################################################
# PLOT 1: cumulative N per application
dummy1.0 <- data.frame(X = c("NH4.N", "NO3.N"), Z = c(1335.444, 1332.710), variable = "1st_app")
dummy1.1 <- data.frame(X = c("NH4.N", "NO3.N"), Z = c(1246.000, 1243.065), variable = "2nd_app")
dummy1.2 <- data.frame(X = c("NH4.N", "NO3.N"), Z = c(1403.889, 1400.903), variable = "3rd_app")
a = c(1335.444, 1332.710)
b = c(1246.000, 1243.065)
c = c(1403.889, 1400.903)

dummy2.0 <- data.frame(X = c("NH4.N", "NO3.N"), Z = c(1494.889, 1491.903))

summer.Nx$N_form <- factor(summer.Nx$N_form, labels = c("NH[4]-N","NO[3]-N"))
# plot with application lines only
 plot0 = ggplot(data = summer.Nx, aes (date, Cum.Sum)) + geom_smooth() + 
   facet_grid(key ~ ., labeller = label_parsed) +
   ggtitle("Cumulative N mass collected in throughfall \nand stemflow after the summer application") +
   labs(x = "date", y = "cumulative N mass (mg/tree)") + theme_bw(base_size = 11) +
   scale_fill_manual(values = c('NH[4]-N' = "lightskyblue",'NO[3]-N' = "mediumblue", 
                                '1st_app' = "red4", '2nd_app' = "black", '3rd_app' = "black"),
                     labels = c(expression(~NH[4]*-N), expression(~NO[3]*-N), expression(1{st}~app~NH[4]*-N), 
                                expression(1{st}~app~NO[3]*-N), expression(2{nd}~app~NH[4]*-N), expression(2{nd}~app~NO[3]*-N),
                                expression(3{rd}~app~NH[4]*-N), expression(3{rd}~app~NO[3]*-N),
                     name = "N applied and collected by form")) +
    geom_hline(data = dummy1.0, aes(yintercept = Z), size = 1) +
    geom_hline(data = dummy1.1, aes(yintercept = Z), linetype = 3, size = 0.8) +
    geom_hline(data = dummy1.2, aes(yintercept = Z), linetype = 3, size = 0.8) +
      theme(plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
          plot.background = element_rect(fill = "transparent",colour = NA)) 
 
 ################################################################################
 
# create a variable column to apply labels
 
N.input.winter$variable = NA

N.input.winter = transform(N.input.winter, variable=ifelse(key=="NH4", "NH4.input", 
                                      ifelse(key=="NO3", "NO3.input", NA)))
 
N.input.winter$key <- factor(N.input.winter$key, labels = c("NH[4]-N","NO[3]-N"))
 
names(winter.Nx)[names(winter.Nx)=="N_form"] <- "key"

winter.Nx$variable = NA

#back to simple levels of key
winter.Nx$key <- factor(winter.Nx$key, labels = c("NH4","NO3"))

winter.Nx = transform(winter.Nx, variable=ifelse(key=="NH4", "NH4.output", 
                                             ifelse(key=="NO3", "NO3.output", NA)))
winter.Nx$key <- factor(winter.Nx$key, labels = c("NH[4]-N","NO[3]-N"))

# calculate Cum.Sum for outputs



 plot1 = ggplot() + 
   geom_smooth(data = winter.Nx, aes (date, Cum.Sum, fill = variable, color = variable)) + 
   geom_smooth(data = N.input.winter, aes (date, Cum.Sum, fill = variable, color = variable)) +
   facet_grid(key ~ ., labeller = label_parsed) +
   ggtitle("Cumulative N mass collected in throughfall \nand stemflow after the summer application") +
   labs(x = "date", y = "cumulative N mass (mg/tree)") + theme_bw(base_size = 11) +
     scale_fill_manual(values = c('NH4.output' = "olivedrab3",'NO3.output' = "seagreen4",'NH4.input' = "steelblue",'NO3.input' = "mediumblue"),
                     labels = c('NH4.output' = expression(~~TF+SF~NH[4]*-N), 'NO3.output' = expression(~~TF+SF~NO[3]*-N), 
                                'NH4.input' = expression(~N[dep]+Treatm.~NH[4]*-N), 'NO3.input' = expression(~~N[dep]+Treatm.~NO[3]*-N)), 
                     name = "applied and \ncollected N \nby form") +
     scale_color_manual(values = c('NH4.output' = "olivedrab3",'NO3.output' = "seagreen4",'NH4.input' = "steelblue",'NO3.input' = "mediumblue"),
                       labels = c('NH4.output' = expression(~~TF+SF~NH[4]*-N), 'NO3.output' = expression(~~TF+SF~NO[3]*-N), 
                                  'NH4.input' = expression(~N[dep]+Treatm.~NH[4]*-N), 'NO3.input' = expression(~~N[dep]+Treatm.~NO[3]*-N)), 
                       name = "applied and \ncollected N \nby form") +
   theme(plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
         plot.background = element_rect(fill = "transparent",colour = NA)) 
 
   
   # per aggiungere un text per ogni facet grid qui lo spiega meglio https://stackoverflow.com/questions/20428902/geom-text-writing-all-data-on-all-facets
 # ma comunque e' da farsi scoppiare la testa a cazzo, tanto vale creare la curva cumulata
 

ggplot(data = winter.Nx, aes (date, Cum.Sum)) + geom_smooth(color = 'midnightblue') + facet_grid(N_form ~ .) +
    ggtitle("Cumulative N mass collected in throughfall \nand stemflow after the winter application") +
  labs( x = "date", y = "cumulative N mass (mg/tree)") + theme_bw(base_size = 11) +
  geom_hline(data = dummy2.0, aes(yintercept = Z), color = "red3", linetype = 3, size = 1) +
  theme(plot.title = element_text(hjust = 0.1, size = 16, colour = 'midnightblue'),
        plot.background = element_rect(fill = "transparent", colour = NA)) 

rm(dummy1.0, dummy1.1, dummy1.2, dummy2.0, N_app, N_app_long,N_app_sum, N_app_win,
   T1.area, T1.TF.coll, T2.area, T2.TF.coll, T3.area, T3.TF.coll)

# NOTES
## Winter plots taking into account of the relevant natural deposition of the period. Hence the CTF concentration will be 
## subtracted from the values of March (collection 1 and 2). BOH

# Pensavo di mostrare dove control e treatment si equivalgono e quindi ci metto una linea verticale
# In realta' col plot 0 mostro la comparazione tra dati della serie storica e dati del 15N plot per far vedere che ci siamo
# come ordine di grandezza


###########################################################################
##########                  PLOT 15N vs N               ###################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                         SUMMER APPLICATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Crate a df with 15N results

d15N.TF.summer = N_Ndep_15N_simplified[which(N_Ndep_15N_simplified$Sample== "T1" | N_Ndep_15N_simplified$Sample== "T2" | N_Ndep_15N_simplified$Sample== "T3"), c(1,2,6,7)]

# Temp: filtering the available dates
d15N.TF.summer = d15N.TF.summer[d15N.TF.summer$Date < "2016-11-01",]

#long:
long.d15N.TF.summer = melt(d15N.TF.summer, id.vars = c("Date", "Sample"))

names(long.d15N.TF.summer) = c("date", "sample", "N_form", "value")

#substring variable to get a common key with season.nx for faceting
long.d15N.TF.summer$key = substr(long.d15N.TF.summer$N_form, start=6, stop=8)

#summer.Nx$key = substr(summer.Nx$N_form, start=1, stop=3)

# box_plot with dates on the x axis (from https://stackoverflow.com/questions/20074061/ggplot2-multiple-factors-boxplot-with-scale-x-date-axis-in-r)
long.d15N.TF.summer$Date <- as.Date(long.d15N.TF.summer$Date, format = "%d/%m/%Y")

#summer.Nx$key <- factor(summer.Nx$key, labels = c("NH[4]-N","NO[3]-N"))
long.d15N.TF.summer$key <- factor(long.d15N.TF.summer$key, labels = c("NH[4]-N","NO[3]-N"))

Edired.text <- element_text(face = "plain", color = "#DF0057")

plot2 = ggplot() + 
  geom_smooth(data = summer.Nx, aes(date, Nmass, fill = N_form)) + 
  facet_grid(key ~ ., labeller = label_parsed) +
  geom_boxplot(data = long.d15N.TF.summer, aes(date, value, fill = N_form, group = interaction(factor(date), N_form, alpha = 0.5))) +
  scale_fill_manual(values = c('d15N_NH4' = "red4",'d15N_NO3' = "pink",'NH4.N' ="lightskyblue",'NO3.N' ="mediumblue"),
                    labels = c(expression(paste(~~~~delta^{15},NH[4]-N,~"in TF")), expression(paste(~~~~delta^{15},NO[3]-N,~"in TF")),
                               expression(~NH[4]*-N), expression(~NO[3]*-N)), 
                    name = "N form \nand isotope") +
  ggtitle(expression(atop("N mass and"~paste(delta^{15},"N (\u2030) collected under"), paste("the canopy after the summer application")))) +
  labs( x = "date", y = "cumulative N mass (mg/tree)") + theme_bw(base_size = 11) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = expression(paste(delta^{15},"N (\u2030)")))) +
  theme(axis.text.y.right = Edired.text, axis.title.y.right = Edired.text, 
        plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
        plot.background = element_rect(fill = "transparent",colour = NA)) 


#library(Rmisc)
#multiplot(ctrl1, ctrl2, plot2, plot1, cols = 2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               WINTER APPLICATION 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Crate a df with 15N results

d15N.TF.winter = N_Ndep_15N_simplified[which(N_Ndep_15N_simplified$Sample== "T1" | N_Ndep_15N_simplified$Sample== "T2" | N_Ndep_15N_simplified$Sample== "T3"), c(1,2,6,7)]

# Temp: filtering the available dates
d15N.TF.winter = subset(d15N.TF.winter, d15N.TF.winter$Date > "2016-12-20")

#long:
long.d15N.TF.winter = melt(d15N.TF.winter, id.vars = c("Date", "Sample"), na.rm = T)

names(long.d15N.TF.winter) = c("date", "sample", "N_form", "value")

#substring variable to get a common key with season.nx for faceting
long.d15N.TF.winter$key = substr(long.d15N.TF.winter$N_form, start=6, stop=8)

#winter.Nx$key = substr(winter.Nx$N_form, start=1, stop=3)

#winter.Nx$key <- factor(winter.Nx$key, labels = c("NH[4]-N","NO[3]-N"))
long.d15N.TF.winter$key <- factor(long.d15N.TF.winter$key, labels = c("NH[4]-N","NO[3]-N"))

plot2 = ggplot() + 
  geom_smooth(data = winter.Nx, aes(date, N_mass, fill = key)) + 
  facet_grid(key ~ ., labeller = label_parsed) +
  geom_boxplot(data = long.d15N.TF.winter, aes(date, value, fill = N_form, group = interaction(factor(date), N_form, alpha = 0.5))) +
  scale_fill_manual(values = c('d15N_NH4' = "red4",'d15N_NO3' = "pink",'NH4.N' ="lightskyblue",'NO3.N' ="mediumblue"),
                    labels = c(expression(paste(~~~~delta^{15},NH[4]-N,~"in TF")), expression(paste(~~~~delta^{15},NO[3]-N,~"in TF")),
                               expression(~NH[4]*-N), expression(~NO[3]*-N)), 
                    name = "N form \nand isotope") +
  ggtitle(expression(atop("N mass and"~paste(delta^{15},"N (\u2030) collected under"), paste("the canopy after the winter application")))) +
  labs( x = "date", y = "cumulative N mass (mg/tree)") + theme_bw(base_size = 11) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = expression(paste(delta^{15},"N (\u2030)")))) +
  theme(axis.text.y.right = Edired.text, axis.title.y.right = Edired.text, 
        plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
        plot.background = element_rect(fill = "transparent",colour = NA)) 

# 07/08/2017: OK, corretto i plot e ottenuto anche il cumulativo invernale e i plot 15N/N invernale, che pero'
# non so a quanto serva. Mi servono
# a) error propagation sul N sperimentale + N Griffin
# b) boh, forse il plot e' meglio tra N-15N cumulato e d15N anziche' come e' adesso tra d15N e N non cumulato? Risp: NO

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 Error propagation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# to be written and later added above