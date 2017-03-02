#############################################
#
#         Checking Rob RF data              #
#
#############################################

# clear the memory
rm(list=ls())

.libPaths("C:/Workspace/R")

# 1st file (around 38k entries): histograms do not seem to help to understand how the data are distributed...
# NOT NEEDED ANYMORE
Rob1st <- read.csv("~/My PhD/R/PhD-local_repo/Rob RF data/C00m precip_long.csv")
summary(Rob1st)
Rob1st$Date.Time = as.Date(Rob1st$Date.Time, format = "%d/%m/%Y")
daily.Rob1st = aggregate(value ~ Date.Time + Plot, data = Rob1st, FUN=sum, na.rm=TRUE)
summary(daily.Rob1st)
# check frequences: 
plot1stdata <- ggplot(daily.Rob1st, aes(value)) +
geom_histogram(breaks=c(1:73)) + ggtitle("Rob precipitation data - 1st file") # me par ben!!! [which(daily.Rob1st$value>0),]
# without 0s:
dailyRob1st.filtered = daily.Rob1st[which(daily.Rob1st$value>0),]
summary(dailyRob1st.filtered)
# check frequences: 
plot1stdata.no0 <- ggplot(dailyRob1st.filtered, aes(value)) +
  geom_histogram(breaks=c(-1:73)) + ggtitle("Rob precipitation data - 1st file no 0s") # me par ben!!! [which(daily.Rob1st$value>0),]


############                2ND FILE from Rob (about 48k entries)              ####################

Gfert_precip_long <- read.csv("~/My PhD/R/PhD-local_repo/Rob RF data/Gfert_precip_long.csv")


# class(Gfert_precip_long$Date.Time) - DAILY CHECK - NOT NEEDED FOR COMPARISONS WITH MY DATA
Gfert_precip_long$Date.Time = as.Date(Gfert_precip_long$Date.Time, format = "%d/%m/%Y")
daily.Gfert_precip_long = aggregate(precip ~ Date.Time + Plot, data = Gfert_precip_long, FUN=sum, na.rm=TRUE)
summary(daily.Gfert_precip_long)
hist(daily.Gfert_precip_long$value, breaks=c(1:73), main="Distribution of value", xlab="value")
# check frequences: 
plot2nddata <- ggplot(daily.Gfert_precip_long[which(daily.Gfert_precip_long$value>0),], aes(value)) +
  geom_histogram(breaks=c(1:73)) + ggtitle("Rob precipitation data - 2nd file") # me par ben!!!

# plot

library(ggplot2)

ggplot(data=daily.Gfert_precip_long, aes(x=Date.Time, y= precip)) + geom_line(aes(colour = Plot, group = Plot), alpha = 0.4) +
  labs(x = "Date", y = expression(precip~ depth~"(mm"~~day^"-1"*")")) + theme(
    #axis.text = element_text(size = 14),
    #legend.key = element_rect(fill = daily.Gfert_precip_long$Plot),
    # legend.background = element_rect(),
   # legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "grey80"),
    #panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")) + 
  scale_color_manual(aes(colour = Plot), values = c("control" = "black", "treatment" = "red3"))

# NB: alpha needs to be outsde of aes, otherwise a legend is created! 
# NB2: the apparent lack of colours in the legend is only apparent! colours are visible when zooming or exporting the graph!!!
  

# If I needed to work on the Control/ treatment series separately:

controlnew = daily.Gfert_precip_long[which(daily.Gfert_precip_long$Plot =="Control"),]

treatmentnew = Gfert_precip_long[which(Gfert_precip_long$Plot =="Treatment"),]

summary(treatmentnew)


#  MONTHLY ACCUNULATED DATA - TO BE COMPARED WITH GRIFFIN DATA

Gfert_precip_long$Date.Time = as.Date(Gfert_precip_long$Date.Time, format = "%d/%m/%Y")

Gfert_precip_long$date = format(Gfert_precip_long$Date.Time, "%Y%m")


monthly.Gfert_precip_long = aggregate(precip ~ date + Plot, data = Gfert_precip_long, FUN=sum, na.rm=TRUE)

#summary(monthly.Gfert_precip_long)

#monthly.Gfert_precip_long = monthly.Gfert_precip_long[ , c(4,5,6)]

library(zoo)

# turn a mmYYYY character into a date: (had to spit blood on this!)

monthly.Gfert_precip_long$date = as.yearmon(as.character(monthly.Gfert_precip_long$date), "%Y%m")

class(monthly.Gfert_precip_long$date)


# plot (anche questo capolavoro, realizzato con x espresso come yearmon object)
library(ggplot2)

ggplot(data=monthly.Gfert_precip_long, aes(x=as.factor(as.yearmon(date)))) + geom_line(aes(y = precip, colour = Plot, group = Plot),  alpha = 0.4) +
  labs(x = "Date", y = expression(precip~ depth~"(mm"~~month^"-1"*")")) + theme(
    #axis.text = element_text(size = 14),
    #legend.key = element_rect(fill = daily.Gfert_precip_long$Plot),
    # legend.background = element_rect(),
    # legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "grey80"),
    #panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_color_manual(aes(colour = Plot), values = c("control" = "black", "treatment" = "red3"))

# COOL! ORA IMPORTIAMO I MONTHLY DATA FROM GRIFFIN RF C30 E C31 FROM MAY 2012 TO SEP 2014. E FACCIAMOCI UN GRAFICO!

library(RSQLite)

db = dbConnect(SQLite(), dbname="field_lab/Daily_Griffin.SQLite")

