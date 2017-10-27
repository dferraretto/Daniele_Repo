# 15N recovery calculations:
# THIS SHORT SCRIPT CALCULATES THE 15n RECOVERY AS % OF THE APPLIED 15N. 

# NEW "CORRECTED" APPROACH: 15Nx RECOVERED PER EACH COLLECTION = (cd+d/1000)*Nx_recovered, 
# where c = d15N as from the labs, d = R standard.

# I was using a wrong approach based on 15N "mixed into a certain pool with its own N
# winter has been calculated with the 1st date only. The second date wouldn't change the 
# figures much as the N recovered is minimal although the 15N signal is still higher
# than control
rm(list=ls())
d = 0.0036765 # R standard for 15N/14N

# N.15.rec = (b/a)*((c*d+d)/1000)/(1+((c*d+d)/1000))*100

#################             WINTER              ###################
library(readr)
# Import database
winter_recovery <- read_csv("~/Daniele_Repo/15N_experiment/15N_fieldlab/winter_recovery.csv")

# wide to long
library(reshape2)
winter_rec_long = melt(winter_recovery, id.vars = c("Date", "Tree", "N_form", "15N_applied", "N_rec","SD_d15N", "mean_d15N", "SD_Nrec","mean_Nrec"))

# 15N recovery in TF?, %: 
winter_rec_long$N15.rec = ((winter_rec_long$value*d+d)/1000 * winter_rec_long$N_rec)/winter_rec_long$`15N_applied`*100
library(ggplot2)

ggplot(winter_rec_long, aes(x = compartment, y = N15.rec)) + 
  geom_boxplot(aes(fill = N_form)) +
  theme(plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  theme_bw(base_size = 11) +
  scale_fill_manual(values = c('NH4-N' ="red4",'NO3-N' ="pink"),
                    labels = c(expression(~NH[4]*-N), expression(~NO[3]*-N)), 
                    name = "N form") +
  ggtitle(expression({}^15*N~~recovered~"(%) in throughfall after the summer application")) + # HOW TO START a title/label with superscript/subscript numbers: https://stackoverflow.com/questions/33783643/expression-of-italics-and-superscripts-in-ggpot-axis-title
  labs( x = "N form", y = expression({}^15*N~~recovered~"(%)"))


# WINTER ERROR PROPAGATION

N15w.rec.mean <- tapply(winter_rec_long$N15.rec, winter_rec_long$N_form, mean)
# select two rows from N15.rec.mean, one for each unique data (NH4, NO3)
N15.win.mean =  winter_rec_long[c(1,12), c(3,6:9)]

# error propagation IF I only consider the binomy XY (X = d15N recovered, Y = N recovered)
wrong.error15N = N15w.rec.mean * ((N15.win.mean$SD_d15N*2/N15.win.mean$mean_d15N)^2+(N15.win.mean$SD_Nrec*2/N15.win.mean$mean_Nrec)^2)^0.5

# concettualmente il piu' corretto degli errori sarebbe l'errore di un binomio di tipo aXY + aY, con a = d/1000 che porta l'errore del primo monomio a niente,
# per cui il grosso dell'errore e' l'errore di Y.
error15N.aXY.aY = ((d/1000 * N15w.rec.mean *((N15.win.mean$SD_d15N*2/N15.win.mean$mean_d15N)^2+(N15.win.mean$SD_Nrec*2/N15.win.mean$mean_Nrec)^2)^0.5)^2 + 
                  (d/1000*2*N15.win.mean$SD_Nrec)^2)^0.5



 
###################                   SUMMER RECOVERY                     #####################
#
# Differently from winter, I am immediately calculating recovery on two different dates.
# I can then proceed using this script to calculate the winter recovery based on two collections




# Import database
summer_recovery <- read_csv("~/Daniele_Repo/15N_experiment/15N_fieldlab/summer_recovery.csv")


# 15N recovery in TF, %: 
summer_recovery$N15.rec = ((summer_recovery$d15N*d+d)/1000 * summer_recovery$N_rec)/summer_recovery$`15N_applied_mg`*100

# Calculate the 15N recovered as cumulative value
require(data.table)
summer_cumsum <- data.table(summer_recovery)
summer_cumsum = summer_cumsum[, Cum.Sum := cumsum(N15.rec),by=list(N_form, Tree)]

# Plot the summer recovery:
library(ggplot2)
# only the cumulated data:
summer_cumsum = summer_cumsum[summer_cumsum$Date=="12/08/2016",]
ggplot(summer_cumsum, aes(x = N_form, y = Cum.Sum)) + geom_boxplot()

###################                   SUMMER ERROR PROPAGATION                     #####################
# 2 dates, hence once calculated the error per each date, I then have to calculate the (time) 
# error propagation as square root of squares (dX^2+dY^")^0.5

library(plyr)

# calculate the mean recovered 15N for the error propagation formula (combined formula 1, 2 and 3 from http://lectureonline.cl.msu.edu/~mmp/labs/error/e2.htm )
N15.sumrec.mean <- aggregate(N15.rec ~ N_form+Date, data = summer_recovery, FUN = mean,  na.rm = TRUE)




# remove NAs and prepare the table for the error propagation:
summer_recovery = summer_recovery[complete.cases(summer_recovery), c(1,3,7:10)]
sum_rec = merge(summer_recovery, N15.sumrec.mean, by = c("Date", "N_form"))



# concettualmente il piu' corretto degli errori sarebbe l'errore di un binomio di tipo aXY + aY, con a = d/1000 che porta l'errore del primo monomio a niente,
# per cui il grosso dell'errore e' l'errore di Y. Quindi:
sum_rec$error15N = ((d/1000 * sum_rec$N15.rec *((sum_rec$SD_d15N*2/sum_rec$mean_d15N)^2+
                                                  (sum_rec$SD_N_rec*2/sum_rec$mean_N_rec)^2)^0.5)^2 + 
                      (d/1000*2*sum_rec$SD_N_rec)^2)^0.5

sum_rec$error15N.RN = (sum_rec$N15.rec *((sum_rec$SD_d15N*2/sum_rec$mean_d15N)^2+
                                                  (sum_rec$SD_N_rec*2/sum_rec$mean_N_rec)^2)^0.5)^2
                      
# now calculate the error propagation between dates (rows) = err prop of the Cum.Sum
NH4.cum.err = ((sum_rec$error15N[1])^2+(sum_rec$error15N[3])^2)^0.5
NO3.cum.err = ((sum_rec$error15N[2])^2+(sum_rec$error15N[2])^2)^0.5

NH4.cum.err.RN = ((sum_rec$error15N.RN[1])^2+(sum_rec$error15N.RN[3])^2)^0.5
NO3.cum.err.RN = ((sum_rec$error15N.RN[2])^2+(sum_rec$error15N.RN[2])^2)^0.5

# plotting mean value and error
summer.error.RN = as.data.frame(c(NH4.cum.err.RN, NO3.cum.err.RN))
summer.error    = as.data.frame(c(NH4.cum.err, NO3.cum.err))

N15.sumrec.mean$N_form = as.factor(N15.sumrec.mean$N_form)
N15.sumrec = N15.sumrec.mean[, c(1,3)]

N15.sumrec = aggregate(N15.sumrec.mean$N15.rec, by = list(N_form=N15.sumrec.mean$N_form), FUN = sum)

summer.15Nrec.err= as.data.frame(c(N15.sumrec,summer.error,summer.error.RN))
names(summer.15Nrec.err) = c("N_form", "N_rec","my.err", "RN.err")


RN.error = ggplot(summer.15Nrec.err, aes(x = N_form, y = N_rec)) + 
  geom_point() + geom_errorbar(aes(ymin=N_rec-my.err, ymax=N_rec+my.err), colour="black", width=.1)

my.error = ggplot(summer.15Nrec.err, aes(x = N_form, y = N_rec)) + 
  geom_point() + geom_errorbar(aes(ymin=N_rec-RN.err, ymax=N_rec+RN.err), colour="black", width=.1)

###################                   WINTER RECOVERY                     #####################
#
# Differently from winter, I am immediately calculating recovery on two different dates.
# I can then proceed using this script to calculate the winter recovery based on two collections




# Import database
winter_recovery <- read_csv("C:/Users/s1373890/Daniele_Repo/15N_experiment/15N_fieldlab/winter_recovery.csv")


# 15N recovery in TF, %: 
summer_recovery$N15.rec = ((summer_recovery$d15N*d+d)/1000 * summer_recovery$N_rec)/summer_recovery$`15N_applied_mg`*100

# Calculate the 15N recovered as cumulative value
require(data.table)
summer_cumsum <- data.table(summer_recovery)
summer_cumsum = summer_cumsum[, Cum.Sum := cumsum(N15.rec),by=list(N_form, Tree)]

# Plot the summer recovery:
library(ggplot2)
# only the cumulated data:
summer_cumsum = summer_cumsum[summer_cumsum$Date=="12/08/2016",]
ggplot(summer_cumsum, aes(x = N_form, y = Cum.Sum)) + geom_boxplot()


###################                   WINTER ERROR PROPAGATION                     #####################
# 2 dates, hence once calculated the error per each date, I then have to calculate the (time) 
# error propagation as square root of squares (dX^2+dY^")^0.5

library(plyr)

# calculate the mean recovered 15N for the error propagation formula (combined formula 1, 2 and 3 from http://lectureonline.cl.msu.edu/~mmp/labs/error/e2.htm )
N15.sumrec.mean <- aggregate(N15.rec ~ N_form+Date, data = summer_recovery, FUN = mean,  na.rm = TRUE)




# remove NAs and prepare the table for the error propagation:
summer_recovery = summer_recovery[complete.cases(summer_recovery), c(1,3,7:10)]
sum_rec = merge(summer_recovery, N15.sumrec.mean, by = c("Date", "N_form"))



# concettualmente il piu' corretto degli errori sarebbe l'errore di un binomio di tipo aXY + aY, con a = d/1000 che porta l'errore del primo monomio a niente,
# per cui il grosso dell'errore e' l'errore di Y. Quindi:
sum_rec$error15N = ((d/1000 * sum_rec$N15.rec *((sum_rec$SD_d15N*2/sum_rec$mean_d15N)^2+
                                                  (sum_rec$SD_N_rec*2/sum_rec$mean_N_rec)^2)^0.5)^2 + 
                      (d/1000*2*sum_rec$SD_N_rec)^2)^0.5

sum_rec$error15N.RN = (sum_rec$N15.rec *((sum_rec$SD_d15N*2/sum_rec$mean_d15N)^2+
                                           (sum_rec$SD_N_rec*2/sum_rec$mean_N_rec)^2)^0.5)^2

# now calculate the error propagation between dates (rows) = err prop of the Cum.Sum
NH4.cum.err = ((sum_rec$error15N[1])^2+(sum_rec$error15N[3])^2)^0.5
NO3.cum.err = ((sum_rec$error15N[2])^2+(sum_rec$error15N[2])^2)^0.5

NH4.cum.err.RN = ((sum_rec$error15N.RN[1])^2+(sum_rec$error15N.RN[3])^2)^0.5
NO3.cum.err.RN = ((sum_rec$error15N.RN[2])^2+(sum_rec$error15N.RN[2])^2)^0.5

# plotting mean value and error
summer.error.RN = as.data.frame(c(NH4.cum.err.RN, NO3.cum.err.RN))
summer.error    = as.data.frame(c(NH4.cum.err, NO3.cum.err))

N15.sumrec.mean$N_form = as.factor(N15.sumrec.mean$N_form)
N15.sumrec = N15.sumrec.mean[, c(1,3)]

N15.sumrec = aggregate(N15.sumrec.mean$N15.rec, by = list(N_form=N15.sumrec.mean$N_form), FUN = sum)

summer.15Nrec.err= as.data.frame(c(N15.sumrec,summer.error,summer.error.RN))
names(summer.15Nrec.err) = c("N_form", "N_rec","my.err", "RN.err")


RN.error = ggplot(summer.15Nrec.err, aes(x = N_form, y = N_rec)) + 
  geom_point() + geom_errorbar(aes(ymin=N_rec-my.err, ymax=N_rec+my.err), colour="black", width=.1)

my.error = ggplot(summer.15Nrec.err, aes(x = N_form, y = N_rec)) + 
  geom_point() + geom_errorbar(aes(ymin=N_rec-RN.err, ymax=N_rec+RN.err), colour="black", width=.1)

