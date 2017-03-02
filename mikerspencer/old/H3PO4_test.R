#######         STATS ON H3PO4 APPLICATION         #######
#######         sTARTED: 12/05/2016                #######
###             EMMO' SOCCAZZI TUA!                    ###

### Testing February - samples right BEFORE H3PO4 application
# clear the memory
rm(list=ls())

### set working dir for pc:
#setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd per desktop
setwd("M:/My PhD/R/PhD-local_repo")

H3PO4_pretest <- read.csv("M:/My PhD/WP1 Isotopes/H3PO4_pretest.csv")

library(dplyr)

Tplot.pre = filter(H3PO4_pretest, !grepl("C1",sample))

shapiro.test(Tplot.pre$NO3)
shapiro.test(Tplot.pre$NH4)

# note:  the p-value is >> 0.05 for NH4 causing a failure to reject the null (that the data are normal). 
# Does this mean we are to conclude that the data are normal?
# (hint: the answer is no). Failure to reject is not the same thing as accepting. 
# Source: http://stackoverflow.com/questions/7781798/seeing-if-data-is-normally-distributed-in-r/7788452#7788452
# Detto cio': data are normal for NH4, vediamo quindi di lavorare su NH4

Tplot.pre$group = "Control"
Tplot.pre[c(1,5,9), 4] = "treatment"

library(ggplot2)
# boxplot complete:
Tplot.pre$plot = "Tplot"
ggplot(Tplot.pre, aes(x = plot, y = NH4)) +
  geom_boxplot(fill = "lavenderblush2", colour = "blue") +
  scale_x_discrete() + xlab("throughfall collectors") +
  ylab("NH4 concentration (mg/l)") + ggtitle("NO3 concentrations in TF after \n partial treatment with H3PO4")

ggplot(Tplot.pre, aes(x = group, y = NO3)) +
  geom_boxplot(fill = "grey75", colour = "blue") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("NH4 concentration (mg/l)") + ggtitle("NO3 concentrations in TF before \n partial treatment with H3PO4")

ggplot(Tplot.pre, aes(x = group, y = NH4)) +
  geom_boxplot(fill = "grey75", colour = "blue") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("NH4 concentration (mg/l)") + ggtitle("NH4 concentrations in TF before \n partial treatment with H3PO4")


# no clear differences are shown between the means in the plot.

# creating the linear model

noacid.treatm = lm(NH4 ~ group, data = Tplot.pre)

summary(noacid.treatm)

anova(noacid.treatm) # this confirms there are no differences between the means

###############################################################
### Testing March - samples 1 month AFTER H3PO4 application ###

H3PO4_test <- read.csv("M:/My PhD/WP1 Isotopes/H3PO4_test.csv")

Tplot.after = filter(H3PO4_test, !grepl("C1",sample))

shapiro.test(Tplot.after$NO3)
shapiro.test(Tplot.after$NH4)

# note:  the p-value is >> 0.05 for NO3 causing a failure to reject the null (that the data are normal). 
# Does this mean we are to conclude that the data are normal?
# (hint: the answer is no). Failure to reject is not the same thing as accepting. 
# Source: http://stackoverflow.com/questions/7781798/seeing-if-data-is-normally-distributed-in-r/7788452#7788452
# Detto cio': data are normal for NH4, vediamo quindi di lavorare su NH4

Tplot.after$group = "Control"
Tplot.after[c(1,5,9), 4] = "treatment"

library(ggplot2)

# boxplot complete:
ggplot(Tplot.after, aes(x = plot, y = NH4)) +
  geom_boxplot(fill = "lavenderblush", colour = "blue") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("NH4 concentration (mg/l)") + ggtitle("NH4 concentrations in TF after \n partial treatment with H3PO4")

ggplot(Tplot.after, aes(x = sample, y = NH4)) +
  geom_boxplot(fill = "lavenderblush", colour = "blue") +
  scale_x_discrete() + xlab("Throughfall collectors") +
  ylab("NH4 concentration (mg/l)") + ggtitle("NH4 concentrations in TF after \n partial treatment with H3PO4")
# boxplot complete:
Tplot.after$plot = "Tplot"
ggplot(Tplot.after, aes(x = plot, y = NO3)) +
  geom_boxplot(fill = "lavenderblush2", colour = "blue") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("NH4 concentration (mg/l)") + ggtitle("NO3 concentrations in TF after \n partial treatment with H3PO4")

ggplot(Tplot.after, aes(x = group, y = NO3)) +
  geom_boxplot(fill = "lavenderblush2", colour = "blue") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("NH4 concentration (mg/l)") + ggtitle("NO3 concentrations in TF after \n partial treatment with H3PO4")


# no clear differences are shown between the means in the plot.

# creating the linear model for NO3

acid.treatm.NO3 = lm(NO3 ~ group, data = Tplot.after)

summary(acid.treatm.NO3)

anova(acid.treatm.NO3)

# creating the linear model for NH4

acid.treatm.NH4 = lm(NH4 ~ group, data = Tplot.after)

summary(acid.treatm.NH4)

anova(acid.treatm.NH4)

Tplot.after$group = as.factor(Tplot.after$group)
kruskal.test(NH4 ~ group, data = Tplot.after) 

##### 23/05/2016: paired t-test #####

colnames(Tplot.pre)[2] = "NO3.b"
colnames(Tplot.pre)[3] = "NH4.b"
colnames(Tplot.after)[2] = "NO3.a"
colnames(Tplot.after)[3] = "NH4.a"

Tplot = cbind (Tplot.pre, Tplot.after)

aNO3= Tplot$NO3.b-Tplot$NO3.a

t.test(aNO3[c(1,5,9)], aNO3[c(3,4,8)], paired=TRUE)

aNH4= Tplot$NH4.b-Tplot$NH4.a

t.test(aNH4[c(1,5,9)], aNH4[c(3,4,8)], paired=TRUE)
#ultimo aggiornamento: pare che voglia x e y della same length sto cacacazzi
# run with several random control triplets. The p value is always > 0.53
### END ###