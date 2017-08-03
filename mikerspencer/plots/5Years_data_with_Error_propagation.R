####################################################################################################
#
#                     Historical data series with error propagation 

rm(list=ls())

#setwd("M:/My PhD/R/PhD-local_repo")
setwd("C:/Users/s1373890/Daniele_Repo")

source("mikerspencer/Error_propagation_TF_SF_RF_fog.R")

source("mikerspencer/RFTSW_monthly.R")

###                         PRE filtering 2011-2017 data                   #########################

long.N.RFTSW = long.N.RFTSW[long.N.RFTSW$year>2011 & long.N.RFTSW$year<2017,]
long.N.error = long.N.error[long.N.error$year>2011 & long.N.error$year<2017,]

library(ggplot2)


# **************************************************************************************************

# table 1: canopy effect -  Ndep (RF+fog) vs. TF+SF(comparison)

# **************************************************************************************************

N.inout <- long.N.RFTSW[long.N.RFTSW$variable %like% "input" | long.N.RFTSW$variable %like% "output",]

N.inout <- subset(N.inout, variable != "total.Ninput")
N.inout = droplevels(N.inout)

# creating a INOUT.error dataframe
INOUT.error = rbind(dIN.error, dOutput.err)

INOUT.error = transform(INOUT.error, Ym = as.yearmon(as.character(Ym), "%Y%m"))

colnames(INOUT.error)[1] = "mY"

INOUT.error$variable = revalue(INOUT.error$variable, c("dIN.NO3" = "NO3.N.input", "dIN.NH4" = "NH4.N.input", 
                                                       "dOUT.NO3" = "NO3.output", "dOUT.NH4" = "NH4.output")) # Back to former level names
# turn mg/m2 into Kg/ha:
INOUT.error$value = INOUT.error$value/100

N.inout.err = merge(N.inout, INOUT.error, by = c("mY", "variable"))
names(N.inout.err) = c("mY", "variable", "mean", "month", "year", "err")

#N.inout$variable <- factor(N.inout$variable, levels = c("NO3.N.RFfog", "NH4.N.RFfog", "NO3.N.TF", "NH4.N.TF"))

Inout.err = ggplot(data = N.inout.err, aes (month, mean, fill = variable))

plot.inout = Inout.err + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  geom_errorbar(aes(ymin = mean-err, ymax = mean+err), width = .3, position = position_dodge(.9)) +
  scale_fill_manual(values = c("cadetblue4", "cadetblue2", "Dark Green", "Yellow Green"), name = "N flux \n and form", labels = c(expression(~fog+RF~NH[4]*-N), expression(~fog+RF~NO[3]*-N), expression(~TF+SF~NH[4]*-N), expression(~TF+SF~NO[3]*-N))) +
  facet_grid(year ~ .) + ggtitle("Nitrogen deposition and N content under the canopy") +
  labs( x = "Month", y = expression(N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) + theme_bw(base_size = 14)+
  theme(panel.border = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA), 
        plot.title = element_text(hjust = 0.5, size = 18, colour = '#DF0057')) 

# **************************************************************************************************

# table 2: canopy effect -  Ndep (just RF) vs. TF only

# **************************************************************************************************

N.RFTF <- long.N.RFTSW[long.N.RFTSW$variable %like% ".N.RF" | long.N.RFTSW$variable %like% ".N.TF" ,]

# creating a INOUT.error dataframe
RFTF.error = rbind(dRF.err, dTF.err)

RFTF.error = transform(RFTF.error, Ym = as.yearmon(as.character(Ym), "%Y%m"))

colnames(RFTF.error)[1] = "mY"

RFTF.error$variable = revalue(RFTF.error$variable, c("dRF.NO3" = "NO3.N.RF", "dRF.NH4" = "NH4.N.RF", 
                                                       "dTF.NO3" = "NO3.N.TF", "dTF.NH4" = "NH4.N.TF")) # Back to former level names
# turn mg/m2 into Kg/ha:
RFTF.error$value = RFTF.error$value/100

N.RFTF.err = merge(N.RFTF, RFTF.error, by = c("mY", "variable"))
names(N.inout.err) = c("mY", "variable", "mean", "month", "year", "err")

#N.inout$variable <- factor(N.inout$variable, levels = c("NO3.N.RFfog", "NH4.N.RFfog", "NO3.N.TF", "NH4.N.TF"))

RFTF.err = ggplot(data = N.inout.err, aes (month, mean, fill = variable))

plot.RFTF = RFTF.err + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  geom_errorbar(aes(ymin = mean-err, ymax = mean+err), width = .1, position = position_dodge(.9)) +
  scale_fill_manual(values = c("cadetblue4", "cadetblue2", "Dark Green", "Yellow Green"), name = "N flux \n and form",
                    labels = c(expression(~RF~NH[4]*-N), expression(~RF~NO[3]*-N), expression(~TF~NH[4]*-N), expression(~TF~NO[3]*-N))) +
  facet_grid(year ~ .) + ggtitle("Wet deposition and N content in throughfall") +
  labs( x = "Month", y = expression(N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) + theme_bw(base_size = 14)+
  theme(panel.border = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA), 
        plot.title = element_text(hjust = 0.5, size = 18, colour = '#DF0057')) 
