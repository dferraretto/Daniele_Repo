# ------------------------------------------------------------------
####  TABLES FOR POSTER  MAY  2016 (SOURCE: RFTSW_monthly)    ######
#     last update: 09/02/2017. Author: Daniele Ferraretto
# ------------------------------------------------------------------
# -------------------------------------------------------
#
#          Correlations on monthly data
#
# -------------------------------------------------------

library(psych) 

pairs.panels(table.m.NX[c(2,4,6,8,10)], main = "Griffin NH4-N deposition by month - correlations between fluxes")
pairs.panels(table.m.NX[c(3,5,7,9,11)], main = "Griffin NO3-N deposition by month - correlations between fluxes")

print(corr.test(table.m.NX[c(2,4,6,8,10)]), short=FALSE)

###                         PRE filtering 2011-2017 data                   #########################

long.N.RFTSW = long.N.RFTSW[long.N.RFTSW$year>2011 & long.N.RFTSW$year<2017,]


# **************************************************************************************************

# table 1: N input -  rainfall vs. fog (comparison) and monthly precipitation depth

# **************************************************************************************************
# B4 starting:
.libPaths("C:/Workspace/R")

#install.packages(data.table) # to make subsetting easier
library(data.table)
library(ggplot2)
library(extrafont)

# Precipitation - preparing the df RF.m.vol

RF.m.vol = transform(RF.m.vol, mY = as.yearmon(as.character(mY), "%Y%m"))
RF.m.vol$month = format(RF.m.vol$mY, "%m")
RF.m.vol$year = format(RF.m.vol$mY, "%Y")
colnames(RF.m.vol)[2] = "value"
RF.m.vol$variable = "prec"
RF.m.vol = RF.m.vol[RF.m.vol$year<2017&RF.m.vol$year>2011,]
RF.m.vol$value = RF.m.vol$value/280 # 280 is the result of the ration maximum monthly prec depth/max main y axis value (~0.6):
# when a second axis is used (see below, scale_y_continuous), then the same value is used to turn the secondary scale into the originary values


# Ninput <- long.N.RFTSW[long.N.RFTSW$variable %like% "N.RF"| long.N.RFTSW$variable %like% "N.fog",]
N.input <- long.N.RFTSW[long.N.RFTSW$variable %in% "NH4.N.RF" | long.N.RFTSW$variable %in% "NO3.N.RF"| long.N.RFTSW$variable %in% "NH4.N.fog"| long.N.RFTSW$variable %in% "NO3.N.fog",]

x = ggplot(data = N.input, aes (month, value, fill = variable))

plot.RFfog = x + geom_bar(stat = "identity", position = "dodge") + 
  geom_line(data = RF.m.vol, aes(x = month, y = value, group = "variable"), colour = "darkblue", size = 1.1, linetype = 3) + 
  scale_fill_manual(values = c("royal blue", "Sky Blue","grey30", "grey60", "darkblue"), name = " N flux \n and form", labels = c(expression(~RF~NH[4]*-N), expression(~RF~NO[3]*-N), expression(~fog~NH[4]*-N), expression(~fog~NO[3]*-N), expression(~precipitation))) +
  facet_grid(year ~ .) + ggtitle("Nitrogen deposition in \n rainfall and fog") +
  theme(plot.title=element_text(face="bold", size = 16)) +
  labs( x = "month", y = expression(N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) +
  theme(panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  scale_y_continuous(sec.axis = sec_axis(~.*280, name = "monthly precipitation [mm]"))

# Here for fine legends: http://stackoverflow.com/questions/18394391/r-custom-legend-for-multiple-layer-ggplot
# risolve il mio ? sul fatto che davo un colore ma poi non usciva quel colore. Praticamente in aes si "mappa" il colore,
# ma poi va dato in scale_fill_identity o scale_colour_manual o li mortacci sua. Questo per separare N flux and form da precipitation
# e dare a precipitation in legenda forma di linea. to be updated, last update: 09/02/2017
  

prec <- ggplot(RF.m.vol, aes(month, value)) + 
  geom_line(colour = "dodgerblue3", group = "variable") +  
# **************************************************************************************************

# table 2: undercanopy fluxes TF vs. SF

# **************************************************************************************************
N.TFSF <- long.N.RFTSW[long.N.RFTSW$variable %in% "NH4.N.TF" | long.N.RFTSW$variable %in% "NO3.N.TF"| long.N.RFTSW$variable %in% "NH4.N.SF"| long.N.RFTSW$variable %in% "NO3.N.SF",]

w = ggplot(data = N.TFSF, aes (month, value, fill = variable))

plot.TFSF = w + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("Dark Green", "Yellow Green","Saddle Brown", "Burlywood"), name = " N flux \n and form", labels = c(expression(~TF~NH[4]*-N), expression(~TF~NO[3]*-N), expression(~SF~NH[4]*-N), expression(~SF~NO[3]*-N))) +
  facet_grid(year ~ .) + ggtitle("Nitrogen deposition in \n throughfall and stemflow") +
  theme(plot.title=element_text(face="bold", size = 16)) +
  labs( x = "Month", y = expression(N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) +
  theme(panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
# **************************************************************************************************

# table 3: canopy effect -  RF vs. TF

# **************************************************************************************************

N.RFTF <- long.N.RFTSW[long.N.RFTSW$variable %in% "NH4.N.RF" | long.N.RFTSW$variable %in% "NO3.N.RF"| long.N.RFTSW$variable %in% "NH4.N.TF"| long.N.RFTSW$variable %in% "NO3.N.TF",]
N.RFTF$variable = factor(N.RFTF)
y = ggplot(data = N.RFTF, aes (month, value, fill = variable))

plot.RFTF = y + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("royal blue", "Sky Blue","Dark Green", "Yellow Green"), name = " N flux \n and form", labels = c(expression(~RF~NH[4]*-N), expression(~RF~NO[3]*-N), expression(~TF~NH[4]*-N), expression(~TF~NO[3]*-N))) +
  facet_grid(year ~ .) + ggtitle("Nitrogen deposition in \n rainfall and throughfall") +
  theme(plot.title=element_text(face="bold", size = 16)) +
  labs( x = "Month", y = expression(N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) +
  theme(panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))

# **************************************************************************************************

# table 4: canopy effect -  fog (line) vs. TF(comparison)

# **************************************************************************************************
N.fogTF <- long.N.RFTSW[long.N.RFTSW$variable %like% "N.fog"| long.N.RFTSW$variable %like% "N.TF",]

z = ggplot(data = N.fogTF, aes (month, value, fill = variable))

plot.fogTF = z + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("grey30", "grey60","Dark Green", "Yellow Green"), name = " N flux \n and form", labels = c(expression(~fog~NH[4]*-N), expression(~fog~NO[3]*-N), expression(~TF~NH[4]*-N), expression(~TF~NO[3]*-N))) +
  facet_grid(year ~ .) + ggtitle("Nitrogen deposition in fog and throughfall") +
  theme(plot.title=element_text(face="bold", size = 16)) +
  labs( x = "Month", y = expression(N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) +
  theme(panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))

# **************************************************************************************************

# table 4a: canopy effect -  fog+RF (line) vs. TF(comparison)

# **************************************************************************************************
N.RFfogTF <- long.N.RFTSW[long.N.RFTSW$variable %like% "N.RFfog"| long.N.RFTSW$variable %like% "N.TF",]

N.RFfogTF$variable <- factor(N.RFfogTF$variable, levels = c("NO3.N.RFfog", "NH4.N.RFfog", "NO3.N.TF", "NH4.N.TF"))
Wow = ggplot(data = N.RFfogTF, aes (month, value, fill = variable))

plot.RFfogTF = Wow + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("magenta4", "maroon", "Dark Green", "Yellow Green"), name = " N flux \n and form", labels = c(expression(~fog+RF~NH[4]*-N), expression(~fog+RF~NO[3]*-N), expression(~TF~NH[4]*-N), expression(~TF~NO[3]*-N))) +
  facet_grid(year ~ .) + ggtitle("Nitrogen deposition in \n fog+precipitation and throughfall") +
  theme(plot.title=element_text(face="bold", size = 16)) +
  labs( x = "Month", y = expression(N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) +
  theme(panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))




### MULTIPLOT FOR POSTER
#install.packages("Rmisc")
library(Rmisc)
multiplot(plot.RFfog,plot.RFTF,plot.TFSF,plot.RFfogTF, cols = 2)


# **************************************************************************************************

# Yearly Fluxes, plot and tab

# **************************************************************************************************

table.m.NX.small= table.m.NX[ , c(1,10:13)]

#########         WIDE TO LONG NX TABLE:

long.N.inout = melt(table.m.NX.small, id.vars = "mY") 

# turn character into month+year, month (numeric) and year(numeric):

long.N.inout$year = format(long.N.inout$mY, "%Y")

long.N.inout.Y = aggregate(value ~ year + variable, data = long.N.inout, FUN=sum)

a = ggplot(data = long.N.inout.Y, aes (year, value, fill = variable))

plot.year.inout = a + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("cadetblue4", "cadetblue2", "darkgoldenrod4", "darkgoldenrod2"), 
                    name = "N flux \n and form", labels = c(expression(input~NH[4]*-N), expression(input~NO[3]*-N), 
                                                            expression(output~NH[4]*-N), expression(output~NO[3]*-N))) +
  ggtitle("Yearly N input and output in Griffin") +
  theme(plot.title=element_text(face="bold", size = 16)) +
  labs( x = "Year", y = expression(N~flux~~"(kg N"~~ha^"-1"~y^"-1"*")")) +
  theme(panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # PLOTTING the annual fluxes (DC) for Funds request
  
  x = ggplot(data = mNX.rfsftf, aes (mY, vals, fill = var))

plottone = x + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("royal blue", "Sky Blue", "Dark Green", "Yellow Green", "Saddle Brown", "Burlywood"), name = "N flux \n and form", labels = c(expression(RF~NH[4]*-N), expression(RF~NO[3]*-N), expression(TF~NH[4]*-N), expression(TF~NO[3]*-N), expression(SF~NH[4]*-N), expression(SF~NO[3]*-N))) +
  labs( x = "YEAR", y = expression(N~flux~~"(kg N"~~ha^"-1"~y^"-1"*")")) 

ggsave("M:/My PhD/R/PhD-local_repo/output_tables_plots/N_fluxes_Griffin.png", width = 6, height = 4, dpi = 100, plottone)

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
LEO's filter on ggplot                                                                                      '
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  headwater_isotopes = chem2 %>%
  
  filter(Catchment %in% c("EGS16", "EGS06", "EGS07", "EGS10", "EGS12", "EGW17")) %>%
  
  ggplot(., aes(x = d18O, y=dD, group=Catchment, colour=Catchment, shape = Catchment)) +
  
  geom_point() +
  
  xlab(expression({delta}^18*"O VSMOW2")) +
  
  ylab(expression({delta}^2*"H VSMOW2")) +
  
  geom_point(data = filter(river_isotope_summary, Catchment %in% c("EGS16", "EGS06", "EGS07", "EGS10", "EGS12", "EGW17")),
             
             aes(x = Mean_d18O, y = Mean_dD, group=Catchment)) +
  
  geom_errorbarh(data = filter(river_isotope_summary, Catchment %in% c("EGS16", "EGS06", "EGS07", "EGS10", "EGS12", "EGW17")), aes(xmin = Mean_d18O - sd_d18O,
                                                                                                                                   
                                                                                                                                   xmax = Mean_d18O + sd_d18O, y = Mean_dD, x = Mean_d18O, group=Catchment), height = .5) +
  
  geom_errorbar(data = filter(river_isotope_summary, Catchment %in% c("EGS16", "EGS06", "EGS07", "EGS10", "EGS12", "EGW17")), aes(ymin = Mean_dD - sd_dD,
                                                                                                                                  
                                                                                                                                  ymax = Mean_dD + sd_dD, x = Mean_d18O, y = Mean_dD, group=Catchment), width = .01) +
  
  stat_function(fun=function(x)8*x+10, geom="line", colour="Black") +
  
  stat_function(fun=function(x)7.7*x+6.7, geom="line", colour="Black", linetype=3) +
  
  annotate("text", -8.5, -57.3, label = "GMWL", size=4) +
  
  annotate("text", -8.4, -60.0, label = "LMWL", size=4) +
  
  ggtitle("Headwater isotopes Dec 2014 - Jan 2016 raw data") +
  
  theme(plot.title = element_text(family = "Arial", face="bold", size=12, hjust=0.5))

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

