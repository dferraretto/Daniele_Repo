# ------------------------------------------------------------------
####  TABLES FOR POSTER  MAY  2016 (SOURCE: RFTSW_monthly)    ######
#     last update: 09/02/2017. Author: Daniele Ferraretto
# ------------------------------------------------------------------
# -------------------------------------------------------
#
#          Correlations on monthly data
#
# -------------------------------------------------------
# to run this script you need to run RFTSW_monthly first, including the long version df

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

RF.m.depth = transform(RF.m.vol, mY = as.yearmon(as.character(mY), "%Y%m"))
RF.m.depth$month = format(RF.m.depth$mY, "%m")
RF.m.depth$year = format(RF.m.depth$mY, "%Y")
colnames(RF.m.depth)[2] = "value"
RF.m.depth$variable = "xprec"
RF.m.depth = RF.m.depth[RF.m.depth$year<2017&RF.m.depth$year>2011,]
RF.m.depth$value = RF.m.depth$value/280 # 280 is the result of the ration maximum monthly prec depth/max main y axis value (~0.6):
# when a second axis is used (see below, scale_y_continuous), then the same value is used to turn the secondary scale into the originary values


# Ninput <- long.N.RFTSW[long.N.RFTSW$variable %like% "N.RF"| long.N.RFTSW$variable %like% "N.fog",]
N.input <- long.N.RFTSW[long.N.RFTSW$variable %in% "NH4.N.RF" | long.N.RFTSW$variable %in% "NO3.N.RF"| long.N.RFTSW$variable %in% "NH4.N.fog"| long.N.RFTSW$variable %in% "NO3.N.fog",]

###### HEAD
x = ggplot(data = N.input, aes (month, value, fill = variable)) 
########
#N.input$variable= factor(N.input$variable, levels = c("NH4.N.RF", "NO3.N.RF", "NH4.N.fog", "NO3.N.fog")) # orders my factors not to mess with bar colors

N.input=droplevels(N.input)

x = ggplot(data = N.input, aes (month, value, fill = variable))

#orange.bold.text <- element_text(face = "bold", color = "orange") # per bold AND italic: bold.italic
#orange.text <- element_text(face = "plain", color = "orange")
Edired.text <- element_text(face = "plain", color = "#DF0057")

plot.RFfog = x + geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values = c('NH4.N.RF'="Sky Blue", 'NO3.N.RF' = "royal blue",
                                 'NH4.N.fog' = "grey60", 'NO3.N.fog'="grey30", 'xprec'="#DF0057"), 
                      breaks = c('NH4.N.RF', 'NO3.N.RF', 'NH4.N.fog', 'NO3.N.fog', 'xprec'), 
                      name = " N flux \n and form", 
                      labels = c(expression(~RF~NH[4]*-N), expression(~RF~NO[3]*-N), 
                                 expression(~fog~NH[4]*-N), expression(~fog~NO[3]*-N), 
                                 expression(~precipitation))) +
    facet_grid(year ~ .) + ggtitle("Nitrogen deposition in rainfall and fog\n compared to monthly precipitation") +
    theme(plot.title=element_text(face="bold", size = 16)) +
    labs( x = "month", y = expression(N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) +
    theme(panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
    scale_y_continuous(sec.axis = sec_axis(~.*280, name = "monthly precipitation (mm)")) +
    geom_line(data = RF.m.depth, aes(x = month, y = value, group = "variable"), 
              colour = "#DF0057", size = 0.5, linetype = 2) +
  geom_point(data = RF.m.depth, aes(x = month, y = value, group = "variable"), 
            colour = "#DF0057", size = 1.5, show.legend = FALSE) + theme_bw(base_size = 14) +
    theme(axis.text.y.right = Edired.text, axis.title.y.right = Edired.text, plot.title = element_text(hjust = 0.5, size = 20, colour = '#DF0057'))



# Here for fine legends: http://stackoverflow.com/questions/18394391/r-custom-legend-for-multiple-layer-ggplot
# risolve il mio ? sul fatto che davo un colore ma poi non usciva quel colore. Praticamente in aes si "mappa" il colore,
# ma poi va dato in scale_fill_identity o scale_colour_manual o li mortacci sua. Questo per separare N flux and form da precipitation
# e dare a precipitation in legenda forma di linea. to be updated, last update: 09/02/2017
  
# **************************************************************************************************

# table 2: undercanopy fluxes TF+SF vs. TF depth

# **************************************************************************************************

  # Throughfall - preparing the df TF.m.vol
  
TF.m.depth = transform(tf.m.vol, mY = as.yearmon(as.character(mY), "%Y%m"))

TF.m.depth$month = format(TF.m.depth$mY, "%m")
TF.m.depth$year = format(TF.m.depth$mY, "%Y")
colnames(TF.m.depth)[2] = "value"
TF.m.depth$variable = "TF.depth"
TF.m.depth = TF.m.depth[TF.m.depth$year<2017&TF.m.depth$year>2011,]
TF.m.depth$value = TF.m.depth$value/(177*2) # 177 maximum monthly TF depth depth, 0.5 the max value on the y axis, to fit TF data into the TF/SF plot:
# when a second axis is used (see below, scale_y_continuous), then the same value is used to turn the secondary scale into the originary values

  
  N.TFSF <- long.N.RFTSW[long.N.RFTSW$variable %in% "NH4.N.TF" | long.N.RFTSW$variable %in% "NO3.N.TF"| 
                         long.N.RFTSW$variable %in% "NH4.N.SF"| long.N.RFTSW$variable %in% "NO3.N.SF",]

w = ggplot(data = N.TFSF, aes (month, value, fill = variable))

# Edired.bold.text <- element_text(face = "bold", color = "#DF0057") # per bold AND italic: bold.italic

plot.TFSF = w + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c('NH4.N.TF'="Dark Green", 'NO3.N.TF' = "Yellow Green",
                               'NH4.N.SF' = "Saddle Brown", 'NO3.N.SF'="Burlywood", 'TF.depth'="#DF0057"), 
                    breaks = c('NH4.N.TF', 'NO3.N.TF', 'NH4.N.SF', 'NO3.N.SF', 'TF.depth'), 
                    name = "N flux \n and form", 
                    labels = c(expression(~TF~NH[4]*-N), expression(~TF~NO[3]*-N), 
                               expression(~SF~NH[4]*-N), expression(~SF~NO[3]*-N), 
                               expression(~TF~depth))) +
  facet_grid(year ~ .) + ggtitle("Nitrogen deposition in throughfall (TF) and stemflow (SF)\n compared to monthly throughfall depth") +
  theme(plot.title=element_text(face="bold", size = 16)) +
  labs( x = "month", y = expression(N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) +
  theme(panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  scale_y_continuous(sec.axis = sec_axis(~.*280, name = "monthly throughfall depth (mm)")) +
  
  geom_line(data = TF.m.depth, aes(x = month, y = value, group = "variable"), 
            colour = "#DF0057", size = 0.5, linetype = 2) +
  
  geom_point(data = TF.m.depth, aes(x = month, y = value, group = "variable"), 
             colour = "#DF0057", size = 1.5, show.legend = FALSE) + theme_bw(base_size = 14) +
  theme(axis.text.y.right = Edired.text, axis.title.y.right = Edired.text, plot.title = element_text(hjust = 0.5, size = 20, colour = '#DF0057'))



# **************************************************************************************************

# table 5: canopy effect -  Ndep (RF+fog) vs. TF+SF(comparison)

# **************************************************************************************************
N.inout <- long.N.RFTSW[long.N.RFTSW$variable %like% "input"| long.N.RFTSW$variable %like% "output",]
N.inout = N.inout[N.inout$year>2011 & N.inout$year<2017,]

#N.inout$variable <- factor(N.inout$variable, levels = c("NO3.N.RFfog", "NH4.N.RFfog", "NO3.N.TF", "NH4.N.TF"))
Inout = ggplot(data = N.inout, aes (month, value, fill = variable))

plot.inout = Inout + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("cadetblue4", "cadetblue2", "Dark Green", "Yellow Green"), name = " N flux \n and form", labels = c(expression(~fog+RF~NH[4]*-N), expression(~fog+RF~NO[3]*-N), expression(~TF+SF~NH[4]*-N), expression(~TF+SF~NO[3]*-N))) +
  facet_grid(year ~ .) + ggtitle("Nitrogen deposition and N content under the canopy") +
  labs( x = "Month", y = expression(N~flux~~"(kg N"~~ha^"-1"~month^"-1"*")")) + theme_bw(base_size = 14)+
  theme(panel.border = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA), 
  plot.title = element_text(hjust = 0.5, size = 20, colour = '#DF0057')) 


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
multiplot(plot.RFfog,plot.inout, plot.TFSF, cols = 2)


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


##############################################################################

#           Yearly mean figures for period 2012-2016

##############################################################################
table.m.NX$month = format(table.m.NX$mY, "%m")
table.m.NX$year = format(table.m.NX$mY, "%Y")
table.2012_2016 = table.m.NX[table.m.NX$year>2011 & table.m.NX$year<2017,]

mean.fogNH4=sum(table.m.NX$NH4.N.fog)/5
mean.fogNO3=sum(table.m.NX$NO3.N.fog)/5
mean.RFNH4=sum(table.m.NX$NH4.N.RF)/5
mean.RFNO3=sum(table.m.NX$NO3.N.RF)/5
mean.TFNH4=sum(table.m.NX$NH4.N.TF)/5
mean.TFNO3=sum(table.m.NX$NO3.N.TF)/5
mean.SFNH4=sum(table.m.NX$NH4.N.SF)/5
mean.SFNO3=sum(table.m.NX$NO3.N.SF)/5
