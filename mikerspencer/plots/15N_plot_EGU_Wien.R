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

N_Ndep_15N_simplified <- read_csv("C:/Users/s1373890/Daniele_Repo/15N_experiment/15N_fieldlab/15N_Ndep_simplified.csv")
# crop all samples but TF and SF
TFTS = c("T1", "T2", "T3", "S1", "S2", "S3")
TFSF15.N = Incomplete_d15N_results[which(Incomplete_d15N_results$Sample == "T1" | Incomplete_d15N_results$Sample == "T2" | Incomplete_d15N_results$Sample == "T3"
                                         | Incomplete_d15N_results$Sample == "S1"| Incomplete_d15N_results$Sample == "S2"| Incomplete_d15N_results$Sample == "S3"),]

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
  







