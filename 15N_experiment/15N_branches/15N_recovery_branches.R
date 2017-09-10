# --------------------------------------------------------
  #  15N on branches data analysis
  #  started on September 04th 2017
  #  updated: 
  # --------------------------------------------------------
# --------------------------------------------------------
# clear the memory
rm(list=ls())

#.libPaths("C:/Workspace/R") # adjust the issues with my desktop demmerda
### set working dir for pc:
setwd("C:/Users/Daniele Ferraretto/Documents/Daniele_Repo")
### setwd per desktop
#setwd("M:/My PhD/R/PhD-local_repo")
setwd("C:/Users/s1373890/Daniele_Repo")

library(readr)
branch_15N <- read_csv("~/Daniele_Repo/15N_experiment/15N_branches/branch_15N.csv")

app = 0.463702762 # 15N applied to the branches (mg/cm)

d = 0.0036765 # R standard for 15N/14N

# Prepare the dataset: all characters to factors
cols <- c("T_C", "Girdling", "Tree", "compartment", "branch")

branch_15N[cols] <- lapply(branch_15N[cols], factor)

# long-ish to wide

library(data.table)
# https://stackoverflow.com/questions/37622935/wide-format-with-dcast-data-table: per dcast mi serve una table => setDT
branch_15N_wider = dcast(setDT(branch_15N), branch + compartment  ~ T_C, value.var = c("Total_N_perc","d15N", "DM_by_length"))


# 15N recovery in BRANCHES per compartment, %: (see 15N formula explained.doc, adapted)
# 15Nsample = (cd+d)/1000/(1+(cd+d)/1000)   15Nexcess

branch_15N_wider$N15.rec = (((branch_15N_wider$d15N_T-branch_15N_wider$d15N_C)*d+d)/1000)/ 
  (1+((branch_15N_wider$d15N_T-branch_15N_wider$d15N_C)*d+d)/1000) * # = 15Nexcess perc
  (branch_15N_wider$Total_N_perc_T * branch_15N_wider$DM_by_length_T)*1000*100/ app # = N/applied 15N; 1000 e' per convertire i g DM in mg!, 100 da formula

# correcting factor: scale the real length of each branch in order to calculate the real amount of 15N applied per length (the former is calculated by
# using the length factor LF, which was obtained in the field by measuring the main sub-branches of each branch)

ratio.15N <- read_csv("~/Daniele_Repo/15N_experiment/15N_branches/ratio_15N.csv")
ratio.15N = ratio.15N[rep(seq_len(nrow(ratio.15N)), each=4),]
ratio.15N$branch = as.factor(ratio.15N$branch)

branch_15N_corrected = cbind(branch_15N_wider, ratio.15N[ , 2])
branch_15N_corrected$N15.corrected = branch_15N_corrected$N15.rec*branch_15N_corrected$ratio

total.15N.rec = aggregate(N15.corrected ~ branch, branch_15N_corrected, FUN = sum)

d15N.mean = aggregate(d15N_T ~ compartment, branch_15N_corrected, FUN = mean)
mean.15N.rec = aggregate(N15.corrected ~ compartment, branch_15N_corrected, FUN = mean)
branch_15N_corrected$d15N.exc = branch_15N_corrected$d15N_T-branch_15N_corrected$d15N_C

#######             PLOTS             ##############
library(ggplot2)
# By Compartment:

## Total N %
ggplot(branch_15N_wider, aes(x=compartment, y=Total_N_perc_T, fill=compartment))+
  geom_boxplot() + ggtitle("N (% on DM)")  + theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5)) +  guides(fill=FALSE) +
    labs(x = 'comparment', y = "N (%)") +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs')) #theme_bw va prima di theme o fotte hjust e compagnia briscola

# d15N excess N
ggplot(branch_15N_corrected, aes(x=compartment, y=d15N.exc, fill=compartment))+
  geom_boxplot() + theme_bw(base_size = 12) + theme(plot.title = element_text(hjust = 0.5, color = "red4")) +
  ggtitle(expression(~delta^15*N~"(\u2030) (excess)")) + 
  labs(y = expression(~delta^15*N~"(\u2030)")) +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))+
  theme(axis.title.x=element_blank()) +
  theme(legend.position = "none")
  
## Total 15N recovery (see how to start an axis lab with superscript by using PASTE)

ggplot(branch_15N_corrected, aes(x=compartment, y=N15.corrected, fill=compartment))+
  geom_boxplot() + ggtitle(expression(paste(' '^{15}, "N (%) recovered by compartment", sep = "")))  + theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5)) +  guides(fill=FALSE) +
  labs(y = (expression(paste(' '^{15}, "N (%)", sep = ""))) ) +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))


