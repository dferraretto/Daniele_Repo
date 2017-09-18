# --------------------------------------------------------
  #  15N on branches data analysis
  #  started on September 04th 2017
  #  updated: 18th September 2017, after 
  # --------------------------------------------------------
# --------------------------------------------------------
# clear the memory
rm(list=ls())

.libPaths("C:/Workspace/R") # adjust the issues with my desktop demmerda
### set working dir for pc:
setwd("C:/Users/Daniele Ferraretto/Documents/Daniele_Repo")
### setwd per desktop
#setwd("M:/My PhD/R/PhD-local_repo")
setwd("C:/Users/s1373890/Daniele_Repo")

library(readr)
branch_15N <- read_csv("15N_experiment/15N_branches/branch_15N.csv") # ~/Daniele_Repo/ in case needed from laptop

app = 0.463702762 # 15N applied to the branches (mg/cm)

AR = 0.0036765 # R standard for 15N/14N

# Prepare the dataset: all characters to factors
cols <- c("T_C", "Girdling", "Tree", "compartment", "branch")

branch_15N[cols] <- lapply(branch_15N[cols], factor)

# long-ish to wide

library(data.table)
# https://stackoverflow.com/questions/37622935/wide-format-with-dcast-data-table: per dcast mi serve una table => setDT
branch_15N_wider = dcast(setDT(branch_15N), branch + compartment  ~ T_C, value.var = c("Total_N_perc","d15N", "DM_by_length"))

# R in labelled and unlabelled samples

branch_15N_wider$Rsample = ((branch_15N_wider$d15N_T/1000) + 1) * AR

branch_15N_wider$Rcontrol = ((branch_15N_wider$d15N_C/1000) + 1) * AR

# Atom % excess

branch_15N_wider$atm_perc_exc = branch_15N_wider$Rsample - branch_15N_wider$Rcontrol

# N content by length

branch_15N_wider$N_cont_mg = branch_15N_wider$Total_N_perc_T * branch_15N_wider$DM_by_length_T *1000/1000

N15_excess_mg = branch_15N_wider$N_cont_mg*branch_15N_wider$atm_perc_exc/100 # this is a value per cm!


# correcting factor: scale the real length of each branch in order to calculate the real amount of 15N applied per length (the former is calculated by
# using the length factor LF, which was obtained in the field by measuring the main sub-branches of each branch)

# Calculating the corrected applied 15N to each branch per cm

ratio.15N <- read_csv("15N_experiment/15N_branches/ratio_15N.csv") # ~/Daniele_Repo/ for laptop
ratio.15N = ratio.15N[rep(seq_len(nrow(ratio.15N)), each=4),]
ratio.15N$branch = as.factor(ratio.15N$branch)

branch_15N_corrected = cbind(branch_15N_wider, ratio.15N[ , 2])
branch_15N_corrected$applied_N15 = branch_15N_corrected$ratio* app

# 15N recovery per compartment
branch_15N_corrected$N15_rec = branch_15N_corrected$N_cont_mg / branch_15N_corrected$applied_N15 * 100

total.15N.rec = aggregate(N15_rec ~ branch, branch_15N_corrected, FUN = sum)

d15N.mean = aggregate(d15N_T ~ compartment, branch_15N_corrected, FUN = mean)
mean.15N.rec = aggregate(N15_rec ~ compartment, branch_15N_corrected, FUN = mean)
branch_15N_corrected$d15N.exc = branch_15N_corrected$d15N_T-branch_15N_corrected$d15N_C

#######             PLOTS             ##############
library(ggplot2)
# By Compartment:

## Dry weight of each organ

ggplot(branch_15N_corrected, aes(x=compartment, y=DM_by_length_T, fill=compartment))+
  geom_boxplot() + ggtitle("Dry weight (g/cm) per each organ")  + theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5)) +  guides(fill=FALSE) +
    labs(x = 'comparment', y = "N (%)") +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))

# N content per organ (mg/cm)
ggplot(branch_15N_corrected, aes(x=compartment, y=N_cont_mg, fill=compartment))+
  geom_boxplot() + theme_bw(base_size = 12) + theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("N content per organ (mg/cm)") + 
  labs(y = "N (mg/cm)") +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))+
  theme(axis.title.x=element_blank()) +
  theme(legend.position = "none")
  
## Total 15N recovery (see how to start an axis lab with superscript by using PASTE)

ggplot(branch_15N_corrected, aes(x=compartment, y=N15_rec, fill=compartment))+
  geom_boxplot() + ggtitle(expression(paste(' '^{15}, "N (%) recovered by compartment", sep = "")))  + theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5)) +  guides(fill=FALSE) +
  labs(y = (expression(paste(' '^{15}, "N / applied ", ' '^{15}, "N (%)", sep = ""))) ) +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))


