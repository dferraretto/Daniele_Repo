# --------------------------------------------------------
  #  15N on branches data analysis
  #  started on September 04th 2017
  #  updated: 18th September 2017, after the course at INRA
# --------------------------------------------------------
# --------------------------------------------------------
# clear the memory
rm(list=ls())

#   ignore these - they are different settings for my desktop that needs to know where to recover the libraries and 
# my laptop
.libPaths("C:/Workspace/R") # adjust the issues with my desktop demmerda
### set working dir for pc:
setwd("C:/Users/Daniele Ferraretto/Documents/Daniele_Repo")
### setwd per desktop
#setwd("M:/My PhD/R/PhD-local_repo")
setwd("C:/Users/s1373890/Daniele_Repo")
### END OF SETTINGS


library(readr)
branch_15N <- read_csv("15N_experiment/15N_branches/branch_15N.csv") # ~/Daniele_Repo/ in case needed from laptop

app = 0.004637028 # 15N applied to the branches (mg/cm, as calculated in field as an approximation of the total linear length of branches)

AR = 0.0036765 # R standard for 15N/14N

# Prepare the dataset: all characters to factors
cols <- c("T_C", "Girdling", "Tree", "compartment", "branch")

branch_15N[cols] <- lapply(branch_15N[cols], factor)

#g to mg for DM CHECKED (dry weight was expressed in grams in my .csv file)
branch_15N$DM_by_length = branch_15N$DM_by_length * 1000

# long to wide

library(data.table)
# https://stackoverflow.com/questions/37622935/wide-format-with-dcast-data-table:  dcast needs a table => setDT
branch_15N_wider = dcast(setDT(branch_15N), branch + compartment  ~ T_C, value.var = c("Total_N_perc","d15N", "DM_by_length"))

# R in labelled and unlabelled samples CHECKED

branch_15N_wider$atom_perc_sample = 100 * AR * (branch_15N_wider$d15N_T/1000 + 1) / (1 + AR * (branch_15N_wider$d15N_T/1000 + 1))

branch_15N_wider$atom_perc_control = 100 * AR * (branch_15N_wider$d15N_C/1000 + 1) / (1 + AR * (branch_15N_wider$d15N_C/1000 + 1))

# Atom % excess (15N excess perc) CHECKED

branch_15N_wider$atm_perc_exc = branch_15N_wider$atom_perc_sample - branch_15N_wider$atom_perc_control

# N content by length = [N] * DW / 100  CHECKED

branch_15N_wider$N_cont_mg = branch_15N_wider$Total_N_perc_T * branch_15N_wider$DM_by_length_T /100

# 15N excess perc converted to 15N exess content (mg/cm) CHECKED
branch_15N_wider$N15_excess_mg = branch_15N_wider$N_cont_mg*branch_15N_wider$atm_perc_exc/100 # this is a value per cm!


# correcting factor: scale the real length of each branch in order to calculate the real amount of 15N applied per length (the former is calculated by
# using the length factor LF, which was obtained in the field by measuring the main sub-branches of each branch)

# Calculating the corrected applied 15N to each branch per cm

ratio.15N <- read_csv("15N_experiment/15N_branches/ratio_15N.csv") # ~/Daniele_Repo/ for laptop
ratio.15N = ratio.15N[rep(seq_len(nrow(ratio.15N)), each=4),]
ratio.15N$branch = as.factor(ratio.15N$branch)

branch_15N_corrected = cbind(branch_15N_wider, ratio.15N[ , 2])
branch_15N_corrected$applied_N15 = branch_15N_corrected$ratio* app

# 15N recovery per compartment
branch_15N_corrected$N15_rec = branch_15N_corrected$N15_excess_mg / branch_15N_corrected$applied_N15 * 100

# MEAN 15N recover and value
total.15N.rec = aggregate(N15_rec ~ branch, branch_15N_corrected, FUN = sum)
mean(total.15N.rec[ , "N15_rec"])
N15SD = sd(total.15N.rec[ , "N15_rec"])
#error of mean 15N recovery:
N15SD/(10^0.5)*2.26

d15N.mean = aggregate(d15N_T ~ compartment, branch_15N_corrected, FUN = mean)
mean.15N.rec = aggregate(N15_rec ~ compartment, branch_15N_corrected, FUN = mean)
branch_15N_corrected$d15N.exc = branch_15N_corrected$d15N_T-branch_15N_corrected$d15N_C

#######             PLOTS             ##############
library(ggplot2)


## Dry weight of each organ by compartment

ggplot(branch_15N_corrected, aes(x=compartment, y=DM_by_length_T, fill=compartment))+
  geom_boxplot() + ggtitle("Dry weight of labelled branches (g/cm) per each organ")  + theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +  #axis.title.x rimuove il nome dell'asse lasciando quello dei livelli
  labs(y = "DM (g/cm)") + guides(fill=FALSE) + 
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))

# N content per organ (%). NOTE:  picea abies under 1,3% N in needles is deficient!
ggplot(branch_15N_corrected, aes(x=compartment, y=Total_N_perc_T, fill=compartment)) +
  geom_boxplot() + theme_bw(base_size = 12) + theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("N content per organ (%)") + 
  labs(y = "N (%)") +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))+
  theme(axis.title.x=element_blank()) +
  theme(legend.position = "none")

# PLOT the means and SE95:
# A1: MEAN depth value by sampling date 
N_perc_mean=aggregate(Total_N_perc_T ~ compartment, data = branch_15N_corrected, FUN = mean, na.rm = TRUE )

# A2: SD of depth value by sampling date
N_perc_SD=aggregate(Total_N_perc_T ~ compartment, data = branch_15N_corrected, FUN = sd, na.rm = TRUE )

# A4. SE = SD/(N)^0.5. SE (95%) = 2.26*SE; df = 9. 90% = 1.83
N_perc_SE.95 =  as.data.frame(1.83*N_perc_SD$Total_N_perc_T/(10^0.5))

N_perc.table = cbind(N_perc_mean, N_perc_SE.95)
names(N_perc.table) = c("compartment", "N_perc_mean","N_perc_SE")

########### N content per organ (%) AS MEAN VAL AND ERROR 90% OR 95% (CHECK ABOVE)
ggplot(N_perc.table, aes(x=compartment, y=N_perc_mean)) +
  geom_point() + theme_bw(base_size = 12) + 
  geom_errorbar(aes(ymin=N_perc_mean-N_perc_SE, ymax=N_perc_mean+N_perc_SE), width=.1) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("N content (% on DM) per compartment") + 
  labs(y = "N (%)") +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))+
  theme(axis.title.x=element_blank()) +
  theme(legend.position = "none")  

## Total 15N recovery (see how to start an axis lab with superscript by using PASTE)

ggplot(branch_15N_corrected, aes(x=compartment, y=N15_rec, fill=compartment))+
  geom_boxplot() + ggtitle(expression(paste(' '^{15}, "N (%) recovered by compartment", sep = "")))  + theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5)) +  guides(fill=FALSE) +
  labs(y = (expression(paste(' '^{15}, "N / applied ", ' '^{15}, "N (%)", sep = ""))) ) +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))


