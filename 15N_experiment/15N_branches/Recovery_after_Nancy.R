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

AR = 0.0036765 # R standard for 15N/14N

# Prepare the dataset: all characters to factors
cols <- c("T_C", "Girdling", "Tree", "compartment", "branch")

branch_15N[cols] <- lapply(branch_15N[cols], factor)

# long-ish to wide

library(data.table)
# https://stackoverflow.com/questions/37622935/wide-format-with-dcast-data-table: per dcast mi serve una table => setDT
branch_15N_wider = dcast(setDT(branch_15N), branch + compartment  ~ T_C, value.var = c("Total_N_perc","d15N", "DM_by_length"))


# 15N recovery in BRANCHES per compartment, %: (see 15N formula explained.doc, adapted)
# 15Nsample = (cd+d)/1000/(1+(cd+d)/1000)   15Nexcess



# Recovery after Nancy


# atm% (Nancy 1)
branch_15N_wider$atm_perc = 100 * AR * (branch_15N_wider$d15N_T/1000 + 1) / (1 + AR * (branch_15N_wider$d15N_T/1000 + 1))

