--------------------------------------------------------
  #  15N on branches data analysis
  #  started on August 16th 2017
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

library(readr)
branch_15N <- read_csv("C:/Users/s1373890/Daniele_Repo/15N_experiment/15N_branches/branch_15N.csv")

summary(branch_15N)

cols <- c("T_C", "Girdling", "Tree", "compartment")

# Use lapply() to coerce and replace the chosen columns:
  
branch_15N[cols] <- lapply(branch_15N[cols], factor)


library(ggplot2)
treated_branches = branch_15N[branch_15N$T_C == "T",]
control_branches = branch_15N[branch_15N$T_C == "C",]
# d15N in treatment twigs and leaves
p1 = ggplot(treated_branches, aes(x=compartment, y=d15N, fill=factor(compartment)))+
  geom_boxplot() + theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
                         plot.background = element_rect(fill = "transparent",colour = NA)) +
    ggtitle(expression(~delta^15*N~"(\u2030) (treatment)")) + 
  labs(y = expression(~delta^15*N~"(\u2030)")) +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))

p2 = ggplot(control_branches, aes(x=compartment, y=d15N, fill=factor(compartment)))+
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
                        plot.background = element_rect(fill = "transparent",colour = NA)) +
  theme_bw(base_size = 12) +
  ggtitle(expression(~delta^15*N~"(\u2030) (control)")) + 
  labs(y = expression(~delta^15*N~"(\u2030)")) +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))


# Girdling effect?
p3 = ggplot(treated_branches, aes(x=compartment, y=d15N, fill=Girdling))+
  geom_boxplot() + theme_bw(base_size = 12) +
theme(plot.title = element_text(hjust = 0.5, size = 16, colour = 'red4'),
      plot.background = element_rect(fill = "transparent",colour = NA)) +
     ggtitle(expression("Girdling effect on"~delta^15*N~"(treatment)")) + 
  labs(y = expression(~delta^15*N~"(\u2030)")) +
scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs')) +
  scale_fill_discrete(name="Girdling",
                      breaks=c("FALSE", "TRUE"),
                      labels=c("Girdled", "Not girdled"))


#tree effect? 
p4 = ggplot(control_branches, aes(x=compartment, y=d15N, fill=factor(Tree)))+
  geom_boxplot() + 
  theme(plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
                         plot.background = element_rect(fill = "transparent",colour = NA)) +
  theme_bw(base_size = 12) +
  ggtitle(expression("Tree effect on"~delta^15*N~"(control)")) + 
  labs(y = expression(~delta^15*N~"(\u2030)")) +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))
  

# total N in T/C
p5 = ggplot(branch_15N, aes(x=compartment, y=Total_N_perc, fill=factor(T_C)))+
  geom_boxplot() + 
  theme(plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
                         plot.background = element_rect(fill = "transparent",colour = NA)) +
  theme_bw(base_size = 12) +
  ggtitle(expression("Total nitrogen in leaves and twigs")) + 
  labs(y = "N (% on DM)") +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))
  

# total N by tree
ggplot(branch_15N, aes(x=compartment, y=Total_N_perc, fill=factor(Tree)))+
  geom_boxplot() + ggtitle("total N by tree")

# total N by height
p6 = ggplot(branch_15N, aes(x=compartment, y=Total_N_perc, fill=factor(height)))+
  geom_boxplot() + ggtitle("total N by height") +
  theme(plot.title = element_text(hjust = 0.1, size = 16, colour = 'red4'),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  theme_bw(base_size = 12) +
  labs(y = "N (% on DM)") +
  scale_x_discrete(labels = c('new leaves','new twigs','old leaves', 'old twigs'))

library(Rmisc)
multiplot(p1, p3, p5, p2, p4, p6, cols = 2)

#############################################################################
#                           15N   RECOVERY
#
# PREPARE THE DATASET TO CALCULATE RECOVERY BY SUBTRACTING d15N(T) - d15N(C)
d15N_wide = merge (treated_branches, control_branches, by = c("branch", "compartment"))
d15N_wide = d15N_wide[ , c(1,2,4,5,8,13,14)]
names(d15N_wide) = c("branch", "comparment", "N_perc_T", "d15N_T", "girdling", "N_perc_C", "d15N_C")
d15N_wide$N15_rec = (d15N_wide$d15N_T - d15N_wide$d15N_C)/