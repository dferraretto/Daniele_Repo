# QUESTO FILE MI SERVERE A CALCOLARE LE MASSE SECCHE DI AGHI E RAMOSCELLI,
# DATE LE PESATE IN LAB E IL DIAMETRO DEI RAMI CONSIDERATI

# clear the memory
rm(list=ls())

.libPaths("C:/Workspace/R") # adjust the issues with my desktop demmerda

#setwd("M:/My PhD/R/PhD-local_repo")
setwd("C:/Users/s1373890/Daniele_Repo")

library(readr)


### BASIC WORK ON Weight by length
weights_per_length <- read_csv("C:/Users/s1373890/Daniele_Repo/15N_experiment/15N_branches/weights_per_length.csv")

weights_per_length$tozzy = weights_per_length$`twigs weight`/weights_per_length$Length # tozzy is in DM(g)/cm
weights_per_length$needles_tozzy = weights_per_length$`Leaf weight`/weights_per_length$Length # needles weight in DM(g)/cm

names(weights_per_length) = c("code", "length", "needles_weight", "twigs_weight", "twigs_DM_cm", "needles_DM_cm")

library(ggplot2)

#via BN2:
weights_per_length = weights_per_length[!weights_per_length$code == "BO3",]

ggplot(weights_per_length, aes(x=twigs_DM_cm, y=needles_DM_cm)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(span=1)+
  ggtitle("Tutti valori di twigs DM per cm < 0.04") # span = 1, escluso BO3, bella curva con un max che mostra che dopo un certo
# diametro del rametto la massa degli aghi cala (ed e' proprio cosi')

# Import scaled values for new and old twigs from the application field data

From_diameters_to_DM_per_length <- read_csv("C:/Users/s1373890/Daniele_Repo/15N_experiment/15N_branches/From_diameters_to_DM_per length.csv")
# trim useless lines
From_diameters_to_DM_per_length = From_diameters_to_DM_per_length[1:10,]

## ottieni i valori di peso di aghi e twigs dal geom_smooth lm "Tutti valori..."  https://stat.ethz.ch/pipermail/r-help/2011-October/291411.html
new_leaves = as.data.frame(predict(loess(needles_DM_cm~twigs_DM_cm, weights_per_length), From_diameters_to_DM_per_length$new_twigs))

old_leaves = as.data.frame(predict(loess(needles_DM_cm~twigs_DM_cm, weights_per_length), From_diameters_to_DM_per_length$old_twigs))

# new_twigs =  as.data.frame(predict(loess(twigs_weight~tozzy,weights_per_length), From_diameters_to_DM_per_length$new_twigs))

# old_twigs =  as.data.frame(predict(loess(twigs_weight~tozzy,weights_per_length), From_diameters_to_DM_per_length$old_twigs))

# Con queste 2 linee (foglie) ho predetto i valori che usero' per scalare il 15N sui ramozzi trattati. Ho gia' riportato tutto in .csv "15N_branches"

