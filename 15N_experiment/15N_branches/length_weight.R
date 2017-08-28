# clear the memory
rm(list=ls())

.libPaths("C:/Workspace/R") # adjust the issues with my desktop demmerda

#setwd("M:/My PhD/R/PhD-local_repo")
setwd("C:/Users/s1373890/Daniele_Repo")

library(readr)


### BASIC WORK ON Weight by length
weights_per_length <- read_csv("C:/Users/s1373890/Daniele_Repo/15N_experiment/15N_branches/weights_per_length.csv")

weights_per_length$tozzy = weights_per_length$`twigs weight`/weights_per_length$Length # tozzy is in DM(g)/cm
names(weights_per_length) = c("code", "length", "needles_weight", "twigs_weight", "tozzy")

library(ggplot2)

#via BN2:
weights_per_length = weights_per_length[!weights_per_length$code == "BN2",]

ggplot(weights_per_length, aes(x=tozzy, y=needles_weight)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(span=0.99)+
  ggtitle("Tuttifrutti meno BN2")

# Import scaled values for new and old twigs from the application field data

From_diameters_to_DM_per_length <- read_csv("C:/Users/s1373890/Daniele_Repo/15N_experiment/15N_branches/From_diameters_to_DM_per length.csv")
# trim useless lines
From_diameters_to_DM_per_length = From_diameters_to_DM_per_length[1:10,]

new_leaves = as.data.frame(predict(loess(needles_weight~tozzy,weights_per_length), From_diameters_to_DM_per_length$new_twigs))

old_leaves = as.data.frame(predict(loess(needles_weight~tozzy,weights_per_length), From_diameters_to_DM_per_length$old_twigs))

new_twigs =  as.data.frame(predict(loess(twigs_weight~tozzy,weights_per_length), From_diameters_to_DM_per_length$new_twigs))

old_twigs =  as.data.frame(predict(loess(twigs_weight~tozzy,weights_per_length), From_diameters_to_DM_per_length$old_twigs))

# Con queste 4 linee ho predetto i valori che usero' per scalare il 15N sui ramozzi trattati. Ho gia' riportato tutto in .csv "15N_branches"

########################################################################
# altri modelli altenrativi poi scartati

# via outlier new
a = min(weights_per_length$tozzy)

weights_per_length = weights_per_length[!weights_per_length$tozzy == a,]

# via AO2 (old)
weights_per_length = weights_per_length[!weights_per_length$code == "BO2",]

# new vs old

new = weights_per_length[grep("N", weights_per_length$code),]
old = weights_per_length[grep("O", weights_per_length$code),]

ggplot(new, aes(x=tozzy, y=needles_weight)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   #  born to be linear! 

# OLD - all
ggplot(old, aes(x=tozzy, y=needles_weight)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(span = 0.9)  + # born to be curvy 
ggtitle("OLD twigs - all data")
  
smooth_vals_OLDALL = predict(loess(needles_weight~tozzy,old), old$tozzy)

# plot and predicted values without AO2
old_no_AO2 = old[!old$code == "AO2",]  

ggplot(old_no_AO2, aes(x=tozzy, y=needles_weight)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(span=0.85)  + # born to be curvy 
  ggtitle("OLD twigs - !AO2 data")

smooth_vals_old_noAO2 = predict(loess(needles_weight~tozzy,old_no_AO2), old$tozzy)

# plot and predicted values without BO2
old_no_BO2 = old[!old$code == "BO2",]  

ggplot(old_no_BO2, aes(x=tozzy, y=needles_weight)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()  + # born to be curvy 
  ggtitle("OLD twigs - !BO2 data")

smooth_vals_old_noAO2 = predict(loess(needles_weight~tozzy,old_no_AO2), old$tozzy)

# plot and predicted values without AO2 AND BO2
old_no_ABO2 = old[!old_no_BO2$code == "AO2",]  

ggplot(old_no_ABO2, aes(x=tozzy, y=needles_weight)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(span = 0.8)  + # born to be curvy 
  ggtitle("OLD twigs - !BO2 data")

smooth_vals_old_noABO2 = predict(loess(needles_weight~tozzy,old_no_ABO2), old$tozzy)


