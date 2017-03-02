# ----------------------------------------------
# ----------------------------------------------
# Define median litter tray weight
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# Set up
# ----------------------------------------------

# Define working directory
# Make this the project directory, not repo
setwd("~/Copy/Michael/Uni_temp/Griffin")
library(readxl)

# Based on 

# Read data
f = list.files("Sampling/Hydrology", pattern="xls", full.names=T)[c(1,3:10)]

trays = lapply(f, function(i){read_excel(i, sheet=6, skip=1)})
# extract sample and tray columns
trays = lapply(trays, function(i){i[,c(1, 3)]})
trays = do.call("rbind", trays)
names(trays)[2] = "tray"

# Variation
boxplot(tray ~ Sample, trays)

# Median for each sample
tray.m = tapply(trays$tray, trays$Sample, median, na.rm=T)
tray.m = data.frame(Sample = names(tray.m), tray.med = tray.m)

# Merge into sample order
x = merge(trays[1:18,], tray.m, sort=F)

# Write to csv
write.csv(x[,c(1,3)], "Sampling/Hydrology/tray_mass.csv", row.names=F)
