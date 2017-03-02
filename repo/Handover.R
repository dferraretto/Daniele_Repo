# ----------------------------------------------
# ----------------------------------------------
# Creates db as handed over to Daniele
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# Set up
# ----------------------------------------------
.libPaths("C:/Workspace/R")

rm(list=ls())

# Define working directory

### set working dir for pc:
setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd 4 desktop
setwd("M:/My PhD/R/PhD-local_repo")

# source the scripts
source("repo/Create_db.R")
source("repo/Write2db.R")


# ----------------------------------------------
# Write MS data to db
# ----------------------------------------------

# Files to import
f = list.files("field_lab", pattern="xls", full.names=T)

# Write to db
lapply(f, field.data)
lapply(f, lab.data)


# ----------------------------------------------
# Write pre MS data to db
# ----------------------------------------------

source("repo/old_data.R")

# Note: litter, acidification and POC data not in master sheet, so script not written.
# Throughfall master sheet altered as DF sent one had date errors.

# ----------------------------------------------
# Disconnect and tidy
# ----------------------------------------------

dbDisconnect(db)
rm(f, field.data, lab.data, db, bucket, NH4, NO3, precip, sb, stem, stream, through, V.notch, v.notch.lookup, circle, colander, funnel, gutter, large, small, square)

