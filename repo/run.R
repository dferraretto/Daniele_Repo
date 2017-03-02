# ----------------------------------------------
# ----------------------------------------------
# Example run file for Griffin data processing
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# Set up
# ----------------------------------------------

# Define working directory

### set working dir for pc:
setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd 4 desktop
setwd("M:/My PhD/R/PhD-local_repo")

source("repo/Write2db.R")

# ----------------------------------------------
# Field to database
# ----------------------------------------------

# Files to import
f = head(list.files("field_lab", pattern="xls", full.names=T), -2)

# Connect to db
db = dbConnect(SQLite(), dbname="repo/Griffin.SQLite")

# Read a file, process and write file to db
field.data(f[1])
lab.data(f[1])

# Process a set of files
lapply(f, field.data)
lapply(f, lab.data)

x=dbGetQuery(db, "SELECT * FROM fielddata")

# Depending on how you manage your files, you could add a script to move processed files to an archive folder. Something like (untested):

lapply(f, function(i){
   system(paste0("mv ", i, "~/dir/dir/archive"))
})
