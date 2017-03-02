# ----------------------------------------------
# ----------------------------------------------
# Set up Griffin daily database
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# Set up
# ----------------------------------------------

library(RSQLite)

### set working dir for pc:
setwd("C:/Users/Daniele Ferraretto/Documents/PhD-local_repo")
### setwd 4 desktop
setwd("M:/My PhD/R/PhD-local_repo")

# ----------------------------------------------
# Create db
# ----------------------------------------------
# Create field table

db = dbConnect(SQLite(), dbname="field_lab/Griffin_daily.SQLite")
# this works on pc only


dbSendQuery(db,
            "CREATE TABLE fielddata
            (date DATETIME,
            sample TXT,
            site TXT,
            variable TXT,
            vals REAL,
            overflowing TXT,
            QC TXT,
            PRIMARY KEY(date, sample, variable))
            ")

# Create lab table
dbSendQuery(db,
            "CREATE TABLE labdata
            (date DATETIME,
            sample TXT,
            Nform TXT,
            site TXT,
            vals REAL,
            QC TXT,
            PRIMARY KEY(date, sample, Nform))
            ")

dbDisconnect(db)
