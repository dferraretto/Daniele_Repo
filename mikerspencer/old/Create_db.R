# ----------------------------------------------
# ----------------------------------------------
# Set up Griffin database
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# Set up
# ----------------------------------------------
#install.packages("RSQLite")
library(RSQLite)
library(sqldf) # I addes this after readin rbloggers
# Define working directory
# Make this the project directory, not repo
setwd("M:/My PhD/R/mikerspencer-griffin-e2070d91e30c/mikerspencer-griffin-e2070d91e30c")

# ----------------------------------------------
# Create db
# ----------------------------------------------
db = dbConnect(SQLite(), dbname="~Griffin.SQLite")

# Create field table
dbSendQuery(db,
      "CREATE TABLE fielddata
         (date DATETIME,
         time DATETIME,
         sample TXT,
         site TXT,
         variable TXT,
         vals REAL,
         overflowing TXT,
         QC TXT,
         comments TXT,
         PRIMARY KEY(date, sample, variable))
")

# Create lab table
dbSendQuery(db,
      "CREATE TABLE labdata
         (date DATETIME,
         sample TXT,
         site TXT,
         variable TXT,
         vals REAL,
         QC TXT,
         comments TXT,
         PRIMARY KEY(date, sample, variable))
")

# Consider writing a meta data table
# e.g. describing variables

# To delete a table
# dbSendQuery(db, "DROP TABLE fielddata")