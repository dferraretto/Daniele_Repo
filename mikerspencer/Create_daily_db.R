# ----------------------------------------------
# ----------------------------------------------
# Set up Griffin  DAILY database
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# Set up
# ----------------------------------------------
#install.packages("RSQLite")

library(RSQLite)


# ----------------------------------------------
# Create db
# ----------------------------------------------
db = dbConnect(SQLite(), dbname="field_lab/Daily_Griffin.SQLite")
# this works on pc only

# Create field table
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
            site TXT,
            variable TXT,
            vals REAL,
            PRIMARY KEY(date, sample, variable))
            ")
