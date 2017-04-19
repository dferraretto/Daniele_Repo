# Griffin data processing and plotting repo

## Contents

Files for reading raw field and lab data and writing to an SQLite database.

* **Handover.R** creates the db as handed over to Daniele and runs some of the subsequent files
* **Create_db.R** makes the db and two tables, field and lab (run as part of Handover.R)
* **Write2db.R** contains functions which take monthly field and lab data and appends them to the existing db. You need to source this file to load the functions
* **run.R** This is an example workflow for running the functions in Write2db.R
* **old_data.R** Takes Pre Nov 2013 data and writes to db (run as part of Handover.R)
* **sampling_summary.Rmd/html** An example of Rmarkdown in action, writing to a webpage. I'm using this to check for spurious outliers in the data
* **Trays.R** Finds median tray mass over 9 months and writes to csv. The results are now in the data template.
* **Outliers_Aug2016 It detects potential outliers. Run it and accept the removal only for the two vals from fieldwork prec (see script). 
* **Outliers.R** to be run after Handover.R, corrects Outliers previously detected by  Outliers_Aug2016 (row ID has shown to be a bad key to remove lines, changed for a multiple conditions on columns
* **fog_C30D1_regression ultimately corrects OF, QC and other not acceptable (negative) fog values with predictions of a regression

