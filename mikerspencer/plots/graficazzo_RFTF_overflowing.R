### MI SERVONO GLI oVERFLOWS!!!!

dbDisconnect(db)

db = dbConnect(SQLite(), dbname="field_lab/Griffin.SQLite")

rf = dbGetQuery(db, "SELECT * FROM fielddata WHERE sample = 'C30D1' or sample = 'C31D1'   ORDER BY date")

rf$overflowing = replace(rf$overflowing, c(7,8,18,56,63,70,79,100,101), 1)

rf.OF= rf[, c("date","overflowing")]

  rf.OF$date = as.Date(rf.OF$date)
  rf.OF$overflowing = as.numeric(rf.OF$overflowing*100)
  rf.OF[rf.OF == 0] <- NA # turn 0 into NA
  
  long.TF.RF.depth =  melt(TF.RF.depth, id.vars = "date") 
  
  
  library(zoo)
  
  rf.OF$date = format(rf.OF$date, "%Y%m") # adapt date format to melt with RF/TF 

ggplot() + geom_line(data=long.TF.RF.depth, aes(x=date, y= value,  ymin=1, ymax=100, colour = variable, group = variable)) +
           geom_text(data=rf.OF, aes(x=date, y=overflowing, ymin=1, stat="identity"), size=3, label = "OF")

           