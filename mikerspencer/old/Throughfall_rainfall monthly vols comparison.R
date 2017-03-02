# --------------------------------------------------------
#  Daniele Ferraretto, working on data in Griffin,SQLite
#  started on 26th November, 2015
#  updated: 26/11/2015          last update: 26/11/2015
# --------------------------------------------------------

# I create 4 dataframes, TF, TFT, TFC and RF aggregating the
# mean daily depths by month. Then I create a tf.rf df to plot
# and a table by merging the 4 df.

setwd("M:/My PhD/R/PhD-local_repo")

library(RSQLite)

db = dbConnect(SQLite(), dbname="field_lab/Daily_Griffin.SQLite")


# --------------------------------------------------------
#         1. THROUGHFALL DEPTH (l/m2/month)
# --------------------------------------------------------

tf  = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' AND vals>=0 ORDER BY date")

# aggregate vals by month and sample:
meantf <- aggregate(vals ~ date, data = tf, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# create a column to aggregate by month or year
meantf$mY=strftime(meantf$date,"%Y%m")

monthlyTF = aggregate(vals ~ mY,  data = meantf, FUN = sum)
tfdepth = "TF depth"
both = "both"
mTF=data.frame(cbind(monthlyTF, both, tfdepth))
colnames(mTF)=c("mY", "vals", "site", "var")

### throughfall depth in TREATMENT by month

tfT  = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' AND vals>=0 and site = 'Treatment' ORDER BY date")

# aggregate vals by month and sample:
meantfT <- aggregate(vals ~ date, data = tfT, FUN = mean,  na.rm = TRUE)
# create a column to aggregate by month or year
meantfT$mY=strftime(meantfT$date,"%Y%m")

monthlyTFT = aggregate(vals ~ mY,  data = meantfT, FUN = sum)
tfdepth = "TF depth"
treatment = "treatment"
mTFT=data.frame(cbind(monthlyTFT, treatment, tfdepth))
colnames(mTF)=c("mY", "vals", "site", "var")

#### TF DEPTH IN CONTROL

tfC  = dbGetQuery(db, "SELECT * FROM fielddata WHERE variable = 'through depth' AND vals>=0 AND site = 'Control' ORDER BY date")

# aggregate vals by month and sample:
meantfC <- aggregate(vals ~ date, data = tfC, FUN = mean,  na.rm = TRUE) #wrong, sums all samples but I need an average

# create a column to aggregate by month or year
meantfC$mY=strftime(meantfC$date,"%Y%m")

monthlyTFC = aggregate(vals ~ mY,  data = meantfC, FUN = sum)
tfdepth = "TF depth"
control = "control"
mTFC=data.frame(cbind(monthlyTFC, control, tfdepth))
colnames(mTF)=c("mY", "vals", "site", "var")

###########################################
###           RAINFALL DEPTH            ###
###########################################
rf = dbGetQuery(db, "SELECT * FROM fielddata WHERE sample = 'C30D1' or sample = 'C31D1'   ORDER BY date")

meanrf <- aggregate(vals ~ date, data = rf, FUN = mean,  na.rm = TRUE)

meanrf$mY=strftime(meanrf$date,"%Y%m")

monthlyrf = aggregate(vals ~ mY, data = meanrf, FUN = sum)
RF = "RF"
mRF=data.frame(cbind(monthlyrf, both, RF))
colnames(mRF)=c("mY", "vals", "site", "var")


# Now rbind TF&RF
tf.rf=rbind(mRF, mTF)
# Create a table to compare tf and rf monthly depths
tf.rf.table1=merge(monthlyrf, monthlyTF, by="mY")
tf.rf.table2=merge(monthlyTFC, monthlyTFT, by="mY")
tf.rf.table=merge(tf.rf.table1,tf.rf.table2, by="mY")
colnames(tf.rf.table)=c("year month", "prec (mm)", "TF(mm)", "TF Control (mm)", "TF Treatment (mm)")

#   Export depths table   -------------------------

library(utils)
write.table(tf.rf.table, "M:/My PhD/R/PhD-local_repo/output_tables_plots/TFRFtable_20151221.txt", sep="\t") 


tf.rf$Y=strtrim(tf.rf$mY,4) # creates a column "Year" to be uset to facet
yearly.tf.rf=aggregate(vals ~ Y+var, tf.rf, FUN = sum )


###           PLOT           #### 

library(ggplot2)

ggplot(tf.rf, aes(mY, vals, color=var)) + geom_point() + geom_smooth()



