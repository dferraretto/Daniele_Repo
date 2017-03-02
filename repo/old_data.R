# ----------------------------------------------
# ----------------------------------------------
# For reading pre Nov 2013 data to db
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# Setup
# ----------------------------------------------
library(gdata)
library(RSQLite)
library(reshape2)


# ----------------------------------------------
# Precipitation
# ----------------------------------------------
print("precip")
# Read
precip = read.xls("repo/Master_sheet_water_data_old.xls", sheet=6, stringsAsFactors=F)


# Strip unwanted
precip = precip[1:85 ,c(1:5, 10)]
# Drop rows with no sample
precip = precip[!is.na(precip$Bottle...sample..g.),]

# Make date column
precip$Date = as.Date(precip$Date, format="%d/%m/%y")

# Define funnel size
funnel = pi * 7.5^2
# acid
precip$acid = 4.225
precip$acid[precip$Bottle.code == "C30D2"] = 4.225*2
# Convert mass to depth/day
precip$depth = (((precip$Bottle...sample..g. - precip$Bottle.empty..g.) - precip$acid) / funnel / precip$Time.from.previous.sampling) * 10
# Prep for db
precip = data.frame(date=as.character(precip$Date), time=precip$HO..Harp.fallen.off, sample=precip$Bottle.code, site="Both", variable="precip depth", vals=precip$depth, overflowing="", QC="", comments="")
# Write to db
dbWriteTable(conn=db, name="fielddata", precip, append=T, row.names=F)


# ----------------------------------------------
# Stream water
# ----------------------------------------------
print("stream")

stream = read.xls("repo/Master_sheet_water_data_old.xls", sheet=5, stringsAsFactors=F)

# Ditch unwanted
stream = stream[1:300, c(1,3:7)]

# Split data by dates
stream = lapply(unique(stream$Date), function(i){
   x = stream[stream$Date==i,]
   x = x[-3,]
   x = x[-nrow(x),]
   x$Date = as.Date(x$Date, "%d/%m/%y")
   x
})

# Stageboard
# ----------------------------------------------
sb = lapply(stream, function(i){
   # Subset row of interest
   x = i[2, -1]
   # Wide to long
   melt(x, id.vars="Date")
})
sb = do.call("rbind", sb)
# prep for db
sb$value = as.numeric(sb$value)
sb = data.frame(date=as.character(sb$Date), time="", sample=sb$variable, site="", variable="stageboard cm", vals=sb$value, overflowing="", QC="", comments="")


# V-notch
# ----------------------------------------------
# v notch lookup
# p161 from Discharge measurement structures, Bos, 1989
v.notch.lookup = read.csv("repo/Vnotch.csv")

V.notch = lapply(stream, function(i){
   # Subset row of interest
   x = i[1, -1]
   # Wide to long
   x = melt(x, id.vars="Date")
   # Convert obs to metres
   x$value = as.numeric(x$value) / 100
   # Look up Q
   x = merge(x, v.notch.lookup, by.x="value", by.y="h")
   # Convert to l/s
   x$Q = x$Q * 1000
   x
})
V.notch = do.call("rbind", V.notch)
# prep for db
V.notch = data.frame(date=as.character(V.notch$Date), time="", sample=V.notch$variable, site="", variable="v-notch flow", vals=V.notch$Q, overflowing="", QC="", comments="")

# Gauging
# ----------------------------------------------
bucket = lapply(stream, function(i){
   # Subset rows of interest
   x = i[3:nrow(i), -1]
   # Wide to long
   vol = melt(x[1:(nrow(x)/2), ], id.vars="Date")
   x = melt(x[((nrow(x)/2)+1):nrow(x), ], id.vars="Date")
   x$vol = vol$value
   x$value = as.numeric(x$vol) / as.numeric(x$value)
   x = data.frame(x$Date[1], melt(tapply(x$value, x$variable, mean, na.rm=T)))
   names(x)[1:2] = c("Date", "sample")
   x$value[is.nan(x$value)] = NA
   x
})
bucket = do.call("rbind", bucket)
# Remove NaNs
bucket = bucket[is.finite(bucket$value),]
# prep for db
bucket = data.frame(date=as.character(bucket$Date), time="", sample=bucket$sample, site="", variable="gauged flow", vals=bucket$value, overflowing="", QC="", comments="")

# Combine variables
stream = rbind.data.frame(sb, V.notch, bucket)

# Site
stream$site = substr(stream$sample, 1, 1)
stream$site[stream$site=="C"] = "Control"
stream$site[stream$site=="T"] = "Treatment"

# Write to db
dbWriteTable(conn=db, name="fielddata", stream, append=T, row.names=F)


# ----------------------------------------------
# Stem flow
# ----------------------------------------------
print("stem flow")

# Read data
stem = read.xls("repo/Master_sheet_water_data_old.xls", sheet=4, stringsAsFactors=F, skip=1, na.strings="M")

# Ditch unwanted
stem = stem[3:80, 1:24]

# Define which collectors are large/small barrels
# Checked on 2014-05-21
small = c(1:9, 11, 13:17)
large = c(10, 12, 18:22)

# Split by date
stem = lapply(unique(stem$Date..dd.mm.yy.), function(i){
   x = stem[stem$Date..dd.mm.yy.==i,]
   x = x[-2,-1]
   x$Date..dd.mm.yy. = as.Date(x$Date..dd.mm.yy.)
   depth = melt(x[1,], id.vars = "Date..dd.mm.yy.")
   depth$value = as.numeric(depth$value)
   x = melt(x[2,], id.vars = "Date..dd.mm.yy.", value.name="comments")
   # Calculate volume
   x$vals[large] = (depth$value[large] - 0.8857) / 0.523
   x$vals[small] = (depth$value[small] - 0.6078) / 1.3756
   x
})
stem = do.call("rbind", stem)

# Change comments and add QC
stem$QC = ""
stem$QC[stem$comments=="B"] = T
stem$comments[stem$comments=="B"] = "Broken"
stem$overflowing = ""
stem$overflowing[stem$comments=="F"] = T
stem$comments[stem$comments=="F"] = ""
stem$comments[is.na(stem$comments)] = ""

# Label sites
stem$site = substr(stem$variable, 1, 1)
stem$site[stem$site=="C"] = "Control"
stem$site[stem$site=="T"] = "Treatment"

# Prep for db
stem = data.frame(date=as.character(stem$Date..dd.mm.yy.), time="", sample=stem$variable, site=stem$site, variable="stem vol", vals=stem$vals, overflowing=stem$overflowing, QC=stem$QC, comments=stem$comments)
# Write to db
dbWriteTable(conn=db, name="fielddata", stem, append=T, row.names=F)


# ----------------------------------------------
# Through fall
# ----------------------------------------------
print("throughfall")

# Read data
through = read.xls("repo/Master_sheet_water_data_old.xls", sheet=3, stringsAsFactors=F, skip=2, na.strings="M")

# Ditch unwanted
through = through[4:107, 1:20]
# Does this need Sep/Oct 2013 data adding?

# Barrel sizes
# Checked on 2014-05-21
square = c(1, 8, 9, 12:15)
circle = c(2:7, 10, 11, 16:18)
# Collection area
gutter = c(16, 16, 16, 15, 14, 14, 15, 17, 16, 13, 17, 12, 15, 14, 14, 15, 17, 16)
gutter = cos(gutter * pi / 180)
colander = pi * (12.25 / 100) ^ 2

# Split by date
through = lapply(unique(through$Date), function(i){
   x = through[through$Date==i,]
   x = x[c(1,4),-1]
   x$Date = as.Date(x$Date)
   depth = melt(x[1,], id.vars = "Date")
   depth$value = as.numeric(depth$value)
   x = melt(x[2,], id.vars = "Date", value.name="comments")
   # Merge in comments
   x = merge(x, depth, by=c("Date", "variable"), sort=F)
   # Calculate volume
   x$vol[circle] = (x$value[circle] - 0.8857) / 0.523
   x$vol[square] = (x$value[square] - 0.9357) / 0.791
   # Depth
   x$depth = (x$vol * 10^6) / (4.02 * 0.114 * 2 * gutter + colander) / 10^6 # from .234 to .114
   x
})
through = do.call("rbind", through)

# Label sites
through$site = substr(through$variable, 1, 1)
through$site[through$site=="C"] = "Control"
through$site[through$site=="T"] = "Treatment"

# Prep for db
through = data.frame(date=as.character(through$Date), time="", sample=through$variable, site=through$site, variable=rep(c("through depth", "through vol"), each=nrow(through)), vals=c(through$depth, through$vol), overflowing="", QC="", comments=through$comments)
# Write to db
dbWriteTable(conn=db, name="fielddata", through, append=T, row.names=F)

# ----------------------------------------------
# Litter fall
# ----------------------------------------------

# All data are not in the master sheet.

# ----------------------------------------------
# DOC acidifcation
# ----------------------------------------------

# All data are not in the master sheet.

# ----------------------------------------------
# POC
# ----------------------------------------------

# All data are not in the master sheet.

# ----------------------------------------------
# NO3
# ----------------------------------------------
print("NO3")

# Read data
NO3 = read.xls("repo/Master_sheet_water_data_old.xls", sheet=1, stringsAsFactors=F, skip=1, na.strings=c("M", "No sample", ""))

# Ditch unwanted
NO3 = NO3[2:27, 1:51]

# Wide to long
NO3 = melt(NO3, id.vars = "Date")

# Concentration
NO3$NO3.N = as.numeric(NO3$value) * 14 / 62

# Split by date
NO3 = lapply(unique(NO3$Date), function(i){
   # Pick those dates that match
   x = NO3[NO3$Date==i,]
   # Convert to date
   x$Date = as.Date(x$Date, "%d/%m/%y")
   # Subtract blank from results
   x$NO3.N[-48] = x$NO3.N[-48] - x$NO3.N[48]
   # Make <0 values 0
   x$NO3.N[x$NO3.N < 0] = 0
   x
})
NO3 = do.call("rbind", NO3)

# Label sites
NO3$site = substr(NO3$variable, 1, 1)
NO3$site[NO3$site=="C"] = "Control"
NO3$site[NO3$site=="T"] = "Treatment"
NO3$site[NO3$site=="B"] = "Both"

# Prep for db
NO3 = data.frame(date=as.character(NO3$Date), sample=NO3$variable, site=NO3$site, variable="NO3.N", vals=NO3$NO3.N, QC="", comments="")
# Write to db
dbWriteTable(conn=db, name="labdata", NO3, append=T, row.names=F)


# ----------------------------------------------
# NH4
# ----------------------------------------------
print("NH4")

# Read data
NH4 = read.xls("repo/Master_sheet_water_data_old.xls", sheet=2, stringsAsFactors=F, skip=1, na.strings=c("M", "No sample", ""))

# Ditch unwanted
NH4 = NH4[1:26, 1:51]

# Wide to long
NH4 = melt(NH4, id.vars = "Date")

# Concentration
NH4$NH4.N = as.numeric(NH4$value) * 14 / 18

# Split by date
NH4 = lapply(unique(NH4$Date), function(i){
   # Pick those dates that match
   x = NH4[NH4$Date==i,]
   # Convert to date
   x$Date = as.Date(x$Date, "%d/%m/%y")
   # Subtract blank from results
   x$NH4.N[-48] = x$NH4.N[-48] - x$NH4.N[48]
   # Make <0 values 0
   x$NH4.N[x$NH4.N < 0] = 0
   x
})
NH4 = do.call("rbind", NH4)

# Label sites
NH4$site = substr(NH4$variable, 1, 1)
NH4$site[NH4$site=="C"] = "Control"
NH4$site[NH4$site=="T"] = "Treatment"
NH4$site[NH4$site=="B"] = "Both"

# Prep for db
NH4 = data.frame(date=as.character(NH4$Date), sample=NH4$variable, site=NH4$site, variable="NH4.N", vals=NH4$NH4.N, QC="", comments="")
# Write to db
dbWriteTable(conn=db, name="labdata", NH4, append=T, row.names=F)
