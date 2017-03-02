# ----------------------------------------------
# ----------------------------------------------
# Reading raw Griffin data to a db (function)
# ----------------------------------------------
# ----------------------------------------------

# Function for:
# field data
# lab data

# ----------------------------------------------
# Set up
# ----------------------------------------------

#install.packages("gdata")
#install.packages("RSQLite")
#install.packages("reshape2")
library(gdata)
library(RSQLite)
library(reshape2)


# ----------------------------------------------
# Field data function
# ----------------------------------------------

field.data = function(file){
   # Progress
   print(file)
   # Read file
   field = lapply(1:5, function(i){
      read.xls(file, sheet=i, skip=1, stringsAsFactors=F)
   })

   # General
   # ----------------------------------------------
   # How many days since last sampling?
   days = as.numeric(as.Date(field[[1]][1,2]) - as.Date(field[[1]][2,2]))
   # Clean QC column
   for(i in c(2:5)){
      field[[i]]$QC[is.na(field[[i]]$QC)] = F
   }
   # Clean overflowing columns
   for(i in c(2, 4, 5)){
      field[[i]]$Overflowing[is.na(field[[i]]$Overflowing)] = F
   }
   # Define sites
   for(i in 3:5){
      field[[i]]$site = substr(field[[i]]$Sample, 1, 1)
      field[[i]]$site[field[[i]]$site=="C"] = "Control"
      field[[i]]$site[field[[i]]$site=="T"] = "Treatment"
      field[[i]]$site[field[[i]]$site=="B"] = "Both"
      field[[i]]$site[field[[i]]$site=="S"] = "Both"
   }
   
   # Precipitation
   # ----------------------------------------------
   # In case there are blank rows
   precip = field[[2]][1:3, ]
   # Define funnel size
   funnel = pi * 7.5^2
   # Convert mass to depth/day
   # add this to try2solve binary operator error
   precip$Bottle.and.sample.g = as.numeric(precip$Bottle.and.sample.g)
   precip$Bottle.empty.g = as.numeric(precip$Bottle.empty.g)
  
   depth = (((precip$Bottle.and.sample.g - precip$Bottle.empty.g) / 1 - c(4.225, 4.225, 4.225*2)) / funnel / days) * 10
   # Prep for db
   precip = data.frame(date=field[[1]][1,2], time=precip$Time, sample=precip$Sample, site="Both", variable="precip depth", vals=depth, overflowing=precip$Overflowing, QC=precip$QC, comments=precip$Comments)
   # Write to db
   dbWriteTable(conn=db, name="fielddata", precip, append=T, row.names=F)
      
   # Stream water
   # ----------------------------------------------
   # Remove gauging data
   stream = field[[3]][c(1, 5, 9, 13), c(1:4, 7:9)]
   
   # v notch lookup
   # p161 from Discharge measurement structures, Bos, 1989
   v.notch = read.csv("repo/Vnotch.csv")
   # Convert obs to metres
   y = stream[,c(1,3)]
   y$Vnotch.cm = y$Vnotch.cm/100
   # Look up Q
   y = merge(y, v.notch, by.x="Vnotch.cm", by.y="h")
   # Convert to l/s
   y$Q = y$Q * 1000
   
   # Gauging data
   x = field[[3]][,c(1,5:6)]
   # Flow
   x$flow = x[,2] / x[,3]
   # Average flow
   x = data.frame(Sample=unique(x$Sample), mean.flow=unname(tapply(x$flow, x$Sample, mean)))
   # Combine
   stream = merge(stream[,c(1:2, 4:7)], x, by="Sample")
   stream = merge(stream, y, by="Sample")
   # Variable names
   names(stream)[c(3, 7, 9)] = c("stageboard cm", "gauged flow", "v-notch flow")
   # Wide to long format
   stream = melt(stream[,c(1:7, 9)], id.vars=c("Sample", "Time", "QC", "site", "Comments"))
   # Prep for db
   stream = data.frame(date=field[[1]][1,2], time=stream$Time, sample=stream$Sample, site=stream$site, variable=stream$variable, vals=stream$value, overflowing=NA, QC=stream$QC, comments=stream$Comments)
   # Write to db
   dbWriteTable(conn=db, name="fielddata", stream, append=T, row.names=F)
   
   # Stem flow
   # ----------------------------------------------
   # Define which collectors are large/small barrels
   # Checked on 2014-05-21
   small = c(1:9, 11, 13:17)
   large = c(10, 12, 18:22)
   # Trim potential blank rows
   stem = field[[4]][1:22, ]
   # Calculate volume
   stem$vol[large] = (stem$Depth.cm[large] - 0.8857) / 0.523
   stem$vol[small] = (stem$Depth.cm[small] - 0.6078) / 1.3756
   # Prep for db
   stem = data.frame(date=field[[1]][1,2], time=stem$Time, sample=stem$Sample, site=stem$site, variable="stem vol", vals=stem$vol, overflowing=stem$Overflowing, QC=stem$QC, comments=stem$Comments)
   # Write to db
   dbWriteTable(conn=db, name="fielddata", stem, append=T, row.names=F)
   
   # Through fall
   # ----------------------------------------------
   # Trim potential blank rows
   through = field[[5]][1:18, ]
   # Barrel sizes
   # Checked on 2014-05-21
   square = c(1, 8, 9, 12:15)
   circle = c(2:7, 10, 11, 16:18)
   # Volume
   through$vol[circle] = (through$Depth.cm[circle] - 0.8857) / 0.523
   through$vol[square] = (through$Depth.cm[square] - 0.9357) / 0.791
   # Depth
   gutter = c(16, 16, 16, 15, 14, 14, 15, 17, 16, 13, 17, 12, 15, 14, 14, 15, 17, 16)
   gutter = cos(gutter * pi / 180)
   colander = pi * (12.25 / 100)^2
   through$depth = (through$vol * 10^6) / (4.02 * 0.234 * 2 * gutter + colander) / 10^6
   # Prep for db
   through = data.frame(date=field[[1]][1,2], time=through$Time, sample=through$Sample, site=through$site, variable=rep(c("through depth", "through vol"), each=nrow(through)), vals=c(through$depth, through$vol), overflowing=through$Overflowing, QC=through$QC, comments=through$Comments)
   # Write to db
   dbWriteTable(conn=db, name="fielddata", through, append=T, row.names=F)
}


# ----------------------------------------------
# lab data function
# ----------------------------------------------

lab.data = function(file){
   # Progress
   print(file)
   # Read file
   lab = lapply(c(1, 6, 7, 8, 9), function(i){
      read.xls(file, sheet=i, skip=1, stringsAsFactors=F)
   })
   
   # General
   # ----------------------------------------------
   # How many days since last sampling?
   days = as.numeric(as.Date(lab[[1]][1,2]) - as.Date(lab[[1]][2,2]))
   # Clean QC column
   for(i in c(4)){
      lab[[i]]$QC[is.na(lab[[i]]$QC)] = F
   }
   # Define sites
   for(i in 2:5){
      lab[[i]]$site = substr(lab[[i]]$Sample, 1, 1)
      lab[[i]]$site[lab[[i]]$site=="C"] = "Control"
      lab[[i]]$site[lab[[i]]$site=="T"] = "Treatment"
      lab[[i]]$site[lab[[i]]$site=="B"] = "Both"
      lab[[i]]$site[lab[[i]]$site=="S"] = "Both"
   }
   
   # Litter fall
   # ----------------------------------------------
   # Trim potential blank rows
   litter = lab[[2]][1:18, ]
   
   # The litter section may need correcting if extra blank columns are added accidentally into the raw xls sheet...
   
   # Calculate litter weight
   
   ## Check does ncol need -1?
   
   dry = litter[,ncol(litter)-1] - litter$Tray.g
   
   # Prep for db
   litter = data.frame(date=lab[[1]][1,2], sample=litter$Sample, site=litter$site, variable="litter mass", vals=dry, QC=NA, comments=litter$Comments)
   # Write to db
   dbWriteTable(conn=db, name="labdata", litter, append=T, row.names=F)
   
   # DOC acidification
   # ----------------------------------------------
   # trim potential blank rows
   acid = lab[[3]][1:49, ]
   
   # Prep for db
   acid = data.frame(date=lab[[1]][1,2], sample=acid$Sample, site=acid$site, variable="acidity", vals=acid$Start.pH, QC=NA, comments="")
   # Write to db
   dbWriteTable(conn=db, name="labdata", acid, append=T, row.names=F)
   
   # POC
   # ----------------------------------------------
   # Trim potential blank rows
   POC = lab[[4]][1:5, ]
   
   # Calculate POC
   Van.Bemmeln = 0.58
   POC$conc = (POC$Oven.dry.mass.mg - POC$Ashed.mass.mg) * Van.Bemmeln / (POC$Sample.filtered.ml / 1000)
   # Subtract blank
   POC$conc = c(NA, POC$conc[2:5] - POC$conc[1])
   # Remove blank
   POC = POC[2:5,]
   
   # Prep for db
   POC = data.frame(date=lab[[1]][1,2], sample=POC$Sample, site=POC$site, variable="POC conc", vals=POC$conc, QC=POC$QC, comments=NA)
   # Write to db
   dbWriteTable(conn=db, name="labdata", POC, append=T, row.names=F)
   
   # Nitrate
   # ----------------------------------------------
   # Trim potential blank rows
   Nite = lab[[5]][1:49, 2:5]
   
   # NO3
   Nite$NO3.N = Nite$NO3.concentration.mg.L * 14 / 62
   # Get mean blank value
   x = mean(Nite$NO3.N[1:2])
   # Subtract blanks from results
   Nite$NO3.N[3:49] = Nite$NO3.N[3:49] - x
   # Make <0 values 0
   Nite$NO3.N[3:49][Nite$NO3.N[3:49] < 0] = 0
   
   # NH4
   Nite$NH4.N = Nite$NO4.concentration.mg.L * 14 / 18
   # Get mean blank value
   x = mean(Nite$NH4.N[1:2])
   # Subtract blanks from results
   Nite$NH4.N[3:49] = Nite$NH4.N[3:49] - x
   # Make <0 values 0
   Nite$NH4.N[3:49][Nite$NH4.N[3:49] < 0] = 0
   
   # Wide to long
   names(Nite)[c(2, 3)] = c("NO3.N", "NH4.N")
   Nite = melt(Nite, id.vars=c("Sample", "site"))
   
   # Prep for db
   Nite = data.frame(date=lab[[1]][1,2], sample=Nite$Sample, site=Nite$site, variable=Nite$variable, vals=Nite$value, QC=NA, comments=NA)
   # Write to db
   dbWriteTable(conn=db, name="labdata", Nite, append=T, row.names=F)
}

