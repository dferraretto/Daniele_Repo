# --------------------------------------------------------
#  Daniele Ferraretto, working on WP2 data from Griffin:
#  Soil cores and 15N fate
#  started on 17th August, 2015
#  updated:           last update:
# --------------------------------------------------------
# --------------------------------------------------------
# Import Dataset
# --------------------------------------------------------
# --------------------------------------------------------
soilcores <- read.csv("M:/My PhD/WP2 -15N Soil cores/15N plots/lab table for extracting cores.csv")
View(soilcores)
head(soilcores)
names(soilcores) # column names

mean(soilcores[,"Surface..volume"],na.rm=TRUE) # trying to see if a basic operation works
# create a new variable moisture.content as ratio of two columns
moisture.content=(soilcores[ ,'Total.sample.dry.weight'])/(soilcores[ ,'Fresh.weight'])
moisture.content
###tentativi miseri ma riusciti di plottare qualche valore
plot (Code~Moisture,content)
plot (soilcores$Code,moisture.content) #to have a rough idea of the distribution of the values. There is a high moisture nosense value...
soil.spec.weight=(soilcores[ ,'Total.sample.dry.weight'])/(soilcores[ ,'Surface..volume'])
soil.spec.weight

# provo a calcolare il peso specifico per tipo di profilo con tapply:
sample.spec.weight=tapply(soilcores$layer,soilcores[ ,'Total.sample.dry.weight'])/(soilcores[ ,'Surface..volume'])
plot(sample.spec.weight) #this is not meaningful as it doesn't consider the weight of stones and roots. However, it shows two xtreme values (>>0.3, wilst the rest of the samples ~<2)
# furthermore, tapply gives me a vector but I would rather get a dataframe or at least add it to my dataframe. Or extract a label column

# miserabili tentativi di subset: subsettiamo litter
Lit=subset(soilcores, Layer.composition  == "Lit")
#filter(soilcores, layer== "-2") DC non funziona
# esempio si subset:  y[which(y[,1]>1 & y[,2]<7),]
only.layers=soilcores[which(soilcores$layer==-2 & soilcores$layer==-3)] #l'ho creato ma ha 0 columns DP
only.layers

#### Create a db with all significant columns
db=soilcores[,c(1:5,8,9,14,15)]
names(db)
# add a column (sample.spec.weight)
db$sample.spec.weight= c(sample.spec.weight)
View(db)

## ho fatto qualche operazionucola sul db, veramente cagate, domani DEVO provare qualche plot e qualche statistica. Poi vorrei provare soprattutto su quel casso di db di GRIFFIN!!!
