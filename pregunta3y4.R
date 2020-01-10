
datos = read.csv("BASEFUGA_GENERAL.csv", header = T,sep = ',')



library(dummies)
library(kableExtra)

base <- read.csv("BASEFUGA.CSV")
kable(head(base),format = "markdown")


#creando un dataframe para visualizzar variables indicadoras nuevas 
base.dummy <- dummy.data.frame(base)
kable(head(base.dummy),format = "markdown")
