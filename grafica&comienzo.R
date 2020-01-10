library(ggplot2)
install.packages("ggplot2")
datosgenerales = read.csv("basefuga_general.csv", header = T, sep=',')
datosgenerales

nofuga = datosgenerales$FUGA == "NO FUGA"

mi_df <- data.frame(
"renta" =  c(datosgenerales$RENTA),
"Fuga" = datosgenerales$FUGA
 
)

sum(
  is.na(datosgenerales$NIV_EDUC)
)

datos = na.omit(datosgenerales)

#observacion de nivel educativo con renta para ver los beneficios que traen
#los clientes a la organizacion
ggplot(datos, aes(x=NIV_EDUC, y=RENTA)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=RENTA), vjust=1.6, color="white", size=3.5)+
  theme_minimal()





