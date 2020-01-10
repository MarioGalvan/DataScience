"------------------------------------------------------------------------------------------------------------------------------
                                                         PUNTO 1
------------------------------------------------------------------------------------------------------------------------------"
#IMPORTAMOS LOS DATOS
"en este apartado colocas la ruta donde tienes guardado el BASEFUGA_GENERAL.csv
header=T quiere decir que se incluirán los nombres de las columnas.
na=NA hace referencia a los valores faltantes que tiene el dataset. Como en la hoja de excel hay campos que dícen NA, 
este será el identificador para cada uno de ellos.

NA= Valores faltantes

a la variable general le asignamos el dataset"

#CON ESTA OPCIÓN EVITAMOS NOTACIONES CIENTIFICAS EN LAS VARIABLES CUANTITATIVAS
options(scipen=999) 
general<-read.csv("D:/Trabajos/Modelo_fugas/BASEFUGA_GENERAL.csv", header = T, na="NA", sep=";")

"Esta función cuenta los valores NA que hay en cada columna"
sapply(general, function(x) sum(is.na(x)))


#TÉCNICAS DE IMPUTACIÓN PARA SUSTITUIR LOS NA

"Para la variable GENERO, NIV_EDUC, E_CIVIL, CIUDAD se utilizó la moda

este comando nos permite obtener la frecuencia de los posibles valores que se encuentran en la variable especificada"
table(general$GENERO)
table(general$NIV_EDUC)
table(general$E_CIVIL)
table(general$CIUDAD)

"Para la variable EDAD Y COD_COM se utilizó la media hallada obviando los valores faltantes

na.rm hace referencia a la existencia de valores faltantes, por lo que es necesario colocarle TRUE para que los omita
"
mean(general$EDAD,na.rm = TRUE)
mean(general$COD_COM,na.rm = TRUE)

#REEMPLAZAMOS LOS VALORES HALLADOS

"Primero las variables cualitativas o categóricas"
general$GENERO[is.na(general$GENERO)] <- "M"
general$NIV_EDUC[is.na(general$NIV_EDUC)] <- "UNV"
general$E_CIVIL[is.na(general$E_CIVIL)] <- "CAS"
general$CIUDAD[is.na(general$CIUDAD)] <- "SANTIAGO"

"Segundo las variables cuantitativas o numéricas"
general$EDAD[is.na(general$EDAD)] <- 44
general$COD_COM[is.na(general$COD_COM)] <- 108

#VERIFICAMOS SI LOS VALORES NA FUERON MODIFICADOS CON LA FUNCIÓN SAPPLY
sapply(general, function(x) sum(is.na(x)))

#POR OTRA PARTE, SE ENCONTRARON CARACTERES ESPECIALES EN LA VARIABLE EDAD, YA QUE EL VALOR MÍNIMO ESTÁ DADO POR UN VALOR NEGATIVO
#Summary: muestra un resumen estadistico de la varible EDAD
summary(general$EDAD)

#POR LO TANTO SE UTILIZÓ LA FUNCIÓN gsub PARA ELIMINAR EL CARACTER "-"
general$EDAD<-gsub("-","", general$EDAD)

#PERO COMO ESTA FUNCIÓN SE TRABAJA PARA CADENAS O VARIABLES CUALITATIVAS, SE REALIZÓ UNA CONVERSIÓN DE CADENA A INTEGER
general$EDAD<-as.integer(general$EDAD)

#MIRAMOS SI LA CONVERSIÓN SE REALIZÓ CON ÉXITO
class(general$EDAD)

#HUBO OTRO PROBLEMA, YA QUE SE ENCONTRARON VALORES EN NOTACIÓN CIENTIFICA REVUELTOS CON VALORES EN NOTACIÓN DECIMAL EN LA VARIABLE MONTO
#POR LA CUAL TOCÓ EJECUTAR EL COMANDO options(scipen=999) ANTES DE IMPORTAR EL DATASET PARA DESACTIVAR DICHA NOTACIÓN
#COMO SE TRATA DE UNA VARIABLE DECIMAL, SE REALIZÓ LA CONVERSIÓN
general$MONTO<-as.numeric(general$MONTO)

"------------------------------------------------------------------------------------------------------------------------------
                                                             PUNTO 2
------------------------------------------------------------------------------------------------------------------------------"
"análisis descriptivo de los datos: 

1.tipos de variables estadisticas
2.medidas de posición central
3.medidas de posición no central
4.Medidas de dispersión
5.Tabla de frecuencias
6.Gráficos"

"1.variables estadisticas"

#NÚMERO DE VARIABLES CON EL COMANDO NCOL
ncol(general)

"2.medida de posición central"

#PARA ESTO, NECESITAMOS INSTALAR Y LLAMAR LA LIBRERÍA PARA UTILIZAR LA FUNCIÓN SKIMR 
#ESTA NOS ARROJA LAS MEDIDAS DE POSICIÓN CENTRAL, NO CENTRAL Y MEDIDAS DE DISPERSIÓN
install.packages("skimr") "instalas 1 sola vez"
library(skimr)

skim(general)
"Este comando divide las medidas por tipo de variable (Variable type), en nuestro caso tenemos 2 tipos de variables:

                                      factor=cualitativa o categórica
                                      numeric=cuantitativa o numérica

-PARA Variable type: factor

skim_variable: variables cualitativas
n_missing: valores faltantes
ordered: si el conjunto está ordenado
n_unique: los posibles casos que tiene una variable
top_count: es igual a la moda para cada variable

-PARA Variable type: numeric

n_missing: valores faltantes
skim_variable: variables cuantitativas
mean: media
sd: desviación típica o estándar
p0: valor mínimo
p25: primer cuartil
p50: segundo cuartil
p75: tercer cuartil
p100: valor máximo
hist: es un histograma con respecto a la distribución de los datos
"
#RANGO INTERCUATILICO POR CADA VARIABLE
IQR(general$ID)
IQR(general$RENTA)
IQR(general$EDAD)
IQR(general$COD_OFI)
IQR(general$COD_COM)
IQR(general$D_Marzo)
IQR(general$D_Abril)
IQR(general$D_Mayo)
IQR(general$D_Junio)
IQR(general$D_Julio)
IQR(general$D_Agosto)
IQR(general$D_Septiembre)
IQR(general$M_MOROSO)
IQR(general$MONTO)


#VARIANZA PARA CADA VARIABLE
var(general$ID)
var(general$RENTA)
var(general$EDAD)
var(general$COD_OFI)
var(general$COD_COM)
var(general$D_Marzo)
var(general$D_Abril)
var(general$D_Mayo)
var(general$D_Junio)
var(general$D_Julio)
var(general$D_Agosto)
var(general$D_Septiembre)
var(general$M_MOROSO)
var(general$MONTO)

"5.Tabla de frecuencias para las variables CUANTITATIVAS EDAD y MONTO"

#TABLA DE FRECUENCUA PARA LA VARIABLE EDAD
"utilizamos esta función que nos arroja la tabla de frecuencia.

Donde x=la variable, k=numero de intervalos, A=amplitud, p=precisión de los datos, p=1 porque la precisión son unidades."
TablaFrecs = function(x,k,A,p){ 
  L = min(x)-p/2+A*(0:k)
  x_cut = cut(x, breaks = L, right=FALSE)
  intervals = levels(x_cut)
  mc = (L[1]+L[2])/2+A*(0:(k-1))
  Fr.abs = as.vector(table(x_cut)) 
  Fr.rel = round(Fr.abs/length(x),4) 
  Fr.cum.abs = cumsum(Fr.abs) 
  Fr.cum.rel = cumsum(Fr.rel)
  tabla = data.frame(intervals, mc, Fr.abs, Fr.cum.abs, Fr.rel, Fr.cum.rel)
  tabla
}

#NÚMERO DE INTERVALOS MEDIANTE LA REGLA DE STURGES
nclass.Sturges(general$EDAD)

#AMPLITUD
A = diff(range(general$EDAD)) / 13
A

#REDONDEAMOS LA AMPLITUD
A=8.4

#EJECUTAMOS LA FUNCIÓN
TablaFrecs(general$EDAD, k = 13, A, p = 0.1)

#INTERVALOS DE CLASE OBTENIDOS DE LA TABLA DE FRECUENCIAS
L = c( 20.9,  29.4,  37.8,  46.2,  54.5,  63,  71.4,  79.8,  88.2,  96.6, 105, 113, 122, 130)

#ESTO ES UN SIMPLE PAQUETE DE PALETAS DE COLORES
install.packages("RColorBrewer") "solo lo instalas 1 sola vez"

library("RColorBrewer")

"cada vez que vayas a graficar, ejecutas primero esto"
display.brewer.all() 

#FUNCIÓN PARA OBTENER EL HISTOGRAMA DE FRECUENCIAS ABSOLUTAS
histAbs = function(x,L) {
  h = hist(x, breaks = L, right = FALSE, freq = FALSE,
           xaxt = "n", yaxt = "n", col = brewer.pal(n = 13, name = "Set1"), 
           main = "Histograma de frecuencias absolutas", 
           xlab = "Intervalos y marcas de clase",ylab = "Frecuencias absolutas")
  axis(1, at=L)
  text(h$mids, h$density/2, labels=h$counts, col="black") 
}

#HISTOGRAMA DE FRECUENCIAS ABSOLUTAS
histAbs(general$EDAD,L)

#FUNCIÓN PARA OBTENER EL HISTOGRAMA DE FRECUENCIAS RELATIVAS
histRel = function(x,L) {
  h = hist(x, breaks=L, right=FALSE , plot=FALSE)
  t = round(1.1*max(max(density(x)[[2]]),h$density),2) 
  plot(h, freq = FALSE, col = brewer.pal(n = 13, name = "Set1"), 
       main = "Histograma de frec. relativas\ny curva de densidad estimada", 
       xaxt="n", ylim=c(0,t), xlab="Intervalos", ylab="Densidades")
  axis(1, at = L) 
  text(h$mids, h$density/2, labels = round(h$counts/length(x),2), col = "black")
  lines(density(x), col = "black", lwd = 2) 
}

#HISTOGRAMA DE FRECUENCIAS RELATIVAS
histRel(general$EDAD,L)

#TABLA DE FRECUENCUA PARA LA VARIABLE MONTO

#NÚMERO DE INTERVALOS MEDIANTE LA REGLA DE STURGES
nclass.Sturges(general$MONTO)

#AMPLITUD
A = diff(range(general$MONTO)) / 13
A

#REDONDEAMOS LA AMPLITUD
A=576923


#EJECUTAMOS LA FUNCIÓN
TablaFrecs(general$MONTO, k = 14, A, p = 1)

#INTERVALOS DE CLASE OBTENIDOS DE LA TABLA DE FRECUENCIAS
"L = c( 5e+05, 1.08e+06, 1.65e+06, 2.23e+06,2.81e+06, 3.38e+06, 3.96e+06, 4.54e+06, 5.12e+06, 5.69e+06, 6.27e+06, 6.85e+06, 7.42e+06, 8e+06,8.58e+06)"
L=c(500000, 1080000, 1650000, 2230000, 2810000, 3380000, 3960000, 4540000, 5120000, 5690000, 6270000, 6850000, 7420000, 8000000, 8580000)

"cada vez que vayas a graficar, ejecutas primero esto"
display.brewer.all() 

#FUNCIÓN PARA OBTENER EL HISTOGRAMA DE FRECUENCIAS ABSOLUTAS (ES LA MISMA SOLO CAMBIAN LOS COLORES)
histAbs = function(x,L) {
  h = hist(x, breaks = L, right = FALSE, freq = FALSE,
           xaxt = "n", yaxt = "n", col = brewer.pal(n = 13, name = "Paired"), 
           main = "Histograma de frecuencias absolutas", 
           xlab = "Intervalos y marcas de clase",ylab = "Frecuencias absolutas")
  axis(1, at=L)
  text(h$mids, h$density/2, labels=h$counts, col="black") 
}

#HISTOGRAMADE FRECUENCIAS ABSOLUTAS
histAbs(general$MONTO,L)

#FUNCIÓN PARA OBTENER EL HISTOGRAMA DE FRECUENCIAS RELATIVAS (ES LA MISMA SOLO QUE LE CAMBIO EL NÚMERO DE COLORES DE 13 A 14)
histRel = function(x,L) {
  h = hist(general$MONTO, breaks=L, right=FALSE , plot=FALSE)
  t = round(1.1*max(max(density(general$MONTO)[[2]]),h$density),10) 
  plot(h, freq = FALSE, col = brewer.pal(n = 13, name = "Paired"), 
       main = "Histograma de frec. relativas\ny curva de densidad estimada", 
       xaxt="n", ylim=c(0,t), xlab="Intervalos", ylab="Densidades")
  axis(1, at = L) 
  text(h$mids, h$density/2, labels = round(h$counts/length(x),2), col = "black")
  lines(density(x), col = "black", lwd = 2) 
}

#HISTOGRAMA DE FRECUENCIAS RELATIVAS
histRel(general$MONTO,L)

"5.Tabla de frecuencias para las variables CUALITATIVAS GENERO, NIVEL_EDUC, E_CIVIL Y FUGA"

#INSTALAS UNA SOLA VEZ
install.packages("descr") #para tablas de frecuencias 
install.packages("tidyverse") #para graficos

library(descr)
library(tidyverse)

#TABLA DE FRECUENCIAS SIMPLES Y RELATIVAS PARA LA VARIABLES. Donde Frequency=frecuencia simple y Percent=frecuencia relativa, cum percent=relativa acumulada 

"Variable GENERO"
freq(ordered(general$GENERO), plot = FALSE)

"Variable NIVEL_EDUC"
freq(ordered(general$NIV_EDUC), plot = FALSE)


"Variable E_CIVIL"
freq(ordered(general$E_CIVIL), plot = FALSE)


#TABLA DE FRECUENCIAS CRUZADAS SIMPLES Y RELATIVAS ENTRE EL NIVEL_EDUC y EL GENERO
crosstab(general$NIV_EDUC, general$GENERO, prop.c = TRUE, plot = FALSE)

#DIAGRAMA DE BARRAS PARA EL NIVEL_EDUC Y GENERO
legend_title <- "Género"
ggplot(data = general) +
  geom_bar(mapping = aes(x = general$NIV_EDUC, fill = general$GENERO),position = "dodge", color="black")+
  scale_fill_manual(legend_title,values=c("#8b2506", "#f0c189"))+theme_minimal()+theme(legend.position="top")+
  annotate("text", x=0.8, y=50, label= "6 (0.7%)")+
  annotate("text", x=1.25, y=40, label= "5 (0.4%)")+
  annotate("text", x=1.8, y=50, label= "8 (0.9%)")+
  annotate("text", x=2.25, y=40, label= "4 (0.3%)")+
  annotate("text", x=2.8, y=300, label= "272 (30.3%)")+
  annotate("text", x=3.25, y=370, label= "343 (24.6%)")+
  annotate("text", x=3.8, y=460, label= "432 (48.1%)")+
  annotate("text", x=4.25, y=290, label= "266 (19.1%)")+
  annotate("text", x=4.8, y=210, label= "180 (20.0%)")+
  annotate("text", x=5.25, y=805, label= "778 (55.7%)")+
  xlab("Nivel educativo") + ylab("Conteo") +
  ggtitle("Relación Nivel educativo/género") +
  theme(plot.title = element_text(hjust = 0.5))
  

#TABLA DE FRECUENCIAS CRUZADAS SIMPLES Y RELATIVAS ENTRE EL E_CIVIL y EL GENERO
crosstab(general$E_CIVIL, general$GENERO, prop.c = TRUE, plot = FALSE)

#DIAGRAMA DE BARRAS PARA EL E_CIVIL Y GENERO
legend_title <- "Género"
ggplot(data = general) +
  geom_bar(mapping = aes(x = general$E_CIVIL, fill = general$GENERO),position = "dodge", color="black")+
  scale_fill_manual(legend_title,values=c("#8b2506", "#f0c189"))+theme_minimal()+theme(legend.position="top")+
  annotate("text", x=0.8, y=440, label= "404 (45.0%)")+
  annotate("text", x=1.25, y=1080, label= "1036 (74.2%)")+
  annotate("text", x=1.8, y=90, label= "48 (5.3%)")+
  annotate("text", x=2.25, y=110, label= "66 (4.7%)")+
  annotate("text", x=2.8, y=410, label= "366 (40.8%)")+
  annotate("text", x=3.25, y=320, label= "276 (19.8%)")+
  annotate("text", x=3.8, y=120, label= "80 (8.9%)")+
  annotate("text", x=4.25, y=65, label= "18 (1.3%)")+
  xlab("Estado civil") + ylab("Conteo") +
  ggtitle("Relación Estado civil/género") +
  theme(plot.title = element_text(hjust = 0.5))

#TABLA DE FRECUENCIAS CRUZADAS SIMPLES Y RELATIVAS ENTRE EL E_CIVIL Y EL NIVEL_EDUC
crosstab(general$NIV_EDUC, general$E_CIVIL, prop.c = TRUE, plot = FALSE)

#TABLA DE FRECUENCIAS CRUZADAS SIMPLES Y RELATIVAS PARA EL GENERO Y FUGA
crosstab(general$GENERO, general$FUGA, prop.c = TRUE, plot = FALSE)

#TABLA DE FRECUENCIAS CRUZADAS SIMPLESY RELATIVAS PARA EL E_CIVIL Y FUGA
crosstab(general$E_CIVIL, general$FUGA, prop.c = TRUE, plot = FALSE)


