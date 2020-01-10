#redes neuronales

datos = read.csv("pruebaND2.CSV", header = T, sep = ';')
#Haciendo uso de la función str,
#podemos explorar la estructura del dataframe que contiene el conjunto de datos
str(datos)


#A partir de la tabla anterior, también podemos ver que,
#a excepción de la variable FUGA,
#todos los demás registros son numéricos.
#Sin embargo, ya que se trata de un problema de clasificación
#es conveniente que nuestra variable a predecir, 
#es decir la variable dependiente FUGA,
#sea un Factor o variable categórica.
#en nuestro caso ya es un factor


summary(datos)

# la aplicación de las redes neuronales artificiales exige 
#que las variables de entrada estén todas rescaladas en el mismo rango, 
#por lo que haremos uso de la función scale para cumplir con esta condición

datos[, c(2:6)] <- scale(datos[, c(2:6)])
summary(datos)


#Por último, vamos a crear un conjunto de entrenamiento (40%) 
#y un conjunto de validación (60%) a partir de nuestro dataset original

install.packages("caTools")
install.packages("h2o")

library(caTools)
set.seed(100) #asignamos una semilla para el entrenamiento
split <- sample.split(datos$FUGA, SplitRatio = 0.40)
training_set <- subset(datos, split == TRUE)
test_set <- subset(datos, split == FALSE)


#A fin de asegurarnos de que la proporción de estrellas pulsares y no pulsares
#es aproximadamente la misma en ambos conjuntos de datos

table(training_set$FUGA) #para la tabla del entrenamiento
table(test_set$FUGA)  #para la table del test o evaluacion


#La implementación de la red neuronal la haremos haciendo uso del paquete H2O

library(h2o)
h2o.init(nthreads = -1)

#AHORA CREAMOS nuestro objeto clasificador 


classifier = h2o.deeplearning(y = 'FUGA',
                              training_frame = as.h2o(training_set),
                              activation = 'Rectifier',
                              hidden = c(5, 5),
                              epochs = 100,
                              train_samples_per_iteration = -2)


#realizamos la prediccion


prob_pred <- h2o.predict(classifier, newdata = as.h2o(test_set))

#REALIZAMOS LA MATRIZ DE CONFUSION
y_pred <- as.vector(ifelse(prob_pred$predict == 'NO FUGA', 0, 1))
y_test_set <- ifelse(test_set$FUGA == 'FUGA ', 0, 1)

cm <- table(y_test_set, y_pred)
cm
#En este caso, tenemos un total de 1400 predicciones correctas, 
#lo que  nos genera una precisión en la predicción del 70%




