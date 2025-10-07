# prueba NN

# Cargamos la libreria keras
library(keras)
library(tidyverse)

###############################
## Preprocesamiento de datos ##    
###############################

# Cargar los datos
data <- read.csv("log2.csv", header = TRUE)

# Detectar todos los NA en el conjunto de datos
apply(data, 2, function(x) length(which(is.na(x))))

# Todas las columnas son numericas, menos Action que corresponde a la etiqueta
str(data)
summary(data)
unique(data$Action) # Valores que puede tomar: 4 "allow"      "drop"       "deny"       "reset-both"
data$Action <- as.factor(data$Action)
data %<>% mutate_if(is.factor, as.numeric)

# Separamos las variables de entrada y la etiqueta
y <- select(data, Action)
x <- select(data, -Action)

y # La base de datos esta ordenada

# Al convertir la columna de la etiqueta en numerica, los valores originales 
# se transforman en 1 (M) y 2 (R) --> restamos 1 para obtener 0 y 1
y <- y - 1 # para keras siempre se tiene que empezar en 0
y

# Vemos que las clases aparecen ordenadas en el dataset. Vamos a cambiarlo
set.seed(123)
randomsample <- sample(nrow(x))
randomsample
x <- x[randomsample, ]
y <- y[randomsample,]
y <- as.numeric(y)

# Realizamos una particion en train/test del 80%/20% de los datos
set.seed(123) #  asegurar que podamos reproducir los mismos datos cada vez que se ejecuta el codigo
index <- sample(2, nrow(x), replace = TRUE, prob = c(0.8,0.2))

# Training
x_train <- x[index == 1, ]
y_train <- y[index == 1]

# Test
x_test <- x[index == 2, ]
y_test <- y[index == 2]

# Comprobamos si todas las clases estan representadas proporcionalmente 
# tanto en el conjunto de entrenamiento como en el conjunto de prueba
prop.table(table(data[,5]))
prop.table(table(y_train))
prop.table(table(y_test))


# Normalizacion de datos
# valorNormalizado = (valor-media)/desviacionEstandar

m <- apply(x_train, 2, mean)  # 1 indica filas, 2 indica columnas 
s <- apply(x_train, 2, sd) 
x_train <- scale(x_train, center = m, scale = s)
x_test <- scale(x_test, center = m, scale = s)

# Pasamos los datos de dataframe a matriz
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# Codificacion ONE-HOT de la etiqueta con la funcion to_categorical()
# El vector de etiquetas a convertir debe ser un vector de enteros comenzando en 0
head(y_train)
head(y_test)

# guardamos la codificacion 0/1 para poder obtener posteriormente la matriz de confusion
y_test01 <- y_test 

y_train <- to_categorical(y_train, 4) #4 categorias
y_test <- to_categorical(y_test, 4)

head(y_train)
head(y_test)


#############################
## Red neuronal multicapa  ## 
#############################

model1 <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "relu", input_shape = ncol(x_train)) %>%
  layer_dense(units = 8, activation = "relu") %>% 
  layer_dense(units = 4, activation = "softmax") # capa de salida

# Compilacion

model1 %>% compile(loss = "binary_crossentropy",
                   optimizer = optimizer_sgd(learning_rate = 0.01, # 0.01 es el valor por defecto
                                             momentum = 0.8),      # por defecto es 0
                   metrics = c("accuracy"))

summary(model1)

# Entrenamiento

history1 <- model1 %>% fit(x_train, y_train,
                           epochs = 30,
                           batch_size = 16,
                           validation_split = 0.2)


history1

# Verificamos si el modelo tambien funciona bien en el conjunto de prueba
metrics1 <- model1 %>% evaluate(x_test, y_test, verbose = 0)
metrics1

# keras::k_argmax() devuelve el indice del valor maximo
results1 <- model1 %>% predict(x_test) %>% k_argmax()
results1 <- as.array(results1)
head(results1)

# Podemos ver las predicciones correctas e incorrectas
tabla <- table(y_test01, results1)  
tabla

# A partir de esta tabla, conocida como matriz de contingencia o matriz de confusion,
# tambien podemos calcular el accuracy
sum(diag(tabla)) / sum(tabla)


############################
## Reguralizacion L2 ##
############################

model2 <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "relu", input_shape = ncol(x_train),
              kernel_regularizer= regularizer_l2(0.01)) %>% 
  layer_dense(units = 8, activation = "relu",
              kernel_regularizer= regularizer_l2(0.01)) %>% 
  layer_dense(units = 4, activation = "softmax")

# Compilacion

model2 %>% compile(loss = "categorical_crossentropy",
                   optimizer = optimizer_sgd(learning_rate = 0.01, 
                                             momentum = 0.8),
                   metrics = c("accuracy"))


summary(model2)

# Entrenamiento

history2 <- model2 %>% fit(x_train, y_train,
                           epochs = 30,
                           batch_size = 16,
                           validation_split = 0.2)


history2

# Verificamos si el modelo tambien funciona bien en el conjunto de prueba
metrics2 <- model2 %>% evaluate(x_test, y_test, verbose = 0)
metrics2

# Comparamos con los resultados anteriores
history1
history2

####################
## Early stopping ##
####################

model3 <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "relu", input_shape = ncol(x_train)) %>%
  layer_dense(units = 8, activation = "relu") %>% 
  layer_dense(units = 4, activation = "softmax")

# Compilacion

model3 %>% compile(loss = "categorical_crossentropy",
                   optimizer = optimizer_sgd(learning_rate = 0.01, 
                                             momentum = 0.8),
                   metrics = c("accuracy"))

summary(model3)

# Entrenamiento

# callback_early_stopping:
# "monitor" podria ser loss, accuracy, val_loss o val_accuracy 
# restore_best_weights por defecto es FALSE 
# (con FALSE utiliza los pesos obtenidos en el ultimo paso del entrenamiento, 
# no los mejores)

history3 <- model3 %>% fit(x_train, y_train,
                           epochs = 30,
                           batch_size = 16,
                           validation_split = 0.2,
                           callbacks = callback_early_stopping(patience = 10, 
                                                               monitor = 'val_accuracy', 
                                                               restore_best_weights = TRUE))


history3

# Verificamos si el modelo tambien funciona bien en el conjunto de prueba
metrics3 <- model3 %>% evaluate(x_test, y_test, verbose = 0)
metrics3

# Comparamos con los resultados anteriores
history1
history2
history3

################################
## Dropout en capa de entrada ##
################################

# Usamos tambien regularizacion max-norm, lo que garantiza que la norma de 
# los pesos no exceda un valor de, por ejemplo, 3.

model4 <- keras_model_sequential() %>%
  layer_dropout(0.2, input_shape = ncol(x_train)) %>% 
  layer_dense(units = 10, activation = "relu", 
              kernel_constraint = constraint_maxnorm(3)) %>%
  layer_dense(units = 8, activation = "relu", 
              kernel_constraint = constraint_maxnorm(3)) %>% # si se utiliza drop out, maxnorm
  layer_dense(units = 4, activation = "softmax")

# Compilacion

# Pese a que la recomendacion al usar dropout es aumentar la tasa de aprendizaje 
# y el momentum, puede comprobarse que para este problema no funciona demasiado bien hacerlo
model4 %>% compile(loss = "categorical_crossentropy",
                   optimizer = optimizer_sgd(learning_rate = 0.05, 
                                             momentum = 0.9),
                   metrics = c("accuracy"))


summary(model4)

# Entrenamiento

history4 <- model4 %>% fit(x_train, y_train,
                           epochs = 30,
                           batch_size = 16,
                           validation_split = 0.2)

history4

# Verificamos si el modelo tambien funciona bien en el conjunto de prueba
metrics4 <- model4 %>% evaluate(x_test, y_test, verbose = 0)
metrics4

# Comparamos con los resultados anteriores
history1
history2
history3
history4

##############################
## Dropout en capas ocultas ##
##############################

model5 <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "relu", input_shape = ncol(x_train),
              kernel_constraint = constraint_maxnorm(3)) %>%
  layer_dropout(0.2) %>% # desconecta el 20% de las conexiones de la de arriba entre 0.2 y 0.5
  layer_dense(units = 8, activation = "relu",
              kernel_constraint = constraint_maxnorm(3)) %>% 
  layer_dropout(0.2) %>% 
  layer_dense(units = 4, activation = "softmax")

# Compilacion

model5 %>% compile(loss = "categorical_crossentropy",
                   optimizer = optimizer_sgd(learning_rate = 0.01, 
                                             momentum = 0.8),
                   metrics = c("accuracy"))


summary(model5)

# Entrenamiento

history5 <- model5 %>% fit(x_train, y_train,
                           epochs = 30,
                           batch_size = 16,
                           validation_split = 0.2)
history5

# Verificamos si el modelo tambien funciona bien en el conjunto de prueba
metrics5 <- model5 %>% evaluate(x_test, y_test, verbose = 0)
metrics5

# Comparamos con los resultados anteriores
history4
history3
history2
history1
history5

model3 %>% save_model_hdf5("./GONZALEZMOYANORICARDO.h5")
list.files("./")

#################
## CONCLUSION ##   
#################

# Tras probar lo realizado en clase, el modelo 1 es el que mejor acierto tiene en el conjunto de validacion.
# Desde el principio los accuracy estaban eran casi iguales.
# Sin embargo, he decidido quedarme con el modelo 3 ya que ofrece un resultado practicamente igual mucho antes.
# Al introducir droptout el accuracy en los datos de validacion varia, de una etapa a otra puede subir o bajar. En el caso del modelo 5 en la etapa 24 superó al accuracy de train (valor 97%) y en la siguiente bajo a 76, mientras que el accuracy de trian se mantania estable.
# Esto es lo que no pasaba con los modelos anterioes.
# He probado a poner más neuronas y menos épocas, pero he decicido dejarlo en 30 epocas para ver si convergia en los ultimos y 10 y 8 neuronas por capa porque poner más no cambiaba el resultado.
# Los datos son demasiados, tarda mucho el ordenador en hacer una epoca.