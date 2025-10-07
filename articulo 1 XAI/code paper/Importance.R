train_data <- final_model$final_model[["trainingData"]]
names(train_data)[1] <- "ClassEfficiency"

prueba <- model.matrix(ClassEfficiency~ . - 1, data = train_data)
prueba <- cbind(train_data[1], prueba)

predCaret <- function(M,data) {

    return (predict(M,data)) 
  
}

svm.imp <- Importance(
  M = final_model$final_model$finalModel,
  data = prueba,
  method = "1D-SA",
  PRED = predCaret,
  task = "prob",
  outindex = 1
)

imp_value <- svm.imp$imp; imp_value


bor <- predCaret(M = final_model, data = model.matrix(ClassEfficiency~ . - 1, data = train_data))

class(final_model)



data(iris)
library(MASS)

predlda = function(M,data) {
  
  return (predict(M,data)$posterior) 
}

LDA = lda(Species ~ .,iris, prior = c(1,1,1)/3)

# 4 is the column index of Species
I=Importance(LDA,iris,method="1D-SA",PRED=predlda,outindex=4)

vecplot(I,graph="VEC",xval=1,Grid=10,TC=1,
        main="1-D VEC for Sepal.Lenght (x-axis) influence in setosa (prob.)")










library(caret)
library(kernlab)

# Entrenar modelo con Caret usando kernlab
data(iris)
modelo_entrenado <- train(Species ~ ., data = iris, method = "svmRadial")

# Definir la función de predicción para pasar a rminer
pred_function <- function(modelo, newdata) {
  predict(modelo, newdata)
}

# Calcular la importancia usando rminer
library(rminer)
importancia <- Importance(modelo_entrenado, iris, PRED=pred_function, method="1D-SA")
print(importancia)
