# Paquetes
library(fastshap)
library(shapviz)
library(caret)

load("articulo 1 XAI/code paper/Comparation diferent models/save_datasets_comparation.Rdata")
# determine inputs/outputs
x <- 1
y <- 2
yD <- 3
data <- save_data[[3]][["0.3"]]

# metrics for model evaluation
MySummary <- function (data, lev = NULL, model = NULL) {
  
  # accuracy and kappa
  acc_kpp <- defaultSummary(data, lev, model)
  
  # AUC, sensitivity and specificity
  auc_sen_spe <- twoClassSummary(data, lev, model)
  
  # precision and recall
  pre_rec <- prSummary(data, lev, model)
  
  c(acc_kpp, auc_sen_spe, pre_rec)
  
  
} 

trControl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = MySummary,
  classProbs = TRUE,
  savePredictions = "all"
)

grid <- expand.grid(
  size  = c(1, 5, 10, 20, 30),                # nº de neuronas ocultas
  decay = c(0.1, 0.01, 0.001, 0.001)          # regularización L2
)

model <- train(
  class_efficiency ~ .,
  data = data,
  method = "nnet",
  trControl = trControl,
  preProcess = c("center","scale"),
  tuneGrid = grid,
  metric = "Accuracy",
  trace = FALSE,
  maxit = 500,
  MaxNWts = 10000
)

# SHAP
# Asegura que la clase positiva es la PRIMERA (importa para type="prob")
data$class_efficiency <- factor(data$class_efficiency,
                                levels = c("efficient","not_efficient"))

# Matriz de características (sin la respuesta)
X <- data[, setdiff(names(data), "class_efficiency"), drop = FALSE]

# Función de predicción que devuelve P(clase positiva)
f_pred <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")[, "efficient"]
}

# SHAP para todas las filas usando X como background (nsim controla precisión/tiempo)
set.seed(42)
sh <- fastshap::explain(
  object       = model,   # <- tu modelo caret ya entrenado (rf, svm, nnet, rpart, etc.)
  X            = X,       # background (muestra aleatoria si es muy grande)
  pred_wrapper = f_pred,
  newdata      = X,       # calcula SHAP para estas filas (aquí, todas)
  nsim         = 512      # sube a 2048 si quieres más precisión
)

# Importancia global = media del |SHAP| por variable
imp <- data.frame(
  feature    = colnames(sh),
  importance = colMeans(abs(sh), na.rm = TRUE)
)
imp <- imp[order(-imp$importance), ]
imp


# ========= IMPORTANCIA DE VARIABLES (Permutation Importance con iml) =========
library(iml)
library(pROC)

# 1) Asegura (por consistencia) el orden de niveles de la respuesta
data$class_efficiency <- factor(data$class_efficiency,
                                levels = c("efficient","not_efficient"))

# 2) Matriz de características
X <- data[, setdiff(names(data), "class_efficiency"), drop = FALSE]

# 3) Wrapper de predicción: devuelve P(clase positiva)
#    (robusto al nombre de columna por si cambiaste niveles después de entrenar)
f_pred <- function(object, newdata) {
  pr <- as.data.frame(predict(object, newdata = newdata, type = "prob"))
  pos_col <- if ("efficient" %in% names(pr)) "efficient" else names(pr)[1]
  as.numeric(pr[[pos_col]])
}

# 4) Construir el Predictor de iml
pred_obj <- Predictor$new(
  model = model, data = X, y = data$class_efficiency,
  predict.function = f_pred, type = "prob"
)

# 5) Función de pérdida basada en AUC (menor = peor)
loss_auc <- function(truth, estimate) {
  # truth puede venir como factor → lo mapeamos a 0/1 con positivo = 1
  y_bin <- if (is.factor(truth)) as.numeric(truth == levels(truth)[1]) else as.numeric(truth)
  if (length(unique(y_bin)) < 2) return(NA_real_)  # guardia por si algún bloque queda con 1 sola clase
  1 - as.numeric(pROC::auc(response = y_bin, predictor = estimate))
}

# 6) Permutation Importance (repite permutaciones para estabilidad)
set.seed(42)
fi <- FeatureImp$new(
  predictor = pred_obj,
  loss = loss_auc,
  compare = "difference",      # caída de rendimiento vs. modelo completo
  n.repetitions = 30           # sube si quieres más estabilidad
)

# 7) Tabla de importancias y normalización (mismo formato que usabas con SHAP)
imp <- fi$results[, c("feature", "importance")]
imp <- imp[order(-imp$importance), , drop = FALSE]
imp$importance_norm <- imp$importance / sum(imp$importance, na.rm = TRUE)

imp

