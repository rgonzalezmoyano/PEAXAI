################################################################################
######################## EXAMPLE OF USE 'PEAXAI' ###############################
################################################################################

# ------------------------------------------------------------------------------
# Load PEAXAI ------------------------------------------------------------------
# ------------------------------------------------------------------------------
devtools::document()
devtools::load_all()

# ------------------------------------------------------------------------------
# Load dataset -----------------------------------------------------------------
# ------------------------------------------------------------------------------
load("data/firms.RData")

# filter to valencian comunity
data <- firms[firms$autonomous_community == "Comunidad Valenciana",]

# ------------------------------------------------------------------------------
# Parameters to PEAXAI ---------------------------------------------------------
# ------------------------------------------------------------------------------
# x and y indexes
x <- c(1:4)
y <- c(5)

# Returns to scale assumption
RTS <- "vrs"

# Addressing target imbalance by specifying resampling ratios, using SMOTE units.
balance_data <- c(seq(0.20, 0.5, 0.05))

# Set seed to reproduce results
seed <- 0
set.seed(seed)

#-------------------------------------------------------------------------------
# ML models --------------------------------------------------------------------
# ------------------------------------------------------------------------------
# rf
customRF <- list(
  type    = c("Classification"),
  library = "randomForest",
  loop    = NULL
)

customRF$parameters <- data.frame(
  parameter = c("mtry","ntree"),
  class     = c("numeric","numeric"),
  label     = c("mtry","ntree")
)

customRF$grid <- function(x, y, len = 5, search = "grid") {
  p <- ncol(x)
  expand.grid(
    mtry  = unique(round(seq(1, max(1, p), length.out = len))),
    ntree = unique(round(seq(300, 1200, length.out = len)))
  )
}

customRF$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  randomForest::randomForest(
    x = x, y = y,
    mtry  = param$mtry,
    ntree = param$ntree,
    ...
  )
}

customRF$predict <- function(modelFit, newdata, submodels = NULL) {
  predict(modelFit, newdata)
}

customRF$prob <- function(modelFit, newdata, submodels = NULL) {
  as.data.frame(predict(modelFit, newdata, type = "prob"))
}

customRF$sort   <- function(x) x[order(x$mtry, x$ntree), ]
customRF$levels <- function(x) x$classes

# NN
customNNet <- list(
  type    = c("Classification"),
  library = "nnet",
  loop    = NULL
)

customNNet$parameters <- data.frame(
  parameter = c("size","decay","maxit"),
  class     = c("numeric","numeric","numeric"),
  label     = c("size","decay","maxit")
)

customNNet$grid <- function(x, y, len = 5, search = "grid") {
  expand.grid(
    size  = unique(round(seq(3, 7, length.out = len))),
    decay = 10^seq(-4, -1, length.out = len),
    maxit = unique(round(seq(150, 500, length.out = len)))
  )
}

customNNet$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  x <- as.matrix(x)
  Y <- nnet::class.ind(y)

  fit <- nnet::nnet(
    x, Y,
    size    = param$size,
    decay   = param$decay,
    maxit   = param$maxit,
    softmax = TRUE,
    trace   = FALSE,
    ...
  )
  fit$lev <- lev
  fit
}

customNNet$predict <- function(modelFit, newdata, submodels = NULL) {
  p <- predict(modelFit, newdata)
  if (!is.matrix(p)) p <- cbind(p)
  lev <- modelFit$lev
  cls <- lev[max.col(p)]
  factor(cls, levels = lev)
}

customNNet$prob <- function(modelFit, newdata, submodels = NULL) {
  p <- predict(modelFit, newdata)
  if (!is.matrix(p)) p <- cbind(p)
  colnames(p) <- modelFit$lev
  as.data.frame(p)
}

customNNet$sort   <- function(x) x[order(x$size, x$decay, x$maxit), ]
customNNet$levels <- function(x) x$lev

methods <- list(
  rf = list(
    model = customRF
    # grid  = expand.grid(mtry = 1:5, ntree = c(500, 1000))
  ),
  nnet = list(
    model = customNNet
    # grid  = expand.grid(
    #   size  = c(3,5,7),
    #   decay = c(1e-4, 1e-3, 1e-2),
    #   maxit = c(150, 300, 500)
    # )
  )
)

# ML metric to maximize during cross-validation
metric = "F1"

# Efficiency thresholds
scenarios <- seq(0.75, 0.95, 0.1)

# metrics for model evaluation
MySummary <- function(data, lev = NULL, model = NULL) {

  data$pred <- factor(data$pred, levels = lev)
  data$obs  <- factor(data$obs,  levels = lev)

  cm <- caret::confusionMatrix(
    data = data$pred,
    reference = data$obs,
    positive = "efficient"
  )

  # ROC-AUC
  roc_obj <- pROC::roc(
    response = data$obs,
    predictor = data$efficient,
    levels = rev(lev), # primero el negativo
    direction = "<",
    quiet = TRUE)

  # PR-AUC
  ppos <- data[["efficient"]]

  pr_obj <- PRROC::pr.curve(
    scores.class0 = data[["efficient"]] [data$obs == "efficient"],
    scores.class1 = data[["efficient"]] [data$obs == "not_efficient"],
    curve = TRUE
  )

  # Entropy/Calibration metrics

  eps <- 1e-15
  y <- as.integer(data$obs == "efficient")

  pcl <- pmin(pmax(data[["efficient"]], eps), 1 - eps)
  LogLoss <- -mean(y * log(pcl) + (1 - y) * log(1 - pcl))
  PredEntropy_bits <- -mean(pcl * log2(pcl) + (1 - pcl) * log2(1 - pcl))
  Brier <- mean((pcl - y)^2)

  out <- c(
    cm$overall[c("Accuracy", "Kappa")],
    cm$byClass[c("Recall", "Specificity",
                 "Precision", "F1",
                 "Balanced Accuracy")],
    "ROC" = roc_obj$auc,
    "PR-AUC" = unname(pr_obj$auc.integral),
    "LogLoss" = LogLoss,
    "PredEntropy_bits" = PredEntropy_bits,
    "Brier" = Brier
  )

  return(out)
}

# parameters for controlling the training process
trControl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = MySummary,
  classProbs = TRUE,
  savePredictions = "all",
  allowParallel = FALSE
)

# Split data to validate on samples not used in training
hold_out <- 0.3

# model result
final_model <- efficiency_estimation(
  data = data,
  x = x,
  y = y,
  RTS = RTS,
  balance_data = balance_data,
  trControl = trControl,
  methods = methods,
  metric = metric,
  hold_out = hold_out,
  scenarios = scenarios
)

