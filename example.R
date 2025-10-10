################################################################################
####################### EXAMPLE OF USE 'PEAXAI' ################################
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
# data <- firms
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

# ML metric to maximize during cross-validation
metric = "F1"

# XAI method to determine relative importance of varaibles
importance_method <- "SHAP"

# Efficiency thresholds
scenarios <- seq(0.75, 0.95, 0.1)

# parameters for controlling the training process
trControl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "all",
  allowParallel = FALSE
)

# Split data to validate on samples not used in training
hold_out <- 0.2

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
  ),
  nnet = list(
    model = customNNet
  )
)

# model result
analisys_PEAXAI <- efficiency_estimation(
  data = data,
  x = x,
  y = y,
  RTS = RTS,
  balance_data = balance_data,
  trControl = trControl,
  methods = methods,
  metric = metric,
  hold_out = hold_out,
  importance_method = importance_method,
  scenarios = scenarios
)

