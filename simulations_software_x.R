# # ================= #
# # cobb_douglas_XnY1 #
# # ================= #
#
# # ------------------------------------------------------------------------------
# # Load libraries ---------------------------------------------------------------
# # ------------------------------------------------------------------------------
#
# library(caret)
# library(Benchmarking)
# library(magrittr)
# library(dplyr)
# library(deaR)
#
# library(tibble)
# library(jsonlite)
#
# # load PEAXAI
# devtools::document()
# devtools::load_all()
#
#
# # =========================
# # Parameters
# # =========================
#
# DGP_vec <- c("cobb_douglas_XnY1", "cobb_douglas_CRS_XnY1")
# N_vec <- c(25, 50, 100, 300, 500, 1000, 2000)
# nX_vec <- c(1, 3, 6, 9, 12, 15)
# noise_vec <- c(0, 0.02, 0.05)
# repl <- 100
#
# seed <- 1
#
# set.seed(seed)
#
# for (DGP_i in DGP_vec) {
#   for (N_i in N_vec) {
#     for (nX_i in nX_vec) {
#
#       # set hyperparameters
#       if (nX_i == 1) {
#         x <- 1
#         y <- 2
#       } else if(nX_i == 3) {
#         x <- 1:3
#         y <- 4
#       } else if (nX_i == 6) {
#         x <- 1:6
#         y <- 7
#       } else if(nX_i == 9) {
#         x <- 1:9
#         y <- 10
#       } else if (nX_i == 12) {
#         x <- 1:12
#         y <- 13
#       } else if (nX_i == 15) {
#         x <- 1:15
#         y <- 16
#       }
#
#       # list of parameters for generate the dataset
#       parms <- list(vector, 2)
#       parms[[1]] <- N_i
#       parms[[2]] <- nX_i
#
#       names(parms) <- c("N", "nX")
#
#       # correlation information
#       sim_corr <- matrix(NA)
#
#       # error information
#       sim_err <- matrix(NA)
#
#       # time and resources information
#       sim_time <- matrix(NA)
#
#       # loop for i simulation
#       for (i in 1:repl) {
#
#         # generate the dataset
#         data <- reffcy(
#           DGP = DGP_i,
#           parms = parms
#         )
#
#         for (noise_i in noise_vec) {
#
#           # compute random error
#           random_error <- rnorm(n = N_i, mean = 0, sd = noise_i)
#
#           # compute new vector of outputs
#           data_i[, y] <- data[, y] * exp(random_error)
#
#
#
#
#         } # end noise
#       } # end i simulation
#     } # end nX inputs
#   } # end N size
# } # end DGP
#
#


# ================= #
# Simulation loop   #
# ================= #

library(caret)
library(Benchmarking)
library(magrittr)
library(dplyr)
library(deaR)
library(tibble)
library(jsonlite)

# load PEAXAI
devtools::document()
devtools::load_all()

# =========================
# Parameters
# =========================

DGP_vec   <- c("cobb_douglas_XnY1", "cobb_douglas_CRS_XnY1")
N_vec     <- c(50, 100, 300, 500, 1000, 2000)
nX_vec    <- c(1, 3, 6, 9, 12, 15)
noise_vec <- c(0, 0.02, 0.05)

repl <- 100
seed <- 1

set.seed(seed)

# =========================
# Empty object to save results
# =========================

sim_results <- list()
counter <- 1

# =========================
# Main loop
# =========================

for (DGP_i in DGP_vec) {
  for (N_i in N_vec) {
    for (nX_i in nX_vec) {
      for (r_i in 1:repl) {

        cat(
          "DGP:", DGP_i,
          "| N:", N_i,
          "| nX:", nX_i,
          "| repl:", r_i, "\n"
        )

        # -------------------------
        # Define inputs and outputs
        # -------------------------

        x <- 1:nX_i
        y <- nX_i + 1
        p <- length(x) + length(y)

        # -------------------------
        # Parameters for data generation
        # -------------------------

        parms <- list(
          N  = N_i,
          nX = nX_i
        )

        # -------------------------
        # Generate clean dataset
        # -------------------------

        data_clean <- reffcy(
          DGP   = DGP_i,
          parms = parms
        )

        data_clean <- as.data.frame(data_clean)

        # Save original output
        y_clean <- data_clean[, y]

        # Same base shock for all noise levels
        eps <- rnorm(n = N_i, mean = 0, sd = 1)

        # -------------------------
        # Loop over noise levels
        # -------------------------

        for (noise_i in noise_vec) {

          # Important: start always from the clean data
          data_i <- data_clean

          # Add multiplicative log-normal noise to output
          data_i[, y] <- y_clean * exp(noise_i * eps)


          # -------------------------
          # hypermarameters PEAXAI
          # -------------------------

          # Returns to scale assumption
          RTS <- "vrs"

          # Addressing target imbalance by specifying resampling ratios, using SMOTE units.
          imbalance_rate <- seq(0.1, 0.5, 0.1) #

          # ML metric to maximize during cross-validation
          metric_priority <- c("Balanced_Accuracy", "F1", "ROC_AUC")

          # parameters for controlling the training process
          trControl <- list(
            method = "cv",
            number = 5
          )

          # Split data to validate on samples not used in training
          hold_out <- NULL

          methods <- list(
            "nnet" = list(
              tuneGrid = expand.grid(
                size  = c(1, 3, 5, 10),
                decay = c(0.0001, 0.001, 0.01, 0.1)
              ),
              maxit = 5000,
              preProcess = c("center", "scale"),
              entropy = TRUE,
              skip = FALSE,
              MaxNWts = 500000,
              trace = FALSE,
              weights = NULL
            )
            # "svmPoly" = list(
            #   tuneGrid = expand.grid(
            #     degree = c(2, 3, 5),
            #     scale  = c(0.01, 0.1, 1),
            #     C      = c(0.1, 1, 10)
            #   ),
            #   preProcess = c("center", "scale")
            # ),
            # "svmRadial" = list(
            #   tuneGrid = expand.grid(
            #     sigma = c(1 / (8 * p), 1 / (4 * p), 1 / (2 * p)),
            #     C = c(0.1, 1, 10)
            #   ),
            #   preProcess = c("center", "scale")
            # ),
            # "rf" = list(
            #   tuneGrid = expand.grid(
            #     mtry = 1:p
            #   ),
            #   ntree = 500
            # ),
            # "glm" = list(
            #   weights = "dinamic"
            # )
          )

          # -------------------------
          # Run PEAXAI
          # -------------------------
          time_i <- system.time({

            # Example placeholder:
            fit_i <- PEAXAI_fitting(
              data = data_i,
              x = x,
              y = y,
              RTS = RTS,
              imbalance_rate = imbalance_rate,
              methods = methods,
              trControl = trControl,
              metric_priority = metric_priority,
              hold_out = NULL,
              seed = seed + counter,
              verbose = FALSE
            )

          })

          # -------------------------
          # Save basic simulation info
          # -------------------------
          print("info")
          stop()

          data_clean
          true_score <-

          sim_results[[counter]] <- tibble(
            DGP   = DGP_i,
            N     = N_i,
            nX    = nX_i,
            noise = noise_i,
            repl  = r_i,
            seed  = seed + counter,
            time_seconds = unname(time_i["elapsed"]),

            # Add later, for example:
            balanced_accuracy = as.numeric(fit_i$performance_train$nnet$Imbalance_rate)
            # mse_beta = ...
            # corr_beta = ...
          )

          counter <- counter + 1

        } # end noise loop

      } # end replication loop
    } # end nX loop
  } # end N loop
} # end DGP loop

# =========================
# Combine results
# =========================

sim_results_df <- bind_rows(sim_results)

# =========================
# Save raw results
# =========================

dir.create("GeneratedResults", showWarnings = FALSE)

saveRDS(
  sim_results_df,
  file = "GeneratedResults/simulation_results_raw.rds"
)

write.csv(
  sim_results_df,
  file = "GeneratedResults/simulation_results_raw.csv",
  row.names = FALSE
)

# =========================
# Aggregate results
# =========================

summary_results <- sim_results_df %>%
  group_by(DGP, N, nX, noise) %>%
  summarise(
    mean_time = mean(time_seconds, na.rm = TRUE),
    sd_time   = sd(time_seconds, na.rm = TRUE),
    n_repl    = n(),
    .groups = "drop"
  )

saveRDS(
  summary_results,
  file = "GeneratedResults/simulation_results_summary.rds"
)

write.csv(
  summary_results,
  file = "GeneratedResults/simulation_results_summary.csv",
  row.names = FALSE
)

