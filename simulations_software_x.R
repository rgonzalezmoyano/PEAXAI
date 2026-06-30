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
library(peakRAM)

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

DGP_vec   <- c("cobb_douglas_XnY1")
N_vec     <- c(100)
nX_vec    <- c(1, 3)
noise_vec <- c(0, 0.02)

repl <- 3
seed <- 1

set.seed(seed)

# =========================
# hypermarameters PEAXAI
# =========================
# Addressing target imbalance by specifying resampling ratios, using SMOTE units.
imbalance_rate <- seq(0.1, 0.5, 0.1)

# ML metric to maximize during cross-validation
metric_priority <- c("Balanced_Accuracy", "F1", "ROC_AUC")

# parameters for controlling the training process
trControl <- list(
  method = "cv",
  number = 5
)

# Split data to validate on samples not used in training
hold_out <- NULL

# method and hyperparameters
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
)

# =========================
# Empty object to save results
# =========================

sim_results_time <- list()
sim_results_performace <- list()
sim_results_correlation <- list()

counter <- 1

# =========================
# Main loop
# =========================
for (DGP_i in DGP_vec) {

  if(DGP_i == "cobb_douglas_XnY1") {
    RTS <- "vrs"
  } else {
    RTS <- "crs"
  }

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

          # -------------------------
          # Run PEAXAI (retry with new data on error)
          # -------------------------
          attempt <- 0
          fit_i <- NULL
          score_BDEA <- NULL

          repeat {
            attempt <- attempt + 1

            # Important: start always from the clean data.
            # On retries, regenerate a fresh dataset.
            if (attempt == 1) {
              data_i <- data_clean
              data_i[, y] <- y_clean * exp(noise_i * eps)
            } else {
              cat("  [Retry", attempt, "] Regenerating dataset for noise =", noise_i, "\n")
              data_retry <- reffcy(DGP = DGP_i, parms = parms)
              data_retry <- as.data.frame(data_retry)
              eps_retry  <- rnorm(n = N_i, mean = 0, sd = 1)
              data_i     <- data_retry
              data_i[, y] <- data_retry[, y] * exp(noise_i * eps_retry)
            }

            ram_i <- peakRAM::peakRAM({
stop()
              fit_i <- tryCatch(
                {
                  PEAXAI_fitting(
                    data = data_i,
                    x = x,
                    y = y,
                    RTS = RTS,
                    imbalance_rate = imbalance_rate,
                    methods = methods,
                    trControl = trControl,
                    metric_priority = metric_priority,
                    hold_out = hold_out,
                    seed = seed + counter,
                    verbose = FALSE
                  )
                },
                error = function(e) {
                  cat("  [Error in PEAXAI_fitting]:", conditionMessage(e), "\n")
                  NULL
                }
              )

            })

            score_BDEA <- tryCatch(
              {
                Benchmarking::dea.boot(
                  X = as.matrix(data_i[,x,drop = FALSE]),
                  Y = as.matrix(data_i[,y,drop = FALSE]),
                  NREP = 200,
                  ORIENTATION = "out",
                  alpha = 0.05
                )[["eff.bc"]]
              },
              error = function(e) {
                cat("  [Error in BDEA:", conditionMessage(e), "\n")
                NULL
              }
            )

            if (!is.null(fit_i) & !is.null(score_BDEA)) break
          }

          # -------------------------
          # Save basic simulation info
          # -------------------------
          print("info")

          # -------------------------
          # TRUE results
          # -------------------------
          # output oriented
          true_score <- data_clean$yD / data_clean$y

          # -------------------------
          # DEA results
          # -------------------------
          score_DEA <- Benchmarking::dea(
            X = data_i[, x, drop = FALSE],
            Y = data_i[, y, drop = FALSE],
            RTS = RTS,
            ORIENTATION = "out"
          )$eff

          mse_DEA <- mean((true_score - score_DEA)^2)

          corr_DEA_pearson <- cor(true_score, score_DEA, method = "pearson")
          corr_DEA_spearman <- cor(true_score, score_DEA, method = "spearman")
          corr_DEA_kendall <- cor(true_score, score_DEA, method = "kendall")

          matrix_corr <- as.data.frame(matrix(
            NA, nrow = 3, ncol = 3
          ))
          names(matrix_corr) <- c("threshold", "method", "correlation")

          matrix_corr[1,1] <- "DEA"
          matrix_corr[1,2] <- "pearson"
          matrix_corr[1,3] <- corr_DEA_pearson

          matrix_corr[2,1] <- "DEA"
          matrix_corr[2,2] <- "spearman"
          matrix_corr[2,3] <- corr_DEA_spearman

          matrix_corr[3,1] <- "DEA"
          matrix_corr[3,2] <- "kendall"
          matrix_corr[3,3] <- corr_DEA_kendall

          # -------------------------
          # BDEA results
          # -------------------------
          mse_BDEA <- mean((true_score - score_BDEA)^2)

          corr_BDEA_pearson <- cor(true_score, score_BDEA, method = "pearson")
          corr_BDEA_spearman <- cor(true_score, score_BDEA, method = "spearman")
          corr_BDEA_kendall <- cor(true_score, score_BDEA, method = "kendall")

          matrix_corr_BDEA <- as.data.frame(matrix(
            NA, nrow = 3, ncol = 3
          ))
          names(matrix_corr_BDEA) <- c("threshold", "method", "correlation")

          matrix_corr_BDEA[1,1] <- "BDEA"
          matrix_corr_BDEA[1,2] <- "pearson"
          matrix_corr_BDEA[1,3] <- corr_BDEA_pearson

          matrix_corr_BDEA[2,1] <- "BDEA"
          matrix_corr_BDEA[2,2] <- "spearman"
          matrix_corr_BDEA[2,3] <- corr_BDEA_spearman

          matrix_corr_BDEA[3,1] <- "BDEA"
          matrix_corr_BDEA[3,2] <- "kendall"
          matrix_corr_BDEA[3,3] <- corr_BDEA_kendall

          # -------------------------
          # PEAXAI results
          # -------------------------
          efficiency_thresholds <- c(seq(0.1, 0.9, 0.1),0.99)

          relative_importance <- matrix(0, nrow = 1, ncol = p)
          relative_importance[1, y] <- 1

          relative_importance <- as.data.frame(relative_importance)
          names(relative_importance) <- names(data_i)[c(x,y)]

          score_PEAXAI <- as.data.frame(matrix(
            NA, ncol = length(efficiency_thresholds), nrow = NROW(data_i)
          ))
          names(score_PEAXAI) <- efficiency_thresholds

          # save time
          time_counterfactual_thr <- as.data.frame(matrix(
            NA, ncol = length(efficiency_thresholds), nrow = 1
          ))
          names(time_counterfactual_thr) <- efficiency_thresholds

          RAM_counterfactual_thr <- as.data.frame(matrix(
            NA, ncol = length(efficiency_thresholds), nrow = 1
          ))
          names(RAM_counterfactual_thr) <- efficiency_thresholds

          for (thr in efficiency_thresholds) {

            ram_i_counterfactual <- peakRAM::peakRAM({

              targets <- PEAXAI_counterfactuals(
                data = data_i[,c(x,y), drop = FALSE],
                x = x,
                y = y,
                final_model = fit_i[["best_model_fit"]][["nnet"]],
                efficiency_thresholds = thr,
                directional_vector = list(
                  relative_importance = relative_importance,
                  baseline = "mean"
                ),
                n_expand = 0.15,
                n_grid = 20,
                max_y = 1.2,
                min_x = 1
              )

            })

            time_counterfactual_thr[, as.character(thr)] <- ram_i_counterfactual$Elapsed_Time_sec
            RAM_counterfactual_thr[, as.character(thr)] <- ram_i_counterfactual$Peak_RAM_Used_MiB

            score_PEAXAI_thr <- targets[[as.character(thr)]][["counterfactual_dataset"]][["y"]] / data_clean$y

            score_PEAXAI[, as.character(thr)] <- score_PEAXAI_thr

          }

          mse_PEAXAI <- colMeans((score_PEAXAI - true_score)^2)

          names(mse_PEAXAI) <- paste0(
            "mse_PEAXAI_",
            names(mse_PEAXAI)
          )

          # save results of tunning (time + RAM)
          sim_results_time[[counter]] <- tibble(
            DGP   = DGP_i,
            N     = N_i,
            nX    = nX_i,
            noise = noise_i,
            repl  = r_i,
            seed  = seed + counter,
            time_seconds_training = ram_i$Elapsed_Time_sec,
            RAM_peak_MiB_training = ram_i$Peak_RAM_Used_MiB
          )

          colnames(time_counterfactual_thr) <- paste0(
            "PEAXAI_time_counter_",
            colnames(time_counterfactual_thr)
          )

          colnames(RAM_counterfactual_thr) <- paste0(
            "PEAXAI_RAM_counter_MiB_",
            colnames(RAM_counterfactual_thr)
          )

          sim_results_time[[counter]] <- cbind(
            sim_results_time[[counter]],
            time_counterfactual_thr,
            RAM_counterfactual_thr
          )

          imbalance <- fit_i$performance_train$nnet$Imbalance_rate
          imbalance <- as.numeric(sub("\\*$", "", imbalance))

          # save results of performance (score)
          sim_results_performace[[counter]] <- tibble(
            DGP   = DGP_i,
            N     = N_i,
            nX    = nX_i,
            noise = noise_i,
            repl  = r_i,
            seed  = seed + counter,
            # Add later, for example:
            imbalance_rate = imbalance,
            balanced_accuracy = fit_i$performance_train$nnet$Balanced_Accuracy,
            mse_DEA = mse_DEA,
            mse_BDEA = mse_BDEA
          )

          sim_results_performace[[counter]] <- cbind(sim_results_performace[[counter]], t(mse_PEAXAI))

          thresholds <- as.character(efficiency_thresholds)

          corr_methods <- c("pearson", "spearman", "kendall")

          corr_PEAXAI <- expand.grid(
            threshold = thresholds,
            method = corr_methods,
            stringsAsFactors = FALSE
          )

          corr_PEAXAI$correlation <- mapply(
            function(thr, met) {
              cor(true_score, score_PEAXAI[[thr]], method = met)
            },
            corr_PEAXAI$threshold,
            corr_PEAXAI$method
          )

          matrix_corr <- rbind(matrix_corr, matrix_corr_BDEA, corr_PEAXAI)

          # save results of correlations (correlations)
          sim_results_correlation[[counter]] <- tibble(
            DGP   = DGP_i,
            N     = N_i,
            nX    = nX_i,
            noise = noise_i,
            repl  = r_i,
            seed  = seed + counter,
            threshold   = c(matrix_corr$threshold, matrix_corr_BDEA$threshold),
            method      = c(matrix_corr$method, matrix_corr_BDEA$method),
            correlation = c(matrix_corr$correlation, matrix_corr_BDEA$correlation)
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

sim_results_time_df <- bind_rows(sim_results_time)
sim_results_performace_df <- bind_rows(sim_results_performace)
sim_results_correlation_df <- bind_rows(sim_results_correlation)

# =========================
# Save raw results
# =========================

dir.create("GeneratedResults", showWarnings = FALSE)
#
# saveRDS(
#   sim_results_df,
#   file = "GeneratedResults/simulation_results_raw.rds"
# )
#
# write.csv(
#   sim_results_df,
#   file = "GeneratedResults/simulation_results_raw.csv",
#   row.names = FALSE
# )

# =========================
# Aggregate results
# =========================

vars_summary <- setdiff(
  names(sim_results_time_df),
  c("DGP", "N", "nX", "noise", "repl", "seed")
)

summary_results_time <- sim_results_time_df %>%
  group_by(DGP, N, nX, noise) %>%
  summarise(
    across(
      all_of(vars_summary),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.fn}_{.col}"
    ),
    n_repl = n(),
    .groups = "drop"
  )

vars_summary <- setdiff(
  names(sim_results_performace_df),
  c("DGP", "N", "nX", "noise", "repl", "seed")
)

summary_results_performance <- sim_results_performace_df %>%
  group_by(DGP, N, nX, noise) %>%
  summarise(
    across(
      all_of(vars_summary),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.fn}_{.col}"
    ),
    n_repl = n(),
    .groups = "drop"
  )

vars_summary <- setdiff(
  names(sim_results_correlation_df),
  c("DGP", "N", "nX", "noise", "repl", "seed", "threshold", "method")
)

summary_results_correlation <- sim_results_correlation_df %>%
  group_by(DGP, N, nX, noise, threshold, method) %>%
  summarise(
    across(
      all_of(vars_summary),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.fn}_{.col}"
    ),
    n_repl = n(),
    .groups = "drop"
  )

# saveRDS(
#   summary_results,
#   file = "GeneratedResults/simulation_results_summary.rds"
# )
#
# write.csv(
#   summary_results,
#   file = "GeneratedResults/simulation_results_summary.csv",
#   row.names = FALSE
# )

