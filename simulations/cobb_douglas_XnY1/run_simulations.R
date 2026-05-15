# ========================================== #
# run_simulations.R
# ========================================== #

# ===
# libraries
# ===
library(caret)
library(Benchmarking)
library(magrittr)
library(dplyr)
library(deaR)
library(future)
library(future.apply)
library(pROC)
library(PRROC)

# # Source required PEAXAI functions
# source("C:/Users/Ricardo/Documents/PEAXAI/R/preprocessing.R")
# source("C:/Users/Ricardo/Documents/PEAXAI/R/label_efficiency.R")
# source("C:/Users/Ricardo/Documents/PEAXAI/R/PEAXAI_fitting.R")
# source("C:/Users/Ricardo/Documents/PEAXAI/R/PEAXAI_counterfactuals.R")
# source("C:/Users/Ricardo/Documents/PEAXAI/R/PEAXAI_predict.R")
# source("C:/Users/Ricardo/Documents/PEAXAI/R/SMOTE_data.R")
# source("C:/Users/Ricardo/Documents/PEAXAI/R/SMOTE_Z_data.R")
# source("C:/Users/Ricardo/Documents/PEAXAI/R/training.R")
# source("C:/Users/Ricardo/Documents/PEAXAI/R/convex_facets.R")
# source("C:/Users/Ricardo/Documents/PEAXAI/R/get_SMOTE_DMUs.R")
# source("C:/Users/Ricardo/Documents/PEAXAI/simulations/cobb_douglas_XnY1/scenarios/mejoras en script.R") # if there is anything needed


# ===
# Setup Parallel Backend
# ===
# plan(multisession, workers = availableCores() - 1)

# load PEAXAI
devtools::document()
devtools::load_all()

# ===
# parameters
# ===
DGP <- "cobb_douglas_XnY1"
repl <- 100

scenarios_grid <- expand.grid(
  N = c(25, 50, 150, 200),
  nX = c(1, 3, 6, 9, 12),
  noise = c(0, 0.02, 0.05)
)

label_type_choices <- c("additive", "bootstrapping_dea")

print_progress_metrics <- function(results_so_far, rep_id, total_reps = 100, digits = 4) {

  metric_cols <- grep(
    "^(corr_pearson|corr_spearman|mse|bias)",
    names(results_so_far),
    value = TRUE
  )

  metric_cols <- metric_cols[!grepl("kendall", metric_cols)]

  if (length(metric_cols) == 0) {
    cat("\nNo hay columnas de métricas todavía.\n")
    return(invisible(NULL))
  }

  parse_metric_col <- function(col_name) {

    metric <- NA_character_

    if (grepl("^corr_pearson", col_name)) {
      metric <- "pearson"
    } else if (grepl("^corr_spearman", col_name)) {
      metric <- "spearman"
    } else if (grepl("^mse", col_name)) {
      metric <- "mse"
    } else if (grepl("^bias", col_name)) {
      metric <- "bias"
    }

    technique <- NA_character_
    cut_off <- NA_character_

    # DEA / BDEA
    if (grepl("_DEA$", col_name) && !grepl("PEAXAI", col_name)) {
      technique <- "DEA"
      cut_off <- "-"
    } else if (grepl("_BDEA$", col_name)) {
      technique <- "BDEA"
      cut_off <- "-"
    }

    # PEAXAI
    if (grepl("PEAXAI", col_name)) {

      clean_name <- col_name

      clean_name <- sub("^corr_pearson_yD_score_PEAXAI_", "", clean_name)
      clean_name <- sub("^corr_spearman_yD_score_PEAXAI_", "", clean_name)
      clean_name <- sub("^mse_score_PEAXAI_", "", clean_name)
      clean_name <- sub("^bias_score_PEAXAI_", "", clean_name)

      # Caso nombres tipo: nnet_y_PEAXAI_0.75
      clean_name <- gsub("_y_PEAXAI_", "_", clean_name)

      # Caso nombres tipo: nnet_score_PEAXAI_0.75
      clean_name <- gsub("_score_PEAXAI_", "_", clean_name)

      parts <- strsplit(clean_name, "_")[[1]]

      cut_off <- tail(parts, 1)
      technique <- paste(parts[-length(parts)], collapse = "_")
      technique <- paste0("PEAXAI_", technique)
    }

    data.frame(
      column = col_name,
      metric = metric,
      technique = technique,
      cut_off = cut_off,
      stringsAsFactors = FALSE
    )
  }

  metric_info <- do.call(
    rbind,
    lapply(metric_cols, parse_metric_col)
  )

  metric_info$value <- sapply(metric_cols, function(col) {
    mean(as.numeric(results_so_far[[col]]), na.rm = TRUE)
  })

  summary_table <- reshape(
    metric_info[, c("technique", "cut_off", "metric", "value")],
    idvar = c("technique", "cut_off"),
    timevar = "metric",
    direction = "wide"
  )

  names(summary_table) <- sub("^value\\.", "", names(summary_table))

  expected_cols <- c("technique", "cut_off", "pearson", "spearman", "mse", "bias")

  for (col in expected_cols) {
    if (!col %in% names(summary_table)) {
      summary_table[[col]] <- NA
    }
  }

  summary_table <- summary_table[, expected_cols]

  summary_table <- summary_table[order(summary_table$technique, summary_table$cut_off), ]

  numeric_cols <- c("pearson", "spearman", "mse", "bias")
  summary_table[numeric_cols] <- lapply(summary_table[numeric_cols], function(x) {
    round(as.numeric(x), digits)
  })

  cat("\n")
  cat("============================================================\n")
  cat("Progress after repetition ", rep_id, "/", total_reps, "\n", sep = "")
  cat("Cumulative means within current scenario\n")
  cat("============================================================\n")

  print(summary_table, row.names = FALSE)

  cat("============================================================\n\n")

  invisible(summary_table)
}

# We create a generic function to run a single iteration
run_single_simulation <- function(id, N, nX, std_dev, DGP) {

  # Set x and y indices
  if (nX == 1) {
    x <- 1
    y <- 2
  } else {
    x <- 1:nX
    y <- nX + 1
  }

  # ===
  # Methods configuration
  # ===
  methods <- list(
    "nnet" = list(
      tuneGrid = expand.grid(
        size  = c(2:18),
        decay = c(0.0001, 0.001, 0.01, 0.1)
      ),
      maxit = 2000,
      preProcess = c("center", "scale"),
      entropy = TRUE,
      skip = FALSE,
      MaxNWts = 500000,
      trace = FALSE,
      weights = NULL
    ),
    "svmPoly" = list(
      tuneGrid = expand.grid(
        degree = c(1, 2),
        scale  = c(0.1, 1),
        C      = c(0.05, 0.1, 0.5)
      )
    ),
    "svmRadial" = list(
      tuneGrid = expand.grid(
        sigma = c(0.005, 0.01, 0.02),
        C = c(0.05, 0.1, 0.5)
      )
    ),
    "rf" = list(
      tuneGrid = expand.grid(
        mtry = 1:length(c(x, y)) # Dynamically adapt to columns
      ),
      ntree = 100
    ),
    "glm" = list(
      weights = "dinamic"
    )
  )

  # We prepare a single row data frame to return the metrics
  result_row <- data.frame(
    id = id,
    DGP = DGP,
    scenario = nX,
    N = N,
    noise = std_dev
  )

  repeat {
    # Generate data
    data <- reffcy(
      DGP = DGP,
      parms = list(N = N, nX = nX)
    )

    # Add noise
    random_error <- rnorm(n = N, mean = 0, sd = std_dev)
    data[, y] <- data[, y] * exp(random_error)

    scores_baseline <- list()
    scores_baseline$score_yD <- data[, "yD"] / data[, y]

    # ===
    # DEA
    # ===
    tech_xmat <- as.matrix(data[, x])
    tech_ymat <- as.matrix(data[, y])

    bcc_scores <- dea(
      X = tech_xmat,
      Y = tech_ymat,
      XREF  = tech_xmat,
      YREF = tech_ymat,
      RTS = "vrs",
      ORIENTATION = "out"
    )[["eff"]]

    scores_baseline$score_DEA <- as.vector(bcc_scores)

    # ===
    # BDEA
    # ===
    try_bdea <- tryCatch({
      dea.boot(
        tech_xmat, tech_ymat,
        NREP = 200, ORIENTATION = "out", alpha = 0.01
      )[["eff.bc"]]

    }, error = function(e) NULL)

    if (!is.null(try_bdea)) {
      scores_baseline$score_BDEA <- as.vector(try_bdea)
      break # Valid iteration found
    }

  } # End repeat

  # Save metrics for DEA and BDEA
  for (tech in c("DEA", "BDEA")) {

    result_row[[paste0("corr_pearson_yD_", tech)]] <- cor(
      scores_baseline$score_yD, scores_baseline[[paste0("score_", tech)]], method = "pearson"
    )

    result_row[[paste0("corr_spearman_yD_", tech)]] <- cor(
      scores_baseline$score_yD, scores_baseline[[paste0("score_", tech)]], method = "spearman"
    )

    result_row[[paste0("corr_kendall_yD_", tech)]] <- cor(
      scores_baseline$score_yD, scores_baseline[[paste0("score_", tech)]], method = "kendall"
    )

    # Calculate MSE and Bias
    result_row[[paste0("mse_score_yD_", tech)]] <- mean(
      (scores_baseline$score_yD - scores_baseline[[paste0("score_", tech)]])^2,
      na.rm = TRUE
    )

    result_row[[paste0("bias_score_yD_", tech)]] <- abs(mean(
      scores_baseline$score_yD - scores_baseline[[paste0("score_", tech)]],
      na.rm = TRUE
    ))
  }

  # ===
  # PEAXAI Setup
  # ===
  trControl <- trainControl(
    method = "cv",
    number = 5
  )

  # Prepare directional vector (output expansion)
  rel_imp <- rep(0, length(c(x, y)))
  rel_imp[length(c(x, y))] <- 1 # Output has weight 1
  rel_imp_df <- as.data.frame(t(rel_imp))
  names(rel_imp_df) <- names(data)[c(x, y)]

  directional_vector <- list(
    relative_importance = rel_imp_df,
    baseline = "mean"
  )

  # Fit the ML models (this handles labeling, SMOTE, and CV internally)
  models <- PEAXAI_fitting(
    data = data,
    x = x,
    y = y,
    RTS = "vrs",
    imbalance_rate = seq(0.05, 0.5, 0.05),
    trControl = trControl,
    methods = methods,
    metric_priority = c("Balanced_Accuracy", "F1", "ROC_AUC"),
    seed = 314,
    verbose = TRUE
  )

  # To compute average score across multiple algorithms
  y_counterfactuals_list <- list()

  for (method_name in names(models$best_model_fit)) {
    final_model <- models$best_model_fit[[method_name]]

    # For now using a threshold of 0.5. Can be adapted if model provides another cut_off.
    cut_off <- c(0.75, 0.85, 0.95)

    # Compute Counterfactuals
    PEAXAI_result <- PEAXAI_counterfactuals(
      data = data,
      x = x,
      y = y,
      final_model = final_model,
      efficiency_thresholds = cut_off,
      directional_vector = directional_vector,
      n_expand = 0.1,
      n_grid = 50,
      max_y = 1,
      min_x = 1
    )

    y_PEAXAI_0.75 <- PEAXAI_result[["0.75"]][["counterfactual_dataset"]][["y"]]
    y_PEAXAI_0.85 <- PEAXAI_result[["0.85"]][["counterfactual_dataset"]][["y"]]
    y_PEAXAI_0.95 <- PEAXAI_result[["0.95"]][["counterfactual_dataset"]][["y"]]

    y_counterfactuals_list[[method_name]] <- list(
      y_PEAXAI_0.75 = y_PEAXAI_0.75,
      y_PEAXAI_0.85 = y_PEAXAI_0.85,
      y_PEAXAI_0.95 = y_PEAXAI_0.95)
  }

  # Compute score (output oriented: y_counterfactual / y_observed)
  # Valores observados de y
  y_obs <- as.numeric(data[[y]])

  # Comprobación básica
  if (any(y_obs == 0, na.rm = TRUE)) {
    warning("Hay valores observados iguales a 0. La división generará Inf o NaN.")
  }

  # Crear lista de scores conservando la estructura de y_counterfactuals_list
  scores_counterfactuals_list <- lapply(y_counterfactuals_list, function(method_list) {

    lapply(method_list, function(y_cf) {

      y_cf <- as.numeric(y_cf)

      if (length(y_cf) != length(y_obs)) {
        stop("La longitud de un vector counterfactual no coincide con la longitud de y_obs.")
      }

      y_cf / y_obs
    })
  })

  # # Calculate correlations
  # result_row[[paste0("corr_spearman_yD_score_PEAXAI_", tech_label)]] <- cor(
  #   scores_baseline$score_yD, scores_counterfactuals_list, method = "spearman"
  # )
  # result_row[[paste0("corr_kendall_yD_score_PEAXAI_", tech_label)]] <- cor(
  #   scores_baseline$score_yD, scores_counterfactuals_list, method = "kendall"
  # )
  #
  # # Calculate MSE and Bias
  # result_row[[paste0("mse_score_PEAXAI_", tech_label)]] <- mean((scores_baseline$score_yD - score_cafee_mean)^2, na.rm = TRUE)
  # result_row[[paste0("bias_score_PEAXAI_", tech_label)]] <- abs(mean(scores_baseline$score_yD - score_cafee_mean, na.rm = TRUE))

  # Calculate correlations, MSE and Bias for each method and cut-off
  for (tech_label in names(scores_counterfactuals_list)) {

    method_scores <- scores_counterfactuals_list[[tech_label]]

    for (cut_label in names(method_scores)) {

      score_PEAXAI <- as.numeric(method_scores[[cut_label]])

      # Optional: clean cut-off label for column names
      # Example: score_PEAXAI_0.75 -> 0.75
      cut_clean <- sub("^score_PEAXAI_", "", cut_label)

      # Name suffix
      suffix <- paste0(tech_label, "_", cut_clean)

      # Calculate correlations
      result_row[[paste0("corr_pearson_yD_score_PEAXAI_", suffix)]] <- cor(
        scores_baseline$score_yD,
        score_PEAXAI,
        method = "pearson",
        use = "complete.obs"
      )

      result_row[[paste0("corr_spearman_yD_score_PEAXAI_", suffix)]] <- cor(
        scores_baseline$score_yD,
        score_PEAXAI,
        method = "spearman",
        use = "complete.obs"
      )

      result_row[[paste0("corr_kendall_yD_score_PEAXAI_", suffix)]] <- cor(
        scores_baseline$score_yD,
        score_PEAXAI,
        method = "kendall",
        use = "complete.obs"
      )

      # Calculate MSE and Bias
      result_row[[paste0("mse_score_PEAXAI_", suffix)]] <- mean(
        (scores_baseline$score_yD - score_PEAXAI)^2,
        na.rm = TRUE
      )

      result_row[[paste0("bias_score_PEAXAI_", suffix)]] <- abs(mean(
        scores_baseline$score_yD - score_PEAXAI,
        na.rm = TRUE
      ))
    }
  }

  return(result_row)
}

# ===
# Launch Simulations
# ===

all_simulations <- list()
total_scenarios <- nrow(scenarios_grid)

for (s in 1:total_scenarios) {

  N_s <- scenarios_grid$N[s]
  nX_s <- scenarios_grid$nX[s]
  noise_s <- scenarios_grid$noise[s]

  cat(sprintf("\n\n############################################################\n"))
  cat(sprintf("STARTING SCENARIO %d/%d: N=%d, nX=%d, noise=%.2f\n", s, total_scenarios, N_s, nX_s, noise_s))
  cat(sprintf("############################################################\n"))

  scenario_results <- list()

  for (i in 1:repl) {

    # 1. Ejecutar la repetición
    res <- run_single_simulation(
      id = i,
      N = N_s,
      nX = nX_s,
      std_dev = noise_s,
      DGP = DGP
    )

    # 2. Guardar el resultado en la lista temporal de este escenario
    scenario_results[[i]] <- res

    # 3. Mostrar el progreso actualizado con la media de lo que llevamos de este escenario
    results_so_far <- dplyr::bind_rows(scenario_results)
    print_progress_metrics(results_so_far, rep_id = i, total_reps = repl)
  }

  # Al terminar las 100 repeticiones de ESTE escenario, guardamos en la lista global
  scenario_df <- dplyr::bind_rows(scenario_results)
  all_simulations[[s]] <- scenario_df

  # Unimos todos los escenarios completados hasta ahora
  current_all_df <- dplyr::bind_rows(all_simulations)

  # Backup: Exportar el estado actual por si el script se cae o se interrumpe
  saveRDS(current_all_df, file = "simulaciones_results_partial.rds")
  write.csv(current_all_df, file = "simulaciones_results_partial.csv", row.names = FALSE)

  cat(sprintf("\n>>> Escenario %d completado y exportado exitosamente. <<<\n", s))
}

# Exportación final definitiva
final_simulations <- dplyr::bind_rows(all_simulations)
saveRDS(final_simulations, file = "simulaciones_results.rds")
write.csv(final_simulations, file = "simulaciones_results.csv", row.names = FALSE)

cat("\n¡Todas las simulaciones han terminado con éxito!\n")
