#' @title Projection-Based Efficiency Targets
#'
#' @description
#' Computes efficiency projections for each observation based on a trained
#' classifier from \pkg{caret} that provides class probabilities via
#' \code{predict(type = "prob")}. For each probability threshold, the function
#' finds the direction and magnitude of change in input–output space required
#' for a unit to reach a specified efficiency level, following a directional
#' distance approach.
#'
#' @param data A \code{data.frame} or \code{matrix} containing input and output variables.
#' @param x A numeric vector indicating the column indexes of input variables in \code{data}.
#' @param y A numeric vector indicating the column indexes of output variables in \code{data}.
#' @param final_model A fitted \pkg{caret} model of class \code{"train"} that supports
#'   \code{predict(type = "prob")} and returns a probability column for the efficient class.
#' @param calibration_model Optional probability-calibration model applied to the raw predicted probabilities from \code{final_model} (e.g., Platt scaling or isotonic regression).
#' If provided, calibrated probabilities are used for ranking and threshold-based decisions.
#' Set to \code{NULL} to use uncalibrated predictions.
#' @param efficiency_thresholds A numeric vector of probability levels in (0,1)
#'   that define the efficiency classes (e.g., \code{c(0.75, 0.9, 0.95)}).
#' @param directional_vector A \code{list} with the required information to
#'   construct the directional vector, including:
#'   \itemize{
#'     \item \code{relative_importance}: Numeric vector of variable importances that sum to 1.
#'     \item \code{scope}: \code{"global"} (currently supported) or \code{"local"}.
#'     \item \code{baseline}: \code{"mean"}, \code{"median"}, \code{"self"} or \code{"ones"}.
#'   }
#' @param n_expand Numeric. Number of expansion steps used to enlarge the initial
#'   search range for \eqn{\beta}.
#' @param n_grid Integer. Number of grid points evaluated during each iteration
#'   to refine the cutoff value of \eqn{\beta}.
#' @param max_y Numeric. Upper-limit multiplier for output expansion in the search
#'   procedure (default = 2).
#' @param min_x Numeric. Lower-limit multiplier for input contraction in the search
#'   procedure (default = 1).
#'
#' @details
#' For each observation and for each probability level in \code{efficiency_thresholds},
#' the function searches for the smallest directional distance \eqn{\beta} such that
#' the predicted probability of belonging to the efficient class reaches the target.
#'
#' @return
#' A named \code{list} with one element per threshold. Each element contains:
#' \itemize{
#'   \item \code{data}: A \code{data.frame} of projected input–output values at that threshold.
#'   \item \code{beta}: A two-column \code{data.frame} with the optimal \eqn{\beta}
#'         and the corresponding predicted probability.
#' }
#'
#' @seealso
#' \code{\link{find_beta_maxmin}} for initializing search bounds;
#' \code{\link[caret]{train}} for model training.
#'
#' @examples
#' \donttest{
#'   data("firms", package = "PEAXAI")
#'
#'   data <- subset(
#'     firms,
#'     autonomous_community == "Comunidad Valenciana"
#'   )
#'
#'   x <- 1:4
#'   y <- 5
#'   RTS <- "vrs"
#'   imbalance_rate <- NULL
#'
#'   trControl <- list(
#'     method = "cv",
#'     number = 3
#'   )
#'
#'   # glm method
#'   methods <- list(
#'     "glm" = list(
#'       weights = "dinamic"
#'      )
#'    )
#'
#'   metric_priority <- c("Balanced_Accuracy", "ROC_AUC")
#'
#'   models <- PEAXAI_fitting(
#'     data = data, x = x, y = y, RTS = RTS,
#'     imbalance_rate = imbalance_rate,
#'     methods = methods,
#'     trControl = trControl,
#'     metric_priority = metric_priority,
#'     verbose = FALSE,
#'     seed = 1
#'   )
#'
#'   final_model <- models[["best_model_fit"]][["glm"]]
#'
#'   relative_importance <- PEAXAI_global_importance(
#'     data = data, x = x, y = y,
#'     final_model = final_model,
#'     background = "real", target = "real",
#'     importance_method = list(name = "PI", n.repetitions = 5)
#'   )
#'
#'   efficiency_thresholds <- seq(0.75, 0.95, 0.1)
#'
#'   directional_vector <- list(relative_importance = relative_importance,
#'   scope = "global", baseline  = "mean")
#'
#'   targets <- PEAXAI_targets(data = data, x = x, y = y, final_model = final_model,
#'   efficiency_thresholds = efficiency_thresholds, directional_vector = directional_vector,
#'   n_expand = 0.5, n_grid = 50, max_y = 2, min_x = 1)
#' }
#'
#' @export

PEAXAI_targets <- function (
    data, x, y, final_model, calibration_model = NULL,
    efficiency_thresholds, directional_vector,
    n_expand, n_grid, max_y = 2, min_x = 1
) {

  # validate_parametes_PEAXAI_targets(
  #   data, x, y, final_model,
  #   efficiency_thresholds, directional_vector,
  #   n_expand, n_grid, max_y, min_x
  # )

  data <- as.data.frame(data)

  # reorder index 'x' and 'y' in data
  data <- data[, c(x,y)]
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)

  names_data <- names(data[,c(x,y)])

  # min and max values
  min_x_possible <- apply(as.matrix(data[,x]), 2, min)

  max_y_possible <- apply(as.matrix(data[,y]), 2, max)

  # number of decimals to round
  precision_prob <- 5

  # ----------------------------------------------------------------------------
  # Build vector G (directional vector) ----------------------------------------
  # ----------------------------------------------------------------------------
  if (directional_vector[["scope"]] == "global") {

    # # relative importance
    # score_imp_x <- as.numeric(directional_vector[["relative_importance"]][x])
    # score_imp_y <- as.numeric(directional_vector[["relative_importance"]][y])

    # baseline
    if (directional_vector[["baseline"]] == "mean") {

      # new
      L2_nrom_mean <- sqrt(sum(colMeans(data[, c(x,y)])^2))

      # normalize
      v <- colMeans(data[, c(x,y)])

      v_w <- v * directional_vector[["relative_importance"]]

      # noralize
      L2_nrom_vector <- sqrt(sum(v_w[, c(x,y)]^2))
      v_final <- (v_w / L2_nrom_vector) * L2_nrom_mean

      # vector_gx <- as.data.frame(v_final[,x])
      # names(vector_gx) <- names_data[x]
      # vector_gx <- -vector_gx
      # vector_gy <- as.data.frame(v_final[,y])
      # names(vector_gy) <- names_data[y]
      # end new

      # baseline_x <- vector_gx
      # baseline_y <- vector_gy
      # baseline_x <- as.data.frame(t(apply(as.matrix(data[,x]), 2, mean)))
      # names(baseline_x) <- names_data[x]
      # baseline_y <- as.data.frame(t(apply(as.matrix(data[,y]), 2, mean)))
      # names(baseline_y) <- names_data[y]

    } else if (directional_vector[["baseline"]] == "median") {
stop("Not available.")
      # new
      L2_nrom_mean <- sqrt(sum(colMeans(data[, c(x,y)])^2))

      # normalize
      v <- colMeans(data[, c(x,y)])

      v_w <- v * directional_vector[["relative_importance"]]

      # noralize
      L2_nrom_vector <- sqrt(sum(v_w[, c(x,y)]^2))
      v_final <- (v_w / L2_nrom_vector) * L2_nrom_mean

      # # directional vector
      # vector_gx <- as.data.frame(t(v_final[,x]))
      # names(vector_gx) <- names_data[x]
      # vector_gx <- -vector_gx
      # vector_gy <- as.data.frame(t(v_final[,y]))
      # names(vector_gy) <- names_data[y]
      # end new

      # baseline_x <- as.data.frame(t(apply(as.matrix(data[,x]), 2, median)))
      # names(baseline_x) <- names_data[x]
      # baseline_y <- as.data.frame(t(apply(as.matrix(data[,y]), 2, median)))
      # names(baseline_y) <- names_data[y]

    } else if (directional_vector[["baseline"]] == "self") {
      stop("Not available.")
      baseline_x <- (data[,x])
      names(baseline_x) <- names_data[x]
      baseline_y <- (data[,y])
      names(baseline_y) <- names_data[y]

    } else if (directional_vector[["baseline"]] == "ones") {
      stop("Not available.")
      baseline_x <- as.data.frame(t(rep(1, NCOL(data[,x]))))
      names(baseline_x) <- names_data[x]
      baseline_y <- as.data.frame(t(rep(1, NCOL(data[,y]))))
      names(baseline_y) <- names_data[y]

    }

    # # directional vector
    # vector_gx <- as.data.frame(-(score_imp_x * baseline_x))
    # names(vector_gx) <- names_data[x]
    #
    # vector_gy <- as.data.frame(score_imp_y * baseline_y)
    # names(vector_gy) <- names_data[y]

    # directional vector
    if (nrow(v_final) > 1) {
      v_final <- t(v_final)
    }

    # directional vector
    vector_gx <- as.data.frame(t(v_final[,x]))
    names(vector_gx) <- names_data[x]
    vector_gx <- -vector_gx
    vector_gy <- as.data.frame(t(v_final[,y]))
    names(vector_gy) <- names_data[y]

  } else {

    # norm
    if (directional_vector[["baseline"]] == "mean") {

      L2_nrom_mean <- sqrt(sum(colMeans(data[, c(x,y)])^2))

      # normalize
      v <- (data[, c(x,y)]/sqrt(rowSums(data[, c(x,y)]^2)))*L2_nrom_mean

      v_w <- v * directional_vector[["relative_importance"]]

      # normalize
      L2_nrom_vector <- sqrt(rowSums(v_w[, c(x,y)]^2))
      v_final <- (v_w / L2_nrom_vector) * L2_nrom_mean


    } else if (directional_vector[["baseline"]] == "self") {

      L2_nrom_self <- sqrt(sum(data[, c(x,y)]^2))

      # normalize
      w <- directional_vector[["relative_importance"]]
      w <- w * data[, c(x,y)]
      L2_nrom_w <- sqrt(rowSums(w^2))

      w_nrom <- w/L2_nrom_w
      sqrt(rowSums(w_nrom^2))

      v_final <- w_nrom*L2_nrom_self

      # # directional vector
      # vector_gx <- as.data.frame(v_final[,x])
      # names(vector_gx) <- names_data[x]
      # vector_gy <- as.data.frame(v_final[,y])
      # names(vector_gy) <- names_data[y]

    } else if (directional_vector[["baseline"]] == "ones") {

        L2_nrom_one <- sqrt(sum(rep(1, length(c(x,y)))^2))

        # normalize
        w <- directional_vector[["relative_importance"]]
        L2_nrom_w <- sqrt(rowSums(directional_vector[["relative_importance"]]^2))

        w_nrom <- w/L2_nrom_w
        sqrt(rowSums(w_nrom^2))

        v_final <- w_nrom*L2_nrom_one

        # # directional vector
        # vector_gx <- as.data.frame(v_final[,x])
        # names(vector_gx) <- names_data[x]
        #
        # vector_gy <- as.data.frame(v_final[,y])
        # names(vector_gy) <- names_data[y]

    }

    # directional vector
    vector_gx <- as.data.frame(v_final[,x])
    names(vector_gx) <- names_data[x]
    vector_gx <- -vector_gx

    vector_gy <- as.data.frame(v_final[,y])
    names(vector_gy) <- names_data[y]

  } # end local directional vector


  # ----------------------------------------------------------------------------
  # Determining the max beta  --------------------------------------------------
  # ----------------------------------------------------------------------------

  # find the first approximation of max beta for max(efficiency_thresholds)
  find_beta_maxmin <- find_beta_maxmin(
    data = data,
    x = x,
    y = y,
    final_model = final_model,
    calibration_model = calibration_model,
    efficiency_thresholds = efficiency_thresholds,
    n_expand = n_expand,
    vector_gx = vector_gx,
    vector_gy = vector_gy,
    max_y = max_y,
    min_x = min_x
  )

  result_thresholds <- vector("list", length = length(efficiency_thresholds))
  names(result_thresholds) <- as.character(efficiency_thresholds)

  # for each threshold
  for (thr in efficiency_thresholds) {

    message(paste0("In progress: ", thr))

    data <- data[, c(x,y)]

    # save data points from scenario
    data_scenario <- as.data.frame(
      matrix(
        data = NA,
        ncol = length(c(x,y)),
        nrow = nrow(data)
      )
    )
    names(data_scenario) <- names(data)

    betas <- as.data.frame(matrix(
      data = NA,
      ncol = 2,
      nrow = nrow(data)
    ))

    variables <- c(x, y)

    # loop for each observation
    for (i in 1:nrow(data)) {

      # inicial prediction
      prediction_0 <- PEAXAI_predict(
        data = data[i,variables],
        x = x,
        y = y,
        final_model = final_model,
        calibration_model = calibration_model
      )

      # the DMU is more efficient then threshold?
      if (prediction_0 > thr) {
        betas[i, 1] <- 0
        betas[i, 2] <- thr

        data_scenario[i,] <- data[i,]

      } else {

        # Inicializar el rango inicial de 'y'
        range_beta <- as.matrix(
          seq(from = find_beta_maxmin[i, "min"], # find_beta_maxmin[i, "min"]
              to = find_beta_maxmin[i, "max"],
              length.out = n_grid))

        # Crear la matriz para aplicar predict()
        changes <- as.data.frame(matrix(
          data = NA,
          ncol = length(variables),
          nrow = length(range_beta)
        ))

        # Nombrar las columnas como en data original
        names(changes) <- names(data)

        if(directional_vector[["baseline"]] != "self" & directional_vector[["scope"]] != "local") {

          change_x <- matrix(
            data = rep(as.numeric(vector_gx), each = nrow(changes)),
            nrow = nrow(changes),
            ncol = length(vector_gx)
          )

          change_y <- matrix(
            data = rep(as.numeric(vector_gy), each = nrow(changes)),
            nrow = nrow(changes),
            ncol = length(vector_gy)
          )

        } else {

          change_x <- matrix(
            data = unlist(vector_gx[i,]),
            nrow = nrow(changes),
            ncol = length(vector_gx),
            byrow = TRUE
          )

          change_y <- matrix(
            data = unlist(vector_gy[i,]),
            nrow = nrow(changes),
            ncol = length(vector_gy)
          )

        }

        found_cut_off <- FALSE
        iter_count <- 0

        while (!found_cut_off) {

          iter_count <- iter_count + 1

          # matrix to apply changes
          matrix_eff <- as.data.frame(matrix(
            data = NA,
            ncol = length(variables),
            nrow = length(range_beta)
          ))
          names(matrix_eff) <- names(data)

          # Asignar valores para 'x' y 'y'
          matrix_eff[, x] <- data[i,x]
          matrix_eff[, x] <- sweep(change_x, 1, range_beta, "*") + matrix_eff[, x]

          matrix_eff[, y] <- data[i, y]
          matrix_eff[, y] <- sweep(change_y, 1, range_beta, "*") + matrix_eff[, y]

          # know if there are not possible values
          mx <- as.matrix(matrix_eff[, x, drop = FALSE])
          min_x_possible_vector <- as.numeric(min_x_possible*min_x)

          my <- as.matrix(matrix_eff[, y, drop = FALSE])
          max_y_possible_vector <- as.numeric(max_y_possible*max_y)


          # sums how many TRUE lines are violating the restriction
          viol <- rowSums(mx < rep(min_x_possible_vector, each = nrow(mx))) > 0
          idx_viol_x <- which(viol)
          viol <- rowSums(my > rep(max_y_possible_vector, each = nrow(my))) > 0
          idx_viol_y <- which(viol)

          viol <- rep(FALSE, nrow(mx))
          viol[idx_viol_x] <- TRUE
          viol[idx_viol_y] <- TRUE

          keep <- !viol
          if (any(!keep)) {
            matrix_eff <- matrix_eff[keep, , drop = FALSE]
            range_beta <- range_beta[keep, , drop = FALSE]
          }

          # probability for each row
          eff_vector <- apply(matrix_eff, 1, function(row) {

            row_df <- as.data.frame(t(row))
            colnames(row_df) <- names(data)

            # pred <- unlist(predict(final_model, row_df, type = "prob")[1])
            pred <- PEAXAI_predict(
              data = row_df,
              x = x,
              y = y,
              final_model = final_model,
              calibration_model = calibration_model
            )

            return(pred)

          })

          # # Ensures that each position is at least the maximum value observed up to that point
          eff_vector <- cummax(eff_vector)

          # no changes case: min == max?
          if(round(eff_vector[1], precision_prob) == round(eff_vector[length(eff_vector)], precision_prob)) {

            data_scenario[i, x] <- data[i,x]
            data_scenario[i, y] <- data[i,y]

            betas[i, 1] <- 0
            betas[i, 2] <- eff_vector[1]
            break
          }

          # some problems?
          if (length(eff_vector) == 0 | is.null(eff_vector)) {

            data_scenario[i, x] <- rep(NA, ncol(matrix_eff[,x]))
            data_scenario[i, y] <- rep(NA, ncol(matrix_eff[,y]))

            betas[i, 1] <- NA
            betas[i, 2] <- NA
            break

          } else {
            # If there are not problems...

            # Did any prediction equal the target threshold?
            if (any(round(eff_vector, precision_prob) == round(thr, precision_prob))) {

              # Take the first matching index
              idx <- which(round(eff_vector, precision_prob) == round(thr, precision_prob))[1]
              found_cut_off <- TRUE

              # Store the 'x' and 'y' values that match the threshold
              data_scenario[i, x] <- matrix_eff[idx, x]
              data_scenario[i, y] <- matrix_eff[idx, y]

              betas[i, 1] <- range_beta[idx]
              betas[i, 2] <- thr
              break

            } else {

              # Find the interval where the target threshold lies
              pos <- which(head(eff_vector, -1) < thr & tail(eff_vector, -1) > thr)

              if (length(pos) > 1) {
                pos <- pos[1]
              } else if (length(pos) == 0) {
                idx_max <- which(eff_vector == max(eff_vector))
                pos <- idx_max[1]
              }

              if (is.na(pos)) {
                browser()
                break
              }

              # if position is in the maximum beta
              if (pos == length(eff_vector)) {

                # no more probability to be efficient
                # save best results
                data_scenario[i, x] <- matrix_eff[pos, x]
                data_scenario[i, y] <- matrix_eff[pos, y]

                betas[i, 1] <- range_beta[pos]

                pred <- as.data.frame(data_scenario[i,variables])
                names(pred) <- names(data)

                betas[i, 2] <- eff_vector[pos]
                break

              } else if (pos < 1) {
                browser()
                # if position is before the minimum beta
                pos <- 1

              }

              # Refine the 'y' interval between indices pos and pos + 1
              range_beta <- seq(from = range_beta[pos], to = range_beta[pos + 1], length.out = n_grid)

            }

          }

          if (range_beta[n_grid] - range_beta[1] <= 0.000001) {

            data_scenario[i, x] <- matrix_eff[(n_grid/2), x]
            data_scenario[i, y] <- matrix_eff[(n_grid/2), y]

            betas[i, 1] <- range_beta[(n_grid/2)]
            betas[i, 2] <- eff_vector[(n_grid/2)]

            # print("end while by dif")
            found_cut_off <- TRUE

          }

          if (iter_count == 20) {

            data_scenario[i, x] <- matrix_eff[(n_grid/2), x]
            data_scenario[i, y] <- matrix_eff[(n_grid/2), y]

            betas[i, 1] <- range_beta[(n_grid/2)]
            betas[i, 2] <- NA

            # print("end while by iter")
            found_cut_off <- TRUE

          }

        } # end while

      }

    } # end for

    names(betas) <- c("betas", "probability")

    result_thresholds[[as.character(thr)]][["counterfactual_dataset"]] <- data_scenario
    result_thresholds[[as.character(thr)]][["inefficiencies"]] <- betas
  }

  return(result_thresholds)

}

#' @title Search Range for Directional Efficiency Parameter (\eqn{\beta})
#'
#' @description
#' Estimates, for each observation, the minimum and maximum feasible values of the
#' directional distance parameter \eqn{\beta} used in projection-based efficiency
#' analysis. This function is an internal step of \code{\link{PEAXAI_targets}},
#' providing the initial search bounds for the iterative determination of efficiency targets.
#'
#' @param data A \code{data.frame} or \code{matrix} containing input and output variables.
#' @param x A numeric vector with the column indexes of input variables in \code{data}.
#' @param y A numeric vector with the column indexes of output variables in \code{data}.
#' @param final_model A fitted \pkg{caret} model of class \code{"train"} that supports
#'   \code{predict(type = "prob")} and returns a probability column for the efficient class.
#' @param calibration_model A model to calibrate.
#' @param efficiency_thresholds A numeric vector of probability levels in (0,1).
#'   Its minimum and maximum values delimit the target interval used to bracket \eqn{\beta}.
#' @param n_expand Integer. Increment step size applied to \eqn{\beta} at each iteration.
#' @param vector_gx A numeric vector or \code{data.frame} with directional changes for inputs
#'   (typically negative direction), usually built inside \code{PEAXAI_targets}.
#' @param vector_gy A numeric vector or \code{data.frame} with directional changes for outputs
#'   (positive direction).
#' @param max_y Numeric. Upper-limit multiplier for output expansion relative to observed maxima.
#' @param min_x Numeric. Lower-limit multiplier for input contraction relative to observed minima.
#'
#' @details
#' For each DMU, the function expands outputs and contracts inputs along the specified
#' direction until the predicted probability of efficiency (from \code{final_model})
#' reaches the maximum in \code{efficiency_thresholds} or feasible domain limits.
#' The resulting interval \eqn{[\beta_{\min}, \beta_{\max}]} is then used by
#' \code{\link{PEAXAI_targets}} to refine projections via grid search.
#'
#' @return
#' A \code{data.frame} with two numeric columns:
#' \describe{
#'   \item{\code{min}}{Minimum feasible value of \eqn{\beta} for each observation.}
#'   \item{\code{max}}{Maximum feasible value of \eqn{\beta} for each observation.}
#' }
#'
#' @seealso
#' \code{\link{PEAXAI_targets}} (efficiency projections based on \eqn{\beta});
#' \code{\link[caret]{train}} (model training with class probabilities).
#'
#' @export
#'

find_beta_maxmin <- function(
  data, x, y, final_model, calibration_model,
  efficiency_thresholds, n_expand, vector_gx,
  vector_gy, max_y, min_x
) {

  betas <- as.data.frame(matrix(
    data = NA,
    ncol = 2,
    nrow = nrow(data)
  ))

  variables <- c(x,y)

  max_efficiency_threshold <- max(efficiency_thresholds)
  min_efficiency_threshold <- min(efficiency_thresholds)

  # for each DMU, it will be calculated the max beta possible
  for (i in 1:nrow(data)) {

    # prediction_0 <- predict(final_model, data[i,variables], type = "prob")[1]
    prediction_0 <- PEAXAI_predict(
      data = data[i,variables],
      x = x,
      y = y,
      final_model = final_model,
      calibration_model = calibration_model
    )

    # the DMU is more efficient then threshold?
    if (prediction_0 > max_efficiency_threshold) {
      betas[i, 1] <- 0
      betas[i, 2] <- 0

    } else {

      control_threshold <- FALSE
      control_threshold_min <- FALSE

      names_data <- names(data[, c(x,y)])

      # control. Not min or max than observed resources
      min_x_i <- apply(as.matrix(data[,x]), 2, min) * min_x
      min_x_i <- as.data.frame(t(as.matrix(min_x_i)))
      names(min_x_i) <- names_data[x]

      max_y_i <- apply(as.matrix(data[,y]), 2, max) * max_y
      max_y_i <- as.data.frame(t(as.matrix(max_y_i)))
      names(max_y_i) <- names_data[y]

      beta_j <- 0

      betas[i,1] <- NA

      prediction_j_max <- prediction_0

      while(control_threshold == FALSE) {

        beta_j <- beta_j + n_expand

        if (nrow(vector_gx) > 1) {
          new_x <- data[i,x] + beta_j * vector_gx[i,]
          new_y <- data[i,y] + beta_j * vector_gy[i,]
        } else {
          new_x <- data[i,x] + beta_j * vector_gx
          new_y <- data[i,y] + beta_j * vector_gy
        }

        new_point <- cbind(new_x, new_y)
        names(new_point) <- names(data[,variables])

        # prediction_j <- predict(final_model, new_point, type = "prob")[1]
        prediction_j <- PEAXAI_predict(
          data = new_point,
          x = x,
          y = y,
          final_model = final_model,
          calibration_model = calibration_model
        )

        # Check if the probability has decreased. The probability function should be monotonic.
        if (prediction_j < prediction_j_max) {
          prediction_j <- prediction_j_max
        } else {
          prediction_j_max <- prediction_j
        }

        if (control_threshold_min == FALSE & prediction_j > min_efficiency_threshold) {

          betas[i, 1] <- beta_j - n_expand
          control_threshold_min <- TRUE

        }

        if(prediction_j >= max_efficiency_threshold) {

          # break the while loop
          betas[i, 2] <- beta_j
          control_threshold <- TRUE

        } else if (any(new_point[x] < min_x_i) || any(new_point[y] > max_y_i)) {
          # break the while loop
          betas[i, 2] <- beta_j
          control_threshold <- TRUE
        }


      }

      if (is.na(betas[i,1])) {
        betas[i, 1] <- beta_j - n_expand
      }

    }

  }

  names(betas) <- c("min", "max")

  return(betas)
}
