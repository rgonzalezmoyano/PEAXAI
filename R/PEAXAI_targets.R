#' @title Projections to the Hyperplane
#'
#' @description This function computes the efficiency scores based on a given model.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param z Column indexes of environment variables in \code{data}.
#' @param final_model The best-fitted model used for efficiency score computation.
#' @param efficiency_thresholds Probability levels for determining efficient class scores.
#' @param directional_vector A \code{data.frame} with importance variables results
#' @param n_expand n_expand
#' @param n_grid n_grid
#' @param max_y fd max_y
#' @param min_x fd min_x
#'
#' @return A numeric vector containing the efficiency scores for each observation in the input data.
#'
#' @export

PEAXAI_targets <- function (
    data, x, y, final_model, efficiency_thresholds,
    directional_vector, n_expand, n_grid, max_y = 2, min_x = 1
) {

  # ----------------------------------------------------------------------------
  # Input validation: warnings & errors ----------------------------------------
  # ----------------------------------------------------------------------------
  # data
  if (is.matrix(data)) {
    warning("`data` is a matrix; converting to data.frame.", call. = FALSE)
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  } else if (!is.data.frame(data)) {
    stop("Argument `data` must be a data.frame or a matrix.", call. = FALSE)
  }

  # variables
  # Must be numeric
  if (!(is.numeric(x) && is.numeric(y))) {
    stop("Arguments `x` and `y` must be numeric indices corresponding to columns in `data`.", call. = FALSE)
  }

  # Must be within range
  if (any(x < 1 | x > ncol(data)) || any(y < 1 | y > ncol(data))) {
    stop("Indices `x` and/or `y` are out of range for the dataset.", call. = FALSE)
  }

  # Must not overlap
  if (any(x %in% y)) {
    stop("Indices in `x` and `y` must refer to different columns in `data`.", call. = FALSE)
  }

  # final_model
  if (!(inherits(final_model, "train") ||
        (inherits(final_model, "glm") && family(final_model)$family == "binomial"))) {
    stop("`final_model` must be either a caret model (`train`) or a logistic regression (`glm` with family = 'binomial').",
         call. = FALSE)
  }

  # efficiency_thresholds
  if (!is.numeric(efficiency_thresholds) || !is.null(dim(efficiency_thresholds))) {
    stop("`efficiency_thresholds` must be a numeric vector.", call. = FALSE)
  }

  if (length(efficiency_thresholds) == 0L) {
    stop("`efficiency_thresholds` cannot be empty.", call. = FALSE)
  }

  if (any(!is.finite(efficiency_thresholds))) {
    stop("`efficiency_thresholds` must contain only finite values (no NA/NaN/Inf).", call. = FALSE)
  }

  if (!all(efficiency_thresholds < 1)) {
    stop("All values in `efficiency_thresholds` must be strictly less than 1.", call. = FALSE)
  }

  # directional_vector
  if (!is.list(directional_vector)) {
    stop("`directional_vector` must be a list.", call. = FALSE)
  }

  if(abs(sum(directional_vector[["relative_importance"]]) - 1) > 0.001) {
    stop("Sum of importances must be 1. Change to relative importances.")
  }

  data <- as.data.frame(data)
  # save

  names_data <- names(data[,c(x,y)])

  # min and max values
  min_x_possible <- apply(as.matrix(data[,x]), 2, min)

  # number of decimals to round
  precision_prob <- 5

  # ----------------------------------------------------------------------------
  # Build vector G (directional vector) ----------------------------------------
  # ----------------------------------------------------------------------------
  if (directional_vector[["scope"]] == "global") {

    # relative importance
    score_imp_x <- as.numeric(directional_vector[["relative_importance"]][x])
    score_imp_y <- as.numeric(directional_vector[["relative_importance"]][y])

    # baseline
    if (directional_vector[["baseline"]] == "mean") {

      baseline_x <- as.data.frame(t(apply(as.matrix(data[,x]), 2, mean)))
      names(baseline_x) <- names_data[x]
      baseline_y <- as.data.frame(t(apply(as.matrix(data[,y]), 2, mean)))
      names(baseline_y) <- names_data[y]

    } else if (directional_vector[["baseline"]] == "median") {

      baseline_x <- as.data.frame(t(apply(as.matrix(data[,x]), 2, median)))
      names(baseline_x) <- names_data[x]
      baseline_y <- as.data.frame(t(apply(as.matrix(data[,y]), 2, median)))
      names(baseline_y) <- names_data[y]

    } else if (directional_vector[["baseline"]] == "self") {

      baseline_x <- (data[,x])
      names(baseline_x) <- names_data[x]
      baseline_y <- (data[,y])
      names(baseline_y) <- names_data[y]

    } else if (directional_vector[["baseline"]] == "ones") {

      baseline_x <- as.data.frame(t(rep(1, ncol(data[,x]))))
      names(baseline_x) <- names_data[x]
      baseline_y <- as.data.frame(t(rep(1, ncol(data[,y]))))
      names(baseline_y) <- names_data[y]

    }

    # directional vector
    vector_gx <- as.data.frame(-(score_imp_x * baseline_x))
    names(vector_gx) <- names_data[x]

    vector_gy <- as.data.frame(score_imp_y * baseline_y)
    names(vector_gy) <- names_data[y]

  } else if (directional_vector[["scope"]] == "local") {

  }

  # ----------------------------------------------------------------------------
  # Determining the max beta  --------------------------------------------------
  # ----------------------------------------------------------------------------
  # find the first approximation of max beta for max(efficiency_thresholds)
  find_beta_maxmin <- find_beta_maxmin(
    data = data,
    x = x,
    y = y,
    final_model = final_model,
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

      # message(paste0("In progress: ", (round(i / nrow(data), 4) * 100), "%"))

      if (inherits(final_model, "train")) {
        prediction_0 <- predict(final_model, data[i,variables], type = "prob")[1]
      } else if (inherits(final_model, "glm")) {
        prediction_0 <- predict(final_model, data[i,variables], type = "response")[1]
      }

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

        if(directional_vector[["baseline"]] != "self") {

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
          browser()
          change_x <- matrix(
            data = rep(vector_gx[i,], each =  nrow(changes)),
            nrow = nrow(changes),
            ncol = length(vector_gx)
          )

          change_y <- matrix(
            data = rep((-data[i,y]) * vector_gy, each =  nrow(matrix_eff)),
            nrow = nrow(matrix_eff),
            ncol = length(mean_y)
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
# dfdfdf
          # know if there are not possible values
          mx <- as.matrix(matrix_eff[, x, drop = FALSE])
          min_x_possible_vector <- as.numeric(min_x_possible*min_x)

          # sums how many TRUE lines are violating the restriction
          viol <- rowSums(mx < rep(min_x_possible_vector, each = nrow(mx))) > 0

          keep <- !viol
          if (any(!keep)) {
            matrix_eff <- matrix_eff[keep, , drop = FALSE]
            range_beta <- range_beta[keep, , drop = FALSE]
          }

          # probability for each row
          eff_vector <- apply(matrix_eff, 1, function(row) {

            row_df <- as.data.frame(t(row))
            colnames(row_df) <- names(data)

            if (inherits(final_model, "train")) {
              pred <- unlist(predict(final_model, row_df, type = "prob")[1])
            } else if (inherits(final_model, "glm")) {
              pred <- unlist(predict(final_model, row_df, type = "response")[1])
            }

            return(pred)
          })

          # # Ensures that each position is at least the maximum value observed up to that point
          eff_vector <- cummax(eff_vector)
          # if (i == 20) browser()
          #
          # plot(eff_vector)
          # prob_vector <- eff_vector
          # print(find_beta_maxmin[i,])
          # prob_vector <- as.data.frame(prob_vector)
          # prob_vector$id <- 1:nrow(prob_vector)
          # library(ggplot2)
          # print(ggplot(data = prob_vector) +
          #   geom_line(aes(x = id, y = prob_vector)) +
          #   coord_cartesian(ylim = c(0, 1)) +
          #   theme_minimal())

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
                pos <- 0
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

                if (inherits(final_model, "train")) {
                  pred_max <- unlist(predict(final_model, row_df, type = "prob")[1])
                } else if (inherits(final_model, "glm")) {
                  pred_max <- unlist(predict(final_model, row_df, type = "response")[1])
                }

                betas[i, 2] <- pred_max
                break

              } else if (pos < 1) {

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

            data_scenario[i, x] <- matrix_eff[(length_betas/2), x]
            data_scenario[i, y] <- matrix_eff[(length_betas/2), y]

            betas[i, 1] <- range_beta[(length_betas/2)]
            betas[i, 2] <- NA

            # print("end while by iter")
            found_cut_off <- TRUE

          }

        } # end while

      }

    } # end for

    names(betas) <- c("beta", "probability")

    result_thresholds[[as.character(thr)]][["data"]] <- data_scenario
    result_thresholds[[as.character(thr)]][["beta"]] <- betas
  }

  # data_scenario <- cbind(data_scenario, betas)

  return(result_thresholds)

}

#' @title Projections to the Hyperplane
#'
#' @description This function computes the efficiency scores based on a given model.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param final_model The best-fitted model used for efficiency score computation.
#' @param efficiency_thresholds Probability levels for determining efficient class scores.
#' @param n_expand n_expand
#' @param directional_vector A \code{data.frame} with importance variables results
#' @param vector_gx dfd
#' @param vector_gy dfdfd
#' @param max_y fd max_y
#' @param min_x fd min_x
#'
#' @return A numeric vector containing the efficiency scores for each observation in the input data.
#'
#' @export

find_beta_maxmin <- function(
  data, x, y, final_model, efficiency_thresholds,
  n_expand, vector_gx, vector_gy, max_y, min_x
) {

  betas <- as.data.frame(matrix(
    data = NA,
    ncol = 2,
    nrow = nrow(data)
  ))

  variables <- c(x,y)

  # if (nrow(vector_gx) > 1) {
  #
  # } else {
    max_efficiency_threshold <- max(efficiency_thresholds)
    min_efficiency_threshold <- min(efficiency_thresholds)
  # }

  # for each DMU, it will be calculated the max beta possible
  for (i in 1:nrow(data)) {

    if (inherits(final_model, "train")) {
      prediction_0 <- predict(final_model, data[i,variables], type = "prob")[1]
    } else if (inherits(final_model, "glm")) {
      prediction_0 <- predict(final_model, data[i,variables], type = "response")[1]
    }

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

        if (inherits(final_model, "train")) {
          prediction_j <- predict(final_model, new_point, type = "prob")[1]
        } else if (inherits(final_model, "glm")) {
          prediction_j <- predict(final_model, new_point, type = "response")[1]
        }

        # Check if the probability has decreased. The probability function should be monotonic.
        if (prediction_j < prediction_j_max) {
          prediction_j <- prediction_j_max
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
