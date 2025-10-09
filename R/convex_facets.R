#' @title Create New SMOTE Units to Balance Data combinations of m + s
#'
#' @description This function creates new DMUs to address data imbalances. If the majority class is efficient, it generates new inefficient DMUs by worsering the observed units. Conversely, if the majority class is inefficient, it projects inefficient DMUs to the frontier. Finally, a random selection if performed to keep a proportion of 0.65 for the majority class and 0.35 for the minority class.
#'
#' @param data A \code{data.frame} containing the variables used in the model.
#' @param x Column indexes of the input variables in the \code{data}.
#' @param y Column indexes of the output variables in the \code{data}.
#' @param RTS Text string or number defining the underlying DEA technology /
#'   returns-to-scale assumption (default: \code{"vrs"}). Accepted values:
#'   \describe{
#'     \item{\code{0} / \code{"fdh"}}{Free disposability hull, no convexity assumption.}
#'     \item{\code{1} / \code{"vrs"}}{Variable returns to scale, convexity and free disposability.}
#'     \item{\code{2} / \code{"drs"}}{Decreasing returns to scale, convexity, down-scaling and free disposability.}
#'     \item{\code{3} / \code{"crs"}}{Constant returns to scale, convexity and free disposability.}
#'     \item{\code{4} / \code{"irs"}}{Increasing returns to scale (up-scaling, not down-scaling), convexity and free disposability.}
#'     \item{\code{5} / \code{"add"}}{Additivity (scaling up and down, but only with integers), and free disposability.}
#'   }
#' @param balance_data A numeric vector indicating the different levels of balance required (e.g., c(0.1, 0.45, 0.6)).
#'
#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.

convex_facets <- function (
    data, x, y, RTS = "vrs", balance_data = NULL
) {

  # save a copy
  copy_data <- data

  # determine samples in a batch
  units_batch <- 10000

  # first, create set combinations
  # determinate efficient untis
  data_eff <- data[data$class_efficiency == "efficient", ]

  # number of efficient units
  n_eff <- 1:nrow(data_eff)

  # proportion importance
  len <- length(c(x,y))

  # weight lambda
  prop_imp <- 1/len

  # create lambda
  lambda <- rep(prop_imp, ncol(data_eff[, c(x,y)]))

  # numer of combinations
  n_comb <- length(c(x,y))


  if (length(n_eff) >= n_comb) {

    combinations <- as.data.frame(t(combn(n_eff, n_comb)))

    # randomly shuffle the rows of combinations
    combinations <- combinations[sample(nrow(combinations)),]

    # save efficient combinations
    eff_convex <- as.data.frame(matrix(
      data = NA,
      ncol = ncol(data_eff[, c(x,y)]),
      nrow = 0
    ))

    # save not efficient
    ineff_convex <- as.data.frame(matrix(
      data = NA,
      ncol = ncol(data_eff[, c(x,y)]),
      nrow = 0
    ))

    # if there are a lot of combinations, it will be used a samples of 5000
    if(nrow(combinations) > units_batch) {

      # Partition size
      n_batch <- units_batch

      # shuffle the data
      shuffle_data <- sample(1:nrow(combinations))

      # Create an index for each partition
      combinations$particion <- ceiling(seq_along(shuffle_data) / n_batch)

      batch_all <- split(combinations[shuffle_data, ], combinations$particion)

      n_total_batch <- ceiling(nrow(combinations) / units_batch)

    } else {

      batch_all <- list()
      batch_all[[1]] <- combinations

      n_total_batch <- 1

    }

    # create convex combintaions
    print("calculate combinations points:")
    print(nrow(combinations))

    print("Number of batches:")
    print(n_total_batch)


    save_idx_eff <- NULL
    save_idx_ineff <- NULL

    # --------------------------------------------------------------------------
    # Determine the efficient combinations by Additive DEA ---------------------
    # --------------------------------------------------------------------------
    for (iter in 1:length(batch_all)) {

      if(nrow(eff_convex)/nrow(data) > max(balance_data)) {

        print(paste("There are too many convex facets: ", nrow(eff_convex), " in a dataset of :", nrow(data)))
        break
      }

      # Show the batch
      print(paste("Batch:", iter))

      # units to classify
      results_convx <- t(apply(batch_all[[iter]], 1, function(indices) { #[,c(x,y)]

        # select row
        seleccion <- data_eff[unlist(as.vector(indices))[1:length(c(x,y))], c(x,y)]

        # calculate
        colSums(seleccion * lambda)

      }))

      # change to dataframe
      results_convx <- as.data.frame(results_convx)

      # change name
      names(results_convx) <- names(data_eff[, c(x,y)])

      # test additive
      test_add <- dea.add(
        X = as.matrix(results_convx[,x]),
        Y = as.matrix(results_convx[,y]),
        XREF = as.matrix(data_eff[,x]),
        YREF = as.matrix(data_eff[,y]),
        RTS = RTS
      )[["sum"]]

      # index efficient combinations
      idx_eff <- which(test_add < 0.0001)

      if (length(idx_eff) == 0) {
        ineff_to_save <- results_convx
        next
      }

      idx_dmus_eff <- batch_all[[iter]][idx_eff, c(x,y)]

      # save efficient convex
      eff_convex <- rbind(eff_convex, idx_dmus_eff)

    }

    # change to dataframe
    results_convx <- as.data.frame(eff_convex)

  } else {

    # if there are less units than variables, we will reduce the dimensionality
    # save efficient combinations
    eff_convex <- as.data.frame(matrix(
      data = NA,
      ncol = ncol(data_eff[, c(x,y)]),
      nrow = 0
    ))

    # the same output
    results_convx <- as.data.frame(matrix(
      data = NA,
      ncol = ncol(data_eff[, c(x,y)]),
      nrow = 0
    ))

    iter <- 1
    n_total_batch <- 1

  }

  # ----------------------------------------------------------------------------
  # extreme case: there are not full-dimensional faces -------------------------
  # ----------------------------------------------------------------------------
  if (nrow(results_convx) == 0 & iter == n_total_batch) {

    iter_extreme <- 0

    while (nrow(eff_convex) == 0) {

      # count progress
      iter_extreme <- iter_extreme + 1

      # proportion importance
      len <- len - 1

      # save efficient combinations len-1
      eff_convex <- as.data.frame(matrix(
        data = NA,
        ncol = len,
        nrow = 0
      ))

      prop_imp <- 1/len

      lambda <- rep(prop_imp, len)

      n_comb <- len

      if(length(n_eff) < n_comb) next

      combinations <- as.data.frame(t(combn(n_eff, n_comb)))

      # randomly shuffle the rows of combinations
      combinations <- combinations[sample(nrow(combinations)),]

      if (len == 1) {

        # there are no feasible convex combiantions
        results_convx <- as.data.frame(eff_convex)
        break

      } else {

        if(nrow(combinations) > units_batch) {

          # Partition size
          n_batch <- units_batch

          # shuffle the data
          shuffle_data <- sample(1:nrow(combinations))

          # Create an index for each partition
          combinations$particion <- ceiling(seq_along(shuffle_data) / n_batch)

          batch_all <- split(combinations[shuffle_data, ], combinations$particion)

          n_total_batch <- ceiling(nrow(combinations) / units_batch)

        } else {

          batch_all <- list()
          batch_all[[1]] <- combinations

          n_total_batch <- 1

        }

        # units to classify
        results_convx <- t(apply(batch_all[[iter]], 1, function(indices) {

          # select row
          seleccion <- data_eff[unlist(as.vector(indices)), c(x,y)]

          # calculate
          colSums(seleccion * lambda)

        }))

        # change to dataframe
        results_convx <- as.data.frame(results_convx)

        # change name
        names(results_convx) <- names(data_eff[, c(x,y)])

        # test DEA add
        test_add <- dea.add(
          X = as.matrix(results_convx[,x]),
          Y = as.matrix(results_convx[,y]),
          XREF = as.matrix(data_eff[,x]),
          YREF = as.matrix(data_eff[,y]),
          RTS = RTS
        )[["sum"]]

        # leave original eff units, get index
        idx_eff <- which(test_add < 0.0001)

        # get efficient index
        idx_dmus_eff <- batch_all[[iter]][idx_eff, ]

        # save idx_eff
        eff_convex <- rbind(eff_convex, idx_dmus_eff)

        results_convx <- as.data.frame(eff_convex)

        new_eff_conx_unit <- new_results_convx[idx_eff, ]

      }

    } # len = 1

  } # end exreme cases

  return(results_convx)

}



