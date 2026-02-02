#' @title Create New SMOTE Units to Balance Data combinations of m + s
#'
#' @description This function creates new DMUs to address data imbalances.
#' If the majority class is efficient, it generates new inefficient DMUs by worsering the observed units.
#' Conversely, if the majority class is inefficient, it projects inefficient DMUs to the frontier.
#' Finally, a random selection if performed to keep a proportion of 0.65 for the majority class and 0.35 for the minority class.
#'
#' @param data A \code{list} of \code{data.frames}, where each element represents a dataset with labeled data.
#' @param facets A \code{list} where each element represents a subgroup containing index combinations that generate efficient units.
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
#' @param seed  Integer. Seed for reproducibility.
#'
#' @importFrom dplyr anti_join
#' @importFrom stats quantile
#'
#' @return A \code{list} where each element corresponds to a balance level, containing a single \code{data.frame}
#' with the real and synthetic DMUs, correctly labeled.

get_SMOTE_DMUs <- function (
    data, facets, x, y, RTS = "vrs", balance_data = NULL, seed
) {

  # save a copy
  copy_data <- data

  save_all_datasets_balanced <- vector("list", length(balance_data))
  names(save_all_datasets_balanced) <- as.character(balance_data)

  # reproducibility.
  set.seed(seed)

  # we need to determine, for each balance level, the number of synthetic DMUs to create
  for (balance in balance_data) {

    save_dataset <- as.data.frame(matrix(
      data = NA,
      ncol = ncol(copy_data),
      nrow = 0
    ))

    names(save_dataset) <- names(copy_data)

    # check if it is possible to balance
    if(nrow(facets) == 0) {
      warning("No facets found; could not apply class balancing.")

      save_dataset <- rbind(save_dataset, data)

      # join real data + SMOTE
      new_data_completed <- save_dataset

      # save
      save_all_datasets_balanced[[as.character(balance)]] <- new_data_completed
      browser()
      browser()
      next

    }

    # --------------------------------------------------------------------------
    # determinate number of efficient and not efficient to create --------------
    # --------------------------------------------------------------------------
    # information balancing process
    message(paste0("Balancing: ", balance))

    # determinate numbre of efficient and ineeficient units
    n_real_eff <- nrow(data[data$class_efficiency == "efficient",])
    n_real_ineff <- nrow(data[data$class_efficiency == "not_efficient",])

    prop_real <- n_real_eff / nrow(data)

    n_new_eff <- 0

    n_new_ineff <- 0

    # proportion of efficients
    prop <- prop_real

    sense_balance <- NULL

    # determinate the way to balance, create efficient or not efficient
    if (prop < balance) {

      # need to create efficient units
      sense_balance <- "efficient"

      # in each itaretion we create these DMUs
      add_eff <- 1
      add_not_eff <- 0

    } else {

      # need to create not efficient units
      sense_balance <- "not_efficient"

      # in each itaretion we create these DMUs
      add_eff <- 0
      add_not_eff <- 1

    }

    # determinate how many DMUs create PROPORTION
    eff_level <- balance

    test_n_eff <- n_real_eff
    test_n_ineff <- n_real_ineff

    if (sense_balance == "not_efficient") {

      while (prop > eff_level) {

        test_n_ineff <- test_n_ineff + add_not_eff

        prop <- test_n_eff / (test_n_eff + test_n_ineff)

      }

    } else {

      while (prop < eff_level) {

        test_n_eff <- test_n_eff + add_eff

        prop <- test_n_eff / (test_n_eff + test_n_ineff)
      }

    }

    # it is necessary to create create_eff units
    create_eff <- test_n_eff - n_real_eff

    # it is necessary to create create_ineff units
    create_ineff <- test_n_ineff - n_real_ineff

    # balance perfect, next
    if (create_eff == 0 & create_ineff == 0) {

      message(paste("Balance perfect"))

      save_dataset <- rbind(save_dataset, data)

      # join real data + SMOTE
      new_data_completed <- save_dataset

      # save
      save_all_datasets_balanced[[as.character(balance)]] <- new_data_completed

      next

    }

    # ============================================ #
    # get index to create efficient synthetic DMUs #
    # ============================================ #
    data_eff <- data[data$class_efficiency == "efficient", ]

    # real efficient combination
    idx <- facets
    n_idx <- 1:nrow(idx)

    # number of efficient DMUs
    n_eff <- nrow(data_eff)

    # create units
    # lambda

    # proportion importance
    len <- ncol(facets)

    prop_imp <- 1/len

    lambda <- rep(prop_imp, ncol(facets))

    n_comb <- nrow(data_eff)

    if (sense_balance == "not_efficient") {
      print(sense_balance)

      # # number of not efficient units to create, more than it is necessary
      new_create_ineff <- 4 * create_ineff

      while (nrow(save_dataset) < new_create_ineff) {

        idx_eff <- which(data$class_efficiency == "efficient")

        # first, select a random index and combination
        # number of dimensions
        n_combinations <- ncol(facets)

        random_convex <- sample(idx_eff, size = n_combinations, replace = FALSE)
        selection <- data[unlist(as.vector(random_convex)), c(x,y)]

        # second, determine random weights by DMU
        # process to generate lambda
        generate_lambda <- runif(length(c(x,y)), min = 0.01, max = 0.99)

        normalize_lambda <- generate_lambda/sum(generate_lambda)

        # third, generate the synthetic unit
        new_unit <- colSums(selection * normalize_lambda)
        new_unit <- as.data.frame(t(new_unit))

        # check efficiency
        check_results_convx <- dea.add(
          X = as.matrix(new_unit[,x]),
          Y = as.matrix(new_unit[,y]),
          XREF = as.matrix(data_eff[,x]),
          YREF = as.matrix(data_eff[,y]),
          RTS = RTS
        )[["sum"]] > 0.0001

        # save if the DMU is not_efficient
        if (check_results_convx == TRUE) {

          new_unit$class_efficiency <- "not_efficient"
          save_dataset <- rbind(save_dataset, new_unit)

        } # end check

      } # end while

      # order by score in innefficieny DEA
      check_results_convx <- dea.add(
        X = as.matrix(save_dataset[,x]),
        Y = as.matrix(save_dataset[,y]),
        XREF = as.matrix(data_eff[,x]),
        YREF = as.matrix(data_eff[,y]),
        RTS = RTS
      )[["sum"]]

      # make quiantiles
      q_innef <- quantile(check_results_convx)

      # group_by
      quantiles <- cut(
        check_results_convx,
        breaks = q_innef,
        include.lowest = TRUE,
        labels = c("Q1", "Q2", "Q3", "Q4")
      )

      # choose the same sample by quantile
      need_by_quantile <- create_ineff/4

      copy_save_dataset <- save_dataset
      save_dataset <- save_dataset[0,]

      # sample of each quantile
      for (quantile_i in levels(quantiles)) {

        q_i <- copy_save_dataset[quantiles == quantile_i,]

        q_sample <- sample(1:nrow(q_i), size = need_by_quantile, replace = FALSE)

        save_dataset <- rbind(save_dataset, copy_save_dataset[q_sample,])
      }

    } else {
      print(sense_balance)
      # first, populate the middle point to ensure that all facets are populate
      results_convx <- t(apply(facets, 1, function(indices) {

        # select row
        seleccion <- data[unlist(as.vector(indices)), c(x,y)]

        # calculate
        colSums(seleccion * lambda)

      }))

      results_convx <- as.data.frame(results_convx)

      # too much efficient
      if (nrow(facets) > create_eff) {

        # select the index to save
        idx_save <- sample(nrow(facets), size = create_eff,  replace = FALSE)

        new_unit <- results_convx[idx_save,]

        # save DMUs selected
        new_unit$class_efficiency <- "efficient"
        save_dataset <- rbind(save_dataset, new_unit)

      } else {

        # save the previous SMOTE units generated
        results_convx$class_efficiency <- "efficient"
        save_dataset <- rbind(save_dataset, results_convx)

        # if not too much, is it necessary to create more SMOTE DMUs
        while (nrow(save_dataset) < create_eff) {

          # first, select a random index and combination
          idx_save <- sample(nrow(facets), size = 1)
          dmus_by_facet <- facets[idx_save,]
          selection <- data[unlist(as.vector(dmus_by_facet)), c(x,y)]

          # second, determine random weights by DMU
          # process to generate lambda
          generate_lambda <- runif(ncol(facets), min = 0.01, max = 0.99)

          normalize_lambda <- generate_lambda/sum(generate_lambda)

          # third, generate the synthetic unit
          new_unit <- colSums(selection * normalize_lambda)
          new_unit <- as.data.frame(t(new_unit))

          # fourth, check if the new DMU is efficient
          check_results_convx <- dea.add(
            X = as.matrix(new_unit[,x]),
            Y = as.matrix(new_unit[,y]),
            XREF = as.matrix(data_eff[,x]),
            YREF = as.matrix(data_eff[,y]),
            RTS = RTS
          )[["sum"]] < 0.0001

          # five, save if everything is correct
          if (check_results_convx == TRUE) {

            new_unit$class_efficiency <- "efficient"
            save_dataset <- rbind(save_dataset, new_unit)

          } # end check

        } # end while search

      } # end necessary to populate more

    } # end generating SMOTE

    # join real data + SMOTE
    new_data_completed <- rbind(data, save_dataset)

    # save
    save_all_datasets_balanced[[as.character(balance)]] <- new_data_completed

  } # end balance_data loop

  return(save_all_datasets_balanced)
}
