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
#'
#' @importFrom dplyr anti_join
#'
#' @return A \code{list} where each element corresponds to a balance level, containing a single \code{data.frame}
#' with the real and synthetic DMUs, correctly labeled.

get_SMOTE_DMUs <- function (
    data, facets, x, y, RTS = "vrs", balance_data = NULL
) {

  # save a copy
  copy_data <- data

  save_all_datasets_balanced <- vector("list", length(balance_data))
  names(save_all_datasets_balanced) <- as.character(balance_data)

  # we need to determine, for each balance level, the number of synthetic DMUs to create
  for (balance in balance_data) {

    save_dataset <- as.data.frame(matrix(
      data = NA,
      ncol = ncol(copy_data),
      nrow = 0
    ))

    names(save_dataset) <- names(copy_data[[1]])

    # check if it is possible to balance
    if(nrow(facets) == 0) {
      warning("No facets found; could not apply class balancing.")

      save_dataset <- rbind(save_dataset, data)
      next

    }

    # --------------------------------------------------------------------------
    # determinate number of efficient and not efficient to create --------------
    # --------------------------------------------------------------------------
    # information balancing process
    print(paste("Balance: ", balance))

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

      print(paste("Balance perfect"))

      save_dataset <- rbind(save_dataset, data)
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

    combinations <- as.data.frame(t(combn(n_comb, len)))

    if (sense_balance == "not_efficient") {

      # select  k-create_ineff
      if (nrow(combinations) > (create_ineff * 3)) {

        idx_combinations <- sample(x = 1:nrow(combinations), size = (create_ineff * 3), replace = FALSE)

        idx_ineff <- combinations[idx_combinations,]

        idx <- anti_join(idx_ineff, idx, by = names(idx))

        idx <- na.omit(idx)

      } else if ((nrow(combinations) - length(n_idx)) < create_ineff &
                   nrow(combinations) == length(n_idx)){

        print("No possible create not efficient units")

        save_dataset <- rbind(save_dataset, data)
        next

      } else if ((nrow(combinations) - length(n_idx)) < create_ineff &
                  nrow(combinations) > length(n_idx)) {

        print("No possible create not efficient units")

        save_dataset <- rbind(save_dataset, data)
        next

      }

    } # end not efficient case

    # units to classify
    results_convx <- t(apply(idx, 1, function(indices) {

      # select row
      seleccion <- data_eff[unlist(as.vector(indices)), c(x,y)]

      # calculate
      colSums(seleccion * lambda)

    }))

    # as data.frame
    results_convx <- as.data.frame(results_convx)

    # check all convex are efficient
    check_results_convx <- which(
      dea.add(
        X = as.matrix(results_convx[,x]),
        Y = as.matrix(results_convx[,y]),
        XREF = as.matrix(data_eff[,x]),
        YREF = as.matrix(data_eff[,y]),
        RTS = RTS
      )[["sum"]] < 0.0001)

    # save only efficient
    results_convx <- results_convx[check_results_convx, ]

    idx <- idx[check_results_convx, ]

    # if there are not enough efficient units, use
    if(sense_balance == "efficient" & nrow(results_convx) < create_eff) {

      # need to create
      need_eff <- create_eff - nrow(results_convx)

      # eff_combinations <- idx
      save_lambda_eff <- as.data.frame(matrix(
        data = NA,
        ncol = length(c(x,y)),
        nrow = 0
      ))

      # Second, I search new efficient combinations
      count_browser <- 0

      while (nrow(save_lambda_eff) < need_eff) {

        # count
        count_browser <- count_browser + 1

        print((nrow(save_lambda_eff)/need_eff)*100)

        # process to generate lambda
        generate_lambda <- runif(length(c(x, y)), min = 0.01, max = 0.99)

        normalize_lambda <- generate_lambda/sum(generate_lambda)

        # set lambda
        lambda_eff <- normalize_lambda

        # set combnation to make new unit
        idx_new_eff <- sample(1:nrow(idx), size = 1)
        selec_comb <- idx[idx_new_eff,]

        # units to classify
        seleccion <- data_eff[unlist(as.vector(selec_comb)), c(x,y)]

        # calculate
        new_unit <- colSums(seleccion * lambda_eff)
        # new_unit <- as.matrix((as.matrix(new_unit)))

        # check
        check_test <- dea.add(
          X = matrix(new_unit[x], nrow = 1,
                     dimnames = list(NULL, names(data_eff)[x])),
          Y = matrix(new_unit[y], nrow = 1,
                     dimnames = list(NULL, names(data_eff)[y])),
          XREF = as.matrix(data_eff[,x]),
          YREF = as.matrix(data_eff[,y]),
          RTS = RTS
        )[["sum"]]

        if (check_test < 0.0001) {

          # save if is correct
          save_lambda_eff <- rbind(save_lambda_eff, new_unit)
        }

      } # end loop while

      names(save_lambda_eff) <- names(results_convx)

      results_convx <- rbind(results_convx, save_lambda_eff)

    } else if (sense_balance == "efficient" & nrow(results_convx) >= create_eff) {

      # need to add
      need_eff <- nrow(results_convx) - create_eff

      if (need_eff != 0) {
        print("linea 360")

        new_idx <- sample(1:nrow(results_convx), size = need_eff)
      } else {
        print("linea 364")
        new_idx <- 1:nrow(results_convx)
      }

      results_convx <- results_convx[new_idx,]

    }

    new_data <- results_convx

    if(sense_balance == "not_efficient") {

      new_data$class_efficiency <- "not_efficient"

    } else {

      new_data$class_efficiency <- rep("efficient", nrow(results_convx))

    }

    # join real data + SMOTE
    new_data_completed <- rbind(data, new_data)

    # save
    save_all_datasets_balanced[[as.character(balance)]] <- new_data_completed

  } # end balance_data loop

  return(save_all_datasets_balanced)
}
