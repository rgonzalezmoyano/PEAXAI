#' @title Data preprocessing and efficiency labeling with Additive DEA
#'
#' @description
#' Labels each DMU (Decision Making Unit) as efficient or not using the
#' Additive DEA model, optionally after basic data preprocessing. The resulting
#' factor \code{class_efficiency} has levels \code{c("not_efficient","efficient")},
#' where \code{"efficient"} is the positive class for downstream modeling.
#'
#' @param data A \code{data.frame} or \code{matrix} containing all variables.
#' @param REF Optional reference set of inputs that defines the technology
#'   (defaults to the columns indicated by \code{x} in \code{data}). Must have
#'   the same number of rows as \code{data}.
#' @param x Integer vector with column indices of input variables in \code{data}.
#' @param y Integer vector with column indices of output variables in \code{data}.
#' @param z_numeric Integer vector with column indices of numeric environment variables in \code{data}. By default is \code{NULL}.
#' @param z_factor Integer vector with column indices of factor environment variables in \code{data}. By default is \code{NULL}.
#' @param B number of bootstrap replicates in Conditional DEA.
#' @param m number of units to be included in the reference set.
#' @param RTS Character or integer specifying the DEA technology / returns-to-scale
#'   assumption (default: \code{"vrs"}). Accepted values:
#'   \describe{
#'     \item{\code{0} / \code{"fdh"}}{Free disposability hull (no convexity).}
#'     \item{\code{1} / \code{"vrs"}}{Variable returns to scale (convexity + free disposability).}
#'     \item{\code{2} / \code{"drs"}}{Decreasing returns to scale (convexity, down-scaling, free disposability).}
#'     \item{\code{3} / \code{"crs"}}{Constant returns to scale (convexity + free disposability).}
#'     \item{\code{4} / \code{"irs"}}{Increasing returns to scale (up-scaling only, convexity + free disposability).}
#'     \item{\code{5} / \code{"add"}}{Additivity (integer up/down scaling) with free disposability.}
#'   }
#' @param bandwidth the bandwidth parameters for the unconditional kernel density estimator used in the conditional DEA framework. It is typically obtained using \code{\link[np]{npudensbw}} and supports mixed data types, including continuous variables and discrete unordered or ordered factors. Bandwidths can be selected using normal reference rules, likelihood cross-validation, or least-squares cross-validation following Li and Racine (2003). If \code{NULL}, the bandwidth is estimated internally.
#' @param seed  Integer. Seed for reproducibility.
#'
#' @details
#' Internally relies on \code{\link[Benchmarking]{dea.add}} to compute Additive DEA
#' scores and derive the binary efficiency label.
#'
#' @importFrom Benchmarking dea.add
#' @importFrom np npudensbw npudens
#'
#' @return
#' A \code{data.frame} equal to \code{data} (retaining all input \code{x} and
#' output \code{y} columns) plus a new factor column \code{class_efficiency}
#' with levels \code{c("not_efficient","efficient")}.
#'
#' @seealso \code{\link[Benchmarking]{dea.add}}
#'
#' @examples
#' # Example (assuming columns 1:2 are inputs and 3 is output):
#' # out <- my_fun(data = df, x = 1:2, y = 3, RTS = "vrs")
#' # table(out$class_efficiency)
#'
#' @export

label_efficiency <- function (
    data, REF = data, x, y, z_numeric = NULL, z_factor = NULL, RTS = "vrs",
    B = NULL, alpha = FALSE, m = NULL, bandwidth = NULL, seed
  ) {

  # check if parameters are well introduced
  validate_parametes_label_efficiency(
    data = data,
    x = x,
    y = y,
    RTS = RTS
  )

  if (is.null(z_numeric) & is.null(z_factor)) {
    # the additive DEA

    # benchmarking to calculate additive-DEA
    add_scores <- dea.add(
      X = as.matrix(data[,x]),
      Y = as.matrix(data[,y]),
      XREF = as.matrix(REF[,x]),
      YREF = as.matrix(REF[,y]),
      RTS = RTS
    )[["sum"]]

    # determine efficient and inefficient DMUs
    class_efficiency <- ifelse(add_scores <= 0.0001, 1, 0)

    # add new varaible
    data <- as.data.frame (
      cbind(data, class_efficiency)
    )

    # labels
    data$class_efficiency <- ifelse(
      data$class_efficiency == 1, "efficient", "not_efficient")

    # create the 'class_efficiency' in a factor
    data$class_efficiency <- factor(data$class_efficiency)

    # order of levels
    data$class_efficiency <- factor(
      data$class_efficiency,
      levels = c("efficient", "not_efficient"))

  } else {

    # conditional DEA

    # define preliminary variables:
    n <- nrow(data) # number of observations
    k <- NCOL(data[,c(y)]) # number of outputs
    s <- NCOL(data[,c(x)]) # number of inputs

    # initialize
    eff <- rep(0, n)
    DEA_B <- rep(0, B) # bootstrap

    if (alpha != FALSE) {
      ci_low <- rep(0, n)
      ci_up <- rep(0,n)
    }

    # 1 determine bandwidths
    message(c("R is now computing the bandwidth using the function npudensbw in the package np"))

    exogenous <- c(z_numeric, z_factor)

    # calculate similarity matrix
    if (is.null(bandwidth)) {
      bw <- npudensbw(dat = data[,exogenous])
    } else {
      bw <- bandwidth
    }

    # 2 similarity by unit
    message(c("R is now computing the conditional DEA"))

    set.seed(seed)
    for (i in 1:n) {

      # compute the similarity for each obs i
      kerz <- npudens(
        bws = bw,
        cykertype = "epanechnikov",
        cxkertype = "epanechnikov",
        tdat = data[i,exogenous, drop = FALSE],
        edat = data[,exogenous, drop = FALSE])

      similarity_i <- cbind(kerz$dens)

      # consider only the units that perform at least as good as unit i
      y_i <- data[i, y]
      x_i <- data[i, x]
      Y_Rob <- data[, y]
      X_Rob <- data[, x]
      similarity_Rob <- similarity_i

      # mod
      # Nueva condición: combinación de filtros input y output (dominancia fuerte)
      cond_output <- matrix(TRUE, nrow = nrow(Y_Rob), ncol = k)

      for (l in 1:k) {
        cond_output[, l] <- Y_Rob[, l] >= as.numeric(y_i[l])
      }

      cond_input <- matrix(TRUE, nrow = nrow(X_Rob), ncol = s)

      for (l in 1:s) {
        cond_input[, l] <- X_Rob[, l] <= as.numeric(x_i[l])
      }

      # Intersección lógica: dominan en todos los outputs y usan menos inputs
      combined_filter <- apply(cond_output, 1, all) & apply(cond_input, 1, all)

      # Aplicar el filtro simultáneamente a todas las matrices
      similarity_Rob <- similarity_Rob[combined_filter]
      X_Rob <- as.data.frame(X_Rob[combined_filter,])
      Y_Rob <- as.data.frame(Y_Rob[combined_filter,])
      # mod

      n_sample <- nrow(Y_Rob) #(equivalent to nrow(Y_Rob))

      #pick a sample of random unit in the reference set if there are at least 2 units in the ref
      if (n_sample < 2) {
        eff[i] <- 0
      }
      else {
        for (j in 1:B) {
          #select m random units
          m_sample <- sample(n_sample, round(m,0), prob = similarity_Rob, replace = TRUE)
          Y_ref <- as.data.frame(Y_Rob[m_sample,])
          X_ref <- as.data.frame(X_Rob[m_sample,])

          # compute the DEA for unit i
          # DEA_B[j] <- Benchmarking::dea(
          #   X = data[i,x],
          #   Y = data[i,y],
          #   RTS = RTS,
          #   ORIENTATION = "in-out",
          #   DIRECT = TRUE,
          #   XREF = X_ref,
          #   YREF = Y_ref)$eff

          DEA_B[j] <- dea.add(
            X = as.matrix(data[i,x]),
            Y = as.matrix(data[i,y]),
            XREF = as.matrix(REF[,x]),
            YREF = as.matrix(REF[,y]),
            RTS = RTS
          )[["sum"]]

        }

        eff[i] <- mean(DEA_B[DEA_B != -Inf & DEA_B != Inf])
        if (alpha != FALSE) {
          ci_low[i] <- stats::quantile(DEA_B[DEA_B != -Inf & DEA_B != Inf], alpha/2)
          ci_up[i] <- stats::quantile(DEA_B[DEA_B != -Inf & DEA_B != Inf], 1-alpha/2)
        }

      }
    }

    if(alpha != FALSE) {
      save <- data.frame(eff, ci_low, ci_up)
    }
    else {
      save <- data.frame(eff)
    }

    # assing efficiency
    labels <- ifelse(round(save$ci_low, 4) == 0, "efficient", "not_efficient")
    table(labels)

    # if (labels == "efficient") {browser()}
    data$class_efficiency <- factor(
      labels,
      levels = c("efficient", "not_efficient"))

  }

  return(data)

}
