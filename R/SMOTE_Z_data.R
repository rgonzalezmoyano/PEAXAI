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
#' @param balance_data Indicate level of efficient units to achive and the number of efficient and not efficient units.
#' @param similarity matrix of similarities. In alternative to provide the exogenous variables, the matrix of similarities can be directly provided. This allow to customize the estimation of the similarities.
#' @param seed  Integer. Seed for reproducibility.
#'
#'
#' @importFrom dplyr %>% semi_join
#'
#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.
#' @export

SMOTE_Z_data <- function (
    data, x, y, z_numeric, z_factor, balance_data, RTS = "vrs", B, m, alpha,
    bandwidth = NULL, seed
) {

  n <- nrow(data)
  exogenous <- c(z_numeric, z_factor)

  # expand grid of regions
  if(!is.null(z_factor)) {

    data_factor <- data[,z_factor]
    grid_factor <- unique(data_factor)

  } else {

    grid_factor <- data.frame(1)

  }

  # number of unique factor combiantions
  n_grid <- NROW(grid_factor)

  # create structure to save datasets balanced
  balance_datasets <- vector("list", length(balance_data))
  names(balance_datasets) <- balance_data
  n_balanced <- length(balance_data)

  # original similarity matrix
  s <- matrix(NA, ncol = n, nrow = n)

  # calculate similarity matrix
  if (is.null(bandwidth)) {
    bw <- npudensbw(dat = data[,exogenous])
    bandwidth <- bw

    bw[["bw"]][(length(z_numeric)+1):length(bw[["bw"]])] <- rep(0, length(z_factor))

  } else {
    bw <- bandwidth

    bw[["bw"]][(length(z_numeric)+1):length(bw[["bw"]])] <- rep(0, length(z_factor))
  }

  for (i in 1:n) {
    kerz <- npudens(
      bws = bw,
      cykertype = "epanechnikov",
      cxkertype = "epanechnikov",
      tdat = data[i,exogenous, drop = FALSE],
      edat = data[,exogenous, drop = FALSE])

    s[i, ] <- cbind(kerz$dens)
  }
  s <- as.data.frame(s)

  # ----------------------------------------------------------------------------
  # Create similarity subgroups ------------------------------------------------
  # ----------------------------------------------------------------------------
  # minimum size of each sub-group
  min_group_size <- max(length(x) * length(y), 3 * (length(x) + length(y)))

  # Inicialize
  data$group_similarity <- NA
  group <- 0
  assigned <- rep(FALSE, nrow(data))

  # Group by z_factor
  for (z_i in 1:n_grid) { # sig = z_i

    name_z_i <- as.data.frame(grid_factor[z_i,])
    names(name_z_i) <- names(grid_factor)

    # filter by z_factor_i
    data_z_i <- semi_join(data, data_factor[z_i,], by = names(name_z_i))

    # which are potential candidates
    idxs <- which(!assigned)

    if (length(idxs) < min_group_size) next

    while (any(!assigned[idxs])) {
      candidates <- idxs[!assigned[idxs]]
      random_DMU <- sample(candidates, 1)

      sim_vals <- s[random_DMU, idxs]
      similar <- idxs[which(sim_vals > 0.01 & !assigned[idxs])]

      if (length(similar) >= min_group_size) {
        group <- group + 1
        data$group_similarity[similar] <- group
        assigned[similar] <- TRUE
      } else {
        assigned[random_DMU] <- TRUE
      }
    }
  }

  # Add no subgroup
  data_no_subgroup <- which(is.na(data$group_similarity))

  for (current_id in data_no_subgroup) {

    # filter by z_factor_i
    current_z_factor <- data[current_id, z_factor]

    ### new
    idx_env_match <- function(data, current_z_factor, z_factor = NULL) {

      env_cols <- names(current_z_factor)

      # Comprobar que existen en data
      missing_cols <- setdiff(env_cols, names(data))
      if (length(missing_cols) > 0) {
        stop("Estas columnas no están en `data`: ", paste(missing_cols, collapse = ", "))
      }

      # 2) Extraer el "target" (la combinación concreta)
      if (!is.data.frame(current_z_factor)) {
        stop("`current_z_factor` debe ser un data.frame/tibble de 1 fila con columnas = variables de entorno.")
      }
      if (nrow(current_z_factor) != 1) {
        stop("`current_z_factor` debe tener exactamente 1 fila.")
      }

      target <- current_z_factor[1, env_cols, drop = FALSE]

      # 3) Comparar fila a fila en TODAS las variables de entorno (robusto con factors)
      mat <- sapply(env_cols, function(cl) {
        as.character(data[[cl]]) == as.character(target[[cl]])
      })

      idx <- which(rowSums(mat) == length(env_cols))
      idx
    }

    idx <- idx_env_match(data = data, current_z_factor = current_z_factor, z_factor = z_factor)

    ### end new

    # DMUs which have their group
    candidates <- which(!is.na(data$group_similarity))

    # DMUs which hace the same z_factor
    candidates <- candidates[candidates %in% idx]

    if (length(candidates) == 0) {
      group <- group + 1
      data$group_similarity[current_id] <- group
    } else {

      sim_vals <- s[current_id, candidates]
      ref_unit <- candidates[which.max(sim_vals)]
      data$group_similarity[current_id] <- data$group_similarity[ref_unit]
    }
  }

  resumen_final <- data %>%
    group_by(group_similarity) %>%
    summarise(
      n = dplyr::n(),
      prop_efficient = mean(class_efficiency == "efficient")
    )

  # Step 2: determine the efficient facets
  for (group_i in unique(data[["group_similarity"]])) {

    data_i <- data[data[["group_similarity"]] == group_i,]
    data_i <- data_i[, setdiff(names(data_i), "group_similarity")]

    # second, populate the efficient facets
    if (nrow(data_i) >= min_group_size) {

      facets <- convex_facets(
        data = data_i,
        x = x,
        y = y,
        RTS = RTS
      )

      if (nrow(facets) == 0) {
        balance_datasets_i <- vector("list", length = length(balance_data))
        names(balance_datasets_i) <- balance_data

        balance_datasets_i[] <- list(data_i)

      } else {

        balance_datasets_i <- get_SMOTE_DMUs(
          data = data_i,
          REF_data = data[,  setdiff(names(data_i), "group_similarity")],
          facets = facets,
          x = x,
          y = y,
          z_numeric = z_numeric,
          z_factor = z_factor,
          alpha = alpha,
          balance_data = balance_data,
          bandwidth = bandwidth,
          seed = seed
        )

      }

    } else {

      balance_datasets_i <- vector("list", length = length(balance_data))
      names(balance_datasets_i) <- balance_data

      balance_datasets_i[] <- list(data_i)

    }

    # merge new datasets
    balance_datasets <- Map(function(base_df, add_df) {
      if (is.null(base_df)) return(add_df)
      if (is.null(add_df))  return(base_df)
      rbind(base_df, add_df)
    }, balance_datasets, balance_datasets_i)

  }

  return(balance_datasets)
}
