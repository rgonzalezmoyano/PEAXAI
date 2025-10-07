#' @title Data Preprocessing and Efficiency Labeling Using DEA
#'
#' @description This function uses classification algorithms to estimate the efficiency of a set of DMUs (Decision Making Units).
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param z Column indexes of environment variables in \code{data}.
#'
#' @importFrom dplyr select_if %>% arrange top_n sample_n group_split
#' @importFrom Benchmarking dea.add
#'
#' @return A \code{"cafee"} object.
#'
#' @export

label_efficiency <- function (
    data, x, y, z = NULL, target_method, convexity = TRUE,
    returns = "variable"
    ) {
  
  # original data with all variables
  data_original <- data
 
  # Select only the variables set as x, y, and z in the dataframe
  # save factor variables
  data_factor <- data[, z]

  # pre-processing
  data <- preprocessing (
    data = data,
    x = x,
    y = y
  )
  
  # reorder index 'x' and 'y' in data
  x <- 1:length(x)
  y <- (length(x) + 1):ncol(data)

  if (is.null(z)) {
    z <- NULL
  } else {
  
    x_y <- length(c(x, y))
    z <- (x_y + 1):((x_y) + length(z))
  }

  # number of inputs / outputs as inputs and number of outputs
  nX <- length(x)
  nY <- length(y)
  nZ <- length(z)

  # save a copy from original data after preprocess
  copy_data_no_label <- data
  
  # divide the dataset by z categorical, if z = 0, not do it. After that, we employ DEA to label each dataset.
  if (nZ != 0) {
    
    # label data with z
    
    # get the names z
    contx_name <- names(data_factor)
    
    # join with data_factor before to divide
    data_to_divide <- cbind(data, data_factor)
    
    # save a copy with data_factor
    data_save <- data_to_divide
    
    # divide in groups by z
    dfs <- data_to_divide %>%
      group_split(across(all_of(contx_name)), .keep = TRUE)
    
  } else {
    
    # label data without z (only one group)
    
    # join with data_factor before to divide (same as z case)
    data_to_divide <- data
    
    # save a copy with data_factor (same as z case)
    data_save <- data_to_divide
    
    # divide in groups by z
    dfs <- list(data)
    
  }

  # save all data labeled
  data_labeled <- as.data.frame(matrix(
    data = NA,
    ncol = ncol(data_to_divide) + 1,
    nrow = 0
  ))
  
  # set names
  names(data_labeled) <- c(names(data_to_divide), "class_efficiency")

  for (sub_group in 1:length(dfs)) {
    #sub_group <- 41
    #print(paste("Sub_group",sub_group))
 
    data <- dfs[[sub_group]]
    
    # ============================ #
    # Label by additive model DEA  #
    # ============================ #
    
    # compute DEA scores through a additive model
    # add_scores <-  compute_scores_additive(
    #   data = data,
    #   x = x,
    #   y = y
    # )
    
    add_scores <- dea.add(
      X = as.matrix(data[,x]),
      Y = as.matrix(data[,y]),
      RTS = "VRS"
    )[["sum"]]
    
    # determine efficient and inefficient DMUs
    class_efficiency <- ifelse(add_scores <= 0.0001, 1, 0)
    
    data <- as.data.frame (
      cbind(data, class_efficiency)
    )
    
    data$class_efficiency <- factor(data$class_efficiency)
    data$class_efficiency <- factor (
      data$class_efficiency,
      levels = rev(levels(data$class_efficiency))
    )
    
    levels(data$class_efficiency) <- c("efficient", "not_efficient")
    
    obs_prop <- prop.table(table(data$class_efficiency))
    
    data_labeled <- rbind(data_labeled, data)
  
  }

  # extra information
  # Find out the imbalance by z
  if (is.null(z)) {
    
    # no z, only 1 row and 5 columns
    row_prop <- 1
    ncol_prop <- 5 
    
    # names
    names_data_proportion <- c("efficient", "inefficient", "n_efficient", "n_inefficient", "n_DMUs")
    
  } else {
    
    # list to save different z values
    expan_z <- vector("list", length(z))
    
    # save only the different values
    for (i in seq_along(z)) {
      
      expan_z[[i]] <- unique(data_labeled[,z][i])
     
    }
    
    expan_z <- lapply(expan_z, function(x) as.vector(x[[1]]))

    expan_z_df <- expand.grid(expan_z)
    
    names(expan_z_df) <- names(data)[z]
    
    # rows of proportions informations by z groups
    row_prop <- nrow(expan_z_df)
    
    # colnames by z
    names_data_proportion <- c(names(data_labeled)[z], "efficient", "inefficient", "n_efficient", "n_inefficient", "n_DMUs")
    
    # there are z, z rows and 6 columns
    ncol_prop <- length(names_data_proportion) 
    
    
  }

  data_proportions <- as.data.frame(matrix(
    data = NA,
    nrow = row_prop,
    ncol = ncol_prop
  ))
  
  # set names
  if (is.null(z)) {
    
    colnames(data_proportions) <- c("efficient", "inefficient", "n_efficient", "n_inefficient", "n_DMUs")
    
  } else {
    
    colnames(data_proportions) <- c(names(data_labeled)[z], "efficient", "inefficient", "n_efficient", "n_inefficient", "n_DMUs")
    
  }
  
  # loop to find out
  if (nZ != 0) {
    
    data_proportions[, 1:nZ] <- expan_z_df #unique(data_labeled[, z])
    
    for(i in 1:nrow(data_proportions)) {
   
      idx_contx <- data_proportions[i,1:nZ]
      
      # filter per z
      sub_data <- data_labeled %>%
        filter(across(all_of(names(idx_contx)), ~ . == idx_contx[[cur_column()]]))
      
      # get information and paste
      n_imbalance_sub <- table(sub_data$class_efficiency)
      data_proportions[i, (c(1,2) + nZ)] <- prop.table(n_imbalance_sub)
      
      data_proportions[i, (3 + nZ)] <- length(which(sub_data$class_efficiency == "efficient"))
      data_proportions[i, (4 + nZ)] <- length(which(sub_data$class_efficiency == "not_efficient"))
      data_proportions[i, (5 + nZ)] <- nrow(sub_data)
      
    }
    
    data_proportions <- na.omit(data_proportions)
      
  } else {
    
    table <- table(data_labeled$class_efficiency)
    
    data_proportions[1,c(1,2)] <- prop.table(table)
    data_proportions[1,c(3,4)] <- table
    data_proportions[1, 5] <- nrow(data_labeled)
    
  }
 
  # output: data and index
  output_label_efficiency <- list(
    data_labeled = data_labeled,
    index = list(
      x = x,
      y = y,
      z = z
    ),
    data_proportions = data_proportions
  )
  
  return(output_label_efficiency)
  
}