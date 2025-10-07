#' @title Labeled and Treatment Database for Probabilistic Efficiency Analysis with Explicable Artificial Intelligence (PEAX)
#'
#' @description
#' This function prepares and labels a dataset for efficiency analysis under the Conditional DEA framework, as implemented in the PEAX methodology (Probabilistic Efficiency Analysis with Explicable Artificial Intelligence). It classifies Decision Making Units (DMUs) into efficient or not efficient units. The function explicitly addresses the problem of class imbalance—common in frontier estimation—by allowing the user to control the degree of imbalance. It supports both continuous and discrete environmental variables.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the original dataset used for training and labeling.
#' @param x Column indexes or names of input variables in \code{data}.
#' @param y Column indexes or names of output variables in \code{data}.
#' @param z_c Column indexes or names of continuous contextual variables (optional).
#' @param z_d Column indexes or names of discrete (categorical) contextual variables (optional).
#' @param valid_data A separate \code{data.frame} or \code{matrix} with validation data to assess generalization performance.
#' @param orientation_labeled A \code{character} string specifying the orientation of the model: either \code{"input"} or \code{"output"}.
#' @param p A \code{numeric} value or vector of values in (0,1), representing the quantile threshold(s) used to label efficient units. When multiple values are provided, the function generates one labeled dataset per threshold, allowing users to explore different levels of class imbalance in the efficiency classification.
#'
#' @importFrom caret createDataPartition
#' @importFrom np npudensbw npudens
#' @importFrom dplyr %>% summarise group_by n mutate across select
#'
#' @return A labeled dataset with efficiency classes, contextual variables, and treatment indicators, ready for modeling in the PEAX framework.
#'
#' @details
#' This function is part of the PEAX methodology (Probabilistic Efficiency Analysis with Explicable Artificial Intelligence). It classifies DMUs based on a quantile frontier and supports environmental heterogeneity through contextual variables. Labels generated can be used in classification models to approximate the efficiency frontier probabilistically.
#'
#' @export

PEAX_labeled <- function (
    data, x, y, z_c = NULL, z_d = NULL,
    valid_data = NULL, orientation_labeled = "out",
    p = NULL
    ) {
  
  copy_segurity <- data
  
  # save results to expor
  result <- my_data <- list(
    train_data = NULL,
    valid_data = NULL
  )
  
  # ---------------------------------------------------------------------------
  # Preprocess database -------------------------------------------------------
  # ---------------------------------------------------------------------------
  data_x_y <- preprocessing (
    data = data, 
    x = x, 
    y = y
  )
  
  message("Checking structure and content of the input dataset...")
  
  # check structure
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data.frame or matrix.")
  }
  
  # If data is a matrix, z_d must be NULL or empty
  if (is.matrix(data) && length(z_d) > 0) {
    stop("Discrete contextual variables (z_d) cannot be used when 'data' is a matrix. Please use a data.frame instead.")
  }
  
  # Check for missing values
  if (anyNA(data)) {
    message("There are missing values in the dataset. PEAX requires a complete (NA-free) dataset.")
    stop("Execution stopped: missing values detected.")
  }
  
  # Check if x, y, z_c are numeric
  if (!all(sapply(data[, c(x, y, z_c)], is.numeric))) {
    stop("All variables in x, y, and z_c must be numeric. Please remove or convert factor/character variables.")
  }
  
  # Check if z_d are factors
  if (!all(sapply(data[, z_d, drop = FALSE], is.factor))) {
    stop("All variables in z_d must be factors. Please convert character or numeric variables to factors.")
  }
  
  # Optional: Check that selected columns exist in data
  all_cols <- c(x, y, z_c, z_d)
  if (!all(all_cols %in% seq_len(ncol(data)))) {
    stop("One or more selected column indexes do not exist in 'data'. Check your x, y, z_c, and z_d definitions.")
  }
  
  # Optional: Check for zero-variance columns (common problem in ML)
  if (any(apply(data[, c(x, y, z_c), drop = FALSE], 2, function(col) var(col) == 0))) {
    warning("Some numeric variables have zero variance. Consider removing them before modeling.")
  }
  
  message("Dataset passed all checks.")
  
  # ---------------------------------------------------------------------------
  # Labeling step for the validation dataset (if provided) --------------------
  # ---------------------------------------------------------------------------
  
  # If a validation dataset is provided, apply labeling based on Conditional DEA
  if (!is.null(valid_data)) {
    
    # number of DMUs
    n <- nrow(data)
    
    # initialize the structure of the similarity matrix
    s <- matrix(NA, ncol = n, nrow = n)
    
    # Estimate bandwidth for kernel density based on contextual variables
    bw = npudensbw(dat = data[, c(z_c, z_d)])
    
    # force to be 0 in dummy z
    bw[["bw"]][(length(z_c)+1):length(bw[["bw"]])] <- rep(0, length(z_d))

    for (i in 1:n) {
      kerz <- npudens(bws = bw,
                      tdat = as.data.frame(data[i,c(z_c, z_d)]),
                      edat = data[,c(z_c, z_d)],
                      cykertype = "epanechnikov",
                      cxkertype = "epanechnikov")
      
      s[i, ] <- cbind(kerz$dens)
      s[i,] <- s[i,]/max(s[i,])
    }
    s <- as.data.frame(s)
    
    add_scores <- conditional_DEA(
      input = data[,x],
      output = data[,y],
      exogenous = data[,c(z_c, z_d)],
      m = round(nrow(data)^(2/3)),
      B = 200,
      RTS = 1,
      similarity = s,
      ORIENTATION = "out",
      alpha = 0.01,
      print = T)
    
    # determine efficient and inefficient DMUs
    cond_class_efficiency <- ifelse(add_scores[,2] == 0, 1, 0)
    
    data <- as.data.frame (
      cbind(data, cond_class_efficiency)
    )
    data$cond_class_efficiency <- factor(data$cond_class_efficiency)
    data$cond_class_efficiency <- factor (
      data$cond_class_efficiency,
      levels = rev(levels(data$cond_class_efficiency))
    )
    
    levels(data$cond_class_efficiency) <- c("efficient", "not_efficient")
    
    hold_out <- valid_data # https://topepo.github.io/caret/train-models-by-tag.html
    
    # Crear índice de validación
    valid_index <- createDataPartition(
      data$cond_class_efficiency,
      p = hold_out,
      list = FALSE
    )
    
    # Divide dataset into train and validation datasets
    valid_data <- data[valid_index, c(x,y,z_c, z_d, ncol(data))]
    data <- data[-valid_index, c(x,y,z_c, z_d)]
    
    result[["valid_data"]] <- valid_data
    
  } else {
    
    data <- data[, c(x,y,z_c, z_d)]
    
  }
  
  # ---------------------------------------------------------------------------
  # Labeling step for the training dataset ------------------------------------
  # ---------------------------------------------------------------------------
  
  # It is necessary to reorder the varaibles
  x <- 1:length(x)
  y <- 1:length(y)
  y <- y + length(x)
  
  if (!is.null(z_c)) {
    z_c <- 1:length(z_c)
    z_c <- z_c + length(c(x,y))
  } 
  
  if (!(is.null(z_d))) {
    
    if(is.null(z_c)) {
      
      z_d <- 1:length(z_d)
      z_d <- z_d + length(c(x,y))
      
    } else {
      
      z_d <- 1:length(z_d)
      z_d <- z_d + length(c(x,y, z_c))
      
    }
    
  }
  
  # number of DMUs
  n <- nrow(data)
  
  # initialize the structure of the similarity matrix
  s <- matrix(NA, ncol = n, nrow = n)
  
  # Estimate bandwidth for kernel density based on contextual variables
  bw <- npudensbw(dat = data[, c(z_c, z_d)])
  
  # force to be 0 in dummy z
  bw[["bw"]][(length(z_c)+1):length(bw[["bw"]])] <- rep(0, length(z_d))
  
  for (i in 1:n) {
    kerz <- npudens(bws = bw,
                    tdat = as.data.frame(data[i,c(z_c, z_d)]),
                    edat = as.data.frame(data[,c(z_c, z_d)]),
                    cykertype = "epanechnikov",
                    cxkertype = "epanechnikov")
    
    s[i, ] <- cbind(kerz$dens)
    s[i,] <- s[i,]/max(s[i,])
  }
  s <- as.data.frame(s)
  # s <- round(s, 5)

  add_scores <- conditional_DEA(
    input = data[,x],
    output = data[,y],
    exogenous = data[,c(z_c, z_d)],
    m = round(nrow(data)^(2/3)),
    B = 200,
    RTS = 1,
    similarity = s,
    ORIENTATION = "out",
    alpha = 0.01,
    print = T)
  
  # determine efficient and inefficient DMUs
  cond_class_efficiency <- ifelse(add_scores[,2] == 1 , 1, 0)
  
  data <- as.data.frame (
    cbind(data, cond_class_efficiency)
  )
  
  data$cond_class_efficiency <- factor(data$cond_class_efficiency)
  data$cond_class_efficiency <- factor (
    data$cond_class_efficiency,
    levels = rev(levels(data$cond_class_efficiency))
  )
  
  levels(data$cond_class_efficiency) <- c("efficient", "not_efficient")
  
  # ---------------------------------------------------------------------------
  # Create similarity subgroups -----------------------------------------------
  # ---------------------------------------------------------------------------
  # ID dummys
  # dummy_signature <- apply(data[, z_d, drop = FALSE], 1, paste, collapse = "_")
  # unique(dummy_signature)
  
  data <- data %>%
    mutate(dummy_signature = do.call(paste, c(across(all_of(z_d)), sep = "_")))
  
  min_group_size <- max(length(x) * length(y), 3 * (length(x) + length(y)))
  
  # Inicializar
  data$group_similarity <- NA
  group <- 0
  assigned <- rep(FALSE, nrow(data))
  
  # Agrupar por firma
  for (sig in unique(data$dummy_signature)) {
    idxs <- which(data$dummy_signature == sig & !assigned)
    
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
    current_sig <- data$dummy_signature[current_id]
    candidates <- which(data$dummy_signature == current_sig & !is.na(data$group_similarity))
    
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
      n = n(),
      prop_efficient = mean(cond_class_efficiency == "efficient")
    ) 
  
  # ---------------------------------------------------------------------------
  # Determine efficient and not efficient convex combinations -----------------
  # ---------------------------------------------------------------------------
  
  # Determine the max and min balance rate by user
  max_efficient_rate <- max(p)
  max_not_efficient_rate <- 1 - min(p)
  
  # List to store efficient combinations by group
  efficient_combinations <- list()
  not_efficient_combinations <- list()
  
  # groups list
  groups <- unique(data$group_similarity)

  for (g in groups) {
    
    data_g <- data[data$group_similarity == g,]
    
    if(nrow(data_g) < min_group_size) {
      efficient_combinations[[g]] <- NULL
      not_efficient_combinations[[g]] <- NULL
      next
    }
    
    # determine the maximum combinations that it will be necessary
    # not efficient units:
    # proportion of not efficient units in dataset g
    prop_not_eff <- prop.table(table(data_g$cond_class_efficiency))[2]
    prop_eff <- prop.table(table(data_g$cond_class_efficiency))[1]
    
    n_not_eff <- nrow(data_g[data_g$cond_class_efficiency == "not_efficient",])
    n_eff <- nrow(data_g[data_g$cond_class_efficiency == "efficient",])
    
    # units to generate
    need_generate_not_eff <- round(
      (max_not_efficient_rate*nrow(data_g) - n_not_eff) / (1-max_not_efficient_rate)
    )
    
    if (need_generate_not_eff < 0) {need_generate_not_eff <- 0}
  
    need_generate_eff <- round(
      (max_efficient_rate*nrow(data_g) - n_eff) / (1-max_efficient_rate)
    )
    
    if (need_generate_eff < 0) {need_generate_eff <- 0}
    
    # save results
    max_dimensions <- length(c(x,y))
    control_combinations <- vector("list", max_dimensions)
    # control_combinations_not_eff <- vector("list", max_dimensions)
    # control_combinations_eff <- vector("list", max_dimensions)
    
    new_dmus_not_eff <- data_g[0,1:length(c(x,y,z_c,z_d,1))]
    new_dmus_eff <- data_g[0,1:length(c(x,y,z_c,z_d,1))]
    
    n_dimensions_change <- 0
    iter <- 0
    
    while (nrow(new_dmus_not_eff) < need_generate_not_eff | nrow(new_dmus_eff) < need_generate_eff) {
      
      if (n_dimensions_change == 0 & iter == 0) {
        n_dimensions <- length(c(x,y))
        n_dimensions_max <- length(c(x,y))
      } else if (n_dimensions_change != 0 & iter == 0){
        n_dimensions <- n_dimensions_max - n_dimensions_change
      }
      
      if (iter == 0) {
        
        control_combinations_units <- as.data.frame(matrix(
          data = NA,
          ncol = n_dimensions,
          nrow = 0
        ))
        
      } 
      
      data_g_eff <- data_g[data_g$cond_class_efficiency == "efficient",]
      
      if(nrow(data_g_eff) < n_dimensions) {browser()}
      
      random_dmus <- sample(1:nrow(data_g_eff), n_dimensions, replace = FALSE)
      
      random_dmus <- sort(random_dmus)
      
      position <- max_dimensions - n_dimensions + 1
     
      if(is.null(control_combinations[[position]])) {
        
        control_combinations[[position]] <- t(as.data.frame(random_dmus))
        
      } else {
        
        # if combination has been computed, next
        if (!any(apply(control_combinations[[position]], 1, function(x) all(x == t(as.data.frame(random_dmus)))))) {
          
          control_combinations[[position]] <- rbind(control_combinations[[position]], t(as.data.frame(random_dmus)))
          
        } else {next}
        
      }
      
      iter <- iter + 1
      print(iter)
      
      # calculate new_dmu
      weight <- 1/n_dimensions
      
      weight <- rep(weight, n_dimensions)
      
      data_selected <- data_g_eff[random_dmus,]
      
      # the new dmu
      new_dmu <- data_selected[, c(x,y,z_c)] * weight
      new_dmu <- t(as.data.frame(colSums(new_dmu)))
      
      data_g_zd <- as.data.frame(data_g_eff[1,z_d])
      
      new_dmu <- cbind(new_dmu, data_g_zd)
      
      # test if it is not_efficient
      test_data <- rbind(data[,c(x,y,z_c,z_d)],new_dmu)
      
      # new matrix similarity
      new_n <- nrow(test_data)
      new_s <- matrix(NA, ncol = new_n, nrow = new_n)
      
      for (i in 1:new_n) {
        kerz <- npudens(bws = bw, # new_bw
                        tdat = test_data[i,c(z_c,z_d)],
                        edat = test_data[,c(z_c,z_d)],
                        cykertype = "epanechnikov",
                        cxkertype = "epanechnikov")
        
        new_s[i,] <- cbind(kerz$dens)
        new_s[i,] <- new_s[i,]/max(new_s[i,])
      }
      new_s <- as.data.frame(new_s)
      
      eff_score_combined <- conditional_DEA_one(
        input = test_data[,x],
        output = test_data[,y],
        exogenous = test_data[,c(z_c, z_d)],
        m = round(nrow(test_data)^(2/3)),
        B = 200,
        RTS = 1,
        ORIENTATION = "out",
        similarity = new_s,
        alpha = 0.01,
        print = F)[nrow(test_data),]
      
      if (eff_score_combined[2] == 1) {
        
        # save not efficient
        new_dmu$cond_class_efficiency <- factor(
          "efficient",
          levels = c("efficient", "not_efficient")
        )
        new_dmu$group_similarity <- g
        
        control_combinations_units <- rbind(control_combinations_units, random_dmus)
        
        control_combinations_units <- unname(control_combinations_units)
        
        if(nrow(new_dmus_eff) == 0 & !(need_generate_eff == 0)) {
          
          # control_combinations_not_eff[[g]] <- new_dmu
          new_dmus_eff <- new_dmu
          
        } else if (!(need_generate_eff == 0)){
          
          # control_combinations_eff[[g]] <- rbind(
          #   control_combinations_eff[[g]],new_dmu)
          
          new_dmus_eff <- rbind(new_dmus_eff,new_dmu)
        }
        
      } else {
        
        # save not efficient
        new_dmu$cond_class_efficiency <- factor(
          "not_efficient",
          levels = c("efficient", "not_efficient")
        )
        
        
        control_combinations_units <- rbind(control_combinations_units, random_dmus)
        
        control_combinations_units <- unname(control_combinations_units)
        
        new_dmu$group_similarity <- g
        
        if (nrow(new_dmus_not_eff) == 0 & !(need_generate_not_eff == 0)) {
          
          # control_combinations_not_eff[[g]] <- new_dmu
          new_dmus_not_eff <- new_dmu
          
        } else if (!(need_generate_not_eff == 0)){
          
          # control_combinations_not_eff[[g]] <- rbind(control_combinations_not_eff[[g]],new_dmu)
          new_dmus_not_eff <- rbind(new_dmus_not_eff,new_dmu)
          
        }
        
      }
      
      # if(iter == 100 & n_dimensions_change == 5) {browser()}
      
      
      # control while
      if (iter == 5000) {
       
        if (!is.null(new_dmus_not_eff) & nrow(new_dmus_not_eff) > 1) {
          # hacer combinaciones con pesos al azar
          new_iter <- 0
          while (nrow(new_dmus_not_eff) < need_generate_not_eff | nrow(new_dmus_eff) < need_generate_eff) {
            new_iter <- new_iter + 1
            n_dmu <- sample(1:nrow(control_combinations_units), 1, replace = F)
            
            random_w <- runif(n_dimensions, min = 0.05, max = 1)
            random_w <- random_w / sum(random_w)
            
            random_dmus <- as.numeric(control_combinations_units[as.numeric(n_dmu),])
         
            data_selected <- data_g_eff[random_dmus,]
            
            # the new dmu
            new_dmu <- data_selected[, c(x,y,z_c)] * random_w
            new_dmu <- t(as.data.frame(colSums(new_dmu)))
            
            data_g_zd <- as.data.frame(data_g_eff[1,z_d])
            
            new_dmu <- cbind(new_dmu, data_g_zd)
            
            # test if it is not_efficient
            test_data <- rbind(data[,c(x,y,z_c,z_d)],new_dmu)
            
            # new matrix similarity
            new_n <- nrow(test_data)
            new_s <- matrix(NA, ncol = new_n, nrow = new_n)
            
            for (i in 1:new_n) {
              kerz <- npudens(bws = bw, # new_bw
                              tdat = test_data[i,c(z_c,z_d)],
                              edat = test_data[,c(z_c,z_d)],
                              cykertype = "epanechnikov",
                              cxkertype = "epanechnikov")
              
              new_s[i,] <- cbind(kerz$dens)
              new_s[i,] <- new_s[i,]/max(new_s[i,])
            }
            new_s <- as.data.frame(new_s)
            # browser()
            eff_score_combined <- conditional_DEA_one(
              input = test_data[,x],
              output = test_data[,y],
              exogenous = test_data[,c(z_c, z_d)],
              m = round(nrow(test_data)^(2/3)),
              B = 200,
              RTS = 1,
              ORIENTATION = "out",
              similarity = new_s,
              alpha = 0.01,
              print = F)[nrow(test_data),]
            
            if (eff_score_combined[2] == 1) {
              
              # save not efficient
              new_dmu$cond_class_efficiency <- factor(
                "efficient",
                levels = c("efficient", "not_efficient")
              )
              new_dmu$group_similarity <- g
              
              # control_combinations_units <- rbind(control_combinations_units, random_dmus)
              # 
              # control_combinations_units <- unname(control_combinations_units)
              
              if(nrow(new_dmus_eff) == 0 & !(need_generate_eff == 0)) {
                
                # control_combinations_not_eff[[g]] <- new_dmu
                new_dmus_eff <- new_dmu
                
              } else if (!(need_generate_eff == 0)){
                
                # control_combinations_eff[[g]] <- rbind(
                #   control_combinations_eff[[g]],new_dmu)
                
                new_dmus_eff <- rbind(new_dmus_eff,new_dmu)
              }
              
            } else {
              
              # save not efficient
              new_dmu$cond_class_efficiency <- factor(
                "not_efficient",
                levels = c("efficient", "not_efficient")
              )
              
              
              # control_combinations_units <- rbind(control_combinations_units, random_dmus)
              # 
              # control_combinations_units <- unname(control_combinations_units)
              
              new_dmu$group_similarity <- g
              
              if (nrow(new_dmus_not_eff) == 0 & !(need_generate_not_eff == 0)) {
                
                # control_combinations_not_eff[[g]] <- new_dmu
                new_dmus_not_eff <- new_dmu
                
              } else if (!(need_generate_not_eff == 0)){
                
                # control_combinations_not_eff[[g]] <- rbind(control_combinations_not_eff[[g]],new_dmu)
                new_dmus_not_eff <- rbind(new_dmus_not_eff,new_dmu)
                
              }
              
            }
           
            print(nrow(new_dmus_not_eff))
            print(need_generate_not_eff)
          }
          
        }
        
        print(paste("iter max, n_dimensions:", n_dimensions))
        iter <- 0
        if (n_dimensions_change == 5) {
          break
        } else {
          n_dimensions_change <- n_dimensions_change + 1
        }
      }
      
      if (n_dimensions_change == 5) {
        break
      }
      
    } # while end
    
    efficient_combinations[[g]] <- new_dmus_eff
    not_efficient_combinations[[g]] <- new_dmus_not_eff
    
  } # end for group g
  
nrow(not_efficient_combinations[[2]])
  # df_efficient <- do.call(rbind, efficient_combinations)
  # df_not_efficient <- do.call(rbind, not_efficient_combinations)

  # ---------------------------------------------------------------------------
  # Determine efficient and not efficient convex combinations -----------------
  # ---------------------------------------------------------------------------
  
  result$train_data <- vector("list", length(p) + 1)
  names(result$train_data) <- c("no_balance", p)
  
  result$train_data$no_balance <- data %>%
    select(-dummy_signature)
  
  for (p_i in p) {
    
    save_results <- data %>% 
      select(-dummy_signature)
    
    save_results <- save_results[0,]
    
    for (g in groups) {
      
      # original data of group g
      data_g <- data %>%
        filter(group_similarity == g) %>%
        select(-dummy_signature)
      nrow(data_g)
      
      if (nrow(data_g) < min_group_size) {
        
        save_results <- rbind(save_results, data_g)
        next
        
      }
      
      # determine how many synthetics it is necessary to add
      prop_eff <- prop.table(table(data_g$cond_class_efficiency))[1]
      prop_not_eff <- 1 - prop_eff
      
      n_eff <- nrow(data_g[data_g$cond_class_efficiency == "efficient",])
      n_not_eff <- nrow(data_g) - n_eff
    
      # add efficient units
      if (prop_eff < p_i) {
      
        efficient_rate <- p_i
        
        # determine number of units
        generate_eff <- round(
          (efficient_rate*nrow(data_g) - n_eff) / (1-efficient_rate)
        )
        
        if (generate_eff == need_generate_eff) {
          # not sample, all
          
          new_dmus_eff <- efficient_combinations[[g]]
          nrow(new_dmus_eff)
          
          save_results <- rbind(save_results, data_g, new_dmus_eff)
          
        } else {
          
          # select a sample of need_generate_not_eff units
          idx_sample <- sample(
            1:nrow(efficient_combinations[[g]]),
            generate_eff,
            replace = FALSE)
          
          new_dmus_not_eff <- efficient_combinations[[g]][idx_sample,]
          nrow(new_dmus_not_eff)
          
          save_results <- rbind(save_results, data_g, new_dmus_eff)
          
        }
        
      } else { # add not efficient units
        
        # if (p_i == 0.8) {
        #   browser()
        #   browser()
        # }
        
        not_efficient_rate <- 1 - p_i
        
        # determine number of units
        generate_not_eff <- round(
          (not_efficient_rate*nrow(data_g) - n_not_eff) / (1-not_efficient_rate)
        )
        
        if (generate_not_eff == need_generate_not_eff) {
          # not sample, all
          
          new_dmus_not_eff <- not_efficient_combinations[[g]]
          nrow(new_dmus_not_eff)
          
          save_results <- rbind(save_results, data_g, new_dmus_not_eff)

        } else {
          
          if (nrow(not_efficient_combinations[[g]]) < generate_not_eff) {
            
            save_results <- rbind(save_results, data_g, new_dmus_not_eff)
            result$train_data[[as.character(p_i)]] <- save_results
            next
            
          }
          
          # select a sample of need_generate_not_eff units
          idx_sample <- sample(
            1:nrow(not_efficient_combinations[[g]]),
            generate_not_eff,
            replace = FALSE)
          
          new_dmus_not_eff <- not_efficient_combinations[[g]][idx_sample,]
          nrow(new_dmus_not_eff)
          
          save_results <- rbind(save_results, data_g, new_dmus_not_eff)
          
        }
        
      }
      
    }
    # save results
     
    
    nrow(save_results)
    prop.table(table(save_results$cond_class_efficiency))
    rownames(save_results) <- NULL
    result$train_data[[as.character(p_i)]] <- save_results
    
  }
  
  return(result)
  
}
