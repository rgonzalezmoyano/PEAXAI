#' @title Create New SMOTE Units to Balance Data combinations of m + s 
#'
#' @description This function creates new DMUs to address data imbalances. If the majority class is efficient, it generates new inefficient DMUs by worsering the observed units. Conversely, if the majority class is inefficient, it projects inefficient DMUs to the frontier. Finally, a random selection if performed to keep a proportion of 0.65 for the majority class and 0.35 for the minority class.
#' 
#' @param data A \code{data.frame} containing the variables used in the model.
#' @param data_factor A \code{data.frame} containing the factor variables used in the model.
#' @param x Column indexes of the input variables in the \code{data}.
#' @param y Column indexes of the output variables in the \code{data}.
#' @param z Column indexes of environment variables in \code{data}.
#' @param balance_data Indicate level of efficient units to achive and the number of efficient and not efficient units.
#' @param sub_frontier Indicate how many units not efficient it must be created.

#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.

SMOTE_balance_data <- function (
    data, data_factor = NULL, x, y, z = NULL, balance_data
) {

  # =========================================================== #
  # determinate number of efficient and not efficient to create #
  # =========================================================== #

  # determinate numbre of efficient and ineeficient units
  n_real_eff <- nrow(data[data$class_efficiency == "efficient",])
  n_real_ineff <- nrow(data[data$class_efficiency == "not_efficient",])
  
  prop_real <- n_real_eff / nrow(data)
  
  # n_new_eff <- n_real_eff
  n_new_eff <- 0
  
  #n_new_ineff <- n_real_ineff
  n_new_ineff <- 0
  
  # proportion of efficients
  prop <- prop_real
  
  sense_balance <- NULL
  
  # determinate the way to balance, create efficient or not efficient
  if (prop < balance_data) {
    
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
  eff_level <- balance_data
  
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

  # ============================================= #
  # create convex combinations to achieve balance #
  # ============================================= #
  
  if (sense_balance == "efficient") {
    
    # first, create set combinations 
    # determinate 
    data_eff <- data[data$class_efficiency == "efficient", ]
    
    idx_eff <- 1:nrow(data_eff)
    
    # proportion importance
    len <- length(c(x,y))
    
    prop_imp <- 1/len
    
    lambda <- rep(prop_imp, ncol(data_eff[, c(x,y)]))
    
    n_comb <- length(c(x,y))
    
    combinations <- as.data.frame(t(combn(idx_eff, n_comb)))
    
    eff_convex <- as.data.frame(matrix(
      data = NA,
      ncol = ncol(data_eff[, c(x,y)]),
      nrow = 0
    ))
    
    names(eff_convex) <- names(data_eff[, c(x,y)])
    
    ineff_convex <- as.data.frame(matrix(
      data = NA,
      ncol = ncol(data_eff[, c(x,y)]),
      nrow = 0
    ))
    
    names(ineff_convex) <- names(data_eff[, c(x,y)])
  
    if(nrow(combinations) > 5000) {
      
      # Partition size
      n_batch <- 5000
      
      # shuffle the data
      shuffle_data <- sample(1:nrow(combinations))
      
      # Create an index for each partition
      combinations$particion <- ceiling(seq_along(shuffle_data) / n_batch)
      
      batch_all <- split(combinations[shuffle_data, ], combinations$particion)
      
      n_total_batch <- ceiling(nrow(combinations) / 5000)
      
    } else {
      
      batch_all <- list()
      batch_all[[1]] <- combinations
      
      n_total_batch <- 1
      
    }
    
    # create convex combintaions
    print("calculate combinations points")
    
    iter <- 0
    count_batch <- 0
    
    save_idx_eff <- NULL
    save_idx_ineff <- NULL
    
    # ================================= #
    # create efficient units to balance #
    # ================================= #

    while (nrow(eff_convex) < create_eff) {
 
      count_batch <- count_batch + 1
      iter <- iter + 1
      print(iter)
   
      # units to classify
      results_convx <- t(apply(batch_all[[iter]], 1, function(indices) { #[,c(x,y)]
       
        # select row
        seleccion <- data_eff[unlist(as.vector(indices)), c(x,y)]
        
        # calculate
        colSums(seleccion * lambda)
        
      }))
      
      # change to dataframe
      results_convx <- as.data.frame(results_convx)
      
      # change name
      names(results_convx) <- names(data_eff[, c(x,y)])
      
      # create test dataset additive
      test_eff <- rbind(data_eff[, c(x,y)], results_convx)
      
      # test additive
      test_add <- compute_scores_additive(test_eff, x = x, y = y)
      
      # leave original eff units, get index
      new_results_convx <- test_eff[(nrow(data_eff) + 1):nrow(test_eff),]
      idx_eff <- which(test_add[(nrow(data_eff) + 1):nrow(test_add),] < 0.0001)
      
      # save idx_eff
      save_idx_eff <- c(save_idx_eff, idx_eff)
      
      new_eff_conx_unit <- new_results_convx[idx_eff, ]
      
      # save unit efficient
      eff_convex <- rbind(eff_convex, new_eff_conx_unit)
      
      # save not efficient
      ineff_to_save <- new_results_convx
    
      if(nrow(new_eff_conx_unit) != 0) {
        
        ineff_to_save <- ineff_to_save[-idx_eff,]
        ineff_to_save <- na.omit(ineff_to_save)
        
      }
      
      ineff_convex <- rbind(ineff_convex, ineff_to_save)
      
      # get prop information
      print(paste("There are:", nrow(eff_convex)))
      print(paste("It must be created:", create_eff))
      print(paste(nrow(eff_convex)/create_eff * 100, "%"))
  
      true_eff <- nrow(eff_convex)
      
      # extreme case: there are not full-dimensional faces
      if (length(idx_eff) == 0 & count_batch == n_total_batch) {
        
        iter_extreme <- 0
        
        while (nrow(eff_convex) < create_eff) {
          
          idx_eff <- 1:nrow(data_eff)
          
          # proportion importance
          len <- len - 1
          
          prop_imp <- 1/len
          
          lambda <- rep(prop_imp, len)
          
          n_comb <- len
          
          combinations <- as.data.frame(t(combn(idx_eff, n_comb)))
          
          if(nrow(combinations) > 5000) {
            
            # Partition size
            n_batch <- 5000
            
            # shuffle the data
            shuffle_data <- sample(1:nrow(combinations))
            
            # Create an index for each partition
            combinations$particion <- ceiling(seq_along(shuffle_data) / n_batch)
            
            batch_all <- split(combinations[shuffle_data, ], combinations$particion)
            
            n_total_batch <- ceiling(nrow(combinations) / 5000)
            
          } else {
            
            batch_all <- list()
            batch_all[[1]] <- combinations
            
            n_total_batch <- 1
            
          }
        
          iter_extreme <- iter_extreme + 1

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
          
          # create test dataset additive
          test_eff <- rbind(data_eff[, c(x,y)], results_convx)
          
          # test additive
          test_add <- compute_scores_additive(test_eff, x = x, y = y)
          
          # leave original eff units, get index
          new_results_convx <- test_eff[(nrow(data_eff) + 1):nrow(test_eff),]
          idx_eff <- which(test_add[(nrow(data_eff) + 1):nrow(test_add),] < 0.0001)
          
          # save idx_eff
          save_idx_eff <- c(save_idx_eff, idx_eff)
          
          new_eff_conx_unit <- new_results_convx[idx_eff, ]
          
          # save unit efficient
          eff_convex <- rbind(eff_convex, new_eff_conx_unit)
          
          # save not efficient
          ineff_to_save <- new_results_convx
          
          if(nrow(new_eff_conx_unit) != 0) {
            
            ineff_to_save <- ineff_to_save[-idx_eff,]
            ineff_to_save <- na.omit(ineff_to_save)
            
          }
          
          ineff_convex <- rbind(ineff_convex, ineff_to_save)
          
          # get prop information
          print(paste("There are:", nrow(eff_convex)))
          print(paste("It must be created:", create_eff))
          print(paste(nrow(eff_convex)/create_eff * 100, "%"))
          
          true_eff <- nrow(eff_convex)
          
          if (iter_extreme == length(data[,c(x,y)])) {browser()} 
        }
       
      } # end exreme cases
      
      # if there are not enough efficient units, use
      if(count_batch == n_total_batch & true_eff < create_eff) {
        
        # need to create
        need_eff <- create_eff - true_eff
        
        eff_combinations <- combinations[save_idx_eff,]
        
        save_lambda_eff <- as.data.frame(matrix(
          data = NA,
          ncol = length(c(x,y)),
          nrow = 0
        ))
        
        
        while (nrow(save_lambda_eff) < need_eff) {
          
          # process to generate lambda
          generate_lambda <- runif(length(c(x, y)), min = 0.05, max = 0.95)
          
          normalize_lambda <- generate_lambda/sum(generate_lambda)
          
          # set lambda
          lambda_eff <- normalize_lambda
          
          # set combnation to make new unit
          idx_new_eff <- sample(1:nrow(eff_combinations), size = 1)
          selec_comb <- eff_combinations[idx_new_eff,]
          
          # units to classify
          seleccion <- data_eff[unlist(as.vector(selec_comb)), c(x,y)]
          
          # calculate
          new_unit <- colSums(seleccion * lambda_eff)
          
          # check
          check_data <- rbind(data_eff[, c(x,y)], new_unit)
          
          check_test <- compute_scores_additive(check_data, x = x, y = y)
          
          # save if is correct
          if (check_test[nrow(data_eff) + 1,] < 0.0001) {
            
            save_lambda_eff <- rbind(save_lambda_eff, new_unit)
            
          }
          
        } # end loop while
        
        names(save_lambda_eff) <- names(data_eff[, c(x,y)])
        
        # join eff_data
        eff_convex <- rbind(eff_convex, save_lambda_eff)
        
      } # end case need more efficient units
      
      if (count_batch == n_total_batch & true_eff > create_eff) {
        
        delete_eff <- true_eff - create_eff
        idx_delete <- sample(1:true_eff, size = delete_eff, replace = FALSE)
        
        eff_convex <- eff_convex[-idx_delete,]
        
      }
      iter
    } # end while

    if (nrow(eff_convex) != 0) {
      
      # add class efficiency
      eff_convex[,z] <- data[1,z]
      eff_convex$class_efficiency <- "efficient"
      
      names(eff_convex) <- names(data)
      
      final_data <- rbind(data, eff_convex)
      
    } else {
      
      final_data <- data
      
    }
      
  } else {

    # first, create set combinations 
    # determinate 
    idx <- 1:nrow(data)
    
    # proportion importance
    len <- length(c(x,y))
    
    prop_imp <- 1/len
    
    lambda <- rep(prop_imp, ncol(data[, c(x,y)]))
    
    n_comb <- length(c(x,y))

    combinations <- as.data.frame(t(combn(idx, n_comb)))
    
    if (nrow(combinations) > (create_ineff * 3)) {
      idx_combinations <- sample(x = 1:nrow(combinations), size = (create_ineff * 3), replace = FALSE)
      combinations <- combinations[idx_combinations,]
    }
    
    ineff_convex <- as.data.frame(matrix(
      data = NA,
      ncol = ncol(data[, c(x,y)]),
      nrow = 0
    ))
    
    names(ineff_convex) <- names(data[, c(x,y)])
    
    # ===================================== #
    # create not efficient units to balance #
    # ===================================== #

    batch_all <- list()
    batch_all[[1]] <- combinations
    
    n_total_batch <- 1
    
    print("calculate combinations points")
    
    save_idx_ineff <- NULL
    
    count_batch <- 1
    iter <- 1
    print(iter)
    
    # units to classify
    results_convx <- t(apply(batch_all[[iter]][,c(x,y)], 1, function(indices) {
      
      # select row
      seleccion <- data[unlist(as.vector(indices)), c(x,y)]
      
      # calculate
      colSums(seleccion * lambda)
      
    }))
    
    # change to dataframe
    results_convx <- as.data.frame(results_convx)
    
    # change name
    names(results_convx) <- names(data[, c(x,y)])
   
    while (nrow(ineff_convex) < create_ineff) {
      
      idx_convx <- 1:nrow(results_convx)
    
      # if there are less ineficient untis, use all
      if(length(idx_convx) < create_ineff) {
        
        random_DMU <- idx_convx
        
      } else {
        
        random_DMU <- sample(x = idx_convx, size = create_ineff, replace = FALSE)
        
      }
      
      # create test dataset additive
      test_eff <- rbind(data[data$class_efficiency == "efficient", c(x,y)], results_convx[random_DMU,])

      # test additive
      test_add <- compute_scores_additive(test_eff, x = x, y = y)

      n_eff <- nrow(data[data$class_efficiency == "efficient",])
      
      # leave original eff units, get index
      new_results_convx <- test_eff[(n_eff + 1):nrow(test_eff),]
      idx_ineff <- which(test_add[(n_eff + 1):nrow(test_add),] > 0.0001)
      
      ineff_convex <- rbind(ineff_convex, new_results_convx[idx_ineff,])
      
      true_ineff <- nrow(ineff_convex)
     
      # if there are not enough inefficient units, use
      if(count_batch == n_total_batch & true_ineff < create_ineff) {

        # need to create
        need <- create_ineff - true_ineff

        ineff_combinations <- combinations[idx_ineff,]

        save_lambda_ineff <- as.data.frame(matrix(
          data = NA,
          ncol = length(c(x,y)),
          nrow = 0
        ))
        
        while (nrow(save_lambda_ineff) < need) {
          
          # process to generate lambda
          generate_lambda <- runif(length(c(x, y)), min = 0.05, max = 0.95)
            
          normalize_lambda <- generate_lambda/sum(generate_lambda)
              
          # set lambda
          lambda_ineff <- normalize_lambda

          # set combnation to make new unit
          idx_new_ineff <- sample(1:nrow(combinations), size = 1)
          selec_comb <- combinations[idx_new_ineff,]

          # units to classify
          seleccion <- data[unlist(as.vector(selec_comb)), c(x,y)]

          # calculate
          new_unit <- colSums(seleccion * lambda_ineff)

          # check
          check_data <- rbind(data[data$class_efficiency == "efficient", c(x,y)], new_unit)

          check_test <- compute_scores_additive(check_data, x = x, y = y)

          # save if is correct
          if (check_test[n_eff + 1,] > 0.0001) {

            save_lambda_ineff <- rbind(save_lambda_ineff, new_unit)

          }

        } # end loop while

        names(save_lambda_ineff) <- names(data[, c(x,y)])

        # join eff_data
        ineff_convex <- rbind(ineff_convex, save_lambda_ineff)
      
      } # end case need more inefficient units
      
    }
    
    if (nrow(ineff_convex) > create_ineff) {
      
      idx_save <- 1:create_ineff
      
      ineff_convex <- ineff_convex[idx_save,]
    }
    
    # add class efficiency
    ineff_convex[,z] <- data[1,z]

    ineff_convex$class_efficiency <- "not_efficient"
    
    names(ineff_convex) <- names(data)

    final_data <- rbind(data, ineff_convex)
    
  }
  # # determinate 
  # data_eff <- data[data$class_efficiency == "efficient", ]
  # 
  # idx_eff <- 1:nrow(data_eff)
  # 
  # # proportion importance
  # len <- length(c(x,y))
  # 
  # prop_imp <- 1/len
  # 
  # lambda <- rep(prop_imp, ncol(data_eff[, c(x,y)]))
  # 
  # n_comb <- length(c(x,y))
  # 
  # combinations <- as.data.frame(t(combn(idx_eff, n_comb)))
  # 
  # eff_convex <- as.data.frame(matrix(
  #   data = NA,
  #   ncol = ncol(data_eff[, c(x,y)]),
  #   nrow = 0
  # ))
  # 
  # names(eff_convex) <- names(data_eff[, c(x,y)])
  # 
  # ineff_convex <- as.data.frame(matrix(
  #   data = NA,
  #   ncol = ncol(data_eff[, c(x,y)]),
  #   nrow = 0
  # ))
  # 
  # names(ineff_convex) <- names(data_eff[, c(x,y)])
  # 
  # if(nrow(combinations) > 5000) {
  #   
  #   # Partition size
  #   n_batch <- 5000
  #   
  #   # shuffle the data
  #   shuffle_data <- sample(1:nrow(combinations))
  #   
  #   # Create an index for each partition
  #   combinations$particion <- ceiling(seq_along(shuffle_data) / n_batch)
  #   
  #   batch_all <- split(combinations[shuffle_data, ], combinations$particion)
  #   
  #   n_total_batch <- ceiling(nrow(combinations) / 5000)
  #   
  # } else {
  #   
  #   batch_all <- list()
  #   batch_all[[1]] <- combinations
  #   
  #   n_total_batch <- 1
  #   
  # }
  # 
  # # create convex combintaions
  # print("calculate combinations points")
  # 
  # iter <- 0
  # count_batch <- 0
  # 
  # save_idx_eff <- NULL
  # save_idx_ineff <- NULL
  # 
  # if (sense_balance == "efficient") {
  #   
  #   # ================================= #
  #   # create efficient units to balance #
  #   # ================================= #
  #   
  #   while (nrow(eff_convex) < create_eff) {
  #     
  #     count_batch <- count_batch + 1
  #     iter <- iter + 1
  #     print(iter)
  #     
  #     # units to classify
  #     results_convx <- t(apply(batch_all[[iter]][,c(x,y)], 1, function(indices) {
  #       
  #       # select row
  #       seleccion <- data_eff[unlist(as.vector(indices)), c(x,y)]
  #       
  #       # calculate
  #       colSums(seleccion * lambda)
  #       
  #     }))
  #     
  #     # change to dataframe 
  #     results_convx <- as.data.frame(results_convx)
  #     
  #     # change name
  #     names(results_convx) <- names(data_eff[, c(x,y)])
  #     
  #     # create test dataset additive
  #     test_eff <- rbind(data_eff[, c(x,y)], results_convx)
  #     
  #     # test additive
  #     test_add <- compute_scores_additive(test_eff, x = x, y = y)
  #     
  #     # leave original eff units, get index 
  #     new_results_convx <- test_eff[(nrow(data_eff) + 1):nrow(test_eff),]
  #     idx_eff <- which(test_add[(nrow(data_eff) + 1):nrow(test_add),] < 0.0001)
  #     
  #     if (length(idx_eff) == 0) {
  #       break
  #     }
  #     
  #     # save idx_eff
  #     save_idx_eff <- c(save_idx_eff, idx_eff)
  #     
  #     new_eff_conx_unit <- new_results_convx[idx_eff, ]
  #     
  #     # save unit efficient
  #     eff_convex <- rbind(eff_convex, new_eff_conx_unit)
  #     
  #     # save not efficient
  #     ineff_to_save <- new_results_convx
  #     
  #     if(nrow(new_eff_conx_unit) != 0) {
  #       
  #       ineff_to_save <- ineff_to_save[-idx_eff,]
  #       ineff_to_save <- na.omit(ineff_to_save)
  #       
  #     }
  #     
  #     ineff_convex <- rbind(ineff_convex, ineff_to_save)
  #     
  #     # get prop information
  #     print(paste("There are:", nrow(eff_convex)))
  #     print(paste("It must be created:", create_eff))
  #     print(paste(nrow(eff_convex)/create_eff * 100, "%"))
  #     
  #     true_eff <- nrow(eff_convex)
  #     
  #     # if there are not enough efficient units, use 
  #     if(count_batch == n_total_batch & true_eff < create_eff) {
  #       
  #       # need to create 
  #       need_eff <- create_eff - true_eff
  #       
  #       eff_combinations <- combinations[save_idx_eff,]
  #       
  #       save_lambda_eff <- as.data.frame(matrix(
  #         data = NA,
  #         ncol = length(c(x,y)),
  #         nrow = 0
  #       ))
  #       
  #       while (nrow(save_lambda_eff) < need_eff) {
  #         
  #         # process to generate lambda
  #         generate_lambda <- runif(length(c(x, y)), min = 0.05, max = 0.95) 
  #         
  #         normalize_lambda <- generate_lambda/sum(generate_lambda)
  #         
  #         # set lambda
  #         lambda_eff <- normalize_lambda
  #         
  #         # set combnation to make new unit
  #         idx_new_eff <- sample(1:nrow(eff_combinations), size = 1)
  #         selec_comb <- eff_combinations[idx_new_eff,]
  #         
  #         # units to classify
  #         seleccion <- data_eff[unlist(as.vector(selec_comb)), c(x,y)]
  #         
  #         # calculate
  #         new_unit <- colSums(seleccion * lambda_eff)
  #         
  #         # check 
  #         check_data <- rbind(data_eff[, c(x,y)], new_unit)
  #         
  #         check_test <- compute_scores_additive(check_data, x = x, y = y)
  #         
  #         # save if is correct
  #         if (check_test[nrow(data_eff) + 1,] < 0.0001) {
  #           
  #           save_lambda_eff <- rbind(save_lambda_eff, new_unit)
  #           
  #         }
  #         
  #       } # end loop while
  #       
  #       names(save_lambda_eff) <- names(data_eff[, c(x,y)])
  #       
  #       # join eff_data
  #       eff_convex <- rbind(eff_convex, save_lambda_eff)
  #       
  #     } # end case need more efficient units
  #     
  #     if (count_batch == n_total_batch & true_eff > create_eff) {
  #       
  #       delete_eff <- true_eff - create_eff
  #       idx_delete <- sample(1:true_eff, size = delete_eff, replace = FALSE)
  #       
  #       eff_convex <- eff_convex[-idx_delete,]
  #       
  #     }
  #     
  #   } # end while
  #   
  #   if (nrow(eff_convex) != 0) {
  #     # add class efficiency
  #     eff_convex[,z] <- data[1,z]
  #     eff_convex$class_efficiency <- "efficient"
  #     
  #     final_data <- rbind(data, eff_convex)
  #   } else {
  #     final_data <- data
  #   }
  #   
  #   
  # } else {
  #   
  #   # ===================================== #
  #   # create not efficient units to balance #
  #   # ===================================== #
  #   
  #   # ===
  #   # add quartile not efficient
  #   # ===
  #   
  #   while (nrow(ineff_convex) < create_ineff) {
  #     
  #     count_batch <- count_batch + 1
  #     iter <- iter + 1
  #     print(iter)
  #     
  #     # units to classify
  #     results_convx <- t(apply(batch_all[[iter]][,c(x,y)], 1, function(indices) {
  #       
  #       # select row
  #       seleccion <- data_eff[unlist(as.vector(indices)), c(x,y)]
  #       
  #       # calculate
  #       colSums(seleccion * lambda)
  #       
  #     }))
  #     
  #     # change to dataframe 
  #     results_convx <- as.data.frame(results_convx)
  #     
  #     # change name
  #     names(results_convx) <- names(data_eff[, c(x,y)])
  #     
  #     # create test dataset additive
  #     test_eff <- rbind(data_eff[, c(x,y)], results_convx)
  #     
  #     # test additive
  #     test_add <- compute_scores_additive(test_eff, x = x, y = y)
  #     
  #     # leave original eff units, get index 
  #     new_results_convx <- test_eff[(nrow(data_eff) + 1):nrow(test_eff),]
  #     idx_ineff <- which(test_add[(nrow(data_eff) + 1):nrow(test_add),] > 0.0001)
  #     
  #     # save idx_ineff
  #     save_idx_ineff <- c(save_idx_ineff, idx_ineff)
  #     
  #     new_ineff_conx_unit <- new_results_convx[idx_ineff, ]
  #     
  #     # save unit inefficient
  #     ineff_convex <- rbind(ineff_convex, new_ineff_conx_unit)
  # 
  #     # get prop information
  #     print(paste("There are:", nrow(ineff_convex)))
  #     print(paste("It must be created:", create_ineff))
  #     
  #     true_ineff <- nrow(ineff_convex)
  # 
  #     # if there are not enough efficient units, use 
  #     if(count_batch == n_total_batch & true_ineff < create_ineff) {
  #       
  #       # need to create 
  #       need_ineff <- create_ineff - true_ineff
  #       
  #       ineff_combinations <- combinations[save_idx_ineff,]
  #       
  #       save_lambda_ineff <- as.data.frame(matrix(
  #         data = NA,
  #         ncol = length(c(x,y)),
  #         nrow = 0
  #       ))
  #       
  #       while (nrow(save_lambda_ineff) < need_ineff) {
  #         
  #         # process to generate lambda
  #         generate_lambda <- runif(length(c(x, y)), min = 0.05, max = 0.95) 
  #         
  #         normalize_lambda <- generate_lambda/sum(generate_lambda)
  #         
  #         # set lambda
  #         lambda_ineff <- normalize_lambda
  #         
  #         # set combnation to make new unit
  #         idx_new_ineff <- sample(1:nrow(ineff_combinations), size = 1)
  #         selec_comb <- ineff_combinations[idx_new_ineff,]
  #         
  #         # units to classify
  #         seleccion <- data_eff[unlist(as.vector(selec_comb)), c(x,y)]
  #         
  #         # calculate
  #         new_unit <- colSums(seleccion * lambda_ineff)
  #        
  #         # check 
  #         check_data <- rbind(data_eff[, c(x,y)], new_unit)
  #         
  #         check_test <- compute_scores_additive(check_data, x = x, y = y)
  #         
  #         # save if is correct
  #         if (check_test[nrow(data_eff) + 1,] > 0.0001) {
  #           
  #           save_lambda_ineff <- rbind(save_lambda_ineff, new_unit)
  #           
  #         }
  #         
  #       } # end loop while
  #       
  #       names(save_lambda_ineff) <- names(data_eff[, c(x,y)])
  #       
  #       # join eff_data
  #       ineff_convex <- rbind(ineff_convex, save_lambda_ineff)
  #       
  #     } else if (count_batch == n_total_batch & true_ineff > create_ineff) {
  #       
  #       if (create_ineff * 20 < true_ineff) {
  #         random_ineff_idx <- sample(nrow(ineff_convex), size = (create_ineff * 20), replace = FALSE)
  #       } else {
  #         random_ineff_idx <- sample(nrow(ineff_convex), size = (create_ineff), replace = FALSE)
  #       }
  #   
  #       data_ineff_test <- rbind(data_eff[, c(x,y)], ineff_convex[random_ineff_idx,])
  #       
  #       slack_value <- compute_scores_additive(data = data_ineff_test, x = x, y = y)
  #       
  #       idx_delete <- which(slack_value < 0.00001)
  #       
  #       slack_value <- slack_value[-idx_delete]
  #       
  #       quantile_break <- quantile(slack_value, probs = c(0, 0.25, 0.5, 0.75, 1))
  #       
  #       quartiles <- cut(
  #         slack_value,
  #         breaks = quantile_break,
  #         labels = c("Q1", "Q2", "Q3", "Q4"),
  #         include.lowest = TRUE
  #       )
  #       
  #       # same proportion to each quartile
  #       n_qutl <- create_ineff/4
  #       
  #       # units to each quartile
  #       units_quartil <- floor(n_qutl)
  #       aditional_units <-  create_ineff - sum(units_quartil * 4)
  #       
  #       save_ineff_convex <- as.data.frame(matrix(data = NA, ncol = length(c(x,y)), nrow = 0))
  #       names(save_ineff_convex) <- names(ineff_convex)
  #       
  #       for(qrtl in seq_along(levels(quartiles))) {
  #         
  #         quartile <- levels(quartiles)[qrtl]
  #         
  #         idx <- which(quartiles == quartile)
  #        
  #         if(quartile == "Q4") {
  #           
  #           units_quartil <- units_quartil + aditional_units
  #           
  #           if (length(idx) < units_quartil) {
  #             
  #             delete_unit <- units_quartil - length(idx)
  #             
  #             units_quartil <- length(idx)
  #             
  #             
  #             create_ineff <- create_ineff - delete_unit
  #             
  #           }
  #           
  #         }
  #         
  #         selected_idx <- sample(idx, units_quartil, replace = FALSE)
  #        
  #         save_ineff_convex <- rbind(save_ineff_convex, ineff_convex[selected_idx,])
  #         
  #       }
  #       
  #       ineff_convex <- save_ineff_convex
  #       
  #       
  #     } # end case need more efficient units
  #   
  #   }
  #   
  #   # add class efficiency
  #   ineff_convex[,z] <- data[1,z]
  #   ineff_convex$class_efficiency <- "not_efficient"
  # 
  #   final_data <- rbind(data, ineff_convex)
  # 
  #}

  if (any(is.na(final_data))) {browser()}

  return(final_data)
}

#' @title Create New SMOTE Units to Balance Data combinations of m + s 
#'
#' @description This function creates new DMUs to address data imbalances. If the majority class is efficient, it generates new inefficient DMUs by worsering the observed units. Conversely, if the majority class is inefficient, it projects inefficient DMUs to the frontier. Finally, a random selection if performed to keep a proportion of 0.65 for the majority class and 0.35 for the minority class.
#' 
#' @param data A \code{data.frame} containing the variables used in the model.
#' @param data_factor A \code{data.frame} containing the factor variables used in the model.
#' @param x Column indexes of the input variables in the \code{data}.
#' @param y Column indexes of the output variables in the \code{data}.
#' @param z Column indexes of environment variables in \code{data}.
#' @param balance_data Indicate level of efficient units to achive and the number of efficient and not efficient units.
#' @param sub_frontier Indicate how many units not efficient it must be created.

#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.

SMOTE_data <- function (
    data, x, y, z = NULL, balance_data
) {

  # ================= #
  # create sub_groups #
  # ================= #
  
  if (is.null(z)) {
    
    # label data without z (only one group)
    
    # save a copy with data_factor (same as z case)
    data_save <- data
    
    # divide in groups by z
    dfs <- list(data)
    
  } else {
    
    # get the names z
    contx_name <- names(data)[z]
    
    # save a copy with data_factor
    data_save <- data
    
    # divide in groups by z
    dfs <- data %>%
      group_split(across(all_of(contx_name)), .keep = TRUE)
    
  }

  # save all data labeled
  data_labeled <- as.data.frame(matrix(
    data = NA,
    ncol = ncol(data),
    nrow = 0
  ))
  
  # set names
  names(data_labeled) <- names(data)
 
  # first, determine the efficient facets for each sub_group
  facets <- convex_facets(
    data = dfs,
    x = x,
    y = y,
    z = z
  )

  # second, populate the efficient facets
  balance_datasets <- get_SMOTE_DMUs(
    data = dfs,
    facets = facets,
    x = x,
    y = y,
    z = z,
    balance = balance_data
  )
  
  if (any(is.na(balance_datasets))) {browser()}

  return(balance_datasets)
}
