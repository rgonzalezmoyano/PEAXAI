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
#' @param z Column indexes of environment variables in \code{data} (optional).
#' @param balance A numeric vector indicating the different levels of balance required (e.g., c(0.1, 0.45, 0.6)).
#' 
#' @importFrom dplyr anti_join
#' 
#' @return A \code{list} where each element corresponds to a balance level, containing a single \code{data.frame} 
#' with the real and synthetic DMUs, correctly labeled.

get_SMOTE_DMUs <- function (
    data, facets, x, y, z = NULL, balance_levels = NULL
) {

  copy_data <- data
  
  save_dataset_balanced <- vector("list", length = length(balance_levels))
  
  names(save_dataset_balanced) <- as.character(balance_levels)

  # we need to determine, for each balance level, the number of synthetic DMUs to create
  for (balance in balance_levels) {
    
    save_dataset <- as.data.frame(matrix(
      data = NA,
      ncol = ncol(copy_data[[1]]),
      nrow = 0
    ))
    
    names(save_dataset) <- names(copy_data[[1]])
    
    for(sub_group in 1:length(copy_data)) {
      print(paste("sub_group:", sub_group))
      print(paste("Balance", balance))
      cat("\n")
      
      #if(sub_group == 13 & balance == 0.8) {browser}
      data <- copy_data
      # =========================================================== #
      # determinate number of efficient and not efficient to create #
      # =========================================================== #
      data <- data[[sub_group]]
      
      if(nrow(facets[[sub_group]]) == 0) {
        print(paste("No facets", sub_group))
        
        save_dataset <- rbind(save_dataset, data)
        next
        
      }

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
        
        print(paste("Balance perfect in", sub_group))
        
        save_dataset <- rbind(save_dataset, data)
        next
        
      }
      
      # ============================================ #
      # get index to create efficient synthetic DMUs #
      # ============================================ #
      # if(sub_group == 3){browser()}
      # total combinations 
      # eff data
      data_eff <- data[data$class_efficiency == "efficient", ]
      
      # real efficient combination
      idx <- facets[[sub_group]]
      n_idx <- 1:nrow(idx)
      
      # number of efficient DMUs
      n_eff <- nrow(data_eff)
      
      # create units
      # lambda
      
      # proportion importance
      len <- ncol(facets[[sub_group]])
      
      prop_imp <- 1/len
      
      lambda <- rep(prop_imp, ncol(facets[[sub_group]]))
      
      n_comb <- nrow(data_eff)
      
      combinations <- as.data.frame(t(combn(n_comb, len)))
      
      if (sense_balance == "not_efficient") {

        # delete combinations efficient
        #combinations <- anti_join(combinations, idx, by = names(idx))
        
        # select  k-create_ineff
        if (nrow(combinations) > (create_ineff * 3)) {
          
          idx_combinations <- sample(x = 1:nrow(combinations), size = (create_ineff * 3), replace = FALSE)
          
          idx_ineff <- combinations[idx_combinations,]
          
          idx <- anti_join(idx_ineff, idx, by = names(idx))
          
          idx <- na.omit(idx)
          
        } else if ((nrow(combinations) - length(n_idx)) < create_ineff &
                   nrow(combinations) == length(n_idx)){
          
          # like there are less possible combinations than units, not possible to create, we need more facts
          # print("bro")
          # browser()
          
          print(paste("No possible create not efficient units in", sub_group))
          
          save_dataset <- rbind(save_dataset, data)
          next
          
        } else if ((nrow(combinations) - length(n_idx)) < create_ineff &
                   nrow(combinations) > length(n_idx)) {
          
          print(paste("No possible create not efficient units in", sub_group))
          
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
      check_results_convx <- which(compute_scores_additive(results_convx, x = x, y = y) < 0.0001)
      
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
        
        # names(save_lambda_eff) <- names(data)[c(x,y)]
        # 
        # # Fisrt, I paste the convex combinations
        # save_lambda_eff <- rbind(save_lambda_eff, results_convx )
        # 
        # # Second, I search new efficient combinations
        count_browser <- 0
        while (nrow(save_lambda_eff) < need_eff) {
          count_browser <- count_browser + 1
          #print(count_browser)
          #if(count_browser == 5000) {browser()}
          print((nrow(save_lambda_eff)/need_eff)*100)
          # if(sub_group == 13 & balance == 0.7) {browser}
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
          
          # check
          check_data <- rbind(data_eff[, c(x,y)], new_unit)
          
          check_test <- compute_scores_additive(check_data, x = x, y = y)
          
          # save if is correct
          if (check_test[nrow(data_eff) + 1,] < 0.0001) {
            
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
      
      # add context if it is necessary
      if (!(is.null(z))) {
        # create contex information
        
        vector_contex <- unique(data[,z])
        
        n_col <- length(vector_contex)
        
        contex <- as.data.frame(matrix(
          data = NA,
          nrow = nrow(results_convx),
          ncol = n_col
        ))
        
        names(contex) <- names(vector_contex)
        
        for (colum_contex in 1:length(contex)) {
          
          contex[, colum_contex] <- rep(as.numeric(vector_contex[,colum_contex]), nrow(results_convx)) 
          
        }
        
        new_data <- cbind(results_convx, contex)
        
      } else {
        
        # no z case
        new_data <- results_convx
        
      }
      
      if(sense_balance == "not_efficient") {
        
        new_data$class_efficiency <- "not_efficient"
        
      } else {
        
        new_data$class_efficiency <- rep("efficient", nrow(results_convx))
        
      }
      
      new_data_completed <- rbind(data, new_data)
      
      save_dataset <- rbind(save_dataset, new_data_completed)
      
    } # end loop sub_group
    
    save_dataset_balanced[[as.character(balance)]] <- save_dataset
    
  } # end loop balance
    
  return(save_dataset_balanced)
  
}
