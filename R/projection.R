#' @title Projections to the Hyperplane
#'
#' @description This function computes the efficiency scores based on a given model.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param z Column indexes of environment variables in \code{data}.
#' @param final_model The best-fitted model used for efficiency score computation.
#' @param orientation The direction in which data should be projected to calculate efficiency scores.
#' @param cut_off Probability levels for determining efficient class scores.
#'
#' @return A numeric vector containing the efficiency scores for each observation in the input data.
#'
#' @export

compute_scores <- function (
    data, x, y, z = NULL, final_model, orientation, cut_off
) {
    
    # environment variable 
    if (is.null(z)) {
      z <- 0
    }
  
    # vector of optimal scores
    scores <- matrix(NA, nrow = nrow(data), ncol = 1)
    
    # maximum and minimum values posibles a esperar
    max_value_x <- apply(X = as.matrix(data[, x]), MARGIN = 2, FUN = max)
    min_value_x <- apply(X = as.matrix(data[, x]), MARGIN = 2, FUN = min)
    
    max_value_y <- apply(X = as.matrix(data[, y]), MARGIN = 2, FUN = max)
    min_value_y <- apply(X = as.matrix(data[, y]), MARGIN = 2, FUN = min)
    
    for (i in 1:nrow(data)) {

      print(paste("DMU: ", i))
      print(paste("En curso:", (i/nrow(data)) * 100))
      data[i,y] <- min_value_y
      #print(data[i,])
      
      # probability of being efficient
      prob_eff <- predict(final_model, data[i, ], type = "prob")[1]
      
      incr <- 0
      
      if (orientation == "input") {
        
        # if (prob_eff > 0.5) {
        #   
        #   # ======================== #
        #   # case dmu super-efficient #
        #   # ======================== #
        #     
        #     # calculate increments to make the efficient class the minority
        #     while (prob_eff > 0.5) {
        #       
        #       # Check if it exceed the minimum observed values
        #       if (any(data[i, x] + (data[i, x] * incr) > max_value_x)) {
        #         
        #         # probability of being efficient
        #         prob_eff <- predict(final_model, data[i, ], type = "prob")[1]
        #         incr <- 0
        #         
        #         while (prob_eff > 0.5) {
        #           
        #           incr <- incr + 0.01
        #           
        #           # the dmu with the increments
        #           new_point <- cbind(data[i, x] * (1 + incr), data[i, y])
        #           colnames(new_point) <- names(data[c(x, y)])
        #           
        #           prob_eff <- predict(final_model, new_point, type = "prob")[1]
        #           
        #           if (any(data[i, x] + (data[i, x] * incr) > max_value_x * 2)) {
        #             
        #             scores[i] <- NA
        #             break
        #             
        #           }
        #           
        #           scores[i] <- 1 + incr - 0.005
        #           
        #         }
        #         
        #         break
        #         
        #       }
        #       
        #     } # end first while
        #       
        #       # Once the threshold is crossed, make the majority class efficient again
        #       if (!(any(data[i, x] + (data[i, x] * incr) > max_value_x * 2))) {
        #           
        #         while (prob_eff < 0.5) {
        #           
        #           # Increase by 0.1
        #           incr <- incr - 0.01
        #           
        #           # the dmu with the increments
        #           new_point <- cbind(data[i, x] * (1 + incr), data[i, y])
        #           colnames(new_point) <- names(data[c(x, y)])
        #           
        #           prob_eff <- predict(final_model, new_point, type = "prob")[1]
        #           
        #         }
        #         
        #         scores[i] <- 1 + (incr - 0.005) 
        #     
        #     } # end dmu super efficient
        #     
        #   } else {
        #     
        #     # ====================== #
        #     # case dmu not efficient #
        #     # ====================== #
        #     
        #     # calculate increments to make not efficient class the minority
        #     while (prob_eff < 0.5) {
        #       
        #       # increase by 0.1
        #       incr <- incr + 0.1
        #       
        #       # case inputs reduction not available
        #       if (incr > 1) {
        #         
        #         scores[i] <- NA
        #         break
        #         
        #       }
        #       
        #       # the dmu with the increments of 
        #       new_point <- cbind(data[i, x] * (1 - incr), data[i, y])
        #       colnames(new_point) <- names(data[c(x, y)])
        #       
        #       prob_eff <- predict(final_model, new_point, type = "prob")[1]
        #       
        #     }
        #     
        #     if (!(incr > 1)) {
        #       
        #       # Once the threshold is crossed, make the majority class non-efficient again
        #       while (prob_eff > 0.5) {
        #         
        #         # Increase by 0.1
        #         incr <- incr - 0.01
        #         
        #         # the dmu with the increments
        #         new_point <- cbind(data[i, x] * (1 - incr), data[i, y])
        #         colnames(new_point) <- names(data[c(x, y)])
        #         
        #         prob_eff <- predict(final_model, new_point, type = "prob")[1]
        #         
        #       }
        #       
        #       scores[i] <- 1 - (incr + 0.005)
        #       
        #     }
        #     
        #   }
          
        } else { # OUTPUT ORIENTATION
          
          if (prob_eff > cut_off) {
            
            # ======================== #
            # case dmu super-efficient #
            # ======================== #
            
            scores[i] <- 1
            
            while (prob_eff > cut_off) {
              
              if (any(data[i, y] - (data[i, y] * incr) < 0)) {
                
                scores[i] <- NA
                break
                
              } else {
                
                # Increase by 0.1
                incr <- incr + 0.01
                
                # the dmu with the increments of 
                new_point <- cbind(data[i, x], data[i, y] * (1 - incr), data[i, z])
                colnames(new_point) <- names(data[c(x, y, z)])
                
                prob_eff <- predict(final_model, new_point, type = "prob")[1]
                
              }
              
              scores[i] <- 1 - (incr - 0.005)
              
            } # end while
            
          } else {
            
            # ====================== #
            # case dmu not efficient #
            # ====================== #
            
            # calculate increments to make not efficient class the minority
            while (prob_eff < cut_off) {

              if (any(data[i, y] + (data[i, y] * incr) > max_value_y * 2)) {
                
                scores[i] <- NA
                break
                
              } else {
                
                # Increase by 0.1
                incr <- incr + 0.01
                
                # the dmu with the increments of 
                new_point <- cbind(data[i, x], data[i, y] * (1 + incr), data[i, z])
                colnames(new_point) <- names(data[c(x, y, z)])
                
                prob_eff <- predict(final_model, new_point, type = "prob")[1]
                
              }
              
              scores[i] <- 1 + (incr - 0.005)
              
            } # end while
            
          } # end case inefficient
          
        } # end output orientation
        
      } # end bucle for
      
  return(scores)
      
}
