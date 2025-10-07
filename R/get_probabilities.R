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
#' @param imp_vector A \code{data.frame} with importance variables results 
#'
#' @return A numeric vector containing the efficiency scores for each observation in the input data.
#'
#' @export

compute_target <- function (
    data, x, y, z = NULL, final_model, orientation, cut_off, imp_vector
) {
    
    # change output model by model: regression vs ML
    if(c("glm") %in% class(final_model)) {
      output_model <- "response"
    } else {
      output_model <- "prob"
    }
  
  if (is.null(z)) {
    z <- 0
  }
    
    # save data points from scenario
    data_scenario <- as.data.frame(
      matrix(
        data = NA,
        ncol = length(c(x,y)),
        nrow = nrow(data)
      )
    )
   
    names(data_scenario) <- names(data)
 
    # JA aproach
    mean_x <- apply(as.matrix(data[,x]), 2, mean)
    mean_y <- apply(as.matrix(data[,y]), 2, mean)
   
    # #BORRARR
    # mean_x[1:4] <- rep(1, 4)
    # mean_y[1] <- 1

    score_imp_x <- as.numeric(imp_vector[x])
    score_imp_y <- as.numeric(imp_vector[y])
  
    betas <- as.data.frame(matrix(
      data = NA,
      ncol = 2,
      nrow = nrow(data)
    ))
    
    length_betas <- 100

    variables <- NULL
    if (z == 0) {
      variables <- c(x, y)
    } else {
      variables <- c(x, y, z:length(data))
    }
    
    data <- as.data.frame(data)
    
    # loop for each observation
    for (i in 1:nrow(data)) {
      # if (i == 4) {browser()}
      print(paste("DMU: ", i))
      print(paste("En curso:", (round(i/nrow(data), 4) * 100)))

      if (predict(final_model, data[i,variables], type = output_model)[1] > cut_off) {
        betas[i, 1] <- 0
        betas[i, 2] <- cut_off
        
        data_scenario[i,] <- data[i,]
        
      } else {
    
        # Inicializar el rango inicial de 'y'
        range_beta <- as.matrix(seq(from = 0, to = 10, length.out = length_betas)) # 20 base
        
        # Crear la matriz para aplicar predict()
        matrix_eff <- as.data.frame(matrix(
          data = NA,
          ncol = length(c(x, y)),
          nrow = length(range_beta)
        ))
        
        # Nombrar las columnas como en data original
        names(matrix_eff) <- names(data)
        
        change_x <- matrix(
          data = rep((-score_imp_x) * mean_x, each =  nrow(matrix_eff)),
          nrow = nrow(matrix_eff),
          ncol = length(mean_x)
        )
       
        # change_x <- matrix(
        #   data = rep((-data[i,x]) * mean_x, each =  nrow(matrix_eff)),
        #   nrow = nrow(matrix_eff),
        #   ncol = length(mean_x)
        # )
        
        change_y <- matrix(
          data = rep((score_imp_y) * mean_y, each = nrow(matrix_eff)),
          nrow = nrow(matrix_eff),
          ncol = length(mean_y)
        )
        
        # change_y <- matrix(
        #   data = rep((-data[i,y]) * mean_y, each =  nrow(matrix_eff)),
        #   nrow = nrow(matrix_eff),
        #   ncol = length(mean_y)
        # )
        
        found_cut_off <- FALSE
        iter_count <- 0
        
        while (!found_cut_off) {
         
          iter_count <- iter_count + 1
          # print(iter_count)
          
          if (z == 0) {
            
            # Crear la matriz para aplicar predict()
            matrix_eff <- as.data.frame(matrix(
              data = NA,
              ncol = length(c(x, y)),
              nrow = length(range_beta)
            ))
            
          } else {
            
            # Crear la matriz para aplicar predict()
            matrix_eff <- as.data.frame(matrix(
              data = NA,
              ncol = length(c(x, y, z)),
              nrow = length(range_beta)
            ))
            
          }
          
          # Nombrar las columnas como en data original
          names(matrix_eff) <- names(data)
          # if (i == 4) {
          #   browser()
          # }
          
          # Asignar valores para 'x' y 'y'
          matrix_eff[, x] <- data[i,x] 
          # browser()
          # change_x <- matrix(unlist(change_x), nrow = dim(change_x)[1], ncol = dim(change_x)[2])
       
          matrix_eff[, x] <- sweep(change_x, 1, range_beta, "*") + matrix_eff[, x]
          
          matrix_eff[, y] <- data[i, y] 
          
          matrix_eff[, y] <- sweep(change_y, 1, range_beta, "*") + matrix_eff[, y]
          
          if (!(z == 0)) {matrix_eff[, z] <- data[i, z]}
      
          # know if there are not posible values
          min_x <- apply(as.matrix(data[,x]), 2, min)

          min_x_matrix <- matrix(rep(min_x, each = length(range_beta)), ncol = length(min_x), byrow = FALSE)
          
          colnames(min_x_matrix) <- colnames(data[, x])
   
          if (any(which(matrix_eff[,x] < min_x_matrix))) {
            
            select_idx <- matrix(
              data = FALSE,
              ncol = 1,
              nrow = nrow(matrix_eff)
            )
            
            for (idx in 1:nrow(select_idx)) {
              if (any(matrix_eff[idx,x] < min_x_matrix[idx,])) {
                select_idx[idx, ] <- TRUE
              }
            }
            
            delete_range_beta <- which(select_idx == TRUE)
      
            #delete_range_beta <- which(apply(matrix_eff[,x], 1, function(x) any(x < 0)))
            
            range_beta <- range_beta[-delete_range_beta,]
            matrix_eff <- matrix_eff[-delete_range_beta,]
            
          }
          
          #matrix_eff <- as.data.frame(matrix_eff)
          
          # Calcular probabilidad de eficiencia para cada fila
          eff_vector <- apply(matrix_eff, 1, function(row) {
           
            row_df <- as.data.frame(t(row))
            colnames(row_df) <- names(data)
           
            if (!(z == 0)) {row_df <- change_class(data = row_df, to_numeric = c(x,y), to_factor = z)}
            
            pred <- unlist(predict(final_model, row_df, type = output_model)[1])
            
            return(pred)
          })
          
          # no changes case
          if(eff_vector[1] == eff_vector[length(eff_vector)]) {
            
            data_scenario[i, x] <- data[i,x]
            data_scenario[i, y] <- data[i,y]
            
            betas[i, 1] <- 0
            betas[i, 2] <- eff_vector[1]
            break
          }
          
          if (length(eff_vector) == 0 | is.null(eff_vector)) {
            
            data_scenario[i, x] <- rep(NA, ncol(matrix_eff[,x]))
            data_scenario[i, y] <- rep(NA, ncol(matrix_eff[,y]))
            
            betas[i, 1] <- NA
            betas[i, 2] <- NA
            break
            
          } else {
            
            # Verificar si alguna predicción coincide con el cut_off
            if (any(round(eff_vector, 10) == round(cut_off, 10))) {
              idx <- which(round(eff_vector, 7) == round(cut_off, 7))[1]  # Tomamos la primera coincidencia
              found_cut_off <- TRUE
              
              # Guardar los valores de 'x' y 'y' que coinciden con el cut_off
              data_scenario[i, x] <- matrix_eff[idx, x]
              data_scenario[i, y] <- matrix_eff[idx, y]
             
              betas[i, 1] <- range_beta[idx]
              betas[i, 2] <- cut_off
              # print("end while")
              break
              
            } else {
              
              # Encontrar el intervalo donde se encuentra el cut_off
              pos <- which(eff_vector < cut_off & c(eff_vector[-1], Inf) > cut_off) # [1] 
             
              if (length(pos) == 2) {
                pos <- pos[1]
              } else if (length(pos) == 3) {
                pos <- pos[2]
              } else if (length(pos) > 3){
                pos <- pos[as.integer(length(pos)/2)]
              } else if (length(pos) == 0) {
                pos <- NA
              }
              
              if (is.na(pos)) {
                #range_beta <- range_beta *2 
                # range_beta <- seq(from = range_beta[pos], to = range_beta[pos + 1], length.out = length(range_beta))
                break
              } else {
                # if (i == 26) {browser()}
                if (pos == length(eff_vector)) {
                  # if (i == 64) {browser()}
                  # no more probability to be efficient
                  # save best results
                  data_scenario[i, x] <- matrix_eff[pos, x]
                  data_scenario[i, y] <- matrix_eff[pos, y]
                  #data_scenario[i, z] <- matrix_eff[pos, z]
                  
                  betas[i, 1] <- range_beta[pos]
                  
                  #pred <- change_class(data_scenario[i,], to_factor = z)
                  pred <- as.data.frame(data_scenario[i,c(x,y)]) # borrar por la de arriba
                  names(pred) <- names(data)
                  
                  pred_max <- unlist(predict(final_model, pred, type = output_model)[1])
                  betas[i, 2] <- pred_max
                  break
                }
                
                # Refinar el rango de 'y' entre las posiciones pos y pos + 1
                range_beta <- seq(from = range_beta[pos], to = range_beta[pos + 1], length.out = length_betas)
              }
              
            }
            
          }
          
          if (range_beta[length_betas] - range_beta[1] < 0.0000000001) {
            
            data_scenario[i, x] <- matrix_eff[(length_betas/2), x]
            data_scenario[i, y] <- matrix_eff[(length_betas/2), y]
            
            betas[i, 1] <- range_beta[(length_betas/2)]
            betas[i, 2] <- eff_vector[(length_betas/2)]
            
            # print("end while by dif")
            found_cut_off <- TRUE
            
          }
          
          if (iter_count == 20) {
            
            data_scenario[i, x] <- matrix_eff[(length_betas/2), x]
            data_scenario[i, y] <- matrix_eff[(length_betas/2), y]
            
            betas[i, 1] <- range_beta[(length_betas/2)]
            betas[i, 2] <- NA
            
            # print("end while by iter")
            found_cut_off <- TRUE
            
          }
          
        } # end while
        
      }
    
    } # end for

    names(betas) <- c("beta", "probability")

    # data_scenario <- cbind(data_scenario, betas)
  
  return(list(data_scenario = data_scenario, betas = betas)) 
      
}
