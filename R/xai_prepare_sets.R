#' @title Prepare Training and Target Datasets from a caret Model
#'
#' @description
#' Extracts and formats the training and/or target datasets from a machine learning model trained with \code{caret::train},
#' allowing for distinction between using the full training data or only the original subset used for modeling.
#' It standardizes the class column to be named \code{"class_efficiency"} and positions it as the last column.
#'
#' @param data A \code{data.frame} containing the original dataset used to train the model. Only needed when using \code{"real"} as background or target.
#' @param x Not currently used. Reserved for future input variable selection.
#' @param y Not currently used. Reserved for future output variable specification.
#' @param final_model A trained model object of class \code{"train"} from the \pkg{caret} package.
#' @param background A character string, either \code{"train"} or \code{"real"}, specifying the background dataset used for explainability.
#' @param target A character string, either \code{"train"} or \code{"real"}, specifying the target dataset to be explained.
#' @param type Not currently used. Reserved for future prediction types.
#' @param threshold Not currently used. Reserved for future thresholding logic.
#' @param levels_order A character vector specifying the levels of the response factor, typically \code{c("not_efficient", "efficient")}. Not currently used, but can help in reordering or relabeling.
#'
#' @return A \code{list} with two elements:
#' \describe{
#'   \item{\code{train_data}}{A \code{data.frame} representing the background dataset, with the class column renamed to \code{"class_efficiency"} and positioned last.}
#'   \item{\code{target_data}}{A \code{data.frame} representing the target dataset, formatted in the same way.}
#' }
#'
#' @importFrom caret train
#'
#' @examples
#' \dontrun{
#' model <- caret::train(y ~ ., data = my_data, method = "rf")
#' result <- xai_prepare_sets(
#'   data = my_data,
#'   x = NULL, y = NULL,
#'   final_model = model,
#'   background = "train",
#'   target = "real",
#'   type = NULL,
#'   threshold = NULL,
#'   levels_order = c("not_efficient", "efficient")
#' )
#' str(result$train_data)
#' str(result$target_data)
#' }
#'
#' @export

xai_prepare_sets <- function (
    data, x, y, final_model, background, target,
    type, threshold, levels_order
    ) {

  # domain set; by default is "train"
  if (background == "train") {

    # a ML model train by caret
    # save train data, change name and position
    train_data <- final_model[["trainingData"]]
    names(train_data)[1] <- "class_efficiency"

    train_data <- train_data[,c(2:length(train_data),1)]

  } else if (background == "real") {

    n <- nrow(data)

    # a ML model train by caret
    # save train data, change name and position
    train_data <- final_model[["trainingData"]][1:n,]
    names(train_data)[1] <- "class_efficiency"

    train_data <- train_data[,c(2:length(train_data),1)]


  }

  # target data set; by default is "train"
  if (target == "train") {

    # a ML model train by caret
    # save train data, change name and position
    target_data <- final_model[["trainingData"]]
    names(target_data)[1] <- "class_efficiency"

    target_data <- target_data[,c(2:length(target_data),1)]

  } else if (target == "real") {

    n <- nrow(data)

    # a ML model train by caret
    # save train data, change name and position
    target_data <- final_model[["trainingData"]][1:n,]
    names(target_data)[1] <- "class_efficiency"

    target_data <- target_data[,c(2:length(target_data),1)]

  }

  output <- list(
    train_data = train_data,
    target_data = target_data
  )

  return(output)

}
