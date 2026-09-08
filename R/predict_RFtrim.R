
# Makes predictions on an object of class "RFtrim"
# Some parts based on the predict() function from the 'ranger' R package
# 'sample.inds' is used for calculating OOB errors
# Written by Nikola Surjanovic

#' @export

predict.RFtrim <- function(object, data=NULL) {
  if (is.null(data)) {
    bootstrap <- TRUE
    sample.inds <- lapply(object$ranger.mod$inbag.counts, FUN=function(x) x > 0) # For OOB errors
    data <- object$ranger.mod$data
  } else {
    bootstrap <- FALSE
    sample.inds <- list(rep(TRUE, nrow(data)))
  }
  
  
  # Basic input checks
  if (!inherits(object, "RFtrim")) stop("'object' is not of class 'RFtrim'.")
  if (sum(!(object$ranger.mod$forest$independent.variable.names %in% colnames(data)) > 0)) # Line from 'ranger' package
    stop("Not all variables were found in the provided data set.")
  
  
  # Prepare data
  column.names <- 1:ncol(data)
  names(column.names) <- colnames(data)
  factor.levels <- object$ranger.mod$factor.levels
  
  for (j in 1:ncol(data)) {
    if (is.factor(data[1, j])) {
      if (!identical(levels(data[1, j]), factor.levels[[colnames(data)[j]]])) 
        warning(paste0("Missing factor level in variable ", colnames(data)[j], "."))
      data[, j] <- match(data[, j], factor.levels[[colnames(data)[j]]])      
    }
  }
  data <- as.matrix(data)
  
  
  # Make predictions
  pred.mat <- predictAllTrees(data=data, tree_str_list=object$tree.str.list, 
                               sample_inds=sample.inds, column_names=column.names)
  
  if (bootstrap) {
    result <- as.numeric(base::rowMeans(pred.mat, na.rm=TRUE))
  } else {
    result <- as.numeric(base::rowMeans(pred.mat))
  }
  
  return(result)
}