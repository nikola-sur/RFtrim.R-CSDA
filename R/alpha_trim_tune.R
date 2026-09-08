# Determines an appropriate value of alpha for an alpha-trimmed tree structure for an object of class "ranger"
# 'alpha' is a vector of possible values of alpha
# 'ycol' is the column number for the response in the data used as input to ranger()
# Written by Nikola Surjanovic

#' @export

alpha.trim.tune <- function(object, ycol, alpha=seq(0, 1.5, 0.1), criterion="BIC", verbose=FALSE) {
  forest <- object$forest
  data <- object$data
  
  # Basic input checks
  if (!inherits(object, "ranger")) stop("'object' is not of class 'ranger'.")
  if (any(alpha < 0)) stop("All elements of 'alpha' must be non-negative.")
  if (!(criterion %in% c("AIC", "BIC", "CC"))) stop("'criterion' must be one of 'AIC', 'BIC', or 'CC'.")
  
  oob.mspe <- numeric(length(alpha))
  names(oob.mspe) <- alpha
  object.trim.min <- NA
  
  tree.str.list.in <- vector(mode='list', length=object$num.trees)
  for (tree in 1:object$num.trees) {
    tree.str.list.in[[tree]] <- treeInfo(object=object, tree=tree)
  }
  
  if (verbose) prog.bar <- progress::progress_bar$new(total=length(alpha))
  i <- 1
  oob.mspe.min <- .Machine$double.xmax
  alpha.min <- NA
  for (a in alpha) {
    if (verbose) prog.bar$tick()
    object.trim.temp <- alpha.trim(object=object, alpha=a, criterion=criterion, tree.str.list.in=tree.str.list.in)
    preds <- predict.RFtrim(object=object.trim.temp, data=NULL) # OOB predictions
    oob.mspe[i] <- mean((data[,ycol] - preds)^2)
    if (oob.mspe[i] < oob.mspe.min) {
      oob.mspe.min <- oob.mspe[i]
      object.trim.min <- object.trim.temp
      alpha.min <- a
    }
    i <- i + 1
  }
  rm(i)
  
  result <- list(alpha.min       = alpha.min,
                 object.trim.min = object.trim.min,
                 oob.mspe        = oob.mspe)
  return(result)
}