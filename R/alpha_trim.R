
# Determines an alpha-trimmed tree structure for an object of class "ranger"
# Currently only accepts a single value of alpha
# Written by Nikola Surjanovic

#' @export

alpha.trim <- function(object, alpha=1, criterion="BIC", tree.str.list.in=NULL) {
  # Retrieve object(s)
  forest <- object$forest
  
  # Basic input checks
  if (!inherits(object, "ranger")) stop("'object' is not of class 'ranger'.")
  if (alpha < 0) stop("'alpha' must be non-negative.")
  if (!(criterion %in% c("AIC", "BIC", "CC"))) stop("'criterion' must be one of 'AIC', 'BIC', or 'CC'.")
  
  # Start trimming
  if (is.null(tree.str.list.in)) {
    tree.str.list.in <- vector(mode='list', length=object$num.trees)
    for (tree in 1:object$num.trees) {
      tree.str.list.in[[tree]] <- treeInfo(object=object, tree=tree)
    }
  }
  tree.str.list <- alphaTrim(tree_str_list_in=tree.str.list.in, alpha=alpha, criterion=criterion)
  
  result <- list(tree.str.list = tree.str.list,
                 ranger.mod = object)
  class(result) <- c("RFtrim", class(result)) # Add class "RFtrim"
  return(result)
}
