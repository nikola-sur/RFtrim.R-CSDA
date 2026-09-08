
// Written by Nikola Surjanovic

#include <Rcpp.h>
//[[Rcpp::export]]

Rcpp::NumericMatrix predictAllTrees(const Rcpp::NumericMatrix data, const Rcpp::List tree_str_list, 
                                    const Rcpp::List sample_inds, const Rcpp::NumericVector column_names) {
  // Import data and extract information
  const bool bootstrap = (sample_inds.size() > 1);
  const int num_trees = tree_str_list.size();
  const int n = data.nrow();
  
  if ((num_trees == 1) & (bootstrap == 0)) Rcpp::warning("Not yet implemented.");
  
  
  // Declare data and variables
  bool done, move_left;
  int node;
  Rcpp::NumericMatrix out(n, num_trees);
  Rcpp::LogicalVector inds;
  

  // Make predictions
  for (int tree=0; tree < num_trees; tree++) {
    if (bootstrap) {
      inds = sample_inds[tree];
    }
    const Rcpp::List tree_str = Rcpp::as<Rcpp::List>(tree_str_list[tree]);
    const Rcpp::LogicalVector terminal = Rcpp::as<Rcpp::LogicalVector>(tree_str["terminal"]);
    const Rcpp::CharacterVector splitvarName = Rcpp::as<Rcpp::CharacterVector>(tree_str["splitvarName"]);
    const Rcpp::NumericVector splitval = Rcpp::as<Rcpp::NumericVector>(tree_str["splitval"]);
    const Rcpp::NumericVector leftChild = Rcpp::as<Rcpp::NumericVector>(tree_str["leftChild"]);
    const Rcpp::NumericVector rightChild = Rcpp::as<Rcpp::NumericVector>(tree_str["rightChild"]);
    const Rcpp::NumericVector mean_y = Rcpp::as<Rcpp::NumericVector>(tree_str["mean.y"]);
    Rcpp::NumericVector columnNumber(splitvarName.length());
    
    for (int nd=0; nd < splitvarName.length(); nd++) {
      Rcpp::String temp_name = splitvarName[nd];
      if (!(temp_name == NA_STRING)) {
        columnNumber[nd] = column_names[temp_name] - 1; // Indexes start from zero
      }
    }
    
    for (int i=0; i<n; i++) {
      if (bootstrap && (inds[i] == 1)) { // Don't make predictions for "in bag" observations
        out(i, tree) = NA_REAL;
        continue;
      }

      done = 0;
      node = 0; // Start at the root node
      
      while (!done) {
        if (terminal[node] == 1) {
          done = 1;
        } else {
          move_left = (data(i, columnNumber[node]) <= splitval[node]);
          
          if (move_left) { // Go to left child
            node = leftChild[node];
          } else { // Go to right child
            node = rightChild[node];
          }
        }
      }
      out(i, tree) = mean_y[node];
    }
  }
  return out;
}
