// Written by Nikola Surjanovic

#include <Rcpp.h>
//[[Rcpp::export]]

Rcpp::DataFrame permuteDF(Rcpp::DataFrame data, Rcpp::NumericVector inds, Rcpp::CharacterVector col_names) {
  // Check that the length of 'inds' is the same as the number of rows in 'data'
  if (data.nrows() != inds.size()) Rcpp::warning("The length of 'inds' is not the same as the number of rows in 'data'.");
  
  // Check if inds are out of the correct range
  for (int i=0; i<inds.size(); i++) {
    if (inds[i] >= data.nrows()) Rcpp::stop("'inds' are out of the correct range.");
  }
      
  // Declare output
  Rcpp::DataFrame data_out = Rcpp::clone(data);
  
  // Permute data
  for (int j=0; j<col_names.size(); j++) {
    Rcpp::String col_name = col_names[j];
    Rcpp::NumericVector col_values = data[col_name];
    Rcpp::NumericVector col_values_out(col_values.size());
    for (int i=0; i<data.nrows(); i++) {
      col_values_out[i] = col_values[inds[i]];
    }
    data_out[col_name] = col_values_out;
  }
  
  return data_out;
}

