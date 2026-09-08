
// Written by Nikola Surjanovic

#include <Rcpp.h>
#include <cmath>
//[[Rcpp::export]]

Rcpp::List alphaTrim(const Rcpp::List tree_str_list_in, const double alpha, const Rcpp::String criterion) {
  // Make a deep copy of 'tree_str_list_in'
  Rcpp::List tree_str_list_in2 = Rcpp::clone(tree_str_list_in); // New (right): Copy, but takes up memory!
  // Rcpp::List tree_str_list_in2 = tree_str_list_in; // Old (wrong): lists are pass-by-reference
  
  // Import data and extract information
  const int num_trees = tree_str_list_in2.size();
  
  // Define variables
  const double pi = 3.141592653589; 
  bool merge_crit;
  int l_child, r_child, num_nodes;
  double s2_hat, n_l, n_r, n_0;
  
  
  // Declare export data
  Rcpp::List tree_str_list_out(num_trees);
  
  // Start alpha-trimming
  for (int tree=0; tree < num_trees; tree++) {
    // Import existing tree information
    Rcpp::DataFrame tree_str = Rcpp::as<Rcpp::DataFrame>(tree_str_list_in2[tree]);
    num_nodes = tree_str.nrow();
    Rcpp::LogicalVector terminal = Rcpp::as<Rcpp::LogicalVector>(tree_str["terminal"]);
    const Rcpp::NumericVector leftChild = Rcpp::as<Rcpp::NumericVector>(tree_str["leftChild"]);
    const Rcpp::NumericVector rightChild = Rcpp::as<Rcpp::NumericVector>(tree_str["rightChild"]);
    const Rcpp::NumericVector mean_y = Rcpp::as<Rcpp::NumericVector>(tree_str["mean.y"]);
    const Rcpp::NumericVector n = Rcpp::as<Rcpp::NumericVector>(tree_str["n"]);
    const Rcpp::NumericVector sse = Rcpp::as<Rcpp::NumericVector>(tree_str["sse"]);
    
    // Create new tree information
    Rcpp::NumericVector branch_sse(num_nodes);
    Rcpp::NumericVector I_A(num_nodes);
    
    // Trim this tree
    for (int node=(num_nodes-1); node >= 0; node--) {
      if (terminal[node] == 1) {
        continue; // Only work on internal nodes
      }
      
      // 'I_A' is necessarily missing for this node
      n_0 = n[node];
      if (criterion == "CC") {
        // Cost-complexity: the information quantity is the within-node SSE
        I_A[node] = sse[node];
      } else {
        s2_hat = sse[node]/n_0; // s2_hat under null
        I_A[node] = n_0*log(2*pi*s2_hat) + n_0;
      }
      
      
      l_child = leftChild[node];
      r_child = rightChild[node];
      
      if (terminal[l_child] == 1) { // Define 'branch_sse'
        branch_sse[l_child] = sse[l_child];
      }
      if (terminal[r_child] == 1) {
        branch_sse[r_child] = sse[r_child];
      }

      if (criterion == "CC") {
        // Cost-complexity needs no variance estimate; terminal children
        // contribute their own SSE
        if (terminal[l_child] == 1) I_A[l_child] = sse[l_child];
        if (terminal[r_child] == 1) I_A[r_child] = sse[r_child];
      } else {
        // s2_hat under alternative
        s2_hat = (branch_sse[l_child] + branch_sse[r_child])/n[node];
        if (s2_hat < 1e-15) {
          s2_hat = sse[node]/(2*n_0); // s2_hat under null --> BAD IDEA!!!(?)
          if (s2_hat < 1e-15) Rcpp::stop("'s2_hat' is too small. Try increasing 'min.node.size'.");
        }

        if (terminal[l_child] == 1) { // Define 'I_A'
          n_l = n[l_child];
          I_A[l_child] = n_l*log(2*pi*s2_hat) + sse[l_child]/s2_hat;
        }
        if (terminal[r_child] == 1) {
          n_r = n[r_child];
          I_A[r_child] = n_r*log(2*pi*s2_hat) + sse[r_child]/s2_hat;
        }
      }

      
      if (criterion == "AIC") {
        merge_crit = !(I_A[node] - I_A[l_child] - I_A[r_child] > alpha*8);
      } else if (criterion == "BIC") {
        merge_crit = !(I_A[node] - I_A[l_child] - I_A[r_child] > alpha*3*log(n_0));
      } else if (criterion == "CC") {
        merge_crit = !(I_A[node] - I_A[l_child] - I_A[r_child] > alpha);
      }

      if (!merge_crit) { // Keep split
        if (criterion == "AIC") {
          I_A[node] = I_A[l_child] + I_A[r_child] + alpha*8; // Update 'I_A'
        } else if (criterion == "BIC") {
          I_A[node] = I_A[l_child] + I_A[r_child] + alpha*3*log(n_0);
        } else if (criterion == "CC") {
          I_A[node] = I_A[l_child] + I_A[r_child] + alpha;
        }
        branch_sse[node] = branch_sse[l_child] + branch_sse[r_child]; // Update 'branch_sse'
      } else { // Merge nodes
        terminal[node] = 1; // Define new terminal node (*not* a recursive update!)
        branch_sse[node] = 0; // Reset 'branch_sse'
        I_A[node] = 0; // Reset 'I_A'
      }
    }
    
    // Update 'tree_str'
    tree_str["terminal"] = terminal;
    tree_str["branch.sse"] = branch_sse;
    tree_str["I_A"] = I_A;
    
    tree_str_list_out[tree] = tree_str;
  }
  
  return tree_str_list_out;
}
