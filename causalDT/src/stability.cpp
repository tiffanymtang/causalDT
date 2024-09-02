#include <Rcpp.h>
using namespace Rcpp;

//' Jaccard similarity index
//'
//' @description
//' This function computes the Jaccard similarity index between two vectors of
//' cluster membership labels.
//'
//' @param x Numeric vector of cluster memberships (encoded as integers).
//' @param y Numeric vector of cluster memberships (encoded as integers).
//'
//' @return Computed Jaccard similarity metric according to Ben-Hur (2001).
//'
//' @export
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double jaccardCPP(NumericVector x, NumericVector y) {
  // length of x vector; number of samples
  int n = x.size();

  // initialize jaccard output
  double jaccard = 0.0;

  // initialize 2x2 N matrix where N = [N_00, N_01; N_10, N_11]
  NumericMatrix N(2);
  N(0, 0) = n; // accounting for diagonal elements being 0's a priori
  // compute N matrix (for details see Ben-Hur 2001)
  for (int i = 0; i < n; i++) {   // for each row of sim_mat
    for (int j = i+1; j < n; j++) { // for each column, but for efficiency,
      // use symmetry & no need to check diagonals
      int Cx_ij = x[i] == x[j];
      int Cy_ij = y[i] == y[j];
      N(Cx_ij, Cy_ij) += 2; // because of symmetry
    }
  }

  jaccard = N(1, 1) / (N(0, 1) + N(1, 0) + N(1, 1));
  return jaccard;
}
