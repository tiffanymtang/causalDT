#include <Rcpp.h>
using namespace Rcpp;

//' Jaccard subgroup similarity index
//'
//' @description
//' This function computes the Jaccard similarity index between two vectors of
//' subgroup membership labels, scaling it such that each leaf node contributes
//' equal weight to the overall similarity.
//'
//' @param x Numeric vector of subgroup memberships (encoded as integers).
//' @param y Numeric vector of subgroup memberships (encoded as integers).
//'
//' @return Computed Jaccard subgroup similarity metric
//'
//' @export
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double jaccardSSI(NumericVector x, NumericVector y) {
  // length of x vector; number of samples
  int n = x.size();
  // number of clusters/leaf nodes
  int x_size = unique(x).size();
  int y_size = unique(y).size();

  // initialize #clusters x 2 N matrix where
  // N = [N_00, N_10, N_01, N_11] for each leaf node
  NumericMatrix Nx(x_size, 4);
  NumericMatrix Ny(y_size, 4);

  // compute N matrix (for details see Ben-Hur 2001) for each leaf node
  for (int i = 0; i < n; i++) {   // for each sample
    for (int j = i+1; j < n; j++) { // for each column
      int Cx_ij = x[i] == x[j];
      int Cy_ij = y[i] == y[j];
      int N_idx = Cx_ij + 2 * Cy_ij;
      Nx(x[i], N_idx) += 1;
      Ny(y[i], N_idx) += 1;
      Nx(x[j], N_idx) += 1;
      Ny(y[j], N_idx) += 1;
    }
  }

  double jaccardx = 0.0;
  for (int i = 0; i < x_size; i++) {
    double denom = Nx(i, 1) + Nx(i, 2) + Nx(i, 3);
    if (denom != 0) {
      jaccardx += Nx(i, 3) / denom;
    }
  }
  jaccardx /= x_size;

  double jaccardy = 0.0;
  for (int i = 0; i < y_size; i++) {
    double denom = Ny(i, 1) + Ny(i, 2) + Ny(i, 3);
    if (denom != 0) {
      jaccardy += Ny(i, 3) / denom;
    }
  }
  jaccardy /= y_size;

  double jaccard = 0.0;
  jaccard = (jaccardx + jaccardy) / 2;
  return jaccard;
}
