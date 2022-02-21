#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericVector lower_bound(
    NumericVector isoy,
    NumericVector cc,
    IntegerVector part_lwr,
    IntegerVector ind_to_block
) {
  // Input:
  //     isoy: the isotonic regression
  //     cc: vector of constants [sqrt(2 * log((N^2 + N) / alpha) / k),
  //         k = 1,...,n]
  //     part_lwr: the first index in each partition block
  //     part_upr: the last index in each partition block
  
  int n = isoy.length();
  int m = part_lwr.length();
  NumericVector lwr(n, 0.0); // vector of lower bounds
  NumericVector csumy(m, 0.0); // cumulative sums of isoy over k:(k+m-1)
  double tmp = 0.0; // current value of bound
  
  // first iteration
  tmp = isoy[0] - cc[0];
  csumy[0] = isoy[0];
  lwr[0] = std::max(tmp, 0.0);
  
  // loop
  int lwr_current; // lower bound of current partition block
  for (int k = 1; k < n; k++) {
    tmp = 0.0;
    for (int j = ind_to_block[k]; j >= 0; j--) {
      lwr_current = part_lwr[j];
      csumy[j] = csumy[j] + isoy[k];
      tmp = std::max(csumy[j] / (k - lwr_current + 1.0) - cc[k - lwr_current], tmp);
    }
    lwr[k] = tmp;
  }
  
  return lwr;
}


//[[Rcpp::export]]
NumericVector upper_bound(
    NumericVector isoy,
    NumericVector cc,
    IntegerVector part_upr,
    IntegerVector ind_to_block
) {
  int n = isoy.length();
  int m = part_upr.length();
  NumericVector upr(n, 1.0);
  NumericVector csumy(m, 0.0);
  double tmp = 0.0;
  double tmp_y;
  int n1 = n - 1;
  int ind;
  
  tmp = isoy[n1] + cc[0];
  tmp_y = isoy[n1];
  csumy[m - 1] = tmp_y;
  upr[n1] = std::min(tmp, 1.0);
  
  int upr_current;
  for (int k = 1; k < n; k++) {
    ind = n1 - k;
    tmp_y = isoy[ind];
    tmp = 1.0;
    for (int j = ind_to_block[ind]; j < m; j++) {
      upr_current = part_upr[j];
      csumy[j] = csumy[j] + tmp_y;
      tmp = std::min(csumy[j] / (upr_current - ind + 1.0) + cc[upr_current - ind], tmp);
    }
    upr[ind] = tmp;
  }
  
  return upr;
}