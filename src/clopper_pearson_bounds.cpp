#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericVector cp_lower_bound(NumericVector ys, NumericVector ns, double alpha) {
  // parameters and containers
  int n = ys.length();
  int i_1 = 0;
  double alpha_corr = alpha / (n + 1.0) / n;
  NumericVector lwr_bound (n, 0.0);
  double current_lwr = 0.0;

  // lower bound
  double x = ys[0]; // x (number of successes for CP-bound)
  double n_x_1 = ns[0] - x + 1.0; // n - x + 1 (number failures + 1, for CP-bound)
  double current_x = x;
  double current_n_x_1 = n_x_1;
  double quantile = 0.0;
  lwr_bound[0] = R::qbeta(alpha_corr, current_x, current_n_x_1, 1, 0);
  
  for (int i = 1; i < n; i++) {
    // In the outer loop, x = \sum_{k=0}^i ys[j] and
    // n_x_1 = 1 + \sum_{j=0}^i (ns[j]-ys[j]). This is updated in the inner
    // loop by increasing the lower index in the sum from j=0 until j=i-1,
    // which steps through all index intervals j:i, j=0,...,i-1.
    // At the next iteration of the outer loop, x and n_x_1 are updated to
    // x = \sum_{k=0}^{i+1} ys[j] and
    // n_x_1 = 1 + \sum_{j=0}^{i+1} (ns[j]-ys[j]).
    i_1 = i - 1;
    x = x + ys[i];
    n_x_1 = n_x_1 + ns[i] - ys[i];
    current_x = x;
    current_n_x_1 = n_x_1;
    quantile = R::qbeta(alpha_corr, x, n_x_1, 1, 0);
    current_lwr = std::max(lwr_bound[i_1], quantile);
    for (int j = 0; j < i; j++) {
      current_x = current_x - ys[j];
      current_n_x_1 = current_n_x_1 + ys[j] - ns[j];
      quantile = R::qbeta(alpha_corr, current_x, current_n_x_1, 1, 0);
      current_lwr = std::max(current_lwr, quantile);
    }
    lwr_bound[i] = current_lwr;
    Rcpp::checkUserInterrupt();
  }

  // return
  return lwr_bound;
}

//[[Rcpp::export]]
NumericVector cp_upper_bound(NumericVector ys, NumericVector ns, double alpha) {
  int n = ys.length();
  int i_1 = 0;
  int n_1 = n - 1;
  double alpha_corr = 1 - alpha / (n + 1.0) / n;
  NumericVector upr_bound (n, 1.0);
  double current_upr = 0.0;
  double quantile = 0.0;
  
  double x_1 = ys[n_1] + 1.0;
  double n_x = ns[n_1] - ys[n_1];
  double current_x_1 = x_1;
  double current_n_x = n_x;
  upr_bound[n_1] = R::qbeta(alpha_corr, current_x_1, current_n_x, 1, 0);
  
  for (int i = n - 2; i >= 0; i--) {
    i_1 = i + 1;
    x_1 = x_1 + ys[i];
    n_x = n_x + ns[i] - ys[i];
    current_x_1 = x_1;
    current_n_x = n_x;
    quantile = R::qbeta(alpha_corr, x_1, n_x, 1, 0);
    current_upr = std::min(upr_bound[i_1], quantile);
    for (int j = n_1; j > i; j--) {
      current_x_1 = current_x_1 - ys[j];
      current_n_x = current_n_x - ns[j] + ys[j];
      quantile = R::qbeta(alpha_corr, current_x_1, current_n_x, 1, 0);
      current_upr = std::min(current_upr, quantile);
    }
    upr_bound[i] = current_upr;
    Rcpp::checkUserInterrupt();
  }
  
  return upr_bound;
}