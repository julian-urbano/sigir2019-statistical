#include <Rcpp.h>
#include <random>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
NumericVector testCpp_permutation(NumericVector x, int B = 1e5) {
  int n = x.size();
  double x_mean = mean(x);
  double x_absmean = std::abs(x_mean);

  std::random_device rd;
  std::mt19937 mt(rd());
  std::uniform_int_distribution<int> dist(0, 1);

  double p2 = 0;
  double p1 = 0;

  for(int trial = 0; trial < B; trial++) {
    double x_B = 0;
    for(int i = 0; i < n; i++) {
      x_B += (dist(mt) == 0 ? x[i] : -x[i]);
    }
    x_B /= n;

    p2 += std::abs(x_B) >= x_absmean;
    p1 += x_B >= x_mean;
  }

  return NumericVector::create(p1 / B, p2 / B);
}

// [[Rcpp::export]]
NumericVector testCpp_bootstrap(NumericVector x, int B = 1e5) {
  int n = x.size();
  double x_mean = mean(x);
  double x_absmean = std::abs(x_mean);

  std::random_device rd;
  std::mt19937 mt(rd());
  std::uniform_int_distribution<int> dist(0, n-1);

  NumericVector p(B);

  for(int trial = 0; trial < B; trial++) {
    double x_B = 0;
    for(int i = 0; i < n; i++) {
      int j = dist(mt);
      x_B += x[j];
    }
    x_B /= n;
    p[trial] = x_B;
  }
  p = p - mean(p);

  double p2 = sum(abs(p) >= x_absmean);
  double p1 = sum(p >= x_mean);

  return NumericVector::create(p1 / B, p2 / B);
}
