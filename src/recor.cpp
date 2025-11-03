#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <cmath>

using namespace Rcpp;

// Calculate covariance between two vectors
double calculate_cov(const NumericVector& x, const NumericVector& y) {
    int n = x.size();
    double mean_x = mean(x);
    double mean_y = mean(y);
    double cov = 0.0;
    
    for (int i = 0; i < n; i++) {
        cov += (x[i] - mean_x) * (y[i] - mean_y);
    }
    
    return cov / (n - 1);
}

// Calculate recor between two vectors
double recor_cpp(const NumericVector& x, const NumericVector& y) {
    double numerator = calculate_cov(x, y);
    
    // Create sorted copies of x and y
    NumericVector x_sorted = clone(x);
    NumericVector y_sorted = clone(y);
    
    std::sort(x_sorted.begin(), x_sorted.end());
    double denominator;
    
    if (numerator >= 0) {
        std::sort(y_sorted.begin(), y_sorted.end());
        denominator = std::abs(calculate_cov(x_sorted, y_sorted));
    } else {
        std::sort(y_sorted.begin(), y_sorted.end(), std::greater<double>());
        denominator = std::abs(calculate_cov(x_sorted, y_sorted));
    }
    
    return numerator / denominator;
}

// Calculate recor matrix
// [[Rcpp::export]]
NumericMatrix recor_matrix(const NumericMatrix& x, const Nullable<NumericMatrix>& y = R_NilValue) {
    int nrow = x.nrow();
    int p = x.ncol();
    
    if (y.isNull()) {
        // single matrix case
        NumericMatrix result(p, p);
        
        // set diagonal to 1
        for (int i = 0; i < p; i++) {
            result(i, i) = 1.0;
        }
        
        // calculate upper triangle and mirror to lower triangle
        for (int i = 0; i < p; i++) {
            for (int j = 0; j < i; j++) {
                double cor_val = recor_cpp(x(_, i), x(_, j));
                result(i, j) = cor_val;
                result(j, i) = cor_val;
            }
        }
        
        return result;
    } else {
        // double matrix case
        NumericMatrix y_mat = as<NumericMatrix>(y);
        int q = y_mat.ncol();
        
        if (y_mat.nrow() != nrow) {
            stop("The number of rows in x and y must be the same");
        }
        
        NumericMatrix result(p, q);
        
        for (int i = 0; i < p; i++) {
            for (int j = 0; j < q; j++) {
                result(i, j) = recor_cpp(x(_, i), y_mat(_, j));
            }
        }
        
        return result;
    }
}

// Calculate recor between two vectors
// [[Rcpp::export]]
double recor_vector(const NumericVector& x, const NumericVector& y) {
    if (x.size() != y.size()) {
        stop("The lengths of input vectors must be the same");
    }
    
    if (x.size() < 2) {
        stop("The length of input vectors must be at least 2");
    }
    
    return recor_cpp(x, y);
}