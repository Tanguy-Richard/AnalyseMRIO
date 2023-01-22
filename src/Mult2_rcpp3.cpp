#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

//' Simple matrix multiplication
//'
//' @param matA Matrix
//' @param matB Matrix
//'
//' @return Product of matrices
//' @export
//'
//' @importFrom Rcpp evalCpp
//'
//' @examples
//' A <- matrix(1:9, 3, 3)
//' B <- matrix(11:19, 3, 3)
//' Mult2_rcpp3(A, B)
// [[Rcpp::export]]
arma::mat Mult2_rcpp3(arma::mat matA, arma::mat matB){
  return matA*matB;
}
