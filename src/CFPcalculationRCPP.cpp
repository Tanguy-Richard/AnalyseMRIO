#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

//' Simple matrix multiplication
//'
//' @param ef Matrix
//' @param L Matrix
//' @param FD Matrix
//'
//' @return Product of matrices
//' @export
//'
//' @importFrom Rcpp evalCpp
//'
//' @examples
//' A <- matrix(1:9, 3, 3)
//' B <- matrix(11:19, 3, 3)
//' C <- matrix(4:12, 3, 3)
//' CFPcalculationRCPP(A, B, C)
// [[Rcpp::export]]
arma::mat CFPcalculationRCPP(arma::mat ef, arma::mat L,arma::mat  FD){
  return ef*L*FD;
}
