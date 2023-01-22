#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

//' Inversion de matrice
//'
//' @param m1 Matrix
//'
//' @return Inverse de la matrice
//' @export
//'
//' @importFrom Rcpp evalCpp
//'
//' @examples
//' A <- diag(2,3)
//' inversion_rcpp3(A)
// [[Rcpp::export]]
arma::mat inversion_rcpp3(arma::mat m1){
  arma::mat m2 = inv(m1);
  return(m2);
}
