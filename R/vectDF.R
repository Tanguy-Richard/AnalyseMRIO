#' vectDF
#'
#' Fonction de transformation en vecteur dataframe
#'
#' @param vect Un vecteur
#'
#' @return un vecteur data frame (colonne)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' v <- c(1,2,3,4)
#' vectDF(v)
#'
#' }
#'
vectDF<- function(vect){
  out<-as.data.frame(vect, drop=FALSE)
  return(out)
}
