#' GereInfNA
#'
#' Fonction pour convertir les infinity et les NA en une autre valeur
#'
#' @param df un data frame
#' @param impute la valeur par laquel les remplacé
#'
#' @return Un data frame avec les NA et Inf remplacé par impute
#' @export
#'
#'
#' @examples
#' \dontrun{
#'
#' Age <- c(23, 41, Inf, 58, 26)
#' df <- data.frame(Age)
#' GereInfNA(df)
#'
#' }
#'
GereInfNA <- function(df,impute=0){  # Fonction de contréle de la cohérence comptable.
  if(ncol(df)==1){ # Si c'est un vecteur qui est gere comme une liste
    df<-lapply(df, function(x) {
      x[is.infinite(x)] <- impute
      return(x)
    } )
    df<-lapply(df, function(x) {
      x[is.na(x)] <- impute
      return(x)
    } )
    df<-vectDF(df)
  }else{ # Si c'est une matrice avec plusieurs colonnes
    is.na(df)<-sapply(df, is.infinite)
    df[is.na(df)]<-impute
  }
  return(df)
}
