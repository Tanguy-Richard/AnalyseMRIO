#' Variation
#'
#' Determine les rangs d'un chemin dans une liste de resultat de SPA
#'
#' @param chemin nom du chemin
#' @param don liste de data.table issue de SPA_opt2
#'
#' @return un tableau avec le rang du chemnin par data.table
#' @export
#'
#' @examples
#' \dontrun{
#'
#' data(SPA_ECOLE)
#'
#' Variation("CHN_AGR_INDU",SPA_ECOLE)
#'
#' }
Variation = function(chemin, don){

  ans_nom=names(don)
  valeur=c()

  for(annee in don){
    if(chemin %in% annee$node){
       don_temp <- annee[order(annee$contribution,decreasing = TRUE),]
       valeur = c(valeur,which(don_temp$node == chemin))
    }else{
      valeur = c(valeur,NA)
    }
  }
  return(rbind(ans_nom,valeur))
}
