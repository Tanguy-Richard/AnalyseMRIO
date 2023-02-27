#' centralite_totale
#'
#' @param SPA liste de data.table issue de SPA_opt2
#' @param by Soit "count" pour compter le nombre de chemin, soit "value" pour la contribution total
#'
#' @return un data.frame avec une colonne noeud et une colonne par année correspondant à la valeur de l'indice
#' @export
#'
#' @importFrom cli cli_abort
#'
#' @examples
#' \dontrun{
#'
#' data(SPA_ECOLE)
#'
#' # Pour dénombrer le nombre de chemin passant par le noeud en intermédiaire
#' centralite_totale(SPA_ECOLE)
#'
#' # Pour comptabiliser la valeur totale des chemin passant par le noeud en intermédiaire
#' centralite_totale(SPA_ECOLE, by = "value")
#'
#' }
centralite_totale = function(SPA, by = "count"){
  RES = centralite_annee(SPA[[1]],by = by)
  ind <- 1
  colnames(RES) <- c("secteur", paste("annee",ind,sep="_"))
  for(i in SPA[-1]){
    ind = ind + 1
    res_temp <- centralite_annee(i,by = by)
    colnames(res_temp) <- c("secteur", paste("annee",ind,sep="_"))
    RES = merge(RES,res_temp, by = "secteur")
  }
  return(RES)
}
