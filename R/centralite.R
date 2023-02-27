#' centralite
#'
#' calcul le nombre de passage par un noeud (pays_secteur) pour les chemins de longueur 3 ou plus
#' C'est une mesure de betweenness
#'
#' @param SPA_annee data.table issue de SPA_opt2
#' @param node nom du pays_secteur
#' @param by Soit "count" pour compter le nombre de chemin, soit "value" pour la contribution total
#'
#' @return la valeur correspondante
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
#' centralite(SPA_ECOLE$don1990, "CHN_AGR_INDU")
#'
#' # Pour comptabiliser la valeur totale des chemin passant par le noeud en intermédiaire
#' centralite(SPA_ECOLE$don1990, "CHN_AGR_INDU", by="value")
#'
#' }
centralite = function(SPA_annee, node, by = "count"){
  total = 0
  len = length(SPA_annee$node)
  if(length(by)>1){
    cli_abort(c(
      "{.var by} must be a character string choosed between 'count' and 'value' ",
      "x" = "You've supplied a {.cls {class(by)}} vector."
    ))
  }
  if(!(by %in% c("count","value"))){
    cli_abort(c(
      "{.var by} must be choose between 'count' and 'value' ",
      "x" = "You've supplied '{by}' ."
    ))
  }
  for(i in 1:len){
    chemin <- strsplit(SPA_annee$node[i], split = "~")[[1]]
    long_chemin = length(chemin)
    if(long_chemin >= 3){
      chem_temp = chemin[-c(1,long_chemin)]
      if(node %in% chem_temp){
        if(by == "count"){
          total = total + 1
        }else{
          total = total + SPA_annee$contribution[i]
        }
      }
    }
  }
  return(total)
}
