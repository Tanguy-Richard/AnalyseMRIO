#' centralite_annee
#'
#' Récupère un tableau de d'indice de betweenness (compte ou valeur total)
#' pour toute les noeuds pays/secteur
#'
#' @param SPA_annee data.table issue de SPA_opt2
#' @param by Soit "count" pour compter le nombre de chemin, soit "value" pour la contribution total
#'
#' @return un data.frame avec une colonne noeud et une colonne correspondant à la valeur de l'indice
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
#' centralite_annee(SPA_ECOLE$don1990)
#'
#' # Pour comptabiliser la valeur totale des chemin passant par le noeud en intermédiaire
#' centralite_annee(SPA_ECOLE$don1990, by = "value")
#'
#' }
centralite_annee = function(SPA_annee, by = "count"){
  RES = NULL
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
      chem_temp = chem_temp[ -duplicated(chem_temp) ]
      if(by == "count"){
        for(nom in chem_temp){
          if(nom %in% RES$secteur){
            RES[RES$secteur== nom, "valeur"] = RES[RES$secteur== nom, "valeur"] + 1
          }else{
            res_temp <- data.frame(secteur = nom, valeur = 1)
            RES <- rbind(RES,res_temp)
          }
        }
      }else{
        for(nom in chem_temp){
          if(nom %in% RES$secteur){
            RES[RES$secteur== nom, "valeur"] = RES[RES$secteur== nom, "valeur"] + SPA_annee$contribution[i]
          }else{
            res_temp <- data.frame(secteur = nom, valeur = SPA_annee$contribution[i])
            RES <- rbind(RES,res_temp)
          }
        }
      }
    }
  }
  return(RES)
}
