#' meilleur_chemin
#'
#' determine les meilleurs chemins entre noeuds (pays_secteur)
#'
#' @param SPA_annee data.table issue de SPA_opt2
#' @param lon longeur du chemin 3 ou 4
#' @param by Soit "count" pour compter le nombre de chemin, soit "value" pour la contribution total
#'
#' @return
#' @export
#'
#'
#' @importFrom cli cli_abort
#' @importFrom tidyr separate
#'
#' @examples
#' \dontrun{
#'
#' data(SPA_ECOLE)
#'
#' # Pour dénombrer le nombre de chemin passant par le noeud en intermédiaire
#' meilleur_chemin(SPA_ECOLE$don1990, lon = 3)
#'
#' # Pour comptabiliser la valeur totale des chemin passant par le noeud en intermédiaire
#' meilleur_chemin(SPA_ECOLE$don1990, lon = 4, by = "value")
#'
#' }
meilleur_chemin = function(SPA_annee, lon = 3, by = "count"){
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
  if(lon == 3){
    for(i in 1:len){
      chemin <- strsplit(SPA_annee$node[i], split = "~")[[1]]
      long_chemin = length(chemin)
      if(long_chemin == 3){
        nom = chemin[2]
        if(by == "count"){
          if(nom %in% RES$secteur){
            RES[RES$secteur== nom, "valeur"] = RES[RES$secteur== nom, "valeur"] + 1
          }else{
            res_temp <- data.frame(secteur = nom, valeur = 1)
            RES <- rbind(RES,res_temp)
          }
        }
        if(by == "value"){
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
  if(lon == 4){
    for(i in 1:len){
      chemin <- strsplit(SPA_annee$node[i], split = "~")[[1]]
      long_chemin = length(chemin)
      if(long_chemin == 4){
        nom = paste(chemin[2],chemin[3],sep="~")
        if(by == "count"){
          if(nom %in% RES$secteur){
            RES[RES$secteur== nom, "valeur"] = RES[RES$secteur== nom, "valeur"] + 1
          }else{
            res_temp <- data.frame(secteur = nom, valeur = 1)
            RES <- rbind(RES,res_temp)
          }
        }
        if(by == "value"){
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
  if(lon == 4){
    RES <- separate(RES, secteur ,c("secteur1","secteur2"),"~")
  }
  return(RES)
}
