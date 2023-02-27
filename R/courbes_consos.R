#' courbes_consos
#'
#' Courbe de l'évolution de l'impacte de pays ou secteurs dans les productions intermédiaire d'un pays
#'
#' @param SPA liste de data.table issue de SPA_opt2
#' @param pays Code pays présent dans les données
#' @param by "secteurs" ou "pays" selon l'aggrégation souhaité
#' @param listepays liste de code pays des pays nous interessant pour l'analyse (nécessaire pour by ="pays")
#' @param prop si TRUE, affiche la proportion du total par année, si FALSE, affiche les valeurs brutes
#' @param liss ajouter un lissage linéaire sur les courbes
#'
#' @return un graphique ggplot2 correspondant à la demande
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_smooth
#'
#' @examples
#' \dontrun{
#'
#' data(SPA_ECOLE)
#'
#' courbes_consos(SPA_ECOLE,"FRA", by = "pays",
#'  listepays = c("FRA","CHN","ROW","UE_OTHERS","USA"), liss = TRUE)
#'
#' courbes_consos(SPA_ECOLE,"FRA", by = "secteurs",prop = FALSE)
#'
#' }
courbes_consos = function(SPA, pays, by= "secteurs", listepays = NA, prop = TRUE, liss = FALSE){

  if(by == "pays"){
    if(length(listepays) ==1){
      if(is.na(listepays)){
        return("Erreur : fournir 'listepays' avec 'by = pays' ")
      }}}

  if(!(by %in% c("secteurs","pays"))){
    return("Erreur : 'by' doit etre soit 'secteurs' ou 'pays'  ")
  }

  if(by == "pays"){
    data=NULL
    annee=1
    for(i in SPA){
      temp = consomation(i,pays, by = "pays",listepays = listepays)
      temp = cbind(temp,annee = annee)
      temp = temp[temp$pays %in% listepays, ]
      if(prop){
        temp$contribution <- temp$contribution/sum(temp$contribution)
      }
      data = rbind(data,temp)
      annee = annee +1
    }
    graph <- ggplot(data = data) + aes(x = annee,y = contribution, by= pays, color = pays) +geom_line()
  }else{
    data=NULL
    annee=1
    for(i in SPA){
      temp = consomation(i,pays, by = "secteurs")
      temp = cbind(temp,annee = annee)
      if(prop){
        temp$contribution <- temp$contribution/sum(temp$contribution)
      }
      data = rbind(data,temp)
      annee = annee +1
    }
    graph <- ggplot(data = data) + aes(x = annee,y = contribution, by= secteur, color = secteur) +geom_line()
  }
  if(liss){
    graph = graph + geom_smooth(method = "lm")
  }
  return(graph)
}

