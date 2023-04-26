#' consomation
#'
#' tableau de l'impacte des consomation intermédiaire par pays ou secteur
#'
#' @param SPA_annee data.table issue de SPA_opt2
#' @param pays Code pays présent dans les données
#' @param by "secteurs" ou "pays" selon l'aggrégation souhaité
#' @param listepays liste de code pays des pays nous interessant pour l'analyse (nécessaire pour by ="pays")
#'
#' @return data.table à 2 colonnes correspondant au nom du pays ou secteurs et la valeur de la contribution totale
#' @export
#'
#' @importFrom data.table data.table
#'
#' @examples
#' \dontrun{
#'
#' data(SPA_ECOLE)
#'
#' don1990 <- SPA_ECOLE$don1990
#'
#' tt <- consomation(don1990,"FRA")
#' View(tt)
#'
#' tt2 <- consomation(don1990,"FRA", by = "pays", listepays = c("FRA","CHN","ROW","UE_OTHERS","USA"))
#' View(tt2)
#'
#' }
consomation = function(SPA_annee,pays, by = "secteurs",listepays = NA){
  len = length(SPA_annee$node)
  res=data.table()
  if(by == "pays"){
    if(length(listepays) ==1){
    if(is.na(listepays)){
      return("Erreur : fournir 'listepays' avec 'by = pays' ")
    }}}

  for(i in 1:len){
    chemin <- strsplit(SPA_annee$node[i], split = "~")[[1]]
    if(length(chemin) != 1){
      starts = chemin[1]
      if(startsWith(starts, pays)){
        len_node = length(chemin)
        ends = chemin[len_node]
        if(by == "pays"){
          pays_ends = "Pays_non_fournies"
          for(pp in listepays){
            if(startsWith(ends, pp)){
              pays_ends = pp
            }
          }
          if(pays_ends %in% res$pays){
            res[res$pays == pays_ends,"contribution"] = res[res$pays == pays_ends,"contribution"] + SPA_annee$contribution[i]
          }else{
            res_temp <- data.frame(pays = pays_ends, contribution = SPA_annee$contribution[i])
            res <- rbind(res,res_temp)
          }
        }else{
          if(by == "secteurs"){
            if(ends %in% res$secteur){
              res[res$secteur == ends,"contribution"] = res[res$secteur == ends,"contribution"] + SPA_annee$contribution[i]
            }else{
              res_temp <- data.frame(secteur = ends, contribution = SPA_annee$contribution[i])
              res <- rbind(res,res_temp)
            }
          }else{
            return("Erreur : argument inexistant 'by'")
          }
        }
      }
    }
  }
  return(res)
}

