#' restructure_tab
#'
#' passe une table d'indice de betweenness de wide à long
#'
#' @param tab_centre une table d'indice de betwenness par ans
#' @param listepays la liste des pays présents
#'
#' @return une table à 4 colonnes pays, secteur, valeur et annee
#' @export
#'
#' @importFrom stringr str_sub
#'
#' @examples
#' \dontrun{
#'
#' data(SPA)
#'
#' toute_centre <- centralite_totale(SPA_ECOLE)
#'
#' pays <- c("CHN","FRA","USA","UE_OTHERS","ROW")
#'
#' toute_centre_long <- restructure_tab(toute_centre, pays)
#'
#' }
restructure_tab = function(tab_centre,listepays){
  RES = NULL
  secteurs = tab_centre$secteur
  pays = c()
  secteur = c()
  for(i in secteurs){
    for(pp in listepays){
      if(startsWith(i, pp)){
        pays = c(pays,pp)
        n1 = nchar(pp)
        n2 = nchar(i)
        secteur = c(secteur,str_sub(i, n1+1, n2))
      }
    }
  }
  if(length(pays)!=nrow(tab_centre)){
    cli_abort(c(
      "{.var listepays} must contain every country of {.var tab_centre}",
      "x" = "{.var listepays} cover only {length(pays)} of {nrow(tab_centre)} lines;"
    ))
  }
  for(i in 2:ncol(tab_centre)){
    rest_temp <- data.frame(pays = pays, secteur = secteur, valeur = tab_centre[,i],annee = paste(i-1))
    RES <- rbind(RES,rest_temp)
  }
  return(RES)
}
