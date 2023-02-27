#' Evolution
#'
#' @param SPA liste de data.table issue de SPA_opt2
#' @param num nombre de chemins à conserver
#'
#' @return un data frame de l'évolution des rangs
#' @export
#'
#' @examples
#' \dontrun{
#'
#' data(SPA_ECOLE)
#'
#' t1 <- Evolution(SPA_ECOLE,100)
#' View(t1)
#'
#' }
Evolution = function(SPA,num){
  an1 <- SPA[[1]]
  an1 <- an1[order(an1$contribution,decreasing = TRUE),]
  chemins =an1$node[1:num]
  RES = data.frame()
  for(i in chemins){
    rang <- Variation(i,SPA)
    RES <- rbind(RES, as.integer(rang[2,]))
  }
  row.names(RES) <- chemins
  return(RES)
}



