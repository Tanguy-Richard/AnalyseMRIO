#' MatCol
#'
#' Supprime les colonnes Lig_Country et Lig_Indus en rend une matrice
#'
#' @param MAT Un data.frame avec une colonne "C1" et une "C2"
#' @param C1 Un nom de colonne pour nommer la matrice
#' @param C2 Un second nom de colonne
#'
#' @return Une matrice ayant pour nom de ligne "C1"_"C2"
#' @export
#'
#' @examples
#' \dontrun{
#'
#' data(BASE_ECOLE)
#'
#' #Construire la matrice de Leontief
#' MRIO_1990_ALL<-CompoECOLEouA17(BASE_ECOLE,"OptFullOptionsBonus",date=1990)
#' A <- MRIO_1990_ALL[["A_tab"]][,-1] # On enlève la colonne année
#' Leontief <- MatCol(A,"Lig_Country","Lig_Indus")
#' View(Leontief)
#'
#' }
MatCol <- function(MAT,C1,C2){
  temp <- MAT
  noms <- c()
  for(i in 1:nrow(temp)){
    noms <- c(noms, paste(temp[,..C1][i], temp[,..C2][i], sep="_")) #On nomme les lignes
  }
  temp[,which(c(C1,C2) %in% colnames(temp))] <- NULL
  temp <- as.matrix(temp) # On ne garde que la matrice qui nous interesse
  row.names(temp) <- noms
  return(temp)
}
