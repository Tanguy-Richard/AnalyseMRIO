#' Read_Tree
#'
#' Parcours d'arbre pour déterminer l'impacte de chaque noeud
#'
#' @param tree un arbre issue de Construct_Tree
#'
#' @return Un data frame à 2 colonne, une avec le nom du noeud et une autre avec sa contribution
#' @export
#'
#' @importFrom data.table data.table rbindlist
#'
#' @examples
#' \dontrun{
#'
#' data(BASE_ECOLE)
#'
#' # Pour avoir la matrice de Leontief :
#' MRIO_1990_ALL<-CompoECOLEouA17(BASE_ECOLE,"OptFullOptionsBonus",date=1990)
#' A <- MRIO_1990_ALL[["A_tab"]][,-1] # On enlève la colonne année
#' A <- MatCol(A,"Lig_Country","Lig_Indus")
#'
#' # Pour aller chercher Y (demande finale)
#' Y <- MRIO_1990_ALL[["DF_TOT"]]
#' Y <- Y[,-c(1,2)] # prendre uniquement la colonne d'interet
#' Y <- MatCol(Y,"Lig_Country","Lig_Indus")
#'
#' # Pour aller chercher VA/PROD
#' VA <- MRIO_1990_ALL[["VA"]]
#' PROD <- MRIO_1990_ALL[["PROD"]]
#' VA <- VA[,-c(1,2)] # prendre uniquement la colonne d'interet
#' PROD <- PROD[,-c(1,2)]
#' VA <- MatCol(VA,"Col_Country","Col_Indus")
#' PROD <- MatCol(PROD,"Lig_Country","Lig_Indus")
#' VA <- VA/PROD
#'
#'
#' # On donnes un parcours
#' Z <- c( "CHN_ENRJ")
#'
#' # Contribution du sous arbre
#' test <- Construct_Tree(VA,A,Y,5,0.5,Z)
#'
#' lecture <- Read_Tree(test)
#' head(lecture[order(lecture$contribution,decreasing = TRUE),])
#'
#'
#' test2 <- Construct_Tree(VA,A,Y,5,0.5)
#' lecture2 <- Read_Tree(test2)
#' head(lecture2[order(lecture2$contribution,decreasing = TRUE),])
#' }
Read_Tree <- function(tree){
  if("Tout" %in% tree$Z){
    Res <- data.table()
    if(length(tree$Next)==0){
      return(Res)
    }else{
      for(i in tree$Next){
        Res <- rbindlist(list(Res, Read_Tree(i)))
      }
      return(Res)
    }
  }else{
    contribution <- tree$Node
    node <- paste(tree$Z, collapse = "~")
    Res <- data.table(node,contribution)
    if(length(tree$Next)==0){
      return(Res)
    }else{
      for(i in tree$Next){
        Res <- rbindlist(list(Res, Read_Tree(i)))
      }
      return(Res)
    }
    }
}
