#' Read_Tree
#'
#' Parcours d'arbre pour déterminer l'impacte de chaque noeud
#'
#' @param tree un arbre issue de Construct_Tree
#'
#' @return Un data frame à 2 colonne, une avec le nom du noeud et une autre avec sa contribution
#' @export
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
#' # Pour aller chercher VA
#' VA <- MRIO_1990_ALL[["VA"]]
#' VA <- VA[,-c(1,2)] # prendre uniquement la colonne d'interet
#' VA <- MatCol(VA,"Col_Country","Col_Indus")
#'
#'
#' # On donnes un parcours
#' Z <- c( "CHN_ENRJ")
#'
#' # Contribution du sous arbre
#' test <- Construct_Tree(VA,A,Y,5,4000,Z)
#'
#' lecture <- Read_Tree(test)
#' head(lecture[order(lecture$Node,decreasing = TRUE),])
#'
#' }
Read_Tree <- function(tree){
    Node <- tree$Node
    Z <- paste(tree$Z, collapse = " ")
    Res <- data.frame(Z,Node)
    if(length(tree$Next)==0){
      return(Res)
    }else{
      for(i in tree$Next){
        Res <- rbind(Res, Read_Tree(i))
      }
      return(Res)
    }
}