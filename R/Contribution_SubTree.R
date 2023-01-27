#' Contribution_SubTree
#'
#' Calcule la contribution du sous arbre issue du noeud
#'
#' @param f vecteur des contributions par pays_secteur par unité de production
#' @param A matrice de Leontief
#' @param Y Demande Finale
#' @param Z Noeud (sous la forme d'un vecteur de pays_secteur)
#' @param L inverse de leontief (pour faciliter le temps de calcul)
#' "Calcul" => la fonction calcul elle même
#' @param Ftot effet total (pour faciliter le temps de calcul)
#' "Calcul" => la fonction calcul elle même
#'
#' @return La valeur de la contribution finale
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
#'
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
#' # On donnes un parcours
#' Z <- c( "CHN_ENRJ", "CHN_AGR_INDU", "UE_OTHERS_AGR_INDU")
#'
#'
#' # Contribution du sous arbre
#' Contribution_SubTree(VA,A,Y,Z)
#' }
Contribution_SubTree <- function(f,A,Y,Z,L = "Calcul",Ftot = "Calcul"){

  # Inverse de Leontief
  if(is.character(L)){
    L <- inversion_rcpp3(diag(ncol(A))-A)
  }

  # Calcul de l'impact total
  if(is.character(Ftot)){
    Ftot <- Mult2_rcpp3(t(f),L)
  }

  tau <- length(Z)
  node_z <- Y[Z[1],]

  if(tau>1){
    for (t in 2:tau){
      node_z <- node_z*A[Z[t],Z[t-1]]
    }
  }

  y_Z <- as.matrix(node_z * as.integer(row.names(Y) == Z[tau]))

  return(Mult2_rcpp3(Ftot,y_Z))

}
