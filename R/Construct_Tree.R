#' Construct_Tree
#'
#' Construit récursivement un arbre récapitulant les effets à chaques étapes.
#'
#' @param f vecteur des contributions par pays_secteur par unité de production
#' @param A matrice de Leontief
#' @param Y Demande Finale
#' @param Tmax Profondeur maximale autorisée
#' @param tol Valeur limite
#' @param Z Noeud actuel
#'
#' @return Un arbre sous la forme d'une liste de 3 éléments :
#'  - Z vecteur étiquettant le noeud
#'  - Node Valeur de la contribution du noeud
#'  - Next Liste étiqueté par les états de :
#'          - sous arbres dépendants du noeud sous la même forme
#'          - NULL s'il n'y a pas de sous arbre
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
#' test2 <- Construct_Tree(VA,A,Y,5,0.5)
#'
#' }
Construct_Tree <- function(f, A, Y, Tmax, tol, Z = "Tout"){

  L <- inversion_rcpp3(diag(ncol(A))-A)
  Ftot <- Mult2_rcpp3(t(f),L)

  return(fonct_temp(f, A, Y, Tmax, tol, Z ,L,Ftot))
}

