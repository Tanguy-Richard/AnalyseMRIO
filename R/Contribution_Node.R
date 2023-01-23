#' Contribution_Node
#'
#' Calcule l'impact d'un noeud (chemin de production)
#'
#' @param f vecteur des contributions par pays_secteur par unité de production
#' @param A matrice de Leontief
#' @param Y Demande Finale
#' @param Z Noeud (sous la forme d'un vecteur de pays_secteur)
#'
#' @return La valeur de la contribution
#' @export
#'
#' @examples
#' \dontrun{
#'
#' data(BASE_ECOLE)
#'
#' # Séparation en composante du MRIO :
#' MRIO_1990_ALL<-CompoECOLEouA17(BASE_ECOLE,"OptFullOptionsBonus",date=1990)
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
#' Z <- c( "CHN_ENRJ", "CHN_AGR_INDU", "UE_OTHERS_AGR_INDU")
#'
#' # Contribution du noeud
#' Contribution_Node(VA,A,Y,Z)
#' }
Contribution_Node <- function(f,A,Y,Z){
  tau <- length(Z)
  node_z <- Y[Z[1],"value"]
  if(tau > 1){
    for (t in 2:tau){
      node_z <- node_z * A[Z[t],Z[t-1]]
    }
  }
  return(f[Z[tau],"value"]*node_z)

}
