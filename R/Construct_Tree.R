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
#'
#'
#' }
Construct_Tree <- function(f, A, Y, Tmax, tol, Z){
  Tr = length(Z)
  sector <- row.names(Y)
  if(Tr >= Tmax) {
    suite <-  list()
    for(i in sector) {
      suite[[i]] <-  NULL
    }
    suite = suite[-1]
    tree <-  list(Z = Z,
                Node = Contribution_Node(f, A, Y, Z),
                Next = suite)
  }else{
    suite <-  list()
    for(i in sector) {
      if(Contribution_SubTree(f, A, Y, c(Z, i)) < tol) {
        suite[[i]] = NULL
      }else{
        suite[[i]] = Construct_Tree(f, A, Y, Tmax, tol, c(Z, i))
      }
    }
    suite = suite[-1]
    tree = list(Z = Z,
                Node = Contribution_Node(f, A, Y, Z),
                Next = suite)
  }
  return(tree)
}

