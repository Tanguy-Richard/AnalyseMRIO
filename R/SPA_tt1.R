#' SPA_tt1
#'
#' @param f vecteur des contributions par pays_secteur par unité de production
#' @param A matrice de Leontief
#' @param Y Demande Finale
#' @param Tmax Profondeur maximale autorisée
#' @param tol Valeur limite
#' @param L inverse de Leontief
#' @param Ftot effet total
#' @param Z Noeud actuel
#' @param cc coefficient intemédiaire
#'
#' @return Un data frame à 2 colonnes:
#'  - une avec le nom du noeud
#'  - une avec la contribution
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
#' # On donne la valeur du coef associé au chemin
#' cc <- Y[Z,]
#'
#' #Calcul de paramètre
#' L <- inversion_rcpp3(diag(ncol(A))-A)
#' Ftot <- Mult2_rcpp3(t(VA),L)
#'
#' # Contribution du sous arbre
#' test <- SPA_tt1(VA,A,Y,5,0.5,L,Ftot,Z, cc)
#'
#' test2 <- SPA_tt1(VA,A,Y,5,0.5,L,Ftot)
#'
#' }
SPA_tt1 <- function(f,
                    A,
                    Y,
                    Tmax,
                    tol,
                    L,
                    Ftot,
                    Z = "Tout",
                    cc = 1) {
  sector <- row.names(Y)
  Res <- data.table()
  if ("Tout" %in% Z) {
    for (i in sector) {
      node_z <- Y[i, ]
      y_Z <- as.matrix(node_z * as.integer(row.names(Y) == i))
      contrib <- Mult2_rcpp3(Ftot, y_Z)
      if (contrib < tol) {

      } else{
        Res <- rbindlist(list(Res, SPA_tt1(f, A, Y, Tmax, tol, L, Ftot, i , node_z )))
      }
    }
  } else{
    Tr = length(Z)
    if (Tr >= Tmax) {
      contribution <- f[Z[length(Z)],] *  cc
      node <- paste(Z, collapse = "~")
      Res <- data.table(node, contribution)

    } else{
      contribution <- f[Z[length(Z)],] *  cc
      node <- paste(Z, collapse = "~")
      Res <- data.table(node, contribution)

      for (i in sector) {
        cc_temp <- cc * A[i, Z[length(Z)]]
        y_Z <-
          as.matrix(cc_temp * as.integer(row.names(Y) == Z[length(Z)]))
        contrib <- Mult2_rcpp3(Ftot, y_Z)
        if (contrib < tol) {

        } else{
          Res <-
            rbindlist(list(Res, SPA_tt1(f, A, Y, Tmax, tol, L, Ftot, c(Z, i) , cc_temp)))
        }
      }
    }
  }
  return(Res)
}
