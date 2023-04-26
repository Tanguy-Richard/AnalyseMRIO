#' SPA_iter
#'
#' Construit itérativement un arbre récapitulant les effets à chaques étapes.
#'
#' @param f vecteur des contributions par pays_secteur par unité de production
#' @param A matrice de Leontief
#' @param Y Demande Finale
#' @param Tmax Profondeur maximale autorisée
#' @param tol Valeur limite
#' @param Z Noeud actuel
#' @param cc coefficient intermédiaire (cc = 1 pour Z = "Tout)
#'
#' @return Un data frame à 2 colonnes:
#'  - une avec le nom du noeud
#'  - une avec la contribution
#' @export
#'
#' @importFrom data.table rbindlist
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
#' # Contribution du sous arbre
#' test <- SPA_iter(VA,A,Y,5,0.5,Z,cc)
#' View(test)
#'
#' test2 <- SPA_iter(VA,A,Y,5,0.5)
#' View(test2)
#'
#' }
SPA_iter <- function(f, A, Y, Tmax, tol, Z = "Tout", cc = 1){
  # On calcul les objets qui nous servirons pour toute l'analyse
  sector <- row.names(Y) # non des secteurs
  L <- inversion_rcpp3(diag(ncol(A))-A) # Inverse de Leontief
  Ftot <- Mult2_rcpp3(t(f),L) # Impact total par secteur

  A_traiter=list() # Liste pour stocker les chemins à traiter
  Res <- list() # liste pour stocker les résultats

  ############################################################
  #   Initialisation de la boucle while
  ############################################################

  if ("Tout" %in% Z) { # Cas ou on souhaite tout les chemins
    for (i in sector) {

      node_z <- Y[i, ]
      y_Z <- as.matrix(node_z * as.integer(row.names(Y) == i))
      contrib <- Mult2_rcpp3(Ftot, y_Z)
      if (contrib < tol) {

      }else{

        ajout = list( Chem = i ,node_z = node_z)
        A_traiter = c(A_traiter, list(ajout))

      }
    }
  }else{ # Cas ou on commence d'un chemin

    Tr = length(Z)

    if (Tr >= Tmax) {

      contribution <- f[Z[length(Z)],] *  cc
      node <- paste(Z, collapse = "~")
      Res_temp <- list(data.table(node, contribution))
      Res <- c(Res, Res_temp)

    } else{

      contribution <- f[Z[length(Z)],] *  cc
      node <- paste(Z, collapse = "~")
      Res_temp <- list(data.table(node, contribution))
      Res <- c(Res, Res_temp)

      for (i in sector) {

        cc_temp <- cc * A[i, Z[length(Z)]]
        y_Z <-
          as.matrix(cc_temp * as.integer(row.names(Y) == Z[length(Z)]))
        contrib <- Mult2_rcpp3(Ftot, y_Z)

        if (contrib < tol) {

        } else{
          ajout = list(Chem = c(Z, i) ,node_z = cc_temp)
          A_traiter = c(A_traiter, list(ajout))
        }

      }
    }
  }
  ############################################################
  #   Itération sur les cas à traité
  ############################################################

  while (length(A_traiter) != 0) {

    len = length(A_traiter)
    # On considère le dernier ajout an premier (plus profond dans l'arbre => ne pas surcharger le vecteur A_traité)
    en_traitement = A_traiter[[len]]
    # On le supprime
    A_traiter = A_traiter[-len]

    node_z = en_traitement$node_z
    Chem = en_traitement$Chem

    Tr = length(Chem)

    contribution <- f[Chem[Tr],] *  node_z
    node <- paste(Chem, collapse = "~")
    Res_temp <- list(data.table(node, contribution))
    Res <- c(Res, Res_temp)


    if (Tr >= Tmax) {

    } else{

      for (i in sector) {

        cc_temp <- node_z * A[i, Chem[Tr]]
        y_Z <-
          as.matrix(cc_temp * as.integer(row.names(Y) == Chem[Tr]))
        contrib <- Mult2_rcpp3(Ftot, y_Z)

        if (contrib < tol) {

        } else{

          ajout = list(Chem = c(Chem, i) ,node_z = cc_temp)
          A_traiter = c(A_traiter, list(ajout))

        }
      }
    }
  }

  return(rbindlist(Res))

}
