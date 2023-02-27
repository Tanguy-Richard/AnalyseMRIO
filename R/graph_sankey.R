#' graph_sankey
#'
#' Produit un graphique de Sankey représentant les chemins d'une longueur fixe donnée par les paramètres choisis
#'
#' @param SPA_annee data.table issue de SPA_opt2
#' @param data_chemin data.frame issu de meilleur_chemin
#' @param liste_debut liste de noeuds pour la couche de consomation finale
#' @param liste_fin liste de noeuds pour la couche de production intermédiaire lié au chemin
#' @param by soit "count" pour compter le nombre de chemin, soit "value" pour la contribution total
#'
#' @return un graphique de sankey issu du package networkD3
#' @export
#'
#' @importFrom networkD3 sankeyNetwork
#'
#' @examples
#' \dontrun{
#'
#' data(SPA_ECOLE)
#'
#' secteur_deb <- c("CHN_AGR_INDU","CHN_ENRJ" ,"CHN_SERV_ABRIT","CHN_SERV_EXPO",
#'                  "FRA_AGR_INDU","FRA_ENRJ","FRA_SERV_ABRIT","FRA_SERV_EXPO",
#'                  "ROW_AGR_INDU","ROW_ENRJ","ROW_SERV_ABRIT","ROW_SERV_EXPO",
#'                  "UE_OTHERS_AGR_INDU","UE_OTHERS_ENRJ","UE_OTHERS_SERV_ABRIT","UE_OTHERS_SERV_EXPO",
#'                  "USA_AGR_INDU","USA_ENRJ", "USA_SERV_ABRIT","USA_SERV_EXPO")
#' secteur_fin <- c("CHN_AGR_INDU","CHN_ENRJ" ,"CHN_SERV_ABRIT","CHN_SERV_EXPO",
#'                  "FRA_AGR_INDU","FRA_ENRJ","FRA_SERV_ABRIT","FRA_SERV_EXPO",
#'                  "ROW_AGR_INDU","ROW_ENRJ","ROW_SERV_ABRIT","ROW_SERV_EXPO",
#'                  "UE_OTHERS_AGR_INDU","UE_OTHERS_ENRJ","UE_OTHERS_SERV_ABRIT","UE_OTHERS_SERV_EXPO",
#'                  "USA_AGR_INDU","USA_ENRJ", "USA_SERV_ABRIT","USA_SERV_EXPO")
#'
#' chem1 <- meilleur_chemin(SPA_ECOLE$don1990, lon = 3)
#'
#' graph_sankey(SPA_ECOLE$don1990,
#'              chem1,
#'              secteur_deb,
#'              secteur_fin)
#'
#' chem2 <- meilleur_chemin(SPA_ECOLE$don1990, lon = 4, by = "value")
#'
#' graph_sankey(SPA_ECOLE$don1990,
#'              chem2,
#'              secteur_deb,
#'              secteur_fin,
#'              by = "value)
#'
#' }
graph_sankey = function(SPA_annee, data_chemin, liste_debut, liste_fin, by = "count"){
  lon = length(data_chemin[1,]) + 1
  len = length(SPA_annee$node)
  if(length(by)>1){
    cli_abort(c(
      "{.var by} must be a character string choosed between 'count' and 'value' ",
      "x" = "You've supplied a {.cls {class(by)}} vector."
    ))
  }
  if(!(by %in% c("count","value"))){
    cli_abort(c(
      "{.var by} must be choose between 'count' and 'value' ",
      "x" = "You've supplied '{by}' ."
    ))
  }
  if(lon == 3){
    secteur = data_chemin$secteur
    lon1 = length(liste_debut)
    lon2 = length(secteur)
    lon3 = length(liste_fin)
    name = c(liste_debut,secteur,liste_fin)
    links = data.frame(source = c(rep(0:(lon1-1),lon2),
                                  rep((lon1):(lon1+lon2-1),lon3)),
                       target = c(rep((lon1):(lon1+lon2-1), each = lon1),
                                  rep((lon1+lon2):(lon1+lon2+lon3-1), each = lon2)),
                       value = 0)
    for(i in 1:len){
      chemin <- strsplit(SPA_annee$node[i], split = "~")[[1]]
      if(length(chemin)==3){
        deb = chemin[1]
        nom = chemin[2]
        fin = chemin[3]
        if(nom %in% secteur){
          if(deb %in% liste_debut){
            if(fin %in% liste_fin){
              n1 = which(liste_debut == deb)
              n2 = which(secteur == nom)
              n3 = which(liste_fin == fin)
              if(by == "count"){
                ind1 = n1 + lon1 * ( n2 - 1 )
                ind2 = lon1 * lon2 + n2 + lon2 * ( n3 - 1 )
                links[ind1,"value"] = links[ind1,"value"] + 1
                links[ind2,"value"] = links[ind2,"value"] + 1
              }else{
                ind1 = n1 + lon1 * ( n2 - 1 )
                ind2 = lon1 * lon2 + n2 + lon2 * ( n3 - 1 )
                links[ind1,"value"] = links[ind1,"value"] + SPA_annee$contribution[i]
                links[ind2,"value"] = links[ind2,"value"] + SPA_annee$contribution[i]
              }
            }
          }
        }
      }
    }

  }
  if(lon == 4){
    secteur1 = data_chemin$secteur1
    secteur2 = data_chemin$secteur2
    secteur = paste(secteur1, secteur2, sep = "~")
    secteur1 <- secteur1[!duplicated(secteur1)]
    secteur2 <- secteur2[!duplicated(secteur2)]
    lon1 = length(liste_debut)
    lon2 = length(secteur1)
    lon3 = length(secteur2)
    lon4 = length(liste_fin)
    name = c(liste_debut, secteur1, secteur2, liste_fin)
    links = data.frame(source = c(rep(0:(lon1-1),lon2),
                                  rep((lon1):(lon1+lon2-1),lon3),
                                  rep((lon1 +lon2):(lon1+lon2 +lon3-1),lon4)),
                       target = c(rep((lon1):(lon1+lon2-1), each = lon1),
                                  rep((lon1+lon2):(lon1+lon2+lon3-1), each = lon2),
                                  rep((lon1+lon2+lon3):(lon1+lon2+lon3+lon4-1), each = lon3)),
                       value = 0)
    for(i in 1:len){
      chemin <- strsplit(SPA_annee$node[i], split = "~")[[1]]
      if(length(chemin)==4){
        deb = chemin[1]
        nom1 = chemin[2]
        nom2 = chemin[3]
        fin = chemin[4]
        if(paste(nom1,nom2,sep="~") %in% secteur){
          if(deb %in% liste_debut){
            if(fin %in% liste_fin){
              n1 = which(liste_debut == deb)
              n2 = which(secteur1 == nom1)
              n3 = which(secteur2 == nom2)
              n4 = which(liste_fin == fin)
              if(by == "count"){
                ind1 = n1 + lon1 * ( n2 - 1 )
                ind2 = lon1 * lon2 + n2 + lon2 * ( n3 - 1 )
                ind3 = lon1 * lon2 + lon2 * lon3 + n3 + lon3 * ( n4 - 1 )
                links[ind1,"value"] = links[ind1,"value"] + 1
                links[ind2,"value"] = links[ind2,"value"] + 1
                links[ind3,"value"] = links[ind3,"value"] + 1
              }else{
                ind1 = n1 + lon1 * ( n2 - 1 )
                ind2 = lon1 * lon2 + n2 + lon2 * ( n3 - 1 )
                ind3 = lon1 * lon2 + lon2 * lon3 + n3 + lon3 * ( n4 - 1 )
                links[ind1,"value"] = links[ind1,"value"] + SPA_annee$contribution[i]
                links[ind2,"value"] = links[ind2,"value"] + SPA_annee$contribution[i]
                links[ind3,"value"] = links[ind3,"value"] + SPA_annee$contribution[i]
              }
            }
          }
        }
      }
    }
  }
  links <- links[links$value!=0,]
  nodes = data.frame(name = name)
  return(
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  units = "", fontSize = 12, nodeWidth = 30)
  )
}
