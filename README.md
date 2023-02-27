
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AnalyseMRIO

<!-- badges: start -->
<!-- badges: end -->

L’objectif du package est de fournir des outils pour l’analyse de chaine
de valeur.

## Installation

L’installation du package peut se faire comme si dessous:

``` r
library(devtools)
install_github("Tanguy-Richard/AnalyseMRIO")
```

## Exemple sur la base interne au package

### 1. Mise en place

On importe le package et la base présente

``` r
library(AnalyseMRIO)
data(BASE_ECOLE)
```

Pour afficher les caractéristiques d’une fonction du package vous pouvez
passer par la commande:

``` r
help("SPA_opt")
```

### 2. Extraction de différentes composantes de la base

On choisit l’année 1990, et on importe la liste des différentes
composantes.

``` r
#test pour l'année 1990 dans la base école
ECOLE_1990_ALL<-CompoECOLEouA17(BASE_ECOLE,"OptFullOptionsBonus",date=1990)
```

On met à part les différentes composantes qui vont nous servir. On
cherchera à qualifiér l’impact des différents chemins de production en
terme de valeur ajoutée.

``` r
#La matrice de Leontief
A <- ECOLE_1990_ALL[["A_tab"]][,-1] # On enlève la colonne année
A <- MatCol(A,"Lig_Country","Lig_Indus")

#La demande finale Y
Y <- ECOLE_1990_ALL[["DF_TOT"]]
Y <- Y[,-c(1,2)] # prendre uniquement la colonne d'interet
Y <- MatCol(Y,"Lig_Country","Lig_Indus")

#Le vecteur des impacts (ici en valeur ajoutée) par pays/secteur et unité de production
## valeur ajoutée par pays/secteur
VA <- ECOLE_1990_ALL[["VA"]]
VA <- VA[,-c(1,2)] # prendre uniquement la colonne d'interet
VA <- MatCol(VA,"Col_Country","Col_Indus")

## unités de production par pays/secteur
P <- ECOLE_1990_ALL[["PROD"]]
P <- P [,-c(1,2)]
P <- MatCol(P,"Lig_Country","Lig_Indus")

## vecteur des impacts (valeur ajouté par unité de production)
VA <- VA/P
```

### 3. Contribution d’un noeud

On se donne un noeud (chaine de secteurs), sous la forme d’un vecteur de
chaîne de caractères.

``` r
# On donne un parcours pour la base ECOLE
Z <- c( "CHN_ENRJ", "CHN_AGR_INDU", "UE_OTHERS_AGR_INDU")
```

L’impact en valeur ajoutée du noeud est donné par :

``` r
Contribution_Node(VA,A,Y,Z)
#> [1] 0.8802482
```

### 4. Contribution d’un sous arbre

La contribution d’un sous arbre d’un noeud Z est l’impact de l’ensemble
des productions intermédiaires antérieures servant à la production de ce
noeud. En conservant le même noeud, on la calcule :

``` r
Contribution_SubTree(VA,A,Y,Z)
#>          [,1]
#> [1,] 2.364121
```

### 5. Construction d’un arbre

On va maintenant construire un arbre à partir d’un noeud. On stocke
l’ensemble des chemins et de leur impact sous la forme de listes
imbriquées, donnant le nom du noeud, la valeur de l’impact et une liste
des noeuds suivants (stockés sous la même forme).

Pour s’arreter on fixe une profondeur maximale à 5 et une tolérance à
0.5 (c’est à dire que si la contribution d’un sous arbre issue d’un
noeud est inférieur à 0.5, on ne l’explore pas)

Si on veut partir d’un noeud fixé, on utilise par exemple:

``` r
test <- Construct_Tree(VA,A,Y,5,0.5,Z)
```

Si on veut l’ensemble des chemins on utilise:

``` r
test2 <- Construct_Tree(VA,A,Y,5,0.5)
```

### 6. Lecture de l’arbre

Pour utiliser un arbre on doit le lire. La fonction “Read_Tree” permet
de lire un arbre et de rendre un tableau recensant les noeuds et leurs
impacts. Avec les arbres précédents on obtient:

``` r
lecture <- Read_Tree(test)
# nombre de noeuds dans le tableau :
length(lecture$Node)
#> [1] 0
# Noeuds les plus importants :
head(lecture[order(lecture$Node,decreasing = TRUE),])
#> Empty data.table (0 rows and 2 cols): node,contribution
```

``` r
lecture2 <- Read_Tree(test2)
# nombre de noeuds dans le tableau :
length(lecture2$Node)
#> [1] 0
# Noeuds les plus importants :
head(lecture2[order(lecture2$Node,decreasing = TRUE),])
#> Empty data.table (0 rows and 2 cols): node,contribution
```

### 7. Une fonction tout en 1 plus rapide ?

#### 7.1 Utilisation

La fonction “SPA_opt” permet une compilation directe du tableau sans
passé par la réalisation d’un arbre. Sa structure permet d’avoir moins
d’opération.

Si on veut l’utiliser avec un noeud fixé on doit aussi calculé la valeur
du chemin pour y arriver. C’est à dire le produit de la demande finale
du secteur et des différents coefficients techniques.

``` r
# On donnes un parcours
Z <- c( "CHN_ENRJ")
# On donne la valeur du coef associé au chemin
cc <- Y[Z,] # Ici de profondeur 1 donc cela correspond juste à la demande finale

# Contribution du sous arbre
test3 <- SPA_opt(VA,A,Y,5,0.5,Z,cc)
head(test3[order(test3$contri,decreasing = TRUE),])
#>                                  node contribution
#> 1:                           CHN_ENRJ    1910.6483
#> 2:                  CHN_ENRJ~CHN_ENRJ     529.9068
#> 3:              CHN_ENRJ~CHN_AGR_INDU     351.0810
#> 4:             CHN_ENRJ~CHN_SERV_EXPO     223.8073
#> 5: CHN_ENRJ~CHN_AGR_INDU~CHN_AGR_INDU     147.9172
#> 6:         CHN_ENRJ~CHN_ENRJ~CHN_ENRJ     146.9665
```

Pour avoir tout les noeuds on utilise :

``` r
test4 <- SPA_opt(VA,A,Y,5,0.5)
head(test4[order(test4$contri,decreasing = TRUE),])
#>                             node contribution
#> 1:                ROW_SERV_ABRIT    2853734.4
#> 2:                USA_SERV_ABRIT    2188072.3
#> 3:          UE_OTHERS_SERV_ABRIT    1289154.8
#> 4:                  ROW_AGR_INDU     849620.6
#> 5: ROW_SERV_ABRIT~ROW_SERV_ABRIT     418041.5
#> 6: USA_SERV_ABRIT~USA_SERV_ABRIT     410322.2
```

#### 7.2 Comparaison

On teste la rapidité des deux méthodes sur notre base. (on teste aussi
uns version intermédiaire, construisant directement le tableau en
utilisant les fonctions : contribution_node / contribution_subtree)

``` r
library(rbenchmark)
benchmark("Algorithme papier" = {
            test <- Construct_Tree(VA,A,Y,10,2)
            lecture <- Read_Tree(test2)
          },
          "Version comprimée" = {
            test <- SPA_opt(VA,A,Y,10,2)
          },
          "Version intermédiaire" = {
            test <- SPA_int(VA,A,Y,10,2)
          },
          "Version mieux" = {
            test <- SPA_opt2(VA,A,Y,10,2)
          },
          replications = 5,
          columns = c("test", "replications", "elapsed",
                      "relative", "user.self", "sys.self"))
benchmark("Algorithme papier" = {
            test <- Construct_Tree(VA,A,Y,5,0.5)
            lecture <- Read_Tree(test2)
          },
          "Version comprimée" = {
            test <- SPA_opt(VA,A,Y,5,0.5)
          },
          "Version intermédiaire" = {
            test <- SPA_int(VA,A,Y,5,0.5)
          },
          "Version mieux" = {
            test <- SPA_opt2(VA,A,Y,5,0.5)
          },
          replications = 5,
          columns = c("test", "replications", "elapsed",
                      "relative", "user.self", "sys.self"))
benchmark("Algorithme papier" = {
            test <- Construct_Tree(VA,A,Y,4,0)
            lecture <- Read_Tree(test2)
          },
          "Version comprimée" = {
            test <- SPA_opt(VA,A,Y,4,0)
          },
          "Version intermédiaire" = {
            test <- SPA_int(VA,A,Y,4,0)
          },
          "Version mieux" = {
            test <- SPA_opt2(VA,A,Y,4,0)
          },
          replications = 5,
          columns = c("test", "replications", "elapsed",
                      "relative", "user.self", "sys.self"))
benchmark("Algorithme papier" = {
            test <- Construct_Tree(VA,A,Y,20,1)
            lecture <- Read_Tree(test2)
          },
          "Version comprimée" = {
            test <- SPA_opt(VA,A,Y,20,1)
          },
          "Version intermédiaire" = {
            test <- SPA_int(VA,A,Y,20,1)
          },
          "Version mieux" = {
            test <- SPA_opt2(VA,A,Y,20,1)
          },
          replications = 5,
          columns = c("test", "replications", "elapsed",
                      "relative", "user.self", "sys.self"))
```

Les méthodes sont comparable en temps =\> il faudrait voir sur plusieurs
replications (en gros cela varie celon les essais). Dans l’idée, R a du
mal avec la mémoire, donc si trop de noeud la version comprimé (un
argument supplémentaire) a du mal. Mais si arbre profond elle va plus
vite (plus efficace pour Tmax grand et tol grand)

## Utilisation de nos données en analyse
