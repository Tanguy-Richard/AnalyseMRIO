% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MatCol.R
\name{MatCol}
\alias{MatCol}
\title{MatCol}
\usage{
MatCol(MAT, C1, C2)
}
\arguments{
\item{MAT}{Un data.frame avec une colonne "C1" et une "C2"}

\item{C1}{Un nom de colonne pour nommer la matrice}

\item{C2}{Un second nom de colonne}
}
\value{
Une matrice ayant pour nom de ligne "C1"_"C2"
}
\description{
Supprime les colonnes Lig_Country et Lig_Indus en rend une matrice
}
\examples{
\dontrun{

data(BASE_ECOLE)

#Construire la matrice de Leontief
MRIO_1990_ALL<-CompoECOLEouA17(BASE_ECOLE,"OptFullOptionsBonus",date=1990)
A <- MRIO_1990_ALL[["A_tab"]][,-1] # On enlève la colonne année
Leontief <- MatCol(A,"Lig_Country","Lig_Indus")
View(Leontief)

}
}
