#' CompoECOLEouA17
#'
#' Sélection d'une composante du mrio ECOLE ou A17 (attention priorité par défaut : Figaro sur 2010-2014)
#'
#' @param dt_MRIO Data frame soit Ecole, soit A17
#' @param typeCompo Une composante parmi :
#' - "CI"                     : Consommation intermédiaire
#' - "DF"                     : Demande finale
#' - "PROD"                   : Production
#' - "VA"                     : Valeur ajouté
#' - "A"                      : Matrice des coefficients techniques (Leontief)
#' - "B"                      :
#' - "CI_PR"                  :
#' - "CI_BR"                  :
#' - "DF_TOT"                 :
#' - "L"                      : Inverse de Leontief
#' - "invB"                   :
#' - "OptFullOptions"         : Liste de resultat
#' - "OptFullOptionsBonus"    : Liste de l'ensemble des resultats
#' @param date Année parmi celle présente dans le data frame
#' @param country Pays parmi ceux du data frame
#' @param OptTab Option pour avoir un resultat sous forme de tableau (défault : FALSE)
#' @param OptMRIO1014 Option de choix de base sur la période 2010-2014 : par défaut c’est FIGARO qui est retenu
#' @param OptMRIO2000 Option de choix de base pour l’année 2000 : par défaut c’est WIOD qui est retenu
#'
#' @return Un data frame correspondant à la selection
#' ou pour les 2 options : "OptFullOptions" et "OptFullOptionsBonus"  une liste de data frame
#' @export
#'
#' @importFrom data.table dcast setnames setorder :=
#'
#' @examples
#' \dontrun{
#'
#' data(BASE_ECOLE)
#' MRIO_1990_ALL<-CompoECOLEouA17(BASE_ECOLE,"OptFullOptionsBonus",date=1990)
#' summary(MRIO_1990_ALL$L)
#'
#' }
CompoECOLEouA17 <-
  function(dt_MRIO,
           typeCompo,
           date = 9999,
           country = "ALL",
           OptTab = FALSE,
           OptMRIO1014 = "FIGARO",
           OptMRIO2000 = "WIOD")
    #options: =CI:DF;PROD;VA;A;B;CI_PR;CI_BR;DF_TOTé;L;invBé;OptFullOptionsé;OptFullOptionsBonusé: les deux derniéres options sortent des listes avec l'ensemble des sorties individuelles (le version 'Bonus' est exhaustive mais plus longue car inversions matricielles
  {
    interm <- dt_MRIO

    List_GEO <-
      c(
        "ABW",
        "AFG",
        "AGO",
        "AIA",
        "ALA",
        "ALB",
        "AND",
        "ARE",
        "ARG",
        "ARM",
        "ASM",
        "ATA",
        "ATF",
        "ATG",
        "AUS",
        "AUT",
        "AZE",
        "BDI",
        "BEL",
        "BEN",
        "BFA",
        "BGD",
        "BGR",
        "BHR",
        "BHS",
        "BIH",
        "BLM",
        "BLR",
        "BLZ",
        "BMU",
        "BOL",
        "BRA",
        "BRB",
        "BRN",
        "BTN",
        "BVT",
        "BWA",
        "CAF",
        "CAN",
        "CCK",
        "CHE",
        "CHL",
        "CHN",
        "CIV",
        "CMR",
        "COD",
        "COG",
        "COK",
        "COL",
        "COM",
        "CPV",
        "CRI",
        "CUB",
        "CXR",
        "CYM",
        "CYP",
        "CZE",
        "DEU",
        "DJI",
        "DMA",
        "DNK",
        "DOM",
        "DZA",
        "ECU",
        "EGY",
        "ERI",
        "ESH",
        "ESP",
        "EST",
        "ETH",
        "FIN",
        "FJI",
        "FLK",
        "FRA",
        "FRO",
        "FSM",
        "GAB",
        "GBR",
        "GEO",
        "GGY",
        "GHA",
        "GIB",
        "GIN",
        "GLP",
        "GMB",
        "GNB",
        "GNQ",
        "GRC",
        "GRD",
        "GRL",
        "GTM",
        "GUF",
        "GUM",
        "GUY",
        "HKG",
        "HMD",
        "HND",
        "HRV",
        "HTI",
        "HUN",
        "IDN",
        "IMN",
        "IND",
        "IOT",
        "IRL",
        "IRN",
        "IRQ",
        "ISL",
        "ISR",
        "ITA",
        "JAM",
        "JEY",
        "JOR",
        "JPN",
        "KAZ",
        "KEN",
        "KGZ",
        "KHM",
        "KIR",
        "KNA",
        "KOR",
        "KWT",
        "LAO",
        "LBN",
        "LBR",
        "LBY",
        "LCA",
        "LIE",
        "LKA",
        "LSO",
        "LTU",
        "LUX",
        "LVA",
        "MAC",
        "MAF",
        "MAR",
        "MCO",
        "MDA",
        "MDG",
        "MDV",
        "MEX",
        "MHL",
        "MKD",
        "MLI",
        "MLT",
        "MMR",
        "MNE",
        "MNG",
        "MNP",
        "MOZ",
        "MRT",
        "MSR",
        "MTQ",
        "MUS",
        "MWI",
        "MYS",
        "MYT",
        "NAM",
        "NCL",
        "NER",
        "NFK",
        "NGA",
        "NIC",
        "NIU",
        "NLD",
        "NOR",
        "NPL",
        "NRU",
        "NZL",
        "OMN",
        "PAK",
        "PAN",
        "PCN",
        "PER",
        "PHL",
        "PLW",
        "PNG",
        "POL",
        "PRI",
        "PRK",
        "PRT",
        "PRY",
        "PSE",
        "PYF",
        "QAT",
        "REU",
        "ROU",
        "RUS",
        "RWA",
        "SAU",
        "SDN",
        "SEN",
        "SGP",
        "SGS",
        "SHN",
        "SJM",
        "SLB",
        "SLE",
        "SLV",
        "SMR",
        "SOM",
        "SPM",
        "SRB",
        "STP",
        "SUR",
        "SVK",
        "SVN",
        "SWE",
        "SWZ",
        "SYC",
        "SYR",
        "TCA",
        "TCD",
        "TGO",
        "THA",
        "TJK",
        "TKL",
        "TKM",
        "TLS",
        "TON",
        "TTO",
        "TUN",
        "TUR",
        "TUV",
        "TWN",
        "TZA",
        "UGA",
        "UKR",
        "UMI",
        "URY",
        "USA",
        "UZB",
        "VAT",
        "VCT",
        "VEN",
        "VGB",
        "VIR",
        "VNM",
        "VUT",
        "WLF",
        "WSM",
        "YEM",
        "ZAF",
        "ZMB",
        "ZWE",
        "ROW"
      )
    List_BR <-
      c(
        "AGR_INDU",
        "ENRJ",
        "SERV_ABRIT",
        "SERV_EXPO",
        "A01",
        "A02",
        "A03",
        "AtB",
        "AZ",
        "B",
        "C",
        "C1",
        "C10-C12",
        "C10T12",
        "C13-C15",
        "C13T15",
        "C16",
        "C17",
        "C18",
        "C19",
        "C2",
        "C20",
        "C21",
        "C22",
        "C23",
        "C24",
        "C25",
        "C26",
        "C27",
        "C28",
        "C29",
        "C3",
        "C30",
        "C31_32",
        "C31_C32",
        "C33",
        "C4",
        "C5",
        "D",
        "D15t16",
        "D17t19",
        "D21t22",
        "D23",
        "D24",
        "D25",
        "D26",
        "D27t28",
        "D29",
        "D30t33",
        "D34t35",
        "D35",
        "DE",
        "Dnec",
        "E",
        "E36",
        "E37-E39",
        "E37T39",
        "F",
        "FZ",
        "G",
        "G45",
        "G46",
        "G47",
        "GZ",
        "H",
        "H49",
        "H50",
        "H51",
        "H52",
        "H53",
        "HZ",
        "I",
        "I60t63",
        "I64",
        "IZ",
        "J",
        "J58",
        "J59_60",
        "J59_J60",
        "J61",
        "J62_63",
        "J62_J63",
        "JZ",
        "K",
        "K64",
        "K65",
        "K66",
        "KZ",
        "L",
        "L68",
        "LtQ",
        "LZ",
        "M69_70",
        "M69_M70",
        "M71",
        "M72",
        "M73",
        "M74_75",
        "M74_M75",
        "MN",
        "N",
        "N77",
        "N78",
        "N79",
        "N80T82",
        "O84",
        "OQ",
        "P85",
        "Q",
        "Q86",
        "Q87_88",
        "R_S",
        "R90T92",
        "R93",
        "RU",
        "S94",
        "S95",
        "S96",
        "T",
        "U"
      )
    List_DF <-
      c(
        "CONS_h",
        "CONS_g",
        "CONS_np",
        "GFCF",
        "INVEN",
        "P3_S13",
        "P3_S14",
        "P3_S15",
        "P51G",
        "P5M",
        "xCONS_h",
        "xCONS_g",
        "xGFCF",
        "xINV"
      )
    List_BRDF <-
      c(
        "AGR_INDU",
        "ENRJ",
        "SERV_ABRIT",
        "SERV_EXPO",
        "A01",
        "A02",
        "A03",
        "AtB",
        "AZ",
        "B",
        "C",
        "C1",
        "C10-C12",
        "C10T12",
        "C13-C15",
        "C13T15",
        "C16",
        "C17",
        "C18",
        "C19",
        "C2",
        "C20",
        "C21",
        "C22",
        "C23",
        "C24",
        "C25",
        "C26",
        "C27",
        "C28",
        "C29",
        "C3",
        "C30",
        "C31_32",
        "C31_C32",
        "C33",
        "C4",
        "C5",
        "D",
        "D15t16",
        "D17t19",
        "D21t22",
        "D23",
        "D24",
        "D25",
        "D26",
        "D27t28",
        "D29",
        "D30t33",
        "D34t35",
        "D35",
        "DE",
        "Dnec",
        "E",
        "E36",
        "E37-E39",
        "E37T39",
        "F",
        "FZ",
        "G",
        "G45",
        "G46",
        "G47",
        "GZ",
        "H",
        "H49",
        "H50",
        "H51",
        "H52",
        "H53",
        "HZ",
        "I",
        "I60t63",
        "I64",
        "IZ",
        "J",
        "J58",
        "J59_60",
        "J59_J60",
        "J61",
        "J62_63",
        "J62_J63",
        "JZ",
        "K",
        "K64",
        "K65",
        "K66",
        "KZ",
        "L",
        "L68",
        "LtQ",
        "LZ",
        "M69_70",
        "M69_M70",
        "M71",
        "M72",
        "M73",
        "M74_75",
        "M74_M75",
        "MN",
        "N",
        "N77",
        "N78",
        "N79",
        "N80T82",
        "O84",
        "OQ",
        "P85",
        "Q",
        "Q86",
        "Q87_88",
        "R_S",
        "R90T92",
        "R93",
        "RU",
        "S94",
        "S95",
        "S96",
        "T",
        "U",
        "CONS_h",
        "CONS_g",
        "CONS_np",
        "GFCF",
        "INVEN",
        "P3_S13",
        "P3_S14",
        "P3_S15",
        "P51G",
        "P5M",
        "xCONS_h",
        "xCONS_g",
        "xGFCF",
        "xINV"
      )

    if (date != 9999) {
      interm <- interm[year == date, ]
    }
    if (country != "ALL") {
      interm <- interm[Lig_Country == country & Col_Country == country, ]
    }
    if (date >= 2010 &
        date <= 2014) {
      # Cas ou on a 2 bases mrio possibles sur la période : on privilégie FIGARO par défaut mais on conserve la possibilité de récupérer WIOD
      if (OptMRIO1014 != "FIGARO") {
        interm <- interm[MRIO == "WIOD", ]
      } else{
        interm <- interm[MRIO == "FIGARO", ]
      }
    }
    if (date == 2000) {
      # Cas ou on a 2 bases mrio possibles en 2000 : on privilégie WIOD par défaut mais on conserve la possibilité de récupérer LR-WIOD
      if (OptMRIO2000 == "WIOD") {
        interm <- interm[MRIO == "WIOD", ]
      } else{
        interm <- interm[MRIO == "LR_WIOD", ]
      }
    }
    if (typeCompo == "CI") {
      interm <- interm[Lig_Indus %in% List_BR & Col_Indus %in% List_BR, ]
      if (OptTab == TRUE) {
        interm <-
          dcast(interm,
                year + Lig_Country + Lig_Indus  ~ Col_Country + Col_Indus,
                value.var = "value")
      } # Mise en format tab
    }
    if (typeCompo == "DF") {
      interm <- interm[Lig_Indus %in% List_BR & Col_Indus %in% List_DF, ]
      if (OptTab == TRUE) {
        interm <-
          dcast(interm,
                year + Lig_Country + Lig_Indus  ~ Col_Country + Col_Indus,
                value.var = "value")
      } # Mise en format tab
    }
    if (typeCompo == "PROD") {
      # Recalcul a la volee a partir du MRIO pour eviter les incohérences
      interm <-
        interm[Lig_Indus %in% List_BR & Col_Indus %in% List_BRDF, ]
      interm <-
        interm[, .(sum(value)), by = c("MRIO", "year", "Lig_Indus", "Lig_Country")]
      setnames(interm, "V1", "value")
      setorder(interm, Lig_Country, Lig_Indus)
    }
    if (typeCompo == "A") {
      # Calcul des coefficients techniques
      Part_CI <-
        CompoECOLEouA17(
          interm,
          "CI",
          date = date,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      Part_Prod <-
        CompoECOLEouA17(
          interm,
          "PROD",
          date = date,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      setnames(Part_Prod, "Lig_Indus", "Col_Indus")
      setnames(Part_Prod, "Lig_Country", "Col_Country")
      A_merge <-
        Part_Prod[Part_CI, on = .(Col_Indus, Col_Country, year, MRIO)]
      A_merge$value <- A_merge$i.value / A_merge$value
      A_merge <- GereInfNA(A_merge[, i.value := NULL])
      interm <- A_merge
      if (OptTab == TRUE) {
        interm <-
          dcast(interm,
                year + Lig_Country + Lig_Indus  ~ Col_Country + Col_Indus,
                value.var = "value")
      } # Mise en format tab
    }
    if (typeCompo == "B") {
      # Calcul des coefficients de débouchés
      Part_CI <-
        CompoECOLEouA17(
          interm,
          "CI",
          date = date,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      Part_Prod <-
        CompoECOLEouA17(
          interm,
          "PROD",
          date = date,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      B_merge <-
        Part_Prod[Part_CI, on = .(Lig_Indus, Lig_Country, year, MRIO)]
      B_merge$value <- B_merge$i.value / B_merge$value
      B_merge <- GereInfNA(B_merge[, i.value := NULL])
      interm <- B_merge
      if (OptTab == TRUE) {
        interm <-
          dcast(interm,
                year + Lig_Country + Lig_Indus  ~ Col_Country + Col_Indus,
                value.var = "value")
      } # Mise en format tab
    }
    if (typeCompo == "CI_PR") {
      # Recalcul a la volee a partir du MRIO pour eviter les incohérences
      interm <-
        CompoECOLEouA17(
          interm,
          "CI",
          date = date,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm <-
        interm[, .(sum(value)), by = c("MRIO", "year", "Lig_Indus", "Lig_Country")]
      setnames(interm, "V1", "value")
      setorder(interm, Lig_Country, Lig_Indus)
    }
    if (typeCompo == "CI_BR") {
      # Recalcul a la volee a partir du MRIO pour eviter les incohérences
      interm <-
        CompoECOLEouA17(
          interm,
          "CI",
          date = date,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm <-
        interm[, .(sum(value)), by = c("MRIO", "year", "Col_Indus", "Col_Country")]
      setnames(interm, "V1", "value")
      setorder(interm, Col_Country, Col_Indus)
    }
    if (typeCompo == "DF_TOT") {
      # Recalcul a la volee a partir du MRIO pour eviter les incohérences
      interm <-
        CompoECOLEouA17(
          interm,
          "DF",
          date = date,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm <-
        interm[, .(sum(value)), by = c("MRIO", "year", "Lig_Indus", "Lig_Country")]
      setnames(interm, "V1", "value")
      setorder(interm, Lig_Country, Lig_Indus)
    }
    if (typeCompo == "VA") {
      # On extrait le TEI dans un premier temps
      Part_SommeCI <-
        CompoECOLEouA17(
          interm,
          "CI_BR",
          date = date,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      Part_SommeCI$value <- -Part_SommeCI$value
      # On calcule la prod dans un recond temps
      Part_Prod <-
        CompoECOLEouA17(
          interm,
          "PROD",
          date = date,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      # On calcule le solde entre la prod et la somme des CI Branche (concept de VA au prix de base)
      setnames(Part_Prod, "Lig_Indus", "Col_Indus") # Recode pour passer les PR en BR
      setnames(Part_Prod, "Lig_Country", "Col_Country") # Recode pour passer les PR en BR
      interm <- rbind(Part_Prod, Part_SommeCI)
      interm <-
        interm[, .(sum(value)), by = c("MRIO", "year", "Col_Indus", "Col_Country")]
      setnames(interm, "V1", "value")
      setorder(interm, Col_Country, Col_Indus)
    }
    if (typeCompo == "L") {
      # Calcul de l'inverse de Léontief
      A <-
        CompoECOLEouA17(
          interm,
          "A",
          date = date,
          OptTab = TRUE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      nb_ligcol <- nrow(A)
      MatrixA <- as.matrix(A[, 4:ncol(A)])
      interm <- inversion_rcpp3(diag(nb_ligcol) - MatrixA)
      colnames(interm) <- colnames(A)[4:ncol(A)]
      rownames(interm) <- colnames(A)[4:ncol(A)]
      interm <- as.data.frame(interm)
    }
    if (typeCompo == "invB") {
      # Calcul de l'inverse de Ghosh
      B <-
        CompoECOLEouA17(
          interm,
          "B",
          date = date,
          OptTab = TRUE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      nb_ligcol <- nrow(B)
      MatrixB <- as.matrix(B[, 4:ncol(B)])
      interm <- inversion_rcpp3(diag(nb_ligcol) - MatrixB)
      colnames(interm) <- colnames(B)[4:ncol(B)]
      rownames(interm) <- colnames(B)[4:ncol(B)]
      interm <- as.data.frame(interm)
    }
    if (typeCompo == "OptFullOptions") {
      interm1 <-
        CompoECOLEouA17(
          dt_MRIO,
          "PROD",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm2 <-
        CompoECOLEouA17(
          dt_MRIO,
          "CI",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm3 <-
        CompoECOLEouA17(
          dt_MRIO,
          "DF",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm4 <-
        CompoECOLEouA17(
          dt_MRIO,
          "VA",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm5 <-
        CompoECOLEouA17(
          dt_MRIO,
          "CI_PR",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm6 <-
        CompoECOLEouA17(
          dt_MRIO,
          "CI_BR",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm7 <-
        CompoECOLEouA17(
          dt_MRIO,
          "DF_TOT",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm8 <-
        CompoECOLEouA17(
          dt_MRIO,
          "A",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm2tab <-
        CompoECOLEouA17(
          dt_MRIO,
          "CI",
          date = date,
          country = country,
          OptTab = TRUE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm3tab <-
        CompoECOLEouA17(
          dt_MRIO,
          "DF",
          date = date,
          country = country,
          OptTab = TRUE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm8tab <-
        CompoECOLEouA17(
          dt_MRIO,
          "A",
          date = date,
          country = country,
          OptTab = TRUE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm <-
        list(
          PROD = interm1,
          CI = interm2,
          DF = interm3,
          VA = interm4,
          CI_PR = interm5,
          CI_BR = interm6,
          DF_TOT = interm7,
          A = interm8,
          CI_tab = interm2tab,
          DF_tab = interm3tab,
          A_tab = interm8tab
        )
    }
    if (typeCompo == "OptFullOptionsBonus") {
      interm1 <-
        CompoECOLEouA17(
          dt_MRIO,
          "PROD",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm2 <-
        CompoECOLEouA17(
          dt_MRIO,
          "CI",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm3 <-
        CompoECOLEouA17(
          dt_MRIO,
          "DF",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm4 <-
        CompoECOLEouA17(
          dt_MRIO,
          "VA",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm5 <-
        CompoECOLEouA17(
          dt_MRIO,
          "CI_PR",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm6 <-
        CompoECOLEouA17(
          dt_MRIO,
          "CI_BR",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm7 <-
        CompoECOLEouA17(
          dt_MRIO,
          "DF_TOT",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm8 <-
        CompoECOLEouA17(
          dt_MRIO,
          "A",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm2tab <-
        CompoECOLEouA17(
          dt_MRIO,
          "CI",
          date = date,
          country = country,
          OptTab = TRUE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm3tab <-
        CompoECOLEouA17(
          dt_MRIO,
          "DF",
          date = date,
          country = country,
          OptTab = TRUE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm8tab <-
        CompoECOLEouA17(
          dt_MRIO,
          "A",
          date = date,
          country = country,
          OptTab = TRUE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm9 <-
        CompoECOLEouA17(
          dt_MRIO,
          "B",
          date = date,
          country = country,
          OptTab = FALSE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm10 <-
        CompoECOLEouA17(
          dt_MRIO,
          "B",
          date = date,
          country = country,
          OptTab = TRUE,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        )
      interm11 <-
        CompoECOLEouA17(
          dt_MRIO,
          "L",
          date = date,
          country = country,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        ) # Resultat directement sous forme de tableau
      interm12 <-
        CompoECOLEouA17(
          dt_MRIO,
          "invB",
          date = date,
          country = country,
          OptMRIO1014 = OptMRIO1014,
          OptMRIO2000 = OptMRIO2000
        ) # Resultat directement sous forme de tableau
      interm <-
        list(
          PROD = interm1,
          CI = interm2,
          DF = interm3,
          VA = interm4,
          CI_PR = interm5,
          CI_BR = interm6,
          DF_TOT = interm7,
          A = interm8,
          CI_tab = interm2tab,
          DF_tab = interm3tab,
          A_tab = interm8tab,
          B = interm9,
          B_tab = interm10,
          L = interm11,
          InvBGhosh = interm12
        )
    }
    return(interm)
  }
