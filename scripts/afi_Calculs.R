##### fonction pour rajouter (le cas échéant) le code "M1" ou "M2" dans les codes écolos #####
check_dmh_for_standing_deadwood <- function(arbres_table = NULL) {
  arbres_table <- 
    arbres_table %>% 
    mutate(
      add_deadwood_dmh = case_when(
        (!is.na(Type) | !is.na(Stade)) & Diam1 < 30 ~ "M1",
        (!is.na(Type) | !is.na(Stade)) & Diam1 >= 30 ~ "M2"
      ),
      CodeEcolo = toupper(CodeEcolo),
      tmp = case_when(
        !is.na(CodeEcolo) ~ CodeEcolo,
        !is.na(CodeEcolo) & !str_detect(CodeEcolo, add_deadwood_dmh) ~ paste0(CodeEcolo, add_deadwood_dmh),
        is.na(CodeEcolo) ~ add_deadwood_dmh
      ),
      CodeEcolo = tmp,
      
      add_deadwood_dmh = NULL,
      tmp = NULL
    )
  
  # -- return of 'check_dmh_for_standing_deadwood'
  return(arbres_table)
}


##### fonction de préparation de la table Arbres (extrait de Forestree::) #####

#' Pouvoir d'achat
#'
#' @description Calcul du tableau permet d'exprimer des flux financiers en euros constants.
#'
#' @return La fonction renvoie un data frame.
#'
#' @param an = année retenue
#'
#' @import dplyr
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' library(Forestree)
#' data(PA)
#' Coeft <- PouvoirAchat(2002)
#'
#' @export

PouvoirAchat <- function(
  an = NULL, PA = NULL
) {
  # -- dernière année avec les taux d'inflation renseignées (année actuelle - 1)
  former_year <- as.numeric(format(Sys.time(), "%Y")) - 1 
  
  # -- table des taux d'inflation
  if ( is.numeric(an) & an <= former_year & an >= 1950 ) {

    # set up
    euro = 6.55957
    
    # filter
    tab <- PA %>% rename("year" = "Année") %>% filter(year <= an)
    
    # value max
    value_max = tab$Infla[dim(tab)[1]]
    
    # table pour exprimer les flux financiers en euros constants 
    tab <- 
      tab %>%
      mutate(
        Coefft = 1,
        Coefft = ifelse(year < 2002, 1 / euro, Coefft),
        Coefft = ifelse(year < 1960, 1 / euro / 100, Coefft),
        Infla = value_max / Infla,
        Coefft = Coefft * Infla
      ) %>%
      select(year, Coefft)
    
    # -- return of PouvoirAchat function
    return(tab)
  } else { 
    print(paste0("l'année doit être un entier compris entre 1950 et ", an, "."))
  }
}


##### fonction de préparation de la table Arbres #####
prep_df <- function(df = NULL) {
  df <-
    df %>%
    mutate(
      Cycle = as.numeric(Cycle),
      Limite = 1,
      Distance = as.numeric(Distance),
      Azimut = as.numeric(Azimut),
      Diam1 = as.numeric(Diam1),
      Diam2 = as.numeric(Diam2),
      HautT = as.numeric(HautT),
      Stade = as.numeric(Stade),
      Diam1 = ifelse(is.na(Diam1), Diam2, Diam1),
      Diam2 = ifelse(is.na(Diam2), Diam1, Diam2),
      Diam1 = ifelse(Diam1 == 0, Diam2, Diam1),
      Diam2 = ifelse(Diam2 == 0, Diam1, Diam2),
      Diam = (Diam1 + Diam2) / 2,
      Classe = floor(Diam / 5 + 0.5) * 5,
      Cat = cut(
        Diam,
        breaks = c(0, 17.5, 27.5, 47.5, 67.5, 200),
        labels = c("PER", "PB", "BM", "GB","TGB"),
        include.lowest = T,
        right = F
      ),
      Cat = as.character(Cat),
      Nha = NA,
      # suppression des codes écolo pour les perches
      CodeEcolo = ifelse(Diam < 17.5, NA, CodeEcolo)
    ) %>%
    arrange(NumDisp, NumPlac, NumArbre, Cycle) %>%
    filter(
      !is.na(Diam1) | !is.na(Diam2) | Diam1 == 0 | Diam2 == 0 # supprime les arbres où les 2 diamètres sont vides
    )
  
  # Retour fonction prep_df
  return(df)
}

##### fonction calculs des volumes (pour les précomptables) #####
calculs_Vol <- function(df = NULL, IFN = F, Sup = F) {
  add_var <- ""
  add_Tarif <- ""
  add_Diam <- ""
  if (IFN == T) {
    add_var <- "IFN"
    add_Tarif <- "IFN"
    add_Diam <- ""
  }
  if (Sup == T) {
    add_var <- "Sup"
    add_Tarif <- ""
    add_Diam <- "Sup"
  }
  
  # Pour mémo (1) fonctionnement sans argument "add_var" :
  # var <- enquo(var)
  # var_name <- quo_name(var) # puis appel !!var_name:=f(!!var) dans mutate
  
  # Pour mémo (2) fonctionnement avec argument d'entrée définissant la variable (Vha)
  # var <- enquo(var)
  # var_name <- paste0(quo_name(var),add_var)
  
  # construction des variables
  var_name <- paste0("Vha", add_var)
  var <- quo(!!parse_expr(var_name))
  # variable = Type de tarif
  TypeTarif <- paste0("TypeTarif", add_Tarif)
  TypeTarif <- quo(!!parse_expr(TypeTarif))
  # variable = Numéro de tarif
  NumTarif <- paste0("NumTarif", add_Tarif)
  NumTarif <- quo(!!parse_expr(NumTarif))
  # variable = Diamètre
  Diam <- paste0("Diam", add_Diam)
  Diam <- quo(!!parse_expr(Diam))
  
  # calculs des volumes selon les tarifs de cubage
  df <-
    df %>%
    mutate(
      !!var_name := NA,
      !!var_name :=
        ifelse(
          !!TypeTarif=="SchR",
          5 / 70000 * (8 + !!NumTarif) * (!!Diam - 5) * (!!Diam-10) * Nha,
          !!var
        ),
      !!var_name :=
        ifelse(
          !!TypeTarif=="SchI",
          5 / 80000 * (8 + !!NumTarif) * (!!Diam - 2.5) * (!!Diam - 7.5) * Nha,
          !!var
        ),
      !!var_name :=
        ifelse(
          !!TypeTarif == "SchL",
          5 / 90000*(8 + !!NumTarif) * (!!Diam - 5)* !!Diam * Nha,
          !!var
        ),
      !!var_name :=
        ifelse(
          !!TypeTarif == "SchTL",
          5 / 101250 * (8 + !!NumTarif) * !!Diam * !!Diam * Nha,
          !!var
        ),
      !!var_name :=
        ifelse(
          !!var < 0, 0, !!var
        ) # sécurité pour les tiges de moins de 10 # A revoir ?
    )
  
  # Retour fonction calculs_Vol
  return(df)
}

##### fonction calculs des accroissements #####
calculs_Acct <- function(
  df = NULL, 
  cycles = NULL, 
  last_cycle = NULL,
  tauxR = NULL
) { #, cycles_DF = Cycles
  # df <- Arbres2 #%>% filter(NumPlac == 8 & NumDisp == 4) # debug
  
  # Paramètres initiaux
  # last_cycle <- max(df$cycles, na.rm = T)
  
  if (last_cycle > 1) {
    # table contenant les pas de temps
    tLaps <-
      df %>%
      distinct(NumDisp, NumPlac, NumArbre, IdArbre) %>%
      full_join(cycles[, c("NumDisp", "Cycle", "Annee")], by = "NumDisp", relationship = "many-to-many") %>%
      group_by(NumDisp, NumPlac, NumArbre, IdArbre) %>%
      mutate(
        Inter = Annee - lag(Annee),
        last_cycle = max(Cycle) # TODO : à reporter sur RNF : si las_cycle paramètre, alors si on traite plusieurs dispositifs en même temps pour lesquels last_cyle est différent (ex : NumDisp 4 - last_cycle = 5 et NumDisp 1 last_disp = 6 alors on s'attend à trouver un Cycle 6 pour le Disp 4 -> coupes en trop)
      ) %>%
      ungroup() %>%
      # filter(Cycle > 1) %>%
      left_join(
        distinct(df[, c(
          "IdArbre", "Essence", "EssReg", "Azimut", "Distance"
        )]),
        by = "IdArbre"
      ) %>%
      select(
        NumDisp, NumPlac, NumArbre, Cycle, IdArbre,
        Essence, EssReg, Azimut, Distance, Inter, last_cycle
      )
    
    # Sécurité : contrôle des valeurs dupliquées (ID="NumDisp+NumPlac+NumArbre+Cycle")
    df_Dupl <-
      df %>%
      select(NumDisp, NumPlac, Cycle, Azimut, Distance) %>%  #,Taillis
      arrange(NumDisp, NumPlac, Cycle, Azimut, Distance)
    # rangement de df dans le même ordre que df_Dupl
    df <-
      df %>%
      arrange(NumDisp, NumPlac, Cycle, Azimut, Distance)
    
    # valeurs dupliquées
    Dupl <- which(duplicated(df_Dupl))
    # édition d'un classeur listant les valeurs dupliquées
    if (length(Dupl) > 0) {
      Dupl_INV <- which(duplicated(df_Dupl, fromLast = TRUE))
      Dupl <- c(Dupl, Dupl_INV)
      Error_DF <- df[
        Dupl, c(
          "NumDisp", "NumPlac", "Cycle", "NumArbre", "Essence",
          "Azimut", "Distance", "Diam1"
        )] %>%
        arrange(NumDisp, NumPlac, Cycle, Azimut, Distance)
      write.xlsx(
        Error_DF,
        # file = file.path(output_dir, "Doublons.xlsx")
        file = "Doublons.xlsx"
      )
      stop("Attention : doublon(s) d\u00E9tect\u00E9(s) lors des calculs d'accroissement.\n\nDoublons list\u00E9s dans le classeur excel 'Doublons.xlsx'")
    }
    
    # Calculs des accroissements
    df <-
      df %>% 
      left_join(
        tLaps,
        by = c(
          "NumDisp", "NumPlac", "NumArbre", "Cycle", "IdArbre",
          "Essence", "EssReg", "Azimut", "Distance"
        )
      ) %>%
      # left_join(tLaps, by = c("NumDisp", "NumPlac", "NumArbre", "Cycle")) %>%
      arrange(NumDisp, NumPlac, NumArbre, Cycle) %>%
      group_by(NumDisp, NumPlac, NumArbre) %>%
      mutate(
        # Arbres devenus morts sur pied :
        Coupe =
          ifelse(
            is.na(Type) & !is.na(lead(Type)) & Cycle < last_cycle & is.na(Coupe),
            "C",
            Coupe
          ),
        Coupe =
          ifelse(
            is.na(Type) & !is.na(lead(Type)) & Cycle < last_cycle &
              !is.na(Coupe) & str_detect(Coupe, "PF"),
            "PF/C",
            Coupe
          ),
        Coupe =
          ifelse(
            is.na(Type) & !is.na(lead(Type)) & Cycle < last_cycle &
              !is.na(Coupe) & !str_detect(Coupe, "PF"),
            "C",
            Coupe
          ),
        
        # Arbres coupés :
        Coupe =
          ifelse(
            is.na(lead(Nha)) & Cycle < last_cycle & is.na(Coupe),
            "E",
            Coupe
          ),
        Coupe =
          ifelse(
            is.na(lead(Nha)) & Cycle < last_cycle & !is.na(Coupe) &
              Coupe == "C", #!str_detect(Coupe, "C")
            "C",
            Coupe
          ),
        
        # Arbres passant à la futaie :
        # la colonne coupe peut déjà contenir des infos
        # les notations "C" ou "PF/C" sont à conserver.
        # les notations "E", "PF" ou "PF/E", sont de toutes façons reconstituées
        Coupe =
          ifelse(
            is.na(lag(Nha)) & !is.na(Nha) & Cycle > 1 & !is.na(Coupe) &
              (Coupe == "C" | Coupe == "E"),
            paste("PF", Coupe, sep = "/"),
            Coupe
          ),
        Coupe =
          ifelse(
            is.na(lag(Nha)) & !is.na(Nha) & Cycle > 1 & is.na(Coupe),
            "PF",
            Coupe
          ),
        
        Coupe = ifelse(is.na(Limite), Coupe, NA),
        
        # --- Calcul des accroissements
        # Inter = Annee - lag(Annee),
        Acct_Diam = round( (Diam - lag(Diam, default = NA)) / Inter, digits = 10), # mettre plus bas et ensuite rajouter condition : si Coupe detect C ou E ?
        AcctGper = round( (Gha - lag(Gha, default = 0)) / Inter, digits = 10),
        AcctVper = round( (Vha - lag(Vha, default = 0)) / Inter, digits = 10),
        Gainper = round( (VcHa - lag(VcHa, default = 0)) / Inter, digits = 10),
        VpHaper = Gainper / tauxR
        
      ) %>%
      ungroup() %>%
      as.data.frame() #%>%
    # right_join(
    #   tLaps,
    #   by = c(
    #     "NumDisp", "NumPlac", "NumArbre", "Cycle", "IdArbre",
    #     "Strate", "Essence", "EssReg", "Azimut", "Distance", "Inter"
    #   )
    # ) # A rajouter pour calculs accroissements avec coupés + 1/2 Acct
    
    ### ----- Superassignement : sauvegarde table des accroissements en diamètre ----- ###
    AcctD <<- 
      df %>% 
      select(
        IdArbre, NumDisp, NumPlac, NumArbre, Cycle, 
        Essence, Classe, Acct_Diam
      )
    
    ### ----- Table des accroissements en diamètre ----- ###
    # Attribution d'une valeur d'accroissement en diamètre (si Acct_Diam vide)
    AcctD_DF <-
      df %>% # s'assurer qu'on a bien des arbres limites dans la table
      select(
        IdArbre, NumDisp, NumPlac, NumArbre, Cycle, Essence,
        Classe, Acct_Diam, Coupe
      ) %>%
      
      # valeurs moyennes d'AcctD attribuées par forêt, essence et classe
      group_by(NumDisp, Essence, Classe) %>%
      mutate(AcctD_ForetEssClasse = mean(Acct_Diam, na.rm = T)) %>%
      # sinon valeurs moyennes d'AcctD attribuées par forêt et par essence
      group_by(NumDisp, Essence) %>%
      mutate(AcctD_ForetEss = mean(Acct_Diam, na.rm = T)) %>%
      # sinon valeurs moyennes d'AcctD attribuées par forêt
      group_by(NumDisp) %>%
      mutate(AcctD_Foret = mean(Acct_Diam, na.rm = T)) %>%
      ungroup() %>%
      
      # distribution des valeurs d'AcctD moyennes calculées
      mutate(
        AcctD =
          ifelse(
            is.na(Acct_Diam), AcctD_ForetEssClasse, Acct_Diam
          ),
        AcctD =
          ifelse(
            is.na(AcctD), AcctD_ForetEss, AcctD
          ),
        AcctD =
          ifelse(
            is.na(AcctD), AcctD_Foret, AcctD
          )
      ) %>%
      
      # on remplace les valeurs d'AcctD du cycle 1 par les valeurs du cycle 2
      group_by(NumDisp, NumPlac, NumArbre) %>%
      mutate(
        AcctD =
          ifelse(
            Cycle == 1 & !(Coupe %in% c("E","C")), lead(AcctD), AcctD
          )
      ) %>%
      ungroup() %>%
      
      select(IdArbre, NumDisp,NumPlac, NumArbre, Cycle, AcctD)
    
    # Récupération des AcctD dans la table principale
    df <-
      df %>%
      left_join(
        AcctD_DF,
        c("IdArbre", "NumDisp", "NumPlac", "NumArbre", "Cycle")
      ) %>% 
      # suppression des arbres limites du pool de données
      filter(is.na(Limite))
    # mutate(echant_ID = NA) %>%
    # selection de variables
    # select(
    #   NumDisp, NumPlac, NumArbre, IdArbre, Cycle,
    #   Essence, EssReg, Azimut, Distance,
    #   Diam1, Diam2, Diam, Classe, Cat,
    #   DiamSup, ClasseSup, VhaSup, TauxV,
    #   Qual, Qual1, Qual2, PU, PUSup,
    #   Nha, Gha, Vha, VcHa, #VhaIFN avant VcHa
    #   Coupe, Limite,
    #   CodeEcolo, # Ref_CodeEcolo = AFI systématiquement
    #   HautT, HautL, Ray1, Dh1, Ray2, Dh2,
    #   
    #   AcctGper, AcctVper, Gainper, AcctD,
    #   Coupe, Inter,
    #   
    #   Type, Stade, Observations
    # )
  } else {
    df <- df %>% 
      mutate(
        AcctD = NA, TauxPU = NA, Taux = NA,
        AcctVper = NA, AcctGper = NA, Gainper = NA, VpHaper = NA,
        AcctV = NA, AcctG = NA, Gain = NA, VpHa = NA
      )
    
    ### ----- Superassignement : sauvegarde table des accroissements en diamètre ----- ###
    AcctD <<- 
      tibble(
        IdArbre = as.numeric(), 
        NumDisp = as.numeric(), 
        NumPlac = as.numeric(), 
        NumArbre = as.numeric(), 
        Cycle = as.numeric(), 
        Essence = as.character(), 
        Classe = as.numeric(), 
        Acct_Diam = as.numeric()
      )
  }
  
  # Retour fonction calculs_Acct
  return(df)
}

##### fonction calculs des quantités de carbone #####
calculs_Carbone <- function(
  df = NULL, 
  height_by_class_and_NumTarif = NULL,
  DecroissanceRx = NULL,
  yield_table = NULL,
  DureeVieD = NULL, 
  DureeVieConnexes = NULL
) {
  # # -- table des rendements
  # yield_table <- tibble(
  #   qual = c("A", "B", "C", "D"),
  #   yield = c(0.6, 0.6, 0.6, 0.99)
  # )
  
  # -- table principale
  # df <- Arbres # debug
  df <- 
    df %>% 
    
    # jonction de la table Algan pour avoir les hauteurs de grume (billons 1 et 2) en fonction du numéro de tarif et de la classe de diamètre
    left_join(
      height_by_class_and_NumTarif, 
      by = c("NumTarif" = "Num", "Classe")
    ) %>% 
    
    mutate(
      # -- rappel du poids
      Poids = Nha,
      
      # --- calcul des volumes
      # calcul du volume schaeffer (= total des 2 billons)
      VSch = ifelse(
        TypeTarif == "SchR", 
        # Schaeffer rapide
        5 / 70000 * (8 + NumTarif) * (Diam - 5) * (Diam - 10) * Poids, 
        # Schaeffer lent
        5 / 90000 * (8 + NumTarif) * (Diam - 5) * Diam * Poids
      ),
      # volume du billon 1 (les Fs sur 3m et les Rx sur 6m)
      V1 = ifelse(
        TypeEss == "Fs", 
        # formule du cylindre avec diamètre médian à 1,3 m (~1,5 m)
        pi * Diam ^ 2 / 40000 * 3 * Poids,
        # formule du cylindre avec diamètre médian à 3 m
        pi * (Diam - (DecroissanceRx * 1.7)) ^ 2 / 40000 * 6 * Poids),
      # volume du billon 2 (de 3 m à la decoupe gestionnaire)
      # HYPOTHESE ==> Pour les petits diamètre le volume Schaeffer est considéré nul <== HYPOTHESE #
      V2 = ifelse(VSch - V1 < 0, 0, VSch - V1),
      # volume du billon 3 (qualite fixee a D) = houppier
      V3 = CoefHoupp * VSch,
      
      # -- valeur du taux de carbone
      TauxCarbone = case_when(
        !is.na(TauxCarbone) ~ TauxCarbone / 100,
        # essence avec taux carbone vide ont taux moyen
        is.na(TauxCarbone) & TypeEss == "Rx" ~ 0.508,
        is.na(TauxCarbone) & TypeEss == "Fs" ~ 0.488
      )) %>% # ,
    select(IdArbre, Cycle, Poids, Diam, Hauteur, TypeEss, TauxCarbone, InfraDensite, Qual1, V1, V2, V3) %>% 
    
    # pivot volumes V1, V2 et V3
    pivot_longer(
      cols = c("V1", "V2", "V3"),
      names_to = "billon",
      names_pattern = "V(.)",
      names_transform = list(billon = as.numeric),
      # names_ptypes = list(billon = numeric()),
      values_to = "volume"
    ) %>% 
    
    # -- définition des qualités des billons
    mutate(
      # qualités des billons
      qual_billon = case_when(
        billon == 1 ~ Qual1,
        # HYPOTHESE ==> qualites du billon 2 (de 3m à la decoupe gestionnaire) :
        # si A, B ou C au premier billon alors cela sera du C et si D alors D pour le reste)
        billon == 2 & Qual1 != "D" ~ "C",
        billon == 2 & Qual1 == "D" ~ "D",
        billon == 3 ~ "D"
      ),
      # qualités des connexes
      qual_connexe = "D",
      # suppression de la qualité globale de la grume (Qual1)
      Qual1 = NULL
    ) %>% 
    
    # jonction de la table des rendements de sciage
    left_join(yield_table, by = c("qual_billon" = "qual")) %>% 
    left_join(yield_table, by = c("qual_connexe" = "qual"), suffix = c("_billon", "_connexe")) %>% 
    
    mutate(
      # -- calculs des quantités de carbone
      # quantités de carbone des billons
      tCha_billon = volume * TauxCarbone * yield_billon * InfraDensite,
      # quantités de carbone des billons - connexes de scierie
      tCha_connexe = volume * TauxCarbone * (yield_connexe - yield_billon) * InfraDensite,
      # suppression des variables de rendement
      yield_billon = NULL,
      yield_connexe = NULL
      
    ) %>% 
    
    # pivot qual
    pivot_longer(
      cols = c("qual_billon", "qual_connexe", "tCha_billon", "tCha_connexe"),
      names_to = c(".value", "produit"),
      names_pattern = "(.*)_(.*)"
      # values_to = c("qual", "tCha") # pas nécessaire
    ) %>% 
    
    mutate(
      # -- calculs des durées de vie
      # hauteurs
      # Pour avoir la duree de vie d'un billon, il faut son diamètre médian.
      # Pour avoir le diamètre médian du billon 2, on utilise le volume Schaeffer (V2) et 
      # la hauteur de la table Algan (tarifs gradues Algan (1961)).
      # -> on réajuste d'abord la hauteur pour le 2ème billon
      Hauteur = case_when(
        billon == 1 & TypeEss == "Fs" ~ 3,
        billon == 1 & TypeEss == "Rx" ~ 6,
        billon == 2 & TypeEss == "Fs" ~ Hauteur - 3, 
        billon == 2 & TypeEss == "Rx" ~ Hauteur - 6,
        billon == 3 ~ 0
      ),
      
      # diamètres médians
      diam_median = case_when(
        # diam médian du billon 1 : distinction feuillus/résineux
        billon == 1 & TypeEss == "Fs" ~ Diam, # diamètre médian à 1,3 m pour 1er billon de 3 m (1,3 m ~ 1,5 m)
        billon == 1 & TypeEss == "Rx" ~ Diam - (DecroissanceRx * 1.7), # diamètre médian à 3 m pour 1er billon de 6 m (1,3 + 1,7),
        # Pour avoir le diamètre médian du billon 2, on utilise le volume V2 et 
        # la formule du cylindre (volume = (diam_med / 100) ^ 2 (pi * Hauteur * Poids) / 4
        billon == 2 ~ sqrt( volume * 4 / (pi * Hauteur * Poids) ) * 100
        # le diamètre médian pour le billon 3 ou les connexes de scierie est sans objet
      ),
      
      duree_vie = case_when(
        # NB : tous les connexes sont de qualité D -> distinction billon/connexe pas nécessaire pour A, B et C
        qual == "A" ~ 1 + (130 / (1 + (4000 * exp( - 0.16 * diam_median)))), # durée de vie pour A
        qual == "B" ~ 1 + (120 / (1 + (2500 * exp( - 0.15 * diam_median)))), # # durée de vie pour B
        qual == "C" ~ 1 + (100 / (1 + (2500 * exp( - 0.14 * diam_median)))), # durée de vie pour C
        qual == "D" & produit == "billon" & tCha != 0 ~ DureeVieD, # durée de vie pour D - grume
        qual == "D" & produit == "connexe" & tCha != 0 ~ DureeVieConnexes, # durée de vie pour D - connexes
        tCha == 0 ~ 0
      )
    ) %>% 
    select(IdArbre, Cycle, billon, produit, tCha, duree_vie)
  
  # Retour de la fonction calculs_Carbone
  return(df)
}

#### fonction_DoubleMercuriale ####
## ATTENTION : Nécessite package Forestree pour la fonction PouvoirAchat -> fonction rajoutée
DoubleMercuriale = function(
  An = NULL, TauxR = NULL, 
  PUvar = NULL, PU = NULL, PA = NULL
) {
  # initialisation
  Prix <- expand_grid(
    Annee = 1993:An,
    PU
  ) %>% 
    # pas de distinction Chêne S/P dans la base de la Forêt Privée -> on utilise "Chêne"
    filter(!Essence %in% c("Chêne P","Chêne S")) %>% 
    rename(PUafi = PU) %>% 
    select(Annee, Essence, Qual, Classe, PUafi)
  
  # table des prix afi, de la forêt privée (en euros courants et constants)
  Prix <- 
    Prix %>% 
    left_join(
      PUvar, 
      by = c("Annee" = "Année", "Essence", "Qual", "Classe")
    ) %>% 
    arrange(Annee, Essence, Qual, Classe) %>% 
    mutate(
      PUsup = lead(PU),
      PUinf = lag(PU),
      PU = ifelse(is.na(PU), (PUsup + PUinf) / 2, PU),
      PU = ifelse(is.na(PU), PUafi, PU)
    ) %>% 
    rename(PUfp = PU) %>% # PUfp = PU forêt privée
    left_join(PouvoirAchat(An, PA), by = c("Annee" = "year")) %>%
    rename(Inflation = Coefft) %>% 
    mutate(PUfpConst = PUfp * Inflation) %>%  # transformer euros courants en euros constants de l'année en cours
    select(Annee, Essence, Qual, Classe, PUafi, PUfp, PUfpConst)
  
  # table de l'année de référence (1995 - année de fixation des prix AFI)
  Ref <- 
    Prix %>% 
    filter(Annee == 1995) %>%
    rename(PU95 = PUfpConst) %>% 
    select(Essence, Qual, Classe, PU95)
  
  # calcul du taux d'actualisation # TODO : notion à confirmer
  Prix <- 
    Prix %>% 
    left_join(Ref, by = c("Essence", "Qual", "Classe")) %>% 
    mutate(ratio = PUfpConst / PU95) %>% 
    arrange(Annee, Essence, Qual, Classe) %>% 
    mutate(Ratiosup = lead(ratio))
  
  # -- return of 'DoubleMercuriale' function
  return(Prix)
}

##### fonction calculs dendromicrohabitats (DMH) #####
calculs_DMH <- function(df = NULL) { # TODO : reprendre les éléments de simplification dans le PSDRF
  # df <- Codes # debug
  # ---- Liste des niveaux
  if (nrow(df) > 0) {
    Niveaux <- c(
      "g1", "g2", "g3", "h1", "h2", "h3", "f1", "f2", "f3", 
      "a1", "a2", "a3", "p1", "p2", "p3", "i1", "i2", "i3", "c1", 
      "c2", "c3", "e1", "e2", "e3", "b1", "b2", "b3", 
      "l1", "l2", "l3", "r1", "r2", "r3", "k", "t", "ts", "tc", "tn", "tx", 
      "d", "u", "m1", "m2", "n", "j", 
      "s1", "s01", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s25",  #petites branches mortes
      "x1", "x01", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10",  #moyennes branches mortes
      "y1", "y01", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9", "y10", 
      "z"
    )
    Niveaux <- c(Niveaux, toupper(Niveaux))
    # ---- Decomposition
    NbCodes <-length(Niveaux)
    df$CodeEcolo <- str_replace(df$CodeEcolo, "0", "")
    List <- c()
    for (i in 1:nrow(df)) {
      List <- c(
        List, 
        list(
          str_extract(df$CodeEcolo[i], Niveaux)[
            !is.na(str_extract(df$CodeEcolo[i], Niveaux))
            ]
        )
      )
    }
    df2 <- tibble(
      NumDisp = rep.int(df$NumDisp, sapply(List, length)), 
      NumPlac = rep.int(df$NumPlac, sapply(List, length)), 
      NumArbre = rep.int(df$NumArbre, sapply(List, length)), 
      Cycle = rep.int(df$Cycle, sapply(List, length)), 
      CodeEcolo = unlist(List)
    ) %>% 
      left_join(
        df[, !colnames(df) %in% "CodeEcolo"],
        by = c("NumDisp", "NumPlac", "NumArbre", "Cycle")
      ) %>% 
      as.data.frame()
    
  } else {
    df2 <- tibble(
      NumDisp = as.numeric(), 
      NumPlac = as.numeric(), 
      NumArbre = as.numeric(), 
      Cycle = as.numeric(), 
      CodeEcolo = as.character(), 
      
      Essence = as.character(), 
      EssReg = as.character(), 
      Azimut = as.numeric(), 
      Distance = as.numeric(), 
      Diam1 = as.numeric(), 
      Diam2 = as.numeric(), 
      Diam = as.numeric(), 
      Classe = as.numeric(), 
      Cat = as.character(), 
      Qual1 = as.character(), 
      Qual2 = as.character(), 
      Qual = as.character(), 
      HautT = as.numeric(), 
      Red = as.numeric(), 
      Green = as.numeric(), 
      Blue = as.numeric(), 
      Dhoup = as.numeric(), 
      Choup = as.numeric(), 
      Nha = as.numeric(), 
      Gha = as.numeric(), 
      Vha = as.numeric(), 
      VcHa = as.numeric(), 
      VpHa = as.numeric(), 
      AcctD = as.numeric(), 
      AcctV = as.numeric(), 
      AcctVper = as.numeric(), 
      AcctG = as.numeric(), 
      AcctGper = as.numeric(), 
      TauxV = as.numeric(), 
      TauxPU = as.numeric(), 
      Taux = as.numeric(), 
      Gain = as.numeric(), 
      Gainper = as.numeric(), 
      TotalCarbha = as.numeric(), 
      Couvert = as.numeric(), 
      Coupe = as.character(), 
      Lim = as.character()
    ) %>% 
      as.data.frame()
  }
  
  # Retour de la fonction calculs_DMH
  return(df2)
}


##### fonction calculs des perches #####
calculs_Perches <- function(df = NULL) {
  df <-
    df %>%
    mutate(
      # UTILITE A VERIFIER
      # Qual = ifelse(Qual == "", "C", Qual),
      # Qual = ifelse(is.na(Qual), "C", Qual),
      # Qual1 = ifelse(Qual1 == "", "C", Qual1),
      # Qual1 = ifelse(is.na(Qual1), "C", Qual1),
      # Qual2 = ifelse(Qual2 == "", "C+D", Qual2),
      # Qual2 = ifelse(is.na(Qual2), "C+D", Qual2),
      # # ----- Poids/densité de tiges
      # Nha = 10000 / (pi * 10 ^ 2),
      # # -- gestion des arbres limites (cercle de 10 m)
      # Limite = ifelse(Distance <= 10, NA, Limite), # Condition devrait être avec Diam1
      # # -- suppression des arbres limites
      # Nha = ifelse(is.na(Limite), Nha, 0),
      # # ----- surface terrière
      # Gha = pi * Diam ^ 2 / 40000 * Nha,
      # ----- volume
      Vha = Gha * 10,
      # ----- variables en supplément (agrégations)
      VcHa = NA, VpHa = NA,
      AcctD = NA,
      AcctV = NA, AcctVper = NA,
      AcctG = NA, AcctGper = NA,
      TauxV = NA, TauxPU = NA, Taux = NA,
      Gain = NA, Gainper = NA, Coupe = Coupe, Lim = NA
    ) %>%
    arrange(NumDisp, Cycle, NumPlac) %>%
    select(
      NumDisp, Cycle, NumPlac, NumArbre, Essence, 
      EssReg, Coupe,
      Qual1, Qual2, Diam, Classe,
      Nha, Gha, Vha, VcHa, VpHa,
      AcctD,  AcctV,  AcctVper,  AcctG,  AcctGper,
      TauxV,  TauxPU, Taux,
      Gain, Gainper, 
      
      Coupe, Limite, Observations
    )
  
  # retour de la fonction calculs_Perches
  return(df)
}

##### fonction calculs du bois mort sur pied #####
calculs_BMP <- function(df = NULL) {
  df <-  
    df %>% 
    mutate(
      Nha = ifelse( # Condition devrait être avec Diam1
        Diam1 < 30, 
        10000 / pi / 10 ^ 2, 
        10000 / pi / 20 ^ 2
      ),
      # -- gestion des arbres limites
      # cercle de 10 m
      Limite = ifelse(Diam1 < 30 & Distance <= 10, NA, Limite), # Condition devrait être avec Diam1
      # cercle de 20 m
      Limite = ifelse(Diam1 >= 30 & Distance <= 20, NA, Limite),
      # suppression des arbres limites
      Nha = ifelse(is.na(Limite), Nha, 0),
      
      Gha = pi * Diam ^ 2 / 40000 * Nha, 
      Vha = ifelse(
        is.na(HautT),
        8 * Gha, 
        pi * (Diam - (HautT / 2 - 1.3)) ^ 2 / 40000 * HautT * Nha
      ),  # Décroissance métrique fixée à 1
      StadeE = floor(Stade / 10), 
      StadeD = Stade - StadeE * 10, 
      Type = ifelse(Diam1 < 30, "Pied inf30", "Pied sup30")
    ) %>%
    # rename(Nha = Poids) %>%
    select(
      NumDisp, Cycle, NumPlac, NumArbre, Essence, EssReg, 
      CodeEcolo, StadeD, StadeE, Diam, Classe, Cat, Nha, Gha, Vha, Type,
      Observations
    )
  
  # Retour de la fonction calculs_BMP
  return(df)
}

# #### fonction de calcul du carbone dans le bois mort sur pied ####
# calculs_BMP_carbone <- function(
#   df = NULL,
#   species_table = NULL,
#   dead_wood_carbon_content = NULL, 
#   dead_wood_density = NULL, 
#   decomposition_stage_code = NULL 
# ) { 
#   # certaines essences non comptées car pas de données 
#   standing_dead_wood_carbon_content <- 
#     dead_wood_carbon_content %>% 
#     filter(Type == "Bois mort sur pied") %>% 
#     dplyr::select(-Type)
#   standing_dead_wood_density <- 
#     dead_wood_density %>% 
#     filter(Type == "Bois mort sur pied") %>% 
#     dplyr::select(-Type)
#   
#   df <- 
#     df %>% 
#     mutate(
#       Stade_AFI = paste(StadeE, StadeD, sep = "."),
#       Inter = paste(".", StadeD, sep = "")
#     ) %>% 
#     left_join(
#       decomposition_stage_code, 
#       by = c("Inter" = "Stade_AFI")
#     ) %>% 
#     mutate(
#       Code = case_when(
#         Stade_AFI == "1.1" ~ 1,
#         Stade_AFI == "2.1" ~ 1,
#         Stade_AFI == "4.4" ~ 5,
#         Stade_AFI == "4.5" ~ 5,
#         TRUE ~ Code
#       )
#     ) %>% 
#     # taux carbone Fs/Rx
#     left_join(
#       species_table[,c("Essence","TypeEss")], 
#       by = "Essence") %>% 
#     # taux de carbone dans feuillus/resineux en fonction stade
#     left_join(
#       standing_dead_wood_carbon_content,
#       by = c("TypeEss","Code")
#     ) %>% 
#     left_join(
#       standing_dead_wood_density, 
#       by = c("Essence","Code")
#     ) %>% 
#     mutate(
#       tCha = Vha * Infradensite * (Taux_carbone) / 100
#     ) %>% 
#     select(
#       -Stade_AFI, -Inter, -Code, -TypeEss, 
#       -Taux_carbone, -SRF, -Infradensite
#     )
#   
#   # -- return of calculs_BMP_carbone function
#   return(df)
# }

##### fonction de calcul du bois mort au sol linéaire #####
calculs_BMSLineaires <- function(df = NULL) {
  df <- 
    df %>% 
    filter(!is.na(Essence)) %>% 
    mutate(
      Classe = floor(Diam / 5 + 0.5) * 5,
      Cat = cut(
        Diam, 
        breaks = c(0, 17.5, 27.5, 47.5, 67.5, 200),
        labels = c("PER", "PB", "BM", "GB","TGB"),
        include.lowest = T,
        right = F
      ),
      Cat = as.character(Cat),
      Vha = pi ^ 2 / 8 / 60 * Diam ^ 2 / cos(Angle / 180 * pi),
      StadeE = floor(Stade / 10),
      StadeD = Stade - StadeE * 10
    ) %>%
    select(
      NumDisp, Cycle, NumPlac, Transect, Essence, EssReg,
      Diam, Classe, Cat, Stade, StadeD, StadeE, Vha,
      Observations
    )
  
  # Retour de la fonction calculs_BMSLineaires
  return(df)
}

##### fonction calculs du bois mort au sol par surface fixe #####
calculs_BMSsup30 <- function(df = NULL) {
  df <- 
    df %>% 
    mutate(
      StadeE = floor(Stade / 10),
      StadeD = Stade - StadeE * 10,
      DiamIni = ifelse(is.na(DiamIni), 0, DiamIni),
      DiamMed = ifelse(is.na(DiamMed), 0, DiamMed),
      DiamFin = ifelse(is.na(DiamFin), 0, DiamFin),
      Vha = 0,
      Classe = 0,
      # ---- formule de Huber
      Vha = ifelse(
        (DiamIni+ DiamFin) == 0,
        pi / 40000 * DiamMed ^ 2 * Longueur * 10000 / pi / 20 ^ 2,
        Vha
      ),
      Classe = ifelse(
        (DiamIni + DiamFin) == 0,
        floor(DiamMed / 5 + 0.5) * 5,
        Classe
      ),
      # ---- formule de Smalian
      Vha = ifelse(
        (DiamIni + DiamFin) != 0 & DiamMed == 0,
        pi / 80000 * (DiamIni ^ 2 + DiamFin ^ 2) * 
          Longueur * 10000 / pi / 20 ^ 2,
        Vha
      ),
      Classe = ifelse(
        (DiamIni + DiamFin) != 0 & DiamMed == 0,
        floor((DiamIni + DiamFin) / 2 / 5 + 0.5) * 5,
        Classe
      ),
      # ---- formule de Newton
      Vha = ifelse(
        (DiamIni + DiamFin) != 0 & DiamMed != 0,
        pi / 240000 * (DiamIni ^ 2 + DiamFin ^ 2 + 4 * DiamMed ^ 2) * 
          Longueur * 10000 / (pi * 20 ^ 2),
        Vha
      ),
      Classe = ifelse(
        (DiamIni + DiamFin) != 0 & DiamMed != 0,
        floor((DiamIni + DiamFin + DiamMed) / 3 / 5 + 0.5) * 5,
        Classe
      ),
      
      # sécurité si table vide (Classe doit être numeric pour cut)
      Classe = as.numeric(Classe)
    ) %>%
    mutate(
      Cat = cut(
        Classe, 
        breaks = c(0, 17.5, 27.5, 47.5, 67.5, 500),
        labels = c("PER", "PB", "BM", "GB", "TGB"),
        include.lowest = T, 
        right = F
      ),
      Cat = as.character(Cat)
    ) %>%
    select(
      NumDisp, NumPlac, Cycle, Essence, EssReg, # Contact, Chablis,
      Classe, Cat, Stade, StadeD, StadeE, Vha,
      Observations
    )
  
  # Retour de la fonction calculs_BMSsup30
  return(df)
}

#### fonction calculs du carbone dans le bois mort ####
calculs_BM_carbone = function(
  df = NULL, 
  living_trees_table = NULL,
  species_table = NULL,
  type = NULL, # "Bois mort au sol" ou "Bois mort sur pied"
  dead_wood_carbon_content = NULL, 
  dead_wood_density = NULL, 
  decomposition_stage_code = NULL 
){
  # certaines essences bois mort non comptées car pas de données (ex Pin L)
  # associer les Ind. à l'essence majoritaire de la placette 
  
  # -- set up
  # main species by plot
  main_species_by_plot_and_cycle <- 
    living_trees_table %>% 
    group_by(NumDisp, Cycle, NumPlac, Essence) %>% 
    summarise(Vha = sum(Vha)) %>% 
    filter(Vha == max(Vha)) %>% 
    rename(main_species = Essence) %>% 
    select(-Vha)
  
  # filter dead_wood_carbon_content & dead_wood_density
  dead_wood_carbon_content_filtered <- 
    dead_wood_carbon_content %>% 
    filter(Type == type) %>% 
    select(-Type)
  dead_wood_density_filtered <- 
    dead_wood_density %>% 
    filter(Type == type) %>% 
    select(-Type)
  
  # -- processing
  df <- 
    df %>% 
    mutate(
      Stade_AFI = paste(StadeE, StadeD, sep = "."),
      Inter = paste(".", StadeD, sep ="")
    ) %>% 
    left_join(
      decomposition_stage_code, 
      by = c("Inter" = "Stade_AFI")
    ) %>% 
    mutate(
      Code = case_when(
        Stade_AFI == "1.1" ~ 1,
        Stade_AFI == "2.1" ~ 1,
        Stade_AFI == "4.4" ~ 5,
        Stade_AFI == "4.5" ~ 5,
        TRUE ~ Code
      )
    ) %>% 
    # essence majoritaire du dispositif pour bois mort indéterminé
    left_join(
      main_species_by_plot_and_cycle, 
      by = c("NumDisp", "Cycle", "NumPlac")
    ) %>% 
    mutate(
      species_carb = ifelse(
        Essence %in% c(
          "Ind.", "Résineux Ind.", "Feuillus Ind.", "Feuillus", "Résineux"
        ), 
        main_species, 
        Essence
      ),
      # sécurité si table vide (Essence doit être character pour jonction)
      species_carb = as.character(species_carb)
    ) %>% 
    left_join(
      species_table[, c("Essence", "TypeEss")], 
      by = c("species_carb" = "Essence")
    ) %>% 
    # taux de carbone dans feuillus/resineux en fonction stade
    left_join(
      dead_wood_carbon_content_filtered, 
      by = c("TypeEss", "Code")
    ) %>% 
    left_join(
      dead_wood_density_filtered, 
      by = c("species_carb" = "Essence", "Code")
    ) %>% 
    # carbone en tC/ha
    mutate(
      tCha = Vha * Infradensite * SRF * (Taux_carbone) / 100
    ) %>% 
    select(
      -Stade_AFI, -Inter, -Code, -main_species, 
      -TypeEss, -Taux_carbone, -SRF, -Infradensite, -species_carb
    )
  
  # -- return of 'calculs_BM_carbone' function
  return(df)
}

##### fonction calculs de la régénération #####
calculs_Reges <- function(df = NULL) {
  df <- 
    df %>% 
    filter(!is.na(Essence)) %>% # rqe : il peut y avoir des essences vides si on a fait une Ss-placette et juste noté 1 observation
    # select(-one_of("Rejet","Observations")) %>%
    arrange(NumDisp, Cycle, NumPlac, SsPlac) %>%
    # replace(is.na(.),0) %>%
    mutate(
      Recouv = ifelse(is.na(Recouv), 0, Recouv),
      Class1 = ifelse(is.na(Class1), 0, Class1),
      Class2 = ifelse(is.na(Class2), 0, Class2),
      Class3 = ifelse(is.na(Class3), 0, Class3),
      
      Surf = ifelse(Class1 + Class2 + Class3 >= 5, 1, 0),
      Recouv = Recouv / 3,
      Classe1Ha = Class1 * 10000 / (pi * 1.5 ^ 2) / 3,
      Classe2Ha = Class2 * 10000 / (pi * 1.5 ^ 2) / 3,
      Classe3Ha = Class3 * 10000 / (pi * 1.5 ^ 2) / 3
    )
  
  # Retour de la fonction calculs_Reges
  return(df)
}

##### fonction calculs du taillis #####
calculs_Taillis <- function(df = NULL) {
  if (nrow(df) > 0) {
    df <- 
      df %>% 
      filter(!is.na(Essence)) %>% # rqe : il peut y avoir des essences vides si on a fait une placette et juste noté 1 observation
      mutate(
        Classe = floor(Diam / 5 + 0.5) * 5, 
        Cat = cut(
          Diam, 
          breaks = c(0, 17.5, 27.5, 47.5, 67.5, 200), 
          labels = c("PER", "PB", "BM", "GB", "TGB"), 
          include.lowest = T, 
          right = F
        ), 
        Nha = Nbre * 10000 / (pi * 10 ^ 2), 
        Gha = round(pi * Diam ^ 2 / 40000 * Nbre * 10000 / (pi * 10 ^ 2), 10), 
        Vha = Gha * 7
      ) %>% 
      select(
        NumDisp, NumPlac, Cycle, 
        Essence, EssReg, 
        Diam, Classe, Cat, 
        Nha, Gha, Vha,
        Observations
      )
    
  } else {
    df <- 
      tibble(
        NumDisp = numeric(), NumPlac = numeric(), Cycle = numeric(), 
        Essence = character(), EssReg = character(), 
        Diam = numeric(), Classe = numeric(), Cat = character(), 
        Nha = numeric(), Gha = numeric(), Vha = numeric()
      )
  }
  
  # Retour de la fonction calculs_Taillis
  return(df)
}



##### fonction afi_Calculs #####
#' Calcul par arbre des variables dendrométriques, économiques et écologiques.
#' @description Cette fonction permet de calculer les différentes variables d'analyse à l'échelle de chaque arbre. C'est la première étape de calcul.
#'
#' @return Génère l'archive AFITablesBrutes.Rdata dans le dossier 'Tables'.
#'
#' @param repAFI = répertoire contenant les données
#' @param lang = langue (sélectionnée) de l'interface
#' @param repSav répertoire de sauvegarde des données (pour édition du livret AFI via interface opérateur)
#' @param disp choix du dispositif (pour édition du livret AFI via interface opérateur "N°-nom du dispositif")
#' @param TauxR = taux d'actualisation (0.04 par défaut)
#'
#' @author Valentin Demets, Bruciamacchie Max
#' @export


afi_Calculs <- function(
  wd = NULL,
  output_dir = NULL,
  # repSav = repAFI, 
  # lang = "FRA", 
  TauxR = 0.04, 
  disp = NULL,
  last_cycle = NULL,
  complete_progress = NULL,
  i18n = NULL
) {
  print(wd)
      ##### 1/ Initialisation #####
      # -- création du dossier de sortie
      output_dir <- file.path(output_dir, "tables")
      # output_dir <- file.path("out", disp, i18n()$t("livret_AFI"))
      dir.create(output_dir, showWarnings = F, recursive = T)
      
      # -- chargement des données d'inventaire et administratives
      # define Rdata to load
      inventory_data <- file.path(wd, "tables/afiDonneesBrutes.Rdata")
      admin_data <- file.path(wd, "tables/afiCodes.Rdata")
      buying_power_data <- file.path(wd, "tables/PA.rda")
      
      # incrémentation de la barre de progression
      detail = i18n()$t("Chargement des données d'inventaire")
      
      # -- switch shiny ***
      incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
      # *** --
      print(detail)
      # print("repere1")
      
      # loading
      inventory_tables <- load(inventory_data)
      # print("repere2")
      inventory_tables <- c(
        "IdArbres", "BMortSup30","BMortLineaires", "Cycles", #"Placettes",
        "Reges", "Coords", "Taillis"
      )
      load(admin_data)
      # print("repere3")
      # data "pouvoir d'achat" from forestree package
      load(buying_power_data)
      
      # # TODO SHINY : sélection des dispositifs et de output_dir dooit se faire en amont
      # # tables list to get last_cycle and disp_list
      # df_list <- load(inventory_data)
      # 
      # # TODO SHINY : sélection des dispositifs et de output_dir dooit se faire en amont
      # # -- sélection des données propres au(x) dispositif(s) choisi(s) :
      # # if (repSav == repAFI) {
      # #   # -- choix du dispositif
      # #   # initialisation
      # #   check_all_msg <- ifelse(
      # #     lang == "FRA",
      # #     "Editer les r\u00E9sultats pour tous les dispositifs",
      # #     "Edit results for all stands"
      # #   )
      #   disp_list <- choose_disp(df_list, Dispositifs, check_all_msg)
      #   # ----
      #   # 2020 - MLM
      # # disp_list <- c(
      # #   "1-Bois des Brosses", "2-Bois du Chanois", "3-Forêt de Chamberceau",
      # #   "4-Bois des Etangs d'Aige et du Prince", "5-Forêt de Gergy",
      # #   "6-Forêt de la Quiquengrogne", "7-Bois de Censey",
      # #   "9-Bois de la Rente du Fretoy", "10-Bois Banal", "11-Bois de Cosges",
      # #   "13-Bois de la Pérouse", "14-Bois des Feuillées", "15-Bois du Château",
      # #   "16-Forêt de Folin", "17-Bois Royal de Belval",
      # #   "18-Forêt d'Epernay", "19-Forêt de la Grange Perrey",
      # #   "20-Forêt de Perrecy les Forges",
      # #   "21-Les Grands Bois", "23-Bois du Grand Lomont", "24-Forêt du Grand Vernet",
      # #   "25-Forêt de la Brisée", "26-Les Grands Bois", "27-Bois du Luth",
      # #   "30-Forêt du Hailly",
      # #   "32-Bois du Pré Jeanreau", "33-Forêt d'Is/Tille",
      # #   "34-Forêt de Robert-Magny", "36-Bois de Brice", "37-Bois de Frilouze",
      # #   "38-Forêt de Rai", "40-Bois de la Barre", "41-Forêt de la Rivière",
      # #   "42-Forêt de Montalibord", "43-Forêt d'Ombrée", "44-Forêt du Régnaval",
      # #   "45-Bois de Belle Assise", "46-Forêt de la Chevreté", "47-Bois de Jebsheim",
      # #   "48-Forêt de Landsberg", "49-Forêt de la Métairie rouge",
      # #   "50-Forêt de Montesault", "51-Bois des Soriots", "52-Bois de la Cayère",
      # #   "53-Forêt de la Chaine",
      # #   "54-Forêt de Marchenoir", "55-Forêt de Montmirail",
      # #   "56-Bois de Chanteloube",
      # #   "57-La Touche aux Loups",
      # #   "58-Forêt Domaniale de Saint Gobain",
      # #   "59-Forêt de la Queue de Boué",
      # #   "61-La Forêt", "62-Bois du Faussé", "63-Bois de la Forêt",
      # #   "64-Forêt de la Marsaudière", "65-Bois de Paris", "66-Bois du Beau Mousseau",
      # #   "68-Forêt Domaniale de Wiltz-Merkholtz", "69-Forêt Communale de Wiltz",
      # #   "70-Forêt Communale de Bettborn", "71-Forêt de Metendal",
      # #   "72-Forêt de la Montroche", "73-Curraghchasse Forest Park",
      # #   "74-Forêt Domaniale du Grand Bois", "75-Forêt Communale de Rouvroy",
      # #   "76-Forêt de la SOMICAL (1)", "78-Forêt Communale d'Igney",
      # #   "79-Forêt Domaniale de Muterhouse",
      # #   "80-Le Hohenfels",
      # #   "81-Forêt de la Sémoline",
      # #   "82-Forêt de Fontréal", "83-Forêt de Saint Lager", "84-Forêt d'Algères",
      # #   "85-Forêt de la SOMICAL (2)", "86-Forêt de Rivedieu", "87-Domaine de Rochemure",
      # #   "88-Bois de la Côte", "89-Forêt Domaniale des Chambons",
      # #   "90-Bois du Château de Dufau", "91-Forêt de Londeix", "92-Forêt de la SOMICAL (3)",
      # #   "93-Bois de Barnal", "95-Bois des Mauves", "96-Bois de la Vancre",
      # #   "97-Forêt Domaniale d'Andelfingen",
      # #   "98-La Clavière",
      # #   "99-Forêt Domaniale de Jussy", "100-Forêt d'Authumes",
      # #   "101-Forêt Communale d'Hallau",
      # #   # -- exclusion des dispositifs anglais
      # #   # "102-Forest of Monivea",
      # #   # "103-Forest of Mellory", "104-Forest of Killsheelan",
      # #   # "105-Forest of Lisdowney", "106-Forest of Knockrath", "107-Forest of Rahin",
      # #   "109-Forêt de Cardine", "110-Forêt Communale de Lacoste",
      # #   "111-Forêt Domaniale de Versoix", "112-Les Cravives",
      # #   "113-Bois du Crêt Lescuyer", "114-Forêt Indivise de Rabat les Trois Seigneurs",
      # #   "115-Forêt Communale de Boussenac", "116-Forêt communale de Rimont",
      # #   "117-Bois du Bousquet", "118-Forêt Communale de Grenchen",
      # #   "119-Forêt de la Fabrie",
      # #   # "120-Chasse Woods - Rushmore Estate",
      # #   # "121-Ballykilkavan",
      # #   "122-Bois de l'Ardère",
      # #   # "123-Berth Ddu",
      # #   "124-Forêt Communale de Niozelles", "125-La Tuilière", "126-Plan de Liman",
      # #   "127-Forêt de Mollberg", "128-Forêt Communale de Lalaye", "129-Forêt du Nivot",
      # #   # "130-Forêt de Notre Dame des Neiges",
      # #   # "132-Forêt du Prieuré d'Ardène",
      # #   "133-Bois de Luthenay", "135-Forêt Communale de Zurich"
      # # )
      #   
      #   
      #   # AG AFI 2021
      #   disp_list <- c(
      #     "7-Bois de Censey",
      #     "32-Bois du Pré Jeanreau",
      #     "33-Forêt d'Is sur Tille",
      #     "62-Bois du Faussé",
      #     "72-Forêt de la Montroche",
      #     "78-Forêt Communale d'Igney",
      #     "79-Forêt Domaniale de Mouterhouse",
      #     "122-Bois de l'Ardère",
      #     "137-Forêt des Puechs",
      #     "138-Forêt du Lévezou",
      #     
      #     "9-Bois de la Rente du Fretoy",
      #     "10-Bois Banal",
      #     "11-Bois de Cosges",
      #     "14-Bois des Feuillées",
      #     "15-Bois du Château",
      #     "44-Forêt du Régnaval",
      #     "45-Bois de Belle Assise",
      #     "48-Forêt de Landsberg",
      #     "55-Forêt de Montmirail",
      #     "58-Forêt Domaniale de Saint Gobain",
      #     "59-Forêt de la Queue de Boué",
      #     "61-La Forêt",
      #     "80-Le Hohenfels",
      #     "87-Domaine de Rochemure",
      #     "88-Bois de la Côte",
      #     "89-Forêt Domaniale des Chambons",
      #     "93-Bois de Barnal",
      #     "96-Bois de la Vancre",
      #     "114-Forêt Indivise de Rabat les Trois Seigneurs",
      #     "115-Forêt Communale de Boussenac",
      #     "116-Forêt communale de Rimont",
      #     "117-Bois du Bousquet",
      #     "144-Les_Saint_Peyres",
      #     "145-Cousses_et_Brian",
      #     "146-FD CHorin",
      #     "147-FD Lübben",
      #     "148-Bois de la Fayolle",
      #     "149-Bois de Cressu",
      #     "150-Saint Yvoce",
      #     "151-Forêt de Champlalot"
      #   )
      # # ----
      #   # num_list <- as.numeric( str_sub(disp_list, 1, str_locate(disp_list, "-")[, 1] - 1) )
      # } else {
      
      # 2022 - calculs sur l'ensemble de la base AFI
      # disp_num <- unique(IdArbres$NumDisp)
      # disp_name <- with(Dispositifs, Nom[match(disp_num, NumDisp)])
      # disp_list <- paste0(disp_num, "-", clean_names(disp_name))
      
      
      disp_list <- disp # traitement courant
      # }

      # get last_cycle from df_list
      # last_cycle <- get_last_cycle(df_list, disp_list)

      # TODO SHINY : sélection des dispositifs et de output_dir dooit se faire en amont
      # -- filtre des tables d'inventaire en fonction des numéros de dispositif sélectionnés
      # inventory_tables <- c(
      #   "IdArbres", "BMortSup30","BMortLineaires", "Cycles", #"Placettes",
      #   "Reges", "Coords", "Taillis"
      # )
      filter_by_disp(tables = inventory_tables, disp_list = disp_list, cycle = last_cycle) # traitement courant
      ##### / \ #####
      
      ##### 0/ Table NbPlac ##### # TODO : reprendre les éléments de simplification dans le PSDRF + numérotation
      # table NbPlac
      NbPlac <- Cycles %>% select(NumDisp, Cycle, NbPlacettes)
      
      # table 'sampling_param_by_plot'
      # (gestion des cas où Coefft et DiamLim renseignés dans la feuille Coords car variables entre placettes)
      sampling_param_by_plot <- Coords %>% select(NumDisp, NumPlac, Coefft, DiamLim)
      filled_param1 <- which(!is.na(sampling_param_by_plot$Coefft))
      filled_param2 <- which(!is.na(sampling_param_by_plot$DiamLim))
      
      # security
      if (length(setdiff(filled_param1, filled_param2)) > 0) {
        stop("Incohérences détectées entre les colonnes DiamLim et Coefft de la feuille Coord du dispositif (les 2 valeurs doivent être renseignées)")
      }
      sampling_param_by_plot <- sampling_param_by_plot[filled_param1, ]
      ##### / \ #####
      
      ##### 1/ Préparation des tables de l'onglet Arbres #####
      Arbres <- 
        IdArbres %>% 
        left_join(ValArbres, by = "IdArbre")  %>% 
        left_join(Essences, by = "Essence") %>% 
        # filter(NumPlac == 5 & NumArbre == 20) %>% 
        check_dmh_for_standing_deadwood() %>% 
        prep_df()
      ##### / \ #####
      
      ##### 2/ Définition des populations de perches, précomptables et de bois mort sur pied #####
      # # Perches
      # Perches <- 
      #   Arbres %>% 
      #   filter(
      #     Diam >= 7.5 & # Diam et pas Diam1 car risque d'avoir apparaître des classes 5 (ex : Diam1 = 7,5 et Diam2 = 6)
      #       Diam1 < 17.5 & is.na(Type)
      #   )
      
      # -- distinction bois vivant / bois mort
      # BMP
      BMP <- Arbres %>% filter(!is.na(Type))
      # Précomptables
      # Arbres <- Arbres %>% filter(Diam1 >= 17.5 & is.na(Type))
      Arbres <- Arbres %>% filter(is.na(Type))
      ##### / \ #####
      
      ##### 3/ Calculs sur les précomptables #####
      ##### --- 3.1/ Calculs de Nha, Gha, Vha, VcHa, ... #####
      
      # incrémentation de la barre de progression
      detail = i18n()$t("Traitement des donn\u00E9es de pr\u00E9comptables...")
      # -- switch shiny ***
      incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
      # *** --
      print(detail)
      
      # --
      Arbres <- 
        # ----- Jonctions et préparation
        Arbres %>% 
        left_join(
          Cycles[, c("NumDisp", "Cycle", "DiamLim", "Coefft")], 
          by = c("NumDisp", "Cycle")
        ) %>% # Prec
        
        # gestion des paramètres d'échantillonnage renseignés dans la feuille Coords
        left_join(sampling_param_by_plot, by = c("NumDisp", "NumPlac"), suffix = c("", "_bis")) %>% 
        mutate(
          Coefft = ifelse(!is.na(Coefft_bis), Coefft_bis, Coefft),
          DiamLim = ifelse(!is.na(DiamLim_bis), DiamLim_bis, DiamLim),
          Coefft_bis = NULL,
          DiamLim_bis = NULL
          ) %>% 
        
        left_join(Qual, by = c("Qual" = "Nom")) %>% 
        left_join(Tarifs, by = c("NumDisp", "Essence")) %>% 
        left_join(PU, by = c("Essence", "Classe", "Qual1" = "Qual")) %>% 
        mutate(PU = ifelse(Classe < 7.5 & !is.na(Classe), 0, PU)) %>%
        filter(!is.na(Essence) & !is.na(Azimut) & !is.na(Distance)) %>%  # Suppression des arbres sans leurs coordonnées (voir calculs d'accroissement)
        
        mutate(
          # -- arbres limites
          Limite = case_when(
            # cercle
            Diam1 < DiamLim & Distance > 10 ~ 1,
            # angle fixe
            Diam1 >= DiamLim & Diam1 < Distance * Coefft ~ 1
          ),
          
          # -- poids/densité de tiges des tiges sur le cercle
          Nha = case_when(
            # suppression des arbres limites
            Limite == 1 ~ 0,
            # cercle
            Diam1 < DiamLim ~ 10000 / (pi * 10 ^ 2),
            # angle fixe
            Diam1 >= DiamLim ~ 10 ^ 4 * Coefft ^ 2 / (pi * Diam1 ^ 2)
          ),
          
          # -- surface terrière
          Gha = pi * Diam1 ^ 2 / 40000 * Nha
        )
      
      # distinction des perches (plus vraiment les perches mais la population inventoriée sur le cercle de 10 m et < 17,5m - en fait cela devrait être la population inventoriée sur le cercle et 17,5 devait être paramétrable)
      Perches <- Arbres %>% filter(Diam >= 7.5 & Diam < 17.5)
      Arbres <- Arbres %>% filter(Diam >= 17.5)
      
      Arbres <- 
        Arbres %>% 
        # ----- volume
        calculs_Vol() %>% 
        mutate(
          # ----- valeur de consommation
          VcHa = Vha * PU,
          # ----- taux d'accroissement en volume
          # -- volume de la classe supérieure
          DiamSup = Diam + 5,
          ClasseSup = Classe + 5
        ) %>%
        calculs_Vol(Sup = T) %>%
        mutate(
          TauxV = ifelse(Vha > 0, log(VhaSup / Vha) / 5, 0)
        ) %>%
        # ----- A DECALER : Calcul prix de la classe supérieure (préparation valeur potentielle)
        left_join(
          PU, # PrixSup
          by = c("Essence", "ClasseSup" = "Classe", "Qual1" = "Qual"),
          suffix = c("", "Sup")
        ) %>% 
        # ----- Autres variables
        mutate(
          Dhoup = (Dh1 * Dh2) ^ 0.5,
          Choup = (Dhoup / Diam) ^ 2 * 10000,
          Couvert = ifelse(is.na(Limite), (Dhoup / Diam) ^ 2 * 10000, 0)
        )
      
      ##### --- 3.2/ Calculs d'accroissement #####
      Arbres <- 
        Arbres %>% 
        calculs_Acct(cycles = Cycles, last_cycle = last_cycle, tauxR = TauxR) %>% 
        mutate(
          TauxPU = log(PUSup / PU) / 5,
          Taux = (TauxPU + TauxV) * AcctD,
          AcctV = TauxV * Vha * AcctD,
          Gain = Taux * VcHa,
          VpHa = Gain / TauxR,
          AcctG = pi / 20000 * AcctD * Diam * Nha
        )
      
      ##### --- 3.3/ Calcul des quantités de carbone #####
      # -- paramètres de calcul
      DecroissanceRx <- 1
      # RendementABC <- 0.6
      # RendementD <- 0.99
      DureeVieD <- 3
      DureeVieConnexes <- 1
      
      # -- calcul
      # table des rendements
      yield_table <- tibble(
        qual = c("A", "B", "C", "D"),
        yield = c(0.6, 0.6, 0.6, 0.99)
      )
      # table principale
      living_trees_carbon_splitted_by_log <- 
        Arbres %>% 
        # calcul des tC/ha et durées de vie des billons des arbres sur pied
        calculs_Carbone(
          height_by_class_and_NumTarif = Algan, 
          DecroissanceRx = DecroissanceRx, 
          yield_table = yield_table,
          DureeVieD = DureeVieD, 
          DureeVieConnexes = DureeVieConnexes
        ) 
      # %>% 
      #   dplyr::select(
      #     NumDisp, NumPlac, NumArbre, Cycle, 
      #     Essence, EssReg, Azimut, Distance, 
      #     Diam1, Diam2, Diam, Classe, Cat, 
      #     Qual1, Qual2, Qual, HautT, CodeEcolo, 
      #     Red, Green, Blue, Dhoup, Choup, 
      #     Nha, Gha, Vha, VcHa, VpHa, 
      #     AcctD, AcctV, AcctVper, AcctG, AcctGper, 
      #     TauxV, TauxPU, Taux, 
      #     Gain, Gainper, Couvert,
      #     Carb1, Carb11, Carb2, Carb21, Carb3, TotalCarbHa,  # Carb en tC/ha
      #     DureeVie1, DureeVie11, DureeVie2, DureeVie21, DureeVie3,
      #     Coupe, Limite, Observations
      #   )
      
      # carbone total par arbre
      living_trees_carbon <- 
        living_trees_carbon_splitted_by_log %>% 
        group_by(IdArbre, Cycle) %>% 
        summarise(tCha = sum(tCha, na.rm =T)) %>% 
        ungroup()
      
      Arbres <- 
        Arbres %>%
        # jonction de la table living_trees_carbon
        left_join(
          living_trees_carbon,
          by = c("IdArbre", "Cycle")
        ) %>% 
        select(
          IdArbre, NumDisp, NumPlac, NumArbre, Cycle, 
          Essence, EssReg, Azimut, Distance, 
          Diam1, Diam2, Diam, Classe, Cat, 
          Qual1, Qual2, Qual, HautT, CodeEcolo, 
          Red, Green, Blue, Dhoup, Choup, 
          Nha, Gha, Vha, VcHa, VpHa, 
          AcctD, AcctV, AcctVper, AcctG, AcctGper, 
          TauxV, TauxPU, Taux, 
          Gain, Gainper, Couvert,
          tCha,
          # Carb1, Carb11, Carb2, Carb21, Carb3, TotalCarbHa,  # Carb en tC/ha
          # DureeVie1, DureeVie11, DureeVie2, DureeVie21, DureeVie3,
          Coupe, Limite, Observations
        )
      # # -- création table Carbone.m # TODO : fonction ?
      # Carbone.m <-
      #   Arbres %>%
      #   select(
      #     NumDisp, NumPlac, NumArbre, Cycle, Essence,
      #     Carb1, Carb11, Carb2, Carb21, Carb3,
      #     DureeVie1, DureeVie11, DureeVie2, DureeVie21, DureeVie3
      #   ) %>%
      #   gather(variable, value, Carb1:DureeVie3) %>%
      #   mutate(
      #     Position = NA,
      #     Position = ifelse(
      #       variable %in% c("Carb1", "DureeVie1"),
      #       1, Position
      #     ),
      #     Position = ifelse(
      #       variable %in% c("Carb11", "DureeVie11"),
      #       11, Position
      #     ),
      #     Position = ifelse(
      #       variable %in% c("Carb2", "DureeVie2"),
      #       2, Position
      #     ),
      #     Position = ifelse(
      #       variable %in% c("Carb21", "DureeVie21"),
      #       21, Position
      #     ),
      #     Position = ifelse(
      #       variable %in% c("Carb3", "DureeVie3"),
      #       3, Position
      #     ),
      #     variable = ifelse(
      #       str_detect(variable, "Carb"), "Carbone", "Duree"
      #     ),
      #     Position = factor(Position, levels = c("1", "11", "2", "21", "3"))
      #   ) %>%
      #   spread(variable, value) %>%
      #   arrange(NumDisp, NumPlac, NumArbre, Cycle)
      
      
      #### --- 3.4/ Variables eco variable aux dates d'inventaire ####
      An <- as.numeric(format(Sys.time(), "%Y")) - 1 
      # TODO : créer sécurité pour vérifier que l'année "an" est bien renseignée 
      # dans la table des prix variables
      Prix = DoubleMercuriale(An, TauxR, PUvar, PU, PA)
      
      Arbres <- 
        Arbres %>% 
        left_join(
          Cycles[, c("NumDisp", "Cycle", "Annee")],
          by = c("NumDisp", "Cycle")
        ) %>% 
        mutate(
          # remplace "Chêne S" et "Chêne P" par "Chêne" car pas de distinction Chêne S/P dans la base de la Forêt Privée
          Essence1 = ifelse(grepl("Chêne", Essence), "Chêne", Essence)
        ) %>% 
        left_join(
          Prix, 
          by = c("Annee", "Essence1" = "Essence", "Classe", "Qual1" = "Qual")
        ) %>% 
        mutate(
          # valeur de consommation "variable"
          VcHav = VcHa * ratio, 
          # valeur potentielle "variable"
          VpHav = 
            (ratio * (Gain + log(Ratiosup / ratio) / 5 * VcHa * AcctD)) / TauxR 
        ) %>%
        select(-Annee, -Essence1, -PUafi, -PUfp, -PUfpConst, -PU95, -ratio, -Ratiosup)
      
      
      ##### 4/ Calculs sur les perches #####
      
      # incrémentation de la barre de progression
      detail = i18n()$t("Traitement des perches...")
      # -- switch shiny ***
      incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
      # *** --
      print(detail)
      
      Perches <- 
        Perches %>% 
        # left_join(Qual, by = c("Qual" = "Nom")) %>% 
        calculs_Perches()
      ##### / \ #####
      
      
      ##### 5/ Calculs sur les bois morts sur pied #####
      
      # incrémentation de la barre de progression
      detail = i18n()$t("Traitement du bois mort sur pied...")
      # -- switch shiny ***
      incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
      # *** --
      print(detail)
      
      BMP <- 
        BMP %>% 
        calculs_BMP() %>% 
        calculs_BM_carbone(
          species_table = Essences,
          living_trees_table = Arbres,
          type = "Bois mort sur pied",
          dead_wood_carbon_content = TauxCarboneBoisMort,
          dead_wood_density = DensiteBoisMort,
          decomposition_stage_code = CodeStadeDecomposition
        )
      ##### / \ #####
      
      
      ##### 6/ Note ecologique ######
      Codes <- bind_rows(Arbres, BMP) %>% filter(CodeEcolo != "") %>% calculs_DMH()
      ##### / \ #####
      
      
      ##### 7/ Calculs sur les bois morts au sol #####
      
      # incrémentation de la barre de progression
      detail = i18n()$t("Traitement du bois mort au sol...")
      # -- switch shiny ***
      incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
      # *** --
      print(detail)
      
      ##### --- 7.1/ Echantillonnage linéaire #####
      
      # incrémentation de la barre de progression
      detail = i18n()$t("...par \u00E9chantillonnage lin\u00E9aire")
      # -- switch shiny ***
      incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
      # *** --
      print(detail)
      
      BMSLineaires <- 
        BMortLineaires %>% 
        left_join(Essences[, c("Essence", "EssReg")], by = "Essence") %>% 
        calculs_BMSLineaires() %>% 
        calculs_BM_carbone(
          living_trees_table = Arbres,
          species_table = Essences,
          type = "Bois mort au sol",
          dead_wood_carbon_content = TauxCarboneBoisMort,
          dead_wood_density = DensiteBoisMort,
          decomposition_stage_code = CodeStadeDecomposition
        )
      
      ##### --- 7.2/ Cercle 20 m #####
      
      # incrémentation de la barre de progression
      detail = i18n()$t("...par \u00E9chantillonnage surfacique")
      # -- switch shiny ***
      incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
      # *** --
      print(detail)
      
      BMSsup30 <- 
        BMortSup30 %>% 
        left_join(Essences[, c("Essence", "EssReg")], by = "Essence") %>% 
        calculs_BMSsup30() %>% 
        calculs_BM_carbone(
          living_trees_table = Arbres,
          species_table = Essences,
          type = "Bois mort au sol",
          dead_wood_carbon_content = TauxCarboneBoisMort,
          dead_wood_density = DensiteBoisMort,
          decomposition_stage_code = CodeStadeDecomposition
        )
      ##### / \ #####
      
      
      ##### 8/ Régénération #####
      
      # incrémentation de la barre de progression
      detail = i18n()$t("Traitement des donn\u00E9es de r\u00E9g\u00E9n\u00E9ration...")
      # -- switch shiny ***
      incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
      # *** --
      print(detail)
      
      # paramètre
      EssRegeneration$EssValor <- 1
      
      # calcul
      Reges <- 
        Reges %>% 
        left_join(EssRegeneration, by = c("NumDisp", "Essence")) %>% 
        calculs_Reges()
      # # A rajouter dans calculs_Reges ? -> suppression des essences arbustives dans les données de régénération (relevés conservés)
      # shrub_species <- 
      #   Essences %>% 
      #   select(Essence, EssReg) %>% 
      #   filter(EssReg == "Espèces arbustives")
      # Reges <- 
      #   Reges %>% anti_join(shrub_species, by = "Essence")
      ##### / \ #####
      
      
      ##### 9/ Calculs sur le taillis #####
      
      # incrémentation de la barre de progression
      detail = i18n()$t("Traitement des donn\u00E9es de taillis...")
      # -- switch shiny ***
      incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
      # *** --
      print(detail)
      
      Taillis <- 
        Taillis %>% 
        left_join(Essences[, c("Essence", "EssReg")], by = "Essence") %>% 
        calculs_Taillis()
      ##### / \ #####
      
      
      ##### 10/ Sauvegarde #####
      # -- sécurité : perches inventoriées avec angle (Diam1 = 16 et Diam2 = 18)
      stems_to_add <- Arbres %>% filter(Cat == "PER") %>% select( names(Perches) )
      Perches <- rbind(Perches, stems_to_add)
      Arbres <- Arbres %>% filter(Cat != "PER")
      
      # if (repSav == repAFI) {
      save(
        NbPlac, Arbres, BMP, Perches, 
        living_trees_carbon_splitted_by_log, # Carbone.m, 
        Reges, Taillis, 
        BMSLineaires, BMSsup30, Codes, AcctD, 
        file = file.path(output_dir, "afiTablesBrutes.Rdata")
      )
      # }
      
      # -- messages de fin
      
      # incrémentation de la barre de progression
      detail = i18n()$t("Calculs par arbres termin\u00E9s et archiv\u00E9s")
      print(detail)
}
