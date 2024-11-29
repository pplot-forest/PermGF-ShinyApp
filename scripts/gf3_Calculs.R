# ----- chargement des packages nécessaires -----
suppressMessages({
  library("dplyr")
  library("stringr")
  library("rlang")
  library("tidyr")
})

##### fonction calculs des quantités de carbone pour les arbres vivants #####
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
      # -- Rappel du poids
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
    select(IdArbre, Cycle, Poids, Diam, Hauteur, TypeEss, TauxCarbone, InfraDensite, Reg1, V1, V2, V3) %>% 
    
    # pivot volumes V1, V2 et V3
    pivot_longer(
      cols = c("V1", "V2", "V3"),
      names_to = "billon",
      names_pattern = "V(.)",
      names_transform = list(billon = as.numeric),
      values_to = "volume"
    ) %>% 
    
    # -- Définition des qualités des billons
    mutate(
      # qualités des billons
      qual_billon = case_when(
        billon == 1 ~ Reg1,
        # HYPOTHESE ==> qualites du billon 2 (de 3m à la decoupe gestionnaire) :
        # si A, B ou C au premier billon alors cela sera du C et si D alors D pour le reste)
        billon == 2 & Reg1 != "D" ~ "C",
        billon == 2 & Reg1 == "D" ~ "D",
        billon == 3 ~ "D"
      ),
      # qualités des connexes
      qual_connexe = "D",
      # suppression de la qualité globale de la grume (Reg1)
      Reg1 = NULL
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
        # la formule du cylindre (V2 = (diam_med / 100) ^ 2 (pi * Hauteur * Poids) / 4
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
#       time_span = paste(".", StadeD, sep = "")
#     ) %>% 
#     left_join(
#       decomposition_stage_code, 
#       by = c("time_span" = "Stade_AFI")
#     ) %>% 
#     mutate(
#       Code = ifelse(Stade_AFI == "1.1", 1, Code),
#       Code = ifelse(Stade_AFI == "2.1", 1, Code),
#       Code = ifelse(Stade_AFI == "4.4", 5, Code),
#       Code = ifelse(Stade_AFI == "4.5", 5, Code)
#     ) %>% 
#     # taux carbone Fs/Rx
#     left_join(
#       species_table[,c("Essence","TypeEss")], 
#       by = "Essence") %>% 
#     # taux de carbone dans feuillus/resineux en fonction stade
#     left_join(
#       standing_dead_wood_carbon_content,
#       by = c("TypeEss", "Code")
#     ) %>% 
#     left_join(
#       standing_dead_wood_density, 
#       by = c("Essence", "Code")
#     ) %>% 
#     mutate(
#       Carbone = Vha * Infradensite * (Taux_carbone) / 100
#     ) %>% 
#     select(
#       -Stade_AFI, -time_span, -Code, -TypeEss, 
#       -Taux_carbone, -SRF, -Infradensite
#     )
#   
#   # -- return of calculs_BMP_carbone function
#   return(df)
# }

#### fonction calculs du carbone dans le bois mort au sol ####
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
    group_by(NumForet, Cycle, NumPlac, Essence) %>% 
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
      time_span = paste(".", StadeD, sep ="")
    ) %>% 
    left_join(
      decomposition_stage_code, 
      by = c("time_span" = "Stade_AFI")
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
      by = c("NumForet", "Cycle", "NumPlac")
    ) %>% 
    mutate(
      species_to_join = ifelse(
        Essence %in% c(
          "Ind.", "Indéterminée", "Non relevée", 
          "Résineux Ind.", "Feuillus Ind.", "Feuillus", "Résineux"
        ), 
        main_species, 
        Essence
      ),
      # sécurité si table vide (Essence doit être character pour jonction)
      species_to_join = as.character(species_to_join)
    ) %>% 
    left_join(
      species_table[, c("Nom", "TypeEss")], 
      by = c("species_to_join" = "Nom")
    ) %>% 
    # taux de carbone dans feuillus/resineux en fonction stade
    left_join(
      dead_wood_carbon_content_filtered, 
      by = c("TypeEss", "Code")
    ) %>% 
    left_join(
      dead_wood_density_filtered, 
      by = c("species_to_join" = "Essence", "Code")
    ) %>% 
    # carbone en tC/ha
    mutate(
      tCha = Vha * Infradensite * SRF * (Taux_carbone) / 100
    ) %>% 
    select(
      -Stade_AFI, -time_span, -Code, -main_species, 
      -TypeEss, -Taux_carbone, -SRF, -Infradensite, -species_to_join
    )
  
  # -- return of 'calculs_BM_carbone' function
  return(df)
}


##### fonction de traitement de la régénération #####
calculs_rege <- function(
  df = NULL, code_essreg = NULL
  ) {
  df <- 
    df %>% 
    left_join(code_essreg, by = c("NumForet", "Essence")) %>% 
    filter(!is.na(Essence)) %>% # Il peut y avoir des essences vides si on a fait une Ss-placette et juste noté 1 observation
    # select(-one_of("Rejet","Observations")) %>%
    arrange(NumForet, Cycle, NumPlac, SsPlac) %>%
    # replace(is.na(.),0) %>%
    mutate(
      # EssReg = ifelse(!is.na(EssRegPar), EssRegPar, EssReg),
      
      Recouv = ifelse(is.na(Recouv), 0, Recouv),
      Class1 = ifelse(is.na(Class1), 0, Class1),
      Class2 = ifelse(is.na(Class2), 0, Class2),
      Class3 = ifelse(is.na(Class3), 0, Class3),
      
      Recouv = as.numeric(Recouv),
      Class1 = as.numeric(Class1),
      Class2 = as.numeric(Class2),
      Class3 = as.numeric(Class3),
      
      # Surf = ifelse(Class1 + Class2 + Class3 >= 5, 1, 0),
      plac_nb = NbSousPlac,
      
      Recouv = Recouv / plac_nb,
      Classe1Ha = Class1 * 10000 / (pi * RayonSousPlac ^ 2) / plac_nb,
      Classe2Ha = Class2 * 10000 / (pi * RayonSousPlac ^ 2) / plac_nb,
      Classe3Ha = Class3 * 10000 / (pi * RayonSousPlac ^ 2) / plac_nb
    )
  
  # -- retour de la fonction calculs_Reges
  return(df)
}

##### fonction pour joindre Placettes et Echantillonnages #####
set_up_calcul_tables <- function(
  df = NULL, 
  plot_df = Placettes, 
  settings_df = Echantillonnages,
  add_mirror = F
) {
    if (nrow(df) > 0) {
      df <- 
        df %>%
        left_join(
          plot_df[, c(
            "NumForet", "NumPlac", "Cycle", "Strate", "PoidsPlacette", 
            "Pente", "CoeffPente", "Parcelle", "Station", 
            "Miroir_Azimut", "Miroir_Dist", "Miroir_Transect", "Miroir_nb_SsPlac"
          )], 
          by = c("NumForet", "NumPlac", "Cycle")
        ) %>%
        left_join(
          settings_df[, c(
            "NumForet", "Cycle", "Strate", "NbPlac",
            "Taillis", "BMP", "TypeTarifBMP", "NumTarifBMP",
            "BMSLineaire", "BMSCercle",
            "NbSousPlac", "RayonSousPlac"
          )], 
          by = c("NumForet", "Cycle", "Strate")
        )
      
      # Cas des transects amputé sur les placettes miroir - 
      # gestion des longueurs de transect différentes (2024 FC Canton Genève)
      if ("Transect" %in% names(df)) {
        df <- 
          df %>% 
          mutate(BMSLineaire = ifelse(
            !is.na(Miroir_Transect), 
            Miroir_Transect, 
            BMSLineaire
          ))
      }
      
      # Cas des sous-placettes supprimées car transect amputé sur les placettes miroir - 
      # gestion du nombre de placettes différentes (2024 FC Canton Genève)
      if ("SsPlac" %in% names(df)) {
        df <- 
          df %>% 
          mutate(NbSousPlac = ifelse(
            !is.na(Miroir_nb_SsPlac), 
            Miroir_nb_SsPlac, 
            NbSousPlac
          ))
      }
    } #else {df}
  if (add_mirror == TRUE) {
    df <- df %>% miroir_add()
  }
  
  # -- retour de la fonction set_up_calcul_tables
  return(df)
}

##### fonction pour calculer les résultats sur le cercle #####
calculs_cercles <- function(
  df = NULL, population = NULL, dist_max = NULL,
  add_bmp_vars = F, code_essreg = NULL,
  diam_cat = NULL
) {
  # id vars
  id_vars <- c("NumForet", "NumPlac", "Cycle")
  
  # bmp vars to add
  attribute_vars <- c(
    "Essence", "EssReg", "Diam", "Classe", "Cat", 
    "Haut", "Type", "Stade"
  )
  if (add_bmp_vars == F) {
    attribute_vars <- 
      setdiff(attribute_vars, c("Haut", "Type", "Stade"))
  }
  
  # results_vars
  results_vars <- c("Nha", "Gha", "Vha")
  vars_to_select <- c(
    id_vars, attribute_vars, results_vars
  )
  
  # quote
  quo_population <- quo(!!parse_expr(population))
  
  # process
  df <- 
    if (nrow(df) > 0) {
      df %>%
        filter(Population == population) %>% 
        mutate(
          # !!population := ifelse(
          #   is.na(!!quo_population), 
          #   dist_max, # valeur par défaut
          #   !!quo_population
          # ),
          var = !!quo_population,
          var = ifelse(
            is.na(var), 
            dist_max, # valeur par défaut
            var
          ),
          Nha = 10000 / pi/ (var ^ 2) * Nbre,
          Gha = pi / 40000 * Diam ^ 2 * Nha, 
          Classe = floor(Diam / 5 + 0.5) * 5, 
          Cat = cut(
            Diam, 
            breaks = c(diam_cat$Diam, Inf), 
            labels = diam_cat$Cat, 
            include.lowest = T, right = F
          ), 
          Cat = as.character(Cat), 
          # Vha = Gha * 7
          Vha = case_when(
            # if population = "Taillis" then volume = Gha * 7
            population == "Taillis" ~ Gha * 7,
            # if population = "BMP" with empty heights then volume = Gha * 8 ...
            is.na(Haut) & population == "BMP" ~ Gha * 8,
            # ... else volume -> cyclindre
            !is.na(Haut) & population == "BMP" ~ pi / 40000 * (Diam - (Haut / 2 - 1.30)) ^ 2 * Haut * Nha,
            TRUE ~ 0
          )
        ) %>%
        left_join(code_essreg, by = c("NumForet", "Essence")) %>%
        select(
          all_of(vars_to_select)
        ) #%>% # TODO : à contrôler
      # find_ESSREG()
    } else {
      data.frame(
        NumForet = numeric(), NumPlac = character(), 
        Cycle = numeric(), Essence = character(), 
        EssReg = character(), Diam = numeric(), 
        Classe = numeric(), Cat = character(), 
        Nha = numeric(), Gha = numeric(), Vha = numeric(), 
        stringsAsFactors = F
      ) %>% 
        select( all_of(vars_to_select) )
    }
  
  # -- retour de la fonction calculs_cercles
  return(df)
}

##### fonction de calcul des volumes de bois mort sur pied #####
calculs_bmp <- function(df = NULL) {
  df <- 
    if (nrow(df) > 0) {
      df %>% 
        mutate(
          TypeTarifBMP = ifelse(
            is.na(Type) | Type == "C"| Type == "S", 
            NA, 
            TypeTarifBMP
          ), 
          NumTarifBMP = ifelse(
            is.na(Type) | Type == "C"| Type == "S", 
            NA, 
            NumTarifBMP
          )
        ) %>% 
        rename(
          TypeTarif = TypeTarifBMP,
          NumTarif = NumTarifBMP
        ) %>% 
        calculs_Vol() %>%
        mutate(
          Vha = ifelse(
            !is.na(Vha), Vha, 
            # Type == "A" & !is.na(Type), Vha, 
            ifelse(
              is.na(Haut), 
              8 * Gha, 
              pi / 40000 * (Diam - (Haut / 2 - 1.30)) ^ 2 * 
                Haut * Nha
            )
          ),
          StadeE = floor(Stade / 10),
          StadeD = Stade - StadeE * 10
        ) %>%
        select(
          NumForet, NumPlac, Cycle, Dist, Essence, EssReg, 
          Diam, Classe, Cat, Type, Limite, Stade, StadeD, StadeE, Nha, Gha, Vha
        )
    } else {
      data.frame(
        NumForet = numeric(), NumPlac = character(), 
        Cycle = numeric(), Dist = numeric(), Transect = character(), 
        Essence = character(), EssReg = character(), 
        Diam = numeric(), Classe = numeric(), Cat = character(), 
        Type = character(), Limite = numeric(), Stade = numeric(),
        StadeD = numeric(), StadeE = numeric(), 
        Nha = numeric(), Gha = numeric(), Vha = numeric(), 
        stringsAsFactors = F
      )
    }
  
  # -- retour de la fonction calculs_bmp
}

##### fonction de calcul des volumes de bois mort au sol > 30 cm #####
# ----- calcul des Vha
calculs_bms_sup30 <- function(
  df = NULL, code_essreg = NULL
  ) { # TODO : si un jour besoin changement protocole, cf PermPSDRF2
  df <- 
    if (nrow(df) > 0) {
    df %>% 
        rename("Rayon" = "BMSCercle") %>%
      mutate(
        DiamIni = ifelse(is.na(DiamIni), 0, DiamIni),
        DiamMed = ifelse(is.na(DiamMed), 0, DiamMed),
        DiamFin = ifelse(is.na(DiamFin), 0, DiamFin),
        Vha = 0,
        Classe = 0,
        # ---- formule de Huber
        Vha = ifelse(
          (DiamIni+ DiamFin) == 0,
          pi / 40000 * DiamMed ^ 2 * Longueur * 10000 / pi / Rayon ^ 2,
          Vha
        ),
        Classe = ifelse(
          (DiamIni + DiamFin) == 0,
          floor(DiamMed / 5 + 0.5) * 5,
          Classe
        ),
        StadeE = floor(Stade / 10),
        StadeD = Stade - StadeE * 10,
        # ---- formule de Smalian
        Vha = ifelse(
          (DiamIni + DiamFin) != 0 & DiamMed == 0,
          pi / 80000 * (DiamIni ^ 2 + DiamFin ^ 2) * 
            Longueur * 10000 / pi / Rayon ^ 2,
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
            Longueur * 10000 / (pi * Rayon ^ 2),
          Vha
        ),
        Classe = ifelse(
          (DiamIni + DiamFin) != 0 & DiamMed != 0,
          floor((DiamIni + DiamFin + DiamMed) / 3 / 5 + 0.5) * 5,
          Classe
        )
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
      left_join(code_essreg, by = c("NumForet", "Essence")) %>% 
      # TODO : add find_ESSREG ? sur autres tables aussi
      mutate(EssReg = as.character(EssReg)) %>% 
      select(
        NumForet, NumPlac, Cycle, Essence, EssReg, 
        DiamIni, DiamMed, DiamFin,
        Classe, Cat, Stade, StadeD, StadeE, Vha
      )
  } else {
    BMSsup30 <-
      data.frame(
        NumForet = numeric(), NumPlac = character(), 
        Cycle = numeric(), 
        Essence = character(), EssReg = character(), 
        Diam = numeric(), Classe = numeric(), 
        Cat = character(), 
        Stade = numeric(),
        StadeD = numeric(), StadeE = numeric(), Vha = numeric(), 
        stringsAsFactors = F
      )
  }
  
  # -- retour de la fonction calculs_BMSsup30
  return(df)
}


##### fonction de traitement du bois mort au sol < 30 cm #####
calculs_bm_lineaire <- function(
  df = NULL, code_essreg = NULL
  ) {
  df <- if (nrow(df) > 0) {
    df %>% 
      # rename("Lineaire" = "BMSLin\u00E9aire") %>%
      rename("Lineaire" = "BMSLineaire") %>%
    mutate(
      Angle = ifelse(is.na(Angle), 0, Angle),
      Classe = floor(Diam / 5 + 0.5) * 5,
      Cat = cut(
        Diam, 
        breaks = c(0, 17.5, 27.5, 47.5, 67.5, 200),
        labels = c("PER", "PB", "BM", "GB","TGB"),
        include.lowest = T,
        right = F
      ),
      Cat = as.character(Cat),
      StadeE = floor(Stade / 10),
      StadeD = Stade - StadeE * 10,
      Vha = pi ^ 2 / 8 / Lineaire * Diam ^ 2 / cos(Angle / 180 * pi)
    ) %>%
    left_join(code_essreg, by = c("NumForet", "Essence")) %>% 
    mutate(EssReg = as.character(EssReg)) %>% 
    select(
      NumForet, NumPlac, Cycle, Transect, Essence, EssReg,
      Diam, Classe, Cat, Stade, StadeD, StadeE, Vha
    )
  } else { # TODO : raccourcir avec quotes ?
    data.frame(
      NumForet = numeric(), NumPlac = character(), 
      Cycle = numeric(),
      Transect = character(), Essence = character(), 
      EssReg = character(), Diam = numeric(), 
      Classe = numeric(), Cat = character(), 
      Stade = numeric(),
      StadeD = numeric(), StadeE = numeric(), Vha = numeric(), 
      stringsAsFactors = F
    )
  }
  
  # -- retour de la fonction calculs_BMSLineaires
  return(df)
}

##### fonction dmh_split #####
dmh_split <- function (
  df0 = NULL, 
  list = list
) {
  df <- data.frame(
    NumForet = rep.int(df0$NumForet, sapply(list, length)), 
    NumPlac = rep.int(df0$NumPlac, sapply(list, length)), 
    NumArbre = rep.int(df0$NumArbre, sapply(list, length)), 
    CodeEcolo = unlist(list), 
    stringsAsFactors  =  F
  )
  # suppression de la colonne CodeEcolo
  df0 <- df0 %>% select(-CodeEcolo)
  # fusion de df et df0
  df <- df %>% left_join(df0, by = c("NumForet", "NumPlac", "NumArbre"))
  
  # -- retour de dmh_split
  return(df)
}

##### fonction de calculs des dmh (duplique les lignes de la table Arbres pour chaque dmh reconnu) #####
calculs_dmh <- function(
  df = NULL, dmh_df = NULL
) {
  
  if (nrow(df) > 0) {
    df <- df %>% mutate(Ref_CodeEcolo = tolower(Ref_CodeEcolo))
    df_ProSilva <- df %>% filter(Ref_CodeEcolo == "prosilva")
    df_AFI <- df %>% filter(Ref_CodeEcolo == "engref" | Ref_CodeEcolo == "afi")
    df_EFI <- df %>% filter(Ref_CodeEcolo == "efi")
    df_IRSTEA <- df %>% filter(Ref_CodeEcolo == "irstea")
    
    # ----- Codification ProSilva
    if (nrow(df_ProSilva) > 0) {
      # ---- décomposition
      # liste
      list <- with(
        df_ProSilva,
        str_split(CodeEcolo, boundary("word"))
      )
      # df
      codes1 <- dmh_split(df_ProSilva, list)
    } else {
      codes1 <- data.frame()
    }
    
    # ----- Codification AFI
    if (nrow(df_AFI) > 0) {
      # ----- df # TODO : à mettre dans le job2
      df_AFI <- df_AFI %>% mutate(CodeEcolo = str_replace(CodeEcolo, "0", ""))
      # ----- niveaux
      niveaux <- 
        dmh_df %>% 
        filter(Codification == "engref") %>% 
        select(Code) %>% 
        unlist()
      # ---- décomposition
      # liste
      list <- c()
      for (i in 1:nrow(df_AFI)) {
        list0 <- with(df_AFI, str_extract(CodeEcolo[i], niveaux))
        list0 <- list0[ !is.na(list0) ]
        list <- c(list, list(list0))
      }
      # df
      codes2 <- dmh_split(df_AFI, list)
    } else {
      codes2 <- data.frame()
    }
    
    # ----- Codification EFI
    if (nrow(df_EFI) > 0) {
      # ---- décomposition
      # liste
      list <- with(
        df_EFI,
        str_split(CodeEcolo, boundary("word"))
      )
      # df
      codes3 <- dmh_split(df_EFI, list)
    } else {
      codes3 <- data.frame()
    }
    
    # ----- Codification IRSTEA
    if (nrow(df_IRSTEA) > 0) {
      # ----- df # TODO : à mettre dans le job2
      df_IRSTEA <- df_IRSTEA %>% mutate(CodeEcolo = str_replace(CodeEcolo, "0", ""))
      # ----- niveaux
      niveaux <- 
        dmh_df %>% 
        filter(Codification == "IRSTEA") %>% 
        select(Code) %>% 
        unlist()
      # ---- décomposition
      # liste
      list <- c()
      for (i in 1:dim(df_IRSTEA)[1]) {
        list0 <- with(df_IRSTEA, str_extract(CodeEcolo[i], niveaux))
        list0 <- list0[ !is.na(list0) ]
        list <- c(list, list(list0))
      }
      # df
      codes4 <- dmh_split(df_IRSTEA, list)
    } else {
      codes4 <- data.frame()
    }
    
    codes <- rbind(codes1, codes2, codes3, codes4)
  } else {
    codes <- df
  }
  
  # retour de la fonction calculs_dmh
  return(codes)
}

##### fonction change_protocole : extrait de la table Echantillonnages les cycles concernés par le changement de protocole (s'il existe) #####
change_protocole <- function (echant_DF) {
  # echant_DF <- Echantillonnages # debug
  # on détecte si le protocole change à travers les cycles d'inventaire
  # echant_NAMES <- syms(setdiff(names(echant_DF), "Cycle"))
  echant_NAMES <- c(
    "NumForet", "Strate", "DiamLim1", "Rayon1", "DiamLim2", "Rayon2",
    "DiamLim3", "Rayon3", "Coeff", "DiamLim"
  )
  echant_DF <-
    echant_DF %>%
    distinct(!!!syms(echant_NAMES), .keep_all = T)
  # on recrée une table "echant_DF" avec les paramètres du plus
  # grand dénominateur commun entre les 2 (ou plus) protocoles d'inventaire
  # différents
  echant_DF <-
    echant_DF %>%
    group_by(NumForet, Strate) %>%
    mutate(
      DiamLim1 = min(DiamLim1),
      Rayon1 = min(Rayon1),
      
      DiamLim2 = min(DiamLim2),
      Rayon2 = min(Rayon2),
      
      DiamLim3 = min(DiamLim3),
      Rayon3 = min(Rayon3),
      
      DiamLim = min(DiamLim),
      Coeff = max(Coeff),
      
      # Identifiant pour retrouver les arbres concernés par le
      # changement de protocole
      echant_ID =
        paste0(
          NumForet, "-", Cycle,"-", Strate
        ),
      Observations = NULL
    ) %>%
    ungroup()
  # retour fonction change_protocole
  return(echant_DF)
}

##### fonction pour accoler des EssReg par défaut à une table #####
find_ESSREG <- function(
  df = NULL, code_essences = NULL
  ) {
  pos <- which(is.na(df$EssReg))
  
  if (length(pos) > 0) {
    essences_LISTE <- unique(df$Essence[pos])
    essences_DF <- filter(code_essences, Nom %in% essences_LISTE) %>% select(Nom, Reg)
    df <-
      df %>%
      left_join(essences_DF, by = c("Essence" = "Nom")) %>%
      mutate(EssReg = ifelse(is.na(EssReg), Reg, EssReg)) %>% 
      select(-Reg)
  }
  
  # -- retour de la fonction find_ESSREG
  return(df)
}

##### fonction de contôle des arbres dupliqués (arbres repérés az+dist uniquement) #####
check_duplicated_trees <- function(table = NULL) {
  # -- arrange table
  table <- table %>% arrange(NumForet, NumPlac, Cycle, Azimut, Dist)
  
  # -- valeurs dupliquées
  duplicated_trees <- 
    which(duplicated(table[, c("NumForet", "NumPlac", "Cycle", "Azimut", "Dist")]))
  
  # -- édition d'un classeur listant les valeurs dupliquées
  if (length(duplicated_trees) > 0) {
    # get duplicated trees from last
    duplicated_trees_fromLast <- which(duplicated(
      table[, c("NumForet", "NumPlac", "Cycle", "Azimut", "Dist")], 
      fromLast = TRUE
    ))
    # stack row values
    duplicated_trees <- c(duplicated_trees, duplicated_trees_fromLast)
    # duplicated table
    duplicated_trees_table <- table[
      duplicated_trees, c(
        "NumForet", "NumPlac", "Cycle", "NumArbre", "Essence", 
        "Azimut", "Dist", "Diam1"
      )] %>%
      arrange(NumForet, NumPlac, Cycle, Azimut, Dist)
    
    # write table
    write.xlsx(duplicated_trees_table, file = "doublons_calcul_arbres.xlsx")
    # stop message
    stop("Attention : doublon(s) d\u00E9tect\u00E9(s) lors des calculs d'accroissement.\n\nDoublons list\u00E9s dans le classeur excel 'doublons_calcul_arbres.xlsx'")
  }
  
  # -- return of 'check_duplicated_trees' function
  return(table)
}

##### fonction de calcul des accroissements #####
calculate_increments <- function(
  df = NULL, 
  echant_change = F,
  cycles_table = NULL, 
  plot_table = NULL,
  vars = c("Diam", "Gha", "Vha", "VcHa")
  ) {
  table <- 
    df %>%
    # Arbres %>%
    # filter(NumPlac %in% c("1")) %>%  # debug 1, 11, 129
    select(
      "NumForet", "NumPlac", "NumArbre", "IdArbre", "Cycle", "Type", "Coupe", 
      "Nha", # repère pour savoir si coupe ou PF
      all_of(vars)
    ) %>% 
    # pivot_longer(cols = vars) %>%
    arrange(NumForet, NumPlac, NumPlac, IdArbre)
  
  # table contenant les temps de passage entre chaque cycle
  time_span_table <- 
    cycles_table %>% 
    group_by(NumForet) %>% 
    mutate(time_span = Annee - lag(Annee)) %>% 
    ungroup() %>% 
    select(NumForet, Cycle, time_span)
  
  # table de suivi des placettes entre les inventaires (+ pas de temps)
  monitoring_table <- 
    plot_table %>% 
    # filter(NumPlac %in% c(10, 129)) %>%  # debug champlalot
    
    # join time_span_table
    left_join(time_span_table, by = c("NumForet", "Cycle")) %>% 
    
    # select & arrange
    select(NumForet, NumPlac, Cycle, time_span) %>% 
    arrange(NumForet, NumPlac, Cycle) %>% 
    
    # liste des placettes pour lesquelles le calcul des accroissements est possible
    group_by(NumForet, NumPlac) %>% 
    mutate(test = case_when(
      Cycle > 1 & !is.na(lag(Cycle)) ~ 1, 
      Cycle == 1 ~ 1
    )) %>% 
    ungroup() %>% 
    filter(test == 1) %>% 
    select(-test)
  
  # main table
  table <- left_join(
    monitoring_table, table,
    by = c("NumForet", "NumPlac", "Cycle")
  ) %>% 
    arrange(NumForet, NumPlac, NumArbre, IdArbre, Cycle) %>%
    
    group_by(NumForet) %>% 
    mutate(last_cycle = last(Cycle)) %>% 
    
    group_by(NumForet, NumPlac, NumArbre, IdArbre) %>% 
    # head() %>% # debug
    mutate(
      Coupe = as.character(Coupe),
      
      # arbres disparus (coupés ou chablis ou morts sur pied)
      missing = case_when( 
        # arbres devenus morts sur pied et non exploités (valeur par défaut car notation non disponible) :
        is.na(Type) & !is.na(lead(Type)) & Cycle < last_cycle & is.na(Coupe) ~ 
          "C",
        # arbres devenus morts sur pied et exploités ou chablis (avec notation disponible) :
        is.na(Type) & !is.na(lead(Type)) & Cycle < last_cycle & !is.na(Coupe) ~ 
          Coupe,
        
        # arbres coupés :
        is.na(lead(Nha)) & !is.na(Nha) & Cycle < last_cycle & is.na(Coupe) ~  
          "E",
        is.na(lead(Nha)) & !is.na(Nha) & Cycle < last_cycle & Coupe == "C" ~ 
          "C",
        is.na(lead(Nha)) & !is.na(Nha) & Cycle < last_cycle & Coupe != "C" ~ 
          "E"
      ),
      
      # arbres passant à la futaie (N.B : avec case_when, impossible de combiner "PF" et "E" ):
      # la colonne coupe peut déjà contenir des infos
      # les notations "C" ou "PF/C" sont à conserver.
      # les notations "E", "PF" ou "PF/E", sont de toutes façons reconstituées
      promoted = case_when(
        is.na(lag(Nha)) & !is.na(Nha) & Nha > 0 & Cycle > 1 ~ 
          "PF",
      # cas des arbres limites déjà présents dans l'inventaire et qui deviennent bons
        (!is.na(lag(Nha)) & lag(Nha) == 0) & # arbre précédemment limite
          !is.na(Nha) & Nha > 0 & # arbre actuellement non limite
          Cycle > 1 ~ 
          "PF"
      ),
      
      # merge missing and promoted columns
      Coupe = case_when(
        promoted == "PF" & is.na(missing) ~ "PF",
        promoted == "PF" & !is.na(missing) ~ paste0(promoted, "/", missing),
        is.na(promoted) ~ missing
      ),
      # missing = NULL,
      # promoted = NULL,
      
      # Coupe = ifelse(is.na(Limite), Coupe, NA)
      # --- Calcul des accroissements
      # time_span = Annee - lag(Annee),
      # default_value = ifelse(name == "Diam", NA, 0),
      # digits_value = 10, # possibilité de changer si besoin
      Acct_Diam = round( (Diam - lag(Diam, default = NA)) / time_span, digits = 10),
      AcctGper = round( (Gha - lag(Gha, default = 0)) / time_span, digits = 10),
      AcctVper = round( (Vha - lag(Vha, default = 0)) / time_span, digits = 10),
      Gainper = round( (VcHa - lag(VcHa, default = 0)) / time_span, digits = 10),
      
      # cleaning
      last_cycle = NULL,
      # time_span = NULL,
      Diam = NULL,
      Gha = NULL,
      Vha = NULL,
      VcHa = NULL
    ) %>%
    ungroup() %>%
    as.data.frame() #%>% 
    # select(names(df1))
  
  df <- 
    df %>% 
    select(-Coupe) %>% 
    left_join(
      table, 
      by = c("NumForet", "NumPlac", "NumArbre", "IdArbre", "Cycle", "Type", "Nha")
    )
  
  # -- return of 'calculate_increments' function
  return(df)
}

##### fonction de partage des accroissements en diamètre calculés aux autres arbres #####
share_accD <- function(df = NULL, echant_change = F) {
  ### ----- Table des accroissements en diamètre ----- ###
  # Attribution d'une valeur d'accroissement en diamètre (si Acct_Diam vide)
  AcctD_DF <-
    df %>% # s'assurer qu'on a bien des arbres limites dans la table
    select(
      IdArbre, NumForet, NumPlac, NumArbre, Cycle, Essence, 
      Classe, Acct_Diam, Coupe
    ) %>%
    
    # valeurs moyennes d'AcctD attribuées par forêt, essence et classe
    group_by(NumForet, Essence, Classe) %>%
    mutate(AcctD_ForetEssClasse = mean(Acct_Diam, na.rm = T)) %>%
    # sinon valeurs moyennes d'AcctD attribuées par forêt et par essence
    group_by(NumForet, Essence) %>%
    mutate(AcctD_ForetEss = mean(Acct_Diam, na.rm = T)) %>%
    # sinon valeurs moyennes d'AcctD attribuées par forêt
    group_by(NumForet) %>%
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
    group_by(NumForet, NumPlac, NumArbre) %>%
    mutate(
      AcctD = 
        ifelse(
          Cycle == 1 & !(Coupe %in% c("E", "C")), lead(AcctD), AcctD
        )
    ) %>%
    ungroup() %>%
    
    select(IdArbre, NumForet, NumPlac, NumArbre, Cycle, AcctD)
  
  # Récupération des AcctD dans la table principale
  df <-
    df %>%
    left_join(
      AcctD_DF, 
      c("IdArbre", "NumForet", "NumPlac", "NumArbre", "Cycle")
    )
  if (echant_change == F) {
    df <-
      df %>%
      mutate(echant_ID = NA) %>%
      # selection de variables
      select(
        NumForet, NumPlac, NumArbre, IdArbre, Cycle, Strate, 
        Essence, EssReg, Azimut, Dist, 
        
        NumTarif, TypeTarif, TypeEss, CoefHoupp, TauxCarbone, InfraDensite, 
        
        Diam1, Diam2, Diam, Classe, Cat, 
        DiamSup, ClasseSup, VhaSup, TauxV, 
        Qual, Reg1, Reg2, PU, PUSup, 
        Nha, Gha, Vha, VhaIFN, VcHa, 
        Coupe, Limite, 
        CodeEcolo, Ref_CodeEcolo, 
        Type, Haut, Stade, Caract1, Caract2, Caract3, Observations, 
        
        AcctGper, AcctVper, Gainper, AcctD, 
        Coupe, time_span
      )
  } else {
    # selection de variables
    df <-
      df %>%
      select(
        NumForet, NumPlac, NumArbre, IdArbre, Cycle, Strate, 
        Essence, EssReg, Azimut, Dist, 
        Diam1, Diam2, Diam, Classe, Cat, 
        DiamSup, ClasseSup, VhaSup, TauxV, 
        Qual, Reg1, Reg2, PU, PUSup, 
        Nha, Gha, Vha, VhaIFN, VcHa, 
        Coupe, Limite, echant_ID, 
        CodeEcolo, Ref_CodeEcolo, 
        Type, Haut, Stade, Caract1, Caract2, Caract3, Observations, 
        
        AcctGper, AcctVper, Gainper, AcctD, 
        Coupe, time_span
      )
  }
  # Changer les noms !!!
  # + Raccorder à AccD lorsqu'il n'y a qu'un seul cycle
  # + Attention aux exigences du modèle .xls
  
  # -- return of 'share_accD' function
  return(df)
}

##### fonction de calcul des variables éconoomiques #####
calculs_Eco <- function(
  df = NULL, code_prix = NULL
  ) {
  df <-
    df %>%
    # ---------- Valeur de consommation
    mutate(
      VcHa = Vha * PU, 
      # ---------- Volume de la classe supérieure
      DiamSup = Diam + 5, 
      ClasseSup = Classe + 5
    ) %>%
    calculs_Vol(Sup = T) %>%
    # ---------- Taux d'accroissement en volume
    mutate(
      TauxV = ifelse(Vha > 0, log(VhaSup / Vha) / 5, 0)
    ) %>%
    # ---------- Calcul prix de la classe supérieure (préparation valeur potentielle)
    left_join(
      code_prix, # PrixSup
      by = c("Essence", "ClasseSup" = "Classe", "Reg1" = "Qual"), 
      suffix = c("", "Sup")
    )
  # retour fonction calculs_Eco
  return(df)
}

##### fonction de calcul des volumes #####
# STEP4
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
  # var_name <- quo_name(var) # puis appel !!var_name: = f(!!var) dans mutate
  
  # Pour mémo (2) fonctionnement avec argument d'entrée définissant la variable (Vha)
  # var <- enquo(var)
  # var_name <- paste0(quo_name(var), add_var)
  
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
          !!TypeTarif == "SchR", 
          5 / 70000 * (8 + !!NumTarif) * (!!Diam - 5) * (!!Diam - 10) * Nha, 
          !!var
        ), 
      !!var_name := 
        ifelse(
          !!TypeTarif == "SchI", 
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
        ), # sécurité pour les tiges de moins de 10 # A revoir ?
      
      # cas des chandelles et des souches :
      !!var_name := 
        ifelse(
          !is.na(Type) & Type %in% c("C", "S"),
          ifelse(
            is.na(Haut), 8 * Gha, 
          # calcul du volume à partir de la hauteur (diamètre médian et décroissance métrique = 1cm/m par défaut)
          pi / 40000 * (!!Diam - (Haut / 2 - 1.30)) ^ 2 * Haut  ) * Nha,
          !!var
        )
    ) 
  # retour fonction calculs_Vol
  return(df)
}

##### fonction de calcul du poids (feuille Arbres) #####
calculs_Nha <- function(df = NULL) {
  ##### Ancienne version
  # # # debug - tests
  # # df <- data.frame(
  # #   Cat = c(rep("PER", 10), rep("PB",5), rep("BM",5)),
  # #   Diam1 = c(
  # #     17, 16, 12, 14, 15, 16, 6, 8, 12, 13,
  # #     20, 18, 21, 27, 28,
  # #     28, 26, 33, 38, 37
  # #   ),
  # #   Diam2 = c(
  # #     17, 18, 13, 14, 15, 16, 6, 6, 12, 13,
  # #     20, 17, 21, 28, 27,
  # #     27, 26, 33, 38, 37
  # #   ),
  # #   Dist = c(
  # #     NA, NA, NA, NA, NA, rep(9, 5),
  # #     7, 6, 4, 10, 8,
  # #     8, 8, 5, 4, 3
  # #   ),
  # #   Rayon1 = c(
  # #     rep(10, 10),
  # #     10, 10, 10, 10, NA,
  # #     10, 10, 10, NA, NA
  # #   ),
  # #   Rayon2 = c(
  # #     rep(20, 10),
  # #     20, 20, 20, 20, NA,
  # #     20, 20, 20, NA, NA
  # #   ),
  # #   Rayon3 = c(
  # #     rep(30, 5), rep(NA, 5),
  # #     rep(NA, 2), rep(30, 3),
  # #     rep(30, 2), rep(NA, 3)
  # #     # 30, 30, 30, NA, NA,
  # #     # 30, 30, 30, NA, NA
  # #   ),
  # #   DiamLim1 = rep(7.5, 20),
  # #   DiamLim2 = rep(17.5, 20),
  # #   DiamLim3 = c(
  # #     rep(27.5, 5), rep(NA, 5),
  # #     rep(NA, 2), rep(27.5, 3),
  # #     rep(27.5, 2), rep(NA, 3)
  # #   ),
  # #   DiamLim = c(
  # #     rep(NA, 5), rep(27.5, 5), 
  # #     rep(27.5, 2), rep(NA, 3), 
  # #     rep(NA, 2), rep(27.5, 3)
  # #   ),
  # #   Coeff = c(
  # #     rep(NA, 5), rep(0.03, 5), 
  # #     rep(0.03, 2), rep(NA, 3), 
  # #     rep(NA, 2), rep(0.03, 3)
  # #   ),
  # #   CoeffPente = rep(1, 20),
  # #   
  # #   Type = rep(NA, 20)
  # # )
  # 
  # 
  # # ---------- Cas des perches sans mesure de distance
  # pos <- with(
  #   df, 
  #   which(Cat == "PER" & is.na(Dist) & Diam1 >= DiamLim1 & is.na(Type))
  # ) 
  # # Changement Verif_Calculs : rajout de la condition "Diam1 >= DiamLim1"
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / df$Rayon1[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  # 
  # # ---------- Cercles uniques
  # pos <- with(
  #   df, 
  #   which(
  #     is.na(Nha) & is.na(DiamLim2) & Diam1 >= DiamLim1 &
  #       Dist <= Rayon1 * CoeffPente & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / df$Rayon1[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  # # ---------- Cercles concentriques
  # # -- rappel : possible d'excluer les perches car la définition 
  # # de cett catégorie de diamètre est paramétrée dans 
  # # la feuille "Cats"
  # 
  # # 3eme cercle
  # pos <- with(
  #   df, 
  #   which(
  #     is.na(Nha) & !is.na(DiamLim3) & Diam1 >= DiamLim3 &
  #       Dist <= Rayon3 * CoeffPente & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / df$Rayon3[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  # # 2eme cercle avec 3eme cercle
  # pos <- with(
  #   df, 
  #   which(
  #     is.na(Nha) & !is.na(DiamLim3) & Diam1 >= DiamLim2 & Diam1 < DiamLim3 &
  #       Dist <= Rayon2 * CoeffPente & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / df$Rayon2[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  # # 2eme cercle sans 3eme cercle
  # pos <- with(
  #   df, 
  #   which(
  #     is.na(Nha) & is.na(DiamLim3) & Diam1 >= DiamLim2 &
  #       Dist <= Rayon2 * CoeffPente & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / df$Rayon2[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  # # 1er cercle avec 2eme cercle
  # pos <- with(
  #   df, 
  #   which(
  #     is.na(Nha) & !is.na(DiamLim2) & Diam1 >= DiamLim1 &
  #       Diam1 < DiamLim2 & Dist <= Rayon1 * CoeffPente & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / df$Rayon1[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  # 
  # # ---------- Angle fixe
  # # exclut les arbres vivants dont le diamètre est < DiamLim
  # pos <- which(df$Diam1 < df$DiamLim & is.na(df$Type))
  # if (length(pos) > 0) {
  #   df[pos, "Coeff"] <- NA
  # }
  # # réinitialise "Nha" et "Limite" pour les arbres vivants (perches incluses !) à considérer dans l'inventaire à angle fixe 
  # pos <- which(df$Diam1 >= df$DiamLim & is.na(df$Type)) # Changement Verif_Calculs Diam devient Diam1 -> indispensable sinon arbre de 29 par 32 sera inventorié ni par surface ni par angle fixe
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- NA # Remise à zéro au cas où il y aurait déjà des valeurs renseignées (cercle(s))
  #   df[pos, "Limite"] <- 1
  # }
  # pos <- with(
  #   df, 
  #   which(
  #     !is.na(Coeff) & Diam1 >= Dist * Coeff & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10 ^ 4 * df$Coeff[pos] ^ 2 / pi / df$Diam1[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  # # cas des Arbres à inventorier par angle relascopique qui auraient été considérés comme non limite parce
  # # que > DiamLim1, mais qui en fait sont hors inventaire
  # # -- Nov 2022 -> commande inutile ?
  # pos <- with(
  #   df, 
  #   which(
  #     !is.na(Coeff) & Diam1 < Dist * Coeff & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- NA
  #   df$Limite[pos] <- 1
  # }
  # 
  # 
  # 
  # 
  # # -- spécifique inventaire PP La Madeleine 
  # # protocole BMP : 
  # # + 1 cercle de 10m pour les Diam < 30
  # # + 1 cercle de 20m pour les Diam >= 30
  # pos <-
  #   with(
  #     df,
  #     which(
  #       is.na(Nha) & Diam1 >= 30 &
  #         Dist <= 20 & !is.na(Type)
  #     )
  #   )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / 20 ^ 2
  #   df$Limite[pos] <- NA
  #   df$TypeTarif[pos] <- "SchL" # default tarif for BMP
  #   df$NumTarif[pos] <- 6
  # }
  # # 1er cercle avec 2eme cercle pour les BMP
  # pos <-
  #   with(
  #     df,
  #     which(
  #       is.na(Nha) & Diam1 < 30 &
  #         Dist <= 10 & !is.na(Type)
  #     )
  #   )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / 10 ^ 2
  #   df$Limite[pos] <- NA
  #   df$TypeTarif[pos] <- "SchL" # default tarif for BMP
  #   df$NumTarif[pos] <- 6
  # }
  # 
  # # Nha mis à 0 pour les arbres limites
  # pos <- which(df$Limite == 1)
  # if (length(pos) > 0) df[pos, "Nha"] <- 0
  ##### fin Ancienne version
  # tk_messageBox(type = "ok", message = "Attention recaler paramètres d'échantillonnage en attendant modif\n+ cercle 3 ne répond pas")
  df <- df %>% 
    mutate(
      Nha = case_when(
        # -- *** perches ***
        # - cas des perches vivantes... 
        Cat == "PER" & Diam1 >= DiamLim1 & is.na(Type) &
          # ... sans distance
          is.na(Dist) ~ 
          # Nha = 
          10000 / pi / Rayon1 ^ 2,
        
        # - cas des perches vivantes... 
        Cat == "PER" & Diam1 >= DiamLim1 & is.na(Type) &
          # ... avec distance
          !is.na(Dist) & Dist < Rayon1 ~ 
          # Nha = 
          10000 / pi / Rayon1 ^ 2,
        
        
        # -- *** précomtables ***
        # - cas des arbres précomtables vivants ...
        Cat != "PER" & is.na(Type) &
          # --- inventoriés par angle fixe
          Diam1 >= DiamLim & Diam1 >= Dist * Coeff ~
          # Nha =
          10 ^ 4 * Coeff ^ 2 / pi / Diam1 ^ 2,
        
        # - cas des arbres précomptables vivants...
        Cat != "PER" & is.na(Type) &
          # ... inventoriés sur le 3ème cercle (2 autres sous-entendus + exclure angle fixe)
          Diam1 < DiamLim & Diam1 >= DiamLim3 & Dist <= Rayon3 * CoeffPente ~
          # Nha =
          10000 / pi / Rayon3 ^ 2,
        
        # - cas des arbres précomptables vivants...
        Cat != "PER" & is.na(Type) &
          # ... inventoriés sur le 2ème cercle (1er sous-entendu + exclure angle fixe) avec ou sans 3ème cercle ...
          Diam1 < DiamLim & Diam1 >= DiamLim2 & Dist <= Rayon2 * CoeffPente ~ 
          # Nha = 
          10000 / pi / Rayon2 ^ 2,
        
        # - cas des arbres précomptables vivants...
        Cat != "PER" & is.na(Type) &
          # ... inventoriés sur le 1er cercle avec ou sans 2ème cercle (exclure angle fixe)
          Diam1 < DiamLim & Diam1 >= DiamLim1 & Dist / CoeffPente <= Rayon1 ~
          # Nha = 
          10000 / pi / Rayon1 ^ 2,
        
        
        # -- *** bois mort sur pied ***
        # - cas des bois morts sur pied...
        !is.na(Type) & #BMP -> manque argument !!!!
          # >= 30 cm de diamètre
          # ... inventoriés sur le 2ème cercle (1er sous-entendu + exclure angle fixe) avec ou sans 3ème cercle ...
          Diam >= 30 & Dist <= 20 ~
          # Nha =
          10000 / pi / 20 ^ 2,
        
        # !is.na(Type) & 
        #   # >= 30 cm de diamètre
        #   # ... inventoriés par angle fixe
        #   Diam >= 30 & Diam1 >= Dist * Coeff  ~
        #   # Nha =
        #   10 ^ 4 * Coeff ^ 2 / pi / Diam1 ^ 2,
        
        # - cas des bois morts sur pied...
        !is.na(Type) &
          # < 30 cm de diamètre
          Diam >= DiamLim1 & Diam < 30 & Dist <= 10 ~
          # Nha =
          10000 / pi / 10 ^ 2
      ),
      
      # définition attribut limite d'un arbre
      Limite = ifelse(is.na(Nha), 1, 0),
      # arbre limite => Nha = 0
      Nha = ifelse(is.na(Nha), 0, Nha)
    ) # debug %>% select(NumPlac, NumArbre, Essence, Azimut, Dist, Diam1, Diam, Type, Nha, DiamLim, Coeff, BMP, DiamLim1, Rayon1, Limite)
  
  # rm(pos)
  # -- retour fonction calculs_Nha
  return(df)
}

##### fonction de préparation de la table Arbres #####
# TODO : rajouter les autres tables en arguments
prep_df <- function(
  df = NULL, echant_DF = NULL, 
  code_qual = NULL, code_essreg = NULL,
  code_tarif = NULL, code_prix = NULL,
  diam_cat = NULL
) {
  df <-
    df %>%
    # filter(NumPlac == "21_3308") %>% # debug
    left_join(
      code_qual %>% select(-Couleur),
      by = c("NumForet", "Qual" = "Nom")
    ) %>%
    left_join(
      code_essreg %>% select(-Couleur),
      by = c("NumForet", "Essence")
    ) %>%
    mutate(
      Cycle = as.numeric(Cycle), 
      # NumArbre = as.numeric(NumArbre), 
      Limite = 1, 
      Dist = as.numeric(Dist), 
      Azimut = as.numeric(Azimut), 
      Diam1 = as.numeric(Diam1), 
      Diam2 = as.numeric(Diam2), 
      Haut = as.numeric(Haut), 
      Stade = as.numeric(Stade), 
      Diam1 = ifelse(is.na(Diam1), Diam2, Diam1), 
      Diam2 = ifelse(is.na(Diam2), Diam1, Diam2), 
      Diam1 = ifelse(Diam1 == 0, Diam2, Diam1), 
      Diam2 = ifelse(Diam2 == 0, Diam1, Diam2), 
      Diam = (Diam1 + Diam2) / 2, 
      Classe = floor(Diam / 5 + 0.5) * 5, 
      # définition de Cat à partir de Diam et pas Diam1 car on veut que la 
      # population ait un sens au point de vue dendrométrique 
      # (mais attention, calculs toujours fondés sur Diam1)
      Cat = cut(
        Diam, 
        breaks = c(diam_cat$Diam, Inf), 
        labels = diam_cat$Cat, 
        include.lowest = T, 
        right = F
      ), 
      Cat = as.character(Cat), 
      Nha = NA
    ) %>%
    left_join(code_tarif, by = c("NumForet", "Essence", "Strate")) %>%
    left_join(code_prix, by = c("NumForet", "Essence", "Classe", "Reg1" = "Qual")) %>%
    mutate(PU = ifelse(
      Classe < 7.5 & !is.na(Classe), 0, PU
    )) %>%
    arrange(NumForet, NumPlac, NumArbre, Cycle) %>%
    filter(
      !is.na(Diam1) | !is.na(Diam2) | Diam1 == 0 | Diam2 == 0 # supprime les arbres où les 2 diamètres sont vides
    )
  
  # -- retour fonction prep_df
  return(df)
}

##### fonction de calcul du Nha, Gha, Vha #####
calculs_Arbres <- function(
  df = NULL, echant_change = F, 
  code_qual = NULL, code_essreg = NULL,
  code_tarif = NULL, code_prix = NULL,
  diam_cat = NULL
) {
  # df <- Arbres # debug
  # df <- Arbres_Acct # debug
  # echant_DF <- Echantillonnages # debug
  # -- préparation
  df <- 
    df %>% 
    prep_df(
    echant_DF, 
    code_qual = code_qual, code_essreg = code_essreg,
    code_tarif = code_tarif, code_prix = code_prix,
    diam_cat = diam_cat
    )
  
  # -- calcul du poids
  df <- df %>% calculs_Nha()
  
  # -- calcul de la surface terrière
  df$Gha <- pi * df$Diam1 ^ 2 / 40000 * df$Nha # Utiliser Diam1 pour retrouver le coeff relascopique (2 à 2%)
  
  # -- calcul du volume
  # volume gestionnaire
  df <- calculs_Vol(df)
  # volume IFN
  df <- calculs_Vol(df, IFN = T)
  
  # -- calcul des variables économiques
  df <- calculs_Eco(df, code_prix)
  
  if (echant_change == F) {
    # selection de variables
    df <-
      df %>%
      select(
        NumForet, NumPlac, NumArbre, IdArbre, Cycle, Strate, 
        Essence, EssReg, Azimut, Dist, 
        
        NumTarif, TypeTarif, TypeEss, CoefHoupp, TauxCarbone, InfraDensite,
        
        Diam1, Diam2, Diam, Classe, Cat, 
        DiamSup, ClasseSup, VhaSup, TauxV, 
        Qual, Reg1, Reg2, PU, PUSup, 
        Nha, Gha, Vha, VhaIFN, VcHa, 
        Coupe, Limite, 
        CodeEcolo, Ref_CodeEcolo, 
        Type, Haut, Stade, Caract1, Caract2, Caract3, Observations
      )
  } else {
    df <-
      df %>%
      mutate(echant_ID = NA) %>%
      # selection de variables
      select(
        NumForet, NumPlac, NumArbre, IdArbre, Cycle, Strate, 
        Essence, EssReg, Azimut, Dist, 
        
        NumTarif, TypeTarif, TypeEss, CoefHoupp, TauxCarbone, InfraDensite, 
        
        Diam1, Diam2, Diam, Classe, Cat, 
        DiamSup, ClasseSup, VhaSup, TauxV, 
        Qual, Reg1, Reg2, PU, PUSup, 
        Nha, Gha, Vha, VhaIFN, VcHa, 
        Coupe, Limite, echant_ID, 
        CodeEcolo, Ref_CodeEcolo, 
        Type, Haut, Stade, Caract1, Caract2, Caract3, Observations
      )
  }
  
  # -- retour fonction calculs_Arbres
  return(df)
}

##### fonction rajout des arbres - placettes miroir #####
miroir_add <- function(df, PCQM = F) {
  # df <- Arbres # debug
  # df <- Taillis_PCQM # debug
  # plac_df <- Placettes # debug
  miroir_DF <-
    # jonction with plot df needed beforehand
    df %>% filter(!is.na(Miroir_Azimut) & !is.na(Miroir_Dist))
  
  if (dim(miroir_DF)[1] > 0) {
    miroir_DF <-
      miroir_DF %>%
      # mutate(Azimut = c(0, 100, 200, 300), Dist = rep(1, 4), Miroir_Azimut = 300, Miroir_Dist = 1) %>% # debug
      mutate(
        # Azimut = Azimut + 400, 
        
        # - ancienne version avant commande modulo :) !!!
        # theta = 500 - Azimut, 
        # theta = (theta/400 - floor(theta/400)) * 400, # même sens que coordonnées polaires
        theta = (500 - Azimut) %% 400,
        
        # theta = theta * pi / 200, # conversion en radians
        X = Dist * cos(theta * pi / 200), 
        Y = Dist * sin(theta * pi / 200), 
        # Azimut = Azimut + 400, 
        # coordonnées du point miroir
        Miroir_Azimut = 
          ifelse(Miroir_Azimut < 200, Miroir_Azimut + 200, Miroir_Azimut - 200), # inverse la position du centre miroir
        Miroir_theta = 500 - Miroir_Azimut, 
        Miroir_theta = (Miroir_theta/400 - floor(Miroir_theta/400)) * 400, # même sens que coordonnées polaires
        # Miroir_theta = Miroir_theta * pi / 200, # conversion en radians
        Miroir_Dist = Miroir_Dist * 2, 
        c_X = Miroir_Dist * cos(Miroir_theta * pi / 200), 
        c_Y = Miroir_Dist * sin(Miroir_theta * pi / 200), 
        
        X = X + c_X, # coordonnées symétrie du centre placette miroir
        Y = Y + c_Y, 
        
        X = ifelse(abs(X) < 10^(-10), 0, X), 
        Y =  ifelse(abs(Y) < 10^(-10), 0, Y), 
        # Retrouve Azimut et Distance MAJ
        theta = ifelse(X != 0, atan(Y/X) * 200 / pi, 0), 
        Dist = sqrt(X^2 + Y^2), 
        COS = X/Dist, 
        SIN = Y/Dist, 
        # Dist = round(Dist, digits = 1), 
        
        Azimut = ifelse(COS < 0 & theta != 0, 300 - theta, 0), 
        Azimut = ifelse(COS > 0 & theta != 0, 100 - theta, Azimut), 
        Azimut = ifelse(COS > 0 & Y == 0, 100, Azimut)
      ) %>%
      # suppression des variables
      select(names(df))
    
    df <- rbind(df, miroir_DF) # Rajouter 400 grd aux azimuts ?
    # Redéfinir les éléments des 4 quarts pour le PCQM
    if (PCQM == T) {
      df <-
        df %>%
        mutate(
          Quart = cut(
            Azimut, 
            breaks = c(0, 100, 200, 300, 400), 
            labels = c("NE", "SE", "SO", "NO"), 
            include.lowest = T, 
            right = T
          )
        ) %>%
        group_by(NumForet, NumPlac, Population, Quart) %>%
        filter(Dist == min(Dist)) %>% 
        ungroup()
    }
  }
  
  # -- retour fonction miroir_add
  return(df)
}

##### fonction gf_Calculs #####
# = Calcul des variables d'analyse (dendrométrique, écologique et économique) à
# l'échelle de chaque élément d'inventaire
gf_Calculs <- function(
  wd = NULL, # TODO : inutile à l'avenir. Gardé par sécurité mais à supprimer
  output_dir = NULL,
  TauxR = 0.03, 
  forest = NULL,
  last_cycle = NULL,
  # arch1 = "tables/gfDonneesBrutes.Rdata", 
  complete_progress = NULL, # shiny related
  i18n = NULL
) {
  ##### 1/ Initialisation #####
  # -- création du dossier de sortie
  # output_dir <- file.path(output_dir, "tables")
  # output_dir <- file.path("out", disp, i18n()$t("livret_AFI"))
  dir.create(file.path(output_dir, "tables"), showWarnings = F, recursive = T)
  
  # -- chargement des données d'inventaire et administratives
  inventory_tables <- load(file.path(wd, "tables/gfDonneesBrutes.Rdata"))
  # -- chargement du dictionnaire de traduction
  load(file.path(wd, "tables/gf_Dictionary.Rdata"))
  
  # incrémentation de la barre de progression
  detail = i18n()$t("Chargement des données d'inventaire")
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  print(detail)
  
  # # -- sélection des données propres au(x) dispositif(s) choisi(s) : Utilisation en cas d'analyse transversale - à tester
  # if (repSav == wd) {
  #   # -- choix du dispositif
  #   # initialisation
  #   check_all_msg <- "Editer les r\u00E9sultats pour tous les dispositifs"
  #   forest_list <- choose_forest(tables, Forets, check_all_msg)
  # } else {
    forest_list <- forest
  # }
  # last_cycle <- get_last_cycle(tables, forest_list)
  
    # -- filtre des tables d'inventaire en fonction des numéros de dispositif sélectionnés
    # inventory_tables <- c(
    #   "IdArbres", "BMortSup30","BMortLineaires", "Cycles", #"Placettes",
    #   "Reges", "Coords", "Taillis"
    # )
    
    
    # tables with "NumForet" -> à filtrer
    tables_to_filter <- c()
    for (table in inventory_tables) {
      if ("NumForet" %in% names(get( table ))) {
        tables_to_filter <- c(tables_to_filter, table)
      }
    }
    filter_by_forest(tables = tables_to_filter, forest_list = forest_list, cycle = last_cycle)
    
  
  ##### 2/ Calculs des variables de la table Arbres ----- #####
    # id variables
    id_vars <- c("NumForet", "NumPlac", "Cycle", "Strate")
  # -- rajout de la colonne TauxCarbone à la table Essences 
  # load("/Users/Valentin/Travail/Outils/Inventaire_PP/tables/afi_species_table.Rdata")
  # Essences <- Essences %>% left_join(afi_species_table[, c("Essence", "TauxCarbone")], by = c("Nom" = "Essence"))
  
  # -- set up
  echant_DF <- Echantillonnages
  Arbres <-
    IdArbres %>%
    left_join(ValArbres, by = c("IdArbre" = "IdArbre")) %>%
    # filter(NumPlac == 102) %>%
    # filter(!is.na(Essence) & !is.na(Azimut) & !is.na(Dist)) %>%
    # et les éléments de bois mort ou non repérés ????
    # filtre ci-dessus important. Sinon pblme dcast accroissement
    
    # jonction avec la table des essences - récupération des informations carbone (TypeEss, CoefHoupp, TauxCarbone, InfraDensite)
    left_join(Essences, by = c("NumForet", "Essence" = "Nom")) %>%
    
    # -- jonction avec la table 'Placettes' -> récupération des variables Strate, CoeffPente, Miroir_Azimut, Miroir_Dist, 
    # initialement, on récupérait : PoidsPlacette (inutile avant le job5), Pente (à préciser avec travail pour Evrard)
    left_join(
      Placettes %>% select(c("NumForet", "NumPlac", "Cycle", "Strate", "CoeffPente", "Miroir_Azimut", "Miroir_Dist")), 
      by = c("NumForet", "NumPlac", "Cycle")
    ) %>%
    # sort names
    select(all_of(id_vars), everything()) %>% 
    
    # -- jonction avec la table Echantillonnages
    left_join(
      echant_DF[, c(
        "NumForet", "Cycle", "Strate", "Surface", "NbPlac", 
        "DiamLim1", "Rayon1", "DiamLim2", "Rayon2", "DiamLim3", "Rayon3", 
        "Coeff", "DiamLim", 

        # "BMP_DiamLim1", "BMP_Rayon1", # TODO : à revoir
        # "BMP_DiamLim2", "BMP_Rayon2", 
        "BMP"
      )], 
      by = c(
        "NumForet" = "NumForet", 
        "Cycle" = "Cycle", 
        "Strate" = "Strate"
      )
    ) %>%
    # correction
    mutate(Coeff = Coeff * 100) # correction nécessaire car 28 >= 14 * 0.02 * 100 est faux (tests B.Meheux)
    
      # -- placettes miroir
  if (length( with(Arbres, which( !is.na(Miroir_Azimut) | !is.na(Miroir_Dist) )) ) > 0) {
    # rajout des arbres miroirs pour les placettes miroirs (Azimut_Mirroir et Dist_Mirroir nécessaires)
    Arbres <- Arbres %>% miroir_add()
    
    # renuméroter arbres à cause du rajout des arbres miroirs
      # -- define id_columns
    id_columns <- 
      c("NumForet", "NumPlac", "NumArbre", "Essence", "Azimut", "Dist")
    
    db_tables <- 
      set_db_id(table = Arbres, id_columns = id_columns, id_var = "IdArbre")
    IdArbres <- db_tables$id_table # add table 'IdArbres'
    ValArbres <- db_tables$value_table
    
    Arbres <-
      IdArbres %>%
      left_join(ValArbres, by = c("IdArbre" = "IdArbre"))
  }
  
  ##### 2.1/ calculs des variables stationnaires #####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  code_qual = Quals
  code_essreg = EssReg
  code_tarif = Tarifs
  code_prix = Prix
  diam_cat = Cats
  
  Arbres <- 
    calculs_Arbres(
      Arbres, 
      code_qual = Quals, code_essreg = EssReg, # TODO : faire une liste avec toutes les codifications
      code_tarif = Tarifs, code_prix = Prix,
      diam_cat = Cats
    ) %>% 
    
    # récupération des regroupements d'essences
    find_ESSREG(code_essences = Essences)

  
  ##### 2.2/ calcul des accroissements (variables dynamiques) #####
  # perches + autres tiges sans Az ni Dist mis à l'écart
  # (bloquant pour les calculs d'accroissement - perche promue apparaîtrait coupée)
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  # -- tri des populations
  # distinction des perches 
  # on utilise Diam et pas Diam1 comme distinction : cela permet d'éviter les classes 5 ou 20 dans les figures des perches)
  stem_threshold_min <- Cats$Diam[Cats$Cat == "PER"]
  stem_threshold_max <- Cats$Diam[Cats$Cat == "PB"] # 17.5 usuellement, 12.5 pour Audenge
  Perches <- 
    Arbres %>% filter(Diam >= stem_threshold_min & Diam < stem_threshold_max) # 12.5 pour Audenge
  # BMP contenus dans les perches :
  BMP <- Perches %>% filter(!is.na(Type)) %>% select(
    NumForet, NumPlac, Cycle, Dist,
    Essence, EssReg, 
    Diam, Classe, Cat, 
    Haut, Type, Limite,
    Stade,
    Nha, Gha, Vha
  )
  Perches <- Perches %>% filter(is.na(Type))
  
  # arbres non repérés
  Arbres_nonRep <- Arbres %>% filter((is.na(Essence) | is.na(Azimut) | is.na(Dist)) & Diam >= stem_threshold_max) # 17.5 usuellement, 12.5 pour Audenge
  
  # arbres repérés
  Arbres <-
    Arbres %>% filter(!is.na(Essence) & !is.na(Azimut) & !is.na(Dist) & Diam >= stem_threshold_max) # 17.5 usuellement, 12.5 pour Audenge

  
  if (max(Arbres$Cycle) > 1) {
    # -- calculs d'accroissement
    # test = acct_tables %>% filter(NumPlac == 1) %>% select(c(1,2,3,5,7,9,10,17,18,19,20,31,32,33,46:50)) # debug
    acct_tables <- 
      Arbres %>% 
      check_duplicated_trees() %>% 
      calculate_increments(
        cycles_table = Cycles, 
        plot_table = Placettes
      )
    
    # -- sauvegarde table des accroissements en diamètre
    AcctD <- 
      acct_tables %>% 
      select(
        IdArbre, NumForet, NumPlac, NumArbre, Cycle, 
        Essence, Classe, Acct_Diam
      )
    
    Arbres <- share_accD(acct_tables)
    # tk_messageBox(type = "ok", message = "rajouter Num dans Essence + contrôle sur les prix + vérifier calculs accroissement pour changement de protocole")
    
    # -- rajout de colonnes vides dans Arbres_nonRep
    Arbres_nonRep <-
      Arbres_nonRep %>%
      # TODO faire une fonction pour aligner les tables automatiquement
      mutate(
        AcctGper = NA, AcctVper = NA, Gainper = NA, AcctD = NA, time_span = NA
      )
    # rajout de colonnes vides dans Perches
    Perches <-
      Perches %>%
      mutate(
        AcctGper = NA, AcctVper = NA, Gainper = NA, AcctD = NA, time_span = NA
      )

    # -- gestion des cas où changement de protocole
    # extrait de la table Echantillonnages des cycles concernés par
    # le changement de protocole (s'il existe)
    echant_DF <- Echantillonnages
    echant_DF <- change_protocole(echant_DF) # TODO : à revoir

    if (length(unique(echant_DF$echant_ID)) > 1) {
      # Recalculs des valeurs à l'hectare selon les paramètres stables dans le temps.
      Arbres_Acct <-
        left_join(
          IdArbres, ValArbres, by = "IdArbre"
        ) %>%
        filter(
          !is.na(Essence) & !is.na(Azimut) & !is.na(Dist)
        ) %>% # on ne prend que les arbres repérés
        
        # jonction avec la table des essences - récupération des informations carbone (TypeEss, CoefHoupp, TauxCarbone, InfraDensite)
        left_join(Essences, by = c("NumForet", "Essence" = "Nom")) %>%
        
        left_join(
          Placettes[, c(
            "NumForet", "NumPlac", "Cycle", "Strate", "PoidsPlacette", 
            "Pente", "CoeffPente", "Parcelle", "Station"
          )], 
          by = c("NumForet", "NumPlac", "Cycle")
        ) %>%
        mutate(
          Qual = as.character(Qual), 
          Coupe = as.character(Coupe), 
          echant_ID = paste0(NumForet, "-", Cycle, "-", Strate)
        ) %>%
        # on ne sélectionne que les arbres concernés par le changement de protocole
        right_join(
          echant_DF[, c(
            "NumForet", "Cycle", "Strate", 
            "Surface", "NbPlac", 
            "DiamLim1", "Rayon1", 
            "DiamLim2", "Rayon2", 
            "DiamLim3", "Rayon3", 
            "Coeff", "DiamLim", 
            "echant_ID"
          )], 
          by = c("NumForet", "Cycle", "Strate", "echant_ID" = "echant_ID")
        ) %>% 
        mutate(Coeff = Coeff * 100)
      
      # Calculs d'accroissement avec le nouveau protocole
      Arbres_Acct <- calculs_Arbres(
        Arbres_Acct, 
        echant_change = T,
        code_qual = Quals, code_essreg = EssReg,
        code_tarif = Tarifs, code_prix = Prix,
        diam_cat = Cats
      ) %>% 
        
        # -- tri : ne tenir compte que des arbres repérés Azimut/Distance
        # arbres repérés
        filter(!is.na(Essence) & !is.na(Azimut) & !is.na(Dist) & Diam >= stem_threshold_max) # 17.5 usuellement, 12.5 pour Audenge

      # repérage des cycles concernés par le changement de protocole
      cycle_INI <- min(echant_DF$Cycle)
      cycle_FIN <- max(echant_DF$Cycle)
      # recalcul des accroissements
      Arbres_Acct <- 
        # calculs_Acct(
        Arbres_Acct %>%  #, 
      #   cycles_table = Cycles, 
      #   echant_change = T
      # ) %>% 
        calculate_increments(
          cycles_table = Cycles, 
          plot_table = Placettes
        )
      
      # fusion de l'ancienne tables arbres avec la table Arbres_Acct
      Arbres <-
        full_join(
          Arbres, 
          Arbres_Acct, 
          by = c(
            "NumForet", "NumPlac", "NumArbre", "Strate", "IdArbre", "Cycle"
          ), 
          suffix = c("", "_changed")
        ) %>%
        select(
          NumForet, NumPlac, NumArbre, IdArbre, Cycle, Strate, 
          Essence, EssReg, Azimut, Dist, 
          Diam1, Diam2, Diam, Classe, Cat, 
          DiamSup, ClasseSup, VhaSup, TauxV, 
          Qual, Reg1, Reg2, PU, PUSup, 
          Nha, Gha, Vha, VhaIFN, VcHa, 
          Coupe, Limite, 
          CodeEcolo, Ref_CodeEcolo, 
          Type, Haut, Stade, Caract1, Caract2, Caract3, Observations, 

          NumTarif, TypeTarif, TypeEss, CoefHoupp, TauxCarbone, InfraDensite,
          
          AcctGper, AcctGper_changed, 
          AcctVper, AcctVper_changed, 
          Gainper, Gainper_changed, 
          AcctD, #AcctD_changed, # on garde quoi qu'il arrive AcctD initial car contient plus d'infos
          Coupe, Coupe_changed, 

          time_span, echant_ID
        ) %>%
        # si le cycle est concerné par le changement de protocole alors on prend
        # les valeurs d'accroissement 'changed'
        mutate(
          AcctGper = 
            ifelse(
              Cycle %in% c(cycle_INI, cycle_FIN), AcctGper_changed, AcctGper
            ), 
          AcctVper = 
            ifelse(
              Cycle %in% c(cycle_INI, cycle_FIN), AcctVper_changed, AcctVper
            ), 
          Gainper = 
            ifelse(
              Cycle %in% c(cycle_INI, cycle_FIN), Gainper_changed, Gainper
            )
        ) %>%
        group_by(NumForet, NumPlac, NumArbre, IdArbre, Cycle, Strate) %>%
        # cas de la colonne Coupe :
        # comme seulement 2 cycles sont concernés dans un changement de protocole.
        # CAS 1 : cycle_INI = 1
        # --- 1/ Cycle == cyle_INI( = 1) il ne peut pas y avoir de notation PF, 
        # PF/E ou PF/C (il faudrait pour cela que cycle_INI soit > 1)
        # = > Donc pour ne pas manquer les bons Coupe_changed = "E" ou "C", je dois
        # récupérer la notation Coupe_changed
        mutate(
          Coupe_temp = 
            ifelse(
              Cycle == cycle_INI, Coupe_changed, Coupe # on récupère les notations "E" ou "C"
            ), 
          Coupe = Coupe_temp
        ) %>%
        ungroup() %>%
        mutate(
          AcctGper_changed = NULL, 
          AcctVper_changed = NULL, 
          Gainper_changed = NULL, 
          Coupe_changed = NULL, 
          Coupe_temp = NULL
        ) %>%
        as.data.frame()
      #        # --- 2/ Cycle = cycle_FIN( = 2) il n'y a pas de "E" ou "C" dans Arbres_Acct.
      #        # Mais l'arbre peut avoir été coupé au tour suivant.
      #        # je dois donc récupérer la notation "E" ou "C" de la colonne Coupe et, 
      #        # si nécessaire, l'associer à la notation "PF" éventuellement contenue
      #        # dans Arbres_Acct

      Arbres <- Arbres
      # rajout de colonnes vides dans Arbres_nonRep
      Arbres_nonRep <-
        Arbres_nonRep %>%
        # faire une fonction pour aligner les tables automatiquement
        mutate(echant_ID = NA)
      Perches <-
        Perches %>%
        mutate(echant_ID = NA)

    }
  } else {
    Arbres <-
      left_join(
        Arbres, AcctD, 
        by = c("NumForet", "Strate", "Essence", "Classe", "Cycle") #, "Cycle"
      ) %>%
      mutate(
        AcctD = AccD, 
        echant_ID = NA, 
        # rajout des colonnes d'accroissement vides
        AcctGper = NA, 
        AcctVper = NA, 
        Gainper = NA
      )

    # rajout de colonnes vides dans Arbres_nonRep
    Arbres_nonRep <-
      Arbres_nonRep %>%
      # TODO faire une fonction pour aligner les tables automatiquement
      mutate(
        AccD = NA, 
        AcctD = NA, 
        echant_ID = NA, 
        # rajout des colonnes d'accroissement vides
        AcctGper = NA, 
        AcctVper = NA, 
        Gainper = NA
      )
    # rajout de colonnes vides dans Perches
    Perches <-
      Perches %>%
      # faire une fonction pour aligner les tables automatiquement
      mutate(
        AccD = NA, 
        AcctD = NA, 
        echant_ID = NA, 
        # rajout des colonnes d'accroissement vides
        AcctGper = NA, 
        AcctVper = NA, 
        Gainper = NA
      )
  } # end of cond if (max(Arbres$Cycle) > 1)
  # A voir si on garde cette partie ci-dessous ?
  # if (dim(AcctD)[1] > 0)  {
  #   pos <- which(is.na(Arbres$AcctD))
  #   if (length(pos) > 0) {
  #     Arbres$AcctD[pos] <- 0.3
  #   }
  # }
  ##### / \ #####

  ##### 2.3/ Fin calculs onglet arbres #####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  Arbres <-
    Arbres %>%
    rbind(Arbres_nonRep) %>%
    # rbind(Perches) %>%
    mutate(
      TauxPU = log(PUSup / PU) / 5, 
      Taux = (TauxPU + TauxV) * AcctD, 
      AcctV = TauxV * Vha * AcctD, 
      Gain = Taux * VcHa, 
      VpHa = Gain / TauxR, 
      AcctG = pi / 20000 * AcctD * Diam * Nha
    ) %>%
    arrange(NumForet, NumPlac, NumArbre, Cycle) #%>%
    # Rajout de la moitié des accroissements pour les arbres coupés
    # group_by(NumForet, NumPlac, NumArbre) %>%
    # mutate(
    #   AcctD = ifelse(
    #     !is.na(lag(Coupe)) &
    #       (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")), 
    #     AcctD/2, AcctD
    #     ), 
    #   AcctG =  ifelse(
    #     is.na(AcctG) &
    #       (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")), 
    #     lag(AcctG)/2, AcctG
    #   ), 
    #   AcctGper =  ifelse(
    #     is.na(AcctGper) &
    #       (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")), 
    #     lag(AcctG)/2, AcctGper
    #   ), 
    #   AcctV =  ifelse(
    #     is.na(AcctV) &
    #       (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")), 
    #     lag(AcctV)/2, AcctV
    #   ), 
    #   AcctVper =  ifelse(
    #     is.na(AcctVper) & (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")), 
    #     lag(AcctV)/2, AcctVper
    #   ), 
    #   Gain =  ifelse(
    #     is.na(Gain) & (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")), 
    #     lag(Gain)/2, Gain
    #   ), 
    #   Gainper =  ifelse(
    #     is.na(Gainper) & (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")), 
    #     lag(Gain)/2, Gainper
    #   )
    # ) %>%
    # ungroup()

  # -- séparation des populations arbres vivants et arbres morts sur pied/taillis
  tAutres <- filter(Arbres, !is.na(Type))
  Arbres <- filter(Arbres, is.na(Type))
  
  # -- cleaning Arbres table
  # define attributes (caract1/2/3) from 'Listes' table
  attributes_to_add <-
    Listes %>%
    filter(Liste == "ListeCaract") %>%
    distinct() %>%  # sécurité - à voir si utile
    filter(!is.na(Attribut) & !is.na(Descriptif_Attribut)) %>%
    select(Attribut) %>%
    unlist() %>%
    unname()
  
  # TODO : faire une liste avec tous les id_vars selon les tables, 
  # toutes les variables d'attribut selon les tables
  # variables d'identifiant
  id_vars <- c(
    "NumForet", "NumPlac", "NumArbre", "Cycle", "Coupe", 
    "IdArbre", "Azimut", "Dist", "Observations"
  )
  # variables d'attribut
  attribute_vars <- c(
    "Essence", "EssReg", "Qual", attributes_to_add, 
    "Type", "Reg1", "Reg2", "Haut", "Stade", "Limite",
    "Diam1", "Diam2", "Diam", "Classe", "Cat", 
    "CodeEcolo", "Ref_CodeEcolo", "PU"
  )
  # variables des attributs carbone (nécessaires au calcul carbone)
  carbon_attributes_vars <- c(
    "TypeTarif", "NumTarif", "TypeEss", "CoefHoupp", "TauxCarbone", "InfraDensite"
    )
  # variables de résultat
  results_vars <- c(
    "Nha", "Gha", "Vha", "VhaIFN", "VcHa", "VpHa", 
    "Gain", "Gainper", "AcctV", "AcctVper", "AcctG", "AcctGper", 
    "Taux", "TauxPU", "TauxV", "AcctD"
  )
  # if (max(Arbres$Cycle) == 1) { # use last_cycle - tester plusieurs forêts en même temps
  #   results_vars <- 
  #     setdiff(results_vars, c("Gainper", "AcctVper", "AcctGper"))
  # } -> NON : mettre les colonnes des variables dynamiques, même si vides.
  
  # selection vars
  vars_to_select <- c(
    id_vars, carbon_attributes_vars, attribute_vars, results_vars
  )
  # select
  Arbres <- Arbres %>% select( any_of(vars_to_select) )
  
  if (nrow(tAutres) > 0) {
    posBMP <- which(is.element(tAutres$Type, c("A", "C", "S")))
    posTaillis <- which(is.element(tAutres$Type, "Taillis"))
    
    if (length(posBMP) > 0) {
      BMP <- rbind(
        BMP,
        tAutres[posBMP, ] %>% select( all_of(names(BMP)) )
      ) %>%
        mutate(
          StadeE = floor(Stade / 10), 
          StadeD = Stade - StadeE * 10, 
          EssReg = as.character(EssReg), 
          Vha = NA
        ) %>%
        select(
          NumForet, NumPlac, Cycle, Dist,
          Essence, EssReg, 
          Diam, Classe, Cat, 
          Haut, Type, Limite,
          Stade, StadeD, StadeE,
          Nha, Gha, Vha
        ) %>%
        find_ESSREG(code_essences = Essences)
    } else {
      BMP <- 
        BMP %>% 
        select(
          NumForet, NumPlac, Cycle, Dist,
          Essence, EssReg, 
          Diam, Classe, Cat, 
          Haut, Type, Limite,
          Stade,
          Nha, Gha, Vha
        )
    }
    if (length(posTaillis) > 0) {
      Taillis <-
        tAutres[posTaillis, ] %>%
        select(
          NumForet, NumPlac, Cycle, 
          # Quart, 
          Essence, EssReg, 
          Diam, Classe, Cat, 
          Nha, Gha, Vha
        ) %>%
        find_ESSREG(code_essences = Essences)
    } else {
      Taillis <-
        data.frame(
          NumForet = numeric(), 
          NumPlac = character(), 
          Cycle = numeric(), 
          # Quart = integer(), 
          Essence = character(), 
          EssReg = character(), 
          Diam = numeric(), 
          Classe = numeric(), 
          Cat = character(), 
          Nha = numeric(), 
          Gha = numeric(), 
          Vha = numeric(), 
          stringsAsFactors = F
        )
    }
  } else {
    BMP <-
      BMP %>% 
      select(
        NumForet, NumPlac, Cycle, Dist,
        Essence, EssReg, 
        Diam, Classe, Cat, 
        Haut, Type, Limite,
        Stade,
        Nha, Gha, Vha
      )
    Taillis <-
      data.frame(
        NumForet = numeric(), 
        NumPlac = character(), 
        Cycle = numeric(), 
        # Quart = integer(), 
        Essence = character(), 
        EssReg = character(), 
        Diam = numeric(), 
        Classe = numeric(), 
        Cat = character(), 
        Nha = numeric(), 
        Gha = numeric(), 
        Vha = numeric(), 
        stringsAsFactors = F
      )
  }

  
  
  # -- calcul du stock de carbone dans les arbres vivants
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
  # table de simulation des hauteurs de grume (à partir des volumes grume + commerciaux + tiges)
  # height_by_class_and_NumTarif <- 
  #   Arbres %>% 
  #   select(Essence, Classe, Vha, Nha) %>% 
  #   mutate(Haut = Vha / Nha *)
  # -> pas possible si pas de DiamMed
  
  # -- chargement table algan
  load(paste0(wd, "/tables/algan_table.Rdata"))
  
  # table principale
  living_trees_carbon_splitted_by_log <- 
    Arbres %>% 
    # exclusion des arbres limites (calculs inutiles)
    filter(is.na(Limite)) %>% 
    # calcul des tC/ha et durées de vie des billons des arbres sur pied
    calculs_Carbone(
      height_by_class_and_NumTarif = Algan, 
      DecroissanceRx = DecroissanceRx, 
      yield_table = yield_table,
      DureeVieD = DureeVieD, 
      DureeVieConnexes = DureeVieConnexes
    ) 
  
  # carbone total par arbre
  living_trees_carbon <- 
    living_trees_carbon_splitted_by_log %>% 
    group_by(IdArbre) %>% 
    summarise(tCha = sum(tCha, na.rm =T)) %>% 
    ungroup()
  
  Arbres <- 
    Arbres %>%
    # jonction de la table living_trees_carbon
    left_join(
      living_trees_carbon,
      by = "IdArbre"
    ) %>% 
    select(-all_of(carbon_attributes_vars))
  #####/\#####

  #### 3/ Régénération ####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  if (nrow(Reges) > 0) {
    # -- calcul densités à l'hectare (poids)
    Reges <-
      Reges %>% 
      set_up_calcul_tables(
        plot_df = Placettes,
        settings_df = Echantillonnages
        ) %>% 
      mutate(
        Recouv = as.numeric(Recouv), 
        Class1 = as.numeric(Class1), 
        Class2 = as.numeric(Class2), 
        Class3 = as.numeric(Class3),
        
        #  initiate EssValor column # TODO : to suppress ?
        EssValor = NA
      ) %>%
      calculs_rege(EssReg)
     
    # -- prise en compte EssInd
    for (i in 1:nrow(Forets)) {
      pos_EssInd <- which(EssInd$NumForet == Forets$NumForet[i])
      if (length(pos_EssInd) > 0) {
        EssEnTour <-
          EssInd$Essence[ EssInd$NumForet == Forets$NumForet[i] ]
        pos <-
          which(
            Reges$NumForet == Forets$NumForet[i] & Reges$Essence %in% EssEnTour
          )
        Reges$EssValor[pos] <- 1
      } else {
        Reges$EssValor <- 1
      }
    }
    Reges <-
      Reges %>%
      mutate(
        Surf = ifelse(Class1 + Class2 + Class3 >= 5, 1, 0), 
        Surf = Surf / NbPlac
      )
    SurfRege <-
      Reges %>%
      group_by(
        NumForet, NumPlac, Cycle, SsPlac, Essence
      ) %>%
      mutate(
        Surf = sum(Surf, na.rm = T), 
        SurfEssValor = sum(Surf * EssValor, na.rm = T)
      ) %>%
      ungroup()
    Reges <-
      Reges %>%
      select(
        NumForet, NumPlac, Cycle, NumPlac, SsPlac, 
        Parcelle, Station, #Groupe, Typologie, Groupe1
        Essence, EssReg, EssValor, 
        Recouv, Class1, Class2, Class3, 
        Rejet, Abroutis, 
        Classe1Ha, Classe2Ha, Classe3Ha
      )
  } else {
    Reges <-
      data.frame(
        NumForet = integer(), 
        Strate = integer(), 
        Cycle = integer(), 
        NumPlac = character(), 
        SsPlac = character(), 
        Parcelle = character(), 
        Groupe = character(), 
        Typologie = character(), 
        Groupe1 = character(), 
        Station = character(), 
        Essence = character(), 
        EssReg = character(), 
        Essvalor = character(), 
        Recouv = numeric(), 
        Class1 = numeric(), 
        Class2 = numeric(), 
        Class3 = numeric(), 
        Rejet = numeric(), 
        Abroutis = numeric(), 
        Classe1Ha = numeric(), 
        Classe2Ha = numeric(), 
        Classe3Ha = numeric(),
        stringsAsFactors = F
      )
  }
  
  #### 4/ Traitement PCQM ####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  if (nrow(PCQM) > 0) {
    # -- paramètres et correction
    PCQM <- 
      PCQM %>% 
      set_up_calcul_tables(
        plot_df = Placettes,
        settings_df = Echantillonnages
      )
      

    # -- calculs
    # population de Taillis
    if (length(which(PCQM$Population == "Taillis")) > 0) {
      # table PCQM
      Taillis_PCQM <- PCQM %>% filter(Population == "Taillis") %>% miroir_add(PCQM = T)

      # table correction des quarts vides
      Corr <-
        data.frame(
          Coeff = table(Taillis_PCQM$NumForet, Taillis_PCQM$Cycle, Taillis_PCQM$NumPlac)
        ) %>%
        dplyr::rename(
          NumForet = Coeff.Var1, 
          Cycle = Coeff.Var2, 
          NumPlac = Coeff.Var3, 
          Nbre = Coeff.Freq
        ) %>%
        mutate(
          NumForet = as.numeric(as.character(NumForet)), 
          NumPlac = as.character(NumPlac), 
          Cycle = as.numeric(as.character(Cycle)), 
          Nbre = as.numeric(Nbre), 
          Vides = 4 - Nbre, 
          Surf = Vides * quantile(Taillis_PCQM$Dist, probs = 0.95) ^ 2
        )

      # table PCQM suite
      Taillis_PCQM <-
        Taillis_PCQM %>%
        mutate(Taillis = ifelse(is.na(Taillis), 25, Taillis)) %>% # valeur par défaut
        filter(Dist <= Taillis) %>%
        left_join(
          Corr[, c("NumForet", "NumPlac", "Cycle", "Surf")], 
          by = c("NumForet", "NumPlac", "Cycle")
        ) %>%
        group_by(NumForet, NumPlac, Cycle) %>%
        mutate(
          Nha = sum(Dist ^ 2, na.rm = T), 
          Nha = Nha + Surf, 
          Nha = 10000 * 3 / pi / Nha
        ) %>%
        ungroup() %>%
        mutate(
          # Nha = Nha * Coeffts, 
          Gha = pi / 40000 * Diam ^ 2 * Nha, 
          Classe = floor(Diam / 5 + 0.5) * 5, 
          Cat = cut(
            Diam, 
            breaks = c(Cats$Diam, Inf), 
            labels = Cats$Cat, 
            include.lowest = T, right = F
          ), 
          Cat = as.character(Cat), 
          Vha = Gha * 7
        ) %>%
        left_join(EssReg, by = c("NumForet", "Essence")) %>%
        select(
          NumForet, NumPlac, Cycle, #Quart, 
          Essence, EssReg, 
          Diam, Classe, Cat, 
          Nha, Gha, Vha
        ) %>%
        find_ESSREG(code_essences = Essences)
    }
    
    # population de BMP
    if (length(which(PCQM$Population == "BMP")) > 0) {
      # table PCQM
      BMP_PCQM <- PCQM %>% filter(Population == "BMP") %>% miroir_add(PCQM = T)

      # table correction des quarts vides
      Corr <-
        data.frame(
          Coeff = table(BMP_PCQM$NumForet, BMP_PCQM$Cycle, BMP_PCQM$NumPlac)
        ) %>%
        dplyr::rename(
          NumForet = Coeff.Var1, 
          Cycle = Coeff.Var2, 
          NumPlac = Coeff.Var3, 
          Nbre = Coeff.Freq
        ) %>%
        mutate(
          NumForet = as.numeric(as.character(NumForet)), 
          NumPlac = as.character(NumPlac), 
          Cycle = as.numeric(as.character(Cycle)), 
          Nbre = as.numeric(Nbre), 
          Vides = 4 - Nbre, 
          Surf = Vides * quantile(BMP_PCQM$Dist, probs = 0.95) ^ 2
        )

      # table PCQM suite
      BMP_PCQM <-
        BMP_PCQM %>% # bug si texte dans le champs
        mutate(BMP = ifelse(is.na(BMP), 25, BMP)) %>% # valeurs par d\u00E9faut
        filter(Dist <= BMP) %>%
        left_join(
          Corr[, c("NumForet", "NumPlac", "Cycle", "Surf")], 
          by = c("NumForet", "NumPlac", "Cycle")
        ) %>%
        group_by(NumForet, NumPlac, Cycle) %>%
        mutate(
          Nha = sum(Dist ^ 2, na.rm = T), 
          Nha = Nha + Surf, 
          Nha = 10000*3 / pi / Nha
        ) %>%
        ungroup() %>%
        mutate(
          # Nha = Nha * Coeffts, 
          Gha = pi / 40000 * Diam ^ 2 * Nha, 
          Classe = floor(Diam / 5 + 0.5) * 5, 
          Cat = cut(
            Diam, 
            breaks = c(Cats$Diam, Inf), 
            labels = Cats$Cat, 
            include.lowest = T, right = F
          ), 
          Cat = as.character(Cat), 
          StadeE = floor(Stade / 10), 
          StadeD = Stade - StadeE * 10, 
          Vha = NA
        ) %>%
        left_join(EssReg, by = c("NumForet", "Essence")) %>%
        mutate(EssReg = as.character(EssReg), Limite = NA) %>%
        find_ESSREG(code_essences = Essences)

      BMP_PCQM <-
        BMP_PCQM %>%
        select(
          NumForet, NumPlac, Cycle, Dist,
          Essence, EssReg, 
          Diam, Classe, Cat, 
          Haut, Type, Limite,
          Stade,
          Nha, Gha, Vha
        )
    }
  }


  #### 5/ Traitement Cercles ####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  # -- set up
  Cercles <- 
    Cercles %>% 
    set_up_calcul_tables(
      plot_df = Placettes,
      settings_df = Echantillonnages
    )
  
  if (nrow(Cercles) > 0) {
  # -- calcul
  # population de Taillis
  Taillis_Cercles <- calculs_cercles(Cercles, "Taillis", 10, code_essreg = EssReg, diam_cat = Cats)
  # population de BMP
  BMP_Cercles <- 
    calculs_cercles(
      df = Cercles, 
      population = "BMP", 
      dist_max = 20, # valeur par défaut si non précisée dans la table Echantillonnages
      code_essreg = EssReg, 
      diam_cat = Cats,
      add_bmp_vars = T
    ) %>% 
    mutate(Dist = NA, Limite = NA)
  }

  ##### 6/ Rassemblement des population Taillis/BMP (inventaires PCQM et/ou Cercles)
  # -- Taillis
  if (!is.element("Taillis_PCQM", ls())) {
    Taillis_PCQM <-
      data.frame(
        NumForet = numeric(), 
        NumPlac = character(), 
        Cycle = numeric(), 
        # Quart = integer(), 
        Essence = character(), 
        EssReg = character(), 
        Diam = numeric(), 
        Classe = numeric(), 
        Cat = character(), 
        Nha = numeric(), 
        Gha = numeric(), 
        Vha = numeric(), 
        stringsAsFactors = F
      )
  }
  if (!is.element("Taillis_Cercles", ls())) {
    Taillis_Cercles <-
      data.frame(
        NumForet = numeric(), 
        NumPlac = character(), 
        Cycle = numeric(), 
        # Quart = integer(), 
        Essence = character(), 
        EssReg = character(), 
        Diam = numeric(), 
        Classe = numeric(), 
        Cat = character(), 
        Nha = numeric(), 
        Gha = numeric(), 
        Vha = numeric(), 
        stringsAsFactors = F
      )
  }
  # -- BMP
  if (!is.element("BMP_PCQM", ls())) {
    BMP_PCQM <-
      data.frame(
        NumForet = numeric(), 
        NumPlac = character(), 
        Cycle = numeric(), 
        Dist = numeric(),
        Essence = character(), 
        EssReg = character(), 
        Diam = numeric(), 
        Classe = numeric(), 
        Cat = character(), 
        Haut = numeric(), 
        Type = character(), 
        Limite = numeric(),
        Stade = numeric(), 
        StadeD = numeric(), 
        StadeE = numeric(), 
        Nha = numeric(), 
        Gha = numeric(), 
        Vha = numeric(), 
        stringsAsFactors = F
      )
  }
  if (!is.element("BMP_Cercles", ls())) {
    BMP_Cercles <-
      data.frame(
        NumForet = numeric(), 
        NumPlac = character(), 
        Cycle = numeric(), 
        Dist = numeric(),
        Essence = character(), 
        EssReg = character(), 
        Diam = numeric(), 
        Classe = numeric(), 
        Cat = character(), 
        Haut = numeric(), 
        Type = character(), 
        Limite = numeric(),
        Stade = numeric(), 
        StadeD = numeric(), 
        StadeE = numeric(), 
        Nha = numeric(), 
        Gha = numeric(), 
        Vha = numeric(), 
        stringsAsFactors = F
      )
  }

  # -- Superposition des tables :
  Taillis <- rbind(Taillis, Taillis_PCQM, Taillis_Cercles)
  BMP <- rbind(BMP, BMP_PCQM, BMP_Cercles)
  # -- Calcul du volume des BMP :
  # load carbon shares deadwood
  load(paste0(wd, "/tables/carbon_codes_tables.Rdata"))
  
  BMP <- 
    BMP %>% 
    set_up_calcul_tables(
      plot_df = Placettes,
      settings_df = Echantillonnages
    ) %>% 
    calculs_bmp() %>% 
    calculs_BM_carbone(
      species_table = Essences,
      living_trees_table = Arbres,
      type = "Bois mort sur pied",
      dead_wood_carbon_content = TauxCarboneBoisMort,
      dead_wood_density = DensiteBoisMort,
      decomposition_stage_code = CodeStadeDecomposition
    )


  ##### 6/ Bois mort au sol ####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  #N.B : pour miroir BMSLineaires, consigne = déplacer Azimut des transects pour les faire malgré tout
  # ----- 6.1/ Linéaires : BMSLineaire
  BMSLineaires <-
    BMSLineaires %>% 
    set_up_calcul_tables(
      plot_df = Placettes,
      settings_df = Echantillonnages
    ) %>%
    filter(!is.na(Essence)) %>% # TODO : à homogénéiser/vérifier
    calculs_bm_lineaire(code_essreg = EssReg) %>% 
    calculs_BM_carbone(
      living_trees_table = Arbres,
      species_table = Essences,
      type = "Bois mort au sol",
      dead_wood_carbon_content = TauxCarboneBoisMort,
      dead_wood_density = DensiteBoisMort,
      decomposition_stage_code = CodeStadeDecomposition
    )

  # ----- 6.2/ Circulaire : BMSCercle
  BMSsup30 <- # TODO : changer nom ?
    BMSCercles %>%
    set_up_calcul_tables(
      plot_df = Placettes,
      settings_df = Echantillonnages
    ) %>%
    calculs_bms_sup30(code_essreg = EssReg) %>% 
    calculs_BM_carbone(
      living_trees_table = Arbres,
      species_table = Essences,
      type = "Bois mort au sol",
      dead_wood_carbon_content = TauxCarboneBoisMort,
      dead_wood_density = DensiteBoisMort,
      decomposition_stage_code = CodeStadeDecomposition
    )

  #### 7/ Dendromicohabitats ####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  Codes <- 
    Arbres %>% 
    filter(
      CodeEcolo != "" &
        !is.na(NumForet) & !is.na(NumPlac) &
        !is.na(NumArbre) & !is.na(CodeEcolo)
    ) %>% 
    mutate(
      Ref_CodeEcolo = tolower(Ref_CodeEcolo),
      CodeEcolo = tolower(CodeEcolo) # TODO : mettre tolower dans job2
    ) %>% 
    calculs_dmh(dmh_df = CodeEcolos)
  ##### / \ #####

  #### 9/ Sauvegarde ####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  # -- setting df classes
  # (évite pblme fonction Agreg01 dans agrégation par placettes)
  Arbres <- data.frame(Arbres)
  Perches <- data.frame(Perches)
  Reges <- data.frame(Reges)
  Taillis <- data.frame(Taillis)
  Reperes <- data.frame(Reperes)
  BMSLineaires <- data.frame(BMSLineaires)
  BMSsup30 <- data.frame(BMSsup30)
  BMP <- data.frame(BMP)
  Codes <- data.frame(Codes)
  
  # # -- save directory
  # output_dir <- "tables"
  # dir.create(output_dir, showWarnings = F, recursive = T)
  # 
  # -- saving
  # if (output_dir == wd) {
  #   save(
  #     TauxR, Arbres, Perches, Reges, Taillis, Reperes, BMSLineaires, 
  #     BMSsup30, BMP, Codes, AcctD, living_trees_carbon_splitted_by_log,
  #     file = "tables/gfTablesBrutes.Rdata"
  #   )
  # } else {
  #   dir.create(
  #     paste0(output_dir, "/tables"), 
  #     showWarnings = F, recursive = T
  #   )
  #   
    save(
      TauxR, Arbres, Perches, Reges, Taillis, Reperes, BMSLineaires, 
      BMSsup30, BMP, Codes, AcctD, living_trees_carbon_splitted_by_log,
      file = file.path(output_dir, "tables/gfTablesBrutes.Rdata")
    )
  # }
}

