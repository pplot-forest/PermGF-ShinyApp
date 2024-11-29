# ----- Chargement des packages nécessaires -----
suppressMessages({
  library("dplyr")
  library("stringr")
  library("gtools")
  library("rlang")
  library("tidyr")
  library("stats")
})


##### fonction de calcul des moyennes, écart-types et erreurs relatives #####
gf_AgregMoySdEr <- function(
  df = NULL,
  group = "Foret", 
  data = "Dendro", 
  last_cycle = NULL, 
  ponderation_DF = NULL, 
  regroup_DF = NULL
) {
  # --- Jonction avec les regroupements choisis (table regroup_DF)
  df <-
    regroup_DF[, c("NumForet", group, "NumPlac", "Cycle", "PoidsPlacette")] %>%
    right_join(df, by = c("NumForet", "NumPlac", "Cycle"))
  
  # --- Cas où df vide
  if (nrow(df) == 0) {
    df <- df %>% mutate(NumPlac = NULL, PoidsPlacette = NULL)
    return(df)
  }
  
  # --- variables de résultats
  results_vars <- c(
    # BV
    "Nha", "Gha", "Vha", "VcHa", "VpHa", "AcctD", "AcctV", "AcctVper", 
    "AcctG", "AcctGper", "TauxV", "TauxPU", "Taux", "Gain", "Gainper", 
    "VcHav", "VpHav", "tCha",
    # BM
    "Vha_BMSinf", "Vha_BMSsup", "Vha_BMPinf", "Vha_BMPsup", "Vha_total", 
    "tCha_BMSinf", "tCha_BMSsup", "tCha_BMPinf", "tCha_BMPsup", "tCha_total",
    # régénération
    "Recouv", "Classe1Ha", "Classe2Ha", "Classe3Ha"
  )
  
  # variables à analyser
  vars <- names(df)[ which(names(df) %in% results_vars) ]
  ncol <- length(vars)
  # définition des variables de regroupement
  group_var <- setdiff(names(df), vars)
  group_var <- group_var[-match(c("NumPlac", "PoidsPlacette"), group_var)]
  
  # --- Table de résultats
  df <- df %>%
    pivot_longer(
      cols = vars, 
      names_to = "var", 
      values_to = "value"
    ) %>% arrange(NumPlac) %>%
    # filter(EssReg == "Hêtre" & Cat == "GB" & var == "AcctGper") %>%
    mutate(
      var = factor(var, levels = vars),
      value1 = PoidsPlacette * value,
      value2 = PoidsPlacette * value ^ 2
    ) %>% #
    group_by_at(c(group_var, "Cycle", "var")) %>%
    summarise(
      value1 = sum(value1),
      value2 = sum(value2)
    ) %>%
    ungroup() %>%
    left_join(ponderation_DF, by = c("NumForet", group, "Cycle")) %>% # joindre ponderation_DF pour pondération juste
    mutate(
      poids =
        ifelse(
          str_detect(var, "Acct") | str_detect(var, "Gain"),
          PoidsAcct, Poids
        ),
      nbre =
        ifelse(
          str_detect(var, "Acct") | str_detect(var, "Gain"),
          NbreAcct, Nbre
        ),
      
      Moy = value1 / poids, # moyenne
      Sd = ((poids * value2 - value1 ^ 2) / poids / (poids - 1)) ^ 0.5, # écart-type
      CV = Sd / Moy * 100, # coefficient de variation
      Er = qt(0.975, nbre) * CV / nbre ^ 0.5 # erreur relative
    ) %>%
    select(
      all_of(group_var), "Cycle", "var", "Poids", "Nbre", "PoidsAcct", "NbreAcct",
      "Moy", "CV", "Er"
    ) %>%
    pivot_longer(
      cols = Moy:Er,
      names_to = "var_result",
      values_to = "value"
    ) %>%
    unite(var, var_result, var) %>%
    mutate(
      var = gsub("Moy_", "", var),
      var = factor(var, levels = c(
        paste0("", vars), 
        paste0("CV_", vars), 
        paste0("Er_", vars)
      ))
    ) %>%
    select(-Poids, -Nbre, -PoidsAcct, -NbreAcct) %>%
    
    # arrange var
    arrange(var) %>% 
    
    pivot_wider(
      id_cols = everything(),
      names_from = "var",
      values_from = "value"
    ) %>%
    # spread(var, value, drop = F, fill = 0) %>%
    left_join(ponderation_DF, by = c("NumForet", group, "Cycle")) %>%
    rename(
      "PoidsPlacettes" = "Poids",
      "NbrePlacettes" = "Nbre",
      "PoidsPlacettes_Acct" = "PoidsAcct",
      "NbrePlacettes_Acct" = "NbreAcct"
    ) %>%
    data.frame()
}


# ----- Fonction gf_AgregPlacettes -----
# = Agrégation, à l'échelle de différents ensembles (forêt,
# strate d'échantillonnage, ...), des différentes variables
# d'analyse (dendrométriques, économique et écologiques)
gf_AgregPlacettes <- function(
  wd = NULL, 
  output_dir = NULL, # à améliorer
  combination_table = NULL, 
  # lang = "FRA",
  forest = NULL, last_cycle = NULL,
  complete_progress = NULL,
  i18n = NULL
) {
  # -- switch shiny ***
  # incrémentation de la barre de progression
  # incProgress(detail = i18n()$t("Agrégation des résultats par dispositif..."))
  # *** --
  
  ##### 1/ Initialisation #####
  # -- création du dossier de sortie
  # output_dir <- file.path( wd, "out", clean_names(disp) )
  # # output_dir <- file.path("out", disp, i18n()$t("livret_AFI"))
  # output_dir <- file.path(output_dir, "tables")
  dir.create(output_dir, showWarnings = F, recursive = T)
  
  # -- chargement des données d'inventaire et administratives
  inventory_tables <- load(file.path(wd, "tables/gfDonneesBrutes.Rdata"))
  # -- chargement du dictionnaire de traduction
  load( file.path(wd, "tables/gf_Dictionary.Rdata") )
  # -- chargement des résultats par arbre
  results_by_plot_data <- load(file.path(output_dir, "tables/gfTablesElaboreesPlac.Rdata"))
  
  # df_list <- load(file.path(repSav, "tables/gfTablesElaboreesPlac.Rdata"))
  
  # # -- chargement des résultats de gf_Calculs()
  # if (repSav == repGF) {
  #   # -- choix du dispositif
  #   # initialisation
  #   check_all_msg <- "Editer les r\u00E9sultats pour tous les dispositifs"
  #   forest_list <- choose_forest(df_list, Forets, check_all_msg) # TODO : laisser le choix du dispositif ?(même si déjà fait au job4)
  # } else {
  #   forest_list <- forest
  # }
  # last_cycle <- get_last_cycle(df_list, forest_list)
  # 
  # # -- filtre des tables d'inventaire en fonction des numéros de dispositif sélectionnés
  # # Placettes,IdArbres,ValArbres,PCQM,Reges,Transect,BMSsup30,Reperes,Cycles
  # tables <- "results_by_plot"
  # filter_by_forest(tables, forest_list, last_cycle)
  # filter_by_forest("Cycles", forest_list, last_cycle)
  
  #  Définition de count_vars_by_pop
  count_vars_by_pop <- data.frame(
    Pop = c(
      "Fpied", "Den", "PFutaie", "Exploit", "Per", "Taillis", "Codes", 
      "Rege", "BM", "BMS", "BMP"
    ), 
    data = c(
      rep("Dendro", 4), "Per", "Taillis", "Codes", "Rege", "BM", "BMS", "BMP"
    )
  )
  

  # -- Problème calcul des accroissements (rajout de l'accroissement sur la moitié de la périodes aux arbres coupés)
  # df4 <- df %>%
  #   filter(EssReg == "Hêtre" & Cat == "GB") %>%
  #   group_by(NumForet) %>%
  #   summarise(AcctGper = sum(AcctGper, na.rm = T) / 153) %>%
  #   ungroup()

  ##### 2/ Boucle(s) - agrégations par ensembles #####
  # --- Paramètres par défaut (dont projet PP Tétras)
  TabData <-
    data_frame(
      Pop = c( # var
        "Tot", "Fpied", "Den", "PFutaie", "Exploit", "Per", "Taillis", "Codes",
        "Rege", "BM", "BMS", "BMP"
      ),
      data = c(rep("Dendro", 6), "Taillis", "Codes", "Rege", "BM", "BMS", "BMP")
    )

  results_by_group <- c()

  # --- Agrégations par ensembles (fixée à forêt pour application SAIC actuelle)
  # Table listant les ensembles à prendre en compte (individuellement) dans l'agrégation (1:forêt)
  # combination_table <-
  #   data.frame(
  #     V1 = "Foret", V2 = NA, V3 = NA, V4 = NA, V5 = NA,
  #     V6 = NA, V7 = NA, V8 = NA, V9 = NA,
  #     stringsAsFactors = F
  #   )
  # combination_table <-
  #   data.frame(
  #     V1=c("Foret", NA), V2=c(NA, "Groupe2"), V3=NA, V4=NA, V5=NA, V6=NA, V7=NA, V8=NA, V9=NA,
  #     stringsAsFactors = F
  #   )
  # combination_table <-
  #   data.frame(
  #     V1=c("Foret", NA), V2=c(NA, "Parcelle"), V3=NA, V4=NA, V5=NA, V6=NA, V7=NA, V8=NA, V9=NA,
  #     stringsAsFactors = F
  #   )

  for (i in 1:nrow(combination_table)) { # loop 'i in 1:nrow(combination_table)'
    # print(paste0("i = ",i))
    group <- combination_table[i, 1:ncol(combination_table)] %>% unlist() %>% unname() %>% na.omit()
    results_by_group_tmp <- c()

    # if (!"Foret" %in% group) {
    #   group <- c("Foret", group)
    # }
    results_by_group_tmp <- c()

    # ----- Tables pondération - distinction stock et accroissement
    # DernierCycle <- max(as.numeric(Placettes$Cycle),na.rm=T)

    # join forest name
    if (i == 1) {
    Placettes <-
      Placettes %>%
      left_join(Forets[, c("NumForet", "Nom")], by = "NumForet") %>%
      rename(Foret = Nom)
    }
    # regroupement de la table Placettes et de la table Regroups
    regroup_DF <-
      Placettes %>%
      select(-Observations) %>%
      left_join(Regroups, by = c("NumForet", "NumPlac", "Cycle")) %>%
      select(-Observations)
    # table contenant les nombres et les poids des placettes des différents cycles
    # (importance de PoidsAcct et NbreAcct pour cycle > 1)
    ponderation_DF <-
      regroup_DF %>%
      select("NumForet", "Foret", all_of(group), "NumPlac", "Cycle", "PoidsPlacette") %>%
      rename(Poids = PoidsPlacette) %>%
      mutate(Nbre = 1) %>%
      # on complète les placettes qui seraient éventuellement absentes
      # (indispensables pour faire les bons calculs d'accroissement)
      group_by(NumForet, Foret) %>%
      complete(NumPlac, Cycle) %>%

      arrange(NumForet, Foret, NumPlac, Cycle) %>%
      group_by(NumForet, Foret, NumPlac) %>%
      mutate(
        NbreAcct = ifelse(
          Cycle > 1, 
          (Nbre + lag(Nbre)) / 2, 
          (Nbre + lead(Nbre)) / 2
          ),
        PoidsAcct = ifelse(
          Cycle > 1, 
          (Poids + lag(Poids)) / 2, 
          (Poids + lead(Poids)) / 2
          ),

        NbreAcct = ifelse(last_cycle > 1, NbreAcct, Nbre),
        PoidsAcct = ifelse(last_cycle > 1, PoidsAcct, Poids)
      ) %>%
      ungroup() %>%
      # mutate(NumPlac = as.numeric(NumPlac)) %>% modif 03/05/2024

      # agrégation :
      group_by_at(c("NumForet", group, "Cycle")) %>%
      summarise_at(c("Poids", "Nbre", "PoidsAcct", "NbreAcct"), sum, na.rm = T) %>%
      ungroup() %>%
      data.frame()

    if (last_cycle == 1) {
      regroup_DF <- regroup_DF %>% mutate(NbreAcct = NULL, PoidsAcct = NULL)
    }

    for (j in 1:length(results_by_plot)) {
      # j=41 # debug
      # j=90 # debug
      # print(j) # debug
      
      # -- paramètres de regroupement
      # nom de la table des résultats par placettes
      table_name <- names(results_by_plot[j])
      # nom de la population
      # data <- str_split(table_name, "_")[[1]][1] %>% str_sub(6, -1)
      pop <- str_sub(table_name, 6, str_locate(table_name, "_")[, 1] - 1)
      data <- unique(TabData$data[TabData$Pop %in% pop])
      # table de résultats par placette
      table <- results_by_plot[[j]]

      # table de résultats par ensemble
      results_table <- gf_AgregMoySdEr(
        df = table,
        group = group, 
        data = data, 
        last_cycle = last_cycle, 
        ponderation_DF = ponderation_DF, 
        regroup_DF = regroup_DF
      )
      
      # group <- "Foret" # debug
      # data <-  data # debug
      results_by_group_tmp <- c(results_by_group_tmp, list(results_table))
      
      # table name
      attributes <- str_sub(table_name, str_locate(table_name, "_")[, 1] + 1, -1)
      table_name <- paste0(
        "gf", paste0(group, collapse = ""), pop, "_", 
        attributes
      )
      
      # assign name
      names(results_by_group_tmp)[j] <- table_name
      
      # incrémentation de la barre de progression
      # -- switch shiny ***
      incProgress(amount = 1 / complete_progress)
      print(paste0("complete_progress = ", complete_progress, "; progress = ", 1 / complete_progress)) # debug
      # *** --
      
    }
    results_by_group <- c(results_by_group,results_by_group_tmp)
  } # end of loop 'i in 1:nrow(combination_table)'

  ##### 3/ Sauvegarde #####
  # if (repSav == repGF) {
  #   save(results_by_group, file = "tables/gfTablesElaborees.Rdata")
  # } else  {
  #   dir.create(file.path(repSav, "tables"), showWarnings = F, recursive = T)
    save(results_by_group, file = paste0(output_dir, "/tables/gfTablesElaborees.Rdata"))
  # }
  
  # msg_str <- "Agr\u00E9gation des r\u00E9sultats par ensemble termin\u00E9e"
  # print(msg_str)
}
