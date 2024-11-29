# ----- Chargement des packages nécessaires -----

suppressMessages({
  library("dplyr")
  library("stringr")
})

#### fonction d'agrégation des résultats par placettes ####
aggregate_tables_by_plot <- function(
  df = NULL, group_var = NULL, vars = NULL
) {
  if (nrow(df) > 0) {
    df <- df %>% group_by_at(group_var) %>% summarise_at(vars, sum, na.rm = T) %>% ungroup()
  } else {
    df <- df %>% select(all_of(group_var), all_of(vars)) #%>%
    # mutate(
    #   NumDisp = as.numeric(NumDisp), # TODO : à régler plus en amont
    #   NumPlac = as.character(NumPlac),
    #   Cycle = as.numeric(Cycle)
    # )
  }
  df <- df %>% data.frame()
  
  # -- return of aggregate_tables_by_plot function
  return(df)
}


##### fonction gf_AgregArbres #####
# = Agrégation, à l'échelle de chaque placette, des différentes variables
# d'analyse (dendrométriques, économique et écologiques)
gf_AgregArbres <- function(
  wd = NULL, 
  output_dir = NULL,
  forest = NULL,
  last_cycle = NULL,
  combination_table = NULL, 
  complete_progress = complete_progress,
  i18n = i18n
) {
  # -- switch shiny ***
  # incrémentation de la barre de progression
  # incProgress(detail = i18n()$t("Agrégation des résultats par placette..."))
  # *** --
  
  ##### 1/ Initialisation #####
  # -- création du dossier de sortie
  # output_dir <- file.path( wd, "out", clean_names(disp) )
  # output_dir <- file.path("out", disp, i18n()$t("livret_AFI"))
  # output_dir <- file.path(output_dir, "tables")
  dir.create(output_dir, showWarnings = F, recursive = T)
  
  # -- chargement des données d'inventaire et administratives
  inventory_tables <- load(file.path(wd, "tables/gfDonneesBrutes.Rdata"))
  # -- chargement du dictionnaire de traduction
  load( file.path(wd, "tables/gf_Dictionary.Rdata") )
  # -- chargement des résultats par arbre
  results_by_tree_data <- load( file.path(output_dir, "tables/gfTablesBrutes.Rdata") )
  
  # incrémentation de la barre de progression
  detail = i18n()$t("Chargement des données d'inventaire")
  
  # -- switch shiny ***
  # incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  print(detail)
  
  # # -- chargement des résultats de gf_Calculs()
  # if (repSav == repGF) {
  #   # -- choix du dispositif
  #   # initialisation
  #   check_all_msg <- 
  #     "Editer les r\u00E9sultats pour toutes les for\u00EAts"
  #   df_list <- load("tables/gfTablesBrutes.Rdata")
  #   forest_list <- choose_forest(df_list, Forets, check_all_msg) # TODO : laisser le choix du dispositif ?(même si déjà fait au job4)
  # } else {
  forest_list <- forest
  # } # end condition "repSav == repGF"
  # last_cycle <- get_last_cycle(df_list, forest_list)
  
  # # -- filtre des tables d'inventaire en fonction des numéros de dispositif sélectionnés
  # # Placettes,IdArbres,ValArbres,PCQM,Reges,Transect,BMSsup30,Reperes,Cycles
  # tables <- c(
  #   "Arbres", "BMSCercles", "BMSLineaires", "Cycles", "Cercles", 
  #   "Placettes", "Reges", "Coords", "Taillis", "Tarifs"
  # )
  # filter_by_forest(tables, forest_list, last_cycle)
  
  # tables with "NumForet" -> à filtrer
  tables_to_filter <- c()
  for (table in inventory_tables) {
    if ("NumForet" %in% names(get( table ))) {
      tables_to_filter <- c(tables_to_filter, table)
    }
  }
  filter_by_forest(tables = tables_to_filter, forest_list = forest_list, cycle = last_cycle)
  
  
  ##### 2/ Définition des populations #####
  # -- gestion des caractéristiques supplémentaires
  # define attributes (caract1/2/3) from 'Listes' table
  attributes_to_add <-
    Listes %>%
    filter(Liste == "ListeCaract") %>%
    distinct() %>%  # sécurité - à voir si utile
    filter(!is.na(Attribut) & !is.na(Descriptif_Attribut)) %>%
    select(Attribut) %>%
    unlist() %>%
    unname()
  
  # -- variables d'identifiant
  vars_id <- c(
    "NumForet", "NumPlac", "NumArbre", "Cycle", "Essence", "EssReg",
    attributes_to_add, 
    "Classe", "Cat", "Reg1", "Qual", "Reg2", "CodeEcolo", "Coupe"
  )
  
  ##### --- 2.1/ Bois vivant --- #####
  # --- variables de résultat
  var <- c(
    "Nha", "Gha", "Vha", "VcHa", "VpHa", #"VcHav", "VpHav",
    "AcctG", "AcctGper", "AcctV", "AcctVper", "Gain", "Gainper", 
    "tCha"
  )
  
  arbres_vivants <- 
    Arbres %>% 
    select(any_of(vars_id), any_of(var)) %>% 
    filter(!is.na(NumForet)) # TODO : vérifier l'utilité
  abondance_codes_ecolo <- 
    Codes %>% 
    select(any_of(vars_id), any_of(var)) %>% 
    filter(!is.na(NumForet)) # TODO : vérifier l'utilité
  tCarbone <- 
    right_join(
      living_trees_carbon_splitted_by_log,
      Arbres[, c("IdArbre", vars_id)], 
      by = c("IdArbre", "Cycle")
    ) %>% 
    mutate(
      Lifetime = cut(
        duree_vie, 
        breaks = c(0, 5, seq(10, 150, 10)),
        labels = paste0(
          c(0, 5, seq(10, 140, 10)), "-", c(5, seq(10, 150, 10))
        ),
        include.lowest = F,
        right = T,
        ordered_result = F
      ),
      Lifetime = as.character(Lifetime)
    )
  vCarbone <- c("tCha")
  
  # sécurité sur les noms de variable de résultats manquantes # TODO : faire une fonction pour les vérifications et tester abondance code ecolo
  missing_names <- which(!var %in% names(arbres_vivants))
  missing_names <- 
    missing_names[!missing_names %in% c("AcctGper", "AcctVper", "Gainper")]
  if (length(missing_names) > 0) {
    stop(
      "Il manque des intitulés de colonne dans la table arbres_vivants :", 
      paste0(missing_names, collapse = ", ")
    )
  }
  # sécurité sur les noms de variable de résultats (inféodés aux remesures) manquants
  if (last_cycle > 1) {
    if (!all( c("AcctGper", "AcctVper", "Gainper") %in% names(arbres_vivants) ))
      stop("les intitulés de colonnes AcctGper, AcctVper et Gainper ne sont pas tous détectés dans la table des arbres vivants")
  }
  
  # --- Précomptables - passages à la futaie et tiges prélevées
  tPFutaie <- # passages_futaie = tPFutaie anciennement
    arbres_vivants %>% 
    filter(is.element(Coupe, c("PF", "PF/E", "PF/C"))) %>%
    mutate(
      Coupe = ifelse(Coupe == "PF/E", "PF", Coupe),
      Coupe = ifelse(Coupe == "PF/C", "PF", Coupe)
    )
  
  # populations pour calcul des prélèvements
  chablis <- 
    arbres_vivants %>% 
    filter(is.element(Coupe, c("PF/C", "C"))) %>%
    mutate(
      Coupe = ifelse(Coupe == "PF/C", "C", Coupe),
      AcctGper = Gha,
      AcctVper = Vha,
      AcctGainper = VcHa
    )
  exploites <- 
    arbres_vivants %>% 
    filter(is.element(Coupe, c("PF/E", "E"))) %>%
    mutate(
      Coupe = ifelse(Coupe == "PF/E", "E", Coupe),
      AcctGper = Gha,
      AcctVper = Vha,
      AcctGainper = VcHa
    )
  # tExploit <- # arbres_preleves = tExploit anciennement
  #   arbres_vivants %>% 
  #   filter(is.element(Coupe, c("PF/E", "PF/C", "E", "C"))) %>%
  #   mutate(
  #     Coupe = ifelse(Coupe == "PF/E", "E", Coupe),
  #     Coupe = ifelse(Coupe == "PF/C", "C", Coupe)
  #   )
  # vPFutaie <- var
  # vExploit <- var
  
  # --- Arbres porteurs de dendromicro-habitats
  tCodes <- abondance_codes_ecolo
  vCodes <- c("Nha", "Gha", "Vha", "tCha") # TODO : var_DMH = vCodes anciennement
  
  # --- Taillis
  tTaillis <- Taillis
  vTaillis <- var[var %in% names(Taillis)]
  
  # -- Précomptables
  # toutes tiges affranchies confondues
  tFpied <- arbres_vivants
  vFpied <- var  # DISTINCTION INUTILE CAR POPULATIONS IDENTIQUES
  
  # prélèvements
  tChablis <- chablis
  tExploites <- exploites
  vChablis <- vExploites <- var
  # # perches exclues
  # tDen <- t[which(t$Cat != "PER"), ]
  # vDen <- var
  
  # -- Perches
  tPer <- select(Perches, -Coupe)
  vPer <- var[var %in% names(Perches)]
  
  ##### --- 2.2/ Bois mort --- #####
  # template if no values
  template_bm <- tibble(
    NumForet = numeric(),
    NumPlac = character(),
    Cycle = numeric(),
    Essence = character(),
    EssReg = character(),
    StadeD = numeric(),
    StadeE = numeric(),
    Classe = numeric(),
    Cat = character(),
    Nha = numeric(),
    Gha = numeric(),
    Vha = numeric(),
    Type = character(),
    tCha = numeric()
  )
  
  # -- Bois mort au sol
  # - tables
  # < 30
  BMSinf <- if (nrow(BMSLineaires) > 0) {
    BMSLineaires %>%
      mutate(Type = "BMSinf") %>% 
      select(
        setdiff(names(template_bm), c("Nha", "Gha"))
      )
  } else template_bm
  # >= 30
  BMSsup <- if (nrow(BMSsup30) > 0) {
    BMSsup30 %>%
      mutate(Type = "BMSsup") %>% 
      select(
        setdiff(names(template_bm), c("Nha", "Gha"))
      )
  } else template_bm
  
  # - BMS outcome variables
  vBMS <- c("Vha","tCha")
  
  # - stack BMSinf & BMSsup
  tBMS <- rbind(BMSinf, BMSsup)
  
  
  # -- Bois mort sur pied
  # - table
  tBMP <- if (nrow(BMP) > 0) {
    BMP %>% 
      mutate(
        Type = ifelse(Diam < 30, "BMPinf", "BMPsup") # Attention avant c'était ">" au lieu de ">=" dans la table BMPsup
      ) %>% 
      select( names(template_bm) )
  } else template_bm
  
  # - BMP outcome variables
  vBMP <- c("Nha", "Gha", "Vha", "tCha")
  
  
  # -- Bois mort total
  # - BM outcome vars
  vBM <- c(
    "Vha_BMSinf", "Vha_BMSsup", "Vha_BMPinf", "Vha_BMPsup", "Vha_total", 
    "tCha_BMSinf", "tCha_BMSsup", "tCha_BMPinf", "tCha_BMPsup", "tCha_total"
  )
  
  # - stack BMSinf & BMSsup
  tBM <- 
    tBMP %>% select(-Nha, -Gha) %>% 
    rbind(tBMS)
  
  # - format BM table - TODO : comprend pas pourquoi la somme des colonnes ne tombe pas rond avec dispositif FD Geneve... -> élémentés étaient manquants
  if (nrow(tBM) > 0) {
    tBM2 <- 
      tBM %>% 
      
      # complete missing Type
      mutate(Type = factor(Type, levels = c("BMSinf", "BMSsup", "BMPinf", "BMPsup"))) %>%
      complete(Type, nesting(NumForet, NumPlac, Cycle), fill = list(Vha = 0, tCha = 0)) %>% 
      
      # calcul total by Type
      group_by(
        NumForet, NumPlac, Cycle, Essence, EssReg, StadeD, StadeE, Classe, Cat, Type
      ) %>%
      summarise(
        Vha = sum(Vha, na.rm = T),
        tCha = sum(tCha, na.rm = T)
      ) %>%
      ungroup()
    
    t1 <- tBM2 %>% 
      
      # pivot
      pivot_wider(
        # id_cols = -c("Type", "tCha", "Vha"),
        names_from = "Type",
        values_from = c("tCha", "Vha"),
        values_fill = list(tCha = NA, Vha = NA) # TODO : vérifier si nécessaire. Sinon garder NA
      )
    
    t2 <- tBM2 %>% 
      # calcul total through all types
      group_by(NumForet, NumPlac, Cycle, Essence, Classe, StadeD, StadeE) %>% 
      summarise(
        tCha_total = sum(tCha, na.rm = T),
        # tCha_total = tCha_BMSinf + tCha_BMSsup + tCha_BMPinf + tCha_BMPsup,
        Vha_total = sum(Vha, na.rm = T)
        # Vha_total = Vha_BMSinf + Vha_BMSsup + Vha_BMPinf + Vha_BMPsup,
      ) %>% 
      ungroup()
    
    tBM <- left_join(t1, t2) %>% 
      
      # filter zero values - pas possible de filtrer avant pivot sinon risque de voir disparaître un type
      filter(tCha_total > 0 | Vha_total > 0) %>% 
      
      # sort
      arrange(NumForet, NumPlac, Cycle, Essence, Classe, StadeD, StadeE)
  } else {
    tmp <- data.frame(matrix(ncol = length(vBM), nrow = 0))
    names(tmp) <- vBM
    tBM <- tBM %>% select(-Type, -Vha, -tCha) %>% cbind(tmp)
  }
  
  #### --- 2.3/ Régénération --- ####
  # - table
  tRege <- Reges
  # - outcome variables
  vRege <- c("Recouv", "Classe1Ha", "Classe2Ha", "Classe3Ha")
  ##### / \ #####
  
  
  ##### 3/ Agrégation #####
  # --- Boucle
  results_by_plot <- c()
  
  for (data in unique(combination_table$Data)) {
    print(data) # debug
    # data <- "Rege" # debug
    # data <- unique(combination_table$Data)[1] # debug
    combination_table_tmp <- combination_table %>% filter(Data == data)
    results_by_plot_tmp <- c()
    
    for (i in 1:nrow(combination_table_tmp)) {
      # paramètres de regroupement
      attributes <- combination_table_tmp[i, -dim(combination_table_tmp)[2]] %>% unlist() %>% unname() %>% na.omit()
      group_var <- c("NumForet", "NumPlac", "Cycle", attributes)
      group_var <- group_var[!is.na(group_var)]
      table <- get( paste0("t", combination_table_tmp$Data[i]) )
      results_vars <- get( paste0("v", data) )
      
      # agrégation de la table de résultats par placette
      results_table <- aggregate_tables_by_plot(
        df = table, 
        group_var = group_var,
        vars = results_vars
      )
      results_by_plot_tmp <- c(results_by_plot_tmp, list(results_table))
      
      # nom de la table
      table_name <- paste0("gfPla", data, "_", paste0(attributes, collapse = ""))
      table_name <- clean_names(table_name)
      names(results_by_plot_tmp)[i] <- table_name
      
      # incrémentation de la barre de progression
      # -- switch shiny ***
      incProgress(amount = 1 / complete_progress)
      print(paste0("complete_progress = ", complete_progress, "; progress = ", 1 / complete_progress)) # debug
      # *** --
      
      # print(table_name) # debug
    }
    results_by_plot <- c(results_by_plot, results_by_plot_tmp)
  }
  ##### / \ #####
  
  
  ##### 4/ Sauvegarde #####
  # if (repSav == repGF) {
    # save("results_by_plot", file = "tables/gfTablesElaboreesPlac.Rdata")
  # } else  {
    # dir.create( paste0(repSav, "/tables"), showWarnings = F, recursive = T)
    save("results_by_plot", file = paste0(output_dir, "/tables/gfTablesElaboreesPlac.Rdata"))
  # }
  
  # msg_str <- "Agr\u00E9gation des r\u00E9sultats par placette termin\u00E9e"
  # print(msg_str)
  # msg <- tk_messageBox(type = "ok", message = msg_str, icon = 'info')
}