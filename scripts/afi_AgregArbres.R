##### fonction d'agrégation des résultats par placette #####
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




#' Agrégation des résultats par placettes
#' @description Cette fonction permet de regrouper les différentes résultats d'analyse par placette.
#' Elle reprend les résultats de la fonction 'afi_Calculs()'. C'est la deuxième étape de calcul.
#'
#' @return Génère l'archive AFITablesElaboreesPlac.Rdata dans le dossier 'tables'.
#'
#' @param repAFI = répertoire contenant les données
#' @param lang = langue (sélectionnée) de l'interface
#' @param combination_table Liste des différentes combinaisons de caractères pour lesquels éditer des résultats
#' @param repSav répertoire de sauvegarde des données (pour édition du livret AFI)
#' @param disp choix du dispositif (pour édition du livret AFI)
#'
#' @author Valentin Demets, Bruciamacchie Max
#'
#' @export

afi_AgregArbres <- function(
  wd = NULL, 
  output_dir = NULL, 
  combination_table = NULL, 
  # lang = "FRA",
  disp = NULL, last_cycle = NULL,
  complete_progress = NULL,
  i18n = NULL
) {
  # incrémentation de la barre de progression
  # -- switch shiny ***
  incProgress(detail = i18n()$t("Agrégation des résultats par placette..."))
  # *** --
  
  ##### 1/ Initialisation #####
  # -- création du dossier de sortie
  # output_dir <- file.path( wd, "out", clean_names(disp) )
  # output_dir <- file.path("out", disp, i18n()$t("livret_AFI"))
  output_dir <- file.path(output_dir, "tables")
  dir.create(output_dir, showWarnings = F, recursive = T)
  
  # -- chargement des données d'inventaire et administratives
  # define Rdata to load
  inventory_data <- file.path(wd, "tables/afiDonneesBrutes.Rdata")
  admin_data <- file.path(wd, "tables/afiCodes.Rdata")
  results_by_tree_data <- file.path(output_dir, "afiTablesBrutes.Rdata")
  
  # loading
  inventory_tables <- load(inventory_data)
  load(admin_data)
  load(results_by_tree_data)
  # df_list <- load(file.path(repSav, "tables/afiTablesBrutes.Rdata"))
  # 
  # # if (repSav == repAFI) {
  # #   # -- choix du dispositif
  # #   # initialisation
  # #   check_all_msg <- ifelse(
  # #     lang == "FRA", 
  # #     "Editer les r\u00E9sultats pour tous les dispositifs", 
  # #     "Edit results for all stands"
  # #   )
  # df_list <- load("tables/afiTablesBrutes.Rdata")
  #   disp_list <- choose_disp(df_list, Dispositifs, check_all_msg) # TODO : laisser le choix du dispositif ?(même si déjà fait au job4)
  # # } else {
  # #   disp_list <- disp
  # # } # end condition "repSav == repAFI"
  # 
  # # # -- chargement des résultats de afi_Calculs()
  #   last_cycle <- get_last_cycle(df_list, disp_list)
  #   
  # # -- filtre des tables d'inventaire en fonction des numéros de dispositif sélectionnés
  # filter_by_disp(inventory_tables, disp_list, last_cycle)
  ##### / \ #####
  
  
  ##### 2/ Définition des populations #####
  ##### --- 2.1/ Bois vivant --- #####
  vars_id <- c(
    "NumDisp", "NumPlac", "NumArbre", "Cycle", "Essence", "EssReg", 
    "Classe", "Cat", "Qual1", "Qual", "Qual2", "CodeEcolo", "Coupe"
  )
  ##### --- 2.1/ Bois vivant --- #####
  # --- Précomptables
  var <- c(
    "Nha", "Gha", "Vha", "VcHa", "VpHa", "VcHav","VpHav",
    "AcctG", "AcctGper", "AcctV", "AcctVper", "Gain", "Gainper", 
    "Couvert", "tCha"
  )
  
  arbres_vivants <- # arbres_vivants = t anciennement
    Arbres %>% 
    # remove arbres limites
    filter(is.na(Limite)) %>% 
    # select
    select(any_of(vars_id), any_of(var)) %>% 
      # TODO : vérifier l'utilité de '!is.na(NumDisp)'
    filter(!is.na(NumDisp)) # debug : %>% filter(NumDisp == 61 & Cycle == 1 & NumArbre == 1 & NumPlac == 3)
  abondance_codes_ecolo <- # abondance_codes_ecolo = tCodes anciennement
    Codes %>% 
    select(any_of(vars_id), any_of(var)) %>% 
    filter(!is.na(NumDisp)) # TODO : vérifier l'utilité
  tCarbone <- 
    right_join(
      living_trees_carbon_splitted_by_log,
      Arbres[, c("IdArbre", vars_id)], 
      by = c("IdArbre", "Cycle")
    ) %>%  # debug : filter(NumDisp == 61 & Cycle == 1 & IdArbre == 17892) %>% 
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
  tExploit <- # arbres_preleves = tExploit anciennement
    arbres_vivants %>% 
    filter(is.element(Coupe, c("PF/E", "PF/C", "E", "C"))) %>%
    mutate(
      Coupe = ifelse(Coupe == "PF/E", "E", Coupe),
      Coupe = ifelse(Coupe == "PF/C", "C", Coupe)
    )
  vPFutaie <- var
  vExploit <- var
  
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
  # # perches exclues
  # tDen <- t[which(t$Cat != "PER"), ]
  # vDen <- var
  
  # -- Perches
  tPer <- select(Perches, -Coupe)
  vPer <- var[var %in% names(Perches)]
  
  ##### --- 2.1/ Bois mort --- #####
  # -- Bois mort au sol
  BMSinf <- 
    BMSLineaires %>%
    mutate(Type = "BMSinf") %>% 
    select(NumDisp, NumPlac, Cycle, Essence, EssReg, StadeD, StadeE, Classe, Cat, Vha, Type, tCha)
  BMSsup <- 
    BMSsup30 %>%
    mutate(Type = "BMSsup") %>% 
    select(NumDisp, NumPlac, Cycle, Essence, EssReg, StadeD, StadeE, Classe, Cat, Vha, Type, tCha)
  vBMS <- c("Vha","tCha")
  tBMS <- rbind(BMSinf, BMSsup)
  
  # -- Bois mort sur pied
  tBMP <- 
    BMP %>% 
    select(
      NumDisp, NumPlac, Cycle, Essence, EssReg, Diam,
      StadeD, StadeE, Classe, Cat, Type, 
      Nha, Gha, Vha, tCha, CodeEcolo
    )
  vBMP <- c("Nha", "Gha", "Vha", "tCha")
  
  # -- Bois mort total
  tBM <- 
    # stack BMSinf & BMSsup
    tBMP %>% 
    mutate(
      Type = ifelse(Diam < 30, "BMPinf", "BMPsup"), # Attention avant c'était ">" au lieu de ">=" dans la table BMPsup
      Diam = NULL, 
      Nha = NULL, 
      Gha = NULL,
      CodeEcolo = NULL
    ) %>% 
    rbind(BMSinf, BMSsup) %>%
    
    # complete missing Type
    mutate(Type = factor(Type, levels = c("BMSinf", "BMSsup", "BMPinf", "BMPsup"))) %>%
    complete(Type, nesting(NumDisp, NumPlac, Cycle), fill = list(Vha = 0, tCha = 0)) %>% 
    
    # calcul total by Type
    group_by(
      NumDisp, NumPlac, Cycle, Essence, EssReg, StadeD, StadeE, Classe, Cat, Type
    ) %>%
    summarise(
      Vha = sum(Vha, na.rm = T),
      tCha = sum(tCha, na.rm = T)
    ) %>%
    # calcul total through all types
    group_by(NumDisp, NumPlac, Cycle, Essence, Classe, StadeD, StadeE) %>% 
    mutate(
      tCha_total = sum(tCha, na.rm = T),
      Vha_total = sum(Vha, na.rm = T)
    ) %>% 
    ungroup() %>% 
    
    # filter zero values
    # filter(tCha > 0 | Vha > 0) %>% 
    
    # pivot
    pivot_wider(
      # id_cols = -c("Type", "tCha", "Vha"),
      names_from = "Type",
      values_from = c("tCha", "Vha"),
      values_fill = list(tCha = 0, Vha = 0) # TODO : vérifier si nécessaire. Sinon garder NA
    ) %>% 
    
    # filter missing species (empty values)
    filter(!is.na(Essence)) %>%
    
    # sort
    arrange(NumDisp, NumPlac, Cycle, Essence, Classe, StadeD, StadeE)
  
  vBM <- c(
    "Vha_BMSinf", "Vha_BMSsup", "Vha_BMPinf", "Vha_BMPsup", "Vha_total", 
    "tCha_BMSinf", "tCha_BMSsup", "tCha_BMPinf", "tCha_BMPsup", "tCha_total"
  )
  
  #### --- 2.3/ Régénération --- ####
  tRege <- Reges
  vRege <- c("Recouv", "Classe1Ha", "Classe2Ha", "Classe3Ha")
  ##### / \ #####
  
  
  ##### 3/ Agrégation #####
  
  # --- Boucle
  results_by_plot <- c()
  
  for (data in unique(combination_table$Data)) { # loop 'data in unique(combination_table$Data)'
    # print(data) # debug
    # data <- "Carbone" # debug
    # data <- unique(combination_table$Data)[2] # debug
    combination_table_tmp <- combination_table %>% filter(Data == data)
    results_by_plot_tmp <- c()
    
    for (i in 1:dim(combination_table_tmp)[1]) { # loop 'i in 1:dim(combination_table_tmp)[1]'
      # print(i) # debug
      # paramètres de regroupement
      attributes <- combination_table_tmp[i, -ncol(combination_table_tmp)] %>% unlist() %>% unname() %>% na.omit()
      group_var <- c("NumDisp", "NumPlac", "Cycle", attributes)
      # group_var <- group_var[!is.na(group_var)]
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
      table_name <- paste0("afiPla", data, "_", paste0(attributes, collapse = ""))
      table_name <- clean_names(table_name)
      names(results_by_plot_tmp)[i] <- table_name
      
      # incrémentation de la barre de progression
      # -- switch shiny ***
      incProgress(amount = 1 / complete_progress)
      print(paste0("complete_progress = ", complete_progress, "; progress = ", 1 / complete_progress)) # debug
      # *** --
      # print(table_name)
    } # end of loop 'i in 1:dim(combination_table_tmp)[1]'
    results_by_plot <- c(results_by_plot, results_by_plot_tmp)
  } # end of loop 'data in unique(combination_table$Data)'
  ##### / \ #####
  
  ##### 4/ Sauvegarde #####
  # if (repSav == repAFI) {
    save("results_by_plot", file = file.path(output_dir, "afiTablesElaboreesPlac.Rdata"))
  # } else {
  #   dir.create( paste0(repSav, "/tables"), showWarnings = F, recursive = T)
  #   save("results_by_plot", file = paste0(repSav, "/tables/afiTablesElaboreesPlac.Rdata"))
  # }
  
  print(i18n()$t("Agr\u00E9gation des r\u00E9sultats par placettes termin\u00E9e"))
}
