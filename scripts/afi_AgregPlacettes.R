##### fonction de calcul des moyennes, écart-types et erreurs relatives - TODO : à extraire de la fonction (factorisation) #####
afi_AgregMoySdEr <- function(
  df, 
  group = "Disp", 
  ponderation_DF = ponderation_DF, 
  admin = NULL
) {
  # # --- Jonction avec les regroupements choisis (table regroup_DF)
  # df <-
  #   regroup_DF[, c("NumForet", group, "NumPlac", "Cycle", "PoidsPlacette")] %>%
  #   right_join(df, by = c("NumForet", "NumPlac", "Cycle"))
  df <-
    df %>%
    mutate(
      NumPlac = as.character(NumPlac), 
      Disp = admin$Nom[match(df$NumDisp, admin$NumDisp)], 
      PoidsPlacette = 1
    ) %>%
    select(
      all_of(group), "NumDisp", "NumPlac", "Cycle", "PoidsPlacette", 
      all_of(setdiff(
        names(df), 
        c(group, "NumDisp", "NumPlac", "Cycle", "PoidsPlacette")
      ))
    )
  
  # --- Récupération des variables de résultats
  # table listant les nombres de colonnes à prendre en compte pour les populations
  results_vars <- c(
    # BV
    "Nha", "Gha", "Vha", "VcHa", "VpHa", "AcctD", "AcctV", "AcctVper", 
    "AcctG", "AcctGper", "TauxV", "TauxPU", "Taux", "Gain", "Gainper", 
    "Couvert", "Carb11", "Carb2", "Carb21", "Carb3", "TotalCarbHa", 
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
  
  
  # --- Cas où df vide
  if (dim(df)[1] == 0) {
    column_names <- c(
      paste0(c(rep("CV_", ncol), rep("Er_", ncol)), rep(vars, 2))
    )
    tmp <- data.frame(matrix(ncol = length(column_names), nrow = 0))
    names(tmp) <- column_names
    tmp <- data.frame(sapply(tmp, function(x) as.numeric(x)))
    
    df <- cbind(df, tmp)
    return(df)
  }
  
  # --- Table de résultats
  df <-
    df %>%
    pivot_longer(
      cols = vars, 
      names_to = "var", 
      values_to = "value"
    ) %>%
    # gather(var, value, vars) %>%
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
    left_join(ponderation_DF, by = c("NumDisp", "Cycle")) %>% # joindre ponderation_DF pour pondération juste
    mutate(
      poids  = 
        ifelse(
          str_detect(var, "Acct") | str_detect(var, "Gain"), 
          PoidsAcct, Poids
        ), 
      nbre  = 
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
      cols = c("Moy", "CV", "Er"), 
      names_to = "var_result", 
      values_to = "value"
    ) %>%
    # gather(var_result, value, Moy:Er) %>%
    unite(var, var_result, var) %>%
    mutate(
      var = gsub("Moy_", "", var), 
      var = factor(
        var, 
        levels = paste0(
          c(rep("", ncol), rep("CV_", ncol), rep("Er_", ncol)), rep(vars, 3)
        )
      )
    ) %>%
    select(-Poids, -Nbre, -PoidsAcct, -NbreAcct) %>%
    spread(var, value, drop = T, fill = 0) %>%
    left_join(ponderation_DF, by = c("NumDisp", "Cycle")) %>%
    rename(
      "PoidsPlacettes" = "Poids", 
      "NbrePlacettes" = "Nbre", 
      "PoidsPlacettes_Acct" = "PoidsAcct", 
      "NbrePlacettes_Acct" = "NbreAcct"
    ) %>%
    data.frame()
  
  # -- retour de la fonction afi_AgregMoySdEr
  return(df)
}
##### / \ #####


#' Agrégation des résultats à l'échelle du dispositif
#' @description Cette fonction permet de regrouper les différentes résultats d'analyse par dispositif.
#' Elle reprend les résultats de la fonction 'afi_AgregPlacettes()'. C'est la troisième et dernière étape de calcul.
#'
#' @return Génère l'archive AFITablesElaborees.Rdata dans le dossier 'Tables'.
#'
#' @param repAFI = répertoire contenant les données
#' @param lang = langue (sélectionnée) de l'interface
#' @param combination_table Liste des différentes combinaisons de caractères pour lesquels éditer des résultats
#' @param repSav répertoire de sauvegarde des données (pour édition du livret AFI)
#' @param Choix choix du dispositif (pour édition du livret AFI)
#'
#' @author Valentin Demets, Bruciamacchie Max
#' @import stringr
#' @import dplyr
#' @import gWidgets2
#' @importFrom stats as.formula
#' @importFrom stats qt
#' @export

afi_AgregPlacettes <- function(
  wd = NULL, 
  output_dir = NULL, # à améliorer
  combination_table = NULL, 
  # lang = "FRA",
  disp = NULL, last_cycle = NULL,
  complete_progress = NULL,
  i18n = NULL
) {
  # incrémentation de la barre de progression
  # -- switch shiny ***
  incProgress(detail = i18n()$t("Agrégation des résultats par dispositif..."))
  # *** --
      
      ##### 1/ Initialisation #####
      # -- création du dossier de sortie
      # output_dir <- file.path( wd, "out", clean_names(disp) )
      # # output_dir <- file.path("out", disp, i18n()$t("livret_AFI"))
  output_dir <- file.path(output_dir, "tables")
  dir.create(output_dir, showWarnings = F, recursive = T)
      
      # -- chargement des données d'inventaire et administratives
      # define Rdata to load
      inventory_data <- file.path(wd, "tables/afiDonneesBrutes.Rdata")
      admin_data <- file.path(wd, "tables/afiCodes.Rdata")
      results_by_plot_data <- file.path(output_dir, "afiTablesElaboreesPlac.Rdata")
      
      # loading
      inventory_tables <- load(inventory_data)
      load(admin_data)
      load(results_by_plot_data) # load "results_by_plot" list
      # df_list <- load(file.path(repSav, "tables/afiTablesElaboreesPlac.Rdata"))
      
      # # -- chargement des résultats de afi_Calculs()
      # if (repSav == repAFI) {
      #   # -- choix du dispositif
      #   # initialisation
      #   check_all_msg <- ifelse(
      #     lang == "FRA",
      #     "Editer les r\u00E9sultats pour tous les dispositifs",
      #     "Edit results for all stands"
      #   )
      #   disp_list <- choose_disp(df_list, Dispositifs, check_all_msg) # TODO : laisser le choix du dispositif ?(même si déjà fait au job4)
      # } else {
      #   disp_list <- disp
      # }
      # last_cycle <- get_last_cycle(df_list, disp_list)
      # 
      # # -- filtre des tables en fonction des numéros de dispositif sélectionnés
      # filter_by_disp(inventory_tables, disp, last_cycle)
      # filter_by_disp("results_by_plot", disp, last_cycle)
      # # tables <- "results_by_plot"
      # # filter_by_disp(tables, disp_list, last_cycle)
      # # filter_by_disp("Cycles", disp_list, last_cycle)
      ##### / \ #####
      
      
      ##### 2/ Agrégation #####
      
      results_by_group <- c()
      for (i in 1:nrow(combination_table)) {
        # print(paste0("i = ", i)) # debug
        group <- combination_table[i, 1:ncol(combination_table)] %>% unlist() %>% unname() %>% na.omit()
        results_by_group_tmp <- c()
        
        # ----- Tables pondération - distinction stock et accroissement
        # DernierCycle <- max(as.numeric(NbPlac$Cycle), na.rm = T)
        
        ponderation_DF <-
          # Nbre <-
          Cycles %>%
          # filter(is.element(NumDisp, num_list)) %>%
          select(NumDisp, Cycle, NbPlacettes) %>%
          mutate(
            Nbre = NbPlacettes, 
            NbreAcct = NbPlacettes, 
            Poids = NbPlacettes, 
            PoidsAcct = NbPlacettes
          ) %>%
          select(NumDisp, Cycle, Nbre, NbreAcct, Poids, PoidsAcct)
        
        for (k in 1:length(results_by_plot)) {
          # k = 4 # debug
          
          # paramètres de regroupement
          # table de résultats par placette
          table <- results_by_plot[[k]]
          # nom de la table d'agrégation par placette
          table_name <- names(results_by_plot)[k]
          # print(table_name) # debug
          # nom de la population (Fpied, BM, ...)
          table_name_splitted <- str_split(table_name, "_", simplify = T)
          pop <- gsub("afiPla", "", table_name_splitted[, 1])

          # agrégation de la table de résultats par ensemble
          results_table <- afi_AgregMoySdEr(
            df = table, 
            group = group, 
            ponderation_DF = ponderation_DF, 
            admin = Dispositifs
          )
          results_by_group_tmp <- c(results_by_group_tmp, list(results_table))
          
          # nouveau nom de la table des résultats par ensemble
          results_table_name <- paste0("afi", paste0(group, collapse = ""), pop, "_", table_name_splitted[, 2])
          names(results_by_group_tmp)[k] <- results_table_name
          
          # incrémentation de la barre de progression
          # -- switch shiny ***
          incProgress(amount = 1 / complete_progress)
          print(paste0("complete_progress = ", complete_progress, "; progress = ", 1 / complete_progress)) # debug
          # *** --
        }
        results_by_group <- c(results_by_group, results_by_group_tmp)
      }
      ##### / \ #####
  
  
  ##### 4/ Sauvegarde #####
  save("results_by_group", file = file.path(output_dir, "afiTablesElaborees.Rdata") )
  print(i18n()$t("Agr\u00E9gations des r\u00E9sultats par dispositif(s) termin\u00E9es"))
}
