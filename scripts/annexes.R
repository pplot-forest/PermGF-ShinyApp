##### Styles de cellules #####
# -- styles
# titre = ID
style_title_id <- createStyle(
  # police texte
  fontName = "Arial", fontSize = 12, 
  fontColour = "dodgerblue4", textDecoration = "bold", 
  # fond cellule
  fgFill = "tan1", 
  # bordure
  border =  "Bottom", 
  # alignement
  valign = "top", halign = "center", 
  # rotation et wrap
  textRotation = 90, wrapText = F
)
# titre = attributs
style_title_attr <- createStyle(
  # police texte
  fontName = "Arial", fontSize = 12, textDecoration = "bold", 
  # bordure
  border =  "Bottom",
  # fond cellule
  fgFill = "mediumaquamarine", 
  # alignement
  valign = "top", halign = "center", 
  # rotation et wrap
  textRotation = 90, wrapText = F
)
# titre = attributs for comment : 'Observation(s)', 'Cheminement'
style_title_comments <- createStyle(
  # police texte
  fontName = "Arial", fontSize = 12, textDecoration = "bold", 
  # bordure
  border =  "Bottom",
  # fond cellule
  fgFill = "mediumaquamarine", 
  # alignement
  valign = "center", halign = "center", 
  # rotation et wrap
  textRotation = 0, wrapText = F
)
# corps général du texte
style_general <- createStyle(
  # police texte
  fontName = "Arial", fontSize = 12, 
  # alignement
  valign = "center", halign = "center",
  # rotation et wrap
  wrapText = F
)
# anciennes valeurs (déjà présentes au précédent inventaire)
style_former_value <- createStyle(
  # police texte
  fontName = "Arial", fontSize = 12, 
  # fond cellule
  fgFill = "#C0C0C0", 
  # alignement
  valign = "center", halign = "center",
  # rotation et wrap
  wrapText = F
)
# nouvelles valeurs (à compléter au nouvel inventaire)
style_new_value <- createStyle(
  # police texte
  fontName = "Arial", fontSize = 12, border =  "Bottom", 
  # alignement
  valign = "center", halign = "center",
  # rotation et wrap
  wrapText = F
)
# ligne de séparation
style_separate_row <- createStyle(
  # bordure
  border = "Bottom"
)
# medium separate row
style_separate_medium_row <- createStyle(
  # bordure
  border = "Bottom", borderStyle = "medium"
)

# -- liste des styles
styles_list <- c(
  style_title_id, 
  style_title_attr,
  style_title_comments,
  style_general,
  style_former_value,
  style_new_value,
  style_separate_row,
  style_separate_medium_row
)
names(styles_list) <- c(
  "style_title_id", 
  "style_title_attr",
  "style_title_comments",
  "style_general",
  "style_former_value",
  "style_new_value",
  "style_separate_row",
  "style_separate_medium_row"
)

# -- sauvegarde des styles
# save(styles_list, file = "tables/wb_styles.Rdata")
##### /\ #####

# --------------------------------------------------------------------------------- #

##### fonction de nettoyage des noms #####
clean_names <- function(string, rm_spaces = T, quote_replacement = "") {
  string <- gsub("/", " sur ", string)
  if (rm_spaces) string <- gsub(" ", "_", string, fixed = T)
  string <- gsub("(", "_", string, fixed = T)
  string <- gsub(")", "_", string, fixed = T)
  
  # string <- gsub("'", "", string, fixed = T)
  string <- gsub("'", quote_replacement, string, fixed = T) # analyse vente
  string <- gsub("\u00EA", "e", string, fixed = T)
  string <- gsub("\u00E2", "a", string, fixed = T)
  string <- gsub("\u00E9", "e", string, fixed = T)
  string <- gsub("\u00E8", "e", string, fixed = T)
  string <- gsub("\u00FB", "u", string, fixed = T)
  string <- gsub("\u00FC", "u", string, fixed = T)
  string <- gsub("\u00EE", "i", string, fixed = T)
  string <- gsub("\u00F4", "o", string, fixed = T)
  string <- gsub("\u00F6", "o", string, fixed = T)
  string <- gsub("\u00E7", "c", string, fixed = T)
  
  # majuscules
  string <- gsub("\u00CA", "E", string, fixed = T)
  string <- gsub("\u00C2", "A", string, fixed = T)
  string <- gsub("\u00C9", "E", string, fixed = T)
  string <- gsub("\u00C8", "E", string, fixed = T)
  string <- gsub("\u00DB", "U", string, fixed = T)
  string <- gsub("\u00CE", "I", string, fixed = T)
  string <- gsub("\u00D4", "O", string, fixed = T)
  string <- gsub("\u00C7", "C", string, fixed = T)
  
  # retour de la fonction clean_names
  return(string)
}

##### fonction pour choisir le dispositif à traiter #####
# TODO : fusionner choose_forest et choose_disp
choose_disp <- function(
  object_list, admin = NULL, check_all_msg = NULL,
  df_2_test = NULL, i18n = i18n
) {
  # -- initialisation
  tmp <- c()
  all_num_list <- c()
  
  # -- boucle
  for (i in 1:length(object_list)) {
    # i = 1 # debug
    tmp <- 
      if (class(object_list) == "list") object_list[[i]] else {
        get(object_list[i], envir = parent.frame())
      }
    
    # cas où tmp est un data.frame
    if ("data.frame" %in% class(tmp)) {
      if ("NumDisp" %in% names(tmp)) {
        tmp <- 
          tmp %>% select(NumDisp) %>% distinct() %>% unlist() %>% unname()
        tmp <- tmp[!tmp %in% all_num_list]
        all_num_list <- c(all_num_list, tmp)
      }
    }
    
    # cas où tmp est une liste (ex : TabPla)
    if ("list" %in% class(tmp)) {
      for (tab in tmp) {
        if ("NumDisp" %in% names(tab)) {
          tab <- 
            tab %>% select(NumDisp) %>% distinct() %>% unlist() %>% unname()
          tab <- tab[!tab %in% all_num_list]
          all_num_list <- c(all_num_list, tab)
        }
      }
    }
  } # end of loop object_list
  all_num_list <- sort( as.numeric( unique(all_num_list) ) )
  
  if (is.element(NA, all_num_list)) warning("NumDisp vide d\u00E9tect\u00E9")
  all_disp_list <- paste0(
    all_num_list, "-", admin$Nom[match(all_num_list, admin$NumDisp)]
  )
  
  # -- choix du dispositif
  disp_list <- tk_select.list(
    choices = c(check_all_msg, as.character(all_disp_list)), 
    multiple = T, 
    title = "Choisir un ou plusieurs dispositifs" 
    # TODO : si on rajoute i18n() ici, alors le message "Error in .Tcl.args.objv(...) : 
    # la promesse est déjà en cours d'évaluation : référence récursive d'argument par défaut ou problème antérieur ?
    #   Called from: .Tcl.args.objv(...)" apparaît -> à résoudre
  )
  # -- sortie si aucun choix fait
  if (length(disp_list) == 0) stop("traitement interrompu - aucun dispositif choisi", call. = FALSE)
  if (is.element(check_all_msg, disp_list)) {disp_list = all_disp_list}
  
  if (!is.null(df_2_test)) {
    # test si les numéros de disp_list sont présents dans la table df_2_test
    num_list <-
      as.numeric( str_sub(disp_list, 1, str_locate(disp_list, "-")[, 1] - 1) )
    pos <- which(!num_list %in% df_2_test$NumDisp)
    if (length(pos) > 0) {
      stop(
        "Can't find the chosen stand number(s) '", 
        paste0(num_list[pos], collapse = ", "), 
        "' within the '", substitute(df_2_test), "' table"
      )
    }
  }
  
  # -- retour de la fonction choose_disp
  return(disp_list)
}

##### fonction pour choisir la forêt à traiter #####
# TODO : fusionner choose_forest et choose_disp
choose_forest <- function(
  object_list, admin = Forets, check_all_msg = NULL,
  df_2_test = NULL
) {
  # -- initialisation
  tmp <- c()
  all_num_list <- c()
  
  # -- loop for each object : get data and find the NumForet
  for (i in 1:length(object_list)) {
    # i = 15 # debug
    tmp <- 
      if (class(object_list) == "list") object_list[[i]] else {
        get(object_list[i], envir = parent.frame())
      }
    
    # cas où tmp est un data.frame
    if ("data.frame" %in% class(tmp)) { # si sf présent -> plusieurs class
      if ("NumForet" %in% names(tmp)) {
        tmp <- 
          tmp %>% select(NumForet) %>% distinct() %>% unlist() %>% unname()
        tmp <- tmp[!tmp %in% all_num_list]
        all_num_list <- c(all_num_list, tmp)
      }
    }
    
    # cas où tmp est une liste (ex : TabPla)
    if ("list" %in% class(tmp)) {
      for (tab in tmp) {
        if ("NumForet" %in% names(tab)) {
          tab <- 
            tab %>% select(NumForet) %>% distinct() %>% unlist() %>% unname()
          tab <- tab[!tab %in% all_num_list]
          all_num_list <- c(all_num_list, tab)
        }
      }
    }
  } # end of loop object_list
  all_num_list <- sort( as.numeric( unique(all_num_list) ) )
  
  if (is.element(NA, all_num_list)) warning("NumForet vide d\u00E9tect\u00E9")
  all_forest_list <- paste0(
    all_num_list, "-", admin$Nom[match(all_num_list, admin$NumForet)]
  )
  
  # -- choix de la forêt
  forest_list <- tk_select.list(
    choices = c(check_all_msg, as.character(all_forest_list)), 
    multiple = T, 
    title = "Choisir une ou plusieurs for\u00EAts"
  )
  # -- sortie si aucun choix fait
  if (length(forest_list) == 0) stop("traitement interrompu - aucune for\u00EAt choisie", call. = FALSE)
  if (is.element(check_all_msg, forest_list)) {forest_list = all_forest_list}
  
  if (!is.null(df_2_test)) {
    # test si les numéros de forest_list sont présents dans la table df_2_test
    num_list <-
      as.numeric( str_sub(forest_list, 1, str_locate(forest_list, "-")[, 1] - 1) )
    pos <- which(!num_list %in% df_2_test$NumForet)
    if (length(pos) > 0) {
      stop(
        "Can't find the chosen forest number(s) '", 
        paste0(num_list[pos], collapse = ", "), 
        "' within the '", substitute(df_2_test), "' table"
      )
    }
  }
  
  # -- retour de la fonction choose_forest
  return(forest_list)
}

##### fonction pour filtrer les tables selon une liste de dispositifs #####
# TODO : fusionner filter_by_forest et filter_by_disp
filter_by_disp <- function(
  tables = NULL, 
  disp_list = NULL, 
  cycle = NULL
) {
  # tables = results_by_plot
  # disp_list = disp
  # cycle = last_cycle
  # -- liste des numéros de dispositifs
  num_list <- as.numeric( str_sub(disp_list, 1, str_locate(disp_list, "-")[, 1] - 1) )
  
  for (tmp in tables) {
    # print(tmp) # debug
    # print(class(tmp)) # debug
    # tmp <- tables[[1]] # debug
    
    # cas où tmp est un nom de table
    # -- nom de la table
    tmp_NAME <- tmp
    # -- data.frame
    tmp <- get(tmp_NAME, envir = parent.frame())
    
    if ("data.frame" %in% class(tmp)) {
      if (dim(tmp)[1] > 0) { # rajout sécurité "NumDisp" %in% names(tmp) pour cas comme ValArbres
        # -- filtre selon la liste de dispositifs sélectionnés
        if ("NumDisp" %in% names(tmp)) {
          tmp <- tmp %>% filter(NumDisp %in% num_list)
        }
        
        # -- filtre selon le cycle
        if ("Cycle" %in% names(tmp)) {
          tmp <- tmp %>% filter(Cycle <= cycle)
        }
        
        if (cycle == 1) {
          tmp <- 
            tmp %>% mutate(AcctGper = NULL, AcctVper = NULL, AcctD = NULL)
        }
      } # end of cond 'dim(tmp)[1] > 0'
    } # end of cond '"data.frame" %in% class(tmp)'
    
    # cas où tmp est une liste
    if ("list" %in% class(tmp)) {
      for (i in 1:length(tmp)) {
        if (dim(tmp[[i]])[1] > 0) {
          # -- filtre selon la liste de dispositifs sélectionnés
          if ("NumDisp" %in% names(tmp[[i]])) {
            tmp[[i]] <- tmp[[i]] %>% filter(NumDisp %in% num_list)
          }
          
          # -- filtre selon le cycle
          if ("Cycle" %in% names(tmp[[i]])) {
            tmp[[i]] <- tmp[[i]] %>% filter(Cycle <= cycle)
          }
          
          if (cycle == 1) {
            tmp[[i]] <- 
              tmp[[i]] %>% 
              mutate(
                AcctGper = NULL, 
                AcctVper = NULL, 
                AcctD = NULL
              )
          }
        }
      } # end of loop 'i in 1:length(tmp)'
    } # end of cond '"list" %in% class(tmp)'
    
    # retour de la fonction filter_by_disp
    assign(tmp_NAME, tmp, envir = parent.frame())
  } # end of loop 'tmp in tables'
}

##### fonction pour filtrer les tables selon une liste de forêts #####
# TODO : fusionner filter_by_forest et filter_by_disp
filter_by_forest <- function(
  tables = NULL, 
  forest_list = NULL, 
  cycle = NULL
) {
  # -- liste des numéros de dispositifs
  num_list <- as.numeric( str_sub(forest_list, 1, str_locate(forest_list, "-")[, 1] - 1) )
  
  for (tmp in tables) {
    # print(tmp) # debug
    # print(class(tmp)) # debug
    # tmp <- tables[[7]] # debug
    
    # cas où tmp est un nom de table
    # -- nom de la table
    tmp_NAME <- tmp
    # -- data.frame
    tmp <- get(tmp_NAME, envir = parent.frame())
    
    if ("data.frame" %in% class(tmp)) {
      if (dim(tmp)[1] > 0) {
        # -- filtre selon la liste de dispositifs sélectionnés
        if ("NumForet" %in% names(tmp)) {
          tmp <- tmp %>% filter(NumForet %in% num_list)
        }
        
        # -- filtre selon le cycle
        if ("Cycle" %in% names(tmp)) {
          tmp <- tmp %>% filter(Cycle <= cycle) 
        }
        
        # if (cycle == 1) {
        #   tmp <- 
        #     tmp %>% 
        #     mutate(
        #       AcctGper = NULL, 
        #       AcctVper = NULL, 
        #       AcctD = NULL
        #     )
        # }
      } # end of cond 'dim(tmp)[1] > 0'
    } # end of cond '"data.frame" %in% class(tmp)'
    
    # cas où tmp est une liste
    if ("list" %in% class(tmp)) {
      for (i in 1:length(tmp)) {
        if (dim(tmp[[i]])[1] > 0) {
          # -- filtre selon la liste de dispositifs sélectionnés
          if ("Cycle" %in% names(tmp[[i]])) {
            tmp[[i]] <- tmp[[i]] %>% filter(NumForet %in% num_list)
          }
          
          # -- filtre selon le cycle
          if ("Cycle" %in% names(tmp[[i]])) {
            tmp[[i]] <- tmp[[i]] %>% filter(Cycle <= cycle)
          }
          
          # if (cycle == 1) {
          #   tmp[[i]] <- 
          #     tmp[[i]] %>% 
          #     mutate(
          #       AcctGper = NULL, 
          #       AcctVper = NULL, 
          #       AcctD = NULL
          #     )
          # }
        }
      } # end of loop length(tmp)
    } # end of cond '"list" %in% class(tmp)'
    
    # retour de la fonction filter_by_forest : assign tmp_NAME to tmp
    assign(tmp_NAME, tmp, envir = parent.frame())
  }
}

##### fonction pour choisir obtenir le numéro du dernier passage en inventaire #####
# TODO : fusionner get_last_cycle (AFI) et get_last_cycle (GF)
get_last_cycle <- function(object_list, disp_list) {
  # -- initialisation
  tmp <- c()
  all_cycle_list <- c()
  
  # -- liste des numéros de dispositifs
  num_list <- as.numeric( str_sub(disp_list, 1, str_locate(disp_list, "-")[, 1] - 1) )
  
  # -- boucle
  for (i in 1:length(object_list)) {
    # i = 10 # debug
    # print(i) # debug
    tmp <- 
      if (class(object_list) == "list") object_list[[i]] else {
        get(object_list[i], envir = parent.frame())
      }
    
    # cas où tmp est un data.frame
    if ("data.frame" %in% class(tmp)) {
      if ("NumDisp" %in% names(tmp) && "Cycle" %in% names(tmp)) {
        tmp <- 
          tmp %>% 
          filter(NumDisp %in% num_list) %>% 
          select(Cycle) %>% 
          distinct() %>% unlist() %>% unname()
        tmp <- tmp[!tmp %in% all_cycle_list]
        all_cycle_list <- c(all_cycle_list, tmp)
      }
    }
    
    # cas où tmp est une liste (ex : TabPla)
    # print(class(tmp)) # debug
    if ("list" %in% class(tmp)) {
      for (i in 1:length(tmp)) {
        tab <- tmp[[i]]
        if ("NumDisp" %in% names(tab) && "Cycle" %in% names(tab)) {
          tab <- 
            tab %>% 
            filter(NumDisp %in% num_list) %>% 
            select(Cycle) %>% 
            distinct() %>% unlist() %>% unname()
          tab <- tab[!tab %in% all_cycle_list]
          all_cycle_list <- c(all_cycle_list, tab)
        }
      }
    }
  }
  all_cycle_list <- sort( as.numeric( unique(all_cycle_list) ) )
  
  # -- numéro du dernier cycle
  last_cycle <- max(all_cycle_list, na.rm = T)
  
  # -- retour de la fonction get_last_cycle
  return(last_cycle)
}

##### fonction pour choisir obtenir le numéro du dernier passage en inventaire #####
# TODO : fusionner get_last_cycle (AFI) et get_last_cycle (GF)
# get_last_cycle <- function(
#   object_list = NULL, 
#   forest_list = NULL
# ) {
#   # -- initialisation
#   tmp <- c()
#   all_cycle_list <- c()
#   
#   # -- liste des numéros de dispositifs
#   num_list <- as.numeric( str_sub(forest_list, 1, str_locate(forest_list, "-")[, 1] - 1) )
#   
#   # -- boucle
#   for (i in 1:length(object_list)) {
#     # i = 10 # debug
#     # print(i) # debug
#     tmp <- 
#       if (class(object_list) == "list") object_list[[i]] else {
#         get(object_list[i], envir = parent.frame())
#       }
#     
#     # cas où tmp est un data.frame
#     if ("data.frame" %in% class(tmp)) {
#       if ("NumForet" %in% names(tmp) && "Cycle" %in% names(tmp)) {
#         tmp <- 
#           tmp %>% 
#           filter(NumForet %in% num_list) %>% 
#           select(Cycle) %>% 
#           distinct() %>% unlist() %>% unname()
#         tmp <- tmp[!tmp %in% all_cycle_list]
#         all_cycle_list <- c(all_cycle_list, tmp)
#       }
#     }
#     
#     # cas où tmp est une liste (ex : TabPla)
#     # print(class(tmp)) # debug
#     if ("list" %in% class(tmp)) {
#       for (i in 1:length(tmp)) {
#         tab <- tmp[[i]]
#         if ("NumForet" %in% names(tab) && "Cycle" %in% names(tab)) {
#           tab <- 
#             tab %>% 
#             filter(NumForet %in% num_list) %>% 
#             select(Cycle) %>% 
#             distinct() %>% unlist() %>% unname()
#           tab <- tab[!tab %in% all_cycle_list]
#           all_cycle_list <- c(all_cycle_list, tab)
#         }
#       }
#     }
#   }
#   all_cycle_list <- sort( as.numeric( unique(all_cycle_list) ) )
#   
#   # -- numéro du dernier cycle
#   last_cycle <- max(all_cycle_list, na.rm = T)
#   
#   # -- retour de la fonction get_last_cycle
#   return(last_cycle)
# }

##### fonction pour créer des objets nuls (package) #####
create_null <- function(objects_list) {
  for (obj in objects_list) assign(obj, NULL, envir = parent.frame())
}

##### fonction de nettoyage compilation pdf #####
clean_after_knit <- function(output) {
  # file.remove(output)
  if (exists(gsub(".tex", ".aux", output))) file.remove(gsub(".tex", ".aux", output))
  if (exists(gsub(".tex", ".log", output))) file.remove(gsub(".tex", ".log", output))
  if (exists(gsub(".tex", ".out", output))) file.remove(gsub(".tex", ".out", output))
  file.remove(output)
}

##### fonction pour construire la table des combinaisons de données à obtenir #####
build_combination_table <- function(vecteur = NULL) {
  df <- data.frame(
    var = vecteur,
    stringsAsFactors = F
  ) %>% 
    mutate(
      # essences
      Var1 = ifelse(str_detect(var, "Essence"), "Essence", NA),
      Var1 = ifelse(str_detect(var, "EssReg"), "EssReg", Var1),
      # diamètre
      Var2 = ifelse(str_detect(var, "Classe"), "Classe", NA),
      Var2 = ifelse(str_detect(var, "Cat"), "Cat", Var2),
      # stades bois mort
      Var3 = ifelse(str_detect(var, "StadeD"), "StadeD", NA),
      Var4 = ifelse(str_detect(var, "StadeE"), "StadeE", NA),
      # qualités
      Var5 = ifelse(str_detect(var, "Qual"), "Qual", NA),
      Var5 = ifelse(str_detect(var, "Qual1"), "Qual1", Var5),
      Var5 = ifelse(str_detect(var, "Qual2"), "Qual2", Var5),
      # type de bmp
      Var6 = ifelse(str_detect(var, "Type"), "Type", NA),
      # durée de vie (carbone)
      Var6 = ifelse(str_detect(var, "Lifetime"), "Lifetime", Var6),
      # DMH
      Var6 = ifelse(str_detect(var, "CodeEcolo"), "CodeEcolo", Var6),
      # coupe et rejet
      Var7 = ifelse(str_detect(var, "Coupe"), "Coupe", NA),
      Var7 = ifelse(str_detect(var, "Rejet"), "Rejet", Var7),
      # populations
      Data = ifelse(str_detect(var, "BM"), "BM", NA),
      Data = ifelse(str_detect(var, "BMP"), "BMP", Data),
      Data = ifelse(str_detect(var, "BMS"), "BMS", Data),
      Data = ifelse(str_detect(var, "Fpied"), "Fpied", Data),
      Data = ifelse(str_detect(var, "Per"), "Per", Data),
      Data = ifelse(str_detect(var, "Taillis"), "Taillis", Data),
      # Data = ifelse(str_detect(var, "Den"), "Den", Data),
      Data = ifelse(str_detect(var, "PFutaie"), "PFutaie", Data),
      Data = ifelse(str_detect(var, "Exploit"), "Exploit", Data),
      Data = ifelse(str_detect(var, "Codes"), "Codes", Data),
      Data = ifelse(str_detect(var, "Carbone"), "Carbone", Data),
      Data = ifelse(str_detect(var, "Rege"), "Rege", Data),
      var = NULL
    ) %>% 
    filter(!is.na(Data)) %>% # sécurité
    distinct()
  
  # retour de la fonction build_combination_table
  return(df)
}
##### /\ #####

##### fonction pour créer des data.frame vides (package) #####
create_empty_df <- function(objects_list) {
  for (obj in objects_list) assign(obj, data.frame(), envir = parent.frame())
}

##### test impact de l'angle des billons de bois mort sur les transects #####
# df <- 
#   expand.grid(
#     Diam = seq(5, 30, 5),
#     Angle = seq(0, 80, 10)
#   ) %>% 
#   mutate(
#     Vha = pi ^ 2 / 8 / 60 * Diam ^ 2 / cos(Angle / 180 * pi),
#     Angle = factor(Angle)
#   )
# 
# ggplot() + 
#   geom_line(
#     df, 
#     mapping = aes(Diam, Vha, colour = Angle)
#   )
