# rm(list = ls())
# repGF <- getwd()
repGF <- "/Users/Valentin/Travail/Outils/GitHub/PermAFI-ShinyApp"

# Installation/Activation des packages nécessaires
library(easypackages)
# suppressMessages(
packages(
  "stringr", "openxlsx", "rmarkdown", "tools",
  "tidyr", "dplyr", "gWidgets2", "gWidgets2tcltk", "knitr", "maptools",
  "xtable", "ggplot2", "ggrepel", "ggthemes", "scales", "gridExtra",
  "rgeos", "rgdal", "gdata", "grid", "fmsb", "rlang",
  "readxl"
)
# )
# library(PermAFI)

translator <- shiny.i18n::Translator$new(translation_json_path = file.path(repGF, "www/translations/translation.json"))
translator$set_translation_language("Français")
# -- i18n reactive function
i18n <- function() {
  translator
}

# chargement du script annexes
source(
  file.path(repGF, "scripts/annexes.R"), 
  encoding = 'UTF-8', echo = FALSE
)
source(
  file.path(repGF, "scripts/gf_ClasseurRem.R"), 
  encoding = 'UTF-8', echo = FALSE
)
source(
  file.path(repGF, "scripts/gf_XlsTranslation.R"), 
  encoding = 'UTF-8', echo = FALSE
)

# TODO : checker les doublons dans les arbres (évite de renuméroter après-coup les arbres si correction nécessaire)

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
    CodeEcolo = tmp
    )
    
  # -- return of 'check_dmh_for_standing_deadwood'
  return(arbres_table)
}

##### fonction pour contrôler la colonne Coupe #####
check_cut_trees <- function(arbres_table = NULL, check_stems = F) {
  
  # -- last_cycle
  last_cycle <- max(arbres_table$Cycle, na.rm = T)
  
  # -- follow living trees through all inventories
  missing_living_trees <-
    arbres_table %>%
    # select living trees
    filter(is.na(Type)) %>% 
    # select trees with NumArbre only
    filter(!is.na(NumArbre)) %>% 
    
    # order
    arrange(NumDisp, NumPlac, Azimut, Distance, Cycle) %>% 
    group_by(NumDisp, NumPlac, NumArbre)
  
  # filter
  # - stems
  add_str <- "_after_stack"
  if (check_stems == F) {
    missing_living_trees <- 
      missing_living_trees %>% filter((Diam1 + Diam2) / 2 > 17.5)
    add_str <- c()
  }
  # - others
  missing_living_trees <-
    missing_living_trees %>% 
    filter(
      # # exclusion des perches (suivi Coupe impossible)
      # # -> VD 04/04/2022 : vrai que suivi pas toujours possible mais
      # # contrôler malgré tout les erreurs listées (permet de reconstituer le suivi
      # # des perches mal reprises (ex : perche à 7,09 m devenant petit bois à 7,1m))
      # (Diam1 + Diam2) / 2 > 17.5 &
        
      !is.na(Diam1) & # à un inventaire donné, l'arbre a été mesuré
        is.na( lead(Diam1) ) & # à l'inventaire suivant, l'arbre n'existe plus
        is.na(Coupe)
    ) %>% 
    group_by(NumDisp, NumPlac) %>% 
    filter(Cycle != last_cycle) %>%  # filtrer pour ne pas prendre en compte le dernier inventaire (car tous les arbres inventoriés au dernier cycle passeraient le filtre))
    ungroup()
  
  # -- follow dead trees through all inventories
  missing_dead_trees <-
    arbres_table %>%
    # select living trees
    filter(!is.na(Type)) %>% 
    # select trees with NumArbre only
    filter(!is.na(NumArbre)) %>% 
    
    # order
    arrange(NumDisp, NumPlac, Azimut, Distance, Cycle) %>% 
    group_by(NumDisp, NumPlac, NumArbre) %>%
    filter(
      # exclusion des perches (suivi Coupe impossible)
      (Diam1 + Diam2) / 2 >= 17.5 & 
        
      !is.na(Diam1) & # à un inventaire donné, l'arbre a été mesuré
        is.na( lead(Diam1) ) & # à l'inventaire suivant, l'arbre n'existe plus
        is.na(Coupe)
    ) %>% 
    group_by(NumDisp, NumPlac) %>% 
    # filtrer pour ne pas prendre en compte le dernier inventaire (car tous les arbres inventoriés au dernier cycle passeraient le filtre))
    filter(Cycle != last_cycle) %>%
    ungroup()
  
  # -- binding
  missing_trees <- rbind(
    missing_living_trees,
    missing_dead_trees
  ) %>%
    distinct() %>% 
    arrange(NumDisp, NumPlac, Azimut, Distance, Cycle)
  
  # -- return of 'check_cut_trees'
  if (nrow(missing_trees) > 0) {
    # stand num
    disp_num <- unique( na.omit(missing_trees$NumDisp) )
    
    # output directory
    output_dir <- 
      paste0(
        "out/stacking_data/", disp_num
      )
    dir.create(output_dir, showWarnings = F, recursive = T)
    
    # writing
    write.xlsx(
      missing_trees, 
      file = file.path(
        output_dir, 
        paste0(
          disp_num, "-missing_cut_trees", add_str, ".xlsx"
        )
      )
    )
    
    # message
    msg <- tk_messageBox(
      type = "ok",
      message = paste0(
        "Il y a des informations manquantes dans la colonne 'Coupe' dans les relevés d'inventaire du dispositif ", 
        disp_num, 
        ".\n\nVoir le fichier '",
        disp_num, "-missing_cut_trees", add_str, ".xlsx'"
      )
    )
    # stop(
    #   "Il y a des informations manquantes dans la colonne 'Coupe' dans les relevés d'inventaire du dispositif ", 
    #   disp_num, 
    #   ".\n\nVoir le fichier '",
    #   disp_num, "-missing_cut_trees", add_str, ".xlsx'"
    # )
    
    # superassignement edit var
    # edit <<- FALSE # TODO : superassignements à suspendre le temps des tests ?
  }
  return(arbres_table)
}


##### fonction de contrôle des qualités ##### 
check_qual <- function(
  table = NULL, qual_codes = NULL
) {
  # table <- arbres_table # debug
  # stop(names(table))
  lookup_table <-  tibble(
    field_notation = c(
      qual_codes$Nom,
      "AP", "AM", "A P", "A M", 
      "BP", "BM", "B P", "B M", 
      "CP", "CM", "C P", "C M", "C ", "c", "cm",
      "DP", "DM", "D P", "D M",
      "a-", "b-", "c-", "d-", 
      "a", "b", "c", "d", 
      "a+", "b+", "c+", "d+"
    ),
    BD_notation = c(
      qual_codes$Nom,
      "A+", "A-", "A+", "A-", 
      "B+", "B-", "B+", "B-", 
      "C+", "C-", "C+", "C-", "C", "C", "C-",
      "D+", "D-", "D+", "D-", 
      "A-", "B-", "C-", "D-",
      "A", "B", "C", "D",
      "A+", "B+", "C+", "D+"
    )
  )
  
  # -- correct table
  # table[1, "Qual"] <- "E" # debug-test
  table <- 
    table %>% 
    mutate(
      corrected_qual = with(
        lookup_table,
        BD_notation[match(Qual, field_notation)]
      ),
      Qual = ifelse(
        !is.na(corrected_qual), 
        corrected_qual, 
        Qual
      ),
      corrected_qual = NULL
    )
  
  # -- manage unknown qual (update lookup_table asap)
  unknown_qual <- with(
    table, {
      # unknown qualities positions
      unknown_qual_pos <- 
        which(!is.na(Qual) & !Qual %in% qual_codes$Nom)
      # get unknown qualities
      sort( unique(Qual[unknown_qual_pos]) )
    }
  )
  
  if (length(unknown_qual) > 0) {
    # msg
    msg <- tk_messageBox(
      type = "ok", 
      message = "Il y a des qualités non reconnues (nomenclature PermAFI2) dans les données d'inventaire\n\n-> réattribuer les qualités (et mettre à jour 'lookup_table' asap)",
      icon = "error"
    )
    
    new <- NULL
    for (qual in unknown_qual) {
      # qual <- unknown_qual[1] # debug
      
      new <- tk_select.list(
        choices = qual_codes$Nom, 
        multiple = F,
        title = paste0(
          "Attribuer la qualité '", 
          qual,
          "' à l'un des choix ci-dessous"
        )
      )
      
      if (new != "") { # sécurité si aucun choix sélectionné
        table <-
          # N.B VD 200409 - pour l'instant les valeurs vides ne sont pas automatiquement remplacées
          # if (is.na(qual)) { 
          # table %>% mutate(Qual = ifelse(is.na(Qual) , new, Qual))
          # } else {
          table %>% 
          mutate(Qual = ifelse(Qual == qual , new, Qual))
        # }
      } # end of cond 'new != ""'
    } # end of loop 'unknown_qual'
  } # end of cond 'length(unknown_qual) > 0'
  
  # -- return of 'check_qual' function
  return(table)
}


##### fonction de contrôle des essences ##### 
check_species <- function(
  table = NULL, table_name = NULL, 
  species_codes = NULL
) {
  # table <- arbres_table # debug
  # table_name <- "Arbres" # debug
  # table <- bm_lineaire_table # debug
  # table_name <- "BMortLineaire"
  
  # -- lookup table
  lookup_table <-  tibble(
    field_notation = c(
      "SAP", "ALB", "FLS",
      "ERA S", "ERS",
      "ERA FO",
      "ERA C", "ERC",
      "ERA P", "ERP",
      "EPC",
      "CHENE S"
    ),
    BD_notation = c(
      "Sapin P", "Alisier B", "Feuillus",
      "Erable S", "Erable S",
      "Erable FO",
      "Erable C", "Erable C",
      "Erable P", "Erable P",
      "Epicéa C",
      "Chêne S"
    )
  )
  
  # -- correct table
  # table[1, "Essence"] <- "EXXX" # debug-test
  table <- 
    table %>% 
    mutate(
      corrected_species = with(
        lookup_table,
        BD_notation[match(Essence, field_notation)]
      ),
      Essence = ifelse(
        !is.na(corrected_species), 
        corrected_species, 
        Essence
      ),
      corrected_species = NULL
    )
  
  # -- manage unknown species
  species_list <- unique(table$Essence)
  species_list <- species_list[ !is.na(species_list) ]
  unknown_species <- species_list[ which(!species_list %in% species_codes$Essence) ]
  
  if (length(unknown_species) > 0) {
    # msg
    msg <- tk_messageBox(
      type = "ok", 
      message = paste0(
        "Il y a des essences non reconnues (table admin de codifications des Essences) dans les données d'inventaire de la table ", 
        table_name, 
        "\n\n-> réattribuer les noms d'essence"
      ),
      icon = "error"
    )
    
    new <- NULL
    for (species in unknown_species) {
      # species <- unknown_species[1] # debug
      
      # -- preselection
      # use species to get a code
      species_abbrev <- 
        str_sub(species, 1, 3) %>% 
        clean_names() %>% 
        toupper()
      # use species to get a code
      species_codes <- 
        species_codes %>% 
        mutate(
          code = toupper( 
            str_sub(Essence, 1, nchar(species_abbrev)) 
          ),
          code = clean_names(code)
        )
      
      preselected_species <- with(
        species_codes, {
          # possible species position
          possible_species_pos <- 
            match(species_abbrev, species_codes$code)
          # get unknown qualities
          sort( unique(Essence[possible_species_pos]) )
        }
      )
      
      new <- tk_select.list(
        choices = species_codes$Essence, 
        multiple = F,
        title = paste0(
          "Attribuer l'essence '", 
          species,
          "' à l'un des choix ci-dessous"
        ),
        preselect = preselected_species
      )
      
      if (new != "") { # sécurité si aucun choix sélectionné
        table <-
          # N.B VD 200409 - pour l'instant les valeurs vides ne sont pas automatiquement remplacées
          # if (is.na(species)) {
          #     table %>% mutate(Essence = ifelse(is.na(Essence) , new, Essence))
          #   } else {
          table %>% 
          mutate(
            Essence = ifelse(
              Essence == species , new, Essence
            )
          )
        # }
      } # end of cond 'new != ""'
    } # end of loop 'unknown_species'
  } # end of cond 'length(unknown_species) > 0'
  
  # return of check_species function
  return(table)
}


##### fonction pour vérifier les coordonnées des arbres #####
# 1/ vérifier qu'il n'y a pas de modification des variables Essence, Azimut et Distance au cours des cycles
# 2/ vérifier qu'il n'y a pas de doublons (2 arbres au même endroit = avec Azimut et Distance identiques)

check_tree_id <- function(
  former_inventory = NULL,
  current_inventory = NULL
) {
  # former_inventory = past_inventory$Arbres # debug
  # current_inventory = inventory$Arbres # debug
  disp_num <- unique( na.omit(current_inventory$NumDisp) )
  
  # -- sécurité : vérifier que les noms sont identiques et surtout que toutes les variables d'identifiant sont bien présentes
  id_vars <- c("NumDisp", "NumPlac", "NumArbre", "Essence", "Azimut", "Distance", "Cycle")
  missing_names <- setdiff(id_vars, names(former_inventory))
  if (length(missing_names) > 0) {
    stop(
      "les noms de colonne '", 
      paste0(id_vars, collapse = "', '"), 
      "' ne sont pas présents dans la table de l'inventaire passé"
    )
  }
  missing_names <- setdiff(id_vars, names(current_inventory))
  if (length(missing_names) > 0) {
    stop("les noms de colonne ", id_vars, " ne sont pas présents dans la table de l'inventaire actuel")
  }
  
  # -- select
  former_inventory <- former_inventory %>% select( all_of(id_vars) )
  current_inventory <- current_inventory %>% select( all_of(id_vars) )
  
  # cas envisagés :
  #   - NumArbre identique dans BD et .xls de relevés mais Essence/Azimut/Distance modifié (corrections dans le .xls lors de la remesure MAIS uniquement sur la nouvelle mesure et pas sur les données du cycle - 1)
  # <=> de manière plus générale : cas où, dans un même classeur, il y a 2 azimut/distance différents pour un même NumArbre
  #   - NumArbre identique dans BD et .xls de relevés mais Essence/Azimut/Distance modifié (corrections dans le .xls lors de la remesure sur la nouvelle mesure ET sur les données du cycle - 1)
  # <=> de manière plus générale : cas où, dans 2 classeurs différents, il y a 2 azimut/distance différents pour un même NumArbre
  
  # -- construction de la table des identifiants (arbres)
  id <- 
    # rassemble les données d'inventaire
    rbind(
      former_inventory, 
      current_inventory
    ) %>% 
    # construction de l'identifiant
    filter(!is.na(NumArbre)) %>% # supprime les arbres "nouveaux"
    select(NumDisp, NumPlac, NumArbre) %>% 
    distinct()
  
  # -- set up
  former_inventory <- 
    former_inventory %>% 
    # suppression des perches des premiers inventaires non repérées Azimut + Distance
    filter(!(is.na(NumArbre) & is.na(Azimut) & is.na(Distance))) %>% 
    mutate(former_inventory = T)
  current_inventory <- 
    current_inventory %>% 
    mutate(current_inventory = T) %>% 
    filter(!is.na(NumArbre))
  
  # -- check inventory table
  check_inventory <- 
    id %>%
    full_join(
      former_inventory, 
      by = c("NumDisp", "NumPlac", "NumArbre")
    ) %>% 
    select(id_vars, "Cycle", "former_inventory") %>% 
    full_join(
      current_inventory %>% filter(!is.na(NumArbre)), 
      by = c("NumDisp", "NumPlac", "NumArbre", "Essence", "Azimut", "Distance", "Cycle")
    ) %>% 
    select(id_vars, "Cycle", "former_inventory", "current_inventory") %>% 
    # filtre des arbres tout juste rentrés dans l'inventaire 
    filter(!(is.na(Essence) & is.na(Azimut) & is.na(Distance) & is.na(former_inventory))) %>% 
    distinct(
      NumDisp, NumPlac, NumArbre, 
      Essence, Azimut, Distance, 
      .keep_all = T
    ) %>% 
    add_count(NumDisp, NumPlac, NumArbre) %>% 
    # select(-Cycle) %>% 
    filter(n > 1) %>% 
    arrange(NumDisp, NumPlac, NumArbre, Cycle)
  
  # -- return of check_tree_id function
  if (nrow(check_inventory) > 0) {
    # output directory
    output_dir <- 
      paste0(
        "out/stacking_data/", disp_num
      )
    dir.create(output_dir, showWarnings = F, recursive = T)
    
    # writing
    write.xlsx(
      check_inventory, 
      file = file.path(
        output_dir, 
        paste0(
          disp_num, "-moving_trees.xlsx", collapse = "-"
        )
      )
    )
    
    # msg
    tk_messageBox(
      type = "ok",
      message = paste0(
        "Il y a des notations 'Essence', 'Azimut' et/ou 'Distance' incohérents entre les différents inventaires.\nVoir le fichier ",
        disp_num, "-moving_trees.xlsx"
      )
    )
    warning(
      "Il y a des notations 'Essence', 'Azimut' et/ou 'Distance' incohérents entre les différents inventaires.\nVoir le fichier ",
      disp_num, "-moving_trees.xlsx"
    )
    edit <<- FALSE
  }
}


##### fonction pour vérifier les numéros/renuméroter des arbres #####
# principe : au premier cycle, on trie par azimut+distance 
# croissants puis les nouveaux arbres incrémentent le n° max 
# de NumArbre. Si passage à la futaie, les perches (numéros 
# 100 & Cie) sont renumérotées suivant ce même principe.

set_tree_num <- function(arbres_table = NULL, disp_num = NULL) {
  # arbres_table <- arbres_table # debug
  # -- setup
  last_cycle <- max(arbres_table$Cycle, na.rm = T)
  disp_num <- unique( na.omit(arbres_table$NumDisp) )
  
  # -- sécurité pour détecter les changements de coordonnées des arbres - fait dans check_tree_id
  
  # -- sécurité pour détecter les arbres sans Diam1 (car oubli) -> impossible à classer en PREC ou PER...
  missing_diam1 <- with(
    arbres_table,
    which(is.na(Diam1) & !(is.na(Diam2) & is.na(Qual)))
  )
  if (length(missing_diam1) > 0) {
    # output directory
    output_dir <-
      paste0(
        "out/stacking_data/", disp_num
      )
    dir.create(output_dir, showWarnings = F, recursive = T)
    
    # writing
    disp_num <- unique(arbres_table$NumDisp)
    output_filename <- paste0(disp_num, "-missing_diam1.xlsx")
    write.xlsx(
      arbres_table[missing_diam1,], 
      file = file.path(
        output_dir, output_filename
      )
    )
    
    # stop msg
    stop("Il y a des arbres (avec les colonnes 'Diam2' et/ou 'Qual' renseignés) pour lesquels il manque le 'Diam1' : voir le fichier missing_diam1.xlsx")
  }
  
  # -- initiate data table - 1er cycle
  table <- 
    arbres_table %>% 
    filter(
      Cycle == 1 & 
        # exclude trees without Azimut or Distance
        !is.na(Azimut) & !is.na(Distance)
    ) %>% 
    arrange(NumDisp, NumPlac, Azimut, Distance) %>% 
    group_by(NumDisp, NumPlac) %>% 
    mutate(
      NumArbre = row_number()
    ) %>% 
    ungroup()
  
  # -- id table
  id <- 
    table %>% 
    select(NumDisp, NumPlac, NumArbre, Azimut, Distance) %>% 
    group_by(NumDisp, NumPlac) %>% 
    mutate(max_tree_num = max(NumArbre, na.rm = T)) %>% 
    ungroup() %>% 
    distinct()
  
  
  for (cycle in 2:last_cycle) { # loop 'cycle in 2:last_cycle'
    # cycle = 2 # debug
    # -- cycle filtered data table
    table_cycle <- 
      arbres_table %>% 
      filter(
        Cycle == cycle &
          # exclude trees without Azimut or Distance
          !is.na(Azimut) & !is.na(Distance)
      ) %>% 
      arrange(NumDisp, NumPlac, Cycle, Azimut, Distance)
    
    # -- joining id and data table
    table_tmp <- 
      right_join(
        id %>% mutate(Cycle = cycle),
        table_cycle %>% select(-NumArbre), 
        by = c("NumDisp", "NumPlac", "Cycle", "Azimut", "Distance")
      ) %>% 
      group_by(NumDisp, NumPlac) %>% 
      mutate(
        max_tree_num = max(max_tree_num, na.rm = T),
        
        # manage missing stem/standing tree cases
        max_tree_num = ifelse(
          is.na(max_tree_num), 0, max_tree_num
        )
      ) %>% 
      
      # count
      group_by(NumDisp, NumPlac, NumArbre) %>% 
      mutate(
        count = row_number(),
        tree_num_updated = ifelse(
          is.na(NumArbre), max_tree_num + count, NumArbre
        )
      ) %>% 
      ungroup() %>% 
      mutate(
        NumArbre = tree_num_updated
      ) %>% 
      select(names(table))
    
    # -- stack former and new cycle data tables
    table <- 
      table %>% 
      rbind(table_tmp) %>% 
      arrange(NumDisp, NumPlac, Azimut, Distance)
    
    # -- update id table
    id <- 
      table %>% 
      select(NumDisp, NumPlac, NumArbre, Azimut, Distance) %>% 
      group_by(NumDisp, NumPlac) %>% 
      mutate(max_tree_num = max(NumArbre, na.rm = T)) %>% 
      ungroup() %>% 
      distinct()
  } # end of loop 'cycle in 2:last_cycle'
  
  # -- stack data without NumArbre
  empty_tree_num_table <- 
    arbres_table %>% 
       # only trees without Azimut or Distance
    filter( is.na(Azimut) | is.na(Distance) ) %>% 
    # set NumArbre et Coupe empty - pas possible de suivre des arbres non géolocalisés
    mutate(NumArbre = NA, Coupe = NA)
  table <- rbind(
    table, 
    empty_tree_num_table
  ) %>% 
    arrange(NumDisp, NumPlac, Azimut, Distance, Cycle)
  
  # -- return function 'set_tree_num'
  return(table)
}

# # styles -----
# # Construction des styles des différentes feuilles du classeur d'inventaire
# # TODO : faire 1 seul style global sur lequel viennent se stacker d'autres styles
# style_title_id <- createStyle(
#   # police texte
#   fontName = "Arial", fontSize = 12, 
#   fontColour = "dodgerblue4", textDecoration = "bold", 
#   # fond cellule
#   fgFill = "tan1", 
#   # bordure
#   border =  "LeftBottomRight",
#   # alignement
#   valign = "top", halign = "center",
#   # rotation et wrap
#   textRotation = 90, wrapText = F
# )
# style_title_attr <- createStyle(
#   # police texte
#   fontName = "Arial", fontSize = 12, 
#   textDecoration = "bold", 
#   # fond cellule
#   fgFill = "mediumaquamarine", 
#   # bordure
#   border =  "LeftBottomRight",
#   # alignement
#   valign = "top", halign = "center",
#   # rotation et wrap
#   textRotation = 90, wrapText = F
# )
# style_title_comments <- createStyle(
#   # police texte
#   fontName = "Arial", fontSize = 12, 
#   textDecoration = "bold", 
#   # fond cellule
#   fgFill = "mediumaquamarine", 
#   # bordure
#   border =  "LeftBottomRight",
#   # alignement
#   valign = "center", halign = "center",
#   # rotation et wrap
#   textRotation = 0, wrapText = F
# )
# # style_title4 <- createStyle(
# #   # police texte
# #   fontName = "Arial", fontSize = 12, 
# #   textDecoration = "bold", 
# #   # fond cellule
# #   fgFill = "lightgoldenrod", 
# #   # bordure
# #   border =  "LeftBottomRight",
# #   # alignement
# #   valign = "center", halign = "center",
# #   # rotation et wrap
# #   textRotation = 0, wrapText = F
# # )
# # style_general <- createStyle(
# #   # police texte
# #   fontName = "Arial", fontSize = 10, 
# #   # bordure
# #   border =  "LeftRight",
# #   # alignement
# #   valign = "center", halign = "center"
# # )
# # # style_general2 <- createStyle(
# # #   # police texte
# # #   fontName = "Arial", fontSize = 10, 
# # #   # bordure
# # #   border =  "LeftBottomRight",
# # #   # alignement
# # #   valign = "center", halign = "center"
# # # )
# 
# # separate_row avec stack ?
# style_separate_medium_row <- createStyle(
#   # bordure
#   border = "Bottom", borderStyle = "medium"
# )
# 
# styles_list <- c(
#   style_title_id, style_title_attr, style_title_comments, #style_title4,
#   style_general
# )
# names(styles_list) <- c(
#   "style_title_id", "style_title_attr", "style_title_comments", #"style_title4",
#   "style_general", "style_separate_medium_row"
# )
# # load("wb_styles.Rdata") TODO
# # ----
# load("tables/wb_styles.Rdata")


##### fonction pour contrôler les feuilles du classeur d'inventaire #####
check_workbook_sheets <- function(
  file = NULL, needed_sheets = NULL
) {
  # -- paramètres
  # feuilles du classeur d'inventaire
  workbook_sheets <- excel_sheets(file)
  
  # nombre de feuilles demandées
  sheets_nb <- length(needed_sheets)
  
  # feuilles manquantes
  missing_sheets <- needed_sheets[ which(!needed_sheets %in% workbook_sheets) ]
  if (length(missing_sheets) > 0) {
    stop(
      "Les noms d'onglet du classeur Excel en import ne sont pas corrects.\n\nRappel : liste des onglets devant figurer dans le classeur (",
      sheets_nb, " au total) =\n'", 
      paste0(needed_sheets, collapse = "','"),
      "'\nIl manque les onglets :\n", 
      paste0(missing_sheets, collapse = ", ")
    )
  }
}


##### fonction pour contrôler les colonnes des feuilles du classeur d'inventaire #####
check_table_columns <- function(
  table = NULL, table_name = NULL, columns_names = NULL
) {
  # -- paramètres
  # feuilles du classeur d'inventaire
  table_columns <- names(table)
  
  # nombre de feuilles demandées
  columns_nb <- length(columns_names)
  
  # feuilles manquantes
  missing_columns <- columns_names[ which(!columns_names %in% table_columns) ]
  if (length(missing_columns) > 0) {
    stop(
      "Les intitulés de colonne de la feuille '", table_name, "' du classeur Excel en import ne sont pas corrects.\n",
      "Il manque la/les colonne(s) suivante(s) : ", 
      paste0(missing_columns, collapse = ", ")
    )
  }
}


##### function to stack new and former inventory #####
# TODO : officialiser la fonction et la redécouper ?
stack_inventory <- function(
  repGF = NULL, file_path = NULL
) {
  # -- initialize
  # wd
  setwd(repGF)
  
  # loading data# -- import past inventory ?
  ans <- tk_messageBox(
    type = "yesno",
    message = "Importer l'ancien classeur d'inventaire ?"
  )
  if (ans == "yes") {
    # wd - calé par défaut sur le dossier PermAFI2
    repGF <- getwd()
    # file path
    file_path <- 
      tk_choose.files(
        caption = "Choix du classeur d'inventaire contenant les anciens relevés",
        multi = T,
        filters = matrix(
          data = c("fichier d'inventaire",".xlsx"),
          nrow = 1, ncol = 2, byrow = T
        )
      )
    if (!length(file_path)) {stop("Import des anciens relevés interrompu - aucun fichier sélectionné")}
    # call
    gf_XlsTranslation(repGF, file_path)
  }
  arch1 = "tables/gfDonneesBrutes.Rdata"
  arch2 = "tables/gfCodes.Rdata"
  arch3 = "tables/gfDictionary.Rdata"
  arch_styles = "tables/wb_styles.Rdata"
  load(arch1)
  load(arch2)
  load(arch3)
  load(arch_styles)
  names(styles_list)[1:3] <- c("style_title1", "style_title2", "style_title3")
  Arbres <- left_join(IdArbres, ValArbres, by = "IdArbre")
  
  # set up
  edit = T
  
  ##### 1/ Import des données d'inventaire actuel et passés #####
  # -- import file
  # if (is.null(file_path)) {
  file_path <- 
    tk_choose.files(
      caption = "Choix du classeur d'inventaire contenant les nouveaux relevés",
      multi = T,
      filters = matrix(
        data = c("fichier d'inventaire",".xls","fichier d'inventaire",".xlsx"),
        nrow = 2, ncol = 2, byrow = T
      )
    )
  if (length(file_path) == 0) stop("Traitement interrompu - aucun classeur sélectionné")
  # }
  
  # -- table de référence contenant les noms des feuilles (permettra de trouver la langue)
  # TODO : remplacer par une archive avec les noms de feuilles / tables ?
  sheet_names_table <-
    dictionary %>% filter(Emplacement == "Inventaire") # TODO : mettre une sécurité ici aussi ?
  
  # -- define file dictionary
  file_dictionary <- 
    dictionary %>% 
    filter(Feuille %in% sheet_names_table$Attribut_FRA) %>% 
    mutate(original = Attribut_FRA)
  
  # -- get input lang
  input_lang <- get_language_from_wb(file_path, sheet_names_table = sheet_names_table) # , i18n = i18n
  
  # -- update file_dictionary
  if (input_lang != "FRA") {
    file_dictionary <- 
      file_dictionary %>% 
      # create 1 column 'original' <=> 'paste0("Attribut_", input_lang)'
      mutate(original = !!sym(paste0("Attribut_", input_lang))) %>% 
      # select the only 2 languages needed
      select(Feuille, Emplacement, Attribut_FRA, original) %>% 
      # join sheet names table to translate 'Feuille' and reduce dictionary dims
      right_join(
        sheet_names_table %>% select(Attribut_FRA, !!sym(paste0("Attribut_", input_lang))),
        by = c("Feuille" = "Attribut_FRA")
      ) %>% 
      mutate(Feuille = !!sym(paste0("Attribut_", input_lang)))
  }
  file_dictionary <- 
    file_dictionary %>% select(Feuille, Emplacement, Attribut_FRA, original)
  
  # needed sheets in the wb
  needed_sheets <- unique(file_dictionary$Feuille)
  
  # # define lookup table for columns # To suppress
  # names_table <- 
  #   dictionary %>% 
  #   filter(Feuille %in% needed_sheets & Emplacement == "ColName")
  
  # -- import des données du nouvel inventaire (format .xls ordinateur terrain)
  # checking sheets names
  check_workbook_sheets(file_path, needed_sheets)
  
  # reading
  inventory <- list()
  all_num_list <- c()
  for (sheet in needed_sheets) { # loop 'sheet in needed_sheets'
    # print(sheet) # debug
    # sheet = "BMortLineaire" # debug
    # sheet = "Arbres" # debug
    # sheet = "Rege" # debug
    # sheet = "Trees" # debug
    
    # define columns_names
    columns_names <- 
      with(file_dictionary, Attribut_FRA[Feuille == sheet & Emplacement == 'ColName'])
    # # read .xls
    # inventory[[sheet]] <- tryCatch(
    #   read_xls(file_path, sheet = sheet),
    #   warning = function(cond) {stop(
    #     "La lecture de la feuille '", sheet, 
    #     "' du classeur contenant les nouveaux relevés renvoie le warning suivant :\n", 
    #     cond, "\nVérifiez les données de la feuille '", sheet, 
    #     "' aux lignes indiquées dans le warning (erreur possible : format date dans une cellule numérique ?)"
    #   )
    #     return(NULL)}
    # )
    tmp <- read_xlsx(
      file = file_path, 
      sheet_name = sheet, 
      input_lang = input_lang,
      file_dictionary = file_dictionary,
      dictionary = dictionary, # si besoin puor traduction des essences
      i18n = i18n
    )
    # translate sheet
    sheet <- sheet_names_table$Attribut_FRA[match(sheet, sheet_names_table[, paste0("Attribut_", input_lang)])]
    inventory[[sheet]] <- tmp
    
    # checking columns names
    check_table_columns(
      table = inventory[[sheet]], table_name = sheet, columns_names
    )
    
    # format
    inventory[[sheet]] <- 
      inventory[[sheet]] %>% 
      select( all_of(columns_names) ) %>% 
      # tidy tmp
      tidy_gf_table(table_name = sheet, column_types = column_types)
    
    # get disp numbers through all inventory tables
    if ("NumDisp" %in% names(inventory[[sheet]])) {
      new_num <- 
        inventory[[sheet]] %>% 
        select(NumDisp) %>% 
        filter(!is.na(NumDisp)) %>% 
        distinct() %>% 
        unlist() %>% 
        unname()
      
      new_num <- new_num[!new_num %in% all_num_list]
      all_num_list <- c(all_num_list, new_num)
    }
  } # end of loop 'sheet in needed_sheets'
  all_num_list <- sort( as.numeric( unique(all_num_list) ) )
  
  # security : different stand numbers
  if (length(all_num_list) > 1) {
    stop("plusieurs numéros de dispositifs ont été détectés dans le classeurs des nouveaux relevés d'inventaire")
  }
  disp_num <- all_num_list
  
  # security empty NumDisp dans la feuille Arbres
  if (NA %in% unique(inventory$Arbres$NumDisp)) { # cond 'NA %in% unique(inventory$Arbres$NumDisp)'
    msg <- 
      tk_messageBox(
        type = "ok",
        message = paste0(
          "Il y a des valeurs vides dans la colonne NumDisp du classeur de remesure ",
          basename(file_path)
        )
      )
    stop(
      "Il y a des valeurs vides dans la colonne NumDisp du classeur de remesure ",
      basename(file_path)
    )
  } # end of cond 'NA %in% unique(inventory$Arbres$NumDisp)'
  
  # -- load past inventory data
  # update needed sheets list
  needed_sheets <- unique(sheet_names_table$Attribut_FRA)
  # update file dictionary
  file_dictionary <- 
    dictionary %>% 
    filter(Feuille %in% sheet_names_table$Attribut_FRA) %>% 
    mutate(original = Attribut_FRA)
  
  # replacement string
  replacement <- c("Reges", "BMortLineaires", "Coords")
  
  # strings to replace
  names(replacement) <- c("Rege", "BMortLineaire", "Coord")
  
  # call str_replace_all
  needed_tables <- str_replace_all(string = needed_sheets, pattern = replacement)
    
  past_inventory <- list()
  # tables_names <- 
  #   c("Arbres", "Taillis", "Reges", "BMortLineaires", "BMortSup30", "Coords")
  # # security
  # if (length(tables_names) != length(needed_sheets)) {stop("tables_names and needed_sheets are not the same length")}
  
  # filter tables
  for (i in 1:length(needed_sheets)) { # loop 'i in 1:length(needed_sheets)'
    # i = 1 # debug
    columns_names <- 
      with(file_dictionary, Attribut_FRA[Feuille == needed_sheets[i]])
    past_inventory[[needed_sheets[i]]] <- 
      get(needed_tables[i]) %>% select(any_of(c(columns_names, "IdArbre")))
    
    # extract data with disp number
    if ("NumDisp" %in% names(past_inventory[[ needed_sheets[i] ]])) {
      past_inventory[[ needed_sheets[i] ]] <- 
        past_inventory[[ needed_sheets[i] ]] %>% filter(NumDisp == disp_num)
      
      # TODO : sécurité si données pas en archive dans la feuille Arbres
      if (
        needed_sheets[i] == "Arbres" &
        dim(past_inventory[[ needed_sheets[i] ]])[1] == 0
      ) {
        stop("Il n'y a pas de données d'inventaire en archive pour le dispositif correspondant au classeur '", basename(file_path), "'")
      }
    }
  } # end of loop 'i in 1:length(needed_sheets)'
  
  
  ##### 2/ Vérification de la cohérence des données d'inventaire actuel et passés #####
  # -- format
  # columns to round
  columns_to_round <- c("HautT", "HautL", "Ray1", "Dh1", "Ray2", "Dh2")
  # past inventory
  past_inventory$Arbres[, columns_to_round] <- lapply(
    past_inventory$Arbres[, columns_to_round],
    # round values
    FUN = function(x) round(x, 2)
  )
  # current inventory
  inventory$Arbres[, columns_to_round] <- lapply(
    inventory$Arbres[, columns_to_round],
    # round values
    FUN = function(x) round(x, 2)
  )
  
  # -- check tree id
  check_tree_id(
    former_inventory = past_inventory$Arbres,
    current_inventory = inventory$Arbres
  )
  
  
  ##### 3/ Rassemblement des données #####
  # -- stacking data
  # filter past_inventory$Arbres with last_cycle
  past_inventory$Arbres <-
    past_inventory$Arbres %>%
    group_by(NumDisp) %>%
    filter(Cycle < max(Cycle, na.rm = T)) %>%
    ungroup() %>%
    select(-IdArbre)
  
  # filter inventory$Arbres with empty rows (missing trees)
  inventory$Arbres <-
    inventory$Arbres %>%
    filter(! (is.na(Essence) & is.na(Azimut) & is.na(Distance)) ) %>%
    filter(! (is.na(Diam1) & is.na(Diam2) & is.na(Qual)) )
  
  arbres_table <-
    rbind(
      past_inventory$Arbres,
      inventory$Arbres
    ) %>%
    # TODO : to test
    check_dmh_for_standing_deadwood() %>% 
    # filter(!(is.na(Essence) & is.na(Azimut) & is.na(Distance))) %>%
    arrange(NumDisp, NumPlac, Azimut, Distance) %>%
    # filter(!is.na(IdArbre)) %>% # security - NO ! sinon élimine les éléments du dernier inventaire
    select(
      NumDisp, Cycle, NumPlac, NumArbre, Essence, Azimut, Distance,
      Diam1, Diam2, HautT, HautL, Ray1, Dh1, Ray2, Dh2, Qual,
      Observations, CodeEcolo, CodeEcoloAncien, Type, Stade, Coupe
    )
  
  if (edit) { # cond 'edit'
    arbres_table <- 
      arbres_table %>% 
      # -- renumérotation des tiges
      # voir plus haut : sécurité si changement essence azimut distance (faite avant renumérotation des arbres)
      set_tree_num() %>%
      check_qual(qual_codes = Qual) %>%
      check_species(
        table_name = "Arbres", 
        species_codes = Essences
        ) %>%
      check_cut_trees() %>% 
      # convert observations to sentence
      mutate(Observations = str_to_sentence(Observations))
    
    # taillis
    taillis_table <- 
      past_inventory$Taillis %>% 
      rbind(inventory$Taillis) %>% 
      check_species(
        table_name = "Taillis", 
        species_codes = Essences
      ) %>% 
      mutate(
        # convert observations to sentence
        Observations = str_to_sentence(Observations),
        # gestion de la colonne Nbre
        Nbre = ifelse(is.na(Nbre), 1, Nbre)
      ) %>%
      # select
      select(
        NumDisp, Cycle, NumPlac, Essence, Azimut, Distance, 
        Nbre, Diam, Haut, Observations
      )
    
    # régénération
    reges_table <- 
      past_inventory$Rege %>% 
      rbind(inventory$Rege) %>% 
      check_species(
        table_name = "Rege", 
        species_codes = Essences
      ) %>% 
      # convert observations to sentence
      mutate(Observations = str_to_sentence(Observations)) %>%
      # filter empty values
      filter( !(
        is.na(Essence) & is.na(Recouv) & 
          is.na(Class1) & is.na(Class2) & is.na(Class3) & 
          is.na(Observations)
      ) ) %>% 
      # select
      select(
        NumDisp, NumPlac, Cycle, SsPlac, Essence, 
        Recouv, Class1, Class2, Class3, Rejet, Abroutis, 
        Observations
      )
    
    # bois mort au sol < 30
    bm_lineaire_table <- 
      past_inventory$BMortLineaire %>% 
      rbind(inventory$BMortLineaire) %>% 
      check_species(
        table_name = "BMortLineaire", 
        species_codes = Essences
      ) %>% 
      # convert observations to sentence
      mutate(Observations = str_to_sentence(Observations)) %>%
      # filter empty values
      filter( !(
        is.na(Essence) & is.na(Diam) & is.na(Observations)
      ) ) %>% 
      # select
      select(
        NumDisp, Cycle, NumPlac, Transect, Essence, 
        Diam, Angle, Contact, Chablis, Stade, Observations
      )
    
    # bois mort au sol > 30
    bm_sup30_table <- 
      past_inventory$BMortSup30 %>% 
      rbind(inventory$BMortSup30) %>% 
      check_species(
        table_name = "BMortSup30", 
        species_codes = Essences
      ) %>% 
      # convert observations to sentence
      mutate(Observations = str_to_sentence(Observations)) %>%
      # select
      select(
        NumDisp, NumPlac, Cycle, #Id, NumArbre, 
        Essence, #Azimut, Distance, #Orientation, 
        DiamIni, DiamFin, DiamMed, Longueur, Contact, Chablis, 
        Stade, Observations
      )
    
    # coords
    coords_table <- 
      past_inventory$Coord %>% 
      rbind(inventory$Coord) %>% 
      distinct() %>% 
      # convert observations to sentence
      mutate(Observations = str_to_sentence(Observations)) %>%
      # select
      select(
        NumDisp, NumPlac, X, Y, SystGPS, 
        CoordGPS1, CoordGPS2, Observations # TODO : supprimer CoordGPS1 et CoordGPS2
      )
    
    
    ##### 4/ Préparation de l'édition du nouveau classeur #####
    # -- définition des niveaux de style
    # niveau 1
    style_level_1 <- c("NumDisp", "Cycle", "NumPlac", "NumArbre", "SsPlac", "Id")
    # niveau 2
    style_level_2 <- c(
      "Transect", "Essence", "Azimut", "Distance", "Orientation", 
      "Nbre", "Diam", "Diam1", "Diam2", "DiamIni", "DiamMed", "DiamFin", 
      
      "Haut", "HautT", "HautL", "Ray1", "Dh1", "Ray2", "Dh2", 
      "Qual", 
      "Longueur", "Angle", "Contact", "Chablis", 
      "Recouv", "Class1", "Class2", "Class3", #"Total", 
      "Taillis", "Limite", "CodeEcolo", 
      "CodeEcoloAncien", "Type", "Stade", "Coupe", 
      "Abroutis", "Repere", "Rejet", 
      "Coeff", "DiamLim", "Date", 
      
      "X", "Y", "SystGPS", "CoordGPS1", "CoordGPS2"
    )
    # niveau 3
    style_level_3 <- c("Observation", "Observations", "Cheminement")
    # # niveau 4
    # style_level_4 <- c("Commentaires")
    
    style_levels <- list(
      style_level_1,
      style_level_2,
      style_level_3#,
      # style_level_4
    )
    names(style_levels) <- c(
      "style_level_1",
      "style_level_2",
      "style_level_3"#,
      # "style_level_4"
    )
    
    # --  création de la barre de progression
    disp_list <- paste0(
      disp_num, "-", Dispositifs$Nom[match(disp_num, Dispositifs$NumDisp)]
    )
    disp_num <- str_sub(disp_list[1], str_locate(disp_list[1], "-")[, 2] + 1, -1)
    pb_title <- "Progression"
    pb_label <- paste0(
      "Edition des classeurs d'inventaire mis à jour : 0\u0025 done - dispositif ", 
      disp_num, " en cours."
    )
    pb <- tkProgressBar(pb_title, pb_label, 0, 100, width = 800)
    
    # for (disp in disp_list) {
    disp <- disp_list[1]
    # -- gestion des noms et num du dispositif
    disp_num <- as.numeric(str_sub(disp, 1, str_locate(disp, "-")[, 1]-1)) #changement2
    disp_name <- 
      str_sub(disp, str_locate(disp, "-")[, 2] + 1, -1)
    
    # TODO : sécurité si le dispositif n'est pas en archive (cf PermAFI - à voir si utile  => dans ce cas adpater FichesRem et PlansArbres ----- #
    
    # -- préparation des tables d'inventaire en tour (disp)
    # arbres
    arbres <- 
      arbres_table %>% 
      filter(NumDisp == disp_num)
    
    # bois mort au sol > 30
    bm_sup30  <- 
      bm_sup30_table %>% 
      filter(NumDisp == disp_num)
    
    # coords
    coords <- 
      coords_table %>% 
      filter(NumDisp == disp_num)
    
    # bois mort au sol < 30
    bm_lineaire <- 
      bm_lineaire_table %>% 
      filter(NumDisp == disp_num)
    
    # régénération
    reges <- 
      reges_table %>% 
      filter(NumDisp == disp_num)
    
    # taillis
    taillis <- 
      taillis_table %>% 
      filter(NumDisp == disp_num)
    
    
    ##### 5/ Edition des classeurs de remesure #####
    # -- distribution des styles entre les intitulés de colonne
    share_styles_by_col <- 
      get_share_styles_by_col(style_levels)
    
    # -- liste des tables à éditer
    table_list <- list( # TODO : nécessaire de garder arbres ? ou ne garder que Arbres ?
      arbres, taillis, reges, bm_lineaire, 
      bm_sup30, coords
    )
    sheet_list <- c(
      "Arbres", "Taillis", "Rege", "BMortLineaire", 
      "BMortSup30", "Coord"
    )
    wb <- createWorkbook()
    
    for (i in 1:length(table_list)) { # loop 'i in 1:length(table_list)'
      # print(i) # debug
      # -- données et nom de la table
      tmp <- table_list[[i]]
      sheet <- sheet_list[i]
      print(sheet)
      
      # -- distribution des styles sur l'ensemble de la feuille à éditer
      # TODO ? : rajouter sécurité si on a une end_col > à une start_row (ex: colonne NumDisp positionnée au milieu)
      share_styles_by_sheet <- 
        get_share_styles_by_sheet(share_styles_by_col, tmp) %>% 
        mutate(sheets = sheet)
      
      # -- écriture des données dans le classeur
      addWorksheet(wb, sheet, gridLines = T)
      writeData(wb, sheet, tmp)
      
      
      # -- mise en forme
      layout_wb(
        wb, styles = styles_list, share_styles_by_sheet,
        style_separate_medium_row, separate_all = T
      )
      
      # -- set cell widths and cell heights
      # remove widths and heights
      removeColWidths(wb, sheet = sheet, cols = 1:dim(tmp)[2])
      removeRowHeights(wb, sheet, rows = 1)
      # new cell widths
      ## title
      setColWidths(wb, sheet = sheet, cols = 1:dim(tmp)[2], widths = "auto")
      # new cell widths
      ## title
      setRowHeights(wb, sheet, rows = 1, heights = 75)
      ## general
      # print(dim(tmp)) # debug
      row_end <- if (dim(tmp)[1] > 0) dim(tmp)[1] + 1 else 2
      setRowHeights(wb, sheet, rows = 2:row_end, heights = 15)
      
      # -- commande pour figer les volets
      freezePane(wb, sheet, firstActiveRow = 2)
    } # end of loop 'i in 1:length(table_list)'
    
    
    ##### 6/ Sauvegarde et écriture du classeur #####
    # -- répertoire de sauvegarde
    output_dir <- 
      paste0(
        "out/stacking_data/", disp_num
      )
    dir.create(output_dir, showWarnings = F, recursive = T)
    
    # -- chemin du fichier de sortie
    file_path <- file.path(output_dir, paste0(disp_num, "_stacked_data.xlsx")) 
    saveWorkbook(wb, file_path, overwrite = TRUE) # TODO : à libérer pour édition
    
    # -- MAJ de la barre de progression
    info <- round(match(disp, disp_list) / length(disp_list) * 100)
    pb_label <- 
      # if (lang == "FRA") {
      paste0(
        "Edition des classeurs de remesures : ", 
        info, "\u0025 done - dispositif ", disp_name, " \u00E9dit\u00E9."
      )
    pb_title <- 
      # if (lang == "FRA") {
      paste0("Edition (",info," \u0025)")
    info <- round(match(disp, disp_list) / length(disp_list) * 100)
    setTkProgressBar(pb, info, pb_title, pb_label)
    # } # end of loop disp_list # TODO : à mettre en place ?
    
    # -- close barre de progression
    close(pb)
  } # end of cond 'edit'
  
  # -- message de fin
  msg <- if (edit == T) {
    tk_messageBox(
      type = "ok", 
      message = paste0(
        "Mise à jour du classeur d'inventaire terminée.\n\nVoir le classeur '", 
        file_path, 
        "'"
      ), 
      icon = "info"
    )
    
    # -- vérification des essences/azimut/distance après compilation
    check_stacked_table <- arbres_table %>% 
      # keep geolocated trees only
      filter(!is.na(NumArbre)) %>% 
      
      # detect differences
      distinct(
        NumDisp, NumPlac, NumArbre, 
        Essence, Azimut, Distance, 
        .keep_all = T
      ) %>% 
      add_count(NumDisp, NumPlac, NumArbre) %>% 
      # select(-Cycle) %>% 
      filter(n > 1) %>% 
      arrange(NumDisp, NumPlac, NumArbre, Cycle)
    
    # -- return of check_tree_id function
    if (nrow(check_stacked_table) > 0) {
      # output directory
      output_dir <- 
        paste0(
          "out/stacking_data/", disp_num
        )
      dir.create(output_dir, showWarnings = F, recursive = T)
      
      # writing
      write.xlsx(
        check_stacked_table, 
        file = file.path(
          output_dir, 
          paste0(
            disp_num, "-moving_trees_after_stack.xlsx", collapse = "-"
          )
        )
      )
      
      # msg
      tk_messageBox(
        type = "ok",
        message = paste0(
          "Après avoir compilé le fichier, il apparaît qu'il y a des notations 'Essence', 'Azimut' et/ou 'Distance' incohérents entre les différents inventaires.\nVoir le fichier ",
          disp_num, "-moving_trees_after_stack.xlsx"
        )
      )
      warning(
        "Il y a des notations 'Essence', 'Azimut' et/ou 'Distance' incohérents entre les différents inventaires.\nVoir le fichier ",
        disp_num, "-moving_trees.xlsx"
      )
      edit <<- FALSE
    }
    
    
    # check cut_trees (perches comprises)
    check_cut_trees(arbres_table, check_stems = T)
  } else {
    tk_messageBox(
      type = "ok", 
      message = "Echec de la mise à jour du classeur d'inventaire -> corrections nécessaires", 
      icon = "info"
    )
  }
}

# # call
# stack_inventory(repGF)
