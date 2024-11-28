#### fonction de traduction des intitulés de colonne et des valeurs #####
#' Fonction de traduction des intitulés de colonne et des valeurs
#' @description La fonction permet de traduire les données d'une langue à une autre
#'
#' @import tcltk
#' @import openxlsx
#' @import tools
#' @export
#'
# TODO : mettre une sécurité pour les essences non reconnues (si espace laissé derrière par exemple)
# TODO : changer le nom de la fonction : translate_table ?
translate_xlsx <- function(
  table = NULL, var = NULL, 
  input_lang = NULL, output_lang = NULL, 
  dictionary = NULL, 
  translate_names = T
) {
  # table <- tmp # debug
  # var <- "Arbres" # debug
  # input_lang = "FRA" # debug
  # output_lang = "ENG" # debug
  
  # Vecteurs
  lang1_VECT <- paste0("Attribut_", input_lang)
  lang2_VECT <- paste0("Attribut_", output_lang)
  # Quotations
  lang1_QUOT <- quo(!!parse_expr(lang1_VECT))
  lang2_QUOT <- quo(!!parse_expr(lang2_VECT))
  
  if (input_lang != "FRA" | output_lang != "FRA") {
    # Dictionnaire secondaire 1
    subdictionary1_DF <-
      dictionary %>%
      filter(Feuille == var)
    
    if (input_lang != "FRA") {
      # -- traduction des intitulés de colonne
      names(table) <- subdictionary1_DF[, lang2_VECT][
        match(names(table), subdictionary1_DF[, lang1_VECT])
        ]
    }
    
    if (dim(table)[1] > 0) {
      # -- traduction du contenu de certaines colonnes
      # On détecte s'il y a ce type de colonnes dans la table "table"
      table2 <-
        subdictionary1_DF %>%
        filter(
          Emplacement != "ColName" |
            (Emplacement == "ColName" & Attribut_FRA == "Essence")
        ) %>%
        mutate(
          Emplacement = if_else(
            Emplacement != "ColName",
            str_sub(Emplacement, 5, -1),
            Emplacement
          )
        )
      
      if (dim(table2)[1] > 0) {
        # Liste des colonnes à traduire
        col2trad_VECT <- unique(table2$Emplacement)
        col2trad_VECT <- str_replace(col2trad_VECT, "ColName", "Essence")
        col2trad_VECT <- unique(col2trad_VECT)
        col2trad_VECT <- intersect(col2trad_VECT, names(table))
        
        for (col in col2trad_VECT) {
          # col = col2trad_VECT # debug
          # Sous-dictionnaire 2
          subdictionary2_DF <-
            dictionary %>%
            filter(Emplacement == paste0("Col_", col)) %>%
            select(!!lang1_QUOT, !!lang2_QUOT) %>%
            rename(!!col := !!lang1_QUOT)
          
          # Jonction de la table à importer et du sous-dictionnaire
          table <-
            table %>%
            left_join(subdictionary2_DF, by = col) %>%
            mutate(
              !!col := !!lang2_QUOT,
              !!lang2_QUOT := NULL
            )
        }
      }
    }
    
    if (output_lang != "FRA" && translate_names == T) {
      # -- traduction des intitulés de colonne
      names(table) <- subdictionary1_DF[, lang2_VECT][
        match(names(table), subdictionary1_DF[, lang1_VECT])
        ]
    }
  }
  
  # Retour fonction translate_xlsx
  return(table)
}


##### fonction pour récupérer la langue d'un classeur excel (attention : définie par les noms d'onglet !!)
# TODO : use 'get_wb_settings' function
get_language_from_wb <- function(
  file = NULL,
  sheet_names_table = NULL#,
  # i18n = NULL
) {
  # file <- "/Users/Valentin/Travail/Outils/GitHub/PermAFI2/data/excel/inventaires/2-Bois du Chanois.xlsx" # debug
  
  # -- get sheet names
  if (file_ext(file) == "xls") {
    sheet_names <- excel_sheets(file)
  }
  if (file_ext(file) == "xlsx") {
    sheet_names <- getSheetNames(file)
  }
  
  # -- recherche de la langue
  look_up_lang <- 
    sheet_names_table %>% 
    select(-Feuille, -Emplacement) %>%
    
    # noms en colonnes
    pivot_longer(
      cols = everything(),
      names_to = "language",
      values_to = "sheet_name"
    ) %>% 
    
    # on filtre les feuilles reconnues dans le classeur importé et on en retire la langue
    filter(sheet_name %in% sheet_names) %>% 
    
    # get the most represented language
    group_by(language) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    arrange(desc(count)) %>% 
    head(1) %>% # 1st row = most represented language
    select(language) %>% 
    unlist() %>% 
    unname()
  
  # -- get input language
  input_lang <- gsub("Attribut_", "", look_up_lang)
  # format
  # input_lang <- i18n()$get_languages()[ match(
  #   input_lang, str_sub( toupper( i18n()$get_languages() ), 1, 3)
  # )]
  
  # -- return from 'get_language_from_wb' function
  return(input_lang)
}


##### fonction pour lire (+ traduire si nécessaire) les fichiers .xlsx #####
read_xlsx <- function(
  file = NULL, 
  sheet_name = NULL, 
  input_lang = NULL,
  file_dictionary = NULL,
  dictionary = dictionary,
  i18n = NULL #, ...
) {
  
  # -- liste des intitulés de colonne
  sheet_dictionary <- file_dictionary %>% filter(Feuille == sheet_name)
  column_names <- 
    with(sheet_dictionary, original[Emplacement == "ColName"])
  
  # -- lecture du classeur d'inventaire
  df <- read.xlsx(file, sheet_name)
  
  # reconvertit les espaces, convertis en point lors de la lecture du file, en underscore
  names(df) <- str_replace_all(names(df), pattern = "\\.", replacement = "_")
  # reconvertit les espaces en underscore # faire disparaître à terme ?
  column_names <- str_replace_all(column_names, pattern = " ", replacement = "_")
  
  # -- traduction des intitulés de colonnes
  if (input_lang != "FRA") { # cond 'input_lang != "FRA"'
    # - translate column_names
    column_names <- with(sheet_dictionary, Attribut_FRA[ match(column_names, original) ])
    # - rename column names in french :
    names(df) <- with(sheet_dictionary, Attribut_FRA[ match(names(df), original) ])
  }
  
  # -- sécurité sur le nombre de colonnes
  
  # --- préalable : gestion colonne Observation(s)
  if ("Observation" %in% names(df)) {
    df <- df %>% rename(Observations = Observation)
  }
  if ("Observation" %in% column_names) {
    column_names <- column_names[column_names != "Observation"]
  }
  
  # -- colonnes manquantes
  missing_columns <- setdiff(column_names, names(df))
  
  # cas particulier feuille Coord (temporaire)
  if ("Coefft" %in% missing_columns) { # A renforcer -> problèmes avec la feuille Cycles des données administrateurs
    df <- df %>% mutate(Coefft = NA)
    missing_columns <- setdiff(missing_columns, "Coefft")
  }
  if ("DiamLim" %in% missing_columns) {
    df <- df %>% mutate(DiamLim = NA)
    missing_columns <- setdiff(missing_columns, "DiamLim")
  }

  if (length(missing_columns) > 0) {
    stop("Sheet '", sheet_name, "' - columns missing in workbook : ", paste0(missing_columns, collapse = ", "))
  }
  
  

  # -- corrections diverses
  # gestion de la colonne Nbre dans la feuille Taillis
  if ("Nbre" %in% names(df)) {
    df <- df %>% mutate(Nbre = ifelse(
      # rajouter 1 comme valeur par défaut pour les valeurs de "Nbre" 
      # non renseignées -> attention uniquement si Essence et Diam bien renseignés
      is.na(Nbre) & !is.na(Essence) & !is.na(Diam), 1, Nbre
      ))
  }
  
  # sélection des bons intitulés de colonne
  df <- df %>% select(all_of(column_names))
  

  if (nrow(df) > 0 && input_lang != "FRA") { # cond 'nrow(df) > 0 && input_lang != "FRA"'
    # -- traduction des valeurs des colonnes
  # 2 possibilités : 
  # - traduction disponible dans sheet_dictionary 
  # (cas de certaines colonnes des tables administrateurs 
  # comme 'Type' dans la table 'Dispositifs')
  
  # - traduction doit être récupérée dans une autre table 
  # (cas des colonnes 'Essence' et 'Type')
    
    # - 1/colonnes repérées dans sheet_dictionary
    column_to_translate <- 
      with(sheet_dictionary, Attribut_FRA[which(Emplacement != "ColName")])
    column_dictionary <- sheet_dictionary %>% filter(Emplacement != "ColName")
    
    # traduction
    if (nrow(column_dictionary) > 0) { # cond 'nrow(column_dictionary) > 0'
      column_to_translate <- unique( gsub("Col_", "", column_dictionary$Emplacement) )
      df[column_to_translate] <- lapply(
        df[column_to_translate], 
        function(x) {
          match_pos <- match(x, column_dictionary$original)
          # test si toutes les traductions ont bien été retrouvées
          translation_issues <- x[which(!is.na(x) & is.na(match_pos))]
          if (length(translation_issues) > 0) {
            stop("Il y a des éléments non traduites dans la feuille ", sheet_name, " du classeur importé  : '", paste0(translation_issues, collapse = "', '"), "'")
          } else {
            column_dictionary$Attribut_FRA[match(x, column_dictionary$original)]
          }
        }
      )
    } # end of cond 'nrow(column_dictionary) > 0'
    
    
    # - 2/ traduction enregistrée dans une autre table
    if ("Essence" %in% names(df)) {
      dictionary_tmp <- dictionary %>% filter(Feuille == "Essences", Emplacement == "Col_Essence")
      df$translation <- dictionary_tmp$Attribut_FRA[
        match( df$Essence, dictionary_tmp[, paste0("Attribut_", input_lang)] )
        ]
      # test si toutes les traductions ont bien été retrouvées
      translation_issues <- 
        with(df, unique(Essence[ which(is.na(translation) & !is.na(Essence)) ]))
      if (length(translation_issues) > 0) {
        stop("Il y a des essences non traduites dans la feuille ", sheet_name, " du classeur importé  : '", paste0(translation_issues, collapse = "', '"), "'")
      } else {df <- df %>% mutate(Essence = translation, translation = NULL)}
    }
    if ("Type" %in% names(df)) {
      dictionary_tmp <- dictionary %>% filter(Feuille == "CodeTypoArbres", Emplacement == "Col_Code")
      df$translation <- dictionary_tmp$Attribut_FRA[
        match( df$Type, dictionary_tmp[, paste0("Attribut_", input_lang)] )
        ]
      # test si toutes les traductions ont bien été retrouvées
      translation_issues <- 
        with(df, unique(Type[ which(is.na(translation) & !is.na(Type)) ]))
      if (length(translation_issues) > 0) {
        stop("Il y a des types non traduits dans la feuille ", sheet_name, " du classeur importé  : '", paste0(translation_issues, collapse = "', '"), "'")
      } else {df <- df %>% mutate(Type = translation, translation = NULL)}
    }
  } # end of cond 'nrow(df) > 0 && input_lang != "FRA"'
  
  # -- retour de la fonction read_xlsx
  return(df)
}


##### fonction pour contrôler les feuilles présentes dans le wb #####
# TODO : voir si utile avec shiny ?
check_needed_sheets <- function(
  file = NULL,
  sheets = NULL, 
  sheet_names_table = NULL, 
  input_lang = NULL
) {
  # -- needed sheets in the wb
  needed_sheets <- 
    sheet_names_table[
      # filtre
      sheet_names_table$Attribut_FRA %in% sheets,
      # sélection de l'attribut
      paste0("Attribut_", input_lang)
      ]
  
  # -- noms présents dans le classeur
  sheet_names <- getSheetNames(file)
  
  # -- test
  missing_names <- setdiff(needed_sheets, sheet_names)
  if (length(missing_names) > 0) {
    stop("Missing sheets in the Workbook : ", paste0(missing_names, collapse = ", "))
  }
}


##### function to tidy afi tables #####
# PARAM
# define column types ATTENTION : MODIFIER l'objet column_types dans la fonction afi_XlsTranslation
column_types <- list(
  numeric = c(
    "NumDisp", "NumPlac", "NumArbre", "IdArbre", "Azimut", "Distance",
    "Diam1", "Diam2", "Diam",
    "HautT", "HautL", "Haut",
    "Ray1", "Dh1", "Ray2", "Dh2",
    "Nbre", "Stade", "Transect", "Angle",
    "DiamIni", "DiamMed", "DiamFin", "Longueur",
    "Recouv", "Class1", "Class2", "Class3", "Abroutis", "Rejet",
    "Contact", "Chablis",
    "Coeff", "DiamLim"
  ),
  character = c("Essence", "Type"),
  rounded_values = c("HautT", "HautL", "Haut", "Ray1", "Dh1", "Ray2", "Dh2"),
  no_empty_values = c("Recouv", "Class1", "Class2", "Class3", "Rejet", "Abroutis", "Angle")
)

tidy_afi_table <- function(table = NULL, table_name = NULL, column_types = NULL) {
  # table = tmp # debug
  table_name <- ifelse(is.null(table_name), "", paste0("table ",table_name, "\n"))
  
  # -- apply column types
  # TODO : compter les NA introduits dans le df ?
  # - numeric
  # detect numeric type columns
  numeric_columns <- intersect(names(table), column_types$numeric)
  
  # apply as.numeric
  table[numeric_columns] <- lapply(
    numeric_columns,  FUN = function(x, t) {
      # x = numeric_columns[7] # debug
      non_digit_values <- t[[x]][grep("[^[:digit:]\\,\\.\\-]", t[[x]])] %>% unique
      # non_digit_values <- which(!is.na( t[[x]][non_digit_values] )) # old
      
      if (length(non_digit_values) > 0) {
        stop(table_name, "Valeur(s) non numérique détectée dans la colonne ", x, " ('", paste0(non_digit_values, collapse = "', '"), "')") # 
      }
      as.numeric(unlist(t[, x]))
    },
    t = table
  )
  
  # - character
  # detect character type columns
  character_columns <- intersect(names(table), column_types$character)
  
  # apply as.character
  table[character_columns] <- lapply(
    table[character_columns],  FUN = function(x) as.character(x)
  )
  
  # - round
  # columns to round
  columns_to_round <- 
    intersect(names(table), column_types$rounded_values)
  
  # apply round(., 2)
  table[columns_to_round] <- lapply(
    table[columns_to_round],  FUN = function(x) round(x, 2)
  )
  
  # - empty
  # columns where to fill NA values with 0
  no_empty_values_columns <- 
    intersect(names(table), column_types$no_empty_values)
  
  # apply replace NA
  table[no_empty_values_columns] <- lapply(
    table[no_empty_values_columns],  FUN = function(x) replace(x, is.na(x), 0)
  )
  
  # -- return from 'tidy_afi_table' function
  return(table)
}


##### function to build an id in table (prepare db) #####
set_db_id <- function(
  table = NULL, 
  id_columns = NULL, 
  id_var = NULL
) {
  # build id table
  id_table <- 
    table %>% 
    select(all_of(id_columns)) %>% 
    distinct() %>% 
    arrange(!!!syms(id_columns)) %>% 
    mutate(!!id_var := row_number()) %>% 
    select(all_of(c(id_var, id_columns)))
  
  # -- security
  if (id_var %in% names(table)) table <- table %>% select(-id_var)
  
  # -- rejoin table
  value_table <- left_join(
    table, id_table, 
    by = c(id_columns)
  ) %>% 
    select(all_of(c(id_var, setdiff(names(table), id_columns))))
  
  # -- return from 'set_id' function
  return(list(id_table = id_table, value_table = value_table))
}


##### fonction pour importer les données d'inventaire AFI #####
afi_XlsTranslation <- function(
  wd = NULL, 
  files_list = NULL, 
  output_dir = file.path(wd, "tables"),
  # lang = "FRA", 
  trad = F,
  i18n = NULL
) {
  
  ##### 1/ Initialisation #####
  # -- chargement du dictionnaire de traduction
  # TODO : créer une fonction pour s'assurer que la table afiDictionary existe bien
  load(file.path(output_dir, "afiDictionary.Rdata"))
  
  # TODO : à sauvegarder dans une archive de paramètres ?
  # define column types
  column_types <- list(
    numeric = c(
      "NumDisp", "NumPlac", "NumArbre", "IdArbre", "Azimut", "Distance", 
      "Diam1", "Diam2", "Diam", 
      "HautT", "HautL", "Haut", 
      "Ray1", "Dh1", "Ray2", "Dh2",
      "Nbre", "Stade", "Transect", "Angle",
      "DiamIni", "DiamMed", "DiamFin", "Longueur",
      "Recouv", "Class1", "Class2", "Class3", "Abroutis", "Rejet",
      "Contact", "Chablis"
    ),
    character = c("Essence", "Type"),
    rounded_values = c("HautT", "HautL", "Haut", "Ray1", "Dh1", "Ray2", "Dh2"),
    no_empty_values = c("Recouv", "Class1", "Class2", "Class3", "Rejet", "Abroutis", "Angle")
  )
  
  # filtre des classeurs ouverts
  pos_Open <- grep("~$", files_list, fixed = T)
  if (length(pos_Open) > 0) files_list <- files_list[-pos_Open]
  
  # message
  print(i18n()$t("Traduction des classeurs d'inventaire"))
  
  # -- création de la barre de progression
  # # -- switch shiny ***
  # withProgress(
  #   message = i18n()$t("Import des données en cours"),
  #   detail = i18n()$t("Progression..."), value = 0, {
  #     # *** --
      
      # -- table de référence contenant les noms des feuilles (permettra de trouver la langue)
  # TODO : remplacer par une archive avec les noms de feuilles / tables ?
      sheet_names_table <-
        dictionary %>% filter(Emplacement == "Inventaire") # TODO : mettre une sécurité ici aussi ?
      
      # calcul du nombre total de tables à importer
      complete_progress <- length(files_list) * dim(sheet_names_table)[1]
      
      # -- error list
      error <- vector(mode = "list", length = nrow(sheet_names_table))
      names(error) <- sheet_names_table$Attribut_FRA # default column
      
        
      # --
      # # -- tests si archives déjà en mémoire # TODO : à remplacer par une fonction
      # if ("afiDonneesBrutes.Rdata" %in% list.files(path = "tables")) {
      #   showModal(modalDialog(
      #     span("Une archive contenant des donn\u00E9es d'inventaire existe d\u00E9j\u00E0. Si les dispositifs s\u00E9lectionn\u00E9s figurent d\u00E9j\u00E0 en archive, ces anciennes donn\u00E9es seront \u00E9cras\u00E9es"),
      #     footer = tagList(
      #       actionButton("yes", i18n$t("Ok")),
      #       actionButton("no", i18n$t("Annuler"))
      #     )
      #   ))
      # 
      #     # si non, alors on arrête le programme
      #     observeEvent(input$no, {
      #       stop("L'import des donn\u00E9es a \u00E9t\u00E9 interrompu")
      #     })
      # }
      # --
      
      
      
      # -- initialize list of all disp numbers
      all_disp_num <- c()
  ##### / \ #####
      
      
      ##### 2/ Import des classeurs d'inventaire #####
      # initialize db list (contain tmp tables)
      db <- error
      # db <- c(error, list(c()))
      # names(db) <- c("IdArbres", "ValArbres", setdiff(names(db), c("Arbres", "")))
      
      # -- boucle d'import des différents classeurs et des différentes feuilles
      for (i in 1:length(files_list)) {  # loop 'i in 1:length(files_list)'
        # i=1 # debug
        # print(file) # debug
        file <- files_list[[i]]
        
        # -- define file dictionary
        file_dictionary <- 
          dictionary %>% 
          filter(Feuille %in% sheet_names_table$Attribut_FRA) %>% 
          mutate(original = Attribut_FRA)
        
        # -- get input lang
        input_lang <- get_language_from_wb(file, sheet_names_table = sheet_names_table) # , i18n = i18n
        
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
        # match(tolower(input_lang), i18n()$get_languages())
        # i18n()$set_translation_language("FRA")
        
        # -- test des intitulés d'onglets à importer (on vérifie qu'il y a bien toutes les feuilles nécessaires)
        # ? check_needed_sheets(file, sheets, sheet_names_table, input_lang = input_lang)
        
        # needed sheets in the wb
        needed_sheets <- unique(file_dictionary$Feuille)
        # needed_sheets <- sheet_names_table[, paste0("Attribut_", input_lang) ]
        # needed_sheets <- sheet_names_table$Attribut_FRA # use i18n ?
        
        # noms présents dans le classeur
        sheet_names_wb <- getSheetNames(file)
        
        # -- detect missing sheet # TODO : passer par la fonction check_workbook_sheets ?
        # set up i18n()
        missing_names <- setdiff(needed_sheets, sheet_names_wb)
        if (length(missing_names) > 0) {
          stop("Missing sheets in the Workbook : ", paste0(missing_names, collapse = ", "))
        }
        print(paste0( 
          i18n()$t("Traduction des classeurs d'inventaire "), file, "..."
        ))
        
        # -- lecture des différentes tables d'inventaire
        for (sheet in needed_sheets) { # loop 'sheet in needed_sheets'
          # sheet <- needed_sheets[1] # debug
          # print(sheet) # debug
          tmp <- read_xlsx(
            file = file, 
            sheet_name = sheet, 
            input_lang = input_lang,
            file_dictionary = file_dictionary,
            dictionary = dictionary, # si besoin puor traduction des essences
            i18n = i18n
          )
          
          # # -- switch shiny ***
          # # incrémentation de la barre de progression
          # incProgress(1 / complete_progress)
          # # *** --
          
          # -- table name
          # TODO : à revoir
          table_name <- if (input_lang != "FRA") {
            sheet_names_table$Attribut_FRA[match(sheet, sheet_names_table[, paste0("Attribut_", input_lang)])]
          } else sheet
          
          # update error list
          if (is.element(TRUE, is.na(tmp$NumDisp))) {
            error[[ table_name ]] <- c(
              error[[ table_name ]],
              # add file name
              str_sub(file, 1, str_locate(file, "-") - 1)[1]
            )
          }
          
          # update NumDisp list
          if ("NumDisp" %in% names(tmp)) {
            all_disp_num <- union(all_disp_num, unique(tmp$NumDisp))
          }
          
          # tidy tmp
          tmp <- tidy_afi_table(
            table = tmp, table_name = table_name, column_types = column_types
            )
          
          
          # -- update table list
          # table_name <- dictionary %>% filter(Feuille == "Feuilles" & Attribut_DEU == sheet) %>% select(Attribut_FRA) %>% unlist() %>% unname()
            db[[ table_name ]] <- rbind(db[[ table_name ]], tmp)
            # assign( paste0(sheet, "_tmp"), tmp )
            
        } # end of loop 'sheet in needed_sheets'
      } # end of loop 'i in 1:length(files_list)'
      
      # -- rename db tables before stack
      
      # replacement string
      replacement <- c("Reges", "BMortLineaires", "Coords")
      
      # strings to replace
      names(replacement) <- c("Rege", "BMortLineaire", "Coord")
      
      # call str_replace_all
      names(db) <- str_replace_all(string = names(db), pattern = replacement)
      
      # -- superposition des tables (si archive déjà existante)
      if ( "afiDonneesBrutes.Rdata" %in% list.files(path = "tables") ) { # cond '"afiDonneesBrutes.Rdata" %in% list.files(path = "tables"'
        # - 1. On filtre les tables déjà présentes dans l'archive (num du disp importé est exclus)
        # -- Numéros des dispositifs importés
        imported_data_stand_num <- all_disp_num
        
        # -- noms des tables en archive
        table_names <-
          c("Arbres", "Taillis", "Reges", "BMortSup30", "BMortLineaires", "Coords")
        # table_names <- load(file.path(output_dir, "afiDonneesBrutes.Rdata"))
        load(file.path(output_dir, "afiDonneesBrutes.Rdata"))
        Arbres <- 
          left_join(IdArbres, ValArbres, by = "IdArbre") %>% 
          mutate(IdArbre = NULL)
        
        
        for (table in table_names) { # loop 'table in table_names'
          # print(table) # debug
          # table <- table_names[1] # debug
          # -- suppression des données d'inventaire correspondant à imported_data_stand_num
          # archived data
          arch <- get(table)
          if ("NumDisp" %in% names(arch)) {
            arch <- arch %>% filter(!NumDisp %in% imported_data_stand_num)
          }
          # security to suppress after re-import
          if ("add_deadwood_dmh" %in% names(arch)) {
            arch <- arch %>% select(-tmp, -add_deadwood_dmh)
          }
          # security - à éliminer après travail shiny ?
          arch <- tidy_afi_table(table = arch, column_types = column_types)
          
          # - 2. On ajoute les tables nouvellement importées
          # -- stack newly imported data
          # imported data
          tmp <- db[[table]]
          # all data stacked (except Coord) # TODO : à voir si nécessaire de faire Coords en fonction des cycles ?
          stack <- if (table != "Coords") rbind(arch, tmp) else tmp
          # reassign
          assign(table, stack)
        } # end of loop 'table in table_names'
      } else {
        for (table in names(db)) { # loop 'table in names(db)'
          assign(table, db[[table]])
        } # end of loop 'table in names(db)'
      } # end of cond '"afiDonneesBrutes.Rdata" %in% list.files(path = "tables"'
      
      # build id for tree tables
      # if (sheet == "Arbres") {
        # define id_columns
        id_columns <- 
          c("NumDisp", "NumPlac", "NumArbre", "Essence", "Azimut", "Distance")
        
        # call 'set_id' function
        db_tables <- 
          set_db_id(table = Arbres, id_columns = id_columns, id_var = "IdArbre")
        
        # assign tables (value_table = 'Arbres' and id_table = 'Id_Arbres')
        # db[["ValArbres"]] <- rbind(
        #   db[["ValArbres"]],
        #   db_tables$value_table %>% 
        #     # add codeecolo M1/M2 for standing deadwood
        #     check_dmh_for_standing_deadwood()
        # )
        ValArbres <- 
          db_tables$value_table #%>%
          # add codeecolo M1/M2 for standing deadwood -> décalé à l'étape de calcul
          # check_dmh_for_standing_deadwood()
        IdArbres <- db_tables$id_table
      # }} # end of cond 'sheet == "Arbres"'
      
      # -- détection des premiers oublis
      # attention : il est important qu'aucun vide dans les NumDisp ne passe car les Verif
      # se font par rapport à NumDisp. Impossible de localiser le vide (quel dispositif ?) ailleurs
      # qu'à cette étape
      warning_msg <- c() 
      troubling_tables <- which( lapply(error, is.null) == FALSE )
      
      for (i in troubling_tables) { # loop 'i in troubling_tables'
        # i <- troubling_tables[1] # debug
        
        # nom de la table
        table_name <- names(error)[i]
        # noms des fichiers
        file_names <- error[[table_name]]
        # message
        msg_tmp <- paste0(
          i18n()$t("Num\u00E9ro(s) de dispositif(s) vide(s) d\u00E9tect\u00E9(s) dans la feuille "),
          table_name,
          i18n()$t(" du/des dispositif(s)\n\n"),
          paste0(file_names, collapse = "\n"),
          i18n()$t("\n\nA compl\u00E9ter")
        )
        warning_msg <- paste(warning_msg, msg_tmp, sep = "\n\n")
      } # end of loop 'i in troubling_tables'
      
      # affichage du warning
      if (length(warning_msg) > 0) warning(warning_msg)
      ##### / \ #####
      
      
      
      ##### 4/ Sauvegarde #####
      dir.create("tables", showWarnings = F)
      save(
        IdArbres, ValArbres, Taillis, Reges, BMortSup30, BMortLineaires, Coords,  #Placettes,
        file = file.path(output_dir, "afiDonneesBrutes.Rdata")
      )
      
      print(
        i18n()$t("Importation termin\u00E9e. Fichiers d'inventaire archiv\u00E9s")
      )
  #     # -- switch shiny ***
  #   }) # end of withProgress
  # 
  # # ending alert
  # show_alert(
  #   title = i18n()$t("Import des données termin\u00E9e !!"),
  #   text = i18n()$t("Fichiers d'inventaire archiv\u00E9s"),
  #   type = "success"
  # )
  # # *** --
  ##### / \ #####
  
  
  ##### 5/ Traduction + édition des classeurs #####
  if (trad == T) {
    ##### TRAD-1/ Initialisation
    # -- sécurité : présence de la table afiCodes.Rdata indispensable
    if (!is.element("afiCodes.Rdata", list.files(path = "tables"))) {
      Ans1 <-
        tk_messageBox(
          type = "ok",
          message = "Aucune archive contenant les donnn\u00E9es administrateurs d\u00E9tect\u00E9e ('afiCodes.Rdata'). Traduction impossible",
          icon = "warning"
        )
      stop("Traduction impossible sans les donn\u00E9es administrateurs")
      
    } else load("tables/afiCodes.Rdata")
    
    
    ##### TRAD-2/ Choix de la langue dans laquelle traduire les classeurs
    # Construction du df contenant les arguments
    TabLang <-
      tibble(
        items = c(
          "Fran\u00E7ais / French",
          "Anglais / English",
          "Fran\u00E7ais / French",
          "Anglais / English"
        ),
        lang = c("FRA", "FRA", "ENG", "ENG"),
        langEdit = c("Anglais", "Fran\u00E7ais", "English", "French"),
        langMsg = c("anglaise", "fran\u00E7ais", "english", "french")
      )
    
    # Définition des choix de langage possibles (d'après 'lang')
    choix <- TabLang$langEdit[which(TabLang$lang == lang)]
    
    # Fenêtre de dialogue
    langEdit <-
      tk_select.list(
        choix,
        title =
          ifelse(
            lang == "FRA",
            "Choix de la langue dans laquelle traduire les classeurs",
            "Please select the language for translation"
          )
      )
    
    langDir <- langEdit
    langEdit <-
      ifelse(
        langEdit == "French" | langEdit == "Fran\u00E7ais",
        "FRA", "ENG"
      )
    
    # Message
    msg_langEdit <- TabLang$langMsg[TabLang$lang == lang & TabLang$langEdit == langDir]
    # load("tables/Translate.Rdata")
    print(
      ifelse(
        lang == "FRA",
        paste0("Traduction des classeurs d'inventaire en version ", msg_langEdit),
        paste0("Translation of inventory workbooks in ", msg_langEdit, " version")
      )
    )
    
    
    ##### TRAD-3/ Choix des dispositifs
    num_LIST <- unique(
      c(
        Arbres$NumDisp,
        Taillis$NumDisp,
        Reges$NumDisp,
        BMortSup30$NumDisp,
        BMortLineaires$NumDisp,
        Coord$NumDisp
      )
    )
    num_LIST <- sort(num_LIST)
    if (is.element(NA, num_LIST)) warning("NumDisp vide d\u00E9tect\u00E9")
    disp_LIST0 <-
      paste0(num_LIST, "-", Dispositifs$Nom[match(num_LIST, Dispositifs$NumDisp)])
    Msg_AllCheck <- ifelse(
      lang == "FRA",
      "Traduire les fichiers d'inventaire de tous les dispositifs",
      "Translate inventory workbook for all stands"
    )
    disp_LIST <- c(Msg_AllCheck, disp_LIST0)
    disp_LIST <-
      tk_select.list(
        as.character(disp_LIST),
        multiple = T,
        title =
          ifelse(
            lang == "FRA",
            "Choisir un ou plusieurs dispositifs",
            "Choose one or several stands"
          )
      )
    if (is.element(Msg_AllCheck, disp_LIST)) {disp_LIST <- disp_LIST0}
    disp_LIST <-
      as.numeric(str_sub(disp_LIST, 1, str_locate(disp_LIST, "-")[,1] - 1))
    
    
    ##### TRAD-4/ Edition des classeurs Excel
    afi_rewrite_disp(wd, disp_LIST, langEdit, langDir)
  }
  ##### / \ #####
}

##### fonction d'édition des classeurs Excel d'inventaire #####
#' Fonction d'édition des classeurs Excel d'inventaire
#' @description La fonction permet d'écrire les classeurs d'inventaire de la base AFI, dans la langue choisie
#'
#' @import tcltk
#' @import openxlsx
#' @import tools
#' @export
#'








# styles -----
# Construction des styles des différentes feuilles du classeur d'inventaire
# TODO : faire 1 seul style global sur lequel viennent se stacker d'autres styles
style_title1 <- createStyle(
  # police texte
  fontName = "Arial", fontSize = 12, 
  fontColour = "dodgerblue4", textDecoration = "bold", 
  # fond cellule
  fgFill = "tan1", 
  # bordure
  border =  "LeftBottomRight",
  # alignement
  valign = "top", halign = "center",
  # rotation et wrap
  textRotation = 90, wrapText = F
)
style_title2 <- createStyle(
  # police texte
  fontName = "Arial", fontSize = 12, 
  textDecoration = "bold", 
  # fond cellule
  fgFill = "mediumaquamarine", 
  # bordure
  border =  "LeftBottomRight",
  # alignement
  valign = "top", halign = "center",
  # rotation et wrap
  textRotation = 90, wrapText = F
)
style_title3 <- createStyle(
  # police texte
  fontName = "Arial", fontSize = 12, 
  textDecoration = "bold", 
  # fond cellule
  fgFill = "mediumaquamarine", 
  # bordure
  border =  "LeftBottomRight",
  # alignement
  valign = "center", halign = "center",
  # rotation et wrap
  textRotation = 0, wrapText = F
)
style_title4 <- createStyle(
  # police texte
  fontName = "Arial", fontSize = 12, 
  textDecoration = "bold", 
  # fond cellule
  fgFill = "lightgoldenrod", 
  # bordure
  border =  "LeftBottomRight",
  # alignement
  valign = "center", halign = "center",
  # rotation et wrap
  textRotation = 0, wrapText = F
)
style_general <- createStyle(
  # police texte
  fontName = "Arial", fontSize = 10, 
  # bordure
  border =  "LeftRight",
  # alignement
  valign = "center", halign = "center"
)
# style_general2 <- createStyle(
#   # police texte
#   fontName = "Arial", fontSize = 10, 
#   # bordure
#   border =  "LeftBottomRight",
#   # alignement
#   valign = "center", halign = "center"
# )
# separate_row avec stack ?
style_separate_medium_row <- createStyle(
  # bordure
  border = "Bottom", borderStyle = "medium"
)

styles_list <- c(
  style_title1, style_title2, style_title3, style_title4,
  style_general
)
names(styles_list) <- c(
  "style_title1", "style_title2", "style_title3", "style_title4",
  "style_general"
)
# load("wb_styles.Rdata") TODO
# ----





##### fonction pour construire la table share_styles_by_col #####
get_share_styles_by_col <- function(style_levels) {
  df <- tibble()
  
  for (i in 1:length(style_levels)) {
    # i=1 # debug
    style_level <- style_levels[i]
    style_level_num <- as.numeric( gsub("style_level_", "", names(style_level)) )
    
    tmp <-
      tibble(
        column_level = style_level_num,
        column_name = unlist(style_level),
        column_style = paste0("style_title", i)
      )
    
    df <- df %>% rbind(tmp)
  }
  
  # retour de la fonction get_share_styles_by_col
  return(df)
}

##### fonction pour obtenir styles share à partir de styles_share # TODO : à tester avec plusieurs feuilles #####
get_share_styles_by_sheet <- function(
  share_table, table_to_edit, var = NA
) {
  # share_table <- share_styles_by_col # debug
  # table_to_edit <- tmp # debug
  # var <- NA # debug
  
  # share_table3 <-  # debug : test si plusieurs styles enchevêtrés
  #   share_table %>% 
  #   mutate(
  #     column_position = match(column_name, names(table_to_edit)),
  #     start_row = 1,
  #     end_row = 1,
  #     styles = column_style
  #   ) %>% 
  #   filter(!is.na(column_position)) %>% 
  #   arrange(column_position)
  # share_table3 <- share_table3[10:20,] %>% 
  #   mutate(column_position = column_position + 13)
  
  # styles des titres
  share_table <- 
    share_table %>% 
    mutate(
      column_position = match(column_name, names(table_to_edit)),
      start_row = 1,
      end_row = 1,
      styles = column_style
    ) %>% 
    filter(!is.na(column_position)) %>% 
    arrange(column_position) %>% 
    # rbind(share_table3) %>%  # debug : test si plusieurs styles enchevêtrés
    mutate(
      level_update = ifelse(lag(column_level) > column_level, 1, 0),
      level_update = ifelse(is.na(level_update), 0, level_update),
      level_update = ifelse(level_update > 0, level_update + 1, level_update),
      level_update = cumsum(level_update),
      
      column_level = column_level + level_update,
      level_update = NULL
    ) %>% 
    group_by(column_level, styles, start_row, end_row) %>%
    # count(name = "column_level")
    #   summarise(column_level = cumsum(column_level))
    summarise(
      start_col = min(column_position, na.rm = T),
      end_col = max(column_position, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      gridExpand = TRUE,
      separate_var = NA
    )
  
  # rajout du style général
  if (dim(table_to_edit)[1] > 0) {
  share_style_general <- 
    share_table %>% 
    mutate(
      column_level = max(share_table$column_level) + 1,
      styles = "style_general",
      start_row = 2,
      end_row = dim(table_to_edit)[1] + 1,
      start_col = 1,
      end_col = dim(table_to_edit)[2],
      separate_var = var
    ) %>% 
    distinct()
  
  share_table <- share_table %>% rbind(share_style_general)
  }
  
  # retour de la fonction get_share_styles_by_sheet
  return(share_table)
}

##### fonction de mise en forme (du classeur "cadastre.xlsx" - meilleure version) #####
layout_wb <- function(
  wb, styles, styles_share, 
  style_separate_row, separate_all = F
) {
  # styles_share <- share_styles_by_sheet # debug
  
  ##### 1/ Tests de sécurité #####
  # -- sécurité : présence dans le wb des feuilles mentionnées dans styles_share
  # test
  all_wb_sheets <- sheets(wb)
  test_sheet <- 
    with(styles_share, which(!sheets %in% all_wb_sheets))
  
  # message
  if (length(test_sheet) > 0) {
    missing_sheets <- paste0(
      unique( styles_share$sheets[test_sheet] ), 
      collapse = ", "
    )
    stop(
      "Certaines feuilles de la table '", substitute(styles_share),
      "' n'existent pas dans le classeur excel \u00E0 mettre en forme : ",
      missing_sheets
    )
  }
  # -- sécurité : présence en arguments des styles mentionnés dans styles_share
  # test
  all_arg_styles <- names(styles_list)
  test_style <- 
    with(styles_share, which(!styles %in% all_arg_styles))
  
  # message
  if (length(test_style) > 0) {
    missing_styles <- paste0(
      unique( styles_share$styles[test_style] ), 
      collapse = ", "
    )
    stop(
      "Certains styles de la table '", substitute(styles_share),
      "' sont introuvables dans les arguments : ",
      missing_styles
    )
  }
  ##### /\ #####
  
  
  ##### 2/ Mise en forme du wb #####
  # -- liste des feuilles du wb
  sheet_name_list <- unique(styles_share$sheets)
  
  for (sheet_name in sheet_name_list) {
    # sheet_name <- sheet_name_list[1] # debug
    
    tmp_sheet <- styles_share %>% filter(sheets == sheet_name)
    # -- liste des styles
    style_name_list <- unique(tmp_sheet$styles)
    
    for (style_name in style_name_list) {
      # style_name <- style_name_list[2] # debug
      tmp_style <- tmp_sheet %>% filter(styles == style_name)
      
      # -- récupération du style
      style <- styles_list[[which(names(styles_list) == style_name)]]
      
      # -- ajout des styles aux différentes plages de cellules
      for (l in 1:dim(tmp_style)[1]) {
        # paramètres
        rows <- with(tmp_style[l, ], start_row:end_row)
        cols <- with(tmp_style[l, ], start_col:end_col)
        gridExpand <- tmp_style[l, ]$gridExpand
        sep_var <- tmp_style[l, ]$separate_var
        
        # ajout du style
        addStyle(wb, sheet_name, style, rows, cols, gridExpand)
      }
      
      # -- séparation des lignes à certains changements de valeur (possible uniquement si style == "style_general")
      if (!is.na(sep_var) && str_detect("style_general", style_name)) {
        df <- readWorkbook(wb, sheet_name)
        rows <- which(!duplicated(df[sep_var]))
        addStyle(
          wb, sheet_name, style_separate_row, 
          rows = c(rows[-1], dim(df)[1] + 1), 
          cols = 1:dim(df)[2], 
          gridExpand = T, stack = T
        )
        
        # TODO : à améliorer ? créer un argument pour all_separate dans table_share ?
        # -- séparation de toutes les lignes ?
        if (separate_all == T) {
          all_rows <- c(2:(dim(df)[1] + 1))
          all_rows <- all_rows[-(rows - 1)]
          addStyle(
            wb, sheet_name, 
            createStyle(
              # bordure
              border = "Bottom"
            ), 
            rows = all_rows, 
            cols = 1:dim(df)[2], 
            gridExpand = T, stack = T
          )
        }
      }
      
      # # -- widths et heights
      # removeColWidths(wb, sheet_name, cols)
      # removeRowHeights(wb, sheet_name, rows)
      # setColWidths(wb, sheet_name, cols, widths = "auto")
      # setRowHeights(wb, sheet_name, rows, heights = "auto")
      
    } # end of loop style_name_list
  } # end of loop sheet_name_list
  ##### /\ #####
  
  # retour de la fonction layout_wb
}




# TODO : décomposer avec une fonction afi_rewrite_xlsx ? (pour le cas où on réécrit les classeurs administrateurs)
afi_rewrite_disp <- function(
  wd = NULL, disp_2_edit = NULL, 
  output_lang = NULL, dir_LANG = NULL,
  styles_arch = NULL,
  output_dir = file.path(wd, "tables")
) {
  # output_lang = "DEU" # debug
  
  ##### 1/ Initialisation
  # -- chargement des données d'inventaire importées
  load(file.path(output_dir, "afiDonneesBrutes.Rdata"))
  
  # -- chargement des données administratives importées
  load(file.path(output_dir, "afiCodes.Rdata"))
  
  # -- choix du dispositif
  # disp_2_edit <- "1-Bois des Brosses" # debug
  df_list <- load("tables/afiDonneesBrutes.Rdata")
  check_all_msg <- "Editer les r\u00E9sultats pour tous les dispositifs"
  disp_list <- choose_disp(df_list, Dispositifs, check_all_msg)
  
  # -- chargement du dictionnaire de traduction
  # TODO : créer une fonction pour s'assurer que la table afiDictionary existe bien
  load(file.path(output_dir, "afiDictionary.Rdata"))
  
  # -- reconstitution de la table Arbres
  table_colnames <- with(dictionary, Attribut_FRA[Feuille == "Arbres"])
  Arbres <- 
    left_join(IdArbres, ValArbres, by = "IdArbre") %>% 
    select(all_of(table_colnames))
  
  # -- styles des titres -TODO : sauvegarder les styles dans une archive ---
  # Title1 <- createStyle(
  #   fontName = "Arial", fontSize = 12, fontColour = "dodgerblue4",
  #   border = "Bottom", fgFill = "tan1", textDecoration = "bold",
  #   wrapText = F,valign = "top",halign = "center", textRotation = 90
  # )
  # Title2 <- createStyle(
  #   fontName = "Arial", fontSize = 12, border = "Bottom",
  #   fgFill = "mediumaquamarine", textDecoration = "bold", wrapText = F,
  #   valign = "top",halign = "center", textRotation = 90
  # )
  # Title3 <- createStyle(
  #   fontName = "Arial", fontSize = 12, border = "Bottom",
  #   fgFill = "mediumaquamarine", textDecoration = "bold", wrapText = F,
  #   valign = "center",halign = "center", textRotation = 0
  # )
  # StyleGeneral <- createStyle(
  #   fontName = "Arial", fontSize = 12,
  #   # border= "RightBottom", textDecoration="bold",
  #   wrapText = F, valign = "center", halign = "center"
  # )
  # StyleNew <- createStyle(
  #   fontName = "Arial", fontSize = 12, border = "Bottom",
  #   # textDecoration="bold",fgFill="gray90",
  #   wrapText = F,
  #   valign = "center", halign = "center"
  # )
  # ---
  
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
  # niveau 4
  style_level_4 <- c("Commentaires")
  
  style_levels <- list(
    style_level_1,
    style_level_2,
    style_level_3,
    style_level_4
  )
  names(style_levels) <- c(
    "style_level_1",
    "style_level_2",
    "style_level_3",
    "style_level_4"
  )
  
  # -- distribution des styles entre les intitulés de colonne
  # TODO : faire une liste avec tous les styles pour tous les cas -> à enregistrer dans une archive
  share_styles_by_col <- 
    get_share_styles_by_col(style_levels)
  
  # -- liste des feuilles à éditer
  # -- table de référence contenant les noms des feuilles (permettra de trouver la langue)
  sheet_names_table <- 
    dictionary %>% filter(Emplacement == "Inventaire") # TODO : mettre une sécurité ici aussi ?
  # sheet names in the wb
  sheet_list <- sheet_names_table[, paste0("Attribut_", output_lang)] # sheet_list
  
  # -- liste des tables à éditer
  # replacement string
  replacement <- c("Reges", "BMortLineaires", "Coords")
  # strings to replace
  names(replacement) <- c("Rege", "BMortLineaire", "Coord")
  # call str_replace_all
  table_list <- 
    str_replace_all(string = sheet_names_table$Attribut_FRA, pattern = replacement)
  
  # -- tables
  db <- mget(table_names)
  
  
  
  # sheets_NAMES <-
  #   c("Arbres", "Taillis", "Rege", "BMortSup30", "BMortLineaire", "Coord") # Ici : se baser sur Dictionary_DF (cf fonction Dictionary2Rdata())
  # if (output_lang == "ENG") {
  #   sheets_NAMES <-
  #     c("Trees", "Coppice", "Regen", "DeadWoodOver30", "DeadWoodLinear", "Coord")
  # }
  # df_LIST <- list(
  #   Arbres, Taillis, Reges, BMortSup30, BMortLineaires, Coords
  # )
  
  # # -- barre de progression
  # pb <-
  #   tkProgressBar(
  #     title = "Traduction/r\u00E9\u00E9criture des classeurs d'inventaire",
  #     label = "Traduction/r\u00E9\u00E9criture des classeurs d'inventaire (%)",
  #     min = 0, max = 100, width=500
  #   )
  ##### \ / ###
  
  
  ##### 2/ Réécriture des classeurs d'inventaire
  # -- choix de la forme : 2 formes -> 1 classeur par dispositif ou 1 classeur unique (corrections générales)
  # answ <- tk_messageBox(
  #   type = "yesno", 
  #   message = "Editer un classeur d'inventaire rassemblant tous les dispositifs ?\n\n(si non : un classeur sera \u00E9dit\u00E9 par dispositif)"
  # )
  # 
  answ <- "no" # debug
  if (answ == "yes") {
    disp_2_edit <- "all_num"
  } else {
    disp_2_edit <- disp_list
  }
  
  # -- écriture du classeur
  for (disp in disp_2_edit) {
    # disp <- disp_2_edit[1] # debug
    
    # -- gestion du choix d'édition ("answ")
    # if (answ == "yes") {
    #   disp_num <- as.numeric(str_sub(disp_list, 1, str_locate(disp_list, "-")[, 1] - 1))
    #   disp_name <- "AFI_database"
    # } else {
      disp_num <- as.numeric(str_sub(disp, 1, str_locate(disp, "-")[, 1] - 1))
      disp_name <- str_sub(disp, str_locate(disp, "-")[, 2] + 1, -1)
    # }
    disp_name_rep <- clean_names(disp_name)
    
    # -- création du classeur
    wb <- createWorkbook()
    modifyBaseFont(wb, fontSize = 11, fontName = "Arial")
    
    for (i in 1:length(sheet_list)) {
      # -- initialisation
      sheet <- sheet_list[i]
      tmp <- db[[i]] # TODO : rajouter filtre si écriture des dispositifs 1 à 1
      if (answ == "no") {
        tmp <- tmp %>% filter(NumDisp == disp_num)
      }
      # if ("Cycle" %in% names(tmp)) {tmp <- tmp %>% filter(Cycle < 3)}
      
      # -- distribution des styles sur l'ensemble de la feuille à éditer
      # TODO ? : rajouter sécurité si on a une end_col > à une start_row (ex: colonne NumDisp positionnée au milieu)
      share_styles_by_sheet <- 
        get_share_styles_by_sheet(share_styles_by_col, tmp) %>% 
        mutate(sheets = sheet)
      
      # -- écriture des données dans le classeur
      # sheet_tr <- sheet_names_table[which(sheet_names_table$Attribut_FRA == sheet), paste0("Attribut_", output_lang)]
      addWorksheet(wb, sheet, gridLines = T)
      # traduction
      tmp <- translate_xlsx(
        table = tmp, var = sheet, 
        input_lang = "FRA", output_lang = output_lang, 
        dictionary = dictionary, translate_names = T
      )
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
      setRowHeights(wb, sheet, rows = 1, heights = 100)
      
      if (dim(tmp)[1] > 0) {
      ## general
      setRowHeights(wb, sheet, rows = 2:dim(tmp)[2], heights = "auto")
      
      # -- commande pour figer les volets
      freezePane(wb, sheet, firstActiveRow = 2)
      }
    } # end of loop '1:length(sheet_list)'
    
    
    ##### 3/ Sauvegarde et écriture du classeur
    # -- création des répertoires de sauvegarde
    # répertoire de sortie
    # dir_LANG <- clean_names(dir_LANG)
    # -- création du dossier de sortie
    # output_dir <- file.path( wd, "out", clean_names(disp) )
    output_dir <- if (answ == "yes") {
      paste0("out/", disp_name_rep, "/translation/", output_lang, "/inventory_files")
    } else {
      paste0("out/", disp_num, "-", disp_name_rep, "/translation/", output_lang, "/inventory_files")
    }
    dir.create(output_dir, showWarnings = F, recursive = T)
    # chemin du fichier
    file_path <- file.path(output_dir, paste0(disp_name_rep, "_inventaires.xlsx"))
    # sauvegarde
    saveWorkbook(wb, file_path, overwrite = T)
    
    # # -- MAJ de la barre de progression
    # info <- round(match(disp, disp_2_edit) / length(disp_2_edit) * 100)
    # setTkProgressBar(
    #   pb, info, paste0("Edition (", info, " \u0025)"),
    #   paste0(info, "\u0025 done")
    # )
  } # end of loop disp_2_edit
  
  # # -- close barre de progression
  # close(pb)
  ##### / \ ###
  
  # -- message de fin
  msg <- tk_messageBox(
    type = "ok",
    message =
      ifelse(
        lang == "FRA",
        "Traduction du/des classeur(s) d'inventaire termin\u00E9e",
        "Translation of inventory folder(s) complete"
      ),
    icon = "info"
  )
}
