# ----- Chargement des packages nécessaires -----
suppressMessages({
  library("openxlsx")
  library("dplyr")
  library("sf")
  library("readxl")
  library("stringr")
  library("rlang")
})

##### fonction d'import des tables d'inventaire #####
read_GF_table <- function(
  file = NULL, 
  table_name = NULL, 
  column_to_check = NULL
) {
  # column_to_check <- c("NumForet", "Nom", "Propri\u00E9taire", "Gestionnaire", "SurfForet") # debug
  # -- parameters
  # number of columns
  column_nb <- length(column_to_check)
  
  # -- reading table
  table <- read.xlsx(file, sheet = table_name) %>% select(one_of(column_to_check))
  table_names <- names(table)
  
  # checking columns names
  missing_columns <- column_to_check[ which(!column_to_check %in% table_names) ]
  if (length(missing_columns) > 0) {
    stop(
      "Les noms de colonnes de l'onglet ", !!table_name,
      " ne sont pas corrects.\n\nRappel : liste des colonnes de l'onglet Foret (",
      column_nb, " au total) =\n'", paste0(column_to_check, collapse = "','"), "'"
    )
  }
  
  # -- retour de la fonction read_GF_table
  return(table)
}

##### fonction pour contrôler les feuilles du classeur d'inventaire #####
check_workbook_sheets <- function(
  file = NULL, sheets_names = NULL
  ) {
  # -- paramètres
  # feuilles du classeur d'inventaire
  workbook_sheets <- getSheetNames(file)
  
  # nombre de feuilles demandées
  sheets_nb <- length(sheets_names)
  
  # feuilles manquantes
  missing_sheets <- sheets_names[ which(!sheets_names %in% workbook_sheets) ]
  if (length(missing_sheets) > 0) {
    stop(
      "Les noms d'onglet du classeur Excel en import ne sont pas corrects.\n\nRappel : liste des onglets devant figurer dans le classeur (",
      sheets_nb, " au total) =\n'", 
      paste0(sheets_names, collapse = "','"),
      "'\nIl manque les onglets :\n", 
      paste0(missing_sheets, collapse = ", ")
    )
  }
}


##### fonction de conversion des Coords au format shape #####
Coords2SHP <- function(Coords = NULL) {

  if (dim(Coords)[1] > 0) {

    # Fonction conversion df en sf
    convert2SHP <- function(shape) {
      shape %>%
        st_as_sf(
          coords = c("Xgps", "Ygps"),
          crs = unique(shape$EPSG),
          remove = F) %>%
        st_transform(crs = 2154)# %>%
      # mutate(
      #   Xgps=st_coordinates()[,1],
      #   Ygps=st_coordinates()[,2]
      # ) %>%
      # select(NumForet, NumPlac, EPSG, Observations, Xgps, Ygps, geometry)
    }

    # -- Tri entre les EPSG
    pos_L93 <- which(Coords$EPSG==2154)
    pos_WGS84 <- which(Coords$EPSG==4326)
    pos_UTM32 <- which(Coords$EPSG==32632 | Coords$EPSG=="32T")
    pos_UTM31 <- which(Coords$EPSG==32631 | Coords$EPSG=="31T")

    # Sécurité sur les EPSG reconnus
    pos_Autres <- which(
      !is.element(Coords$EPSG, c(2154, 4326, 32632, "32T", 32631, "31T")) &
        !is.na(Coords$EPSG)
    )
    if (length(pos_Autres) > 0) {
      Msg2 <- stop(
        message = "Attention, des syst\u00E8mes de projection (EPSG) ne sont pas reconnus dans la feuille 'Coords' du classeur d'inventaire.
        \nRappel - EPSG reconnus :\nLambert 93 = '2154'\nMercator = '4326'\nUTM32 = '32632' ou '32T'\nUTM31 = '32631' ou '31T'"
      )
    }
    # Sécurité sur les EPSG vides
    pos_Vides <- which(is.na(Coords$EPSG))
    if (length(pos_Vides) > 0) {
      Msg2 <- stop(
        message = "Attention, il y a des syst\u00E8mes de projection (EPSG) non renseignés (vides) dans la feuille 'Coords' du classeur d'inventaire.
        \nRappel - EPSG reconnus :\nLambert 93 = '2154'\nMercator = '4326'\nUTM32 = '32632' ou '32T'\nUTM31 = '32631' ou '31T'"
      )
    }
    # -- Création des shapes selon le/les EPSG détecté(s)
    # EPSG = Lambert 93
    if (length(pos_L93) > 0) {
      Plac_L93 <-
        filter(Coords, EPSG==2154 & !is.na(Xgps) & !is.na(Ygps)) %>%
        mutate(
          Xgps = as.numeric(Xgps),
          Ygps = as.numeric(Ygps)
        ) %>%
        convert2SHP()
      # Récupération du shape créé :
      Placettes_SHP <- Plac_L93
    }
    # EPSG = Mercator
    if (length(pos_WGS84) > 0) {
      Plac_WGS84 <-
        filter(Coords, EPSG==4326 & !is.na(Xgps) & !is.na(Ygps)) %>%
        mutate(
          Xgps = as.numeric(Xgps),
          Ygps = as.numeric(Ygps)
        ) %>%
        convert2SHP()
      # Récupération du shape créé :
      Placettes_SHP <-
        if (exists("Placettes_SHP") && dim(Placettes_SHP)[1] > 0) {
          rbind(Placettes_SHP, Plac_WGS84)
        } else Placettes_SHP <- Plac_WGS84
    }
    # EPSG = UTM 32
    if (length(pos_UTM32) > 0) {
      Plac_UTM32 <-
        filter(Coords, (EPSG == 32632 | EPSG == "32T") & !is.na(Xgps) & !is.na(Ygps)) %>%
        mutate(
          EPSG = 32632,
          Xgps = as.numeric(Xgps),
          Ygps = as.numeric(Ygps)
        ) %>%
        convert2SHP()
      # Récupération du shape créé :
      Placettes_SHP <-
        if (exists("Placettes_SHP") && dim(Placettes_SHP)[1] > 0) {
          rbind(Placettes_SHP, Plac_UTM32)
        } else Placettes_SHP <- Plac_UTM32
    }
    # EPSG = UTM 31
    if (length(pos_UTM31) > 0) {
      Plac_UTM31 <-
        filter(Coords, (EPSG == 32631 | EPSG == "31T") & !is.na(Xgps) & !is.na(Ygps)) %>%
        mutate(
          EPSG = 32631,
          Xgps = as.numeric(Xgps),
          Ygps = as.numeric(Ygps)
        ) %>%
        convert2SHP()
      # Récupération du shape créé :
      Placettes_SHP <-
        if (exists("Placettes_SHP") && dim(Placettes_SHP)[1] > 0) {
          rbind(Placettes_SHP, Plac_UTM31)
        } else Placettes_SHP <- Plac_UTM31
    }

    # -- Rassemblement des données
    Placettes_SHP <-
      Placettes_SHP %>%
      mutate(
        NumForet=as.numeric(NumForet),
        Xgps=st_coordinates(Placettes_SHP)[,1],
        Ygps=st_coordinates(Placettes_SHP)[,2]
      )
    } else {
      Placettes_SHP <- Coords # Cas où Coords vide
    }

  # retour de la fonction Coords2SHP
  return(Placettes_SHP)
  }


### A voir à l'utilisation si utile d'intégrer cette fonction ?
##### fonction changement des essences ######
replace_ESS <- function(
  df = NULL, Essence_CHAMP = NULL, species_table = NULL
) {
  df %>%
    mutate(
      Essence_temp =
        species_table$Nom[match(!!parse_expr(Essence_CHAMP), species_table$Code)],
      !!parse_expr(Essence_CHAMP) :=
        ifelse(is.na(Essence_temp), !!parse_expr(Essence_CHAMP), Essence_temp),
      Essence_temp = NULL
    )
}

##### fonction gf_Xls2RData : import du classeur .xlsx (modèle Max B) #####
# TODO : use archive for names parameters ?
# = Import des données contenues dans le classeur d'inventaire
gf_Xls2Rdata <- function(
  wd = NULL,
  files_list = NULL, 
  output_dir = file.path(wd, "tables"),
  i18n = NULL
) {
  
  ##### 1/ Initialisation #####
  # -- chargement du dictionnaire de traduction
  # TODO : créer une fonction pour s'assurer que la table afiDictionary existe bien
  load(file.path(output_dir, "gf_Dictionary.Rdata"))
  
  # TODO : à sauvegarder dans une archive de paramètres ?
  # define column types
  column_types <- list(
    numeric = c(
      "NumForet", "NumArbre", "Cycle", "IdArbre", "Azimut", "Dist",
      "Diam1", "Diam2", "Diam", 
      "HautT", "HautL", "Haut", 
      "Ray1", "Dh1", "Ray2", "Dh2",
      "Nbre", "Stade", "Transect", "Angle",
      "DiamIni", "DiamMed", "DiamFin", "Longueur",
      "Recouv", "Class1", "Class2", "Class3", "Abroutis", "Rejet",
      "Contact", "Chablis"
    ),
    character = c("NumPlac", "Essence", "Type"),
    rounded_values = c("HautT", "HautL", "Haut", "Ray1", "Dh1", "Ray2", "Dh2"),
    no_empty_values = c("Recouv", "Class1", "Class2", "Class3", "Rejet", "Abroutis", "Angle")
  )
  
  # filtre des classeurs ouverts
  pos_Open <- grep("~$", files_list, fixed = T)
  if (length(pos_Open) > 0) files_list <- files_list[-pos_Open]
  
  # message
  print(i18n()$t("Traduction des classeurs d'inventaire"))
  
  # -- création de la barre de progression
  # -- switch shiny ***
  withProgress(
    message = i18n()$t("Import des données en cours"),
    detail = i18n()$t("Progression..."), value = 0, {
      # *** --
      
      # -- table de référence contenant les noms des feuilles (permettra de trouver la langue)
      # TODO : remplacer par une archive avec les noms de feuilles / tables ?
      sheet_names_table <-
        dictionary %>% filter(Emplacement == "Inventaire") # TODO : mettre une sécurité ici aussi ?
      
      # # calcul du nombre total de tables à importer # barre de progression
      # complete_progress <- length(files_list) * dim(sheet_names_table)[1]
      
      # -- error list
      error <- vector(mode = "list", length = nrow(sheet_names_table))
      names(error) <- sheet_names_table$Attribut_FRA # default column
      
      # -- initialize list of all disp numbers
      all_forest_num <- c()
      
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
        
        # -- define file dictionary # TODO : à supprimer (pas )
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
        
        # -- test des intitulés d'onglets à importer (on vérifie qu'il y a bien toutes les feuilles nécessaires)
        # needed sheets in the wb
        needed_sheets <- unique(file_dictionary$Feuille)
        
        # noms présents dans le classeur
        sheet_names_wb <- getSheetNames(file)
        
        # -- detect missing sheet # TODO : passer par une fonction check_workbook_sheets ?
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
          # sheet <- needed_sheets[9] # debug
          # print(sheet) # debug
          tmp <- read_gf_xlsx(
            file = file, 
            sheet_name = sheet, 
            input_lang = input_lang,
            file_dictionary = file_dictionary,
            dictionary = dictionary, # si besoin pour traduction des essences
            i18n = i18n
          )
          
          # -- switch shiny ***
          # incrémentation de la barre de progression
          # incProgress(1 / complete_progress)
          # *** --
          
          # table name
          # TODO : à revoir
          table_name <- if (input_lang != "FRA") {
            sheet_names_table$Attribut_FRA[match(sheet, sheet_names_table[, paste0("Attribut_", input_lang)])]
          } else sheet
          
          # update error list
          if (is.element(TRUE, is.na(tmp$NumForet))) {
            error[[ table_name ]] <- c(
              error[[ table_name ]],
              # add file name
              str_sub(file, 1, str_locate(file, "-") - 1)[1]
            )
          }
          
          # update NumForet list
          if ("NumForet" %in% names(tmp)) {
            all_forest_num <- union(all_forest_num, unique(tmp$NumForet))
          }
          
          # # update NumPlac (cas où on traite plusieurs massifs différents avec risque de doublon des N° de placette)
          # if ("NumPlac" %in% names(tmp)) {
          #   tmp <- tmp %>% 
          #     mutate(
          #       NumPlac = paste0(NumForet, '-', NumPlac),
          #       NumForet = 18
          #       )
          # }
          
          # tidy tmp (convert numeric columns as numeric, character columns as character, etc.)
          tmp <- tidy_gf_table(
            table = tmp, table_name = table_name, column_types = column_types
          )
          
          # update table list
          db[[ table_name ]] <- rbind(db[[ table_name ]], tmp)
          
        } # end of loop 'sheet in needed_sheets'
      } # end of loop 'i in 1:length(files_list)'
      
      
      ##### 3/ Corrections ####
      # -- rename db tables before stack
      # replacement string
      replacement <- 
        c("AcctD", "BMSLineaires", "Cats", "Cercles", "Coords", "CodeEcolos", "Cycles", "Echantillonnages", "Forets", "Quals", "Reges", "Regroups", "Tarifs")
      # N.B : changement "BMSCercle" -> "BMSCercles" réalisé avec "Cercle" -> "Cercles"
      
      # strings to replace
      names(replacement) <-
        fixed(c("AccD", "BMSLineaire", "Cat", "Cercle", "Coord", "CodeEcologie", "Cycle", "Echantillonnage", "Foret", "Qual", "Rege", "Regroup", "Tarif"))
      
      # call str_replace_all
      names(db) <-
        str_replace_all(string = names(db), pattern = replacement)
      
      # -- superposition des tables (si archive déjà existante)
      if ( "gfDonneesBrutes.Rdata" %in% list.files(path = "tables") ) { # cond '"gfDonneesBrutes.Rdata" %in% list.files(path = "tables"'
        # - 1. On filtre les tables déjà présentes dans l'archive (num du disp importé est exclus)
        # -- Numéros des dispositifs nouvellement importés
        imported_data_forest_num <- all_forest_num
        
        # -- noms des tables en archive
        table_names <- load(file.path(output_dir, "gfDonneesBrutes.Rdata"))
        # recreate "Arbres" table
        Arbres <- 
          left_join(IdArbres, ValArbres, by = "IdArbre") %>% 
          mutate(IdArbre = NULL)
        
        for (table in table_names) { # loop 'table in table_names'
          # print(table) # debug
          # table <- table_names[28] # debug
          # -- suppression des données d'inventaire correspondant à imported_data_forest_num
          # archived data
          arch <- get(table)
          if ("NumForet" %in% names(arch)) {
            arch <- arch %>% filter(!NumForet %in% imported_data_forest_num)
          }
          # security to suppress after re-import
          if ("add_deadwood_dmh" %in% names(arch)) {
            arch <- arch %>% select(-tmp, -add_deadwood_dmh)
          }
          # security - à éliminer après travail shiny ?
          arch <- tidy_gf_table(table = arch, column_types = column_types)
          
          # - 2. On ajoute les tables nouvellement importées
          # -- stack newly imported data
          # imported data
          tmp <- db[[table]]
          # all data stacked (except Coord) # TODO : à voir si nécessaire de faire Coords en fonction des cycles ?
          stack <- if (table != "Coords") rbind(arch, tmp) else tmp
          # si tables de codification, supprimer les doublons
          stack <- if (table %in% c("CodeEcolos", "CodeDurete", "CodeEcorce", "CodeTypoArbres", "Essences")) distinct(stack) else stack
          # reassign
          assign(table, stack)
        } # end of loop 'table in table_names'
      } else {
        # -- noms des tables en archive
        table_names <- sort( names(db) )
        # update table_names
        table_names <- c(table_names, "IdArbres", "ValArbres")
        table_names <- table_names[!table_names == "Arbres"]
        
        for (table in names(db)) { # loop 'table in names(db)'
          # si tables de codification, supprimer les doublons
          db[[table]] <- if (table %in% c("CodeDurete", "CodeEcorce", "CodeTypoArbres")) distinct(db[[table]]) else db[[table]]
          
          assign(table, db[[table]])
        } # end of loop 'table in names(db)'
      } # end of cond '"gfDonneesBrutes.Rdata" %in% list.files(path = "tables"'
      
      
      # -- tabe ARBRES : build id for tree tables # TODO : principe à utiliser pour toutes les tables le jour où base AFI passe sur PostGre ?
      # define id_columns
      id_columns <- 
        c("NumForet", "NumPlac", "NumArbre", "Essence", "Azimut", "Dist")
      # call 'set_id' function
      db_tables <- 
        set_db_id(table = db$Arbres, id_columns = id_columns, id_var = "IdArbre")
      # assign tables (value_table = 'Arbres' and id_table = 'Id_Arbres')
      IdArbres <- db_tables$id_table # add table 'IdArbres'
      ValArbres <- db_tables$value_table # add table 'ValArbres'
      
      
      # -- détection des premiers oublis
      # attention : il est important qu'aucun vide dans les NumForet ne passe car les Verif
      # se font par rapport à NumForet. Impossible de localiser le vide (quel dispositif ?) ailleurs
      # qu'à cette étape
      warning_msg <- c() 
      troubling_tables <- which( lapply(error, is.null) == FALSE )
      
      for (i in troubling_tables) { # loop 'i in troubling_tables'
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
      
      
      ##### 4/ Sauvegarde #####
      dir.create("tables", showWarnings = F)
      save(
        list = table_names, file = file.path(output_dir, "gfDonneesBrutes.Rdata")
      )
      
      print(
        i18n()$t("Importation termin\u00E9e. Fichiers d'inventaire archiv\u00E9s")
      )
      # -- switch shiny ***
    }) # end of withProgress

  # ending alert
  show_alert(
    title = i18n()$t("Import des données termin\u00E9e !!"),
    text = i18n()$t("Fichiers d'inventaire archiv\u00E9s"),
    type = "success"
  )
  # *** --
  ##### / \ #####
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

##### fonction pour récupérer la langue d'un classeur excel (attention : définie par les noms d'onglet !!) #####
# TODO : use 'get_wb_settings' function
get_language_from_wb <- function(
  file = NULL,
  sheet_names_table = NULL#,
  # i18n = NULL
) {
  # file <- "/Users/Valentin/Travail/Outils/GitHub/PermAFI2/data/excel/inventaires/2-Bois du Chanois.xlsx" # debug
  
  # -- get sheet names
  sheet_names <- getSheetNames(file)
  
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
read_gf_xlsx <- function(
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
  df <- read.xlsx(file, sheet_name, detectDates = T)
  
  # clean names
  # reconvertit les espaces, convertis en point lors de la lecture du file, en underscore
  # + enlève les accents
  names(df) <- clean_names(names(df))
  column_names <- clean_names(column_names)
  sheet_dictionary$original <- clean_names(sheet_dictionary$original)
  sheet_dictionary$Attribut_FRA <- clean_names(sheet_dictionary$Attribut_FRA)
  
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
  if ("Nbre" %in% names(df) & sheet_name != "Echantillonnage-new") {
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
        function(x) column_dictionary$Attribut_FRA[match(x, column_dictionary$original)]
      )
    } # end of cond 'nrow(column_dictionary) > 0'
    
    
    # - 2/ traduction enregistrée dans une autre table
    if ("Essence" %in% names(df)) {
      dictionary_tmp <- dictionary %>% filter(Feuille == "Essences", Emplacement == "Col_Nom")
      df$Essence <- dictionary_tmp$Attribut_FRA[
        match( df$Essence, dictionary_tmp[, paste0("Attribut_", input_lang)] )
        ]
    }
    if ("Type" %in% names(df)) {
      dictionary_tmp <- dictionary %>% filter(Feuille == "CodeTypoArbres", Emplacement == "Col_Code")
      df$Type <- dictionary_tmp$Attribut_FRA[
        match( df$Type, dictionary_tmp[, paste0("Attribut_", input_lang)] )
        ]
    }
  } # end of cond 'nrow(df) > 0 && input_lang != "FRA"'
  
  # -- retour de la fonction read_gf_xlsx
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

tidy_gf_table <- function(table = NULL, table_name = NULL, column_types = NULL) {
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
  
  # -- return from 'tidy_gf_table' function
  return(table)
}



















