##### fonction pour récupérer les paramètres d'un classeur à l'import (langue + type - inventaire, admin, economie ou codifications). Attention : settings définis par les noms d'onglet !!
get_wb_settings <- function(
  file = NULL,
  sheet_names_table = NULL
) {
  # file <- "/Users/Valentin/Travail/Outils/GitHub/PermAFI2/data/excel/inventaires/2-Bois du Chanois.xlsx" # debug
  
  # -- get sheet names
  sheet_names <- getSheetNames(file)
  
  # -- recherche de la langue
  look_up_settings <- 
    sheet_names_table %>% 
    select(-Feuille) %>%
    
    # noms en colonnes
    pivot_longer(
      cols = -"Emplacement",
      names_to = "language",
      values_to = "sheet_name"
    ) %>% 
    
    # on filtre les feuilles reconnues dans le classeur importé et on en retire la langue
    filter(sheet_name %in% sheet_names) %>% 
    
    # get the most represented language + Emplacement (si mélange des classeurs)
    group_by(language, Emplacement) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    arrange(desc(count)) %>% 
    head(1) %>% # 1st row = most represented language
    select(language, Emplacement)
  
  # -- get input language and wb nature
  input_lang <- gsub("Attribut_", "", look_up_settings$language)
  input_wb <- look_up_settings$Emplacement
  
  # -- return from 'get_language_from_wb' function
  return(list(lang = input_lang, wb = input_wb))
}



#' Import des données administrateurs (classeur AFIListes.xlsx)
#' @description La fonction permet d'importer les données administrateurs contenues dans le classeur 'AFIListes.xlsx' et de les convertir au format .Rdata (archive R).
#'
#' @return Génère l'archive AFICodes.Rdata dans le dossier 'tables' du répertoire administrateur.
#'
#' @author Bruciamacchie Max
#' @param repAFI = répertoire contenant les données
#' @param ListFile = classeur contenant les données administrateurs ('AFI_Listes.xlsx')
#' @param lang = langue (sélectionnée) de l'interface
#' @param trad = paramètre permettant d'éditer une version des classeurs d'inventaire en langue française ou anglaise
#'
#' @import tcltk
#' @import openxlsx
#' @import tools
#' @export

afi_CodesTranslation <- function(
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

  # filtre des classeurs ouverts
  pos_Open <- grep("~$", files_list, fixed = T)
  if (length(pos_Open) > 0) files_list <- files_list[-pos_Open]
  
  # message
  print(i18n()$t("Import des classeurs administrateurs"))
  
  ##### 1/ detect settings #####
  # -- detect wb files settings
  
  # - initialise
  # list for each file
  files <- vector(mode = "list", length = length(files_list))
  names(files) <- files_list
  
  # settings list
  settings <- vector(mode = "list", length = 2)
  names(settings) <- c("lang", "wb")
  
  # fill files list
  for (i in 1:length(files_list)) {
    files[[i]] <- settings
  }
  
  
  # - loop
  # -- table de référence contenant les noms des feuilles (permettra de trouver la langue)
  all_sheet_names_table <- 
    # dictionary %>% filter(Emplacement %in% paste("Classeur", c("Admin", "Economie", "Codifications")))
    dictionary %>% filter(Emplacement %in% c("Admin", "Economie", "Codifications"))
  
  for (i in 1:length(files_list)) {
    file <- files_list[[i]]
    # print(file) # debug
    
    # -- get input lang
    input_settings <- get_wb_settings(file, sheet_names_table = all_sheet_names_table)
    files[[file]]["lang"] <- input_settings$lang
    files[[file]]["wb"] <- input_settings$wb
  }
  
  
  
  # -- test si tous les classeurs admin présents / archive afiCodes.Rdata présente
  
  # wb_types <- lapply(files, function(x) x[["wb"]])
  wb_types <- sapply(files, function(x) x[["wb"]])
  
  # sécurité doublons (Classeur admin sélectionné 2 fois)
  duplicated <- which( duplicated(wb_types) )
  if (length(duplicated) > 0) {
    print(paste0(
      i18n()$t("Il y a des classeurs administrateurs sélectionnés 2x : "),
      wb_types[duplicated]
      ))
  }
  
  # test si tous les types présents pour les classeurs admin
  admin_wb <- which(wb_types %in% c("Admin", "Economie", "Codifications"))
  
  # all admin wb selected
  if (length(admin_wb) == 3) {
    print(i18n()$t("Tous les classeurs admin sont présents"))
  }
  
  # missing admin wb in selection
  if (length(admin_wb) < 3 & length(admin_wb) > 0) {
    print(i18n()$t("Tous les classeurs admin n'ont pas été sélectionnés")) # TODO : transformer en alerte shiny
    
    if ( !"afiCodes.Rdata" %in% list.files(output_dir) ) {
      print(i18n()$t("Tous les classeurs admin n'ont pas été sélectionnés et l'archive 'afiCodes.Rdata' n'existe pas. Sélectionnez tous les classeurs administrateurs pour un premier import des données"))  # TODO : transformer en alerte shiny
    }
    load("tables/afiCodes.Rdata")
  } # end of cond "length(admin_wb) < 3 & length(admin_wb) > 0'
  
  
  # test si classeur d'inventaire ?
  # TODO : rassembler afi_XlsTranslation et afi_CodesTranslation
  # inventory_wb <- which(wb_types %in% c("Admin", "Economie", "Codifications"))
  # message alert si archive existante ?
  ##### / \ #####
  
  
  ##### 2/ Import des classeurs d'inventaire #####
  # -- switch shiny ***
  # -- création de la barre de progression
  withProgress(
    message = i18n()$t("Import des données en cours"),
    detail = i18n()$t("Progression..."), value = 0, {
      # *** --
      
  # calcul du nombre total de tables à importer
  complete_progress <- nrow(filter(all_sheet_names_table, Emplacement %in% wb_types))
  
  
  # loop for each wb_types (admin, economie, codifications, inventaire)
  for (wb_type in wb_types) { # loop 'wb_types'
    # wb_type <- wb_types[1] # debug
    # -- files list
    admin_files <- files[sapply(files, function(x) x[["wb"]] == wb_type)]
    
    # -- table de référence contenant les noms des feuilles (permettra de trouver la langue)
    sheet_names_table <- 
      # dictionary %>% filter(Emplacement %in% paste("Classeur", c("Admin", "Economie", "Codifications")))
      dictionary %>% filter(Emplacement %in% wb_type)
    
    # initialize db list (contain tmp tables)
    db <- vector(mode = "list", length = dim(sheet_names_table)[1])
    names(db) <- sheet_names_table$Attribut_FRA # default column
    
    # import each files
    for (i in 1:length(admin_files)) {
      # i = 1 # debug
      # -- file name
      file <- names(admin_files)[i]
      
      # -- define file dictionary
      file_dictionary <- 
        dictionary %>% 
        filter(Feuille %in% sheet_names_table$Attribut_FRA) %>% 
        mutate(original = Attribut_FRA)
      
      # -- get input lang
      input_lang <- admin_files[[i]]$lang
      
      # -- update file_dictionary
      if (input_lang != "FRA") { # cond 'input_lang != "FRA"'
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
      } # end of cond 'input_lang != "FRA"'
      file_dictionary <- 
        file_dictionary %>% select(Feuille, Emplacement, Attribut_FRA, original)
      
      # needed sheets in the wb
      needed_sheets <- unique(file_dictionary$Feuille)
      # needed_sheets <- 
      #   sheet_names_table[, paste0("Attribut_", input_lang) ]
      
      # noms présents dans le classeur
      sheet_names_wb <- getSheetNames(file)
      
      # -- detect missing sheet
      # set up i18n()
      missing_names <- setdiff(needed_sheets, sheet_names_wb)
      if (length(missing_names) > 0) {
        stop("Missing sheets in the Workbook : ", paste0(missing_names, collapse = ", "))
      }
      print(paste0( 
        i18n()$t("Traduction des classeurs d'inventaire "), file, "..."
      ))
      
      # -- lecture des différentes tables administrateur
      for (sheet in needed_sheets) { # loop 'sheet in needed_sheets'
        # print(sheet) # debug
        # sheet <- needed_sheets[5] # debug
        tmp <- read_xlsx(
          file = file, 
          sheet_name = sheet, 
          input_lang = input_lang,
          file_dictionary = file_dictionary,
          dictionary = dictionary, # si besoin puor traduction des essences
          i18n = i18n
        )
        
        # suppression de la colonne NumDisp dans la table 'Adresses' (en cours de validation au 31/03/2021)
        if (sheet == "Adresses") tmp <- tmp %>% select(-NumDisp)
        
        # -- switch shiny ***
        # incrémentation de la barre de progression
        incProgress(1 / complete_progress)
        # *** --
        
        # -- table name
        # TODO : à revoir
        table_name <- if (input_lang != "FRA") {
          sheet_names_table$Attribut_FRA[match(sheet, sheet_names_table[, paste0("Attribut_", input_lang)])]
        } else sheet
        
        # tidy tmp
        tmp <- tidy_afi_table(tmp, column_types)
        
        
        # -- update table list
        # TODO : à mettre dans une fonction ?
        
        # feuille 'Cycles'
        if (sheet == "Cycles") {
          if (is.numeric(tmp$Date)) {
            tmp <-
              tmp %>% mutate( Date = convertToDate(Date), Date = format(Date, "%d/%m/%Y") )
          }
          
          # rename 'Année' column
          tmp$Annee <- as.numeric(as.character(tmp[, "Ann\u00E9e"]))
          tmp[, "Ann\u00E9e"] <- NULL
          
          # erreurs dues à une mauvaise lecture des classeurs Excel
          # - test 1
          pos1 <- which(tmp$Annee > 3000) # Si mauvaise lecture d'une date jj/mm/aaaa par Excel
          if (length(pos1) > 0) {
            # 1st correction
            tmp <- tmp %>% mutate( Annee = convertToDate(Annee), Annee = format(Annee, "%Y"))
            
            # - test 2
            pos <- which(tmp$Annee < 1900) # A revoir ? revu : 2000 remplacé par 1900
            if (length(pos) > 0) {
              print(paste0(
                i18n()$t("Attention valeurs d'ann\u00E9e fausses dans la feuille Cycles pour le dispositif "),
                Admin_FILE,
                i18n()$t(". Correction auto : Ann\u00E9e r\u00E9cup\u00E9r\u00E9e depuis date")
              ))
              
              # 2nd correction
              tmp <- tmp %>% mutate( Annee = format(Date, "%Y"), Annee = as.numeric(Annee) )
            }
          }
          
          # feuille 'Dispositifs'
        } else if (sheet == "Dispositifs" & class(tmp$DatInstallation) == "Date") {
          tmp <- tmp %>%  mutate(DatInstallation = format(DatInstallation, "%d/%m/%Y"))
          
          # feuille 'Stations'
        } else if (sheet == "Stations" & is.numeric(tmp$DateDescription)) {
          tmp <- tmp %>% mutate( DateDescription = convertToDate(DateDescription), DateDescription = format(DateDescription, "%d/%m/%Y"))
        }
        
        # assign table
        db[[ table_name ]] <- tmp
        
      } # end of loop 'needed_sheets'
    } # end of loop '1:length(admin_files)'
    
    # assign all tables listed in db
    for (i in 1:length(db)) {
      name <- names(db)[i]
      assign(name, db[[i]])
    }
  } # end of loop 'wb_types'
  
  # -- switch shiny ***
    })  # end of withProgress
  # *** --
  ##### / \ #####


  ##### 3/ Sauvegarde #####
  dir.create("tables", showWarnings = F)
  save(
    Algan, Recettes, Depenses, Cycles, Haut, EssRegeneration,
    ClePrixRege,
    CodesEcolo, CodeDurete, CodeEcorce,
    CodeTypoArbres, CoefftCouvert, Dispositifs, PlanComptable,
    PrixRege, Cle, ForfaitCouts, Stations, Admin, Adresses,
    CoefActu, Qual, PU, PUvar, InfosSuiviEco, Tarifs, Essences,
    CodeStadeDecomposition, TauxCarboneBoisMort, DensiteBoisMort,
    file = file.path(output_dir, "afiCodes.Rdata")
  )
  print(i18n()$t("Importation termin\u00E9e. Donn\u00E9es administrateurs archiv\u00E9es"))
  
  # -- switch shiny ***
  # ending alert
  show_alert(
    title = i18n()$t("Import des données termin\u00E9e !!"),
    text = i18n()$t("Fichiers administateurs archiv\u00E9s"),
    type = "success"
  )
  # *** --
  ##### / \ #####


  ##### 4/ Traduction + édition des classeurs #####
  if (trad == T) {
    ### TRAD-2/ Choix de la langue dans laquelle traduire les classeurs ###
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
        paste0("Traduction des classeurs administrateurs en version ", msg_langEdit),
        paste0("Translation of the admin workbooks in ", msg_langEdit, " version")
      )
    )


    ### TRAD-3/ Choix des classeurs administrateurs ###
    admin_LIST0 <- if (lang == "FRA") {
      c("Données admin", "Données économiques", "Données de codifications")
    } else {
      c("Admin data", "Economic data", "Codifications data")
    }
    Msg_AllCheck <- ifelse(
      lang == "FRA",
      "Editer tous les fichiers administrateurs",
      "Edit all the admin workbooks"
    )
    admin_LIST <- c(Msg_AllCheck, admin_LIST0)
    admin_LIST <-
      tk_select.list(
        as.character(admin_LIST),
        multiple = T,
        title =
          ifelse(
            lang == "FRA",
            "Choisir une ou plusieurs données administrateurs",
            "Choose one or several admin data"
          )
      )
    if (is.element(Msg_AllCheck, admin_LIST)) {admin_LIST <- admin_LIST0}
    admin_LIST <- c("Admin", "Eco", "Codes")[match(admin_LIST, admin_LIST0)]


    ### TRAD-4/ Edition des classeurs Excel ###
    afi_rewrite_admin(repAFI, admin_LIST, langEdit, langDir, dictionary)
  }
  ##### / \ #####
}


#' Fonction d'édition des classeurs Excel administrateurs
#' @description La fonction permet d'écrire les classeurs administrateurs de la base AFI, dans la langue choisie
#'
#' @import tcltk
#' @import openxlsx
#' @import tools
#' @export
#'

afi_rewrite_admin <- function( # TODO : à revoir
  repAFI, admin_LIST, to_LANG, dir_LANG, dictionary,
  output_dir = file.path(repAFI, "tables")
) {
  ### 1/ Initialisation ###
  # -- chargement des données administratives importées
  load(file.path(output_dir, "afiCodes.Rdata"))

  # -- styles des titres # TODO : à enregistrer dans une archive à part (ou une autre forme de fichier)
  Title1 <- createStyle(
    fontName = "Arial", fontSize = 12, fontColour = "dodgerblue4",
    border = "Bottom", fgFill = "tan1", textDecoration = "bold",
    wrapText = F,valign = "top",halign = "center", textRotation = 90
  )
  Title2 <- createStyle(
    fontName = "Arial", fontSize = 12, border = "Bottom",
    fgFill = "mediumaquamarine", textDecoration = "bold", wrapText = F,
    valign = "top",halign = "center", textRotation = 90
  )
  Title3 <- createStyle(
    fontName = "Arial", fontSize = 12, border = "Bottom",
    fgFill = "mediumaquamarine", textDecoration = "bold", wrapText = F,
    valign = "center",halign = "center", textRotation = 0
  )
  StyleGeneral <- createStyle(
    fontName = "Arial", fontSize = 12,
    # border= "RightBottom", textDecoration="bold",
    wrapText = F, valign = "center", halign = "center"
  )
  StyleNew <- createStyle(
    fontName = "Arial", fontSize = 12, border = "Bottom",
    # textDecoration="bold",fgFill="gray90",
    wrapText = F,
    valign = "center", halign = "center"
  )

  # -- gestion des langues
  # lang1_VECT <- paste0("Attribut_", from_LANG)
  lang2_VECT <- paste0("Attribut_", to_LANG)
  # quotations
  # lang1_QUOT <- quo(!!parse_expr(lang1_VECT))
  lang2_QUOT <- quo(!!parse_expr(lang2_VECT))

  # -- barre de progression
  pb <- # TODO : version en anglais/allemand
    tkProgressBar(
      title = "Traduction du/des classeur(s) administrateur(s)",
      label = "Traduction du/des classeur(s) administrateur(s) (\u0025)",
      min = 0, max = 100, width=500
    )
  ### \ / ###


  ### 2/ Réécriture des classeurs d'inventaire ###
  for (admin in admin_LIST) {
    # print(admin) # debug
    # admin <- admin_LIST[3] # debug

    # -- noms de feuilles
    # classeur Admin
    if (admin == "Admin") {
      sheets_FRA <-
        c("Admin", "Adresses", "Cle", "Cycles", "Dispositifs", "Stations")
    }

    # classeur Economie
    if (admin == "Eco") {
      sheets_FRA <- c(
        "CoefActu", "Depenses", "ForfaitCouts", "InfosSuiviEco",
        "PlanComptable", "Recettes"
      )
    }

    # classeur Codifications
    if (admin == "Codes") {
      sheets_FRA <- c(
        "Algan", "Essences", "Qual",
        "CoefftCouvert", "EssRegeneration",
        "CodeDurete", "CodesEcolo", "CodeEcorce", "CodeTypoArbres",
        "Haut", "Tarifs",

        "PrixRege", "ClePrixRege", "PU",
        "CodeStadeDecomposition", "TauxCarboneBoisMort", "DensiteBoisMort",
        "PUvar"
      )
    }

    # -- liste des intitulés d'onglets
    sheetsnames_DF <-
      dictionary %>%
      filter(Feuille == "Feuilles" & is.element(Attribut_FRA, sheets_FRA)) %>%
      mutate(Attribut_FRA = factor(Attribut_FRA, levels = sheets_FRA)) %>%
      arrange(Attribut_FRA) %>%
      mutate(Attribut_FRA = as.character(Attribut_FRA))
    sheets_NAMES <-
      sheetsnames_DF %>%
      select(Attribut_FRA) %>%
      unlist %>%
      unname()

    # -- création du classeur
    wb <- createWorkbook()
    modifyBaseFont(wb, fontSize = 11, fontName = "Arial")

    for (i in 1:length(sheets_NAMES)) {
      # print(i) # debug
      # i <- 1 # debug
      # -- initialisation
      sheet <- sheets_NAMES[i]
      df <- get(sheet)
      # if ("NumDisp" %in% names(df)) {df <- df %>% filter(NumDisp == 2)} # extract PSDRF2
      # if ("Cycle" %in% names(df)) {df <- df %>% filter(Cycle < 3)} # extract PSDRF2
      # Correction intitulé "Annee" en "Année"
      if ("Annee" %in% names(df)) {
        df <- df %>% rename_("Ann\u00E9e" = "Annee")
      }

      # -- attribution des styles
      # Style1 ---
      if (sheet == "CoefActu") {
        ColsTitle1 <- factor(colnames(df), levels = c("Ann\u00E9e"))
      }
      if (sheet %in% c("CodeDurete", "CodesEcolo", "CodeEcorce", "CodeTypoArbres",
                       "CodeStadeDecomposition","TauxCarboneBoisMort","DensiteBoisMort")) {
        ColsTitle1 <-
          factor(colnames(df), levels = c("Id", "Codification", "Code", "Code_Ancien"))
      }
      if (sheet %in% c("Essences", "PU","PUvar")) {
        ColsTitle1 <- factor(colnames(df), levels = "Essence")
      }
      if (sheet == "ForfaitCouts") {
        ColsTitle1 <- factor(colnames(df), levels = "TypeIntervenant")
      }
      if (sheet == "Depenses") {
        ColsTitle1 <- factor(
          colnames(df),
          levels = c(
            "Etat","Num", "NumDisp", "Nom", "Identifiant", "NumIFN", "Cycle"
          )
        )
      }
      if (!sheet %in% c(
        "Depenses", "PU", "CoefActu", "Essences", "ForfaitCouts", "PU",
        "CodeDurete", "CodesEcolo", "CodeEcorce", "CodeTypoArbres",
        "PUvar","CodeStadeDecomposition","TauxCarboneBoisMort","DensiteBoisMort"
      )) {
        ColsTitle1 <- factor(
          colnames(df),
          levels = c(
            "Etat", "Num", "NumDisp", "Identifiant",
            "NumIFN", "Nom", "Cycle", "Code"
          )
        )
      }
      ColsTitle1 <- sort(ColsTitle1)
      ColsTitle1 <- as.character(ColsTitle1)
      posTitle1 <- 1:length(ColsTitle1)

      # style2 ---
      if (sheet == "CoefActu") {
        ColsTitle2 <- factor(colnames(df), levels = "CoefActu")
      }
      if (sheet %in% c("CodeDurete", "CodesEcolo", "CodeEcorce", "CodeTypoArbres")) {
        ColsTitle2 <- factor(
          colnames(df),
          levels = c(
            "Couleur", "Descriptif", "Naturaliste", "Chiroptere", "Avifaune", "Entomo"
          )
        )
      }
      if (sheet %in% c("Essences", "PU","PUvar")) {
        ColsTitle2 <- factor(
          colnames(df),
          levels = c(
            "Latin", "EssReg", "Red", "Green", "Blue",
            "TypeEss", "InfraDensite", "CoefHouppier", "CoefStere",
            "CoefHoupp", "Couleur",

            "Classe", "Qual","PU"
          )
        )
      }
      if (sheet == "ForfaitCouts") {
        ColsTitle2 <- factor(colnames(df), levels = c("Co\u00FBtHoraire"))
      }
      if (sheet == "PrixRege") {
        ColsTitle2 <- factor(
          colnames(df),
          levels = c(
            "R\u00E9gion", "Essence", "Facilit\u00E9",
            "Classe1", "Classe2", "Classe3"
          )
        )
      }
      if (sheet %in% c("Depenses", "Stations")) {
        ColsTitle2 <- factor(
          colnames(df),
          levels = c(
            "Essence", "Classe", "Hauteur",
            "Commune", "Dep", "NomDep", "R\u00E9gion",

            "Date", "Ann\u00E9e", "CoefActu", # "Ann\u00E9e"

            "EssenceCatDiam", "Volume",

            "Facilit\u00E9", "Classe1", "Classe2", "Classe3",

            "Adresse1", "Adresse2", "CPT", "Ville", "Pays", "Tel",
            "Port", "Fax", "E-Mail", "Organisme", "CodeGest", "Op\u00E9rateur",

            "Changt", # "Nom",
            "RegionRege", "EssenceRege", "FaciliteRege",
            "CodeEcolo", "Descriptif",

            "Mois", "Quantit\u00E9", "Unit\u00E9",

            "Qual", "Qual1", "Qual2", "PU", "Haut",

            "TotalHT", "Nature", "Code",

            # "Essence",
            "Latin", "EssReg", "Red", "Green", "Blue",
            "TypeEss", "InfraDensite", "CoefHouppier", "CoefStere",
            "CoefHoupp", "Couleur",

            "TypeTarif", "NumTarif",

            "SuiviDendro", "SuiviEcolo", "SuiviEcono", "MiseAJour SuiviEco",
            "D\u00E9partSuiviEco", "RotationCoupesBO", "AnneesCoupesBO",
            "AnneesCoupesBI", "AnneesChablis", "AnneesPr\u00E9visionCoupes",

            "Op\u00E9ration", "Cat\u00E9gorie", "D\u00E9tail",

            "Parcelle", "SurfParc", "SurfForet", "DatInstallation", "DispAssoci\u00E9",

            "a", "b", "Estimation",
            # "Date", "Ann\u00E9e", "Mesure Arbre", "Mesure Houppiers",
            "Mesure Arbre", "Mesure Houppiers",

            "Mesure BM", "Mesure MicroH", "Op\u00E9rateur1",
            "Op\u00E9rateur2", "Op\u00E9rateur3", "Op\u00E9rateur4",
            "Coefft", "Raisons", "Caract\u00E9ristiques", "Commentaires",
            "Photo", "NbPlacettes", "Financeurs",

            "NomRegionNaturelle", "NumSER", "NomSylvoEcoReg", "AuteurDescription",
            "DateDescription", "CatalogueStation", "DateCatalogue",
            "TypeStations", "CarteG\u00E9ologique", "EchelleCarteG\u00E9ol",
            "NbStratesG\u00E9ol", "StratesG\u00E9ologiques", "TypeRochesM\u00E8res",
            "CarteP\u00E9dologique", "P\u00E9dologieAssoci\u00E9e", "GroupeP\u00E9doAFI",
            "Regions\u0026Th\u00E8mes", "ObservationsCompl\u00E9mentaires", "Type", "Compo"
          )
        )
      }
      
      if (!sheet %in% c(
        "Depenses", "Stations", "CoefActu", "Essences", "ForfaitCouts", "PU",
        "CodeDurete", "CodesEcolo", "CodeEcorce", "CodeTypoArbres",
        "PrixRege"
      )
      ) {
        ColsTitle2 <- factor(
          colnames(df),
          levels = c(
            "Essence", "Classe", "Hauteur",
            "Commune", "Dep", "NomDep", "R\u00E9gion",

            "Date", "Ann\u00E9e", "CoefActu",  # "Ann\u00E9e"

            "EssenceCatDiam", "Volume",

            "Facilit\u00E9", "Classe1", "Classe2", "Classe3",

            "Adresse1", "Adresse2", "CPT", "Ville", "Pays", "Tel",
            "Port", "Fax", "E-Mail", "Type", "Organisme", "CodeGest", "Op\u00E9rateur",

            "Changt",  # "Nom",
            "RegionRege", "EssenceRege", "FaciliteRege",
            "CodeEcolo", "Descriptif",


            "Mois", "Quantit\u00E9", "Unit\u00E9",

            "Qual", "Qual1", "Qual2", "PU", "Haut",

            "TotalHT", "Nature",

            # "Essence",
            "Latin", "EssReg", "Red", "Green", "Blue",
            "TypeEss", "InfraDensite", "CoefHouppier", "CoefStere",
            "CoefHoupp", "Couleur",

            "TypeTarif", "NumTarif",

            "SuiviDendro", "SuiviEcolo", "SuiviEcono", "MiseAJour SuiviEco",
            "D\u00E9partSuiviEco", "RotationCoupesBO", "AnneesCoupesBO",
            "AnneesCoupesBI", "AnneesChablis", "AnneesPr\u00E9visionCoupes",

            "Op\u00E9ration", "Cat\u00E9gorie", "D\u00E9tail",

            "Parcelle", "SurfParc", "SurfForet", "DatInstallation", "DispAssoci\u00E9",

            "a", "b", "Estimation",
            # "Date", "Ann\u00E9e", "Mesure Arbre", "Mesure Houppiers",
            "Mesure Arbre", "Mesure Houppiers",

            "Mesure BM", "Mesure MicroH", "Op\u00E9rateur1",
            "Op\u00E9rateur2", "Op\u00E9rateur3", "Op\u00E9rateur4",
            "Coefft", "Raisons", "Caract\u00E9ristiques", "Commentaires",
            "Photo", "NbPlacettes", "Financeurs",

            "NomRegionNaturelle", "SylvoEcoReg", "AuteurDescription",
            "DateDescription", "CatalogueStation", "DateCatalogue",
            "TypeStations", "CarteG\u00E9ologique", "EchelleCarteG\u00E9ol",
            "NbStratesG\u00E9ol", "StratesG\u00E9ologiques", "TypeRochesM\u00E8res",
            "CarteP\u00E9dologique", "P\u00E9dologieAssoci\u00E9e", "GroupeP\u00E9doAFI",
            "Regions\u0026Th\u00E8mes", "ObservationsCompl\u00E9mentaires", "Compo"
          )
        )
      }

      ColsTitle2 <- sort(ColsTitle2)
      ColsTitle2 <- as.character(ColsTitle2)
      posTitle2 <- max(posTitle1, na.rm = T) + 1:length(ColsTitle2)

      # style3 ---
      ColsTitle3 <- factor(
        colnames(df),
        levels = c("Observations", "Actualisation", "ModeVente", "Surface")
      )
      ColsTitle3 <- sort(ColsTitle3)
      ColsTitle3 <- as.character(ColsTitle3)
      if (length(ColsTitle3) > 0) {
        if (length(ColsTitle3) > 1) {
          posTitle3 <- max(posTitle2, na.rm = T) + 1:(length(ColsTitle3)) # Colonne Commentaires reste horizontale
        } else {posTitle3 <- max(posTitle2, na.rm = T) + 1}
      } else {posTitle3 <- NULL}
      #
      df <- df[, c(ColsTitle1, ColsTitle2, ColsTitle3)]
      # ---

      # -- Traduction des intitulés de colonne et des valeurs
      df <- translate_xlsx(df, sheet, "FRA", to_LANG)

      # -- Ecriture des feuilles du classeur
      # traduction du nom d'onglet
      sheet <-
        sheetsnames_DF %>%
        filter(Attribut_FRA == sheet) %>%
        select(!!lang2_QUOT) %>%
        unlist() %>%
        unname()
      # Rajout des feuilles
      addWorksheet(wb, sheetName = sheet)
      # Ecriture des df dans les feuilles respectives
      writeData(wb, sheet = sheet, df)
      # Ajout des styles
      addStyle(wb, sheet, Title1, rows = 1, cols = posTitle1, gridExpand = T)
      if (sheet %in% c("Arbres", "Trees")) {
        addStyle(
          wb, sheet, Title2,
          rows = rep(1, (length(ColsTitle2) - 5)),
          cols = posTitle2[1:(length(posTitle2) - 5)]
        )
        addStyle(
          wb, sheet, Title3,
          rows = rep(1, length(ColsTitle3)),
          cols = posTitle3 - 5
        )
        addStyle(
          wb, sheet, Title2,
          rows = rep(1, 5),
          cols =
            posTitle2[(length(posTitle2) - 4):length(posTitle2)] + length(posTitle3)
        )
      } else {
        addStyle(
          wb, sheet, Title2,
          rows = rep(1, length(ColsTitle2)),
          cols = posTitle2
        )
        addStyle(
          wb, sheet, Title3,
          rows = rep(1, length(ColsTitle3)),
          cols = posTitle3
        )
      }

      # Style Général
      if (dim(df)[1] > 0) {
        addStyle(
          wb, sheet, StyleGeneral, rows = 2:(dim(df)[1] + 1), cols = 1:dim(df)[2],
          gridExpand = T
        )
      }
      addStyle(
        wb, sheet, StyleNew, rows = dim(df)[1] + 1, cols = 1:dim(df)[2],
        gridExpand = T, stack = T
      )
      # addStyle(wb, sheet, Title2, rows = 1, cols=posTitle2, gridExpand = T)
      # addStyle(wb, sheet, Title3, rows = 1, cols=posTitle3, gridExpand = T)
      # Format des cellules

      removeColWidths(wb, sheet = sheet, cols = 1:dim(df)[2])
      removeRowHeights(wb, sheet = sheet, rows = 1)

      setColWidths(wb, sheet = sheet, cols = 1:dim(df)[2], widths = "auto")
      setRowHeights(wb, sheet = sheet, rows = 1, heights = 100)
    } # end of loop 1:length(sheets_NAMES)
    ### \ / ###


    ### 3/ Sauvegarde et écriture du classeur ###
    # -- création des répertoires de sauvegarde
    # répertoire de sortie
    dir_LANG <- clean_names(dir_LANG)
    output_dir <- 
      paste0("out/AFI_database/translation/", dir_LANG, "/admin_files")
    dir.create(output_dir, showWarnings = F, recursive = T)
    # chemin du fichier
    file_path <- file.path(output_dir, paste0("AFI_", admin, ".xlsx"))
    # sauvegarde
    saveWorkbook(wb, file_path, overwrite = T)
    
    # -- MAJ de la barre de progression
    info <- round(match(admin, admin_LIST) / length(admin_LIST) * 100)
    setTkProgressBar(
      pb, info, paste0("Edition (", info, " \u0025)"),
      paste0(info, "\u0025 done")
    )
  } # end of loop admin_LIST
  
  # -- close barre de progression
  close(pb)
  ### / \ ###
  
  # -- message de fin
  msg <- tk_messageBox(
    type = "ok",
    message =
      ifelse(
        lang == "FRA",
        "Traduction des classeurs administrateurs termin\u00E9e",
        "Translation of the admin workbooks complete"
      ),
    icon = "info"
  )
}
