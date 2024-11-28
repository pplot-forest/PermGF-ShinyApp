#' Construction de l'archive Dictionary.Rdata
#' @description La fonction permet d'importer ou de traduire le classeur utilisé dans les traductions français/anglais.
#'
#' @return Génère une archive Dictionary.Rdata, indispensable au traitement des données, dans le dossier 'tables'.
#'
#'
#' @author Valentin Demets
#' @param wd = répertoire contenant les données
#' @param lang = langue (sélectionnée) de l'interface
#' @param trad = paramètre permettant d'éditer une version des classeurs d'inventaire en langue française ou anglaise
#' @param file = choix du classeur contenant les éléments de traduction
#'
#' @import tcltk
#' @import openxlsx
#' @import tools
#' @export
#'
afi_Dictionary2Rdata <- function( # TODO : changer en read_dictionary
  wd = NULL, file = NULL, lang = "FRA", trad = F
) {
  ##### 1/ Initialisation #####
  # -- choix du répertoire de travail
  setwd(wd)
  
  # -- création du df vide
  df <- data.frame()
  
  # -- liste des onglets
  needed_sheets <- c(
    "Admin", "Adresses", "Algan", "Arbres", 
    "BMortLineaire", "BMortSup30", 
    "Cle", "ClePrixRege", "CodeDurete", "CodeEcorce", "CodesEcolo", "CodeStadeDecomposition", "CodeTypoArbres", "CoefActu", "CoefftCouvert", "Coord", "Cycles", 
    "DensiteBoisMort", "Depenses", "Dispositifs", 
    "Essences", "EssRegeneration", 
    "Feuilles", "ForfaitCouts", 
    "Haut", 
    "InfosSuiviEco", 
    "Liste", "Livret_AFI", 
    "Placettes", "PlanComptable", "PrixRege", "PU", "PUvar", 
    "Qual", 
    "Recettes", "Rege", 
    "Stations", 
    "Taillis", "Tarifs", "TauxCarboneBoisMort"
  )
  
  # -- Liste des intitulés de colonne
  column_names <- c("Emplacement", "Attribut_FRA", "Attribut_ENG", "Attribut_DEU")
  ##### / \ #####
  
  
  ##### 2/ Import du classeur de traduction #####
  # -- test si nombre d'onglet du classeur à importer est cohérent
  sheet_names <- getSheetNames(file)
  if (length(sheet_names) < length(needed_sheets)) {
    stop("Missing sheets in the Workbook")
  }
  
  # -- Lecture du classeur
  for (df_name in sheet_names) { # N.B : 40 feuilles en tout
    # df_name <- sheet_names[1] # debug
    # print(df_name) # debug
    # Lecture de l'onglet df_name
    df_temp <- read.xlsx(
      file, df_name
    ) %>% 
      select(all_of(column_names)) %>% 
      mutate(Feuille = df_name) # Attribut_DEU = NA (pour création colonne vide)
    
    # Sécurité sur le nombre de colonnes
    if (dim(df_temp)[2] < length(column_names)) { stop("Columns missing") }
    
    # Empilement des df
    df <- df %>% rbind(df_temp)
  }
  
  
  # -- Traduction - à revoir ?
  # # Détection de la langue (ENG/FR) => traduction ou non
  # var = "Feuilles"
  # var_TRAD <- var
  # to_LANG <- "FRA"
  # lang2_VECT <- paste0("Attribut_", to_LANG) # attr_LANG
  # lang2_QUOT <- quo(!!parse_expr(lang2_VECT)) # attr_LANG
  # # Version anglaise
  # if (length(intersect(sheet_names, sheets_ENG)) == length(sheets_ENG)) {
  #   from_LANG = "ENG" # lang
  #   lang1_VECT <- paste0("Attribut_", from_LANG) # attr_LANG
  #   lang1_QUOT <- quo(!!parse_expr(lang1_VECT)) # attr_LANG
  #   
  #   var_TRAD <- "Sheets"
  # }
  # # Version allemande
  # # ...
  # # ...
  # 
  # # Traduction si nécessaire
  # if (var_TRAD != "Feuilles") {
  #   sheets_DF <- 
  #     df %>% 
  #     filter(is.element(Feuille, var_TRAD)) %>% 
  #     select(!!lang1_QUOT, Attribut_FRA) %>%
  #     rename(
  #       Feuille := !!lang1_QUOT,
  #       Feuille_FRA = Attribut_FRA
  #     )
  #   df <- 
  #     df %>% 
  #     left_join(sheets_DF, by = "Feuille") %>%
  #     mutate(
  #       Feuille = Feuille_FRA,
  #       Feuille_FRA = NULL
  #     )
  # }
  ##### / \ #####
  
  
  ##### 3/ Sauvegarde #####
  # -- Modifications de la table df
  df <- 
    df %>% 
    mutate(
      Emplacement = str_replace(Emplacement, "Intitul\u00E9 de colonne", "ColName"), 
      Emplacement = str_replace(Emplacement, "Colonne ", "Col_")
    )
  
  dictionary <- df %>% rbind(NA)
  # -- Sauvegarde sous forme d'archive
  save(
    dictionary, 
    file = "tables/afiDictionary.Rdata"
  )
  Msg <- tk_messageBox(
    type = "ok", 
    message = ifelse(
      lang == "FRA", 
      "Archivage du classeur de traduction termin\u00E9", 
      "Saving of translation workbooks completed"
    ), 
    icon = "info"
  )
  
  
  ##### 4/ Traduction + édition du classeur - à revoir #####
  # if (trad == T) {
  #   ##### TRAD-1/ Choix de la langue dans laquelle traduire les classeurs #####
  #   # Construction du df contenant les arguments
  #   TabLang <-
  #     tibble(
  #       items = c(
  #         "Fran\u00E7ais / French",
  #         "Anglais / English",
  #         "Fran\u00E7ais / French",
  #         "Anglais / English"
  #       ),
  #       lang = c("FRA", "FRA", "ENG", "ENG"),
  #       langEdit = c("Anglais", "Fran\u00E7ais", "English", "French"),
  #       langMsg = c("anglaise", "fran\u00E7ais", "english", "french")
  #     )
  #   
  #   # Définition des choix de langage possibles (d'après 'lang')
  #   choix <- TabLang$langEdit[which(TabLang$lang == lang)]
  #   
  #   # Fenêtre de dialogue
  #   langEdit <-
  #     tk_select.list(
  #       choix,
  #       title =
  #         ifelse(
  #           lang == "FRA",
  #           "Choix de la langue dans laquelle éditer le classeur",
  #           "Please select the language for editing"
  #         )
  #     )
  #   
  #   langDir <- langEdit
  #   langEdit <-
  #     ifelse(
  #       langEdit == "French" | langEdit == "Fran\u00E7ais",
  #       "FRA", "ENG"
  #     )
  #   
  #   # Message
  #   msg_langEdit <- TabLang$langMsg[TabLang$lang == lang & TabLang$langEdit == langDir]
  #   # load("Tables/Translate.Rdata")
  #   print(
  #     ifelse(
  #       lang == "FRA",
  #       paste0("Edition du classeur de traduction en version ", msg_langEdit),
  #       paste0("Editing of translation workbooks in ", msg_langEdit, " version")
  #     )
  #   )
  #   
  #   
  #   ##### TRAD-2/ Edition du classeur Excel #####
  #   afi_RewriteTranslate(wd, to_LANG = langEdit, dir_LANG = langDir)
  #   ##### / \ #####
  # }
}


#' Fonction d'édition du classeur de traduction
#' @description La fonction permet d'écrire le classeur de traduction de la base AFI, dans la langue choisie (FRA/ENG)
#'
#' @import tcltk
#' @import openxlsx
#' @import tools
#' @export
#'
afi_RewriteTranslate <- function(
  wd = NULL, to_LANG = NULL, dir_LANG = NULL,
  output_dir = file.path(wd, "tables")
) {
  ##### 1/ Initialisation #####
  # -- Chargement du dictionnaire
  load(file.path(output_dir, "afiDictionary.Rdata"))
  
  # -- Gestion des langues
  # Vecteurs
  lang1_VECT <- paste0("Attribut_", from_LANG)
  lang2_VECT <- paste0("Attribut_", to_LANG)
  # Quotations
  lang1_QUOT <- quo(!!parse_expr(lang1_VECT))
  lang2_QUOT <- quo(!!parse_expr(lang2_VECT))
  
  # -- Styles des titres
  # TODO : sauver les styles dans l'archive des styles
  Style0 <- createStyle(
    border =  "TopBottomLeftRight", 
    fgFill = "lightgoldenrod3", 
    textDecoration = "bold", wrapText = F, 
    valign = "center", halign = "center", 
    textRotation = 0
  )
  Style01 <- createStyle(
    border =  "Right", 
    fgFill = "lightgoldenrod1", 
    wrapText = T, 
    valign = "center", halign = "center"
  )
  Style1 <- createStyle(
    border =  "TopBottomLeftRight", 
    fgFill = "lightcyan3", 
    textDecoration = "bold", wrapText = T, 
    valign = "center", halign = "center", 
    textRotation = 0
  )
  Style11 <- createStyle(
    wrapText = T, valign = "center", halign = "center"
  )
  StyleGrid0 <- createStyle(
    border = "TopRight", 
    fgFill = "lightgoldenrod1",
    wrapText = T, 
    valign = "center", halign = "center"
  )
  StyleGrid1 <- createStyle(
    border = "Top", valign = "center", halign = "center"
  )

  # -- Noms de feuilles
  sheet_names <- 
    dictionary %>% 
    filter(Feuille == "Feuilles") %>% 
    select(Attribut_FRA) %>% 
    unlist() %>% 
    unname()
  ##### \ / #####
  
  
  ##### 2/ Réécriture du classeur de traduction #####
    # -- Création du classeur
    wb <- createWorkbook()
    modifyBaseFont(wb, fontSize = 11, fontName = "Arial")
    
    for (i in 1:length(sheet_names)) {
      # -- Initialisation
      sheet <- sheet_names[i]
      # print(sheet) # debug
      df <- 
        dictionary %>% 
        filter(Feuille == sheet) %>%
        select(-Feuille)
      
      # -- Attribution des styles
      posGrid <- match(unique(df$Emplacement), df$Emplacement)
      
      # -- Traduction des intitulés de colonne et des valeurs
      # df <- translate_xlsx(df, sheet, "FRA", to_LANG)
      # Traduction du nom de l'onglet à traduire :
      var_TRAD <-
        dictionary %>%
        filter(Feuille == "Feuilles" & Attribut_FRA == sheet) %>%
        select(!!lang2_QUOT) %>%
        as.character()
      
      # -- Ecriture des feuilles du classeur
      # Rajout des feuilles
      addWorksheet(wb, sheetName = var_TRAD)
      # Ecriture des df dans les feuilles respectives
      writeData(wb, sheet = var_TRAD, df)
      # Ajout des styles
      addStyle(
        wb, # style du titre
        sheet = var_TRAD, 
        Style0, 
        rows = 1, cols = 1
      )
      addStyle(
        wb, # style du titre
        sheet = var_TRAD, 
        Style1, 
        rows = 1, cols = 2:dim(df)[2]
      )
      addStyle(
        wb, 
        sheet = var_TRAD, 
        Style01, 
        rows = 2:(dim(df)[1] + 1), cols = 1
      )
      addStyle(
        wb, 
        sheet = var_TRAD, 
        Style11, 
        rows = rep(2:(dim(df)[1] + 1), dim(df)[2] - 1), 
        cols = sort( rep(2:dim(df)[2], (dim(df)[1])) )
      )
      addStyle(
        wb, #Style lignes séparation
        sheet = var_TRAD, 
        StyleGrid0, 
        rows = posGrid + 1,
        cols = sort(rep(1, length(posGrid)))
      )
      addStyle(
        wb, #Style lignes séparation
        sheet = var_TRAD, 
        StyleGrid1, 
        rows = rep(posGrid + 1, dim(df)[2] - 1), 
        cols = sort(rep(2:dim(df)[2], length(posGrid)))
      )
      # Format des cellules
      # a) largeur
      removeColWidths(
        wb, 
        sheet = var_TRAD, 
        cols = 1:dim(df)[2]
      )
      setColWidths(
        wb, 
        sheet = var_TRAD, 
        cols = 1:dim(df)[2], 
        widths = rep(40, dim(df)[2])
      )
      # b) hauteur
      removeRowHeights(
        wb, 
        sheet = var_TRAD, 
        rows = 1:dim(df)[1]
      )
      setRowHeights(
        wb, 
        sheet = var_TRAD, 
        rows = 1:(dim(df)[1] + 1), 
        heights =  30
      )
    }
    ##### \ / #####
    
    
    ##### 3/ Sauvegarde et écriture du classeur #####
    # -- Création des répertoires de sauvegarde
    dir_LANG <- gsub("\u00E7", "c",  dir_LANG, fixed = T)
    output_dir <- paste0("out/translation/",  tolower(dir_LANG))
    output_dir <- if (lang == "FRA") {
      paste0(output_dir, "/dictionnaire")
    } else {
      paste0(output_dir, "/dictionary")
    }
    dir.create(output_dir, showWarnings = F, recursive = T)
    
    saveWorkbook(
      wb,
      file = file.path(output_dir, "dictionary.xlsx"),
      overwrite = T
    )
    
    # -- msg
    msg <- tk_messageBox(
      type = "ok",
      message =
        ifelse(
          lang == "FRA",
          "Ecriture du classeur de traduction termin\u00E9e", 
          "Writing of translation workbooks completed"
        ),
      icon = "info"
    )
}
