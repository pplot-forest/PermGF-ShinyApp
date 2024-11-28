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
  fontName = "Arial", fontSize = 12, 
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
  share_style_general <- 
    share_table %>% 
    mutate(
      column_level = max(share_table$column_level) + 1,
      styles = "style_general",
      start_row = 2,
      # end_row = dim(table_to_edit)[1] + 2, # au cas où dim(table_to_edit = 0 => alors start_row serait > end_row)
      end_row = 10000,
      start_col = 1,
      end_col = dim(table_to_edit)[2],
      separate_var = var
    ) %>% 
    distinct()
  
  share_table <- share_table %>% rbind(share_style_general)
  
  # retour de la fonction get_share_styles_by_sheet
  return(share_table)
}

##### fonction de fusion des cellules #####
merge_cells <- function(
  wb, sheet, df, group, col
) {
  # df <- suivi_to_edit
  # group <- c("NumDisp", "Cycle") # debug
  # col <- "Strate" # debug
  
  # -- paramètres
  group_vars <- c(group, col)
  col_pos <- match(col, names(df))
  
  # -- positions des cellules à fusionner (min/max)
  merge_positions <- 
    df %>% 
    group_by_at(group_vars) %>% 
    # arrange(NumDisp) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(
      max = cumsum(n) + 1,
      min = max - n + 1
    ) %>% 
    filter(n > 1)
  
  # -- fusion des cellules
  for (i in 1:dim(merge_positions)[1]) {
    print(i) # debug
    tmp <- merge_positions[i, ]
    mergeCells(wb, sheet, rows = c(tmp$min, tmp$max), cols = col_pos)
  }
  
  # retour de la fonction merge_cells
  # if (sav == T) {return(Table)}
}

##### fonction de mise à jour des feuilles pour une nouvelle saisie #####
drain_table <- function(
  df = NULL, num = NULL, last_cycle = NULL, 
  stack = FALSE, col = NULL
  ) {
  # initialisation
  df <- df %>% filter(NumDisp == num & Cycle == last_cycle)
  
  # sécurité
  if (dim(df)[1] >= 1) {
    
    # choix 1 = on ne garde qu'une ligne + on vide toutes les colonnes sauf NumDisp et Cycl
    if (stack == FALSE) {
      df <- 
        df %>% 
        head(1) %>% 
        mutate(Cycle = last_cycle + 1)
      df[, !names(df) %in% c("NumDisp", "Cycle")] <- NA
    }
    
    if (stack == TRUE) {
      # choix 2 (feuille Arbres) = on rajoute une table avec certaines colonnes vides et Cycle = last_cycle + 1
      # + on rajoute 15 lignes vides
      
      
      tmp <- df %>% mutate(Cycle = last_cycle + 1)
      empty <- tmp %>% distinct(NumDisp, NumPlac, Cycle, .keep_all = T)
      empty[, !names(empty) %in% c("NumDisp", "NumPlac", "Cycle")] <- NA
      empty <- empty[ rep( 1:dim(empty)[1], 15 ), ]
      
      # vide certaines colonnes
      tmp[, !names(tmp) %in% c("NumDisp", "Cycle", col)] <- NA
      
      df <- 
        df %>% 
        rbind(tmp) %>% 
        rbind(empty) %>% 
        arrange(NumDisp, NumPlac, NumArbre)
    }
  }
  
  # retour de la fonction drain_table
  return(df)
}


#' Edition du classeur de saisie des remesures.
#' @description La fonction permet d'éditer un classeur excel formaté pour faciliter la saisie des remesures d'inventaire (pour le prochain passage en inventaire)
#'
#' @return Genère un classeur excel
#'
#' @author Valentin Demets, Bruciamacchie Max
#'
#' @param repAFI = répertoire contenant les données
#' @param lang = langue (sélectionnée) de l'interface
#'
#' @import tcltk
#' @import openxlsx
#' @import stringr
#' @export

afi_ClasseurRem <- function(
  wd = NULL,
  files_list = NULL,
  lang = "FRA"
) {
  ##### 1/ Initialisation #####
  # -- chargement du dictionnaire de traduction
  # TODO : créer une fonction pour s'assurer que la table afiDictionary existe bien
  load(file.path(wd, "tables/afiDictionary.Rdata"))
  
  # -- chargement des données d'inventaire et administratives
  # define Rdata to load
  inventory_data <- file.path(wd, "tables/afiDonneesBrutes.Rdata")
  admin_data <- file.path(wd, "tables/afiCodes.Rdata")
  
  # # -- chargement des données
  # load("tables/afiDonneesBrutes.Rdata")
  # load("tables/afiCodes.Rdata")
  # load("tables/afiDictionary.Rdata")
  
  # loading
  inventory_tables <- load(inventory_data)
  inventory_tables <- c(
    "IdArbres", "BMortSup30","BMortLineaires", "Cycles", #"Placettes",
    "Reges", "Coords", "Taillis"
  )
  load(admin_data)
  
  # -- choix du dispositif
  if (is.null(files_list)) {
    # initialisation
    check_all_msg <- i18n()$t("Ecrire le classeur de remesure pour tous les dispositifs")
    # } else {
    #   "Write the remeasure workbook for every stands"
    # }
    # df_list <- load("tables/afiDonneesBrutes.Rdata")
    disp_list <- choose_disp(inventory_tables, Dispositifs, check_all_msg) %>% 
      clean_names()
  } else disp_list <- basename( file_path_sans_ext(files_list) ) %>% clean_names()
  
  # --  création de la barre de progression
  disp_num <- str_sub(disp_list[1], str_locate(disp_list[1], "-")[, 2] + 1, -1)
  pb_title <- i18n()$t("Progression")
  pb_label <- paste0(
    i18n()$t("Edition des classeurs de remesures : 0\u0025 done - dispositif "), 
    disp_num, i18n()$t(" en cours.")
  )
  # } else {
  #   paste0(
  #     "Writing of remeasure workbook(s) : 0\u0025 done - stand ", 
  #     disp_num, " in progress."
  #   )
  # }
  pb <- tkProgressBar(pb_title, pb_label, 0, 100, width = 800)
  ##### / \ #####
  
  
  # -- table de référence contenant les noms des feuilles (permettra de trouver la langue)
  # TODO : remplacer par une archive avec les noms de feuilles / tables ?
  sheet_names_table <-
    dictionary %>% filter(Emplacement == "Inventaire")
  
  # -- define file dictionary
  file_dictionary <- 
    dictionary %>% 
    filter(Feuille %in% sheet_names_table$Attribut_FRA) %>% 
    mutate(original = Attribut_FRA)
  
  
  ##### 2/ Préparation des tables #####
  for (disp in disp_list) { # loop 'disp in disp_list'
    # disp <- disp_list[1] # debug
    # -- gestion des noms et num du dispositif
    disp_num <- as.numeric(str_sub(disp, 1, str_locate(disp, "-")[, 1]-1)) #changement2
    disp_name <- 
      str_sub(disp, str_locate(disp, "-")[, 2] + 1, -1)
    last_cycle <- get_last_cycle(inventory_tables, disp)
    
    dir.create(paste0("out/", disp_num, "-", disp_name), showWarnings = F)
    # ----- Sécurité si le dispositif n'est pas en archive (cf PermAFI - à voir si utile  => dans ce cas adapter FichesRem et PlansArbres ----- #
    
    # -- préparation des tables d'inventaire en tour (disp)
    # arbres
    cols = c(
      "NumDisp", "NumPlac", "NumArbre", 
      "Cycle", "Essence", "Azimut", "Distance"
    )
    arbres <- 
      IdArbres %>% 
      left_join(ValArbres, by = "IdArbre") %>% 
      # récupération des Observations - TODO : à garder dans afi_Calculs ?
      drain_table(disp_num, last_cycle, stack = TRUE, col = cols) %>% 
      select(
        NumDisp, Cycle, NumPlac, NumArbre, Essence, Azimut, Distance, 
        Diam1, Diam2, HautT, HautL, Ray1, Dh1, Ray2, Dh2, Qual, 
        Observations, CodeEcolo, CodeEcoloAncien, Type, Stade, Coupe
      ) %>% 
      arrange(NumDisp, NumPlac, Azimut, Distance, Cycle)
    
    # bois mort au sol > 30
    bm_sup30  <- 
      BMortSup30 %>% 
      drain_table(disp_num, last_cycle) %>% 
      select(
        NumDisp, NumPlac, Cycle, #Id, NumArbre, 
        Essence, #Azimut, Distance, #Orientation, 
        DiamIni, DiamFin, DiamMed, Longueur, Contact, Chablis, 
        Stade, Observations
      )
    
    # coords
    coords <- 
      Coords %>% 
      filter(NumDisp == disp_num) %>% 
      select(
        NumDisp, NumPlac, X, Y, SystGPS, 
        CoordGPS1, CoordGPS2, 
        Coefft, DiamLim, 
        Observations # TODO : supprimer CoordGPS1 et CoordGPS2 ?
      )
    
    # bois mort au sol < 30
    bm_lineaire <- 
      BMortLineaires %>% 
      drain_table(disp_num, last_cycle) %>% 
      select(
        NumDisp, Cycle, NumPlac, Transect, Essence, 
        Diam, Angle, Contact, Chablis, Stade, Observations
      )
    
    # régénération
    reges <- 
      Reges %>% 
      drain_table(disp_num, last_cycle) %>%
      select(
        NumDisp, NumPlac, Cycle, SsPlac, Essence, 
        Recouv, Class1, Class2, Class3, Rejet, Abroutis, 
        Observations
      )
    
    # taillis
    taillis <- 
      Taillis %>% 
      drain_table(disp_num, last_cycle) %>%
      select(
        NumDisp, Cycle, NumPlac, Essence, Azimut, Distance, 
        Nbre, Diam, Haut, Observations
      )
    ##### /\ #####
    
    ##### 3/ Edition des classeurs de remesure #####
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
    share_styles_by_col <- 
      get_share_styles_by_col(style_levels)
    
    # -- liste des tables à éditer
    table_list <- list( # TODO : nécessaire de garder arbres ? ou ne garder que Arbres ?
      arbres, taillis, reges, bm_lineaire, 
      bm_sup30, coords
    )
    # -- liste des feuilles du classeur à éditer
    sheet_list <- c(
      "Arbres", "Taillis", "Rege", "BMortLineaire", 
      "BMortSup30", "Coord"
    )
    
    wb <- createWorkbook()
      
      # default style
      modifyBaseFont(wb, fontSize = 12, fontColour = "black", fontName = "Arial")
    
    for (i in 1:length(table_list)) { # loop 1:length(table_list)
      # print(i) # debug
      # -- données et nom de la table
      tmp <- 
        table_list[[i]] %>% 
        filter(NumDisp == disp_num)
      sheet <- sheet_list[i]
      print(sheet)
      
      # -- distribution des styles sur l'ensemble de la feuille à éditer
      # TODO ? : rajouter sécurité si on a une end_col > à une start_row (ex: colonne NumDisp positionnée au milieu)
      share_styles_by_sheet <- 
        get_share_styles_by_sheet(share_styles_by_col, tmp) %>% 
        mutate(sheets = i18n()$t(sheet))
      
      # if (sheet == "Arbres") {
      #   # function pour ajouter des styles différents dans général ? TODO ?
      #   posArchive <- which(tmp$Cycle != last_cycle)
      #   posArchive <- posArchive + 1
      #   posNew <- which(tmp$Cycle == last_cycle) #(1:dim(tmp)[1])[-posArchive] #
      #   share_style_general_former <- 
      #     share_styles_by_sheet %>% 
      #     filter(str_detect("style_general", styles)) %>% 
      #     mutate(end_row)
      #   
      #   share_styles_by_sheet <- 
      #     share_styles_by_sheet
      #     filter(!str_detect("style_general", styles))
      # }
      
      # -- écriture des données dans le classeur
      addWorksheet(wb, i18n()$t(sheet), gridLines = T, zoom = 100)
      # traduction
      if (lang != "FRA") { # cond 'lang != "FRA"'
        # -- liste des intitulés de colonne
        sheet_dictionary <- file_dictionary %>% filter(Feuille == sheet)
        
        if (nrow(tmp) > 1) { # cond 'nrow(tmp) > 1'
          # -- traduction des valeurs des colonnes
          # 2 possibilités : 
          # - traduction disponible dans sheet_dictionary 
          # (cas de certaines colonnes des tables administrateurs 
          # comme 'Type' dans la table 'Dispositifs')
          
          # - traduction doit être récupérée dans une autre table 
          # (cas des colonnes 'Essence' et 'Type')
          
          # - 1/colonnes repérées dans sheet_dictionary
          # column_to_translate <- # inutile
          #   with(sheet_dictionary, Attribut_FRA[which(Emplacement != "ColName")])
          column_dictionary <- sheet_dictionary %>% filter(Emplacement != "ColName")
          
          # traduction
          if (nrow(column_dictionary) > 0) { # cond 'nrow(column_dictionary) > 0'
            # get columns to translate
            columns_to_translate <- unique( gsub("Col_", "", column_dictionary$Emplacement) )
            
            for (column in columns_to_translate) {
              # column <- columns_to_translate[1] # debug
              tmp <- tmp %>% 
                mutate(
                  translation = !!sym(column),
                  translation = case_when(
                    is.na(translation) ~ translation,
                    !is.na(translation) ~ i18n()$t(translation)
                  ),
                  !!sym(column) := translation,
                  translation = NULL
                )
            }
          } # end of cond 'nrow(column_dictionary) > 0'
          
          
          # - 2/ traduction enregistrée dans une autre table
          if ("Essence" %in% names(tmp)) {
            dictionary_tmp <- dictionary %>% filter(Feuille == "Essences", Emplacement == "Col_Essence")
            # tmp$Essence <- i18n()$t(tmp$Essence)
            tmp["Essence"] <- lapply( tmp["Essence"], function(x) {
              pos = which(!is.na(x))
              if (length(pos) > 0) x[pos] = i18n()$t(x[pos])
              x
              } )
          }
          if ("Type" %in% names(tmp)) {
            dictionary_tmp <- dictionary %>% filter(Feuille == "CodeTypoArbres", Emplacement == "Col_Code")
            # tmp$Type <- i18n()$t(tmp$Type)
            tmp["Type"] <- lapply( tmp["Type"], function(x) {
              pos = which(!is.na(x))
              if (length(pos) > 0) x[pos] = i18n()$t(x[pos])
              x
            } )
          }
        } # end of cond 'nrow(tmp) > 1'
        
        # - translate and rename column names with chosen language :
        names(tmp) <- i18n()$t(names(tmp))
        
        # -- translate sheet
        sheet <- i18n()$t(sheet)
      } # end of cond 'lang != "FRA"'
      writeData(wb, sheet, tmp)
      
      
      # -- mise en forme
      layout_wb(
        wb, styles = styles_list, share_styles_by_sheet,
        style_separate_medium_row, separate_all = T
      )
      
      if (sheet %in% i18n()$t("Arbres")) { # TODO : à mettre plus haut dans share_styles_general ?
        posArchive <- which(tmp$Cycle != last_cycle + 1) # attention décalage !
        posArchive <- posArchive + 1
        posNew <- which(tmp$Cycle == last_cycle + 1) #(1:dim(tmp)[1])[-posArchive] #
        
        addStyle(
          wb, sheet, style_former_value, rows = posArchive, cols = 1:ncol(tmp), 
          gridExpand = T, stack = T
        )
        addStyle(
          wb, sheet, style_new_value, rows = posNew + 1, cols = 1:ncol(tmp), 
          gridExpand = T, stack = T
        )
      }
      
      # -- set cell widths and cell heights
      # remove widths and heights
      removeColWidths(wb, sheet = sheet, cols = 1:ncol(tmp))
      removeRowHeights(wb, sheet, rows = 1)
      
      
      # new cell widths
      ## title
      # TODO : à intégrer dans la fonction layout_wb ? 
      # TODO : supprimer pos_Thin1 & Cie
      setColWidths(wb, sheet = sheet, cols = 1:ncol(tmp), widths = 5)
      # orange columns
      orange_col <- match(i18n()$t(c("NumPlac", "NumArbre")), names(tmp))
      orange_col <- na.omit(orange_col)
      if (length(orange_col) > 0) setColWidths(wb, sheet = sheet, cols = orange_col, widths = 5)
      
      # Essence column, CodeEcolo & coordinates
      species_col <- match(i18n()$t(c("Essence", "CodeEcolo", "CoordGPS1", "CoordGPS2")), names(tmp))
      species_col <- na.omit(species_col)
      if (length(species_col) > 0) setColWidths(wb, sheet = sheet, cols = species_col, widths = 10)
      
      # Observations columns
      observations_col <- match(i18n()$t("Observations"), names(tmp))
      observations_col <- na.omit(observations_col)
      if (length(observations_col) > 0) {
        if (sheet == i18n()$t("Arbres")) {
          setColWidths(wb, sheet = sheet, cols = observations_col, widths = 15)
        } else {
          setColWidths(wb, sheet = sheet, cols = observations_col, widths = 25)
          }
      }
      
      # new cell widths
      ## title
      setRowHeights(wb, sheet, rows = 1, heights = 80)
      ## general
      # print(dim(tmp)) # debug
      # row_end <- if (dim(tmp)[1] > 0) dim(tmp)[1] + 1 else 2
      row_end <- 10000
      setRowHeights(wb, sheet, rows = 2:row_end, heights = 16)
      
      # -- réduction en largeur des colonnes NumPlac et NumArbre
      # pos_Thin1 <- which(names(tmp) %in% c("NumPlac", "PlotNum"))
      # pos_Thin2 <- which(names(tmp) %in% c("NumArbre", "TreeNum"))
      # if (length(pos_Thin1) > 0) {
      #   setColWidths(wb, sheet = sheet, cols = pos_Thin1, widths = 2.83)
      # }
      # if (length(pos_Thin2) > 0) {
      #   setColWidths(wb, sheet = sheet, cols = pos_Thin2, widths = 3.5)
      # }
      
      # -- colonnes à cacher :
      columns_to_hide <- i18n()$t(c("NumDisp", "Cycle", "CodeEcoloAncien"))
      # particularité feuille Taillis
      if (sheet == i18n()$t("Taillis")) columns_to_hide <- c(columns_to_hide, i18n()$t(c("Azimut", "Distance")))
      pos_to_hide <- which(names(tmp) %in% columns_to_hide)
      if (length(pos_to_hide) > 0) {
        setColWidths(
          wb, sheet, cols = pos_to_hide, 
          hidden = rep(TRUE, length(pos_to_hide))
        )
      }
      
      # -- commande pour figer les volets
      freezePane(wb, sheet, firstActiveRow = 2)
    } # end of loop 1:length(table_list)
    ##### /\ #####
    
    
    ##### 4/ Sauvegarde et écriture du classeur #####
    dir.create("out", showWarnings = F)
      # -- répertoire de sauvegarde
      output_dir <- 
        paste0("out/remesures-2024/", disp_num, "-", disp_name, "/remesures/classeur")
      dir.create(output_dir, showWarnings = F, recursive = T)
      
      # -- chemin du fichier de sortie
      file_path <- file.path(output_dir, paste0(disp_num, "_remesure.xlsx")) 
      saveWorkbook(wb, file_path, overwrite = TRUE)
      
    # } else { # langue anglaise
    #   # -- répertoire de sauvegarde
    #   output_dir <- 
    #     paste0("out/", disp_num, "-", disp_name, "/remeasures/workbook")
    #   dir.create(output_dir, showWarnings = F, recursive = T)
    #   
    #   # -- chemin du fichier de sortie
    #   file_path <- file.path(output_dir, paste0(disp_num, "_remeasure.xlsx")) 
    #   saveWorkbook(wb, file_path, overwrite = TRUE)
    # }
    
    # -- MAJ de la barre de progression
    info <- round(match(disp, disp_list) / length(disp_list) * 100)
    # label
    pb_label <- paste0(
      i18n()$t("Edition des classeurs de remesures : "), info, 
      i18n()$t("% done - dispositif "), disp_name, 
      i18n()$t(" édité.")
      )
    # title
    pb_title <- paste0(
        i18n()$t("Edition ("), info, i18n()$t(" \u0025)")
      )
    info <- round(match(disp, disp_list) / length(disp_list) * 100)
    setTkProgressBar(pb, info, pb_title, pb_label)
  ##### /\ #####
  } # end of loop 'disp in disp_list'
  
  # -- close barre de progression
  close(pb)
  
  # -- message de fin
  msg <- tk_messageBox(
    type = "ok", 
    message = ifelse(
      lang == "FRA", 
      "Ecriture du/des classeur(s) de remesure termin\u00E9e", 
      "Writing of re-measurement folder ended"
    ), 
    icon = "info"
  )
}

