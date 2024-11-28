#' Edition des fiches de remesure
#' @description La fonction permet d'éditer les fiches de remesure pour le prochain passage en inventaire (format .pdf)

#' @author Valentin Demets
#'
#' @param wd = répertoire contenant les données
#' @param lang = langue (sélectionnée) de l'interface
#'
#' @import tcltk
#' @import knitr
#' @import stringr
#'
#' @export

afi_EditFichesRem <- function(
  wd = NULL, 
  files_list = NULL,
  lang = "FRA", 
  template = "afi_FichesRem_2021.Rnw"
) {
  ##### 1/ Initialisation #####
  # -- chargement des données
  # chemins relatifs des archives
  arch1 <- file.path(wd, "tables/afiDonneesBrutes.Rdata")
  arch2 <- file.path(wd, "tables/afiCodes.Rdata")
  arch3 <- file.path(wd, "tables/afiDictionary.Rdata")
  
  # création d'un nouvel environnement et chargement
  db = new.env()
  # db = global_env()
  load(arch1, db)
  load(arch2, db)
  load(arch3, db)
  
  # sanity checks
  # indb <- ensures(all(. %in% names(db)))
  # indb(c(load(arch1), load(arch2)))
  
  # -- choix du dispositif
  if (is.null(files_list)) {
    # initialisation
    check_all_msg <- i18n()$t("Editer les fiches terrain pour tous les dispositifs")
    df_list <- load(arch1)
    disp_list <- 
      choose_disp(df_list, get("Dispositifs", envir = db), check_all_msg) %>% 
      clean_names()
  } else disp_list <- basename( file_path_sans_ext(files_list) ) %>% clean_names()
  
  # --  création de la barre de progression
  disp_num <- str_sub(disp_list[1], str_locate(disp_list[1], "-")[, 2] + 1, -1)
  pb_title <- "Progression"
  pb_label <- paste0(
      i18n()$t("Edition des fiches de remesure : 0% done - dispositif "), 
      disp_num, i18n()$t(" en cours.")
    )
  pb <- tkProgressBar(pb_title, pb_label, 0, 100, width = 800)
  ##### / \ #####
  
  
  ##### 2/ Edition des fiches de remesure #####
  for (disp in disp_list) {
    # disp <- disp_list[1] # debug
    with(db, {
      # -- gestion des noms et num du dispositif
      disp_num <- as.numeric(str_sub(disp, 1, str_locate(disp, "-")[, 1] - 1)) #changement2
      disp_name <- with(Dispositifs, Nom[match(disp_num, NumDisp)])
      
      # -- arguments relatifs au dispositifs
      last_cycle <- 
        with(Cycles, max(Cycle[NumDisp == disp_num], na.rm = T))
      ending_year <-
        with(Cycles, Annee[NumDisp == disp_num & Cycle == last_cycle])
      
      if (length(ending_year) > 1) {
        stop("Correction du classeur administrateur nécessaire : il y a 2 années identiques renseignées dans la feuille Cycles")
      }
      
      # -- création du dossier de sortie
      # if (lang == "FRA") {
        output_dir <- file.path("out/remesures-2024/", disp, "remesures/fiches_remesure/")
      # } else {
      #   output_dir <- file.path("out", disp, "remeasures/field_sheets/")
      # }
      dir.create(output_dir, showWarnings = F, recursive = T)
      
      # -- définition des arguments nécessaires au knit
      repPdf <- file.path(wd, output_dir)
      
      # -- superassignements
      # nom de la sortie en .tex
      output <<- 
        # if (lang == "FRA") {
          paste0(repPdf, disp_num, "_fiche_terrain_", ending_year, ".tex")
        # } else {
        #   paste0(repPdf, disp_num, "_field_sheet_", ending_year, ".tex")
        # }
    })
    print(disp)
    ##### 3/ Edition de la/des fiche(s) de remesure (1 template) #####
    # print(output) # debug
    out = knit2pdf(
      input = file.path("template", template), 
      output = output, 
      compiler = "pdflatex", 
      quiet = TRUE,
      envir = db
    )
    clean_after_knit(output)
    
    # -- MAJ de la barre de progression
    info <- round(match(disp, disp_list) / length(disp_list) * 100)
    pb_label <- paste0(
      # if (lang == "FRA") {
      i18n()$t("Edition des fiches de remesure : "), 
      info, i18n()$t("\u0025 done - dispositif "), disp_num, 
      i18n()$t(" \u00E9dit\u00E9.")
    )
    # } else {
    #   paste0(
    #     "Edit of re-measurement form : ",
    #     info, "\u0025 done - dispositif ", disp_num, " edited."
    #   )
    # }
    pb_title <- paste0( #if (lang == "FRA") {
      i18n()$t("Edition ("), info, i18n()$t(" \u0025)")
    )
    # } else {
    #   paste0("Editing (",info," \u0025)")
    # }
    setTkProgressBar(pb, info, pb_title, pb_label)
  }
  
  # -- close barre de progression
  close(pb)
  ##### / \ #####
  
  
  # -- message de fin
  end_msg <- paste0( #if (lang == "FRA") {
    i18n()$t("Edition(s) des fiches de remesure termin\u00E9e(s)")
  )
  # } else {
  #   "Edit of re-measurement form(s) ended"
  # }
  msg <-  tk_messageBox(
    type = "ok", 
    message = end_msg, 
    icon = "info"
  )
}
