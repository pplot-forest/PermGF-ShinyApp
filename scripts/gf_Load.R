rm(list = ls())

# répertoire administrateur :
repGF <- "/Users/Valentin/Travail/Outils/GitHub/PermGF-ShinyApp"
setwd(repGF)
repSav <- repGF


# ----- Librairies nécessaires à l'utilisation de PermGF
# Cas particulier : première utilisation -installation du package "easypackages"
# install.packages("easypackages")

# Installation/Activation des packages nécessaires
library(easypackages)
# suppressMessages(
  packages(
    "stringr", "openxlsx", "rmarkdown", "tools",
    "tidyr", "dplyr", "gWidgets2", "gWidgets2tcltk", "knitr", "maptools",
    "xtable", "ggplot2", "ggrepel", "ggthemes", "scales", "gridExtra",
    "rgeos", "rgdal", "gdata", "grid", "fmsb", "rlang"
  )
# )
# library(PermGF)

  # depo = "inst"
  depo = "scripts"
  
  # chargement du script annexes
  source(file.path(file.path(depo, "annexes.R")), encoding = 'UTF-8', echo = TRUE)
  # chargement du script job 1
  source(file.path(depo, "gf_CodesTranslation.R"), encoding = 'UTF-8', echo = TRUE)
  # chargement du script job 2
  # source(file.path("scripts/gf_XlsTranslation.R"), encoding = 'UTF-8', echo = TRUE)
  # chargement du script job 2
  source(file.path(depo, "gf_XlsTranslation.R"), encoding = 'UTF-8', echo = TRUE)
  
  # # chargement du script job 3
  source(file.path("scripts/gf_Verif.R"), encoding = 'UTF-8', echo = TRUE)
  # chargement du script job 4
  source(file.path(depo, "gf_Calculs.R"), encoding = 'UTF-8', echo = TRUE)
  # chargement du script job 5
  source(file.path(depo, "gf_AgregArbres.R"), encoding = 'UTF-8', echo = TRUE)
  # chargement du script job 6
  source(file.path(depo, "gf_AgregPlacettes.R"), encoding = 'UTF-8', echo = TRUE)
  # # chargement du script job 7
  # source(file.path("scripts/gf_EditCarnet.R"), encoding = 'UTF-8', echo = TRUE)
  
  
  
  
  

##### ----- AFI data processing - MAJ PermGF2----- #####
  
  # arguments shiny
  rv <- list()
  db <- new.env()
  # db = global_env() # debug
  load("tables/gfCodes.Rdata", envir = db)
  with(db, {
    rv <- list(
      repGF = "/Users/Valentin/Travail/Outils/GitHub/PermGF-ShinyApp",
      # lang = "Deutsch", # choisir la langue du fichier d'import
      # lang = "Français",
      lang = "English",
      disp_num = 131
    )
    # -- gestion des noms et num du dispositif
    # TODO : faire le tri des éléments à rendre vraiment réactifs
    # rv$disp_num <- as.numeric(str_sub(rv$disp, 1, str_locate(rv$disp, "-")[, 1] - 1))
    # print(paste0("repGF = ", repGF)) # debug
    rv$disp_name <-
      with(db[["Dispositifs"]], Nom[match(rv$disp_num, NumDisp)])
    rv$disp <- paste0(rv$disp_num, "-", clean_names(rv$disp_name))
    # browser()
    # -- arguments relatifs au dispositif
    rv$last_cycle <-
      with(db[["Cycles"]], max(Cycle[NumDisp == rv$disp_num], na.rm = T))
    rv$last_year <-
      with(db[["Cycles"]], Annee[NumDisp == rv$disp_num & Cycle == rv$last_cycle])
    
    if (length(rv$last_year) > 1) {
      stop("Correction du classeur administrateur nécessaire : il y a 2 années identiques renseignées dans la feuille Cycles")
    }
    
    # -- création du dossier de sortie
    rv$output_dir <- file.path("out", clean_names(rv$disp), "livret_AFI")
    
    # -- définition des arguments nécessaires au knit
    rv$rep_pdf <- file.path(repGF, rv$output_dir)
    rv$rep_logos <- file.path(repGF, "data/images/logos/")
    rv$rep_figures <- file.path(rv$rep_pdf, "figures/")
    rv$repSav <- dirname(rv$rep_pdf)
    
    # chemin du template (absolute path)
    # rv$template_path <- file.path(repGF, "template/gf_Livret_2020_shiny_work.Rnw")
    rv$template_path <- file.path(repGF, "template/appendice_carbon/appendice_carbon.Rnw")
    # nom de la sortie en .tex
    rv$output_filename <- paste0(rv$disp_num, "_livret-AFI_", rv$last_year, ".tex")
    # rv$output_filename <- paste0(rv$disp_num, "_annexe_carbone_AFI_", rv$last_year, ".tex")
    
    rv <<- rv
  })
  complete_progress = 185 # barre de progression
  
  # translator <- shiny.i18n::Translator$new(translation_json_path = file.path("/Users/Valentin/Travail/Outils/GitHub/PermGF2", "translations/translation_test.json"))
  translator <- shiny.i18n::Translator$new(translation_json_path = file.path(repGF, "www/translations/translation.json"))
  translator$set_translation_language(rv$lang)
  # -- i18n reactive function
  i18n <- function() {
    translator
  }
  # N.B : attention barres de progression shiny 
  # -> chercher dans les scripts '# -- switch shiny ***' ou '# *** --'
  
  

##### Job 1 : import des données administrateurs #####
# argument(s) d'entrée
# files_list <- tk_choose.files(multi = T) # debug
  files_list <- c(
    "data/excel/admin/AFI_Admin.xlsx", # _prelevt
    "data/excel/admin/AFI_Codifications.xlsx", # TODO : put Codification on pcloud (+ PUvar, DensiteBoisMort, ...)
    "data/excel/admin/AFI_Economie_220831.xlsx"
  )
  files_list <- file.path(repGF, files_list)

# lancement
# Attention : il faut sourcer le script gf_XlsTranslation.R pour avoir la fonction read_xlsx
gf_CodesTranslation(wd = repGF, files_list = files_list, i18n = i18n)
###### /\ #####

##### Job 2 : import des données d'inventaire #####
# argument(s) d'entrée
# files_list <- tk_choose.files(multi = T)
files_list <- "/Users/Valentin/Travail/Outils/GitHub/PermGF2/out/147-Landeswald_Lubben/translation/147-Landeswald_Lubben_FRA.xlsx"
files_list <- file.path("data/excel/inventaires", c(
  # # 2023
  # "110-Forêt Communale de Lacoste.xlsx",
  # "162-Bois de Berrieux.xlsx",
  # "1-Bois des Brosses.xlsx", 
  "2-Bois du Chanois.xlsx"#,
  # "3-Forêt de Chamberceau.xlsx", 
  # "5-Forêt de Gergy.xlsx"#,
  # "6-Forêt de la Quiquengrogne.xlsx", 
  # "20-Forêt de Perrecy les Forges.xlsx", 
  # "38-Forêt de Rai.xlsx", 
  # "49-Forêt de la Métairie rouge.xlsx", 
  # "56-Bois de Chanteloube.xlsx", 
  # "81-Forêt de la Sémoline.xlsx", 
  # "82-Forêt de Fontréal.xlsx", 
  # "83-Forêt de Saint Lager.xlsx", 
  # "98-La Clavière.xlsx", 
  # "110-Forêt Communale de Lacoste.xlsx", 
  # "111-Forêt Domaniale de Versoix.xlsx", 
  # "112-Les Cravives.xlsx", 
  # "113-Bois du Crêt Lescuyer.xlsx", 
  # "120-Chasse_Woods_Rushmore_Estate.xlsx",
  # "124-Forêt Communale de Niozelles.xlsx", 
  # "125-La Tuilière.xlsx", 
  # "126-Plan de Liman.xlsx", 
  # "129-Forêt du Nivot.xlsx", 
  # "131-Allt Boeth.xlsx"#, 
  # "132-Forêt du Prieuré d'Ardène.xlsx", 
  # "133-Bois de Luthenay.xlsx"
))
  
  
  
  
  # 2022
  # "16-Forêt de Folin.xlsx",
  # "37-Bois de Frilouze.xlsx",
  # "41-Forêt de la Rivière.xlsx",
  # # "66-Bois du Beau Mousseau.xlsx",
  # "67-Stourhead.xlsx",
  # "67-Stourhead.xlsx",
  # # "68-Forêt Domaniale de Wiltz-Merkholtz.xlsx",
  # # "69-Forêt Communale de Wiltz.xlsx",
  # "70-Forêt Communale de Bettborn.xlsx",
  # # "74-Forêt Domaniale du Grand Bois.xlsx",
  # "75-Forêt Communale de Rouvroy.xlsx",
  # "86-Forêt de Rivedieu.xlsx",
  # "91-Forêt de Londeix.xlsx",
  # "94-Cranborne.xlsx",
  # "109-Forêt de Cardine.xlsx",
  # "95-Bois des Mauves.xlsx",
  # "97-Forêt Domaniale d'Andelfingen.xlsx",
  # # "99-Forêt Domaniale de Jussy.xlsx",
  # # # "99-Forêt Domaniale de Jussy-prevision_coupe.xlsx"
  # "100-Forêt d'Authumes.xlsx",
  # "101-Kommunalwald Hallau.xlsx",
  # "102-Forest of Monivea.xlsx",
  # "103-Forest of Mellory.xlsx",
  # "104-Forest of Killsheelan.xlsx",
  # "105-Forest of Lisdowney.xlsx",
  # "106-Forest of Knockrath.xlsx",
  # "107-Forest of Rahin.xlsx",
  # # "108-Forêt de Bicchisano.xlsx",
  # "109-Forêt de Cardine.xlsx",
  # "110-Forêt Communale de Lacoste.xlsx",
  # "123-Berth Ddu.xlsx",
  # "127-Forêt de Mollberg.xlsx",
  # "128-Forêt Communale de Lalaye.xlsx",
  # "129-Forêt du Nivot.xlsx",
  # "130-Forêt de Notre Dame des Neiges.xlsx",
  # "131-Allt Boeth.xlsx",
  # "156-Bois Thoureau.xlsx",
  
  # "17-Bois Royal de Belval.xlsx",
  # "128-Forêt Communale de Lalaye.xlsx",
  # "157-Foret_Domaniale_de_Lure.xlsx",
  # "158-Domaine_du_Peyrourier.xlsx",  
  # "159-Domaine_de_camp_Jusiou.xlsx", 
  # "160-Montmayon.xlsx",
  # 
  # "103-Forest of Mellory.xlsx"
  # "161-Forêt de Lencouacq.xlsx"
# ))

# lancement
# gf_XlsTranslation(wd = repGF, files_list, i18n = i18n) # traitement courant

# import de toute la base AFI - sélectionner les fichiers avec 
# fenêtre de dialogue puis appeler fonction d'import en précisant 
# trad = T en argument d'entrée
# -- N.B  : pour afficher la liste des fichiers (save dans un objet)
  #=> call = 'cat(paste0( str_remove(files_list, paste0(repGF, "/")), collapse = "', \n'"))'
# files_list <- c(
#   "data/excel/inventaires/1-Bois des Brosses.xlsx", 
#   "data/excel/inventaires/2-Bois du Chanois.xlsx", 
#   "data/excel/inventaires/3-Forêt de Chamberceau.xlsx", 
#   "data/excel/inventaires/4-Bois des Etangs d'Aige et du Prince.xlsx", 
#   "data/excel/inventaires/5-Forêt de Gergy.xlsx", 
#   "data/excel/inventaires/6-Forêt de la Quiquengrogne.xlsx", 
#   "data/excel/inventaires/7-Bois de Censey.xlsx", 
#   "data/excel/inventaires/9-Bois de la Rente du Fretoy.xlsx", 
#   "data/excel/inventaires/10-Bois Banal.xlsx", 
#   "data/excel/inventaires/11-Bois de Cosges.xlsx", 
#   "data/excel/inventaires/13-Bois de la Pérouse.xlsx", 
#   "data/excel/inventaires/14-Bois des Feuillées.xlsx", 
#   "data/excel/inventaires/15-Bois du Château.xlsx", 
#   "data/excel/inventaires/16-Forêt de Folin.xlsx", 
#   "data/excel/inventaires/17-Bois Royal de Belval.xlsx", 
#   "data/excel/inventaires/18-Forêt d'Epernay.xlsx", 
#   "data/excel/inventaires/19-Forêt de la Grange Perrey.xlsx", 
#   "data/excel/inventaires/20-Forêt de Perrecy les Forges.xlsx", 
#   "data/excel/inventaires/21-Les Grands Bois.xlsx", 
#   "data/excel/inventaires/23-Bois du Grand Lomont.xlsx", 
#   "data/excel/inventaires/24-Forêt du Grand Vernet.xlsx", 
#   "data/excel/inventaires/25-Forêt de la Brisée.xlsx", 
#   "data/excel/inventaires/26-Les Grands Bois.xlsx", 
#   "data/excel/inventaires/27-Bois du Luth.xlsx", 
#   "data/excel/inventaires/30-Forêt du Hailly.xlsx", 
#   "data/excel/inventaires/32-Bois du Pré Jeanreau.xlsx", 
#   "data/excel/inventaires/33-Forêt d'Is sur Tille.xlsx", 
#   "data/excel/inventaires/34-Forêt de Robert-Magny.xlsx", 
#   "data/excel/inventaires/36-Bois de Brice.xlsx", 
#   "data/excel/inventaires/37-Bois de Frilouze.xlsx", 
#   "data/excel/inventaires/38-Forêt de Rai.xlsx", 
#   "data/excel/inventaires/40-Bois de la Barre.xlsx", 
#   "data/excel/inventaires/41-Forêt de la Rivière.xlsx", 
#   "data/excel/inventaires/42-Forêt de Montalibord.xlsx", 
#   "data/excel/inventaires/43-Forêt d'Ombrée.xlsx", 
#   "data/excel/inventaires/44-Forêt du Régnaval.xlsx", 
#   "data/excel/inventaires/45-Bois de Belle Assise.xlsx", 
#   "data/excel/inventaires/46-Forêt de la Chevreté.xlsx", 
#   "data/excel/inventaires/47-Bois de Jebsheim.xlsx", 
#   "data/excel/inventaires/48-Forêt de Landsberg.xlsx", 
#   "data/excel/inventaires/49-Forêt de la Métairie rouge.xlsx", 
#   "data/excel/inventaires/50-Forêt de Montesault.xlsx", 
#   "data/excel/inventaires/51-Bois des Soriots.xlsx", 
#   "data/excel/inventaires/52-Bois de la Cayère.xlsx", 
#   "data/excel/inventaires/53-Forêt de la Chaine.xlsx", 
#   "data/excel/inventaires/54-Forêt de Marchenoir.xlsx", 
#   "data/excel/inventaires/55-Forêt de Montmirail.xlsx", 
#   "data/excel/inventaires/56-Bois de Chanteloube.xlsx", 
#   "data/excel/inventaires/57-La Touche aux Loups.xlsx", 
#   "data/excel/inventaires/58-Forêt Domaniale de Saint Gobain.xlsx", 
#   "data/excel/inventaires/59-Forêt de la Queue de Boué.xlsx", 
#   "data/excel/inventaires/61-La Forêt.xlsx", 
#   "data/excel/inventaires/62-Bois du Faussé.xlsx", 
#   "data/excel/inventaires/63-Bois de la Forêt.xlsx", 
#   "data/excel/inventaires/64-Forêt de la Marsaudière.xlsx", 
#   "data/excel/inventaires/65-Bois de Paris.xlsx", 
#   "data/excel/inventaires/66-Bois du Beau Mousseau.xlsx", 
#   "data/excel/inventaires/68-Forêt Domaniale de Wiltz-Merkholtz.xlsx", 
#   "data/excel/inventaires/69-Forêt Communale de Wiltz.xlsx", 
#   "data/excel/inventaires/70-Forêt Communale de Bettborn.xlsx", 
#   "data/excel/inventaires/71-Forêt de Metendal.xlsx", 
#   "data/excel/inventaires/72-Forêt de la Montroche.xlsx", 
#   "data/excel/inventaires/74-Forêt Domaniale du Grand Bois.xlsx", 
#   "data/excel/inventaires/75-Forêt Communale de Rouvroy.xlsx", 
#   "data/excel/inventaires/76-Forêt de la SOMICAL (1).xlsx", 
#   "data/excel/inventaires/78-Forêt Communale d'Igney.xlsx", 
#   "data/excel/inventaires/79-Forêt Domaniale de Mouterhouse.xlsx", 
#   "data/excel/inventaires/80-Le Hohenfels.xlsx", 
#   "data/excel/inventaires/81-Forêt de la Sémoline.xlsx", 
#   "data/excel/inventaires/82-Forêt de Fontréal.xlsx", 
#   "data/excel/inventaires/83-Forêt de Saint Lager.xlsx", 
#   "data/excel/inventaires/84-Forêt d'Algères.xlsx", 
#   "data/excel/inventaires/85-Forêt de la SOMICAL (2).xlsx", 
#   "data/excel/inventaires/86-Forêt de Rivedieu.xlsx", 
#   "data/excel/inventaires/87-Domaine de Rochemure.xlsx", 
#   "data/excel/inventaires/88-Bois de la Côte.xlsx", 
#   "data/excel/inventaires/89-Forêt Domaniale des Chambons.xlsx", 
#   "data/excel/inventaires/90-Bois du Château de Dufau.xlsx", 
#   "data/excel/inventaires/91-Forêt de Londeix.xlsx", 
#   "data/excel/inventaires/92-Forêt de la SOMICAL (3).xlsx", 
#   "data/excel/inventaires/93-Bois de Barnal.xlsx", 
#   "data/excel/inventaires/95-Bois des Mauves.xlsx", 
#   "data/excel/inventaires/96-Bois de la Vancre.xlsx", 
#   "data/excel/inventaires/97-Forêt Domaniale d'Andelfingen.xlsx", 
#   "data/excel/inventaires/98-La Clavière.xlsx", 
#   "data/excel/inventaires/99-Forêt Domaniale de Jussy.xlsx", 
#   "data/excel/inventaires/100-Forêt d'Authumes.xlsx", 
#   "data/excel/inventaires/101-Kommunalwald Hallau.xlsx", 
#   "data/excel/inventaires/102-Forest of Monivea.xlsx", 
#   "data/excel/inventaires/103-Forest of Mellory.xlsx", 
#   "data/excel/inventaires/104-Forest of Killsheelan.xlsx", 
#   "data/excel/inventaires/105-Forest of Lisdowney.xlsx", 
#   "data/excel/inventaires/106-Forest of Knockrath.xlsx", 
#   "data/excel/inventaires/107-Forest of Rahin.xlsx", 
#   "data/excel/inventaires/109-Forêt de Cardine.xlsx", 
#   "data/excel/inventaires/110-Forêt Communale de Lacoste.xlsx", 
#   "data/excel/inventaires/111-Forêt Domaniale de Versoix.xlsx", 
#   "data/excel/inventaires/112-Les Cravives.xlsx", 
#   "data/excel/inventaires/113-Bois du Crêt Lescuyer.xlsx", 
#   "data/excel/inventaires/114-Forêt Indivise de Rabat les Trois Seigneurs.xlsx", 
#   "data/excel/inventaires/115-Forêt Communale de Boussenac.xlsx", 
#   "data/excel/inventaires/116-Forêt communale de Rimont.xlsx", 
#   "data/excel/inventaires/117-Bois du Bousquet.xlsx", 
#   "data/excel/inventaires/118-Forêt Communale de Grenchen.xlsx", 
#   "data/excel/inventaires/119-Foret de la Fabrie.xlsx", 
#   "data/excel/inventaires/122-Bois de l'Ardère.xlsx", 
#   "data/excel/inventaires/123-Berth Ddu.xlsx", 
#   "data/excel/inventaires/124-Forêt Communale de Niozelles.xlsx", 
#   "data/excel/inventaires/125-La Tuilière.xlsx", 
#   "data/excel/inventaires/126-Plan de Liman.xlsx", 
#   "data/excel/inventaires/127-Forêt de Mollberg.xlsx", 
#   "data/excel/inventaires/128-Forêt Communale de Lalaye.xlsx", 
#   "data/excel/inventaires/129-Forêt du Nivot.xlsx", 
#   "data/excel/inventaires/130-Forêt de Notre Dame des Neiges.xlsx", 
#   "data/excel/inventaires/131-Allt Boeth.xlsx", 
#   "data/excel/inventaires/132-Forêt du Prieuré d'Ardène.xlsx", 
#   "data/excel/inventaires/133-Bois de Luthenay.xlsx", 
#   "data/excel/inventaires/135-Forêt Communale de Zurich.xlsx", 
#   "data/excel/inventaires/137-Forêt des Puechs.xlsx", 
#   "data/excel/inventaires/138-Forêt du Lévezou.xlsx", 
#   "data/excel/inventaires/139-Gruber Forst.xlsx", 
#   "data/excel/inventaires/140-Forêt Domaniale de Knechtsteden 1.xlsx", 
#   "data/excel/inventaires/141-Forêt Domaniale de Knechtsteden 2.xlsx", 
#   "data/excel/inventaires/142 Hohenhaus 1.xlsx", 
#   "data/excel/inventaires/143 Hohenhaus 2.xlsx", 
#   "data/excel/inventaires/144-Les_Saint_Peyres.xlsx", 
#   "data/excel/inventaires/146-Landeswald Chorin.xlsx", 
#   "data/excel/inventaires/147-Landeswald Lübben.xlsx", 
#   "data/excel/inventaires/148-Bois de la Fayolle.xlsx", 
#   "data/excel/inventaires/149-Bois de Cressu.xlsx", 
#   "data/excel/inventaires/150-Forêt de Saint Yvoce.xlsx", 
#   "data/excel/inventaires/151-Forêt de Champlalot.xlsx", 
#   "data/excel/inventaires/152-Lauenburg Brunsmark.xlsx", 
#   "data/excel/inventaires/153-Lauenburg Hundebusch.xlsx", 
#   "data/excel/inventaires/155-Schiessberg Sommerseite.xlsx", 
#   "data/excel/inventaires/156-Bois Thoureau.xlsx", 
#   "data/excel/inventaires/161-Forêt de Lencouacq.xlsx", 
#   "data/excel/inventaires/162-Bois de Berrieux.xlsx"
# )
gf_XlsTranslation(wd = repGF, files_list, i18n = i18n)

#
# ##### Vérifications de la BD AFI globale - projet document technique #####
# # -- chargement des données
# load("tables/gfDonneesBrutes.Rdata")
# load("tables/gfCodes.Rdata")
# 
# # -- vérifications des stades de BM
# codes_stades <- expand.grid(c(1:4), c(1:5)) %>% mutate(Stade = paste0(Var1, Var2))
# # transects au sol
# df <- 
#   BMortLineaires %>% 
#   filter(!Stade %in% codes_stades$Stade)
# write.xlsx(df, file = "unknown_stade_BMortLineaire.xlsx")
# # billons > 30 au sol
# df <- 
#   BMortSup30 %>% 
#   filter(!Stade %in% codes_stades$Stade)
# write.xlsx(df, file = "unknown_stade_BMortSup30.xlsx")
# # BMP
# df <- 
#   IdArbres %>% 
#   left_join(ValArbres, by = "IdArbre") %>% 
#   filter(!is.na(Type) & !Stade %in% codes_stades$Stade)
# write.xlsx(df, file = "unknown_stade_BMP.xlsx")
#   
# # Réécriture de la base AFI sous la forme d'1 classeur Excel pour corrections
# # arguments :
# # disp_list <- cf script gf_Calculs
# disp_2_edit <- disp_list
# 
# gf_rewrite_disp(
#   repGF, disp_2_edit, to_LANG, dir_LANG,
#   output_dir = file.path(repGF, "tables")
# )
# ##### /\ #####

##### Job 3 : vérification des données #####
# lancement
gf_Verif(repGF)
##### /\ #####

##### Job 4 : calcul des résultats par arbre #####
# argument(s) d'entrée : repGF, repSav, ... (-> default)
# lancement
# gf_Calculs(repGF) # ancienne version - sans shiny

# call function
gf_Calculs(
  wd = rv$repGF, 
  # output_dir = rv$repSav,
  output_dir = rv$repGF,
  # disp = rv$disp,
  disp = NULL,
  # last_cycle = rv$last_cycle,
  last_cycle = 6,
  complete_progress = complete_progress,
  i18n = i18n
)
# # AG AFI 2021 - changer code gf_Calculs en amont
# gf_Calculs(
#   wd = rv$repGF,
#   output_dir = rv$repGF
# )


# -- vérifications des qualités
load("tables/gfTablesBrutes.Rdata")
codes_qual <- Qual$Nom

df <- 
  Arbres %>% 
  filter(!Qual %in% codes_qual & is.na(Limite))
write.xlsx(df, file = "unknown_qual.xlsx")
##### /\ #####

##### Job 5 : agrégation des résultats par placettes #####
##### tables nécessaires pour l'édition du livret AFI #####
# setup
tables_list <- c(
  "gfDispFpied_Qual2", "gfDispBM_", "gfDispBMP_", "gfDispBMS_", 
  "gfDispBM_Essence", "gfDispBM_EssenceClasse", 
  "gfDispBMP_CodeEcolo", "gfDispBMP_EssenceCodeEcolo", "gfDispBMP_ClasseCodeEcolo", "gfDispBMP_CatCodeEcolo",
  "gfDispBM_StadeD", "gfDispBM_StadeDStadeE", "gfDispBM_StadeE", 
  "gfDispBMP_Classe", "gfDispBMP_ClasseType", "gfDispBMS_Classe",
  "gfDispBMP_EssenceCat", "gfDispBMS_EssenceCat",
  "gfDispBMP_Essence", "gfDispBMS_Essence",
  "gfDispBMP_Cat", "gfDispBMS_Cat",
  "gfDispBMS_ClasseStadeD", "gfDispBMP_ClasseStadeD", 
  "gfDispBMS_ClasseStadeE", "gfDispBMP_ClasseStadeE", 
  "gfDispCodes_", "gfDispCodes_Cat", "gfDispCodes_CodeEcolo", "gfDispCodes_CatCodeEcolo", 
  "gfDispFpied_", "gfDispFpied_Essence", "gfDispFpied_", 
  "gfDispFpied_Cat", "gfDispFpied_Classe", "gfDispFpied_ClasseQual", 
  "gfDispFpied_Essence", "gfDispFpied_EssenceCat", "gfPlaFpied_EssReg", 
  
  "gfDispFpied_EssReg", "gfDispFpied_EssRegClasse", 
  "gfDispFpied_ClasseQual1", "gfDispFpied_ClasseQual1", 
  "gfDispFpied_Qual1", "gfDispFpied_EssenceQual1", "gfDispFpied_EssRegQual1", 
  "gfDispFpied_CatQual1", "gfDispPer_Qual1", "gfPlaPer_EssRegClasseQual1", 
  "gfDispPer_EssRegClasse", "gfDispPer_ClasseQual1", "gfDispTaillis_EssRegClasse", 
  "gfDispFpied_EssRegCat", "gfDispFpied_ClasseQual1", 
  "gfDispFpied_EssenceCat", "gfDispFpied_EssenceClasseQual1", "gfDispFpied_Cat", 
  "gfDispFpied_CatCodeEcolo", "gfDispBM_EssRegClasse", 
  
  "gfDispFpied_EssenceClasse", "gfDispFpied_EssenceCoupe", 
  "gfDispFpied_CatQual1", "gfDispPer_EssenceClasse", 
  "gfPlaPer_EssenceClasseQual1", "gfDispFpied_ClasseCodeEcolo", 
  "gfDispFpied_ClasseQual1CodeEcolo", "gfDispCodes_EssenceCodeEcolo", 
  "gfDispFpied_CatCoupe", "gfDispFpied_Coupe", "gfDispFpied_cat", 
  
  "gfDispRege_Rejet", "gfDispRege_EssenceRejet", 
  
  # "gfDispRege_EssenceValideRejet", "gfDispRege_ValideRejet", 
  
  "gfDispPer_EssenceQual2", "gfDispPer_Qual2", "gfDispFpied_Qual2", 
  "gfDispFpied_Classe", "gfDispFpied_EssenceClasse", 
  "gfDispBM_Classe", 
  
  "gfDispFpied_CodeEcolo", "gfDispFpied_CodeEcolo", 
  
  "gfDispFpied_CatQual2CodeEcolo", 
  "gfDispFpied_Qual2CodeEcolo", "gfDispFpied_EssenceCodeEcolo", 
  "gfDispFpied_EssenceCodeEcolo", 
  "gfDispFpied_CatCoupe", "gfDispFpied_Coupe", 
  
  "gfDispFpied_EssRegCoupe", "gfDispFpied_CatCoupe", "gfDispFpied_Qual2Coupe", 
  
  "gfDispFpied_EssenceCatCoupe", "gfDispFpied_CatQual2Coupe", # pour PF
  
  "gfDispFpied_ClasseCodeEcolo", "gfDispFpied_CatQual2CodeEcolo", 
  "gfDispFpied_Qual2CodeEcolo", "gfDispFpied_EssenceCodeEcolo", 
  "gfDispFpied_EssenceCodeEcolo", "gfDispFpied_CatCodeEcolo", 
  
  # "gfDispFpied_Couvert", 
  
  # "gfDispFpied_EssRegParCat", 
  "gfDispPer_", "gfDispPer_Essence", "gfDispPer_EssReg", "gfDispPer_Classe", 
  # "gfDispHabitatBM_", "gfDispHabitatBM_StadeD", "gfDispHabitatBMP_", 
  # "gfDispHabitatBMS_", "gfDispHabitatFpied_", 
  # "gfDispHabitatFpied_Classe", "gfDispHabitatTaillis_", 
  # "gfDispHabitatTaillis_Classe", 
  "gfDispRege_Essence", 
  # "gfDispRege_EssRegPar", 
  "gfDispTaillis_", "gfDispTaillis_Classe", 
  "gfDispTaillis_Essence", "gfDispTaillis_EssenceClasse", 
  # "gfDispTot_", "gfDispTot_Cat", 
  # "gfDispTot_CatCodeEcolo", "gfDispTot_Essence", 
  # "gfDispTot_EssenceClasse", 
  # "gfDispTot_EssRegParCat", 
  "gfPlaFpied_", "gfPlaFpied_CatQual", 
  "gfPlaFpied_Cat", "gfPlaTaillis_Cat", "gfPlaTaillis_", 
  
  "gfDispPFutaie_", "gfDispPFutaie_Essence", "gfDispPFutaie_Classe", "gfDispPFutaie_Cat", 
  "gfDispExploit_", "gfDispExploit_Essence", "gfDispExploit_Classe", "gfDispExploit_Cat", 
  "gfDispExploit_EssenceCatQual2", 
  
  # "gfPlaTot_", "gfPlaTot_EssReg", "gfPlaTot_Cat", 
  "gfPlaBM_", "gfPlaRege_",
  
  
  
  "gfDispCarbone_", "gfDispCarbone_Essence", "gfDispCarbone_Cat", "gfDispCarbone_Qual1",
  "gfDispCarbone_Lifetime", "gfDispCarbone_EssenceLifetime", "gfDispCarbone_CatLifetime", "gfDispCarbone_Qual1Lifetime"
) %>% unique()
save(tables_list, file = "tables/report_tables_list.Rdata")
##### /\ #####
# ----- Lancement manuel -----
load("tables/report_tables_list.Rdata")
results_by_plot_to_get <- build_combination_table(tables_list)

# lancement # TODO : supprimer le message de jonction lors de l'exécution de gf_AgregArbres
# gf_AgregArbres(repGF, combination_table) # ancienne version - sans shiny

# call function
gf_AgregArbres(
  wd = rv$repGF,
  output_dir = rv$repSav,
  combination_table = results_by_plot_to_get,
  disp = rv$disp,
  last_cycle = rv$last_cycle,
  complete_progress = complete_progress,
  i18n = i18n
)
# AG AFI 2021 - changer code gf_Calculs en amont
gf_AgregArbres(
  wd = rv$repGF,
  output_dir = rv$repGF,
  last_cycle = 6,
  combination_table = results_by_plot_to_get
)
##### /\ #####

##### Job 6 : agrégation des résultats par dispositif #####
# argument(s) d'entrée
results_by_stand_to_get <- data.frame(
  V1 = "Disp",
  V2 = NA,
  stringsAsFactors = F
)

# lancement
# gf_AgregPlacettes(repGF, results_by_stand_to_get) # ancienne version - sans shiny

# call function
gf_AgregPlacettes(
  wd = rv$repGF,
  output_dir = rv$repSav,
  combination_table = results_by_stand_to_get,
  disp = rv$disp, last_cycle = rv$last_cycle,
  complete_progress = complete_progress,
  i18n = i18n
)
# AG AFI 2021 - changer code gf_Calculs en amont
gf_AgregPlacettes(
  wd = rv$repGF,
  output_dir = rv$repGF,
  combination_table = results_by_stand_to_get
)
##### /\ #####

# WARNING : le job7 ne vas jusqu'au bout (réglages à faire sur le nouveau modèle de livret -> à voir avec le prochain dispositif traité
###### Job 7 : édition du livret d'analyse #####
# argument(s) d'entrée
continue = T # permet de lancer l'édition du livret juste après l'import des données (appel des jobs 4, 5 et 6 incorporé)

# lancement TODO : mettre une possibilité pour passer les jobs 4, 5 et 6 si archive déjà présente ?
# gf_EditCarnet(repGF, continue = T) # ancienne version - sans shiny

# load( file.path(rv$repSav, "tables/gfTablesBrutes.Rdata") )
# load( file.path(rv$repSav, "tables/gfTablesElaboreesPlac.Rdata") )
# for(i in 1:length(results_by_plot)) {assign(names(results_by_plot)[i], results_by_plot[[i]])}
# load( file.path(rv$repSav, "/tables/gfTablesElaborees.Rdata") )
# for(i in 1:length(results_by_group)) {assign(names(results_by_group)[i], results_by_group[[i]])}

load("tables/report_tables_list.Rdata")
results_by_plot_to_get <- build_combination_table(tables_list)
results_by_stand_to_get <- data.frame(
  V1 = "Disp",
  V2 = NA,
  stringsAsFactors = F
)

disp_list <- c(131)
for (disp in disp_list) {
  # disp <- disp_list[1] # debug
  # arguments shiny
  rv <- list()
  db <- new.env()
  # db = global_env() # debug
  load("tables/gfCodes.Rdata", envir = db)
  with(db, {
    rv <- list(
      repGF = "/Users/Valentin/Travail/Outils/GitHub/PermGF-ShinyApp",
      # lang = "Deutsch", # choisir la langue du fichier d'import
      # lang = "Français",
      lang = "English",
      # lang = "Deutsch",
      disp_num = disp
    )
    # -- gestion des noms et num du dispositif
    # TODO : faire le tri des éléments à rendre vraiment réactifs
    # rv$disp_num <- as.numeric(str_sub(rv$disp, 1, str_locate(rv$disp, "-")[, 1] - 1))
    # print(paste0("repGF = ", repGF)) # debug
    rv$disp_name <-
      with(db[["Dispositifs"]], Nom[match(rv$disp_num, NumDisp)])
    rv$disp <- paste0(rv$disp_num, "-", clean_names(rv$disp_name))
    # browser()
    # -- arguments relatifs au dispositif
    rv$last_cycle <-
      with(db[["Cycles"]], max(Cycle[NumDisp == rv$disp_num], na.rm = T))
    rv$last_year <-
      with(db[["Cycles"]], Annee[NumDisp == rv$disp_num & Cycle == rv$last_cycle])
    
    if (length(rv$last_year) > 1) {
      stop("Correction du classeur administrateur nécessaire : il y a 2 années identiques renseignées dans la feuille Cycles")
    }
    
    # -- création du dossier de sortie
    rv$output_dir <- file.path("out", clean_names(rv$disp), "livret_AFI")
    
    # -- définition des arguments nécessaires au knit
    rv$rep_pdf <- file.path(repGF, rv$output_dir)
    rv$rep_logos <- file.path(repGF, "data/images/logos/")
    rv$rep_figures <- file.path(rv$rep_pdf, "figures/")
    rv$repSav <- dirname(rv$rep_pdf)
    
    # chemin du template (absolute path)
    rv$template_path <- file.path(repGF, "template/gf_Livret_2020_shiny_work.Rnw")
    # rv$template_path <- file.path(repGF, "template/appendice_carbon/appendice_carbon.Rnw")
    # nom de la sortie en .tex
    rv$output_filename <- paste0(rv$disp_num, "_livret-AFI_", rv$last_year, ".tex")
    # rv$output_filename <- paste0(rv$disp_num, "_annexe_carbone_AFI_", rv$last_year, ".tex")
    
    rv <<- rv
  })
  # translator <- shiny.i18n::Translator$new(translation_json_path = file.path("/Users/Valentin/Travail/Outils/GitHub/PermGF2", "translations/translation_test.json"))
  translator <- shiny.i18n::Translator$new(translation_json_path = file.path("/Users/Valentin/Travail/Outils/GitHub/PermGF2", "translations/translation.json"))
  translator$set_translation_language(rv$lang)
  # -- i18n reactive function
  i18n <- function() {
    translator
  }
  
  ##### Job 4 : calcul des résultats par arbre #####
  gf_Calculs(
    wd = rv$repGF,
    output_dir = rv$repSav,
    # output_dir = rv$repGF,
    disp = rv$disp,
    # disp = NULL,
    last_cycle = rv$last_cycle,
    # last_cycle = NULL,
    complete_progress = complete_progress,
    i18n = i18n
  )

  ##### Job 5 : agrégation des résultats par placettes #####
  gf_AgregArbres(
    wd = rv$repGF,
    output_dir = rv$repSav,
    combination_table = results_by_plot_to_get,
    disp = rv$disp,
    last_cycle = rv$last_cycle,
    complete_progress = complete_progress,
    i18n = i18n
  )

  ##### Job 6 : agrégation des résultats par dispositif #####
  gf_AgregPlacettes(
    wd = rv$repGF,
    output_dir = rv$repSav,
    combination_table = results_by_stand_to_get,
    disp = rv$disp, last_cycle = rv$last_cycle,
    complete_progress = complete_progress,
    i18n = i18n
  )

  ##### Job 7 : édition du livret d'analyse #####
  # TODO : filtrer les tables (avec "filter_by_disp" ?)
  wd <- function() {rv$repGF} # define wd() pour lancement manuel
  # rv$lang <- "Deutsch"
  dir.create(rv$rep_pdf, showWarnings = F, recursive = T)
  # out = knit2pdf(
  #   input = rv$template_path,
  #   output = file.path(rv$rep_pdf, rv$output_filename), # lancement gf_Load
  #   # output = rv$output_filename, # lancement shiny
  #   # envir = db,
  #   clean = TRUE
  # )
  out = knit2pdf(
    input = rv$template_path,
    output = file.path(rv$rep_pdf, rv$output_filename), # lancement gf_Load
    # output = rv$output_filename, # lancement shiny
    # envir = db,
    clean = TRUE
  )
}
# file.rename(out, file)
# TODO : pour assurer la continuité entre les versions, archiver des données brutes et des résultats et faire un test à partir de ces données, en piochant au hasard des comparaisons
##### /\ #####

##### Job 8 : édition du classeur de résultats #####
# chargement du script
source(file.path(depo, "gf_Tables2Xls.R"), encoding = 'UTF-8', echo = TRUE)

# lancement
gf_Tables2Xls(repGF, lang = "FRA")
##### /\ #####

###### Job 9 : édition des résultats au format SIG #####
# chargement du script
source(file.path(depo, "gf_ShapesPlac.R"), encoding = 'UTF-8', echo = TRUE)

# lancement
gf_ShapesPlac(repGF)
##### /\ #####

##### Job 10 : édition du classeur de remesure #####
# chargement du script
source(file.path("scripts/gf_ClasseurRem.R"), encoding = 'UTF-8', echo = TRUE)

# lancement
# translator <- shiny.i18n::Translator$new(translation_json_path = file.path("/Users/Valentin/Travail/Outils/GitHub/PermGF2", "translations/translation_test.json"))
translator <- shiny.i18n::Translator$new(translation_json_path = file.path("/Users/Valentin/Travail/Outils/GitHub/PermGF2", "translations/translation.json"))
# translator$set_translation_language("English")
# -- i18n reactive function
i18n <- function() {
  translator
}
# TODO: supprimer paramètre lang et utiliser le paramètre de translator
gf_ClasseurRem(wd = repGF, files_list = files_list, lang = "FRA")
##### /\ #####

##### Job 11 : édition des fiches de remesure #####
# chargement du script
source(file.path("scripts/gf_EditFichesRem.R"), encoding = 'UTF-8', echo = TRUE)
# -> à utiliser comme modèle pour MAJ de vérif, edition des plans par placettes, édition des livrets d'analyse

# lancement
gf_EditFichesRem(wd = repGF, files_list = files_list, lang = "FRA")
##### /\ #####

##### Job 12 : édition des plans des arbres par placettes #####
# chargement du script
source(file.path("scripts/gf_EditPlanArbres.R"), encoding = 'UTF-8', echo = TRUE)
# -> à utiliser comme modèle pour MAJ de vérif, edition des plans par placettes, édition des livrets d'analyse

# lancement
gf_EditPlansArbres(wd = repGF, files_list = files_list, lang = "FRA")
##### /\ #####


##### Job écriture du dictionnaire #####
# chargement du script
source(file.path(depo, "gf_Dictionary2RData.R"), encoding = 'UTF-8', echo = TRUE)

# lancement
# entrée = fichier de dictionaire le plus à jour (feuille PUvar entre autres)
# file <- "/Users/Valentin/Travail/Outils/GitHub/PermGF2/data/excel/dictionary/Translation_Work_ENG_20190426.xlsx"
file <- "/Users/Valentin/Travail/Outils/GitHub/PermGF2/data/excel/dictionary/gf_dictionary.xlsx"
gf_Dictionary2Rdata(repGF, file, trad = T)









##### deploy app #####
library(rsconnect)
# deployApp()
