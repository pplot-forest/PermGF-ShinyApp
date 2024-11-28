# token <- readRDS("droptoken.rds")
# rdrop2::drop_acc(dtoken = token)

# add libraries
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   shiny, shinythemes, shinydashboard, shinyjs, shinyWidgets, shinyFiles, shiny.i18n#, tinytex
# )
# library(easypackages)
# suppressMessages(
# packages(
# "shiny", "shinythemes", "shinydashboard", "shinyjs", "shinyWidgets", "shinyFiles", "shiny.i18n",
# 
# "stringr", "openxlsx", "rmarkdown", "tools",
# "tidyr", "dplyr", "gWidgets2", "gWidgets2tcltk", "knitr", "maptools",
# "xtable", "ggplot2", "ggrepel", "ggthemes", "scales", "gridExtra",
# "rgeos", "rgdal", "gdata", "grid", "fmsb", "rlang"#, "tinytex"
# )
# install.packages(c(
#   "shiny", "shinythemes", "shinydashboard", "shinyjs", "shinyWidgets", "shinyFiles", "shiny.i18n",
#   
#   "stringr", "openxlsx", "rmarkdown", "tools",
#   "tidyr", "dplyr", 
#   # "gWidgets2", "gWidgets2tcltk", 
#   "knitr", "maptools",
#   "xtable", "ggplot2", "ggrepel", "ggthemes", "scales", "gridExtra",
#   "rgeos", "rgdal", "gdata", "grid", "fmsb", "rlang"
# ))

# library(rdrop2)
  library(shiny)
  library(shinythemes)
  library(shinydashboard)
  library(shinyjs)
  library(shinyWidgets)
  library(shinyFiles)
  library(shiny.i18n)
  library(stringr)
  library(openxlsx)
  library(rmarkdown)
  library(tools)
  library(tidyr)
  library(dplyr)
  # library(gWidgets2)
  # library(gWidgets2tcltk)
  library(knitr)
  library(maptools)
  library(xtable)
  library(ggplot2)
  library(ggrepel)
  library(ggthemes)
  library(scales)
  library(gridExtra)
  library(rgeos)
  library(rgdal)
  library(gdata)
  library(grid)
  library(fmsb)
  library(rlang)

source('scripts/annexes.R', echo = TRUE)
source('scripts/afi_XlsTranslation.R', echo=TRUE)
source('scripts/afi_CodesTranslation.R', echo=TRUE)
source('scripts/afi_Calculs.R', echo=TRUE)
source('scripts/afi_AgregArbres.R', echo=TRUE)
source('scripts/afi_AgregPlacettes.R', echo=TRUE)


# -- répertoire PermAFI
# TODO : à connecter au package PermAFI ?
# project_repo <- getwd()
# project_repo <- repAFI <- "/Users/Valentin/Travail/Outils/GitHub/PermAFI2"

mega <- 200 # ? utilité de l'option ?
options(shiny.maxRequestSize = mega * 1024^2)
options(encoding = "UTF-8")

# # i18n
# # rm(i18n)
# # i18n <- shiny.i18n::Translator$new(translation_json_path = system.file("translations/translation.json", package = "shinycnes"))
# i18n <- shiny.i18n::Translator$new(translation_json_path = file.path("/Users/Valentin/Travail/Outils/GitHub/PermAFI2", "translations/translation.json"))
i18n <- shiny.i18n::Translator$new(translation_json_path = "www/translations/translation.json")
# # i18n$set_translation_language("en")
i18n$set_translation_language("Français")
# i18n$set_translation_language("English")
# i18n$set_translation_language("Deutsch")
# # i18n$languages[1]
# i18n()$translation_language
# i18n$t("Vérification des données (rapport .pdf) :")
# i18n$t("Changer la langue")

# -- construction de la liste des dispositifs
# load(file = file.path( project_repo, "tables/afiCodes.Rdata"))
# load("~/Travail/Outils/GitHub/PermAFI2/AFI_UI-v1/tables/report_tables_list.Rdata")
# admin <- Dispositifs
# 
# # -- stand num list
# all_num_list <- sort( as.numeric( unique(admin$NumDisp) ) )
# 
# # -- stand label list
# if (is.element(NA, all_num_list)) warning("NumDisp vide d\u00E9tect\u00E9")
# all_disp_list <- paste0(
#   all_num_list, "-", admin$Nom[match(all_num_list, admin$NumDisp)]
# )

# -- source 'edit_carnet' script to get 'build_tables' function

