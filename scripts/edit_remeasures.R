##### script d'import des dispositifs à remesurer : choix de l'année (onglets du classeur excel EDT_AFI.xlsx)
rm(list = ls())

# répertoire administrateur :
repAFI <- "/Users/Valentin/Travail/Outils/GitHub/PermAFI-ShinyApp"
setwd(repAFI)
repSav <- repAFI


# ----- Librairies nécessaires à l'utilisation de PermAFI
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
# library(PermAFI)

# depo = "inst"
depo = "scripts"


# - chargement du script
source(file.path("scripts/afi_ClasseurRem.R"), encoding = 'UTF-8', echo = TRUE)
source(file.path("scripts/afi_EditFichesRem.R"), encoding = 'UTF-8', echo = TRUE)
source(file.path("scripts/afi_EditPlanArbres.R"), encoding = 'UTF-8', echo = TRUE)
# chargement du script annexes
source(file.path(file.path(depo, "annexes.R")), encoding = 'UTF-8', echo = TRUE)
# chargement du script job 2 (fonction layout_wb nécessaire à afi_ClasseurRem
source(file.path(depo, "afi_XlsTranslation.R"), encoding = 'UTF-8', echo = TRUE)



# translator <- shiny.i18n::Translator$new(translation_json_path = file.path("/Users/Valentin/Travail/Outils/GitHub/PermAFI2", "translations/translation_test.json"))
translator <- shiny.i18n::Translator$new(translation_json_path = file.path(repAFI, "www/translations/translation.json"))
# translator$set_translation_language("English")
# -- i18n reactive function
i18n <- function() {
  translator
}

# file path
wb_path <- file.path(repAFI, "data/excel/admin/EDT AFI.xlsx")

# get years list from sheet names
years_list <- getSheetNames(wb_path)

# choose year
year <- tk_select.list(
  choices = years_list, 
  preselect = years_list[length(years_list)],
  multiple = F, 
  title = "Choisir une année d'inventaire"
)

# read table
table <- readWorkbook(wb_path, sheet = year, startRow = 2)

# stand list
stand_num_list <- table$'N°.Disp.' %>% sort

# add name to stand_num
db <- new.env()
# db = global_env() # debug
load("tables/afiCodes.Rdata", envir = db)
stand_name_list <-
  with(db[["Dispositifs"]], Nom[match(stand_num_list, NumDisp)])
# - remove empty names (new stands)
pos <- which(is.na(stand_name_list))
if (length(pos) > 0) {
  stand_num_list <- stand_num_list[-pos]
  stand_name_list <- stand_name_list[-pos]
}

# file path list
files_list <- paste0(stand_num_list, "-", stand_name_list, ".xlsx")
# correction manuelle
# files_list <- files_list[!files_list %in% c(
#   "108-Forêt de Bicchisano.xlsx", "110-Forêt Communale de Lacoste.xlsx",
#   "120-Chasse Woods - Rushmore Estate.xlsx", "134-Llechwedd Dyrys.xlsx"
# )] # debug
# files_list <- c("120-Chasse Woods - Rushmore Estate.xlsx", "131-Allt Boeth.xlsx") # debug
# files_list <- "81-Forêt de la Sémoline.xlsx" # debug

# call afi_XlsTranslation function
files_list <- files_list[-c(17, 20, 22)] # données Corse (disp déjà remesuré), + celles des dispositifs ISN exclues (non disponibles)
files_list <- files_list[c(22)] # données des dispositifs ISN (non disponibles)
afi_XlsTranslation(wd = repAFI, files_list = file.path(repAFI,"data/excel/inventaires", files_list)) # files_list[c(17,20,21,22)]
# call afi_ClasseurRem function
afi_ClasseurRem(wd = repAFI, files_list = files_list, lang = "ENG")
# call afi_EditFichesRem function
afi_EditFichesRem(wd = repAFI, files_list = files_list, lang = "ENG")
# call afi_EditPlansArbres function
afi_EditPlansArbres(wd = repAFI, files_list = files_list, lang = "ENG")

