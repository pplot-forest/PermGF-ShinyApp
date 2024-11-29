# -- 1/ importer le classeur dictionnaire
wd <- "/Users/Valentin/Travail/Outils/GitHub/PermAFI-ShinyApp"
setwd(wd)
file <- file.path(wd, "data/excel/dictionary/afi_dictionary.xlsx")
library(easypackages)
# suppressMessages(
packages(
  "stringr", "openxlsx", "rmarkdown", "tools",
  "tidyr", "dplyr", "gWidgets2", "gWidgets2tcltk", "knitr", "maptools",
  "xtable", "ggplot2", "ggrepel", "ggthemes", "scales", "gridExtra",
  "rgeos", "rgdal", "gdata", "grid", "fmsb", "rlang",
  "readxl"
)
source(file.path("scripts/afi_Dictionary2RData.R"), encoding = 'UTF-8', echo = TRUE)
afi_Dictionary2Rdata(wd, file, trad = F)




# -- 2/ écrire le json à partir de l'archive dictionary
library(jsonlite)

# function to write json for traduction
# file <- "/Users/Valentin/Travail/Outils/GitHub/PermAFI2/translations/translation_work.xlsx"
# output_dir <- dirname(file)
# table <- read.xlsx(file) %>% mutate(Emplacement = NULL)

load(file.path(wd, "tables/afiDictionary.Rdata"))
dictionary <- 
  dictionary %>% 
  mutate(
    Emplacement = NULL,
    Feuille = NULL
  ) %>% 
  # pivot_longer(cols = everything()) %>% 
  # mutate(value = gsub('\\', '\g', value))
  # pivot_wider()
  rename("Français" = "Attribut_FRA", "English" = "Attribut_ENG", "Deutsch" = "Attribut_DEU") %>% 
  arrange(Français) %>% 
  distinct()

# table <- rbind(table, dictionary)
table <- dictionary

list <- list(
  languages = c("Français", "English", "Deutsch"),
  translation = table
)
output_dir <- file.path(wd, "www/translations")
jsonlite::write_json(list, path = file.path(output_dir, "translation.json"), na = "string")

# tk_messageBox(type = "ok", message = "Penser à remplacer manuellement les // par / dans le fichier .json")
# TODO : import de dictionary + réécriture.

# -- 3/ import du json écrit (test si doublons)
# translation_json_path = file.path("/Users/Valentin/Travail/Outils/GitHub/PermAFI2", "translations/translation.json")
# translation_json_path = file.path("/Users/Valentin/Travail/Outils/GitHub/PermAFI2", "translations/translation.json")
# json <- read_json(translation_json_path)
# # parse_json(translation_json_path)
# 
translator <- shiny.i18n::Translator$new(translation_json_path = file.path(wd, "www/translations/translation.json"))
