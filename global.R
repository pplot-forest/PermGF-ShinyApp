
# -- Install libraries
# install.packages("easypackages")
library(easypackages)
# install_packages(
libraries(
  "shiny", 
  "shinythemes",
  "shinydashboard",
  "shinyjs",
  "shinyWidgets",
  "shinyFiles",
  "shiny.i18n",
  "rmarkdown",
  "xtable",
  "rlang",
  "tools",
  "stringr",
  "openxlsx",
  "tidyr",
  "dplyr",
  "knitr",
  "ggplot2",
  "ggrepel",
  "ggthemes",
  "scales",
  "gridExtra",
  # "rgeos",
  # "rgdal",
  "gdata",
  "grid",
  "fmsb",
  
  "DT"
)
# -- Source codes
source('scripts/annexes.R', echo = TRUE)
source('scripts/gf1_Xls2Rdata.R', echo=TRUE)
source('scripts/gf_CodesTranslation.R', echo=TRUE)
source('scripts/gf3_Calculs.R', echo=TRUE)
source('scripts/gf4_AgregArbres.R', echo=TRUE)
source('scripts/gf5_AgregPlacettes.R', echo=TRUE)


# -- paramètre interface
mega <- 200 # ? utilité de l'option ?
options(shiny.maxRequestSize = mega * 1024^2)
options(encoding = "UTF-8")

# -- fonction de traduction
i18n <- shiny.i18n::Translator$new(translation_json_path = "www/translations/translation.json")
i18n$set_translation_language("Français")
# i18n$set_translation_language("English")
# i18n$set_translation_language("Deutsch")

