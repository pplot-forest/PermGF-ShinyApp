# répertoire de travail
wd <- "/Users/Valentin/Travail/Outils/GitHub/PermAFI-ShinyApp"
setwd(wd)

# library
library(easypackages)
# suppressMessages(
packages(
  "stringr", "openxlsx", "rmarkdown", "tools",
  "tidyr", "dplyr", "gWidgets2", "gWidgets2tcltk", "knitr", "maptools",
  "xtable", "ggplot2", "ggrepel", "ggthemes", "scales", "gridExtra",
  "rgeos", "rgdal", "gdata", "grid", "fmsb", "rlang", "sf", "pgirmess"
)

# source annexe.R
source(file.path(wd, "scripts", "annexes.R"), encoding = 'UTF-8', echo = TRUE)

# chargement des données en archive
load(file.path(wd, "tables/afiDonneesBrutes.Rdata"))
load(file.path(wd, "tables/afiCodes.Rdata"))


# création table de correspondance "N° EPSG <-> label EPSG"
correlation_table <- tibble(
  EPSG = c(rep(2154, 2), 32632, 32631, 5130), 
  SystGPS = c("LAMBERT 93", "L93", "32T", "31T", "30T")
)

# table
table <- Coords %>% 
  filter(!is.na(SystGPS) & !is.na(CoordGPS1) & !is.na(CoordGPS2)) %>% 
  left_join(correlation_table, by = "SystGPS", relationship = "many-to-one") %>% 
  select(NumDisp, NumPlac, SystGPS, CoordGPS1, CoordGPS2, EPSG)

# security test
missing_EPSG_table <- filter(table, is.na(EPSG))
if ( nrow(missing_EPSG_table) > 0 ) {
  missing_EPSG_stand_num <- unique(missing_EPSG_table$NumDisp)
  stop("SystGPS non reconnu dans la table Coords pour le(s) dispositif(s) ", paste0(missing_EPSG_stand_num, collapse = ", "))
}

# convert table to sf - création du shape
# -- obligé de distinguer les EPSG
epsg_list <- unique(table$EPSG)

# empty sf
sf <- tibble(NumDisp = c(), NumPlac = c(), SystGPS = c(), CoordGPS1 = c(), CoordGPS2 = c(), EPSG = c())
# loop for every epsg
for (epsg in epsg_list) { # loop 'i in epsg_list'
  tmp <- table %>% 
    filter(EPSG == epsg) %>% 
    st_as_sf(coords = c("CoordGPS1", "CoordGPS2"), crs = epsg) %>% 
    # convert to WGS84
    st_transform(crs = 4326)
  
  # stack data
  sf <- rbind(sf, tmp)
}


# write files
disp_num_list <- unique(sf$NumDisp)
for(disp_num in disp_num_list) { # loop 'disp in disp_list'
  # disp name
  disp_name <- with(Dispositifs, Nom[which(NumDisp == disp_num)]) %>% clean_names()
  
  # format waypoint
  waypoint <- sf %>% 
    filter(NumDisp == disp_num) %>%
    rename(name = NumPlac) %>% 
    mutate(
      ele = NA,
      time = NA,
      magvar = NA,
      geoidheight = NA
    ) %>% 
    # ID = Name, NAME = Name) %>% 
    select(ele, time, magvar, geoidheight, name)
  
  # -- répertoire de sauvegarde
  output_dir <- 
    file.path("out/remesures-2024", paste0(disp_num, "-", disp_name), "remesures/gpx")
  dir.create(output_dir, recursive = T, showWarnings = F)
  
  # file name
  file_name = paste0(output_dir, "/", disp_num, "-waypoint.gpx")
  
  # write GPX
  st_write(
    waypoint, 
    dsn = file_name, 
    driver = "gpx", 
    dataset_options = "GPX_USE_EXTENSIONS=yes", 
    delete_dsn = TRUE
  )
} # end of loop 'disp in disp_list'
