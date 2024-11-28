##### photothèque AFI : conversion de JPEG en PNG #####
library("jpeg")
library("png")
library("tools")
library("tcltk")

files_list <- 
  tk_choose.files(multi = T)
cat(paste0(files_list, collapse = "'', \n''"))
  
# files_list <- c(
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150823.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150824.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150825.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150826.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150827.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150828.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150829.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150830.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150831.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150832.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150833.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150834.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150835.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150836.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150837.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150838.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150839.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150840.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150841.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150842.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150843.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150844.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150845.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150846.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150847.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150848.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150849.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150850.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150854.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150856.JPG", 
#   "/Users/Valentin/pCloud Drive/Base AFI/Administrateur/PhotothequeAFI/122 Bois de l'Ardere/P1150865.JPG"
# )

# -- save repo
rep_save <- tk_choose.dir()
# rep_save <- "/Users/Valentin/Travail/Outils/GitHub/PermAFI2/data/images/photos/122"
# dir.create(rep_save, showWarnings = F, recursive = T)

for (file in files_list) {
  # file <- files_list[1] # debug
  img <- readJPEG(file)
  # img <- readPNG(file)
  writeJPEG( # writeJPEG
    img, 
    target = file.path(
      rep_save, 
      paste0(file_path_sans_ext(basename(file)), ".jpg")
    ),
    quality = 0.2
  )
  
  # ggsave(path = path, width = width, height = height, device='tiff', dpi=700)
}
