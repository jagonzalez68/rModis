#install.packages('raster', repos="http://cran.rstudio.com/")
#install.packages('rgdal', repos="http://cran.rstudio.com/")
#install.packages('gdalUtils', repos="http://cran.rstudio.com/")
#install.packages("MODIS", repos="http://R-Forge.R-project.org",type="source")

library(raster)
library(RCurl)
library(rgdal)
library(gdalUtils)
library(MODIS)

createParFile <- function(DATPath, PARPath, northC="",  westC="", southC="", eastC="")
{
  # Obtenemos en filePath la ruta a un archivo hdf para usar en 
  # el archivo de parámetros
  files <- list.files(pattern="*.hdf")
  
  fileName <- files[1]
  filePath <- paste(DATPath, fileName, sep = "/")
  
  # Creación del archivo de parámetros inicial
  # Si queremos trabajar con un recorte de la imagen original cambiamos:
  #     - SPATIAL_SUBSET_UL_CORNER
  #     - SPATIAL_SUBSET_LR_CORNER
  # a manera que el bounding box contenga nuestro recorte
  parFile <- vector(mode = "character")
  parFile[1] <- ""
  parFile[2] <- paste("INPUT_FILENAME = ", filePath, sep = "")
  parFile[3] <- ""
  parFile[4] <- "SPECTRAL_SUBSET = ( 1 1 0 0 0 0 0 0 0 0 0 0 )"
  parFile[5] <- ""
  parFile[6] <- "SPATIAL_SUBSET_TYPE = INPUT_LAT_LONG"
  parFile[7] <- ""
  
  # En hdinfo obtenemos los metadatos del archivo .hdf
  # de aquí exraemos las coordenadas del bounding box
  # de las imágenes en el .hdf
  hdinfo <- gdalinfo(filePath, raw_output = TRUE)
  
  # Coordenada Norte
  if(northC=="")
  {
    northC <- hdinfo[grep("NORTHBOUNDINGCOORDINATE", hdinfo)]
    northC <- gsub("NORTHBOUNDINGCOORDINATE=", "", northC)
    northC <- gsub(" ", "", northC)
    
    # Coordenada Este
    eastC  <- hdinfo[grep("EASTBOUNDINGCOORDINATE=", hdinfo)]
    eastC  <- gsub("EASTBOUNDINGCOORDINATE=", "", eastC)
    eastC  <- gsub(" ", "", eastC)
    # Coordenada Oeste
    westC  <- hdinfo[grep("WESTBOUNDINGCOORDINATE=", hdinfo)]
    westC  <- gsub("WESTBOUNDINGCOORDINATE=", "", westC)
    westC  <- gsub(" ", "", westC)
    # Coordenada Sur
    southC  <- hdinfo[grep("SOUTHBOUNDINGCOORDINATE=", hdinfo)]
    southC  <- gsub("SOUTHBOUNDINGCOORDINATE=", "", southC)
    southC  <- gsub(" ", "", southC)
  }
  
  parFile[8] <- paste("SPATIAL_SUBSET_UL_CORNER = ( ", northC, westC, ")", sep = " ")
  parFile[9] <- paste("SPATIAL_SUBSET_LR_CORNER = ( ", southC, eastC, ")", sep = " ")
  parFile[10] <- ""
  fileName <- substr(fileName, 1, nchar(fileName) - 4)
  fileName <- paste(fileName, "tif", sep=".")
  parFile[11] <- paste("OUTPUT_FILENAME = ", fileName, sep = "" )
  parFile[12] <- ""
  parFile[13] <- "RESAMPLING_TYPE = NEAREST_NEIGHBOR"
  parFile[14] <- ""
  parFile[15] <- "OUTPUT_PROJECTION_TYPE = GEO"
  parFile[16] <- ""
  parFile[17] <- "OUTPUT_PROJECTION_PARAMETERS = ("
  parFile[18] <- " 0.0 0.0 0.0"
  parFile[19] <- " 0.0 0.0 0.0"
  parFile[20] <- " 0.0 0.0 0.0"
  
  parFile[21] <- " 0.0 0.0 0.0"
  parFile[22] <- " 0.0 0.0 0.0 )"
  parFile[23] <- ""
  parFile[24] <- "DATUM = WGS84"
  
  # Almacenamiento del Archivo de parámetros
  parFileName <- paste(PARPath, "parameter.par", sep = "/")
  fileC <- file(parFileName, 'w')
  for(i in seq(1:length(parFile)))
  {
    cat(parFile[i], "\n", file = fileC, sep="")
  }
  close(fileC)
  return(parFileName)
}



reproyectarImagenes <- function(parFileName, MRTPath, DATPath, PARPath, OUTPath)
{
  # Creamos el comando para preparar la re-proyección:
  # Se crea el script mrtbatch
  comando1 <- paste("java -jar ", MRTPath, "/", "MRTBatch.jar -d ", DATPath, " -p ", parFileName, " -o ", OUTPath, sep = "")
  system(comando1)
  
  # Comando para agregar permisos de ejecución al
  # script mrtbatch
  curDir <- getwd()
  batchName <- paste(curDir,"mrtbatch", sep="/")
  comando2 <- paste("chmod 755 ", batchName, sep="")
  system(comando2)
  
  # Comando para convertir el archivo de parámetros .par 
  # de formato mac antiguo a formato unix
  system(paste("dos2unix -c mac ", batchName, sep=""))
  
  # Comando para convertir los archivos de parámetros .prm
  # de formato mac antiguo a formato unix
  allParFiles <- paste(OUTPath, "*.prm", sep = "/")
  system(paste("dos2unix -c mac ", allParFiles, sep = ""))
  
  # Comando para ejecutar el script mrtbatch
  comando3 <- paste(curDir,"mrtbatch", sep="/")
  system(comando3)
}

renameFiles <- function(DATPath, OUTPath)
{
  # Ahora creamos la estructura de archivos para procesar
  # y calcular el respectivo CVI
  # Los archivos .tif quedaron guardados en PARPath
  TIFPath <- OUTPath
  setwd(TIFPath)

  # Obteniendo nombres de archivos NDVI y EVI
  ndvs <- list.files(TIFPath, pattern = "*_NDVI.tif")
  evis <- list.files(TIFPath, pattern = "*_EVI.tif")
  
  # Creando estructura para NDVI's
  # Para cada archivo NDVI descargado
  ndviOUTPath <- paste(OUTPath, "NDVI", sep="/")
  system(paste("mkdir ", ndviOUTPath, sep=""))
  for(i in seq(1:length(ndvs)))
  {
    # Obtenemos el nombre del archivo
    ndviDay <- paste("DOY_", substr(ndvs[i], 14, nchar(ndvs[i]) - 22), sep="")
    nSuf <- substr(ndvs[i], 31, nchar(ndvs[i]) - 8)
    nPref <- substr(ndvs[i], 1, nchar(ndvs[i]) - 31)
    nDay <- substr(ndvs[i], 14, nchar(ndvs[i]) - 22)
    nYear <- substr(ndvs[i], 10, nchar(ndvs[i]) - 25)
    mvDir <- file.path(ndviOUTPath, ndviDay)
    mvNam <- file.path(TIFPath, ndvs[i])
    # Si no se ha creado, creamos el directorio DOY_###
    dir.create(mvDir, showWarnings = FALSE)
    mvNam2 <- paste(nDay, "_", nYear, "_", nPref, nSuf, "_NDVI.tif", sep="")
    mvNam2 <- file.path(TIFPath, mvNam2)
    print(mvNam2)
    comando4 <- paste("mv", mvNam, mvNam2, sep=" ")
    system(comando4)
    comando5 <- paste("mv", mvNam2, mvDir, sep=" ")
    system(comando5)
  }
  
  # Creando estructura para EVI's
  # Para cada archivo EVI descargado
  eviOUTPath <- paste(OUTPath, "EVI", sep="/")
  system(paste("mkdir ", eviOUTPath, sep=""))
  for(i in seq(1:length(evis)))
  {
    # Obtenemos el nombre del archivo
    eviDay <- paste("DOY_", substr(evis[i], 14, nchar(evis[i]) - 21), sep="")
    eSuf <- substr(evis[i], 31, nchar(evis[i]) - 7)
    ePref <- substr(evis[i], 1, nchar(evis[i]) - 30)
    eDay <- substr(evis[i], 14, nchar(evis[i]) - 21)
    eYear <- substr(evis[i], 10, nchar(evis[i]) - 24)
    mvDir <- file.path(eviOUTPath, eviDay)
    mvNam <- file.path(TIFPath, evis[i])
    # Si no se ha creado, creamos el directorio DOY_###
    dir.create(mvDir, showWarnings = FALSE)
    mvNam2 <- paste(eDay, "_", eYear, "_", ePref, eSuf, "_EVI.tif", sep="")
    mvNam2 <- file.path(TIFPath, mvNam2)
    print(mvNam2)
    comando4 <- paste("mv", mvNam, mvNam2, sep=" ")
    system(comando4)
    comando5 <- paste("mv", mvNam2, mvDir, sep=" ")
    system(comando5)
  }
}


cleanEnv <- function(OUTPath, PARPath)
{
  system(paste("rm -R ", OUTPath, "/NDVI", sep=""))
  system(paste("rm -R ", OUTPath, "/EVI", sep=""))
  system(paste("rm ", OUTPath, "/*.tif", sep=""))
  system(paste("rm ", OUTPath, "/*.prm", sep=""))
  system(paste("rm ", PARPath, "/parameter.par", sep=""))
}


#                           PROGRAMA PRINCIPAL

# INICIA PARTE A MODIFICAR DEL CÓDIGO para cambiar el ambiente de trabajo
# Variable de ambiente para trabajar con MRT
Sys.setenv(MRT_DATA_DIR = "/Users/jagonzalez/MRT/data")
Sys.setenv(MRT_HOME = "/Users/jagonzalez/MRT")
Sys.setenv(PATH = "/usr/bin/:/bin:/usr/sbin:/usr/local/bin:/Users/jagonzalez/MRT/bin")
# Ruta de binarios del MRT
MRTPath <- "/Users/jagonzalez/MRT/bin"
# Ruta donde se almacenan los archivos hdf
DATPath <- "/Users/jagonzalez/Documents/R/Geospatial/seco/data/hdf"
# Ruta donde guardamos el archivo de parámetros
PARPath <- "/Users/jagonzalez/Documents/R/Geospatial/seco/par"
# Ruta de archivos de salida del script
OUTPath <- "/Users/jagonzalez/Documents/R/Geospatial/seco/out"
# Ruta de mi código fuente
SRCPath <- "/Users/jagonzalez/Documents/R/Geospatial"

# En caso de ser requerido, se pasan las coordenadas del
# boundingbox del área a trabajar
# UL: -105.61,8.86
# LR: -84.67,5.66
northC = ""
westC = ""
southC = ""
eastC = ""
# TERMINA PARTE A MODIFICAR DEL CÓDIGO

# Cambiamos al directorio de trabajo, donde se encuentran
# los archivos de datos de entrada ".hdf"
# inicialmente, aquí descargamos los archivos ".hdf"
cleanEnv(OUTPath, PARPath)
setwd(DATPath)
parFileName <- createParFile(DATPath,PARPath, northC, westC, southC, eastC)
reproyectarImagenes(parFileName, MRTPath, DATPath, PARPath, OUTPath)
renameFiles(DATPath, OUTPath)