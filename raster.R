#install.packages('raster', repos="http://cran.rstudio.com/")
#install.packages('rgdal', repos="http://cran.rstudio.com/")
#install.packages('gdalUtils', repos="http://cran.rstudio.com/")
#install.packages("MODIS", repos="http://R-Forge.R-project.org",type="source")

library(knitr)
library(raster)
library(rgdal)
library(gdalUtils)
library(RCurl)

Sys.setenv(MRT_DATA_DIR = "/Users/jagonzalez/MRT/data")

MRTPath <- "/Users/jagonzalez/MRT/bin"      # Ruta de binarios del MRT
DATPath <- "/Users/jagonzalez/MRT/data/hdf" # Ruta donde se almacenan los archivos hdf
PARPath <- "/Users/jagonzalez/MRT/par"      # Ruta donde guardamos el archivo de parámetros

# Cambiamos al directorio de trabajo, donde se encuentran
# los archivos de datos de entrada ".hdf"
setwd(DATPath)

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

hdinfo <- gdalinfo(filePath, raw_output = TRUE)

northC <- hdinfo[grep("NORTHBOUNDINGCOORDINATE", hdinfo)]
northC <- gsub("NORTHBOUNDINGCOORDINATE=", "", northC)
northC <- gsub(" ", "", northC)

eastC  <- hdinfo[grep("EASTBOUNDINGCOORDINATE=", hdinfo)]
eastC  <- gsub("EASTBOUNDINGCOORDINATE=", "", eastC)
eastC  <- gsub(" ", "", eastC)

westC  <- hdinfo[grep("WESTBOUNDINGCOORDINATE=", hdinfo)]
westC  <- gsub("WESTBOUNDINGCOORDINATE=", "", westC)
westC  <- gsub(" ", "", westC)

southC  <- hdinfo[grep("SOUTHBOUNDINGCOORDINATE=", hdinfo)]
southC  <- gsub("SOUTHBOUNDINGCOORDINATE=", "", southC)
southC  <- gsub(" ", "", southC)

parFile[8] <- paste("SPATIAL_SUBSET_UL_CORNER = ( ", northC, westC, ")", sep = " ")
parFile[9] <- paste("SPATIAL_SUBSET_LR_CORNER = ( ", southC, eastC, ")", sep = " ")
parFile[10] <- ""
fileName <- substr(fileName, 1, nchar(i) - 4)
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
# Termina creación del archivo de parámetros inicial


# Creamos el comando para preparar la re-proyección:
# Se crea el script mrtbatch
comando1 <- paste("java -jar ", MRTPath, "/", "MRTBatch.jar -d ", DATPath, " -p ", parFileName, " -o ", PARPath, sep = "")
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
allParFiles <- paste(PARPath, "*.prm", sep = "/")
system(paste("dos2unix -c mac ", allParFiles, sep = ""))

# Comando para ejecutar el script mrtbatch
comando3 <- paste(curDir,"mrtbatch", sep="/")
system(comando3)

# Ahora creamos la estructura de archivos para procesar
# y calcular el respectivo CVI
# Los archivos .tif quedaron guardados en PARPath
TIFPath <- PARPath
setwd(TIFPath)
# Obteniendo nombres de archivos NDVI y EVI
ndvs <- list.files(TIFPath, pattern = "*_NDVI.tif")
evis <- list.files(TIFPath, pattern = "*_EVI.tif")

# Creando estructura para NDVI's
# Para cada archivo NDVI descargado
for(i in seq(1:length(ndvs)))
{
  # Obtenemos el nombre del archivo
  ndviDay <- paste("DOY_", substr(ndvs[i], 14, nchar(ndvs[i]) - 22), sep="")
  mvDir <- file.path(OUTPath, ndviDay)
  mvNam <- file.path(TIFPath, ndvs[i])
  # Si no se ha creado, creamos el directorio DOY_###
  dir.create(mvDir, showWarnings = FALSE)
  comando5 <- paste("cp", mvNam, mvDir, sep=" ")
  system(comando5)
}

# Creando estructura para EVI's
# Para cada archivo EVI descargado
for(i in seq(1:length(evis)))
{
  # Obtenemos el nombre del archivo
  eviDay <- paste("DOY_", substr(evis[i], 14, nchar(evis[i]) - 21), sep="")
  mvDir <- file.path(OUTPath, eviDay)
  mvNam <- file.path(TIFPath, evis[i])
  # Si no se ha creado, creamos el directorio DOY_###
  dir.create(mvDir, showWarnings = FALSE)
  comando5 <- paste("cp", mvNam, mvDir, sep=" ")
  system(comando5)
}