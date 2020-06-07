########################TEIL 2##############################################
###
###Schritte: 
###  1. Verzeichnisliste einlesen (dirs)
###  2. in jedem Verzeichnis in den Geotif Ordner wechseln (dort, wo die Produkte als komplette große Szenen liegen)
###  3. jeweilige Szene in Raster konvertieren
###  4. Check ob Untersuchungsgebiet in Szene liegt 
###    ja   ---> Untersuchungsgebiet ausschneiden / geotiff/netcdf erstellen
###   nein  ---> Text ausgeben und mit nächsten Untersuchungsgebiet testen
###  5. Ergebnis: insgesamt 12 neue Unterordner Sortiert nach Untersuchungsgebiet und netcdf bzw. geotiff
############################################################################
library(raster)
library(rgdal)
library(sp)
library(ncdf4)
library(tools)
library(rgeos)

workDir = "E:/Analysis/COKAP/MODIS/01_download/"

#Verzeichnis in dem die 12 Produktordner liegen:
setwd(workDir)

#Verzeichnisliste speichern
dirs <- dir()[file.info(dir())$isdir]

#########Shapefiles einlesen#############

##### Fuer alle shapefiles: ####

Thuringia <- readOGR("E:/Analysis/COKAP/MODIS/thuringia_boundary_utm32_wgs84_envelope.shp")
#Thuringia <- readOGR("E:/Analysis/COKAP/MODIS/thuringia_boundary_utm32_wgs84_envelope.shp")
#AGC <- readOGR("F:/Harry/Shapefiles/AGC_5x5km_buffer_5km_wgs84.shp")

#shapes <- as.list.data.frame(c(VWN,AGC,MLP,SKZ,MDB,BBR))
shapes <- as.list.data.frame(c(Thuringia))

#shapes_n <- as.list.data.frame(c("VWN","AGC","MLP","SKZ","MDB","BBR"))
shapes_n <- as.list.data.frame(c("Thuringia"))

for (i in 1:length(dirs)) {
  
  #in den Geotiff Ordner der jeweiligen Variable wechseln
  setwd(paste0(workDir,dirs[i],"/GeoTIFF"))
  stand <- sprintf("Verzeichnis: %s", dirs[i])
  print(stand)
  
  #Liste aller .tif Dateien im Verzeichnis speichern
  files <- list.files(pattern = "tif$")
  #######  Subsetting als netcdf / geotiff ######
  
  for (a in 1:length(files)) {
    ## Volle Szene rastern
    bigscene <- raster(files[a])
    ##dazu ein polygon mit Extent erstellen
    big_extent <- extent(bigscene)
    big_e_sp <- as(big_extent, 'SpatialPolygons')
    crs(big_e_sp) <- crs(bigscene)
    datei <- sprintf("Verzeichnis: %s Datei: %s von %s", dirs[i],a,length(files))
    print(datei)
    
    for (x in 1:length(shapes)) {
      shape <- as(extent(bbox(shapes[[x]])), 'SpatialPolygons')
      crs(shape) <- crs(bigscene)
      ## Nur die Shapes aus Bigscene ausschneiden, die auch überlappen!! ##
      if (gIntersects(shapes[[x]],big_e_sp)) {
        #dir.create(paste0(getwd(),("/"),shapes_n[x]," ","netcdf"))
        dir.create(paste0(getwd(),("/"),shapes_n[x]))
        outtif <- paste0(getwd(),("/"),shapes_n[x],"/",file_path_sans_ext(files[a]),"_",shapes_n[x],".tif")
        #outcdf <- paste0(getwd(),("/"),shapes_n[x]," ","netcdf/",file_path_sans_ext(files[a]),".nc")
        writeRaster(crop(bigscene,shape), filename=outtif, format="GTiff", overwrite=TRUE)
        #writeRaster(crop(bigscene,shape), filename=outcdf, format="CDF", overwrite=TRUE)
      }
      else {
        print("Does not overlap!")
      }
    } 
  }
}