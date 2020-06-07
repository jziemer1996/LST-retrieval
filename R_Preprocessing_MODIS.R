# ------------------------------------------------------------------------------------------------ #
# Tool to preprocess downloaded MODIS data for LST analysis
# - 1st part: Convert (GeoTIFF) and reproject (LatLon or UTM) the downloaded MODIS files (hdf).
# - 2nd part: Subsetting the MODIS files to the extent of Thuringia.
# ------------------------------------------------------------------------------------------------ #
# Author: Sandra Bauer
# Modifided by: Marlin Mueller & Jonas Ziemer
# ------------------------------------------------------------------------------------------------ #

# Load necessary packages into R
library(gdalUtils)
library(raster)
library(sp)
library(rgdal)
library(ncdf4)
library(tools)
library(rgeos)

# require(MODIS)

# ------------------------------------SET UP ENVIRONMENT------------------------------------------ #

## still needs to copy all downloaded hdf files in your desired working folder because 'R_Download_MODIS.R'
## writes it to home directory as default because setting the desired download directory doesn't work yet!


## Jonas working directory
workDir = "F:/411"
## Marlin working directory
# workDir = ""

setwd(workDir)


# List for all hdf files
dirs <- dir()[file.info(dir())$isdir]

# ----------------------------CONVERSION INTO GEOTIFFS AND REPROJECTION---------------------------- #

for (i in 1:length(dirs)) {
  
  setwd(paste0(workDir,"/",dirs[i]))
  
  # List of all hdfs in the directory
  files <- list.files(pattern = "hdf$")
  dir.create("GeoTIFF",showWarnings = FALSE)
  
  # Layernames without raster types and date
  files0 <- substr(gsub(".hdf", "", files), 1, nchar(gsub(".hdf", "", files)) - 14)
  
  # Save names of subdatasets
  sds_names <- sub('.*\\:', '', get_subdatasets(files[1]))
  print(sds_names)
  
  # Conversion into geotiff, corresponding to list of layer names
  stand <- sprintf("Verzeichnis: %s", dirs[i])
  print(stand)
  
  # List of relevant SDS
  vars <-
    #c("LST_Day_1km","QC_Day","Day_view_angl","Clear_day_cov","Clear_sky_days","LST_Night_1km","QC_Night","Night_view_angl","Clear_night_cov","Clear_sky_nights")
    c("1 km monthly NDVI", "1 km monthly VI Quality")

  # For every SDS (here max. SDS 4 necessary, else --> 1:length(sds_names))
  for (a in 1:length("NDVI")) {
    sds1 <- sds_names[a]
    if (sds1 %in% vars)
    {
      s <-
        sprintf("Subdataset %s of %s: %s", a, length(sds_names), sds_names[a])
      print(s)
      for (b in 1:length(files)) {
        setwd(paste0(workDir,"/",dirs[i]))
        g <-sprintf("Converting %s of %s in Folder %s: SDS: %s",b,length(files),getwd(),sds_names[a])
        print(g)
        dir.create("GeoTIFF", showWarnings = FALSE)
        outfile <- paste0(files0[b], "_", sds_names[a], ".tif")
        outname <- paste0(files0[b], "_", sds_names[a])
        outdir <- "/GeoTIFF/"
        gdal_translate(files[b], paste0(getwd(), outdir, outfile), sd_index = a)
        setwd(paste0(getwd(), "/GeoTIFF"))
        
        # ------------------------REPROJECTION OF UNPROJECTED RASTER FILES------------------------ #
        
        ## for further information visit...
        ## https://gis.stackexchange.com/questions/154276/reprojecting-raster-from-lat-lon-to-utm-in-r
        
        # Create RasterLayer object
        r <- raster(paste0(outfile))
        
        # Define the Proj.4 spatial reference 
        sr <- "+proj=longlat +datum=WGS84 +no_defs" # desired GeoTIFF SRS
        
        # Project Raster
        projected_raster <- projectRaster(r, crs = sr)
        
        # Write the RasterLayer to disk (See datatype documentation for other formats)
        writeRaster(projected_raster, filename=paste0(outname, "_latlon_wgs84.tif"), datatype='FLT4S', overwrite=TRUE)
        
        # Remove unprojected files in working directory
        file.remove(outfile)

      }
    }
  }
}

# ----------------------------SUBSETTING FILES TO THURINGIA EXTENT---------------------------- #

###Schritte: 
###  1. Verzeichnisliste einlesen (dirs)
###  2. in jedem Verzeichnis in den Geotif Ordner wechseln (dort, wo die Produkte als komplette große Szenen liegen)
###  3. jeweilige Szene in Raster konvertieren
###  4. Check ob Untersuchungsgebiet in Szene liegt 
###    ja   ---> Untersuchungsgebiet ausschneiden / geotiff/netcdf erstellen
###   nein  ---> Text ausgeben und mit nächsten Untersuchungsgebiet testen
###  5. Ergebnis: insgesamt 12 neue Unterordner Sortiert nach Untersuchungsgebiet und netcdf bzw. geotiff
############################################################################


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
