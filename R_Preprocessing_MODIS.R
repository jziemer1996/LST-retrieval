# ------------------------------------------------------------------------------------------------ #
# Tool to preprocess downloaded MODIS data for LST analysis
# ------------------------------------------------------------------------------------------------- #
# - 1st part: Convert (GeoTIFF) and reproject (LatLon or UTM) the downloaded MODIS files (hdf).
# - 2nd part: Subsetting the MODIS files to the extent of Thuringia.
# - 3rd part: Rescaling of LST values from Kelvin to Celsius.
# ------------------------------------------------------------------------------------------------- #
# Author: Sandra Bauer
# Modified by: Marlin Mueller & Jonas Ziemer
# FSU JENA, 2020
# ------------------------------------------------------------------------------------------------- #


###------------------------------------------1st part---------------------------------------------###
###---------------------------CONVERSION INTO GEOTIFFS AND REPROJECTION---------------------------###

# Set environment language to english
Sys.setenv(LANG = "en")

# Load necessary packages into R
library(gdalUtils)
library(raster)
library(sp)
library(rgdal)
library(ncdf4)
library(tools)
library(rgeos)

# ------------------------------------SET UP ENVIRONMENT------------------------------------------- #

## Working directory
# Example workDir = "F:/411/"
workDir = ""

# Set working directory
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
  files0 <- substr(gsub(".hdf", "", files), 1, nchar(gsub(".hdf", "", files)))
  
  # Save names of subdatasets
  sds_names <- sub('.*\\:', '', get_subdatasets(files[1]))
  print(sds_names)
  
  # Conversion into geotiff, corresponding to list of layer names
  stand <- sprintf("Verzeichnis: %s", dirs[i])
  print(stand)
  
  # List of relevant SDS
  vars <-
    #c("LST_Day_1km","QC_Day","Day_view_angl","Clear_day_cov","Clear_sky_days","LST_Night_1km","QC_Night","Night_view_angl","Clear_night_cov","Clear_sky_nights")
    c("LST_Day_1km")

  # For every SDS (here max. SDS 4 necessary, else --> 1:length(sds_names))
  for (a in 1:length(sds_names)) {
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
        writeRaster(projected_raster, filename=paste0(outname, "_.tif"), datatype='FLT4S', overwrite=TRUE)
        
        # Remove unprojected files in working directory
        file.remove(outfile)

      }
    }
  }
}


###------------------------------------------2nd part---------------------------------------------###
###-----------------------------SUBSETTING FILES TO THURINGIA EXTENT------------------------------###


# ------------------------------------READ IN THURINGIA SHAPEFILE---------------------------------- #

## Path for Shapefile:
# Example Path: Thuringia <- readOGR("F:/GEO411_data/MODIS_R_dir/shape/thuringia.shp")
Thuringia <- readOGR("")

shapes <- as.list.data.frame(c(Thuringia))

shapes_n <- as.list.data.frame(c("Thuringia"))

# ------------------------------------SET UP ENVIRONMENT------------------------------------------- #

## Working directory
# Example workDir = "F:/411/LST/"
workDir = ""

# Directory of the corresponding variable
setwd(workDir)

# List of all directories
dirs <- dir()[file.info(dir())$isdir]

for (i in 1:length(dirs)) {
  
  # Corresponding Geotiff directory
  setwd(paste0(workDir,dirs[i]))
  stand <- sprintf("Verzeichnis: %s", dirs[i])
  print(stand)
  
  # Save list of all tif-files in directory
  files <- list.files(pattern = "tif$")
  
  # ----------SUBSETTING FILES AND DELETE THE ONES WHICH ARE OUT OF THE THURINGIAN BORDERS-------- #
  
  for (a in 1:length(files)) {
    
    # Raster of full scene
    bigscene <- raster(files[a])
    
    # Raster polygon with extent of Thuringian borders
    big_extent <- extent(bigscene)
    big_e_sp <- as(big_extent, 'SpatialPolygons')
    crs(big_e_sp) <- crs(bigscene)
    datei <- sprintf("Verzeichnis: %s Datei: %s von %s", dirs[i],a,length(files))
    print(datei)
    
    for (x in 1:length(shapes)) {
      shape <- as(extent(bbox(shapes[[x]])), 'SpatialPolygons')
      crs(shape) <- crs(bigscene)
      
      # Just take the scenes which overlap with Thuringian borders
      if (gIntersects(shapes[[x]],big_e_sp)) {
        
        dir.create(paste0(getwd(),("/"),shapes_n[x]))
        outtif <- paste0(getwd(),("/"),shapes_n[x],"/",file_path_sans_ext(files[a]),"_",shapes_n[x],".tif")
        writeRaster(crop(bigscene,shape), filename=outtif, format="GTiff", overwrite=TRUE)
      }
      else {
        print("Does not overlap!")
      }
    }
  }
}


###------------------------------------------3rd part---------------------------------------------###
###--------------------------------RESCALING FROM KELVIN TO CELSIUS-------------------------------###


# ------------------------------------SET UP ENVIRONMENT------------------------------------------- #

## Working directory
# Example workDir = "F:/411/LST/GeoTIFF/"
workDir = ""

# Directory of the corresponding variable
setwd(workDir)

# List of all directories
dirs <- dir()[file.info(dir())$isdir]

# -------------------------------CONFIGURE OUTPUT PARAMETERS--------------------------------------- #

# Configure output resolution and desired resampling method
tr=c(1000,1000)
r="bilinear" # "nearest"|"bilinear"|"cubic"|"cubicspline"|"lanczos"|"average"|"mode"

for (i in 1:length(dirs)) {
  
  # Change directory to current used variable
  setwd(paste0(workDir,dirs[i]))
  stand <- sprintf("Verzeichnis: %s", dirs[i])
  print(stand)
  
  # Save list of all .tif files
  files <- list.files(pattern = "*LST*")
  
  # Create new directory
  dir.create(paste0(getwd(),("/"),"scaled"),showWarnings = FALSE)
  
  #--------------------------------RESCALING FROM KELVIN TO CELSIUS-------------------------------#

   for (a in 1:length(files)) {
     
     # Load in raster files
     file <- raster(files[a])
     
     # Conversion of Kelvin into Celsius 
     file_scaled_to_celsius <- calc(file, fun=function(x){x-273.15})
     
     # Print progress
     datei <- sprintf("Verzeichnis: %s Datei: %s von %s", dirs[i],a,length(files))
     print(datei)
     
     # Convert and write new tif-file 
     outtif_celsius <- paste0(getwd(),("/"),"scaled","/",file_path_sans_ext(files[a]),"_","celsius",".tif")
     writeRaster(file_scaled_to_celsius, filename=outtif_celsius, format="GTiff", overwrite=TRUE)
 
   }
 }

###--------------------------------------------------------------------------------------------###
###--------------------------------------------------------------------------------------------###

