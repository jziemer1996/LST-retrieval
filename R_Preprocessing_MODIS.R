# ------------------------------------------------------------------------------------------------ #
# Tool to convert (GeoTIFF) and reproject (LatLon or UTM) the downloaded MODIS files (hdf).
# ------------------------------------------------------------------------------------------------ #
# Author: Sandra Bauer
# Modifided by: Marlin Mueller & Jonas Ziemer
# ------------------------------------------------------------------------------------------------ #

# Load necessary packages into R
library(gdalUtils)
# require(raster)
# require(sp)
# require(MODIS)

# ------------------------------------SET UP ENVIRONMENT------------------------------------------ #

## still needs to copy all downloaded hdf files in your desired working folder because R_Download_MODIS.R 
## writes it to home directory as default because setting the desired download directory doenst work!

workDir = ""
setwd(workDir)


#List for all hdf files
dirs <- dir()[file.info(dir())$isdir]

# ---------------------------------------REPROJECTION--------------------------------------------- #

#Start and end projection:
frm.srs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m -r cubic+no_defs" # original HDF SRS
#to.srs = "+proj=longlat +datum=WGS84 +no_defs" # desired GeoTIFF SRS

#Geographic Coordinates (Latlon WGS84)
#to.srs = "EPSG:4326" #http://spatialreference.org/ref/epsg/4326/

#UTM Zone 32N EPSG:25832
to.srs = "EPSG:32632" #http://spatialreference.org/ref/epsg/32632/

# ---------------------------------CONVERSION INTO GEOTIFFS--------------------------------------- #

for (i in 1:length(dirs)) {
  
  setwd(paste0(workDir,"/",dirs[i]))
  
  #Dateiliste aller Hdf im Verzeichnis
  files <- list.files(pattern = "hdf$")
  dir.create("GeoTIFF",showWarnings = FALSE)
  
  #Produktdateinamen ohne Endung und Erstellungsdatum
  files0 <- substr(gsub(".hdf", "", files), 1, nchar(gsub(".hdf", "", files)) - 14)
  
  #Subdatasetnamen speichern
  sds_names <- sub('.*\\:', '', get_subdatasets(files[1]))
  #umwandeln in geotiff, nach Namensliste:
  stand <- sprintf("Verzeichnis: %s", dirs[i])
  print(stand)
  #Liste der relevanten SDS anlegen:
  vars <-
    #c("LST_Day_1km","QC_Day","Day_view_angl","Clear_day_cov","Clear_sky_days","LST_Night_1km","QC_Night","Night_view_angl","Clear_night_cov","Clear_sky_nights")
    c("LST_Night_1km","LST_Day_1km")
  
  #Für jedes Subdataset(Hier nur maximal SDS 4 benötigt, ansonsten --> 1:length(sds_names))
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
        gdalwarp(paste0(outfile),paste0(outname, "_utm32_wgs84.tif"), s_srs = frm.srs, t_srs = to.srs,
                 #gdalwarp(paste0(outfile),paste0(outname, "_latlon_wgs84.tif"), s_srs = frm.srs, t_srs = to.srs,
                 srcnodata = -3000, dstnodata = -99999, tr=c(1000,1000), r="bilinear", overwrite = T)
        file.remove(outfile)
      }
    }
  }
}