#nohup Rscript ArsAfricaE_KNP_GrassBiomass_ModelPrototype_v01.R
#renice -n 19 -u c4bech
#top -H -u c4bech
#sample <- sample[,-(1:22),drop=FALSE]
#modify reference shapefile so that it contains only the column with the reference information
#make leave out sample

#####################################
message("Loading required functions")
#####################################

#libraries
suppressMessages(library(raster))
suppressMessages(library(rgdal))
suppressMessages(library(foreach))

#####################################
message("Setting basic variables...")
#####################################

## Jonas working directory
wdir = "C:/Users/jz199/Documents/Studium/Master/2. Semester/Vorlesungsmitschriften/GEO411 - Landschaftsmanagement und Fernerkundung/Auszug_Daten_SandraBauer_MA/Auszug_Daten_SandraBauer_MA/"

## Marlin working directory
# wdir = ""

#test site
site="Analyses"

#image data folder
predictors="F:/411/LST/GeoTIFF/Thuringia/scaled/MOD11A1.A2020155.h18v03.006_LST_Day_1km_latlon_wgs84_Thuringia_celsius.tif" #predictors

#reference data folder
reference="C:/Users/jz199/Documents/Studium/Master/2. Semester/Vorlesungsmitschriften/GEO411 - Landschaftsmanagement und Fernerkundung/Auszug_Daten_SandraBauer_MA/Auszug_Daten_SandraBauer_MA/Stationen_Thüringen_Umland_3x3box.shp"

#response variable
response.type="Output"

#reference data shapefile
refdata="Klimastationen_Thueringen_MODIS.shp"

# Altenburg_Rural_Dissolved.shp
# Altenburg_Urban_Dissolved.shp
# Eisenach_Rural_Dissolved.shp
# Eisenach_Urban_Dissolved.shp
# Erfurt_Rural_Dissolved.shp
# Erfurt_Urban_Dissolved.shp
# Gera_Rural_Dissolved.shp
# Gera_Urban_Dissolved.shp
# Gotha_Rural_Dissolved.shp
# Gotha_Urban_Dissolved.shp
# Jena_Rural_Dissolved.shp
# Jena_Urban_Dissolved.shp
# Muehlhausen_Rural_Dissolved.shp
# Muehlhausen_Urban_Dissolved.shp
# Nordhausen_Rural_Dissolved.shp
# Nordhausen_Urban_Dissolved.shp
# Suhl_Rural_Dissolved.shp
# Suhl_Urban_Dissolved.shp
# Weimar_Rural_Dissolved.shp
# Weimar_Urban_Dissolved.shp
# Klimastationen_Thueringen_v3_utm32_wgs84.shp

#refdata column name
refdata.column="FID"

#refdata unit
#refdata.unit=""

#bounding box (mapping extent/tiles)
#bboxname="KNP_buffer1km_transect_tiles_45km_intersect_tile_11_utm36s_v07.shp"

#target resolution
#targetres=1000

#number of cores (up to 100)
n.cores=4

#full sample name
#fullsample.name="_fullsample.RData"

#trained model name
#trainedModel.name="trainedModel.RData"

#load ancillary functions
source(paste(wdir,site,"/01_FB_Functions.R", sep=""))
#source(paste(wdir,"01_FB_Functions.R", sep=""))

#################################
message("Loading image data...")
#################################

#selective loading of predictors;
#(see patternator function)
imagepath=paste(wdir,site,"/",predictors,sep="")
images=lapply(patternator(imagepath,in_p=".tif"),function(x)raster::raster(x))

####################################
message("Loading reference data...")
####################################

#define I/O data
name.sample.in=paste(wdir,site,"/",reference,"/",refdata,sep="")
name.sample.out=paste(wdir,site,"/",response.type,"/Mean_LST_",refdata,sep="")

#read reference data shapefile
sample=shapereader(name.sample.in)

############################################
message("Creating the training data set...")
############################################

#extract predictor values at reference data point locations;
#(see multiextract and multiextract2 function)
smallsample=multiextract(images,sample,cores=n.cores)

#remove NA values;
#not necessarily required;
#smallsample@data=na.omit(smallsample@data)

#save training data set as shapefile
shapewriter(smallsample,name.sample.out)

#subset and rename training data set for later usage
#sample <- sample[,-(1:23),drop=FALSE]
#sample=smallsample@data