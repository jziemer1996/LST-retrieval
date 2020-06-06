# ------------------------------------------------------------------------------------------------ #
# Tool to Access the LP DAAC Data Pool with R
# The following R code example demonstrates how to configure a connection to download data from an
# Earthdata Login enabled server, specifically the LP DAAC Data Pool.
# ------------------------------------------------------------------------------------------------ #
# Author: Cole Krehbiel
# Modifided by: Marlin Mueller & Jonas Ziemer
# ------------------------------------------------------------------------------------------------ #

# Check for required packages, install if not previously installed
if ("sys" %in% rownames(installed.packages()) == FALSE) {install.packages("sys")}
if ("getPass" %in% rownames(installed.packages()) == FALSE) { install.packages("getPass")}
if ("httr" %in% rownames(installed.packages()) == FALSE) { install.packages("httr")}

# Load necessary packages into R
library(sys)
library(getPass)
library(httr)

# ---------------------------------SET UP ENVIRONMENT--------------------------------------------- #

# IMPORTANT: Update the line below if you want to download to a different directory (ex: "c:/data/")
dl_dir <- Sys.getenv("HOME")                                 # Set dir to download files to
setwd(dl_dir)                                                # Set the working dir to the dl_dir
usr <- file.path(Sys.getenv("USERPROFILE"))                  # Retrieve home dir (for netrc file)
if (usr == "") {usr = Sys.getenv("HOME")}                    # If no user profile exists, use home
netrc <- file.path(usr,'.netrc', fsep = .Platform$file.sep)  # Path to netrc file

# ------------------------------------CREATE .NETRC FILE------------------------------------------ #

# If you already have a .netrc file with your Earthdata Login credentials stored in your home
# directory, this portion will be skipped. Otherwise you will be prompted for your NASA Earthdata
# Login Username/Password and a netrc file will be created to store your credentials (in home dir)
if (file.exists(netrc) == FALSE || grepl("urs.earthdata.nasa.gov", readLines(netrc)) == FALSE) {
  netrc_conn <- file(netrc)
  
  # User will be prompted for NASA Earthdata Login Username and Password below
  writeLines(c("machine urs.earthdata.nasa.gov",
               sprintf("login %s", getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :")),
               sprintf("password %s", getPass(msg = "Enter NASA Earthdata Login Password:"))), netrc_conn)
  close(netrc_conn)
}

# ---------------------------CONNECT TO DATA POOL AND DOWNLOAD FILES------------------------------ #

# Below, define either a single link to a file for download, a list of links, or a text file
# containing links to the desired files to download. For a text file, there should be 1 file link
# listed per line. Here we show examples of each of the three ways to download files.
# **IMPORTANT: be sure to update the links for the specific files you are interested in downloading.

# 1. Single file (this is just an example link, replace with your desired file to download):
#files <- "https://e4ftl01.cr.usgs.gov/MOLT/MOD13A3.006/2020.03.01/MOD13A3.A2020061.h20v12.006.2020100130510.hdf"

# 2. List of files (links in extent of Thuringia for MOD13A3):
files <- c("https://e4ftl01.cr.usgs.gov//MODV6_Cmp_B/MOLT/MOD13A3.006/2020.03.01/MOD13A3.A2020061.h18v03.006.2020100131933.hdf",
           "https://e4ftl01.cr.usgs.gov//MODV6_Cmp_B/MOLT/MOD13A3.006/2020.03.01/MOD13A3.A2020061.h18v04.006.2020100131914.hdf",
           "https://e4ftl01.cr.usgs.gov//MODV6_Cmp_B/MOLT/MOD13A3.006/2020.04.01/MOD13A3.A2020092.h18v04.006.2020130020050.hdf",
           "https://e4ftl01.cr.usgs.gov//MODV6_Cmp_B/MOLT/MOD13A3.006/2020.04.01/MOD13A3.A2020092.h18v03.006.2020130015736.hdf")

# Thuringia Extent:
#   BBox (SW): 9.5 E, 49.5 N
#   BBox (NO): 12.5 E, 50.5 N


# 3. Textfile containing links (just an example, replace with your text file location):
#files <- readLines("C:/datapool_downloads/URL_file_list.txt", warn = FALSE)

# Loop through all files
for (i in 1:length(files)) {
  filename <-  tail(strsplit(files[i], '/')[[1]], n = 1) # Keep original filename
  
  # Write file to disk (authenticating with netrc) using the current directory/filename
  response <- GET(files[i], write_disk(filename, overwrite = TRUE), progress(),
                  config(netrc = TRUE, netrc_file = netrc), set_cookies("LC" = "cookies"))
  
  # Check to see if file downloaded correctly
  if (response$status_code == 200) {
    print(sprintf("%s downloaded at %s", filename, dl_dir))
  } else {
    print(sprintf("%s not downloaded. Verify that your username and password are correct in %s", filename, netrc))
  }
}
