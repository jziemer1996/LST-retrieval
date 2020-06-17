# ------------------------------------------------------------------------------------------------ #
# How to Set Up Direct Access the LP DAAC Data Pool with R
# The following R code will configure a netrc profile that will allow users to download data from
# an Earthdata Login enabled server.
# ------------------------------------------------------------------------------------------------ #
# Author: Cole Krehbiel
# Last Updated: 11/20/2018
# ------------------------------------------------------------------------------------------------ #
# Check for required packages, install if not previously installed
if ("sys" %in% rownames(installed.packages()) == FALSE) {install.packages("sys")}
if ("getPass" %in% rownames(installed.packages()) == FALSE) { install.packages("getPass")}

# Load necessary packages into R
library(sys)
library(getPass)

# -----------------------------------SET UP ENVIRONMENT------------------------------------------- #
usr <- file.path(Sys.getenv("USERPROFILE"))  # Retrieve user directory (for netrc file)
if (usr == "") {usr = Sys.getenv("HOME")}    # If no user profile exists, use home directory
netrc <- file.path(usr,'.netrc', fsep = .Platform$file.sep) # Path to netrc file

# ----------------------------------CREATE .NETRC FILE-------------------------------------------- #
# If you do not have a  .netrc file with your Earthdata Login credentials stored in your home dir,
# below you will be prompted for your NASA Earthdata Login Username and Password and a netrc file
# will be created to store your credentials (home dir). Create an account at: urs.earthdata.nasa.gov
if (file.exists(netrc) == FALSE || grepl("urs.earthdata.nasa.gov", readLines(netrc)) == FALSE) {
  netrc_conn <- file(netrc)
  
  # User will be prompted for NASA Earthdata Login Username and Password below
  writeLines(c("machine urs.earthdata.nasa.gov",
               sprintf("login %s", getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov):")),
               sprintf("password %s", getPass(msg = "Enter NASA Earthdata Login Password:"))), netrc_conn)
  close(netrc_conn)
}
