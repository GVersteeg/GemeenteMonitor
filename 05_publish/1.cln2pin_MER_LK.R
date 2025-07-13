# --------------------------------------------------------------------------- #
# Script to create a RStudio Connect Board for Onderzoek/MER_LK files         #
# --------------------------------------------------------------------------- #
# Author: Gerrit Versteeg
# Date: 01-03-2023
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# Prework - Environment Variables
# Prior to beginning, environment variables must be set in your development   #
# environment for pins to work.                                               #
#   1. Create an API key from RStudio Connect                                 #
#                         (https://docs.rstudio.com/connect/user/api-keys/)   #
#   2. Create a `.Renviron` file in this directory to store the API key and   #
#      RStudio Connect server address variables.                              #
#      Example .Renviron file contents                                        #
#           CONNECT_SERVER="<your-connect-server>"                            #
#           CONNECT_API_KEY="<your-connect-api-key>"                          #
# (https://rviews.rstudio.com/2017/04/19/r-for-enterprise-understanding-r-s-startup/).
# --------------------------------------------------------------------------- #

## Housekeeping ------------------------------------------------------------- #
library(pins)

rm(list=ls()) 

dir_base <- Sys.getenv("POS_DATALAKE")
dir_cln <- paste0(dir_base, "schone_data/Onderzoeken/MER_LK/")
dir_cln_ev <- paste0(dir_cln,"energie")
dir_cln_wt <- paste0(dir_cln,"woningen")
dir_cln_geo <- paste0(dir_cln,"geo")
dir_cln_pic <- paste0(dir_cln,"pictures")

fname_onz_zip <- paste0("MER_LK", ".zip")
fpath_onz_zip <- paste0(dir_cln, fname_onz_zip)

# --------------------------------------------------------------------------- #
## --- Register board ------------------------------------------------------- #
# Pins will use CONNECT_API_KEY and CONNECT_SERVER by default,
# but we are being explicit here anyway.
board_register_rsconnect(
  key = Sys.getenv("CONNECT_API_KEY"), 
  server = Sys.getenv("CONNECT_SERVER")
)

# --------------------------------------------------------------------------- #
## --- ZIP alle onderzoeksbestanden ----------------------------------------- #
# --------------------------------------------------------------------------- #
## Collect all RDS-files for zipping       ---------------------------------- #
files2zip <- list.files(dir_cln_ev, 
                        pattern = ".rds", 
                        full.names = TRUE)
files2zip <- c(files2zip, 
               list.files(dir_cln_wt, 
                          pattern = ".rds", 
                          full.names = TRUE))
files2zip <- c(files2zip, 
               list.files(dir_cln_geo, 
                          pattern = ".rds", 
                          full.names = TRUE))

## Collect all Shape-files for zipping     ---------------------------------- #
files2zip <- c(files2zip, 
               list.files(dir_cln_geo, 
                          pattern = "^buurten_ev\\.",
                          full.names = TRUE))
files2zip <- c(files2zip, 
               list.files(dir_cln_geo, 
                          pattern = "^gemeentegrens\\.",
                          full.names = TRUE))
files2zip <- c(files2zip, 
               list.files(dir_cln_geo,
                          pattern = "^focusgebied_vastgesteld\\.",
                          full.names = TRUE))
files2zip <- c(files2zip, 
               list.files(dir_cln_geo,
                          pattern = "^stedin_",
                          full.names = TRUE))
files2zip <- c(files2zip, 
               list.files(dir_cln_geo,
                          pattern = "^scenarios_",
                          full.names = TRUE))

## Collect all picture-files for zipping     -------------------------------- #
files2zip <- c(files2zip, 
               list.files(dir_cln_pic,
                          pattern = ".png",
                          full.names = TRUE))

# --------------------------------------------------------------------------- #
## --- Zip all onderzoeksbestanden and PIN the ZIP to RSConnect ------------- #
# --------------------------------------------------------------------------- #
zip(fpath_onz_zip, files2zip, extras = '-j')

## Pin description data ----------------------------------------------------- #
naam <- "MER_LK_researchdata"
beschr <- paste0("Files for MER-Leerkring research uploaded from datalake: ",
                 Sys.Date())
pin(fpath_onz_zip, 
    name = naam, 
    description = beschr, 
    board = "rsconnect")

## Delete the zip-file
if (file.exists(fpath_onz_zip)) file.remove(fpath_onz_zip)


# -- end of script ---------------------------------------------------------- #
