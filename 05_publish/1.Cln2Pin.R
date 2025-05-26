# --------------------------------------------------------------------------- #
# Script to create a pin on RStudio Connect Board for Ext. Veiligheid files   #
# --------------------------------------------------------------------------- #
# Author: Gerrit Versteeg
# Date: 27-09-2023
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
# --------------------------------------------------------------------------- #
library(tidyverse)
library(rsconnect)
library(pins)

dir_base <- Sys.getenv("POS_DATALAKE")

dir_cln <- paste0(dir_base, "schone_data/")
dir_raw <- paste0(dir_base,"ruwe_data/")
dir_raw_ipo <- paste0(dir_raw,"IPO_InterProvOverleg/")
dir_raw_ipo_evs <- paste0(dir_raw_ipo,"ev_signaleringskaart/")
dir_raw_ipo_agn <- paste0(dir_raw_ipo_evs,"aandachtsgebieden")
dir_raw_ipo_kwg <- paste0(dir_raw_ipo_evs,"kwetsbare_objecten")
dir_raw_ipo_hlp <- paste0(dir_raw_ipo_evs,"hulplagen")
dir_raw_odh <- paste0(dir_raw,"ODH/")
dir_raw_odh_rrk <- paste0(dir_raw_odh,"RRK_RegRisicoKrt_Haaglanden/")
dir_raw_odh_agn <- paste0(dir_raw_odh_rrk,"Aandachtsgebieden")
dir_raw_pzh <- paste0(dir_raw,"PZH_ProvZuidHolland/")
dir_raw_pzh_wn <- paste0(dir_raw_pzh,"warmtenet/")
dir_raw_pzh_wnl <- paste0(dir_raw_pzh_wn,"leidingen")
dir_raw_geo <- paste0(dir_raw,"GeoBasis/")
dir_raw_geo_brt <- paste0(dir_raw_geo,"RD_buurten/2018/buurten")
dir_raw_geo_gem <- paste0(dir_raw_geo,"gemeenten/2019")

# fname_geo_brt <- "/buurten.shp"
# fpath_geo <- paste0(dir_raw_geo_brt, fname_geo_brt)
# fname_bag <- "/a11_qs_brandaandachtsgebieden.shp"
# fpath_bag <- paste0(dir_raw_ipo_agn, fname_bag)
# fname_eag <- "/a12_qs_explosieaandachtsgebieden.shp"
# fpath_eag <- paste0(dir_raw_ipo_agn, fname_eag)
# fname_kwg <- "/zeer_kwetsbare_gebouwen.shp"
# fpath_kwg <- paste0(dir_raw_ipo_kwg, fname_kwg)

dir_zip <- paste0(getwd(), "/zip4pin")
dir.create(file.path(dir_zip), showWarnings = FALSE)
fname_ev_zip <- paste0("/pin4ev", ".zip")
fpath_ev_zip <- paste0(dir_zip, fname_ev_zip)

# --------------------------------------------------------------------------- #
## Register board ----------------------------------------------------------- #
# Pins will use CONNECT_API_KEY and CONNECT_SERVER by default,
# but we are being explicit here anyway.
# --------------------------------------------------------------------------- #
board_register_rsconnect(
  key = Sys.getenv("CONNECT_API_KEY"), 
  server = Sys.getenv("CONNECT_SERVER")
)

# --------------------------------------------------------------------------- #
## Zip all EV-files
# --------------------------------------------------------------------------- #
# --- Copy all files to be zipped into one location ------------------------- #
files2copy <- list.files(dir_raw_geo_brt,
                         pattern = "buurten\\.",
                         full.names = TRUE)
files2copy <- c(files2copy, list.files(dir_raw_geo_gem,
                                       pattern = "^gemeenten",
                                       full.names = TRUE))
files2copy <- c(files2copy, list.files(dir_raw_ipo_agn,
                                       pattern = "^a11",
                                       full.names = TRUE))
files2copy <- c(files2copy, list.files(dir_raw_ipo_agn,
                                       pattern = "^a12",
                                       full.names = TRUE))
files2copy <- c(files2copy, list.files(dir_raw_ipo_kwg,
                                      pattern = "^zeer_kwetsbare_gebouwen",
                                      full.names = TRUE))
files2copy <- c(files2copy, list.files(dir_raw_odh_agn,
                                       pattern = "Gesloten verklaring",
                                       full.names = TRUE))
files2copy <- c(files2copy, list.files(dir_raw_odh_agn,
                                       pattern = "^Routering",
                                       full.names = TRUE))
files2copy <- c(files2copy, list.files(dir_raw_odh_agn,
                                       pattern = "^Ontheffingsroutes",
                                       full.names = TRUE))
files2copy <- c(files2copy, list.files(dir_raw_odh_agn,
                                       pattern = "^200 meter zone BP",
                                       full.names = TRUE))
files2copy <- c(files2copy, list.files(dir_raw_pzh_wnl,
                                       pattern = "^SK_ONDERGRONDSEINFRA_WARM",
                                       full.names = TRUE))

file.copy(files2copy, dir_zip, copy.date = TRUE, overwrite = TRUE)

# --- Zip all files in that location --------------------------------- #
files2zip <- list.files(dir_zip, full.names = TRUE)
zip(fpath_ev_zip, files2zip, extras = '-j')

## Pin description data ----------------------------------------------------- #
pin(fpath_ev_zip, 
    name = "EV_data", 
    description = paste0("Files for Monitoer Externe Veiligheid, uploaded from datalake: ", Sys.Date()), 
    board = "rsconnect")

## Delete the zip-file
if (file.exists(fpath_ev_zip)) file.remove(fpath_ev_zip)

# -- end of script ---------------------------------------------------------- #
