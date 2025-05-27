## Script voor opbouwen LUT regiocodes (std. CBS 10 code) en namen vanuit --- #
##        laatste CBS wijken en buurten bestand                           --- #
## Datum: 2022-11-06                                                      --- #
## Door: Gerrit Versteeg                                                  --- #
## -------------------------------------------------------------------------- #
## ----------- housekeeping ------------------------------------------------- #
## -------------------------------------------------------------------------- #
## bron buurten: https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/wijk-en-buurtkaart-2022
##               NB. handmatig gedwonload 
## -------------------------------------------------------------------------- #
## loading libraries -------------------------------------------------------- #
library(tidyverse)
library(sf)
library(rgdal)
library(rgeos)

## Folders and paths -------------------------------------------------------- #
dir_parm <- "./parameters/"
dir_base <- Sys.getenv("POS_DATALAKE")

dir_raw <- paste0(dir_base, "ruwe_data/CBS/")          ## input dir CBS-files #
dir_lut <- paste0(dir_base, "luts/")               ## output dir LUT voor GWB #
dir_raw_geo <- paste0(dir_raw, "geografie/WijkBuurtkaart_2022_v1/")

fname_brt <- "buurt_2022_v1.shp"               ## shape file met alle buurten #
fpath_brt <- paste0(dir_raw_geo, fname_brt)
fname_wk <- "wijk_2022_v1.shp"                  ## shape file met alle wijken #
fpath_wk <- paste0(dir_raw_geo, fname_wk)
fname_lut_wide <- "cbs_lut_gwb.rds"         ## result rds met wijk/buurt data #
fpath_lut_wide <- paste0(dir_lut, fname_lut_wide)
fname_lut_long <- "cbs_lut_regios.rds" ## result rds met regio codes en namen #
fpath_lut_long <- paste0(dir_lut, fname_lut_long)

## -------------------------------------------------------------------------- #
## -- Inlezen ruwe shapefiles: buurten en wijken ---------------------------- #
## -------------------------------------------------------------------------- #
sf_brt <- readOGR(fpath_brt)
df_brt <- sf_brt@data %>% 
  filter(GM_CODE == "GM0518") %>% 
  select(GM_CODE, GM_NAAM, WK_CODE, BU_CODE, BU_NAAM)

sf_wk <- readOGR(fpath_wk)
df_wk <- sf_wk@data %>% 
  filter(GM_CODE == "GM0518") %>%
  select(WK_CODE, WK_NAAM)

## -------------------------------------------------------------------------- #
## -- Create wide gwb dataset buurten en wijken ----------------------------- #
## -------------------------------------------------------------------------- #
lut_gwb_wide <- df_brt %>% 
  left_join(df_wk) %>% 
  mutate(across(everything(), as.character),
         JAAR = "2022") %>%
  select(JAAR, GM_CODE, GM_NAAM, WK_CODE, WK_NAAM, BU_CODE, BU_NAAM)

## -------------------------------------------------------------------------- #
## -- Create long gwb dataset regiocodes ------------------------------------ #
## -------------------------------------------------------------------------- #
df_tmp_brt <- lut_gwb_wide %>% 
  select(JAAR, starts_with("BU_")) %>% 
  distinct()
colnames(df_tmp_brt) <- c("jaar", "regio", "naam")
df_tmp_wk <- lut_gwb %>% 
  select(JAAR, starts_with("WK_")) %>% 
  distinct()
colnames(df_tmp_wk) <- c("jaar", "regio", "naam")
df_tmp_gm <- lut_gwb %>% 
  select(JAAR, starts_with("GM_")) %>% 
  distinct()
colnames(df_tmp_gm) <- c("jaar", "regio", "naam")

lut_gwb_long <- rbind(df_tmp_gm, df_tmp_wk, df_tmp_brt)

## save the resulting gwb-file in R-format
saveRDS(lut_gwb_wide, file = fpath_lut_wide)
saveRDS(lut_gwb_long, file = fpath_lut_long)

## --end of script ---------------------------------------------------------- #

