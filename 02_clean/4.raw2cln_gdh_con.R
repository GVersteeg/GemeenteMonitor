## Script voor het schonen van het laadkisten-bestand beheerd binnen DSB ---- #
##        het bestand wordt handmatig in ruwe_data geplaatst              --- #
## Datum: 2025-11-07                                                      --- #
## Door: Gerrit Versteeg                                                  --- #
## -------------------------------------------------------------------------- #
## ----------- housekeeping ------------------------------------------------- #
## -------------------------------------------------------------------------- #

## loading libraries -------------------------------------------------------- #
library(tidyverse)
library(sf)
library(openxlsx)

## Clean slate -------------------------------------------------------------- #
rm(list = ls())                                            ## clear workspace #

## Folders and paths -------------------------------------------------------- #
dir_base <- Sys.getenv("POS_DATALAKE")
dir_base <- paste0(dir_base, "DenHaag/")

dir_raw <- paste0(dir_base, "ruwe_data/GDH_GemDenHaag/DSB/") ## dir DSB-files #
dir_cln <- paste0(dir_base, "schone_data/CircEcon/Afvalstromen/GDH/") 
dir.create(dir_cln, showWarnings = FALSE, recursive = TRUE)

fname_raw_con <- "Laadkisten_2025Q1.xlsx"   ## excel file met alle laadkisten #
fpath_raw_con <- paste0(dir_raw, fname_raw_con)
fname_cln_con <- str_replace(fname_raw_con, ".xlsx", ".rds")  ## clean cont's #
fpath_cln_con <- paste0(dir_cln, fname_raw_con)

## -------------------------------------------------------------------------- #
## -- Inlezen ruwe Excel met data over afval-containers --------------------- #
## -------------------------------------------------------------------------- #
df_raw <- read.xlsx(fpath_raw_con, sheet = 1)
df_con <- df_raw |> 
  rename(
    code = Code,
    adres = Location,
    stadsdeel = `Area.level.3`,
    wijk = `Area.level.4`,
    buurt = `Area.level.5`,
    type_1 = `Container.kind`,
    type_2 = `Container.type`,
    type_3 = `Container.type.2`,
    liters = Capacity,
    aantal = `Number.of.containers`,
    afvalsrt = `Waste.type`,
    freq = `Emptying.frequency`,
    dat_start = `Date.operational`,
    dat_end = `Date.operational.until`,
    dat_place = `Date.of.placement`,
    dat_deliver = `Date.of.delivery`,
    dat_replace = `Date.of.replacement`,
    owner = Owner,
    route = `Basic.route`,
    lat = `GPS-X`,
    long = `GPS-Y`,
    loc_oms = `Locatie.omschrijving`,
    opm_pos = `Position.code`,
    opm_loc = `Opmerking`,
    third_party = `Third.party`,
    ABS = `Herkomst.adresgegevens`,
    mon = `Monday`,
    tue = `Tuesday`,
    wed = `Wednesday`,
    thu = `Thursday`,
    fri = `Friday`,
    sat = `Saturday`,
    sun = `Sunday`) |>
  select(code, adres, stadsdeel, wijk, buurt, type_1, type_2, type_3, 
         liters, aantal, afvalsrt, freq,
         dat_start, dat_end, dat_place, dat_deliver, dat_replace,
         owner, route, loc_oms, opm_pos, opm_loc,
         third_party, ABS, mon, tue, wed, thu, fri, sat, sun, lat, long)

## -------------------------------------------------------------------------- #
## -- Correct lat/long-values and convert df_con into a shape file sf_con --- #
## -------------------------------------------------------------------------- #
df_con <- df_con |>             ## correct lat/long values with multiple dots #
  mutate(latnr = str_count(lat, "\\."),           ## count the number of dots #
         longnr = str_count(long, "\\.")) |>
  mutate(lat_cln = ifelse(latnr > 1,         ## remove dots if nr of dots > 1 #
                      str_remove_all(lat, "\\."), lat),
         long_cln = ifelse(longnr > 1, 
                      str_remove_all(long, "\\."), long)) |> 
  mutate(lat_nw = ifelse(latnr > 1,              ## place a dot on position 3 #
                      paste0(substr(lat_cln, 1, 2), ".", 
                        substr(lat_cln, 3, nchar(lat_cln))), lat_cln),
         long_nw = ifelse(longnr > 1,             ## place a dot on position 2 #
                      paste0(substr(long_cln, 1, 1), ".", 
                        substr(long_cln, 2, nchar(long_cln))), long_cln)) |> 
  mutate(lat = as.numeric(lat_nw),
         long = as.numeric(long_nw)) |> 
  select(-latnr, -lat_cln, -lat_nw, 
         -longnr, -long_cln, -long_nw) |> 
  filter(!is.na(lat) & !is.na(long))   ## remove record with missing lat/long #

df_con <- df_con |>                   ## change date columns into data format #
  mutate(dat_start = as.Date(dat_start, origin = "1899-12-30"),
         dat_end = as.Date(dat_end, origin = "1899-12-30"),
         dat_place = as.Date(dat_place, origin = "1899-12-30"),
         dat_deliver = as.Date(dat_deliver, origin = "1899-12-30"),
         dat_replace = as.Date(dat_replace, origin = "1899-12-30"))

sf_con <- df_con |> 
  filter(!is.na(lat) & !is.na(long)) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)


## save the resulting gwb-file in R-format
saveRDS(sf_con, file = fpath_cln_con)

## --end of script ---------------------------------------------------------- #

## aux
# function to remove any dots after the first dot in each string
# remove_extra_dots <- function(x) {
#   vapply(x, FUN.VALUE = character(1), function(s) {
#     i <- regexpr("\\.", s)
#     if (i == -1) return(s)
#     prefix <- substr(s, 1, i)                    # includes first dot
#     rest   <- substr(s, i + 1, nchar(s))         # after first dot
#     paste0(prefix, gsub("\\.", "", rest))       # remove dots from rest
#   })
# }


