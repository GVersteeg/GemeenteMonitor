## ------------------------------------------------------------------------- #
## Script om de bestanden voor de beleidsmonitor vanuit datalake te schonen  #
## en op te slaan in:                                                        #
## schone_data:                                                              #
## - shape-files                                                             #
##    + buurten.*                                                            #
##    + focusgebied_vastgesteld.*                                            #
##    + gemeentegrens.*                                                      #
##    + stedin_laagspanningsstations.*                                       #
##    + stedin_middenspanningsstations.*                                     #
##    + stedin_laagspanningsverbindingen.*                                   #
##    + stedin_middenspanningsverbindingen.*                                 #
## - csv-files                                                               #
##    + focusgebied-buurt.csv (LUT om focusgebieden te koppelen aan buurten  #
## - Excel-files                                                             #
##    + energie_woningtype_2010_2021.xls (jr.gem. energieverbr/woningtype)   #
##    + woningtype_buurt_2010_2022.xls (#woningen per type per buurt)        #
## ------------------------------------------------------------------------- #
## Author   : Gerrit Versteeg                                                #
## Project  : GemeenteMonitor                                                #
## ------------------------------------------------------------------------- #

## ------------------------ libraries -----------------------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)

## -------------------------------------------------------------------------- #
## ------------------------ fresh slate ---------------------------------------
## -------------------------------------------------------------------------- #
rm(list=ls())

## -------------------------------------------------------------------------- #
## ------------------------ source external functions -------------------------
## -------------------------------------------------------------------------- #
## (none needed for this script, but can be added if required)

## -------------------------------------------------------------------------- #
## ----- Read production parameters -------------------------------------------
## Switch for controling where the script actually runs:                      #
## productieomgeving = RStudio Connect (set as variable within the content)   #
## onderzoeksomgeving = RStudio Server Pro within the UDP                     #
## ontwikkelomgeving = Possible local development environment outside of UDP  #
## -------------------------------------------------------------------------- #
context <- Sys.getenv("CONTEXT")
ohg <- Sys.getenv("Onderhanden-Gemeente")
ohg_name <- str_split(ohg, pattern = " | ")[[1]][1]
ohg_code <- str_split(ohg, pattern = " | ")[[1]][3]
ohg_gmcd <- paste0("GM", ohg_code)
ohg_path <- str_split(ohg, pattern = " | ")[[1]][5]

dir_lake <- Sys.getenv("POS_DATALAKE")
dir_lake <- paste0(dir_lake, ohg_name, "/")

dir_proj <- getwd()            
dir_parm <- paste0(dir_proj, "/parameters/")

## -------------------------------------------------------------------------- #
## ------ Setup locations and filenames ---------------------------------------
## -------------------------------------------------------------------------- #
if (context != "productieomgeving") {
  dir_raw <- paste0(dir_lake, "ruwe_data/")
  dir_raw_geo <- paste0(dir_raw, "PDOK_PublDVOpKaart/")       ## gebiedsgrenzen
  dir_raw_alo <- paste0(dir_raw, "ALO_atlasleefomgeving/")      ## leefomgeving
  dir_raw_rev <- paste0(dir_raw, "REV_RegExtVeiligheid/")     ## ext.veiligheid
  dir_raw_cbs <- paste0(dir_raw, "CBS_CentrBureauStat/")          ## demografie
  dir_raw_enx <- paste0(dir_raw, "ENX_Enexis/ENEXIS_20250711")    ## el.netwerk
  dir_raw_ovg <- paste0(dir_raw, "OVG_OverigeBronnen/")  ## overige databronnen
  dir_luts <- paste0(dir_lake, "luts/")                        ## lookup-tables
  
  dir_cln <- paste0(dir_lake, "schone_data/")
  dir.create(dir_cln, showWarnings = FALSE)
  dir_cln_cbs <- paste0(dir_cln, "cbs/")
  dir.create(dir_cln_cbs, showWarnings = FALSE)
  dir_cln_geo <- paste0(dir_cln, "geo/")
  dir.create(dir_cln_geo, showWarnings = FALSE)
  dir_cln_onz <- paste0(dir_cln, "onderzoeken/")
  dir.create(dir_cln_onz, showWarnings = FALSE)
  dir_cln_wbo <- paste0(dir_cln_onz, "wbo_et/")
  dir.create(dir_cln_wbo, showWarnings = FALSE)
  dir_cln_evz <- paste0(dir_cln_wbo, "energie/")
  dir.create(dir_cln_evz, showWarnings = FALSE)
  dir_cln_won <- paste0(dir_cln_wbo, "woningen/")
  dir.create(dir_cln_won, showWarnings = FALSE)
  dir_cln_pic <- paste0(dir_cln_wbo, "pictures/")
  dir.create(dir_cln_pic, showWarnings = FALSE)

  fname_ewt <- "energie_woningtype_2010_2021"            ## jr.gem. EV/Won.type
  fpath_raw_ewt <- paste0(dir_raw_ovg, fname_ewt, ".xls")
  fpath_cln_ewt <- paste0(dir_cln_evz, "energie_woningtype.rds")
  fname_wtb <- "woningtype_buurt_2010_2022"               ## won.type per buurt
  fpath_raw_wtb <- paste0(dir_raw_ovg, fname_wtb, ".xlsx")
  fpath_cln_wtb <- paste0(dir_cln_won, "woningtype_buurt.rds")
  fname_esb <- "elektrschaarste_buurt"            ## LUT Elektr.schaarste/buurt
  fpath_esb <- paste0(dir_luts, fname_esb, ".xlsx")
  fname_lut <- "focusgebied_buurt"                     ## LUT focusgebied/buurt
  fpath_raw_lut <- paste0(dir_raw_ovg, fname_lut, ".xlsx")
  fpath_cln_lut <- paste0(dir_cln_geo, fname_lut, ".rds")
  fname_cbs <- "cbs_dem_2004_2024"                      ## CBS Demografie/buurt
  fpath_cln_cbs <- paste0(dir_cln_cbs, fname_cbs, ".rds")
  fname_lut_wide <- "cbs_lut_gwb.rds"          ## wijde rds met wijk/buurt data
  fpath_lut_wide <- paste0(dir_luts, fname_lut_wide)

  fpath_cln_onz <- paste0(dir_cln_wbo, "demografie_gebied.rds")
  fpath_cln_scen <- paste0(dir_cln_wbo, "scenarios.rds")
}

## -------------------------------------------------------------------------- #
## -- Clean gebiedsgrenzen --------------------------------------------------
## -------------------------------------------------------------------------- #
sf_gem <- read_sf(paste0(dir_raw_geo, "gemeente_gegeneraliseerd.geojson")) %>% 
  st_transform(4326) %>% 
  filter(statcode == ohg_gmcd) %>% 
  select(statcode, statnaam, rubriek, geometry)
sf_wk <- read_sf(paste0(dir_raw_geo, "wijk_gegeneraliseerd.geojson")) %>% 
  st_transform(4326) %>% 
  filter(str_detect(statcode, paste0("WK",ohg_code))) %>% 
  mutate(statnr = str_sub(statnaam, start = 6L, end = 8L)) %>% 
  mutate(statnaam = str_remove(statnaam, statnr)) %>% 
  select(statcode, statnaam, rubriek, geometry)
sf_brt <- read_sf(paste0(dir_raw_geo, "buurt_gegeneraliseerd.geojson")) %>% 
  st_transform(4326) %>% 
  filter(str_detect(statcode, paste0("BU",ohg_code))) %>%
  select(statcode, statnaam, rubriek, geometry)
sf_geo <- bind_rows(sf_gem, sf_wk, sf_brt) %>% 
  mutate(statnaam = str_remove(statnaam, "Gemeente |Wijk |Buurt ")) %>% 
  rename(code = statcode, naam = statnaam, type = rubriek)

write_rds(sf_geo, file = paste0(dir_cln_geo, "gebiedsgrenzen.rds"))
                      ## alle gebiedsgrenzen in de gemeente in /schone_data/geo

## Create lookup table for gemeente, wijk en buurt codes -------------------- #
df_wk <- sf_wk %>% 
  st_drop_geometry() %>%
  select(statcode, statnaam) %>%
  rename(WK_CODE = statcode, WK_NAAM = statnaam) %>% 
  mutate(WK_NAAM = str_remove(WK_NAAM, "Wijk "))
  
df_lut <- sf_brt %>% 
  st_drop_geometry() %>%
  select(-rubriek) %>%
  rename(BU_CODE = statcode, BU_NAAM = statnaam) %>%
  mutate(GM_CODE = ohg_gmcd,
         GM_NAAM = ohg_name,
         WK_CODE = str_sub(BU_CODE, start = 1L, end = 8L),
         WK_CODE = str_replace(WK_CODE, "BU", "WK")) %>%
  left_join(df_wk) %>% 
  select(GM_CODE, GM_NAAM, WK_CODE, WK_NAAM, BU_CODE, BU_NAAM)

rm(df_wk)
write_rds(df_lut, fpath_lut_wide)                   ## cbs_lut_gwb.rds in /luts

## -------------------------------------------------------------------------- #
## -- Clean Enexis netwerk-data ---------------------------------------------
## -------------------------------------------------------------------------- #
## build an index for the Enexis files
ix_fp <- list.files(dir_raw_enx, pattern = ".*\\.shp", full.names = TRUE)
ix_fn <- list.files(dir_raw_enx, pattern = ".*\\.shp", full.names = FALSE)
ix_fn <- str_replace(ix_fn, ".shp", ".rds")     # change file extension to .rds
ix_fn <- str_sub(ix_fn, start = 8L, end = 99L)        # remove prefix 'nbnl_e_'

## read the files, intersect with gemeentegrens and save them as .rds files
for (i in seq_along(ix_fp)) {
  tmp_pad <- ix_fp[i]
  tmp_naam <- str_remove(ix_fn[i], ".rds")
  sf_tmp <- read_sf(ix_fp[i]) %>% 
    st_transform(4326) %>% 
    st_intersection(sf_gem) %>% 
    mutate(sys_date = as.character(as.Date(file.mtime(tmp_pad))),
           sys_source = "ENX | SHP | Download",
           sys_file = tmp_naam) %>% 
    select(sys_source, sys_date, sys_file, everything())
  if (sum(st_is_valid(sf_tmp)) != nrow(sf_tmp)) {
    sf_tmp <- st_make_valid(sf_tmp)
  }
  write_rds(sf_tmp, file = paste0(dir_cln_geo, ix_fn[i]))
}

## -------------------------------------------------------------------------- #
## -- Setup clean data for research WBO Woningbouw-opgave.               ----
## -------------------------------------------------------------------------- #
## -- step 1. Demografische gebiedsdata ---------------------------------------
df_cbs <- read_rds(fpath_cln_cbs).                ## demografie per regio, jaar

## -- step 1a. Elektriciteitsverbruik ----------------------------------------
df_elc_wide <- df_cbs %>%    ## breed DF electriciteitsverbruik per woningtype
  select(Codering, SoortRegio, jaar,
         GemiddeldElektriciteitsverbruikTotaal,
         ends_with("_EV")) %>% 
  rename(tot_ev = GemiddeldElektriciteitsverbruikTotaal,
         app_ev = Appartement_EV,
         tus_ev = Tussenwoning_EV,
         hoek_ev = Hoekwoning_EV,
         tkap_ev = TweeOnderEenKapWoning_EV,
         vrij_ev = VrijstaandeWoning_EV,
         koop_ev = EigenWoning_EV,
         huur_ev = Huurwoning_EV)

df_elc_long <- df_cbs %>%       ## lang DF elektr.verbruik per woningtype/buurt
  filter(SoortRegio == "Buurt") %>%
  select(Codering, jaar, GemiddeldElektriciteitsverbruikTotaal,
         ends_with("_EV")) %>% 
  rename(code = Codering, 
         tot = GemiddeldElektriciteitsverbruikTotaal,
         app = Appartement_EV,
         tus = Tussenwoning_EV,
         hoek = Hoekwoning_EV,
         tkap = TweeOnderEenKapWoning_EV,
         vrij = VrijstaandeWoning_EV,
         koop = EigenWoning_EV,
         huur = Huurwoning_EV) %>% 
  left_join(df_lut[,c(5:6)], by = c("code" = "BU_CODE")) %>%
  rename(buurt = BU_NAAM) %>%
  mutate(verbr_type = "el") %>% 
  select(buurt, code, jaar, verbr_type, everything())

## -- step 1b. Aardgasverbruik ------------------------------------------------
df_gas_wide <- df_cbs %>%            ## breed DF aardgasverbruik per woningtype
  select(Codering, SoortRegio, jaar,
         GemiddeldAardgasverbruikTotaal,
         ends_with("_GV")) %>% 
  rename(tot_gv = GemiddeldAardgasverbruikTotaal,
         app_gv = Appartement_GV,
         tus_gv = Tussenwoning_GV,
         hoek_gv = Hoekwoning_GV,
         tkap_gv = TweeOnderEenKapWoning_GV,
         vrij_gv = VrijstaandeWoning_GV,
         koop_gv = EigenWoning_GV,
         huur_gv = Huurwoning_GV)

df_gas_long <- df_cbs %>%       ## lang DF aardgasverbruik per woningtype/buurt
  filter(SoortRegio == "Buurt") %>%
  select(Codering, jaar, GemiddeldAardgasverbruikTotaal,
         ends_with("_GV")) %>% 
  rename(code = Codering, 
         tot = GemiddeldAardgasverbruikTotaal,
         app = Appartement_GV,
         tus = Tussenwoning_GV,
         hoek = Hoekwoning_GV,
         tkap = TweeOnderEenKapWoning_GV,
         vrij = VrijstaandeWoning_GV,
         koop = EigenWoning_GV,
         huur = Huurwoning_GV) %>% 
  left_join(df_lut[,c(5:6)], by = c("code" = "BU_CODE")) %>%
  rename(buurt = BU_NAAM) %>%
  mutate(verbr_type = "gas") %>% 
  select(buurt, code, jaar, verbr_type, everything())

## -- step 1c. Woningtypen, Autos, Stedelijkheid ------------------------------
df_won_wide <- df_cbs %>%          ## breed DF woningtypen, auto, stedelijkheid
  select(Codering, SoortRegio, jaar,
         MateVanStedelijkheid, PersonenautoSTotaal, 
         PersonenautoSBrandstofBenzine, PersonenautoSOverigeBrandstof,
         NieuwbouwWoningen, Woningvoorraad, 
         p_Koopwoningen, p_HuurwoningenTotaal,
         p_TussenwoningEenGezins, p_HoekwoningEenGezins,
         p_TweeOnderEenKapWoningEenGezins,
         p_VrijstaandeWoningEenGezins, p_Eengezinswoning,
         p_Meergezinswoning) %>%
  rename(mvs = MateVanStedelijkheid,
         auto_tot = PersonenautoSTotaal,
         auto_benz = PersonenautoSBrandstofBenzine,
         auto_ovb = PersonenautoSOverigeBrandstof,
         tot_won = Woningvoorraad,
         nw_won = NieuwbouwWoningen,
         p_app = p_Meergezinswoning,
         p_tus = p_TussenwoningEenGezins,
         p_hoek = p_HoekwoningEenGezins,
         p_tkap = p_TweeOnderEenKapWoningEenGezins,
         p_vrij = p_VrijstaandeWoningEenGezins,
         p_egw = p_Eengezinswoning,
         p_koop = p_Koopwoningen,
         p_huur = p_HuurwoningenTotaal) %>% 
  mutate(app_won = round(p_app * tot_won / 100, 0),
         tus_won = round(p_tus * tot_won / 100, 0),
         hoek_won = round(p_hoek * tot_won / 100, 0),
         tkap_won = round(p_tkap * tot_won / 100, 0),
         vrij_won = round(p_vrij * tot_won / 100, 0),
         egw_won = round(p_egw * tot_won / 100, 0),
         koop_won = round(p_koop * tot_won / 100, 0),
         huur_won = round(p_huur * tot_won / 100, 0)) %>% 
  select(-starts_with("p_"))

df_won_long <- df_cbs %>% ## lang DF woningtypen, auto, stedelijkheid per buurt
  filter(SoortRegio == "Buurt") %>%
  select(Codering, jaar, Woningvoorraad, 
         p_Koopwoningen, p_HuurwoningenTotaal,
         p_TussenwoningEenGezins, p_HoekwoningEenGezins,
         p_TweeOnderEenKapWoningEenGezins,
         p_VrijstaandeWoningEenGezins, p_Meergezinswoning) %>%
  rename(code = Codering, 
         tot = Woningvoorraad,
         p_app = p_Meergezinswoning,
         p_tus = p_TussenwoningEenGezins,
         p_hoek = p_HoekwoningEenGezins,
         p_tkap = p_TweeOnderEenKapWoningEenGezins,
         p_vrij = p_VrijstaandeWoningEenGezins,
         p_koop = p_Koopwoningen,
         p_huur = p_HuurwoningenTotaal) %>% 
  mutate(app = round(p_app * tot / 100, 0),
         tus = round(p_tus * tot / 100, 0),
         hoek = round(p_hoek * tot / 100, 0),
         tkap = round(p_tkap * tot / 100, 0),
         vrij = round(p_vrij * tot / 100, 0),
         koop = round(p_koop * tot / 100, 0),
         huur = round(p_huur * tot / 100, 0)) %>% 
  select(-starts_with("p_")) %>% 
  left_join(df_lut[,c(5:6)], by = c("code" = "BU_CODE")) %>%
  rename(buurt = BU_NAAM) %>%
  select(buurt, code, jaar, everything())

## -- step 2. Combineren t.b.v. Reasearch_WBO ---------------------------------
## -- step 2a. Combine & Write Energieverbruik --------------------------------
df_ewt_long <- df_elc_long %>%                  ## combine gas en electriciteit
  bind_rows(df_gas_long)
write_rds(df_ewt_long, file = fpath_cln_ewt)

## -- step 2b. Write Aantallen woningen per woningtype ------------------------
write_rds(df_wtb, file = fpath_cln_wtb)

## -- step 2c. Combine & Write wide dataframes --------------------------------
df_tot_wide <- df_elc_wide %>% 
  left_join(df_gas_wide, by = c("Codering", "SoortRegio", "jaar")) %>% 
  left_join(df_won_wide, by = c("Codering", "SoortRegio", "jaar"))

write_rds(df_tot_wide, file = fpath_cln_onz)

## -- step 3. Scenario's, Bouwplannen, Warmtevisie ----------------------------
df_tvw <- read_excel(fpath_raw_lut, sheet = "warmte")  ## transitievisie warmte
df_bwp <- read_excel(fpath_raw_lut, sheet = "bouwplan") %>% 
  filter(naam != "TOTAAL")               ## ruimtelijke ontwikkelstrategie 2024

# df_bwp_wide <- df_bwp %>% 
#   rename(strat = won_plan,
#          nodig = won_nodig) %>%
#   pivot_wider(id_cols = c(code, naam),
#               names_from = plan,
#               values_from = c(strat, nodig)) %>% 
#   select(-strat_NA, -nodig_NA)

df_bwp_long <- df_bwp %>% 
  group_by(code, naam) %>%
  summarise(strat = sum(won_plan, na.rm = TRUE),
            nodig = sum(won_nodig, na.rm = TRUE))

df_tot_2ba <- df_tot_wide %>% 
  filter(SoortRegio == "Buurt",
         jaar == "2024") %>%
  select(Codering, tot_ev, tot_won) %>%
  left_join(df_lut[,c(5:6)], by = c("Codering" = "BU_CODE")) %>%
  rename(code = Codering,
         naam = BU_NAAM,
         woning2024 = tot_won,
         elek2024 = tot_ev)
  
sf_scen_nw <- sf_brt %>% 
  st_transform(4326) %>% 
  rename(code = statcode,
         naam = statnaam) %>% 
  select(-rubriek) %>%
  mutate(area = st_area(geometry)) %>%
  left_join(df_tvw) %>% 
  left_join(df_bwp_long) %>%
  left_join(df_tot_2ba) %>%
  rename(cbs = code, brt_naam = naam) %>% 
  rename(strat_extrawoning = strat,
         need_extrawoning = nodig,
         warmtevisie = warmte_srt,
         focusnaam = focusgebied) %>% 
  select(cbs, brt_naam, area, focusnaam, woning2024, elek2024,
         warmtevisie, strat_extrawoning, need_extrawoning, geometry)

write_rds(sf_scen_nw, file =fpath_cln_scen)

## -------------------------------------------------------------------------- #
## -- end of script -----------------------------------------------------------
## -------------------------------------------------------------------------- #
