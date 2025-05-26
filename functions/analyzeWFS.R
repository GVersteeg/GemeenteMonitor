## FUNCTION "ANALYZE WFS"
## G.Versteeg (23 sep 2023)
##
## Function that analyzes a WFS Capabolities XML for:
##    - Feature Types (available layers)
##
## The various components are stored as RDS-files 
## in the directory given as the second paramter
##
## the call to the function: df_clean <- flatlist(path2XML, path2RDS)
##
## steps:
## 1. ## determine feature-types available in the WFS, store in ft.rds
##

## outside function
## dir2RDS <- "capabilities"
## xml_in <- xml_in

analyzeWFS <- function(xml_in) {
  ## -------------------------------------------------------------------------- #
  ## ----------- housekeeping ------------------------------------------------- #
  ## -------------------------------------------------------------------------- #
  ## loading libraries -------------------------------------------------------- #
  require(jsonlite)
  require(tidyverse)

  ## Logic featuretypes ------------------------------------------------------- #
  data <- xmlParse(xml_in)
  lst_getcap <- xmlToList(data)
  lst_ftl <- lst_getcap$FeatureTypeList
  
  ## start de bouw v.h. dataframe voor beschikbare FeatureTypes
  df_ftl <- data.frame(name = character(0),
                       title = character(0),
                       abstract = character(0),
                       keyword_1 = character(0),
                       keyword_2 = character(0),
                       def_crs = character(0),
                       bbox_lc = character(0),
                       bbox_uc = character(0),
                       nr_in_NL = integer(0),
                       nr_in_DH = integer(0),
                       stringsAsFactors = FALSE)
  index <- c(1:length(lst_ftl))
  curfeat <- NA
  
  ## loop voor het vullen van het dataframe voor beschikbare FeatureTypes
  for (i in seq_along(index)) {
    df_ftl[i,] <- NA
    curfeat <- lst_ftl[i]
    df_ftl$name[i] <- curfeat$FeatureType$Name
    df_ftl$title[i] <- curfeat$FeatureType$Title
    df_ftl$abstract[i] <- ifelse(is.null(curfeat$FeatureType$Abstract),"",
                                 curfeat$FeatureType$Abstract)
    keywords <- unlist(curfeat$FeatureType$Keywords)
    df_ftl$keyword_1[i] <- keywords[1]
    df_ftl$keyword_2[i] <- keywords[2]
    df_ftl$def_crs[i] <- curfeat$FeatureType$DefaultCRS
    df_ftl$bbox_lc[i] <- curfeat$FeatureType$WGS84BoundingBox$LowerCorner
    df_ftl$bbox_uc[i] <- curfeat$FeatureType$WGS84BoundingBox$UpperCorner
    # url_hits_NL <- str_replace(url_hits, "typeName=",
    #                            paste0("typeName=",df_ftl$name[i]))
    # xml_hits_NL <- getURL(url_hits_NL, .opts=opts)
    # xml_data <- xmlParse(xml_hits_NL)
    # lst_hits <- xmlToList(xml_data)
    # df_ftl$nr_in_NL[i] <- as.numeric(lst_hits[["numberMatched"]])
    # url_hits_DH <- paste0(url_hits_NL, req_bbox)
    # xml_hits_DH <- getURL(url_hits_DH, .opts=opts)
    # xml_data <- xmlParse(xml_hits_DH)
    # lst_hits <- xmlToList(xml_data)
    # df_ftl$nr_in_DH[i] <- as.numeric(lst_hits[["numberMatched"]])
  }
  
##  write_rds(df_ftl, fpath_tbl_ag)
  return(df_ftl)
}

## REMARKS
##
## --- en of function ------------------------------------------------------- #
