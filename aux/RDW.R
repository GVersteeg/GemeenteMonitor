## url endpoint RDW data (kenteken. voertuigklasse)
rdw_url_kl <- "https://opendata.rdw.nl/api/v3/views/kmfi-hrps/rows.json?accessType=DOWNLOAD"
rdw_url_bs <- "https://opendata.rdw.nl/api/v3/views/8ys7-d773/query.json"

rdw_url_bs <- "https://opendata.rdw.nl/api/v3/views/m9d7-ebf2/query.json"

## read API
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(janitor)

rdw_kl <- fromJSON(rdw_url_kl)
rdw_bs <- fromJSON(rdw_url_bs)


## https://opendata.rdw.nl/api/v3/views/m9d7-ebf2/query.json?pageNumber=1&pageSize=10&app_token=$YOUR_APP_TOKEN

## Install the required package with:
## install.packages("RSocrata")

library("RSocrata")

df <- read.socrata(
  "https://opendata.rdw.nl/api/v3/views/m9d7-ebf2/query.json",
  app_token = "YOURAPPTOKENHERE",
  email     = "user@example.com",
  password  = "fakepassword"
)




