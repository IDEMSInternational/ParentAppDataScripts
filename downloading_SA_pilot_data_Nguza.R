library(jsonlite)
library(here)     
library(ggplot2)
library(rjson)
library(tibble)
library(stringr)
library(forcats)
library(lubridate)
library(purrr)
library(tidyr)
library(dplyr)
library(gt)
library(readxl)
library(openappr)
library(ggthemes)
library(RPostgres)
library(DBI)

#Connect to Database to get original data
plh_con <- DBI::dbConnect(drv = RPostgres::Postgres(),
                          dbname = <db name here>,
                          host = <input host here>,
                          port = <port here>,
                          user = <input user name here>,
                          password = <input password here>)

country <- "South Africa"
study <- "Pilot"
source(here("Metabase Functions.R"))

# calling different variables in
source(here("Metabase Pre-Shiny Setup.R"))

# Calling in the data itself
source(here("Metabase Analysis Setup - SA.R"))
# Note that you can look at "Metabase Analysis Setup.R" to see how we handled nuances in the Pilot and RCT data for TZ and SA

# You can look here for how the ShinyApp was set up
# "Metabase ShinyApp PAPP.R"
# "Metabase ShinyApp.R"