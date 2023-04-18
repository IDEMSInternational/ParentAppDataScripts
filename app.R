library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly)
library(shinydashboard)
library(httr)
library(jsonlite)
library(here)     
library(tidyverse)
library(yesno)
library(gt)
library(readxl)
library(postgresr)
options(dplyr.summarise.inform = FALSE)
country <- "Tanzania" # Tanzania, all
study <- "Optimisation"
source("config/credentials_file.R")
source(here("Metabase Functions.R"))
source(here("Metabase ShinyApp 2.R"))
parentapp_shiny(country = country, study = study)

# try shinyapp 2 vs shinyapp - same time.
# try shinyapp 2 without the other tabs start ui stuff vs shinyapp 2


# on second tab, for "3", not "4":
# `mutate_all()` ignored the following grouping variables:
#* Columns `Support`, `Skin`
#i Use `mutate_at(df, vars(-group_cols()), myoperation)` to silence the message.

# on third tab - we run "3" twice (3, 3, 4)
# third tab - relax - 3, 4, and then a warning for shape palette. Then we keep running 4, again and again and again. And again. 
# re-run 3, 4 for each tab. Is this ok? I think so. 

