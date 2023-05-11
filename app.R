library(shiny)
#library(shinythemes)
library(shinyjs)
library(plotly)
library(shinydashboard)
library(jsonlite)
library(here)     
library(ggplot2)
library(tibble)
library(stringr)
library(forcats)
library(lubridate)
library(purrr)
library(tidyr)
library(dplyr)
library(gt)
library(readxl)
library(postgresr)
options(dplyr.summarise.inform = FALSE)
options(dplyr.warning.inform = FALSE)
country <- "Tanzania" # Tanzania, all
study <- "Pilot" # Optimisation, RCT
source("config/credentials_file.R")
source(here("Metabase Functions.R"))
source(here("Metabase ShinyApp.R"))
source(here("Metabase Pre-Shiny Setup.R"))
parentapp_shiny(country = country, study = study)

# Please use `all_of(var)` (or `any_of(var)`) instead of `.data[[var]]`

# move to be like shiny 2 with the easy to read function

# TODO: run shiny app. Look at output prints.
# See time difference from nothing to "first"
# then "first" to "last"
# then for the observe() times (17 secs?)





# 2: 14.79


# try shinyapp 2 without the other tabs start ui stuff vs shinyapp 2
# 26 secs vs 10 secs


# on second tab, for "3", not "4":
# `mutate_all()` ignored the following grouping variables:
#* Columns `Support`, `Skin`
#i Use `mutate_at(df, vars(-group_cols()), myoperation)` to silence the message.

# on third tab - we run "3" twice (3, 3, 4)
# third tab - relax - 3, 4, and then a warning for shape palette. Then we keep running 4, again and again and again. And again. 
# re-run 3, 4 for each tab. Is this ok? I think so. 

