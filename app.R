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
#options(dplyr.summarise.inform = FALSE)
country <- "South Africa" # Tanzania, all
source(here("Metabase Functions.R"))
source(here("Metabase ShinyApp.R"))
parentapp_shiny(country = country)

#rsconnect::configureApp("SouthAfrica", size="xlarge", account = "parentapp")
#rsconnect::configureApp("Tanzania", size="xlarge", account = "parentapp")


#rsconnect::showLogs(appName  = "SouthAfrica", account = "parentapp")

# TODO: getting errors with merging in a file or something.
# Org is being renamed. Idk why. 
# Oh how about I rename Org everywhere to "Organisation"