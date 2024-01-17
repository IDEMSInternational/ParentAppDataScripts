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
study <- "RCT" # Optimisation, RCT, WASH, Pilot
source("config/credentials_file.R")
source(here("Metabase Functions.R"))
source(here("Metabase Pre-Shiny Setup.R"))
source(here("Metabase Analysis Setup.R"))
study

# plhdata_org_clean1 <- plhdata_org_clean %>%
#   dplyr::select(app_user_id, ClusterName, RCT_access=rp.contact.field.post_rct_access)


# Read in our onboarding data and filter to the relevant clusters for today
onboarding_completion <- readxl::read_excel("data/onboarding_completion.xlsx")
onboarding_completion <- onboarding_completion %>%
  mutate(Group = ifelse(Group == "ParentApp", "RCT", "WASH"))
onboarding_completion <- onboarding_completion %>%
  dplyr::filter(Group == study)
onboarding_completion_today <- onboarding_completion %>%
  filter(as.Date(`Data snapshot 1`) == Sys.Date())

# What are todays clusters?
if (length(onboarding_completion_today) >= 1){
  print(onboarding_completion_today$ClusterName)
}

# Can we match the onboarding clustername with one in our data?
onboarding_completion_today$ClusterName <- toupper(onboarding_completion_today$ClusterName)
plhdata_org_clean$ClusterName <- toupper(plhdata_org_clean$ClusterName)

onboarding_completion_today$ClusterName
unique(plhdata_org_clean$ClusterName)

# Nyakabungo C

ClusterName <- "MAHINA RELINI" #onboarding_completion_today$ClusterName
if (length(onboarding_completion_today$ClusterName) == 1){
  ClusterName <- onboarding_completion_today$ClusterName
} else {
  stop("There's more than one cluster so check")
}


#"KILIMAHEWA B" "KIYUNGI"  

# filter to that one
plhdata_org_clean_today <- plhdata_org_clean %>%
  filter(ClusterName == onboarding_completion_today$ClusterName) #onboarding_completion_today$ClusterName)
#filter(ClusterName == "MAHINA RELINI")


if (length(onboarding_completion_today)>=1){
  if (nrow(plhdata_org_clean_today) < 10){
    stop("Check ClusterName")
  }
}
# MNYAPALA vs MNYAMPALA
# ClusterName
# for each cluster name:
plhdata_org_clean_today <- plhdata_org_clean_today %>%
  dplyr::select(-c("rp.contact.field.user_name"))
plhdata_org_clean_today$contact_fields <- NULL

plhdata_org_clean_today$ClusterName
ClusterName <- plhdata_org_clean_today$ClusterName
#plhdata_org_clean_today1 <- plhdata_org_clean_today


writexl::write_xlsx(plhdata_org_clean_today, path = paste0(study, "_", ClusterName, "_snapshot1.xlsx"))

# 
# head(plhdata_org_clean_today)
# writexl::write_xlsx(plhdata_org_clean_MB, path = "WASH_MLIMANIB_snapshot1.xlsx")
# 

#######################################################################################


# snapshot2

# Read in our onboarding data and filter to the relevant clusters for today
onboarding_completion <- readxl::read_excel("data/onboarding_completion.xlsx")
onboarding_completion <- onboarding_completion %>%
  mutate(Group = ifelse(Group == "ParentApp", "RCT", "WASH"))
onboarding_completion <- onboarding_completion %>%
  dplyr::filter(Group == study)


onboarding_completion_today <- onboarding_completion %>%
  filter(ClusterName == "Kanyerere")

# What are todays clusters?
if (length(onboarding_completion_today) >= 1){
  print(onboarding_completion_today$ClusterName)
}

# Can we match the onboarding clustername with one in our data?
onboarding_completion_today$ClusterName <- toupper(onboarding_completion_today$ClusterName)
plhdata_org_clean$ClusterName <- toupper(plhdata_org_clean$ClusterName)

onboarding_completion_today$ClusterName
unique(plhdata_org_clean$ClusterName)

if (length(onboarding_completion_today$ClusterName) == 1){
  ClusterName <- onboarding_completion_today$ClusterName
} else {
  stop("There's more than one cluster so check")
}

#"KILIMAHEWA B" "KIYUNGI"  


#cluster_name <- "MISSION"
# "NHC" "MAJENGO MAPYA" (44?)

# filter to that one
plhdata_org_clean_today <- plhdata_org_clean %>%
  #filter(ClusterName == onboarding_completion_today$ClusterName) #onboarding_completion_today$ClusterName)
  filter(ClusterName == onboarding_completion_today$ClusterName) #onboarding_completion_today$ClusterName)

if (length(onboarding_completion_today)>=1){
  if (nrow(plhdata_org_clean_today) < 10){
    stop("Check ClusterName")
  }
}
# MNYAPALA vs MNYAMPALA
# ClusterName
# for each cluster name:
plhdata_org_clean_today <- plhdata_org_clean_today %>%
  dplyr::select(-c("rp.contact.field.user_name"))
plhdata_org_clean_today$contact_fields <- NULL

plhdata_org_clean_today$ClusterName
ClusterName <- plhdata_org_clean_today$ClusterName

writexl::write_xlsx(plhdata_org_clean_today, path = paste0(study, "_", ClusterName, "_snapshot2.xlsx"))


# plhdata_org_clean_MB <- plhdata_org_clean_MB[1:29,]
# plhdata_org_clean_today$rp.contact.field.when_to_wash_your_hands_completed <- TRUE
# #plhdata_org_clean_today$rp.contact.field.max_days_between_app_launches <- as.numeric(plhdata_org_clean_today$rp.contact.field.max_days_between_app_launches)
# #plhdata_org_clean_today$rp.contact.field._server_sync_latest <- as_datetime(plhdata_org_clean_today$rp.contact.field._server_sync_latest)
# plhdata_org_clean_MB <- bind_rows(plhdata_org_clean_MB, plhdata_org_clean_today)
# 
# 
# # plhdata_org_clean_today <- plhdata_org_clean_today %>%
# #   mutate(across(is_character, ~ifelse(.x == "true", TRUE,
# #                             ifelse(.x == "false", FALSE,
# #                                    .x))))
# 
# plhdata_org_clean_MB$rp.contact.field.splash_screens_viewed
# plhdata_org_clean_today$rp.contact.field.when_to_wash_your_hands_completed
# 
# # for each cluster name, save a separate file:
# 
# getwd()
# 
# # plhdata_org_clean_G <- plhdata_org_clean %>% filter(ClusterName == "GEDELI")
# # head(plhdata_org_clean_G)
# # nrow(plhdata_org_clean_G)
# # #saveRDS(plhdata_org_clean_G, file = "GEDELI_snapshot.RDS")
