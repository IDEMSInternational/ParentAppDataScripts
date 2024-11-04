# Only run this the first time to download things:
devtools::install_github("IDEMSInternational/postgresr") # the most recent version
install.packages(c("jsonlite", "here", "ggplot2", "tibble", "stringr", "forcats",
                   "lubridate", "purrr", "tidyr", "dplyr", "gt", "readxl"))
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
country <- "Tanzania"

setwd("") # In here set your working directory, e.g., might be "C:/users/esmee/IDEMS/WASH_app"
# If you store all your files in here, then there shouldn't be a problem!

study <- "WASH" # Change to RCT if you want RCT data :)
source("Metabase Functions.R")
if (study == "WASH"){
  source("setup_WASH.R")
} else {
  source("setup_RCT.R")
}

plhdata_org_clean$rp.contact.field.user_name <- NULL
plhdata_org_clean$contact_fields <- NULL

writexl::write_xlsx(plhdata_org_clean, paste0("PLHData_", study,".xlsx"))