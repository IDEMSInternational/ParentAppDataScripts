devtools::install_github("IDEMSInternational/postgresr")

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
library(ggthemes)

#  download EFM app data from Metabase as an RDS file?
source(here("config/Personal Setup.R"))

plhdata_org <- get_user_data(filter_variable = "app_deployment_name",
                             filter_variable_value = "early_family_math",
                             site = plh_con, merge_check = FALSE, filter = TRUE)
names(plhdata_org) <- gsub(x = names(plhdata_org), pattern = "\\-", replacement = ".")  
View(plhdata_org)


# COUNTING the number of clicks --------------------------------------------------------------------
### Creating counts 
x <- c("2023-11-24T09:28:59 ; 2023-11-24T09:30:22", NA, "2023-11-27T14:45:52")

# Function to count dates in each element of the vector
count_dates <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else {
    dates <- unlist(strsplit(x, ";"))
    return(length(dates))
  }
}

# Apply the function to each element of the vector

# METHOD 1: sapply
plhdata_org1 <- plhdata_org %>%
  mutate(count_activities_button_click_history = 
           sapply(rp.contact.field.activities_button_click_history, count_dates))
plhdata_org1 %>% dplyr::select(count_activities_button_click_history, rp.contact.field.activities_button_click_history) %>% View()

# METHOD 2: purrr
plhdata_org2 <- plhdata_org %>%
  mutate(count_activities_button_click_history_purrr = 
           purrr::map_dbl(.x = rp.contact.field.activities_button_click_history,
                          .f = ~ count_dates(.x)))

plhdata_org2 %>% dplyr::select(count_activities_button_click_history, count_activities_button_click_history_purrr, rp.contact.field.activities_button_click_history) %>% View()


# METHOD 3: multiple columns
plhdata_org_3 <- plhdata_org %>%
  mutate(across(ends_with("_click_history"), # put in here a set of variables.
                .names = "{.col}_count",     # rename the new variables
                ~ sapply(.x, count_dates)))  # apply count_dates to them.
plhdata_org_3 %>% View()






