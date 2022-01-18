# run libraries
library(tidyverse)
library(here)
library(mmtable2)
library(yesno)

#' Interaction with chatbot
#' 1. Defining package environment -------------------------------------------
#' Sorting set and get calls for: key, site
#' 
#' 
#' Define package environment
setwd("C:/Users/lzc1n17/OneDrive - University of Southampton/PhD/IDEMS/ParentApp")

#Source the personal setup for data
source("Personal Setup.R")
source(here("Personal Setup.R"))

#Get data from excel
UIC.Tracker <- rio::import(file = "UIC Tracker.xlsx", which = "UIC Tracker 211014")
UIC.Tracker <- rio::import(file = here("UIC Tracker.xlsx"), which = "UIC Tracker 211014")

# Reading in Data ------------------------------------------

#Get the List of PLH Tables and data from server
get_metabase_data <- function(site = plh_con){  # can add in date_from, date_to and filter. 
  plh_tables <- dbListTables(site)
  df <- dbReadTable(conn = site,
                    name = plh_tables[2])
  return(df)
}

get_user_data <- function(site = plh_con, date_from = NULL, date_to = NULL, format_date = "%Y-%m-%d", tzone_date = "UTC", include_UIC_data = TRUE, merge_check = TRUE, UIC_Tracker = UIC.Tracker, join_UIC = "UIC", max_dist = 5){ # ideally would have flatten = FALSE in there, but seems that this isn't an option from metabase.
  df <- get_metabase_data(site = site)
  
  # create empty list to store the data frames
  appdata_df <- list()
  
  # for each row of the data, get the contact_fields
  for (i in 1:nrow(df)) {
    appdata_df[[i]] <- data.frame(jsonlite::fromJSON(df$contact_fields[i]))
  }
  
  # combine the list into a data frame 
  appdata <- plyr::ldply(appdata_df)
  
  # bind into main data frame
  plhdata <- dplyr::bind_cols(df, appdata)
  
  # add UIC data?
  if (include_UIC_data){
    plhdata_org_fuzzy <- fuzzyjoin::stringdist_full_join(x = plhdata, y = UIC_Tracker, by = c("app_user_id" = join_UIC[1]), max_dist = max_dist)
    
    #check the fuzzy matches 
    plhdata_org_fuzzy_comp <- plhdata_org_fuzzy %>% 
      filter(!is.na(plhdata_org_fuzzy$UIC)) %>% 
      filter(app_user_id!=UIC | is.na(app_user_id)) %>% 
      select(app_user_id, UIC)
    
    if (merge_check){
      if (yesno::yesno2("Fuzzy matches are:\n",
                 paste0(capture.output(plhdata_org_fuzzy_comp), collapse = "\n"),
                 "\nDo you want to merge these changes in?") == TRUE){
       return_data <- plhdata_org_fuzzy
      } else {
        return_data <- dplyr::full_join(x=plhdata, y=UIC_Tracker, by=c("app_user_id" = join_UIC))
      }
    }else{
      return_data <- dplyr::full_join(x=plhdata, y=UIC_Tracker, by=c("app_user_id" = join_UIC))
    }
    
  } else {
    return_data <- plhdata
  }
  
  if (!missing(date_from)){
    return_data <- return_data %>% dplyr::filter(as.POSIXct(date_from, format = format_date, tzone = tzone_date) < as.POSIXct(return_data$createdAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  if (!missing(date_to)){
    return_data <- return_data %>% dplyr::filter(as.POSIXct(date_to, format = format_date, tzone = tzone_date) > as.POSIXct(return_data$createdAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  
  return(return_data)
}

# Write back to metabase
dbWriteTable(parent_app_con, "Cleaned PLH data", select(plhdata_org_clean,!(contact_fields)), overwrite=TRUE)


# Creating tables
summary_PT <- function(data, org_name, group_variable){
  plhdata_org_Filter <- dplyr::filter(data, Org == {{org_name}})

  return(plhdata_org_Filter %>%
           group_by(across({{ group_variable }})) %>%
           summarise(N = n(),
                     `Raw %` = n()/nrow(.)))
}

multiple_summary_PT <- function(data, group_variables){

  summary_table <- data %>%
    group_by(across({{ group_variables }})) %>%
    summarise(N = n(),
              `Raw %` = n()/nrow(.))
  
  summary_table <- pivot_longer(summary_table, cols = c(N, `Raw %`))

  return(summary_table)
  }

naming_conventions <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x <- gsub("_", " ", x)
  x
}

# same function used in parent text
summary_PT <- function(data = df, summary_var, denominator = NULL, denominator_level = "Yes", together = FALSE, naming_convention = FALSE){
  
  if (!missing(denominator)) {
    summary_perc <- data %>%
      dplyr::filter({{ denominator }} == denominator_level) %>%
      dplyr::group_by(dplyr::across({{ summary_var }}), .drop = FALSE) %>%
      dplyr::summarise("{{summary_var}}_n" := n(),
                "{{summary_var}}_perc" := n()/nrow(.) * 100)
    
    if (together == TRUE){
      colnames(summary_perc)[length(summary_perc)-1] <- "n"
      colnames(summary_perc)[length(summary_perc)] <- "perc"
      summary_perc <- summary_perc %>%
        mutate("Count (%)" := str_c(`n`, ' (', round(`perc`, 2), ")")) %>%
        dplyr::select(-c(n, perc))
    }
    
    if (naming_convention == TRUE){
      colnames(summary_perc) <- naming_conventions(colnames(summary_perc))
    }
    
    return(summary_perc)
  } else {
    summary_n <- data %>%
      group_by(across({{ summary_var }}), .drop = FALSE) %>%
      summarise("{{summary_var}}_n" := n())
    if (naming_convention == TRUE){
      colnames(summary_n) <- naming_conventions(colnames(summary_n))
    }
    return(summary_n)
  }
}

# not sure if you want this sort of function or not, and if so, what it should do. Will come back to.
get_app_user_IDs <- function(data = plhdata_org, factor_variable, factor_level, show_invalid = FALSE){
  
  data_filter <- data <- filter(across({{ factor_variable }}) == factor_level)
  
  # Show any app user ids that are invalid and do not come from the app.
  if (show_invalid){
    print(data_filter %>% filter(is.na(app_version)) %>% select(app_user_id))
  }
  
  # Create subsets of the data based on valid NONTOBEKOM app user ID's
  data_filter <- data_filter %>% filter(!is.na(app_version))
  
  # Show the summary of app versions
  #sjmisc::frq(x=plhdata_org_NONTOBEKO$'app_version', out="txt")
  
  # Show any app user ids that have only synced initial data.
  data_filter %>% filter(is.na(rp.contact.field.first_app_open)) %>% select('app_user_id')
}






#
