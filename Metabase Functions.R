# run libraries
library(tidyverse)
library(here)
library(yesno)
library(gt)

#' Interaction with chatbot

#Source the personal setup for data
source(here("Personal Setup.R"))

#Get data from excel
UIC.Tracker <- rio::import(file = here("UIC Tracker.xlsx"), which = "UIC Tracker 211014")

# Reading in Data ------------------------------------------

#Get the List of PLH Tables and data from server
get_metabase_data <- function(site = plh_con){ 
  plh_tables <- dbListTables(site)
  df <- dbReadTable(conn = site,
                    name = plh_tables[2])
  return(df)
}

get_user_data <- function(site = plh_con, date_from, date_to = NULL, format_date = "%Y-%m-%d", tzone_date = "UTC", include_UIC_data = TRUE, merge_check = TRUE, UIC_Tracker = UIC.Tracker, join_UIC = "UIC", max_dist = 5){ # ideally would have flatten = FALSE in there, but seems that this isn't an option from metabase.
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

# Write back to metabase - TODO: make into function?
dbWriteTable(parent_app_con, "Cleaned PLH data", select(plhdata_org_clean,!(contact_fields)), overwrite=TRUE)


naming_conventions <- function(x, replace, replace_after) {
  if (!missing(replace)){
    x <- gsub(paste("^.*?", replace, ".*", sep = ""), "", x)
  }
  if (!missing(replace_after)){
    x <- gsub(paste(replace_after, "$", sep = ""), "", x)
  }
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x <- gsub("_", " ", x)
  x
}

# same function used in parent text
summary_PT <- function(data = df, summary_var, denominator = NULL, denominator_level = "Yes", together = FALSE, naming_convention = FALSE, replace = "rp.contact.field.w_"){
  
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
      colnames(summary_perc) <- naming_conventions(colnames(summary_perc), replace = replace)
    }
    
    return(summary_perc)
  } else {
    summary_n <- data %>%
      group_by(across({{ summary_var }}), .drop = FALSE) %>%
      summarise(Count = n(),
                     perc = n()/nrow(.) * 100)
    
    if (together == TRUE){
      colnames(summary_n)[length(summary_n)-1] <- "n"
      colnames(summary_n)[length(summary_n)] <- "perc"
      summary_n <- summary_n %>%
        mutate("Count (%)" := str_c(`n`, ' (', round(`perc`, 2), ")")) %>%
        dplyr::select(-c(n, perc))
    }
    
    if (naming_convention == TRUE){
      colnames(summary_n) <- naming_conventions(colnames(summary_n), replace = replace)
    }
    
    return(summary_n)
  }
}

multiple_summary_PT <- function(data = plhdata_org_clean, by = Org, summary_var, denominator = Org, together = TRUE, naming_convention = TRUE,
                                replace = "rp.contact.field."){
  attach(data, warn.conflicts = FALSE)
  var_by_Org <- NULL
  for (i in levels(Org)){
    var_by_Org[[i]] <- summary_PT(data = data, summary_var = {{ summary_var }}, denominator = {{ denominator }}, denominator_level = i,
                                  together = together, naming_convention = naming_convention,
                                  replace = replace)
    #plhdata_org_clean %>% filter(Org == i) %>% select('app_user_id', "rp.contact.field.user_var")
      }
  names(var_by_Org) <- levels(Org)
  return(var_by_Org)
  detach(data)
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

# a two-way table
two_way_table <- function(data = plhdata_org_clean, column_var = Org, row_var, replace = "rp.contact.field.w_"){
  
  summary_n <- summary_PT(data = data,
                            summary_var = c({{ column_var }}, {{ row_var }}),
                            naming_convention = TRUE,
                            replace = replace)
    
    # TODO: add in mmtable2 into the function. Can't do this yet because of issues
    #summary_table <- (mmtable(summary_n, cells = Count) +
    #         header_left_top(summary_n[1]) +
    #         header_top_left(summary_n[2]))
    
    summary_n_wider <- summary_n %>% pivot_wider(id_cols = names(summary_n)[1], names_from = names(summary_n)[2], values_from = Count)
    
    summary_table <- gt(as_tibble(summary_n_wider)) %>%
      tab_header(
        title = paste("Frequencies of `", names(summary_n)[2], "` by `", names(summary_n)[1], "`.")
      ) %>%
      tab_style(locations = list(cells_body(columns = 1)),
                style = list(cell_borders(
                  sides = "right",
                  color = "black",
                  weight = px(2)),
                  cell_text(weight = "bold"))) %>%
      # We use tab_style() to change style of cells
      # cell_borders() provides the formatting
      # locations tells it where
      # Add black borders to the bottom of all the column labels
      tab_style(locations = list(cells_column_labels(columns = gt::everything())),
                style = list(cell_borders(
                  sides = "bottom",
                  color = "black",
                  weight = px(2)),
                  cell_text(weight = "bold")))
  
    return(summary_table)
}

user_id_print <- function(data = plhdata_org, field, group_by = plhdata_org_clean$Org) {
  plhdata_org_list <- plhdata_org %>%
    select(c('app_user_id', rp.contact.field.parent_point_count_relax, Org)) %>%
    arrange(Org)
  return(plhdata_org_list)
}

