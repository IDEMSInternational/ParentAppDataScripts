# run libraries
library(tidyverse)
library(here)
library(yesno)
library(gt)

#' Interaction with chatbot
#Source the personal setup for data
source(here("config/Personal Setup.R"))

#Get data from excel
UIC.Tracker <- rio::import(file = here("data/UIC Tracker.xlsx"), which = "UIC Tracker 211014")

# Reading in Data ------------------------------------------

#Get the List of PLH Tables and data from server
get_metabase_data <- function(site = plh_con, name = "app_users"){   #name = "app_users", "app_notification_interaction"
  plh_tables <- dbListTables(site)
  df <- dbReadTable(conn = site,
                    name = name)
  return(df)
}

get_nf_data <- function(site = plh_con){
  df <- get_metabase_data(site = site, name = "app_notification_interaction")
  appdata_df <- list()
  
    for (i in 1:nrow(df)) {
      if (!is.na(df$notification_meta[i])){
        appdata_df[[i]] <- data.frame(jsonlite::fromJSON(df$notification_meta[i]))
      } else {
        appdata_df[[i]] <- data.frame(i)
      }
    }
  # combine the list into a data frame 
  appdata <- plyr::ldply(appdata_df)
  
  # bind into main data frame
  plhdata <- dplyr::bind_cols(df, appdata) %>% dplyr::select(-c("i"))
  
  return(plhdata)
}

get_user_data <- function(site = plh_con, date_from, date_to = NULL, format_date = "%Y-%m-%d", tzone_date = "UTC", include_UIC_data = TRUE, merge_check = TRUE, UIC_Tracker = UIC.Tracker, join_UIC = "UIC", max_dist = 5){ # ideally would have flatten = FALSE in there, but seems that this isn't an option from metabase.
  df <- get_metabase_data(site = site, name = "app_users")
  
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
        warning("merging in fuzzy matches:\n", paste0(capture.output(plhdata_org_fuzzy_comp %>% filter(complete.cases(app_user_id))), collapse = "\n"))
        return_data <- plhdata_org_fuzzy
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
#dbWriteTable(parent_app_con, "Cleaned PLH data", select(plhdata_org_clean,!(contact_fields)), overwrite=TRUE)

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

user_id_print <- function(data = plhdata_org, field, group_by = plhdata_org_clean$Org) {
  plhdata_org_list <- plhdata_org %>%
    select(c('app_user_id', rp.contact.field.parent_point_count_relax, Org)) %>%
    arrange(Org)
  return(plhdata_org_list)
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

# same function used in parent text
summary_calculation <- function(data = plhdata_org_clean, factors, columns_to_summarise, summaries = c("frequencies", "mean"),
                                include_country_margins, country_factor, together = FALSE, include_margins = FALSE){
  
  summaries <- match.arg(summaries)
  if (summaries == "frequencies"){
    summary_output <- data %>%
      group_by(across(c({{ columns_to_summarise }}, {{ factors }})), .drop = FALSE) %>%
      summarise(n = n())
    
    if (include_margins){
      cts_margin <- data %>%
        group_by(across(c({{ columns_to_summarise }})), .drop = FALSE) %>%
        summarise(n = n())
      
      ftr_margin <- data %>%
        group_by(across(c({{ factors }})), .drop = FALSE) %>%
        summarise(n = n())
      
      corner_margin <- data %>%
        summarise(n = n())
      
      if (include_country_margins){
        summary_output_country <- data %>%
          group_by(across(c({{ columns_to_summarise }}, {{ country_factor }})), .drop = FALSE) %>%
          summarise(n = n())
        corner_margin_country <- data %>%
          group_by(across(c({{ country_factor }})), .drop = FALSE) %>%
          summarise(n = n())
        names(summary_output_country)[length(summary_output_country)-1] <- names(summary_output)[length(summary_output)-1]
        names(corner_margin_country)[length(corner_margin_country)-1] <- names(summary_output)[length(summary_output)-1]
        #summary_output_country <- dplyr::rename(summary_output_country, Org = {{ country_factor }}) #"{{ factors }}" := {{ country_factor }})
        #corner_margin_country <- dplyr::rename(corner_margin_country, Org = {{ country_factor }}) #"{{ factors }}" := {{ country_factor }})
      } else {
        summary_output_country <- NULL
        corner_margin_country <- NULL
      }
      
      summary_output <- bind_rows(summary_output, cts_margin, ftr_margin, corner_margin, summary_output_country, corner_margin_country, .id = "id")

      summary_output <- summary_output %>%
        ungroup() %>%
        mutate(across({{ factors }}, as.character)) %>%
        mutate(across({{ factors }}, ~ifelse(id %in% c(2, 4), "Total", .x))) %>%
        mutate(across({{ columns_to_summarise }}, ~ifelse(id %in% c(3, 4, 6), "Total", .x)))
      
      summary_output <- summary_output %>%
        mutate(across({{ factors }}, ~fct_relevel(.x, "Total", after = Inf))) %>%
        mutate(across({{ columns_to_summarise }}, ~fct_relevel(.x, "Total", after = Inf))) %>%
        select(-c("id"))
    }
  } else {
    summary_output <- data %>%
      group_by(across({{ factors }}), .drop = FALSE) %>%
      #mutate(across({{ columns_to_summarise }}, ~as.numeric(.))) %>%
      summarise(across({{ columns_to_summarise }}, ~mean(.x, na.rm = TRUE)))
    
    if (include_margins){
      corner_margin <- data %>%
        summarise(across(c({{ columns_to_summarise }}), ~mean(.x, na.rm  = TRUE)))
      
      summary_output <- bind_rows(summary_output, corner_margin, .id = "id")
      
      summary_output <- summary_output %>%
        ungroup() %>%
        mutate(across({{ factors }}, as.character)) %>%
        mutate(across({{ factors }}, ~ifelse(id == 2, "Total", .x)))
      
      summary_output <- summary_output %>%
        mutate(across({{ factors }}, ~fct_relevel(.x, "Total", after = Inf))) %>%
        select(-c("id"))
    }
    
  }
  return(summary_output)
}

summary_table <- function(data = plhdata_org_clean, factors = Org, columns_to_summarise, summaries = c("frequencies", "mean"),
                          replace = "rp.contact.field.", include_margins = FALSE, include_country_margins = TRUE, 
                          country_factor = "country", wider_table = TRUE,
                          display_table = FALSE, naming_convention = TRUE, include_percentages = FALSE,
                          together = TRUE){
  
  summaries <- match.arg(summaries)
  
  return_table <- summary_calculation(data = data,
                                      factors = c({{ factors }}),
                                      columns_to_summarise = c({{ columns_to_summarise }}),
                                      include_margins = include_margins,
                                      include_country_margins = include_country_margins,
                                      country_factor = country_factor,
                                      summaries = summaries,
                                      together = together)
  
  return_table_names <- naming_conventions(colnames(return_table), replace = replace)
  if (summaries == "mean"){
    if (naming_convention){
      colnames(return_table) <- naming_conventions(colnames(return_table), replace = replace)
    }
  }
  if (display_table){
    if (summaries == "frequencies"){
      return_table <- return_table %>% pivot_wider(id_cols = {{ factors }}, names_from =  {{ columns_to_summarise }}, values_from = n)
    }
    
    return_table <- gt(as_tibble(return_table)) %>%
      tab_header(
        title = paste(return_table_names[1], "by", return_table_names[2])  # fix up. 
      ) %>%
      tab_style(locations = list(cells_body(columns = 1)),
                style = list(cell_borders(
                  sides = "right",
                  color = "black",
                  weight = px(2)),
                  cell_text(weight = "bold"))) %>%
      tab_style(locations = list(cells_column_labels(columns = gt::everything())),
                style = list(cell_borders( 
                  sides = "bottom",
                  color = "black",
                  weight = px(2)),
                  cell_text(weight = "bold")))
    #if (summaries == "mean"){
    #  names(return_table$`_data`) <- naming_conventions(names(return_table$`_data`), replace = replace)
    #}
  } else {
    if (wider_table & summaries == "frequencies"){
      return_table <- return_table %>% pivot_wider(id_cols = {{ factors }}, names_from =  {{ columns_to_summarise }}, values_from = n, names_prefix = "")
    }
  }
  return(return_table)
}

summary_plot <- function(data = plhdata_org_clean, columns_to_summarise, naming_convention = TRUE, replace = "rp.contact.field.",
                         plot_type = c("histogram", "boxplot")) {	
  plot_type <- match.arg(plot_type)
  x_axis_label = naming_conventions(colnames(data%>%select(.data[[columns_to_summarise]])), replace = replace)	
  
  return_plot <- ggplot(data) +	
    viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +	
    labs(x = x_axis_label, y = "Count") +	
    theme_classic()	
  
  if(plot_type == "histogram"){
    return_plot <- return_plot + geom_histogram(data = data, aes(x = .data[[columns_to_summarise]]), stat = "count")
  } else {
    return_plot <- return_plot + geom_boxplot(data = data, aes(y = .data[[columns_to_summarise]]))
  }
  
  return(return_plot)	
}
