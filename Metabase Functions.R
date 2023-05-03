#' Interaction with chatbot
#Source the personal setup for data
source(here("config/Personal Setup.R"))

#Get data from excel
UIC.Tracker <- rio::import(file = here("data/UIC Tracker.xlsx"), which = "UIC Tracker 211014")
UIC_Tracker_Tanzania <- rio::import(file = here("data/UIC Tracker Tanzania.xlsx"))

# get_postgres_data <- function (site, name = c("app_users", "app_notification_interaction"), qry = NULL){
#   name <- match.arg(name)
#   if (is.null(qry)){
#     df <- DBI::dbReadTable(conn = site, name = name)
#   } else {
#     df <- DBI::dbGetQuery(site, qry)
#   }
#   return(df)
# }

# todo: get_nf_data - update function to have qry option


#######################################
# Specific to ParentApp --------------------------------------------------
#######################################
# if a variable is missing from the data, add it in to have NA all donw
add_na_variable <- function(data = plhdata_org_clean, variable){
  for (names in variable) {
    if (!names %in% colnames(data)) {
      data[, names] <- NA
      warning(paste(names, "does not exist. Adding NAs"))
    }
  }
  return(data)
}

notification_summary <- function(data = nf_data, factors){
  numerator <- data %>%
    filter(action_id == "tap") %>%
    group_by(across(all_of({{ factors }})), .drop = FALSE) %>%
    summarise(replied = n())
  denominator <- data %>%
    group_by(across(all_of({{ factors }})), .drop = FALSE) %>%
    summarise(received = n())
  notifications_perc <- full_join(numerator, denominator)
  return(notifications_perc)
}

version_variables_rename <- function(data = plhdata_org_clean, survey = "welcome", old_name, new_name, new_name_v = "v0.16.2", old_system_replacement = FALSE){
  col_name <- paste0("rp.contact.field.survey_", survey, "_", old_name)
  if (old_system_replacement){
    col_name_2 <- paste0("rp.contact.field.survey_", survey, "_", new_name, "_", new_name_v)
  } else {
    col_name_2 <- paste0("rp.contact.field.survey_", survey, "_", new_name)
  }
  data <- data %>%
    mutate({{ col_name }} := ifelse(!is.na(.data[[col_name_2]]),
                                    .data[[col_name_2]],
                                    .data[[col_name]]))
  return(data)
}

challenge_freq <- function(data = plhdata_org_clean, group_by = "Org", var, append_var) {
  plh_list <- data %>%
    add_na_variable(variable = var) %>%
    add_na_variable(variable = append_var) %>%
    mutate(plh_list = str_split(!!sym(var), pattern = ", ")) %>%
    mutate(plh_list1 = map2(plh_list, !!sym(append_var), ~c(.x, .y))) %>%
    select(plh_list1, {{ group_by }}) %>%
    unnest(plh_list1) %>%
    filter(!plh_list1 %in% c("other_challenge", "undefined", "null"), !is.na(plh_list1))%>%
    group_by(across(everything())) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = plh_list1, values_from = n, values_fill = 0) %>%
    ungroup()
  
  return(plh_list)
}

# challenge_freq <- function(data = plhdata_org_clean, group_by = "Org", var, append_var){
#   data <- add_na_variable(data = data, variable = var)
#   data <- add_na_variable(data = data, variable = append_var)
#   plh_list <- stringr::str_split(data[[var]], pattern = ", ")
#   plh_append <- data[[append_var]]
#   plh_Org <- data[[group_by]]
#   for (i in 1:length(plh_list)){
#     plh_list1 <- c(plh_list[[i]], plh_append[i])
#     plh_list2 <- as.character(plh_Org[i])
#     df <- data.frame(plh_list1, Org = plh_list2)
#     plh_list[[i]] <- df
#   }
#   plh_list <- purrr::map(.x = plh_list,
#                          .f = ~ unique(.x))
#   
#   plh_list <- plyr::ldply(plh_list)
#   plh_list <- plh_list %>% group_by(plh_list1, Org) %>% summarise(n())
#   
#   plh_list <- plh_list %>%
#     dplyr::filter(!plh_list1 %in% c("other_challenge", "undefined", "null")) %>%
#     dplyr::filter(!is.na(plh_list1)) %>%
#     pivot_wider(names_from = plh_list1, values_from = `n()`) %>%
#     mutate_all(~replace(., is.na(.), 0))
#   return(plh_list)
# }

survey_table <- function(data = plhdata_org_clean, metadata = r_variables_names, location_ID = "survey_past_week",
                         factors = "Org", include_margins = FALSE){
  data_to_tabulate <- metadata %>% filter(location_ID == {{ location_ID }})
  
  summary_table <- data %>%
    # take the data, and select the relevant variables
    dplyr::select(c(all_of(factors), country, app_user_id, data_to_tabulate$metabase_ID)) %>%
    tidyr::unite(col = "Org", {{ factors }}) %>%
    # rearrange the data frame
    pivot_longer(cols = -c(Org, country, app_user_id), names_to = "metabase_ID") %>%
    # join with our metadata on the variables
    merge(., data_to_tabulate) %>%
    group_by(Org, country, app_user_id, display_name) %>%
    #mutate(value = as.numeric(value)) %>%
    filter(complete.cases(value)) %>%
    # take the most recent value that is not NA
    summarise(value = last(value))
  
  summary_table_wider <- summary_table %>%
    # to the format for the summary_table function
    pivot_wider(id_cols = c(Org, country, app_user_id),
                names_from = display_name) %>%
    ungroup()
  
  summary_table_wider <- summary_table_wider %>%
    map(.x = unique(summary_table$display_name),
        .f = ~summary_table(data = summary_table_wider, factors = "Org", columns_to_summarise = .x, display = FALSE, include_margins = include_margins),
        id = .x)
  names(summary_table_wider) <- unique(summary_table$display_name)
  return(summary_table_wider)
}

plot_totals_function <- function(data = table_pp_relax_ws_totals(), factors){
  summary_workshop_long <- data %>%
    pivot_longer(cols = !factors) %>%
    mutate(name = fct_relevel(name, week_order)) %>%  # set the order of variables
    filter()
  
  if (country == "Tanzania"){
    if (study == "Optimisation"){
      summary_workshop_long <- summary_workshop_long %>%
        tidyr::unite(col = "Org", {{ factors }}) %>%
        filter(name != "Total")
    } else {
      summary_workshop_long <- rename(summary_workshop_long, Org = factors) %>% filter(name != "Total")
    }
  } else {
    summary_workshop_long <- summary_workshop_long %>% filter(name != "Total")
  }
  
  return(ggplot(summary_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
           geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
           geom_line() + labs(x = "Workshop week", y = "Number of points"))
}

#######################################
# Shiny / ParentApp functions --------------------------------------------------
#######################################
extract <- function(text) {
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  as.numeric(split)
}

checkbox_input <- function(inputId, country = country, study = study){
  if (country == "South Africa") {
    return(box(width = 6,
               checkboxGroupInput(inputId = paste0("Org", inputId),
                                  label = "Organisations to show:",
                                  choices = c("Amathuba" = "Amathuba",
                                              "Dlalanathi" = "Dlalanathi",
                                              "Joy" = "Joy",
                                              "Nontobeko" = "Nontobeko"),
                                  selected = c("Amathuba","Dlalanathi",
                                               "Joy","Nontobeko")
               )))
  } else if (country == "Tanzania"){
    if (study == "Pilot"){
      return(box(width = 6,
                 checkboxGroupInput(inputId = paste0("Org", inputId),
                                    label = "Site",
                                    c("Mwanza" = "Mwanza",
                                      "Mwanza 2" = "Mwanza 2",
                                      "Shinyanga" = "Shinyanga",
                                      "Unknown" = "Unknown"),
                                    selected = c("Mwanza", "Mwanza 2", "Shinyanga", "Unknown")
                 ),
                 actionButton("goButton", "Submit", class = "btn-success")))
    } else if (study == "Optimisation") {
      # return(box(checkboxInput(inputId = "chk_support",
      #                          label = "Group by support",
      #                          value = TRUE),
      #            uiOutput("opt_chk_support")))
      return(column(width = 12,
                    box(
                      checkboxInput(inputId = "select_cluster",
                                    label = "All clusters",
                                    value = TRUE),
                      textInput(inputId = "opt_cluster",
                                label = "Cluster",
                                placeholder = "Enter values separated by a comma..."),
                      checkboxGroupInput(inputId = "opt_support",
                                         label = "Support",
                                         c("Self-guided" = "Self-guided",
                                           "WhatsApp" = "WhatsApp"),
                                         selected = c("Self-guided", "WhatsApp")),
                      checkboxGroupInput(inputId = "opt_skin",
                                         label = "Skin type",
                                         c("Module" = "Module",
                                           "Workshop" = "Workshop"),
                                         selected = c("Module", "Workshop")),
                      checkboxGroupInput(inputId = "opt_diglit",
                                         label = "Digital literacy",
                                         c("On" = "On",
                                           "Off" = "Off"),
                                         selected = c("On", "Off")),
                      actionButton("goButton", "Submit", class = "btn-success"))))
    } else {
    }
  } else if (country == "all") {
    return(fluidRow(
      column(12, align = "centre",
             box(width = 4,
                 checkboxGroupInput(inputId = paste0("Ctry", inputId),
                                    label = "Countries to select:",
                                    choices = c("South Africa" = "plh_za",
                                                "Tanzania" = "plh_tz"),
                                    selected = c("plh_za","plh_tz"))),
             box(width = 6,
                 checkboxGroupInput(inputId = paste0("Org", inputId),
                                    label = "Organisations to show:",
                                    choices = c("SA: Amathuba" = "Amathuba",
                                                "SA: Dlalanathi" = "Dlalanathi",
                                                "SA: Joy" = "Joy",
                                                "SA: Nontobeko" = "Nontobeko", "TZ: ICS" ="Tanzania"),
                                    selected = c("Amathuba","Dlalanathi",
                                                 "Joy","Nontobeko", "Tanzania")
                 )))))
  } else {
  }
}

reactive_table <- function(data = plhdata_org_clean, country, inputId = "input$OrgDem"){
  # need to do input$ here, but then need reactive here so it knows where to find input$
  observe({
    if (country == "Tanzania"){
      plhdata_checkgroup <- data
    } else {
      plhdata_checkgroup <- data %>% filter(Org %in% c(input$OrgDem))
    }
    return(plhdata_checkgroup) 
  })
}

top_boxes <- function(country){
  if (country == "all"){
    fluidRow(
      shinydashboard::valueBoxOutput("myvaluebox1", width=2), 
      shinydashboard::valueBoxOutput("myvaluebox2", width=2),
      shinydashboard::valueBoxOutput("myvaluebox3", width=2),
      shinydashboard::valueBoxOutput("myvaluebox4", width=2),
      shinydashboard::valueBoxOutput("myvaluebox5", width=2),
      shinydashboard::valueBoxOutput("myvaluebox6", width=2)
    )
  } else if (country == "South Africa"){
    fluidRow(
      shinydashboard::valueBoxOutput("myvaluebox1", width=2), 
      shinydashboard::valueBoxOutput("myvaluebox2", width=2),
      shinydashboard::valueBoxOutput("myvaluebox3", width=2),
      shinydashboard::valueBoxOutput("myvaluebox4", width=2),
      shinydashboard::valueBoxOutput("myvaluebox5", width=2)
    )
  } else if (country == "Tanzania"){
    fluidRow(
      shinydashboard::valueBoxOutput("myvaluebox1", width=3), 
      shinydashboard::valueBoxOutput("myvaluebox2", width=3),
      shinydashboard::valueBoxOutput("myvaluebox3", width=3),
      shinydashboard::valueBoxOutput("myvaluebox4", width=3)
    )
  }
}

summary_table_base_build <- function(data = plhdata_org_clean,
                                     columns_to_summarise = data_baseline_survey,
                                     replace = "rp.contact.field.",
                                     replace_after = NULL,
                                     opt_factors = c("Support", "Skin", "Digital Literacy")){
  if (country == "Tanzania"){
    if (study == "Pilot"){
      return(multiple_table_output(data = data,
                                   columns_to_summarise = columns_to_summarise,
                                   replace = replace,
                                   replace_after = replace_after,
                                   factors = "PilotSite"))
    } else if (study == "Optimisation"){
      return(multiple_table_output(data = data,
                                   columns_to_summarise = columns_to_summarise,
                                   replace = replace,
                                   replace_after = replace_after,
                                   factors = opt_factors)) #))
    } else {
      return(multiple_table_output(data = data,
                                   columns_to_summarise = columns_to_summarise,
                                   replace = replace,
                                   replace_after = replace_after))
    }
  } else {
    # otherwise we have the different Organisations in the code anyway so it's nice and easy. 
    return(multiple_table_output(data = data,
                                 columns_to_summarise = columns_to_summarise,
                                 replace = replace,
                                 replace_after = replace_after))
  }
}

hp_mood_plot <- function(data, factors, manipulation = "longer", limits = c("Sad", "Ok", "Happy", "Unknown"),
                         xlab = "How did you find it?"){
  if (manipulation == "ldply"){
    plot_data <- plyr::ldply(data, `.id` = "name")
  } else if (manipulation == "longer"){
    if ("Total" %in% names(data)){
      plot_data <- data %>%
        dplyr::select(-Total) %>%
        pivot_longer(cols = !factors)
    } else {
      plot_data <- data %>%
        pivot_longer(cols = !factors)
    }
  } else {
    plot_data <- data
  }
  
  if (country == "Tanzania"){
    if (study == "Optimisation"){
      plot_data <- plot_data %>%
        tidyr::unite(col = "Org", {{ factors }})
    } else {
      plot_data <- plot_data %>% mutate(Org = PilotSite)
    }
  }
  
  plot <- ggplot(plot_data, aes(x = name, y = value, fill = Org))
  plot + geom_bar(stat = "identity", position = "dodge") +
    viridis::scale_fill_viridis(discrete = TRUE) +
    labs(x = xlab, y = "Frequency") +
    scale_x_discrete(guide = guide_axis(angle = 90),
                     limits = limits)
}

threshhold_function <- function(data, threshhold, columns = data_completion_level, sign = "gt"){
  
  # Check sign parameter
  if (!sign %in% c("gt", "lt")) {
    stop("sign must be one of 'gt' or 'lt'")
  }
  
  # Check if columns exist in data
  if (!all(columns %in% names(data))) {
    stop("Columns do not exist in data")
  }
  
  if (sign == "gt"){
    plhdata_org_clean_engagement <- data %>%
      mutate(self_care_started = ifelse(rp.contact.field.w_self_care_completion_level > threshhold, 1, 0),
             `1on1_started` = ifelse(rp.contact.field.w_1on1_completion_level > threshhold, 1, 0),
             praise_started = ifelse(rp.contact.field.w_praise_completion_level > threshhold, 1, 0),
             instruct_started = ifelse(rp.contact.field.w_instruct_completion_level > threshhold, 1, 0),
             stress_started = ifelse(rp.contact.field.w_stress_completion_level > threshhold, 1, 0),
             money_started = ifelse(rp.contact.field.w_money_completion_level > threshhold, 1, 0),
             rules_started = ifelse(rp.contact.field.w_rules_completion_level > threshhold, 1, 0),
             consequence_started = ifelse(rp.contact.field.w_consequence_completion_level > threshhold, 1, 0),
             solve_started = ifelse(rp.contact.field.w_solve_completion_level > threshhold, 1, 0),
             safe_started = ifelse(rp.contact.field.w_safe_completion_level > threshhold, 1, 0),
             crisis_started = ifelse(rp.contact.field.w_crisis_completion_level > threshhold, 1, 0),
             celebrate_started = ifelse(rp.contact.field.w_celebrate_completion_level > threshhold, 1, 0))
  } else {
    plhdata_org_clean_engagement <- data %>%
      mutate(self_care_started = ifelse(rp.contact.field.w_self_care_completion_level < threshhold, 1, 0),
             `1on1_started` = ifelse(rp.contact.field.w_1on1_completion_level < threshhold, 1, 0),
             praise_started = ifelse(rp.contact.field.w_praise_completion_level < threshhold, 1, 0),
             instruct_started = ifelse(rp.contact.field.w_instruct_completion_level < threshhold, 1, 0),
             stress_started = ifelse(rp.contact.field.w_stress_completion_level < threshhold, 1, 0),
             money_started = ifelse(rp.contact.field.w_money_completion_level < threshhold, 1, 0),
             rules_started = ifelse(rp.contact.field.w_rules_completion_level < threshhold, 1, 0),
             consequence_started = ifelse(rp.contact.field.w_consequence_completion_level < threshhold, 1, 0),
             solve_started = ifelse(rp.contact.field.w_solve_completion_level < threshhold, 1, 0),
             safe_started = ifelse(rp.contact.field.w_safe_completion_level < threshhold, 1, 0),
             crisis_started = ifelse(rp.contact.field.w_crisis_completion_level < threshhold, 1, 0),
             celebrate_started = ifelse(rp.contact.field.w_celebrate_completion_level < threshhold, 1, 0))
  }
  return(plhdata_org_clean_engagement)
}

# faster? version:
# threshold_function2 <- function(data, threshold, columns = data_completion_level, sign = "gt") {
#   # Check sign parameter
#   if (!sign %in% c("gt", "lt")) {
#     stop("sign must be one of 'gt' or 'lt'")
#   }
#   
#   # Check if columns exist in data
#   if (!all(columns %in% names(data))) {
#     stop("Columns do not exist in data")
#   }
#   
#   data <- data %>% mutate(across(columns, ~ as.numeric(.x)))
#   # Get column indices
#   col_indices <- match(columns, names(data))
#   
#   # Define column sign
#   col_sign <- ifelse(sign == "gt", 1, -1)
#   
#   # Compute logical matrix
#   bool_mat <- (data[, col_indices] * col_sign) > (threshold * col_sign)
#   
#   # Compute column values
#   col_vals <- bool_mat * 1
#   
#   # Define new column names
#   new_col_names <- paste0(
#     naming_conventions(columns, replace = "rp.contact.field.w_",
#                        replace_after = "completion_level",
#                        rename = FALSE), "started")
#   
#   # Create new dataframe
#   plhdata_org_clean_engagement <- cbind(data, col_vals)
#   names(plhdata_org_clean_engagement)[(ncol(data) + 1):(ncol(data) + ncol(col_vals))] <- new_col_names
#   
#   return(plhdata_org_clean_engagement)
# }

# slower version:
# threshhold_function_3 <- function(data, threshhold, columns = data_completion_level, sign = "gt") {
#   
#   # Check sign parameter
#   if (!sign %in% c("gt", "lt")) {
#     stop("sign must be one of 'gt' or 'lt'")
#   }
#   
#   # Check if columns exist in data
#   if (!all(columns %in% names(data))) {
#     stop("Columns do not exist in data")
#   }
#   
#   data <- data %>% mutate(across(columns, ~ as.numeric(.x)))
#   
#   # Define the mutation function
#   mutate_fun <- function(column_name) {
#     if (sign == "gt") {
#       new_column_name <- paste0(naming_conventions(column_name, replace = "rp.contact.field.w_",
#                                                    replace_after = "completion_level",
#                                                    rename = FALSE), "started")
#       data %>%
#         mutate(new_column_name = as.integer({{ column_name}} > threshhold))
#     } else {
#       new_column_name <- paste0(column_name, "_started")
#       data %>%
#         mutate(new_column_name = as.integer({{ column_name}} > threshhold))
#     }
#   }
#   
#   # Use the mutate function to apply mutation on multiple columns
#   plhdata_org_clean_engagement <- data %>%
#     mutate(across(all_of(columns), mutate_fun))
#   
#   return(plhdata_org_clean_engagement)
# }

#######################################
# Used for ParentApp and ParentText --------------------------------------------
#######################################
naming_conventions <- function(x, replace, replace_after, rename = TRUE) {
  if (!missing(replace)){
    x <- gsub(paste("^.*?", replace, ".*", sep = ""), "", x)
  }
  if (!missing(replace_after)){
    x <- gsub(paste(replace_after, "$", sep = ""), "", x)
  }
  if (rename){
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x <- gsub("_", " ", x) 
  }
  x
}

summary_calculation <- function(data = plhdata_org_clean, factors = NULL, columns_to_summarise, summaries = c("frequencies", "mean", "mmm", "sum"), # mmm = mean, min, max. For some of parenttext, summaries == mmm                                include_country_margins, country_factor,
                                together = FALSE, include_margins = FALSE, na.rm = TRUE, drop = FALSE,
                                include_country_margins = FALSE, country_factor = FALSE,
                                include_perc = FALSE){ # include_perc = TRUE for parenttext
  summaries <- match.arg(summaries)
  if (summaries == "frequencies"){
    if (is.numeric(data[[columns_to_summarise]])){ # if statement not in parenttext
      data <- data %>%
        mutate(across(all_of({{ columns_to_summarise }}), ~round(.x))) %>%
        mutate(across(all_of({{ columns_to_summarise }}), ~as.character(.x)))
    }
    data <- data %>%
      mutate(across(all_of({{ factors }}), ~ replace_na(.x, "unknown")))
    factors <- c(factors, columns_to_summarise)
    summary_output <- data %>% 
      group_by(across(all_of({{ factors }})), .drop = drop) %>% 
      summarise(n = n(), .groups = "drop")
    if (include_perc) {
      summary_output <- summary_output %>% 
        mutate(perc = n / sum(n) * 100)
    }
    if (include_margins && !is.null(factors)){
      # if (include_country_margins){
      #   summary_output_country <- data %>%
      #     group_by(across(c({{ columns_to_summarise }}, {{ country_factor }})), .drop = FALSE) %>%
      #     summarise(n = n())
      #   corner_margin_country <- data %>%
      #     group_by(across(c({{ country_factor }})), .drop = FALSE) %>%
      #     summarise(n = n())
      #   names(summary_output_country)[length(summary_output_country)-1] <- names(summary_output)[length(summary_output)-1]
      #   names(corner_margin_country)[length(corner_margin_country)-1] <- names(summary_output)[length(summary_output)-1]
      #   #summary_output_country <- dplyr::rename(summary_output_country, Org = {{ country_factor }}) #"{{ factors }}" := {{ country_factor }})
      #   #corner_margin_country <- dplyr::rename(corner_margin_country, Org = {{ country_factor }}) #"{{ factors }}" := {{ country_factor }})
      # } else {
      #   summary_output_country <- NULL
      #   corner_margin_country <- NULL
      # }
      # summary_output <- bind_rows(summary_output, cts_margin, ftr_margin, corner_margin, summary_output_country, corner_margin_country, .id = "id")
      margin_tables <- list()
      power_sets <- rje::powerSet(factors)
      power_sets_outer <- power_sets[-(c(length(power_sets)))]
      for (facts in power_sets_outer) {
        if (length(facts) == 0) facts <- c()
        new_output <- summary_output %>% group_by(across(all_of(facts))) %>% summarise(n = sum(n), .groups = "drop")
        if (include_perc) {
          new_output <- new_output %>% mutate(perc = n / sum(n) * 100)
        }
        margin_tables[[length(margin_tables) + 1]] <- new_output
      }
      names(margin_tables) <- 1:length(margin_tables)
      summary_output <- summary_output %>% 
        bind_rows(plyr::ldply(margin_tables)) %>%
        ungroup() %>%
        mutate(across(all_of({{ factors }}), ~ replace_na(.x, "Total"))) %>%
        mutate(across(all_of({{ factors }}), ~ fct_relevel(.x, "Total", after = Inf))) %>% 
        select(-c(".id"))
    }
    if (together && include_perc){
      summary_output <- summary_output %>%
        mutate("Count (%)" := str_c(`n`, ' (', round(`perc`, 2), ")")) %>%
        dplyr::select(-c(n, perc))
    }
  } else if (summaries == "mmm"){
    summary_output <- data %>%
      group_by(across(all_of({{ factors }}))) %>%
      summarise(across({{ columns_to_summarise }},
                       list(mean = ~ mean(.x, na.rm = TRUE),
                            min = ~ min(.x, na.rm = TRUE),
                            max = ~ max(.x, na.rm = TRUE)),
                       .names = "{.col}_{.fn}"))
    
    if (include_margins && !is.null(factors)){
      corner_margin <- data %>%
        summarise(across(all_of({{ columns_to_summarise }}),
                         list(mean = ~ mean(.x, na.rm = TRUE),
                              min = ~ min(.x, na.rm = TRUE),
                              max = ~ max(.x, na.rm = TRUE)),
                         .names = "{.col}_{.fn}"))
      
      summary_output <- bind_rows(summary_output, corner_margin, .id = "id") %>%
        ungroup() %>%
        mutate(across(all_of({{ factors }}), as.character)) %>%
        mutate(across(all_of({{ factors }}), ~ifelse(id == 2, "Total", .x))) %>%
        mutate(across(all_of({{ factors }}), ~fct_relevel(.x, "Total", after = Inf))) %>%
        select(-c("id"))
    }
  } else {
    summary_output <- data %>%
      group_by(across(all_of({{ factors }}))) %>%
      summarise(across(all_of({{ columns_to_summarise }}),
                       ~ case_when(
                         summaries == "sum" ~ sum(., na.rm = na.rm),
                         summaries == "mean" ~ mean(., na.rm = na.rm))))
    if (include_margins && !is.null(factors)){
      corner_margin <- data %>%
        summarise(across(all_of({{ columns_to_summarise }}),
                         ~ case_when(
                           summaries == "sum" ~ sum(., na.rm = na.rm),
                           summaries == "mean" ~ mean(., na.rm = na.rm))))
      summary_output <- bind_rows(summary_output, corner_margin, .id = "id") %>%
        ungroup() %>%
        mutate(across(all_of({{ factors }}), ~if_else(id == 2, "Total", as.character(.)))) %>%
        mutate(across(all_of({{ factors }}), ~fct_relevel(.x, "Total", after = Inf))) %>%
        select(-id)
    }
  }
  if (include_margins && !is.null(factors)){
    if (length(data %>% dplyr::select(all_of({{ factors }}))) == 1){
      cell_values_levels <- data %>% pull({{ factors }}) %>% levels()
      if (include_margins){ cell_values_levels <- c(cell_values_levels, "Total") }
      summary_output <- summary_output %>%
        dplyr::mutate(dplyr::across(all_of({{ factors }}), ~ factor(.x))) %>%
        dplyr::mutate(dplyr::across(all_of({{ factors }}), ~ forcats::fct_relevel(.x, cell_values_levels))) %>%
        dplyr::arrange({{ factors }})
    }
    if (length(data %>% dplyr::select(all_of({{ columns_to_summarise }}))) == 1){
      cell_values_levels <- data %>% pull({{ columns_to_summarise }}) %>% levels()
      if (include_margins){ cell_values_levels <- c(cell_values_levels, "Total") }
      if (summaries %in% c("frequencies", "sum")){ # doesn't work for mean because we have multiple columns and rename them
        summary_output <- summary_output %>%
          dplyr::mutate(dplyr::across(all_of({{ columns_to_summarise }}), ~ factor(.x))) %>%
          dplyr::mutate(dplyr::across(all_of({{ columns_to_summarise }}),
                                      ~ forcats::fct_relevel(.x, cell_values_levels))) %>%
          dplyr::arrange({{ columns_to_summarise }})
      }
    }
  }
  return(summary_output)
}

summary_table <- function(data = plhdata_org_clean, factors = NULL, columns_to_summarise = NULL, summaries = c("frequencies", "mean", "mmm", "sum"), # todo: did have factors = Org before.
                          replace = "rp.contact.field.", replace_after = NULL, include_margins = FALSE, wider_table = TRUE, 
                          include_country_margins = TRUE, country_factor = "country", na.rm = TRUE,
                          display_table = FALSE, naming_convention = TRUE, include_perc = FALSE,
                          together = TRUE, drop = FALSE){
  summaries <- match.arg(summaries)
  data <- data %>% dplyr::select({{ factors }}, {{ columns_to_summarise }})
  return_table <- summary_calculation(data = data,
                                      factors = c({{ factors }}),
                                      columns_to_summarise = c({{ columns_to_summarise }}),
                                      include_margins = include_margins,
                                      include_country_margins = include_country_margins,
                                      country_factor = country_factor,
                                      summaries = summaries,
                                      include_perc = include_perc,
                                      together = together,
                                      na.rm = na.rm,
                                      drop = drop)
  if (display_table){
    return_table_names <- naming_conventions(colnames(return_table), replace = replace, replace_after = replace_after)
    if (summaries == "frequencies"){
      return_table <- return_table %>% pivot_wider(id_cols = {{ factors }}, names_from =  {{ columns_to_summarise }}, values_from = n)
    }
    return_table <- gt(as_tibble(return_table)) %>%
      tab_header(title = paste(return_table_names[1], "by", return_table_names[2])) %>%
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
    return(return_table)
    #if (summaries == "mean"){
    #  names(return_table$`_data`) <- naming_conventions(names(return_table$`_data`), replace = replace)
    #}
  } else {
    if (summaries == "frequencies"){
      if (include_perc){
        all_factors <- str_split(gsub("^c\\(|\\)$", "", deparse(substitute(factors))), pattern = ", ")
        all_columns_to_summarise <- str_split(gsub("^c\\(|\\)$", "", deparse(substitute(columns_to_summarise))), pattern = ", ")
        if (wider_table && !missing(columns_to_summarise) && (any(all_factors[[1]] %in% (all_columns_to_summarise)[[1]]) == FALSE)){
          if (together){
            values_from <- "Count (%)"
          } else {
            values_from <- "n"
          }
          return_table <- return_table %>% pivot_wider(id_cols = {{ factors }}, names_from =  {{ columns_to_summarise }}, values_from = values_from, names_prefix = "")
        }
      } else if (include_perc == FALSE && wider_table){
        return_table <- return_table %>% pivot_wider(id_cols = {{ factors }}, names_from =  {{ columns_to_summarise }}, values_from = n, names_prefix = "")
      }
    }
  }
  if (naming_convention){
    colnames(return_table) <- naming_conventions(colnames(return_table), replace = replace, replace_after = replace_after)
  }
  return(return_table)
}

summary_plot <- function(data = plhdata_org_clean, columns_to_summarise, naming_convention = TRUE, replace = "rp.contact.field.",
                         replace_after = NULL,
                         plot_type = c("histogram", "boxplot")) {	
  plot_type <- match.arg(plot_type)
  x_axis_label = naming_conventions(colnames(data%>%select(.data[[columns_to_summarise]])), replace = replace, replace_after = replace_after)	
  
  return_plot <- ggplot(data) +	
    viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +	
    labs(x = x_axis_label, y = "Count") +	
    theme_classic()	
  
  if(plot_type == "histogram"){
    return_plot <- return_plot + geom_bar(data = data, aes(x = .data[[columns_to_summarise]]))
  } else {
    return_plot <- return_plot + geom_boxplot(data = data, aes(y = .data[[columns_to_summarise]]))
  }
  return(return_plot)	
}

multiple_table_output <- function(data = plhdata_org_clean, columns_to_summarise, replace = "rp.contact.field.", replace_after = NULL, summaries = "frequencies", na.rm = TRUE, factors = "Org", include_margins = FALSE){
  # run: add_na_variable here with warning 
  data <- add_na_variable(data = data, variable = columns_to_summarise)
  
  variable_display_names <- naming_conventions(columns_to_summarise, replace = replace, replace_after = replace_after)
  summary_table_values <- data %>%
    map(.x = columns_to_summarise, .f = ~replace_na(.x, "unknown"))  %>%
    map(.x = columns_to_summarise, .f = ~summary_table(data = data,
                                                       columns_to_summarise = .x,
                                                       display = FALSE,
                                                       include_margins = include_margins,
                                                       summaries = summaries,
                                                       factors = factors,
                                                       na.rm = na.rm))
  
  names(summary_table_values) <- variable_display_names
  return(summary_table_values)
}
multiple_plot_output <- function(data = plhdata_org_clean, columns_to_summarise, replace = "rp.contact.field.",
                                 replace_after = NULL, plot_type = c("histogram", "boxplot")){
  variable_display_names <- naming_conventions(columns_to_summarise, replace = replace, replace_after = replace_after)
  summary_plot_values <- data %>%
    map(data = data, .x = columns_to_summarise, .f = ~summary_plot(columns_to_summarise = .x, plot_type = plot_type, replace = replace, replace_after = replace_after))
  
  names(summary_plot_values) <- variable_display_names
  return(summary_plot_values)
}


fluid_row_box <- function(variable1, variable2 = NULL, title1 = NULL, title2 = NULL, status = "primary"){
  if (is.null(title1)) { title1 <- naming_conventions(variable1) }
  if (is.null(title2) && (!is.null(variable2))) { title2 <- naming_conventions(variable2) }
  if (!is.null(variable2)){
    fluidRow(
      box(collapsible = TRUE,
          solidHeader = TRUE,
          title = title1,
          status = status,  
          style='width:100%;overflow-x: scroll;',
          plotlyOutput(outputId = paste0("plot_", variable1), height = "240"), #generates graph
          shiny::tableOutput(paste0("table_", variable1))  #generates table
      ), #closes box
      
      box(collapsible = TRUE,
          solidHeader = TRUE,
          title = title2,
          status = status,  
          style='width:100%;overflow-x: scroll;',
          plotlyOutput(outputId = paste0("plot_", variable2), height = "240"), #generates graph
          shiny::tableOutput(paste0("table_", variable2))  #generates table
      ))
  } else {
    fluidRow(
      box(collapsible = TRUE,
          solidHeader = TRUE,
          title = title1,
          status = status,  
          style='width:100%;overflow-x: scroll;',
          plotlyOutput(outputId = paste0("plot_", variable1), height = "240"), #generates graph
          shiny::tableOutput(paste0("table_", variable1))  #generates table
      ))
  }
}