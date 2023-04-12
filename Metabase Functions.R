#' Interaction with chatbot
#Source the personal setup for data
source(here("config/Personal Setup.R"))

#Get data from excel
UIC.Tracker <- rio::import(file = here("data/UIC Tracker.xlsx"), which = "UIC Tracker 211014")
UIC_Tracker_Tanzania <- rio::import(file = here("data/UIC Tracker Tanzania.xlsx"))

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
    group_by(across(c({{ factors }})), .drop = FALSE) %>%
    summarise(replied = n())
  denominator <- data %>%
    group_by(across(c({{ factors }})), .drop = FALSE) %>%
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

challenge_freq <- function(data = plhdata_org_clean, group_by = "Org", var, append_var){
  data <- add_na_variable(data = data, variable = var)
  data <- add_na_variable(data = data, variable = append_var)
  plh_list <- stringr::str_split(data[[var]], pattern = ", ")
  plh_append <- data[[append_var]]
  plh_Org <- data[[group_by]]
  for (i in 1:length(plh_list)){
    plh_list1 <- c(plh_list[[i]], plh_append[i])
    plh_list2 <- as.character(plh_Org[i])
    df <- data.frame(plh_list1, Org = plh_list2)
    plh_list[[i]] <- df
  }
  plh_list <- purrr::map(.x = plh_list,
                         .f = ~ unique(.x))
  
  plh_list <- plyr::ldply(plh_list)
  plh_list <- plh_list %>% group_by(plh_list1, Org) %>% summarise(n())
  
  plh_list <- plh_list %>%
    dplyr::filter(!plh_list1 %in% c("other_challenge", "undefined", "null")) %>%
    dplyr::filter(!is.na(plh_list1)) %>%
    pivot_wider(names_from = plh_list1, values_from = `n()`) %>%
    mutate_all(~replace(., is.na(.), 0))
  return(plh_list)
}

survey_table <- function(data = plhdata_org_clean, metadata = r_variables_names, location_ID = "survey_past_week",
                         factors = "Org"){
  data_to_tabulate <- metadata %>% filter(location_ID == {{ location_ID }})
  
  summary_table <- data %>%
    # take the data, and select the relevant variables
    dplyr::select(c(factors, country, app_user_id, data_to_tabulate$metabase_ID)) %>%
    tidyr::unite(col = "Org", {{ factors }}) %>%
    # rearrange the data frame
    pivot_longer(cols = -c(Org, country, app_user_id), names_to = "metabase_ID") %>%
    # join with our metadata on the variables
    full_join(., data_to_tabulate) %>%
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
        .f = ~summary_table(data = summary_table_wider, factors = "Org", columns_to_summarise = .x, display = FALSE, include_margins = TRUE),
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
        tidyr::unite(col = "Org", {{ factors }})%>%
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
                 )))
    } else if (study == "Optimisation") {
      # return(box(checkboxInput(inputId = "chk_support",
      #                          label = "Group by support",
      #                          value = TRUE),
      #            uiOutput("opt_chk_support")))
      return(column(width = 12,
                    box(checkboxInput(inputId = "select_cluster",
                                      label = "All clusters",
                                      value = TRUE),
                      textInput(inputId = "opt_cluster",
                                  label = "Cluster",
                                  placeholder = "Enter values separated by a comma...")),
                    fluidRow(box(width = 4,
                                 checkboxGroupInput(inputId = "opt_support",
                                                    label = "Support",
                                                    c("Self-guided" = "Self-guided",
                                                      "WhatsApp" = "WhatsApp"),
                                                    selected = c("Self-guided", "WhatsApp"))),
                             box(width = 4, checkboxGroupInput(inputId = "opt_skin",
                                                               label = "Skin type",
                                                               c("Module" = "Module",
                                                                 "Workshop" = "Workshop"),
                                                               selected = c("Module", "Workshop"))),
                             box(width = 4, checkboxGroupInput(inputId = "opt_diglit",
                                                               label = "Digital literacy",
                                                               c("On" = "On",
                                                                 "Off" = "Off"),
                                                               selected = c("On", "Off")))#,
                             #actionButton("goButton", "Submit", class = "btn-success")
                    )))
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

hp_mood_plot <- function(data, factors, manipulation = "longer", limits = c("sad", "ok", "happy", "NA"),
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
    scale_x_discrete(guide = guide_axis(angle = 90),
                     limits = limits) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    labs(x = xlab, y = "Frequency")
}

threshhold_function <- function(data, threshhold, sign = "gt"){
  if (sign == "gt"){
    plhdata_org_clean_engagement <- data %>%
      mutate(self_care_started = ifelse(rp.contact.field.w_self_care_completion_level > threshhold, 1, 0),
             one_on_one_started = ifelse(rp.contact.field.w_1on1_completion_level > threshhold, 1, 0),
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
  } else if (sign == "lt"){
    plhdata_org_clean_engagement <- data %>%
      mutate(self_care_started = ifelse(rp.contact.field.w_self_care_completion_level < threshhold, 1, 0),
             one_on_one_started = ifelse(rp.contact.field.w_1on1_completion_level < threshhold, 1, 0),
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
  } else {
    stop("sign must be one of gt or lt")
  }
  return(plhdata_org_clean_engagement)
}


#######################################
# Used for ParentApp and ParentText --------------------------------------------
#######################################
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

summary_calculation <- function(data = plhdata_org_clean, factors, columns_to_summarise, summaries = c("frequencies", "mean", "total"),
                                include_country_margins, country_factor, together = FALSE, include_margins = FALSE, na.rm = TRUE){
  
  summaries <- match.arg(summaries)
  if (summaries == "frequencies"){
    if (is.numeric(data[[columns_to_summarise]])){
      data <- data %>%
        mutate(across({{ columns_to_summarise }}, ~round(.x))) 
    }
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
  } else if (summaries == "total") {
    summary_output <- data %>%
      group_by(across({{ factors }}), .drop = FALSE) %>%
      #mutate(across({{ columns_to_summarise }}, ~as.numeric(.))) %>%
      summarise(across({{ columns_to_summarise }}, ~sum(.x, na.rm = na.rm)))
  } else {
    summary_output <- data %>%
      group_by(across({{ factors }}), .drop = FALSE) %>%
      #mutate(across({{ columns_to_summarise }}, ~as.numeric(.))) %>%
      summarise(across({{ columns_to_summarise }}, ~mean(.x, na.rm = na.rm)))
    
    if (include_margins){
      corner_margin <- data %>%
        summarise(across(c({{ columns_to_summarise }}), ~mean(.x, na.rm  = na.rm)))
      
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

summary_table <- function(data = plhdata_org_clean, factors = Org, columns_to_summarise, summaries = c("frequencies", "mean", "total"),
                          replace = "rp.contact.field.", replace_after = NULL, include_margins = FALSE, include_country_margins = TRUE, 
                          country_factor = "country", wider_table = TRUE, na.rm = TRUE,
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
                                      together = together,
                                      na.rm = na.rm)
  
  return_table_names <- naming_conventions(colnames(return_table), replace = replace, replace_after = replace_after)
  if (summaries %in% c("mean", "total")){
    if (naming_convention){
      colnames(return_table) <- naming_conventions(colnames(return_table), replace = replace, replace_after = replace_after)
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
                         replace_after = NULL,
                         plot_type = c("histogram", "boxplot")) {	
  plot_type <- match.arg(plot_type)
  x_axis_label = naming_conventions(colnames(data%>%select(.data[[columns_to_summarise]])), replace = replace, replace_after = replace_after)	
  
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

multiple_table_output <- function(data = plhdata_org_clean, columns_to_summarise, replace = "rp.contact.field.", replace_after = NULL, summaries = "frequencies", na.rm = TRUE, factors = "Org"){
  # run: add_na_variable here with warning 
  data <- add_na_variable(data = data, variable = columns_to_summarise)
  
  variable_display_names <- naming_conventions(columns_to_summarise, replace = replace, replace_after = replace_after)
  summary_table_values <- data %>%
    map(.x = columns_to_summarise, .f = ~replace_na(.x, "unknown"))  %>%
    map(.x = columns_to_summarise, .f = ~summary_table(data = data,
                                                       columns_to_summarise = .x,
                                                       display = FALSE,
                                                       include_margins = TRUE,
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