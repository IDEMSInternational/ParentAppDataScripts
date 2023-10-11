#### To Calculate the Module Completion Rate ####
module_info <- read.csv("modules_info.csv")

calculate_module_completion_percentage <- function() {
  # Create an empty data frame to store the results
  result_df <- data.frame(Module_ID = integer(),
                          Module_Name = character(),
                          Category = character(),
                          Percentage = numeric(),
                          stringsAsFactors = FALSE)
  
  # Iterate over each module
  for (module_id in unique(module_info$module_id)) {
    module_name <- unique(module_info$module_name[module_info$module_id == module_id])
    completed_col <- paste0("rp.contact.field.", module_name, "_completed")
    
    # Get the count of users who completed, not completed, and not started the module
    total_users <- nrow(wash_data)
    completed_users <- sum(wash_data[[completed_col]] == "Yes", na.rm = TRUE)
    not_completed_users <- sum(wash_data[[completed_col]] == "No", na.rm = TRUE)
    not_started_users <- sum(is.na(wash_data[[completed_col]]))
    
    # Calculate the percentage of users for each category
    completed_percentage <- (completed_users / total_users) * 100
    not_completed_percentage <- (not_completed_users / total_users) * 100
    not_started_percentage <- (not_started_users / total_users) * 100
    
    # Add the results to the data frame
    result_df <- rbind(result_df,
                       data.frame(Module_ID = module_id,
                                  Module_Name = module_name,
                                  Category = "Completed",
                                  Percentage = completed_percentage),
                       data.frame(Module_ID = module_id,
                                  Module_Name = module_name,
                                  Category = "Not Completed",
                                  Percentage = not_completed_percentage),
                       data.frame(Module_ID = module_id,
                                  Module_Name = module_name,
                                  Category = "Not Started",
                                  Percentage = not_started_percentage))
  }
  
  result_df$Module_Name <- factor(result_df$Module_Name,
                                  levels = unique(result_df$Module_Name[order(result_df$Module_ID)]))
  
  return(result_df)
}

##### User Progression ####
calculate_section_completion_percentage_for_pie <- function() {
  # Create an empty data frame to store the results
  result_df <- data.frame(Module_ID = integer(),
                          Module_Name = character(),
                          Section_ID = integer(),
                          Section_Name = character(),
                          Category = character(),
                          Percentage = numeric(),
                          stringsAsFactors = FALSE)
  
  # Iterate over each section in the module_info data frame
  for (i in seq_len(nrow(module_info))) {
    module_id <- module_info$module_id[i]
    module_name <- unique(module_info$module_name[module_info$module_id == module_id])
    section_id <- module_info$section_id[i]
    section_name <- module_info$section_name[i]
    completed_col <- paste0("rp.contact.field.", module_name, "_", section_name, "_completed")
    
    # Get the count of users who completed, not completed, and not started the section
    total_users <- nrow(wash_data)
    completed_users <- sum(wash_data[[completed_col]] == "Yes", na.rm = TRUE)
    not_completed_users <- sum(wash_data[[completed_col]] == "No", na.rm = TRUE)
    not_started_users <- sum(is.na(wash_data[[completed_col]]))
    
    # Calculate the percentage of users for each category within the section
    completed_percentage <- (completed_users / total_users) * 100
    not_completed_percentage <- (not_completed_users / total_users) * 100
    not_started_percentage <- (not_started_users / total_users) * 100
    
    # Add the results to the data frame
    result_df <- rbind(result_df,
                       data.frame(Module_ID = module_id,
                                  Module_Name = module_name,
                                  Section_ID = section_id,
                                  Section_Name = section_name,
                                  Category = "Completed",
                                  Percentage = completed_percentage),
                       data.frame(Module_ID = module_id,
                                  Module_Name = module_name,
                                  Section_ID = section_id,
                                  Section_Name = section_name,
                                  Category = "Not Completed",
                                  Percentage = not_completed_percentage),
                       data.frame(Module_ID = module_id,
                                  Module_Name = module_name,
                                  Section_ID = section_id,
                                  Section_Name = section_name,
                                  Category = "Not Started",
                                  Percentage = not_started_percentage))
  }
  
  return(result_df)
}

calculate_section_completion_percentage <- function(module_id) {
  # Filter module_data for the specified module_id
  module_data <- module_info[module_info$module_id == module_id, ]
  
  module_name <- unique(module_data$module_name)
  
  # Get the unique section IDs and section names for the specified module
  unique_section_ids <- unique(module_data$section_id)
  unique_section_names <- unique(module_data$section_name)
  
  # Create an empty data frame to store the results
  result_df <- data.frame(Section_ID = integer(),
                          Section_Name = character(),
                          Category = character(),
                          Percentage = numeric(),
                          stringsAsFactors = FALSE)
  
  # Iterate over each section within the module
  for (i in seq_along(unique_section_ids)) {
    section_id <- unique_section_ids[i]
    section_name <- unique_section_names[i]
    
    # Get the column name for the completed column of this section
    completed_col <- paste0("rp.contact.field.", module_name, "_", section_name, "_completed")
    
    # Calculate the count of users in each category for this section
    total_users <- nrow(wash_data)
    completed_users <- sum(wash_data[[completed_col]] == "Yes", na.rm = TRUE)
    not_completed_users <- sum(wash_data[[completed_col]] == "No", na.rm = TRUE)
    not_started_users <- sum(is.na(wash_data[[completed_col]]))
    
    # Calculate the percentage of users in each category for this section
    completed_percentage <- (completed_users / total_users) * 100
    not_completed_percentage <- (not_completed_users / total_users) * 100
    not_started_percentage <- (not_started_users / total_users) * 100
    
    # Add the results to the data frame for this section
    result_df <- rbind(result_df,
                       data.frame(Section_ID = section_id,
                                  Section_Name = section_name,
                                  Category = "Completed",
                                  Percentage = completed_percentage),
                       data.frame(Section_ID = section_id,
                                  Section_Name = section_name,
                                  Category = "Not Completed",
                                  Percentage = not_completed_percentage),
                       data.frame(Section_ID = section_id,
                                  Section_Name = section_name,
                                  Category = "Not Started",
                                  Percentage = not_started_percentage))
    
  }
  result_df$Section_Name <- factor(result_df$Section_Name,
                                   levels = unique(result_df$Section_Name[order(result_df$Section_ID)]))
  return(result_df)
}

#### Third User Progression through use of Card Click history function ####
calculate_module_click_percentage <- function() {
  # Create a unique list of module names
  unique_module_names <- unique(module_info$module_name)
  
  # Create an empty data frame to store the results
  result_df <- data.frame(Module_ID = integer(),
                          Module_Name = character(),
                          Category = character(),
                          Percentage = numeric(),
                          stringsAsFactors = FALSE)
  
  # Iterate over each module name
  for (module_name in unique_module_names) {
    # Construct the column name for the module's card click history
    module_click_col <- paste0("rp.contact.field.", module_name, "_card_click_history")
    
    # Check if the module_click_col exists in wash_data and has at least one non-NA value
    if (module_click_col %in% colnames(wash_data) && any(!is.na(wash_data[[module_click_col]]))) {
      # Get the count of users who clicked the module card at least once
      total_users <- nrow(wash_data)
      clicked_users <- sum(!is.na(wash_data[[module_click_col]]))
      not_clicked_users <- total_users - clicked_users
      
      # Calculate the percentage of users who clicked and who did not click the module card
      clicked_percentage <- (clicked_users / total_users) * 100
      not_clicked_percentage <- (not_clicked_users / total_users) * 100
      
      # Add the results to the data frame for the module
      result_df <- rbind(result_df,
                         data.frame(Module_ID = module_info$module_id[module_info$module_name == module_name][1],
                                    Module_Name = module_name,
                                    Category = "Clicked",
                                    Percentage = clicked_percentage),
                         data.frame(Module_ID = module_info$module_id[module_info$module_name == module_name][1],
                                    Module_Name = module_name,
                                    Category = "Not Clicked",
                                    Percentage = not_clicked_percentage))
    }
  }
  
  result_df$Module_Name <- factor(result_df$Module_Name,
                                  levels = unique(result_df$Module_Name[order(result_df$Module_ID)]))
  
  return(result_df)
}

calculate_section_click_percentage <- function(module_id) {
  # Create an empty data frame to store the results
  result_df <- data.frame(Section_ID = integer(),
                          Section_Name = character(),
                          Category = character(),
                          Percentage = numeric(),
                          stringsAsFactors = FALSE)
  
  # Get the section IDs and names for the specified module_id
  section_ids <- unique(module_info$section_id[module_info$module_id == module_id])
  section_names <- unique(module_info$section_name[module_info$module_id == module_id])
  module_name <- unique(module_info$module_name[module_info$module_id == module_id])
  
  # Iterate over each section in the module
  for (i in seq_along(section_ids)) {
    section_id <- section_ids[i]
    section_name <- section_names[i]
    
    # Construct the column name for the section's card click history
    section_click_col <- paste0("rp.contact.field.", module_name, "_", section_name, "_card_click_history")
    
    # Check if the section_click_col exists in wash_data and has at least one non-NA value
    if (section_click_col %in% colnames(wash_data) && any(!is.na(wash_data[[section_click_col]]))) {
      # Get the count of users who clicked the section card at least once
      total_users <- nrow(wash_data)
      clicked_users <- sum(!is.na(wash_data[[section_click_col]]))
      not_clicked_users <- total_users - clicked_users
      
      # Calculate the percentage of users who clicked and who did not click the section card
      clicked_percentage <- (clicked_users / total_users) * 100
      not_clicked_percentage <- (not_clicked_users / total_users) * 100
      
      # Add the results to the data frame for the section
      result_df <- rbind(result_df,
                         data.frame(Section_ID = section_id,
                                    Section_Name = section_name,
                                    Category = "Clicked",
                                    Percentage = clicked_percentage),
                         data.frame(Section_ID = section_id,
                                    Section_Name = section_name,
                                    Category = "Not Clicked",
                                    Percentage = not_clicked_percentage))
    }
  }
  result_df$Section_Name <- factor(result_df$Section_Name,
                                   levels = unique(result_df$Section_Name[order(result_df$Section_ID)]))
  return(result_df)
}

# # Call the function to get the percentage of users who clicked each section in the specified module
# module_id <- 2 # Replace this with the desired module_id
# section_click_percentage_df <- calculate_section_click_percentage(module_id)
# print(section_click_percentage_df)

#### Toggle Usage Percentage for Modules ####
calculate_toggle_percentage <- function(toggle_type, toggle_status) {
  # Validate the toggle_type and toggle_status arguments
  # valid_toggle_types <- c("started", "completed")
  # valid_toggle_statuses <- c("toggle", "not_toggle")
  
  # if (!(toggle_type %in% valid_toggle_types && toggle_status %in% valid_toggle_statuses)) {
  #   stop("Invalid toggle_type or toggle_status argument. Please choose from 'started' or 'completed' for toggle_type, and 'toggle' or 'not_toggle' for toggle_status.")
  # }
  
  # Create an empty data frame to store the results
  result_df <- data.frame(Module_ID = integer(),
                          Module_Name = character(),
                          Category = character(),
                          Percentage = numeric(),
                          stringsAsFactors = FALSE)
  
  # Get the unique module IDs and module names
  unique_module_ids <- unique(module_info$module_id)
  unique_module_names <- unique(module_info$module_name)
  
  # Iterate over each module
  for (i in seq_along(unique_module_ids)) {
    module_id <- unique_module_ids[i]
    module_name <- unique_module_names[i]
    
    # Get the column names based on the toggle_type and toggle_status for this module
    toggle_col <- paste0(module_name, "_", toggle_type, "_", toggle_status)
    
    # Check if the toggle_col exists in wash_data and has at least one non-NA value for this module
    if (toggle_col %in% colnames(wash_data) && any(!is.na(wash_data[[toggle_col]]))) {
      # Filter the wash_data for this module
      #module_data <- wash_data[wash_data$Module_ID == module_id, ]
      
      # Calculate the count of users in each category for this module
      total_users <- nrow(wash_data)
      toggle_users <- sum(wash_data[[toggle_col]] == "Yes", na.rm = TRUE)
      not_toggle_users <- total_users - toggle_users
      
      # Calculate the percentage of users in each category for this module
      toggle_percentage <- (toggle_users / total_users) * 100
      not_toggle_percentage <- (not_toggle_users / total_users) * 100
      
      # Add the results to the data frame for this module
      result_df <- rbind(result_df,
                         data.frame(Module_ID = module_id,
                                    Module_Name = module_name,
                                    Category = "Yes",
                                    Count = toggle_users,
                                    Percentage = round(toggle_percentage,2)),
                         data.frame(Module_ID = module_id,
                                    Module_Name = module_name,
                                    Category = "No",
                                    Count = not_toggle_users,
                                    Percentage = round(not_toggle_percentage,2)))
    }
  }
  result_df$Module_Name <- factor(result_df$Module_Name,
                                  levels = unique(result_df$Module_Name[order(result_df$Module_ID)]))
  
  return(result_df)
}

