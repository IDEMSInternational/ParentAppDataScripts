r_variables_names <- readxl::read_excel("shiny_metadata.xlsx")
data_survey_past_week_all <- r_variables_names %>% filter(location_ID == "survey_past_week")
data_baseline_survey <- r_variables_names %>%
  filter(location_ID == "data_baseline_survey") %>%
  filter(display == TRUE)
data_baseline_survey$display_name[which(data_baseline_survey$display_name == "app language")] <- " app language"
data_baseline_survey$object_name[which(data_baseline_survey$object_name == "language")] <- "app_language"

if (study == "RCT"){
  # filter to just RCT_display == TRUE
}
  # TODO, we want a "display RCT option for RCT ones only.
data_completion_level_data <- r_variables_names %>% filter(location_ID == "data_completion_level") %>% filter(display == TRUE)
data_completion_level <- data_completion_level_data$metabase_ID
data_habit_parent_points_data <- r_variables_names %>% filter(location_ID == "data_habit_parent_points_all") %>% filter(display == TRUE)
data_habit_parent_points_all <- data_habit_parent_points_data$metabase_ID

data_baseline_survey$object_name

#w_self_care
#w_1on1
#w_praise
#w_instruct
#w_stress
#w_money
#w_rules
#w_consequence
#w_solve
#w_safe
#w_crisis
#w_celebrate