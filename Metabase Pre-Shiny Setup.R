study <- "RCT"
country <- "Tanzania"

# TODO: 
# redefine data_hp_started to use rp.contact.field.task_gp_w_1on1_hp_review_completed instead of rp.contact.field.w_1on1_hp_review_started
r_variables_names <- readxl::read_excel("shiny_metadata.xlsx")
data_survey_past_week_all <- r_variables_names %>% filter(location_ID == "survey_past_week")
data_baseline_survey <- r_variables_names %>%
  filter(location_ID == "data_baseline_survey") %>%
  filter(display == TRUE)
data_baseline_survey$display_name[which(data_baseline_survey$display_name == "app language")] <- " app language"
data_baseline_survey$object_name[which(data_baseline_survey$object_name == "language")] <- "app_language"

data_app_opens <- c("rp.contact.field.app_launch_count","rp.contact.field.app_launch_count_w_1on1", "rp.contact.field.app_launch_count_w_self_care",
                    "rp.contact.field.app_launch_count_w_praise","rp.contact.field.app_launch_count_w_instruct",
                    "rp.contact.field.app_launch_count_w_stress", "rp.contact.field.app_launch_count_w_solve",
                    "rp.contact.field.app_launch_count_w_money",
                    "rp.contact.field.app_launch_count_w_rules", "rp.contact.field.app_launch_count_w_consequence",
                    "rp.contact.field.app_launch_count_w_safe",
                    "rp.contact.field.app_launch_count_w_crisis", "rp.contact.field.app_launch_count_w_celebrate")

data_app_opens_neat <- c("Overall", "1on1 (2)", "Self Care (1)", "Praise (3)", "Positive Instructions(4)",
                         "Managing Stress(5)", "Problem Solving(6)", "Family Budget(7)","Rules(8)", "Calm Consequences(9)",  
                         "Teen Safety(10)", "Crisis(11)", "Celebration & Next Steps(12)")


# if (study == "RCT"){
#   # filter to just RCT_display == TRUE
# }
#   # TODO, we want a "display RCT option for RCT ones only.
data_completion_level_data <- r_variables_names %>% filter(location_ID == "data_completion_level") %>% filter(display == TRUE)
data_completion_level <- data_completion_level_data$metabase_ID
data_completion_level_names <- data_completion_level_data$display_name
data_habit_parent_points_data <- r_variables_names %>% filter(location_ID == "data_habit_parent_points_all") %>% filter(display == TRUE)
data_habit_parent_points_all <- data_habit_parent_points_data$metabase_ID

data_baseline_survey$object_name

# adding in about new variables: 
additional_week_order <- c("Srh", "Svp", "Grief", "Learn")
ltp_activites <- c("chores", "bao", "walk", "cook_traditional", "garden", "charades", "role_play",
                   "find_pair", "mystery_box", "memory_game", "cook", "tell_stories", "dance",
                   "short_term_goal", "long_term_goal", "clean", "reflect_positive", "check_in_chat",
                   "dream_travel", "famous_party","two_truths", "time_machine", "superpowers",
                   "friendly_chat", "interrupter", "three_options", "yes_no_maybe",
                   "invent_story")
ltp_activites_name <- naming_conventions(ltp_activites)
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