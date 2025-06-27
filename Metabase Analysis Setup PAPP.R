##################################
##################################
####### User Data Analysis #######
##################################
##################################
country <- "Tanzania" # Tanzania, all
study <- "PAPP" # Optimisation, RCT, WASH, Pilot

additional_week_order <- c("Srh", "Svp", "Grief", "Learn")
ltp_activites <- c("chores", "bao", "walk", "cook_traditional", "garden", "charades", "role_play",
                   "find_pair", "mystery_box", "memory_game", "cook", "tell_stories", "dance",
                   "short_term_goal", "long_term_goal", "clean", "reflect_positive", "check_in_chat",
                   "dream_travel", "famous_party","two_truths", "time_machine", "superpowers",
                   "friendly_chat", "interrupter", "three_options", "yes_no_maybe",
                   "invent_story")
ltp_activites_name <- naming_conventions(ltp_activites)
### Set up UIC data

# add in start dates for clusters
# create a data frame with the lookup values
lookup_df <- data.frame(opt_cluster = c(1, 3, 4, 6, 7, 11, 13, 14),
                        whatsapp_start_date = as.Date(c("2022-11-27", "2022-12-17", "2023-01-10",
                                                        "2022-11-24", "2022-11-26", "2023-01-10",
                                                        "2022-12-24", "2022-12-24")))

### extract data ----------------------------------------------------------------------
plhdata_org <- openappr::get_openapp_data(site = plh_con,
                                          filter = TRUE,
                                          filter_variable = NULL,
                                          qry = "SELECT * FROM app_users WHERE \"createdAt\" >= '2025-05-20' AND \"createdAt\" <= '2025-06-11'"
)

appdata_df <- purrr::map(plhdata_org$contact_fields, jsonlite::fromJSON) %>% 
  dplyr::bind_rows()
device_info <- purrr::map(plhdata_org$device_info, jsonlite::fromJSON) %>% 
  dplyr::bind_rows()
plhdata_org <- dplyr::bind_cols(plhdata_org, appdata_df, device_info)

names(plhdata_org) <- gsub(x = names(plhdata_org), pattern = "\\-", replacement = ".")  

plhdata_org <- plhdata_org %>%
  dplyr::filter(isVirtual == FALSE) %>%
  dplyr::filter(platform %in% c("ios", "android"))

#####Create a subset for cleaned organisations ####
plhdata_org_clean <- plhdata_org # %>% filter(Org != "Other")%>% mutate(Org = factor(Org))
plhdata_org_clean <- plhdata_org_clean %>% filter(!is.na(app_user_id))
# only take one of each app user id
plhdata_org_clean <- plhdata_org_clean %>%
  dplyr::group_by(app_user_id) %>%
  filter(row_number() == 1) %>%
  ungroup()

plhdata_org_clean <- plhdata_org_clean %>% mutate(country = country)

# Replace NA with Unknowen

plhdata_org_clean <- plhdata_org_clean %>%
  mutate(rp.contact.field.user_referral_source = replace_na(rp.contact.field.user_referral_source, "Unknown"))
plhdata_org_clean$`Referral source` <- naming_conventions(plhdata_org_clean$rp.contact.field.user_referral_source)


# Sorting Name Changes --------------------------------------------------

if (!is.null(plhdata_org_clean$rp.contact.field.survey_welcome_a_1_final)){
  plhdata_org_clean <- plhdata_org_clean %>%
    mutate(rp.contact.field.survey_welcome_ppf = ifelse(!is.na(rp.contact.field.survey_welcome_ppf), rp.contact.field.survey_welcome_ppf, rp.contact.field.survey_welcome_a_1_final),
           rp.contact.field.survey_welcome_ppp = ifelse(!is.na(rp.contact.field.survey_welcome_ppp), rp.contact.field.survey_welcome_ppp, rp.contact.field.survey_welcome_a_2_final),
           rp.contact.field.survey_welcome_fin_s = ifelse(!is.na(rp.contact.field.survey_welcome_fin_s), rp.contact.field.survey_welcome_fin_s, rp.contact.field.survey_welcome_a_5_part_1_final),
           rp.contact.field.survey_welcome_fin_fi = ifelse(!is.na(rp.contact.field.survey_welcome_fin_fi), rp.contact.field.survey_welcome_fin_fi, rp.contact.field.survey_welcome_a_5_part_2_final)) %>%
    mutate(rp.contact.field.survey_final_ppf = ifelse(!is.na(rp.contact.field.survey_final_ppf), rp.contact.field.survey_final_ppf, rp.contact.field.survey_final_a_1_final),
           rp.contact.field.survey_final_ppp = ifelse(!is.na(rp.contact.field.survey_final_ppp), rp.contact.field.survey_final_ppp, rp.contact.field.survey_final_a_2_final),
           rp.contact.field.survey_final_fin_s = ifelse(!is.na(rp.contact.field.survey_final_fin_s), rp.contact.field.survey_final_fin_s, rp.contact.field.survey_final_a_5_part_1_final),
           rp.contact.field.survey_final_fin_fi = ifelse(!is.na(rp.contact.field.survey_final_fin_fi), rp.contact.field.survey_final_fin_fi, rp.contact.field.survey_final_a_5_part_2_final))
  
}

plhdata_org_clean <- plhdata_org_clean %>%
  mutate(rp.contact.field.user_gender = ifelse(is.na(rp.contact.field.user_gender), "undefined",
                                               rp.contact.field.user_gender))


plhdata_org_clean_mod <- plhdata_org_clean %>% filter(rp.contact.field._app_skin %in% c("modular", "default"))
json_data <- readRDS(file = "data/json_data.RDS")
json_data_af <- readRDS(file = "data/json_data_af.RDS")
# Esmee - what is the definition of completion at the moment for workshop skin?
# which rows.id do they have to have == true in in these to say they've completed?
total_completed_ind <- NULL
total_completed_tog <- NULL
activites_tracked <- NULL
j = 0
for (i in c("self_care", "1on1", "praise", "instruct", "stress", "solve", "money", "rules", "consequence", "safe", "crisis", "celebrate")){
  # which variables to select?
  
  json_data_i <- json_data[[i]]
  # rows.id, rows.individual, rows.together, rows.completed_field
  json_data_i <- json_data_i %>% dplyr::select(c(rows.id, rows.individual, rows.together, rows.completed_field)) %>%
    filter(!rows.id %in% c("home_practice", "hp_review"))
  if (i == "self_care" && study %in% c("RCT", "WASH", "PAPP")) {
    json_data_i$rows.individual[which(json_data_i$rows.completed_field %in% c("task_gp_w_self_care_welcome_individual_completed", "task_gp_w_self_care_survey_completed"))] <- FALSE
  }
  if (i == "celebrate" && study %in% c("RCT", "WASH", "PAPP")) {
    json_data_i$rows.individual[which(json_data_i$rows.completed_field %in% c("task_gp_w_celebrate_survey_activity_completed"))] <- FALSE
  }
  json_data_i_ind <- json_data_i %>% filter(rows.individual == TRUE)
  completed_rows_ind <- paste0("rp.contact.field.", json_data_i_ind$rows.completed_field)
  json_data_i_tog <- json_data_i %>% filter(rows.together == TRUE)
  completed_rows_tog <- paste0("rp.contact.field.", json_data_i_tog$rows.completed_field)
  
  plhdata_org_clean_mod_inds <- add_na_variable(plhdata_org_clean_mod, completed_rows_ind)
  plhdata_org_clean_mod_inds <- plhdata_org_clean_mod_inds %>%
    filter(rp.contact.field.workshop_path != "together") %>%
    dplyr::select(completed_rows_ind) %>%
    dplyr::mutate(across(everything(), ~as.numeric(as.logical(.)))) 
  plhdata_org_clean_mod_inds <- plhdata_org_clean_mod_inds %>%
    dplyr::mutate(total_completed := rowSums(., na.rm = TRUE)/length(.) * 100)
  
  plhdata_org_clean_mod_tog <- add_na_variable(plhdata_org_clean_mod, completed_rows_tog)
  plhdata_org_clean_mod_tog <- plhdata_org_clean_mod_tog %>%
    filter(rp.contact.field.workshop_path == "together") %>%
    dplyr::select(completed_rows_tog) %>%
    dplyr::mutate(across(everything(), ~as.numeric(as.logical(.))))
  plhdata_org_clean_mod_tog <- plhdata_org_clean_mod_tog %>%
    dplyr::mutate(total_completed := rowSums(., na.rm = TRUE)/length(.) * 100)
  j = j + 1
  total_completed_ind[[j]] <- plhdata_org_clean_mod_inds$total_completed
  total_completed_tog[[j]] <- plhdata_org_clean_mod_tog$total_completed
  activites_tracked[[i]] <- json_data_i_ind
}
names(total_completed_ind) <- data_completion_level
names(total_completed_tog) <- data_completion_level
plhdata_org_clean_mod_ind <- plhdata_org_clean_mod %>% filter(rp.contact.field.workshop_path != "together")
plhdata_org_clean_mod_tog <- plhdata_org_clean_mod %>% filter(rp.contact.field.workshop_path == "together")

total_completed_ind <- data.frame(app_user_id = plhdata_org_clean_mod_ind$app_user_id, total_completed_ind)
total_completed_tog <- data.frame(app_user_id = plhdata_org_clean_mod_tog$app_user_id, total_completed_tog)
modular_completion <- rbind(total_completed_ind, total_completed_tog)
plhdata_org_clean <- dplyr::full_join(plhdata_org_clean, modular_completion, by = "app_user_id", suffix = c("", ".mod"))
plhdata_org_clean <- plhdata_org_clean %>% mutate(rp.contact.field.w_self_care_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_self_care_completion_level.mod, rp.contact.field.w_self_care_completion_level),
                                                  rp.contact.field.w_1on1_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_1on1_completion_level.mod, rp.contact.field.w_1on1_completion_level),
                                                  rp.contact.field.w_praise_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_praise_completion_level.mod, rp.contact.field.w_praise_completion_level),
                                                  rp.contact.field.w_instruct_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_instruct_completion_level.mod, rp.contact.field.w_instruct_completion_level),
                                                  rp.contact.field.w_stress_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_stress_completion_level.mod, rp.contact.field.w_stress_completion_level),
                                                  rp.contact.field.w_solve_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_solve_completion_level.mod, rp.contact.field.w_solve_completion_level),
                                                  rp.contact.field.w_money_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_money_completion_level.mod, rp.contact.field.w_money_completion_level),
                                                  rp.contact.field.w_rules_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_rules_completion_level.mod, rp.contact.field.w_rules_completion_level),
                                                  rp.contact.field.w_consequence_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_consequence_completion_level.mod, rp.contact.field.w_consequence_completion_level),
                                                  rp.contact.field.w_safe_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_safe_completion_level.mod, rp.contact.field.w_safe_completion_level),
                                                  rp.contact.field.w_crisis_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_crisis_completion_level.mod, rp.contact.field.w_crisis_completion_level),
                                                  rp.contact.field.w_celebrate_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_celebrate_completion_level.mod, rp.contact.field.w_celebrate_completion_level))

plhdata_org_clean$rp.contact.field.task_gp_w_self_care_intro_completed
plhdata_org_clean$rp.contact.field.task_gp_w_self_care_tools_activity_completed
plhdata_org_clean$rp.contact.field.task_gp_w_self_care_ending_completed
plhdata_org_clean$rp.contact.field.task_gp_w_self_care_relax_completed
plhdata_org_clean$rp.contact.field.task_gp_w_self_care_recognise_completed
plhdata_org_clean$rp.contact.field.task_gp_w_self_care_reward_completed

plhdata_org_clean$rp.contact.field.w_self_care_completion_level[414]

var_names <- names(plhdata_org_clean)
completion_var <- NULL
new_modules <- c("learn", "svp", "grief", "srh")
for (module_name in new_modules){
  completed_mods <- paste0("rp.contact.field.", json_data_af[[module_name]])
  plhdata_org_clean <- add_na_variable(plhdata_org_clean, variable = completed_mods)
  selected_var_x <- plhdata_org_clean %>% dplyr::select(all_of(c("app_user_id", completed_mods)))
  selected_var_x <- selected_var_x %>%
    dplyr::mutate(across(completed_mods, ~as.numeric(as.logical(.)))) %>%
    dplyr::select(-c(paste0("rp.contact.field.task_gp_w_", module_name, "_home_practice_completed")))
  completion_var[[which(new_modules == module_name)]] <- selected_var_x %>%
    dplyr::mutate("rp.contact.field.w_{module_name}_completion_level" := rowSums(.[2:length(selected_var_x)], na.rm = TRUE)/length(.[2:length(selected_var_x)]) * 100) %>%
    dplyr::select("app_user_id", paste0("rp.contact.field.w_", module_name, "_completion_level"))
}
completion_var <- full_join(full_join(full_join(completion_var[[1]], completion_var[[2]]), completion_var[[3]]), completion_var[[4]])
plhdata_org_clean <- full_join(plhdata_org_clean, completion_var)

new_modules_completion_level <- c("rp.contact.field.w_learn_completion_level", "rp.contact.field.w_svp_completion_level", 
                                  "rp.contact.field.w_grief_completion_level", "rp.contact.field.w_srh_completion_level")


#head(plhdata_org_clean$rp.contact.field._app_skin)
#head(plhdata_org_clean$rp.contact.field.w_self_care_completion_level)
#head(xx$rp.contact.field.w_self_care_completion_level)


# More cleaning
# RCT TODO here

plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(ends_with("_completion_level"), ~as.numeric(.)))
plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(starts_with("rp.contact.field.parent_point"), ~as.numeric(.)))
plhdata_org_clean$rp.contact.field.app_launch_count <- as.numeric(plhdata_org_clean$rp.contact.field.app_launch_count)

plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(starts_with("rp.contact.field.app_launch_count"), ~as.numeric(.)))


## Data Analysis ## --------------------------------------------------------

# workshop_path edits ----------
plhdata_org_clean <- add_na_variable(plhdata_org_clean, c("rp.contact.field.workshop_path_user_choice",
                                                          "rp.contact.field.workshop_path"))
plhdata_org_clean <- plhdata_org_clean %>%
  mutate(rp.contact.field.workshop_path = ifelse(is.na(rp.contact.field.workshop_path_user_choice),
                                                 rp.contact.field.workshop_path,
                                                 ifelse(rp.contact.field.workshop_path_user_choice == "false",
                                                        "default",
                                                        rp.contact.field.workshop_path)))

# Tab ?? ----------
#Define workshop week order
# if (study == "RCT"){
week_order <- c("Self care", "1on1", "Praise", "Instruct", "Stress", "Solve", "Money", "Rules", "Consequence", "Safe",
                "Crisis", "Celebrate" )
# } else {
#   week_order <- c("Self care", "1on1", "Praise", "Instruct", "Stress", "Money", "Rules", "Consequence", "Solve", "Safe",
#                   "Crisis", "Celebrate" ) 
# }

#Each habit across workshop weeks
#relax points in each week
relax_workshop_vars <- c( "rp.contact.field.parent_point_count_relax_w_self_care", "rp.contact.field.parent_point_count_relax_w_1on1",
                          "rp.contact.field.parent_point_count_relax_w_praise", "rp.contact.field.parent_point_count_relax_w_instruct",
                          "rp.contact.field.parent_point_count_relax_w_stress", "rp.contact.field.parent_point_count_relax_w_solve", 
                          "rp.contact.field.parent_point_count_relax_w_money",
                          "rp.contact.field.parent_point_count_relax_w_rules", "rp.contact.field.parent_point_count_relax_w_consequence",
                          "rp.contact.field.parent_point_count_relax_w_safe",
                          "rp.contact.field.parent_point_count_relax_w_crisis","rp.contact.field.parent_point_count_relax_w_celebrate")
# treat_yourself points in each week
treat_yourself_workshop_vars <- c( "rp.contact.field.parent_point_count_treat_yourself_w_self_care", "rp.contact.field.parent_point_count_treat_yourself_w_1on1",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_praise", "rp.contact.field.parent_point_count_treat_yourself_w_instruct",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_stress", "rp.contact.field.parent_point_count_treat_yourself_w_solve", 
                                   "rp.contact.field.parent_point_count_treat_yourself_w_money",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_rules", "rp.contact.field.parent_point_count_treat_yourself_w_consequence",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_safe",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_crisis","rp.contact.field.parent_point_count_treat_yourself_w_celebrate")
# praise_yourself points in each week
praise_yourself_workshop_vars <- c( "rp.contact.field.parent_point_count_praise_yourself_w_self_care", "rp.contact.field.parent_point_count_praise_yourself_w_1on1",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_praise", "rp.contact.field.parent_point_count_praise_yourself_w_instruct",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_stress", "rp.contact.field.parent_point_count_praise_yourself_w_solve", 
                                    "rp.contact.field.parent_point_count_praise_yourself_w_money",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_rules", "rp.contact.field.parent_point_count_praise_yourself_w_consequence",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_safe",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_crisis","rp.contact.field.parent_point_count_praise_yourself_w_celebrate")
# spend_time points in each week
spend_time_workshop_vars <- c( "rp.contact.field.parent_point_count_spend_time_w_self_care", "rp.contact.field.parent_point_count_spend_time_w_1on1",
                               "rp.contact.field.parent_point_count_spend_time_w_praise", "rp.contact.field.parent_point_count_spend_time_w_instruct",
                               "rp.contact.field.parent_point_count_spend_time_w_stress", "rp.contact.field.parent_point_count_spend_time_w_solve", 
                               "rp.contact.field.parent_point_count_spend_time_w_money",
                               "rp.contact.field.parent_point_count_spend_time_w_rules", "rp.contact.field.parent_point_count_spend_time_w_consequence",
                               "rp.contact.field.parent_point_count_spend_time_w_safe",
                               "rp.contact.field.parent_point_count_spend_time_w_crisis","rp.contact.field.parent_point_count_spend_time_w_celebrate")
# praise_teen in each week
praise_teen_workshop_vars <- c( "rp.contact.field.parent_point_count_praise_teen_w_self_care", "rp.contact.field.parent_point_count_praise_teen_w_1on1",
                                "rp.contact.field.parent_point_count_praise_teen_w_praise", "rp.contact.field.parent_point_count_praise_teen_w_instruct",
                                "rp.contact.field.parent_point_count_praise_teen_w_stress","rp.contact.field.parent_point_count_praise_teen_w_solve", 
                                "rp.contact.field.parent_point_count_praise_teen_w_money",
                                "rp.contact.field.parent_point_count_praise_teen_w_rules", "rp.contact.field.parent_point_count_praise_teen_w_consequence",
                                "rp.contact.field.parent_point_count_praise_teen_w_safe",
                                "rp.contact.field.parent_point_count_praise_teen_w_crisis","rp.contact.field.parent_point_count_praise_teen_w_celebrate")
# instruct_positively points in each week
instruct_positively_workshop_vars <- c( "rp.contact.field.parent_point_count_instruct_positively_w_self_care", "rp.contact.field.parent_point_count_instruct_positively_w_1on1",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_praise", "rp.contact.field.parent_point_count_instruct_positively_w_instruct",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_stress", "rp.contact.field.parent_point_count_instruct_positively_w_solve", 
                                        "rp.contact.field.parent_point_count_instruct_positively_w_money",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_rules", "rp.contact.field.parent_point_count_instruct_positively_w_consequence",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_safe",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_crisis","rp.contact.field.parent_point_count_instruct_positively_w_celebrate")
# breathe points in each week
breathe_workshop_vars <- c( "rp.contact.field.parent_point_count_breathe_w_self_care", "rp.contact.field.parent_point_count_breathe_w_1on1",
                            "rp.contact.field.parent_point_count_breathe_w_praise", "rp.contact.field.parent_point_count_breathe_w_instruct",
                            "rp.contact.field.parent_point_count_breathe_w_stress", "rp.contact.field.parent_point_count_breathe_w_solve",
                            "rp.contact.field.parent_point_count_breathe_w_money",
                            "rp.contact.field.parent_point_count_breathe_w_rules", "rp.contact.field.parent_point_count_breathe_w_consequence",
                            "rp.contact.field.parent_point_count_breathe_w_safe",
                            "rp.contact.field.parent_point_count_breathe_w_crisis","rp.contact.field.parent_point_count_breathe_w_celebrate")
# money points in each week
money_workshop_vars <- c( "rp.contact.field.parent_point_count_money_w_self_care", "rp.contact.field.parent_point_count_money_w_1on1",
                          "rp.contact.field.parent_point_count_money_w_praise", "rp.contact.field.parent_point_count_money_w_instruct",
                          "rp.contact.field.parent_point_count_money_w_stress", "rp.contact.field.parent_point_count_money_w_solve",
                          "rp.contact.field.parent_point_count_money_w_money",
                          "rp.contact.field.parent_point_count_money_w_rules", #"rp.contact.field.parent_point_count_money_w_consequence",
                          "rp.contact.field.parent_point_count_money_w_safe",
                          #"rp.contact.field.parent_point_count_money_w_crisis",
                          "rp.contact.field.parent_point_count_money_w_celebrate")
# consequence points in each week
consequence_workshop_vars <- c( "rp.contact.field.parent_point_count_consequence_w_self_care", "rp.contact.field.parent_point_count_consequence_w_1on1",
                                "rp.contact.field.parent_point_count_consequence_w_praise", "rp.contact.field.parent_point_count_consequence_w_instruct",
                                "rp.contact.field.parent_point_count_consequence_w_stress", "rp.contact.field.parent_point_count_consequence_w_solve", 
                                "rp.contact.field.parent_point_count_consequence_w_money",
                                #"rp.contact.field.parent_point_count_consequence_w_rules", "rp.contact.field.parent_point_count_consequence_w_crisis",
                                "rp.contact.field.parent_point_count_consequence_w_consequence",
                                "rp.contact.field.parent_point_count_consequence_w_safe",
                                "rp.contact.field.parent_point_count_consequence_w_celebrate")
# safe points in each week
safe_workshop_vars <- c( "rp.contact.field.parent_point_count_safe_w_self_care", "rp.contact.field.parent_point_count_safe_w_1on1",
                         "rp.contact.field.parent_point_count_safe_w_praise", "rp.contact.field.parent_point_count_safe_w_instruct",
                         "rp.contact.field.parent_point_count_safe_w_stress", "rp.contact.field.parent_point_count_safe_w_solve", 
                         "rp.contact.field.parent_point_count_safe_w_money",
                         "rp.contact.field.parent_point_count_safe_w_rules", "rp.contact.field.parent_point_count_safe_w_consequence",
                         "rp.contact.field.parent_point_count_safe_w_safe",
                         "rp.contact.field.parent_point_count_safe_w_crisis","rp.contact.field.parent_point_count_safe_w_celebrate")

## Home Practice ------------------------------------------------------------------
data_hp_started <- c("rp.contact.field.w_1on1_hp_review_started",  "rp.contact.field.w_praise_hp_review_started",
                     "rp.contact.field.w_instruct_hp_review_started",  "rp.contact.field.w_stress_hp_review_started",
                     "rp.contact.field.w_solve_hp_review_started", 
                     "rp.contact.field.w_money_hp_review_started",  "rp.contact.field.w_rules_hp_review_started",
                     "rp.contact.field.w_consequence_hp_review_started",   "rp.contact.field.w_safe_hp_review_started",
                     "rp.contact.field.w_crisis_hp_review_started")

# RCT TODO
# plhdata_org_clean <- plhdata_org_clean %>%
#   dplyr::mutate(rp.contact.field.w_stress_hp_done = ifelse(rp.contact.field.w_stress_hp_talk_done == "yes" & 
#                                                              rp.contact.field.w_stress_hp_breathe_done == "yes",
#                                                            "yes",
#                                                            "no"))

data_hp_done <- c("rp.contact.field.w_1on1_hp_done", "rp.contact.field.w_praise_hp_done", "rp.contact.field.w_instruct_hp_done", "rp.contact.field.w_stress_hp_talk_done", "rp.contact.field.w_solve_hp_done",
                  "rp.contact.field.w_stress_hp_breathe_done","rp.contact.field.w_money_hp_done", "rp.contact.field.w_rules_hp_done", "rp.contact.field.w_consequence_hp_done",
                  "rp.contact.field.w_safe_hp_done", "rp.contact.field.w_crisis_hp_done")

# NB No mood 'review' for week 3 home practice (praise)
data_hp_mood <- c("rp.contact.field.w_1on1_hp_mood", "rp.contact.field.w_instruct_hp_mood", "rp.contact.field.w_stress_hp_breathe_mood", "rp.contact.field.w_stress_hp_talk_mood",
                  "rp.contact.field.w_solve_hp_mood", "rp.contact.field.w_money_hp_mood", "rp.contact.field.w_rules_hp_mood", "rp.contact.field.w_consequence_hp_mood",
                  "rp.contact.field.w_safe_hp_mood", "rp.contact.field.w_crisis_hp_mood") 

# TODO: this should work in function
# plhdata_org_clean <- add_na_variable(variable = data_hp_started)
# plhdata_org_clean <- add_na_variable(variable = data_hp_done)
# plhdata_org_clean <- add_na_variable(variable = data_hp_mood)

challenge_vars <- c("rp.contact.field.w_1on1_hp_challenge_list", "rp.contact.field.w_instruct_hp_challenge_list",
                    "rp.contact.field.w_stress_hp_challenge_list",  "rp.contact.field.w_solve_hp_challenge_list", "rp.contact.field.w_money_hp_challenge_list",
                    "rp.contact.field.w_rules_hp_challenge_list", "rp.contact.field.w_consequence_hp_challenge_list", 
                    "rp.contact.field.w_safe_hp_challenge_list", 
                    "rp.contact.field.w_crisis_hp_challenge_list")
chall_ap_vars <- c("rp.contact.field.w_1on1_hp_challenge", "rp.contact.field.w_instruct_hp_challenge",
                   "rp.contact.field.w_stress_hp_challenge", "rp.contact.field.w_solve_hp_challenge", "rp.contact.field.w_money_hp_challenge",
                   "rp.contact.field.w_rules_hp_challenge", "rp.contact.field.w_consequence_hp_challenge", 
                   "rp.contact.field.w_safe_hp_challenge", 
                   "rp.contact.field.w_crisis_hp_challenge")

# NB No challenge for week 3 home practice (praise)
data_hp_chall <- c("hp_list_challenges_1on1", "hp_list_challenges_instruct", "hp_list_challenges_stress", "hp_list_challenges_solve", "hp_list_challenges_money", "hp_list_challenges_rules",
                   "hp_list_challenges_consequence", "hp_list_challenges_safe", "hp_list_challenges_crisis")

# parent library ------------------------------------------------------------------
data_library <- c("rp.contact.field.click_hs_parent_centre_count", "rp.contact.field.click_pc_help_count",
                  "rp.contact.field.click_pc_my_tips_count", "rp.contact.field.click_pc_essential_tools_count",
                  "rp.contact.field.click_pc_covid_count", "rp.contact.field.click_pc_customisation_count",
                  "rp.contact.field.click_pc_relax_and_activities_count", "rp.contact.field.click_pc_support_contacts_count",
                  "rp.contact.field.click_pc_evidence_base_count", "rp.contact.field.click_pc_technical_support_count",
                  "rp.contact.field.click_pc_message_archive_count","rp.contact.field.click_pc_bereavement_count")

plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(
    starts_with("rp.contact.field.app_launch_count_w_"),
    ~ replace_na(.x, 0)
  ))   

# Down to data_library

# Completion status of baseline survey ------------------------------------------------
# plhdata_org_clean %>%
#   split(.$Org) %>%
#   map(~summary_table(data = .x, factor = NULL, columns_to_summarise = rp.contact.field.survey_welcome_complppplheted, replace = "rp.contact.field.survey"))

# FOR use in the shiny app:

workshop_engagement_cut <- plhdata_org_clean %>%
  mutate(across(all_of(data_completion_level),
                ~cut(.x, breaks = c(0, 1, 40, 80, 99, 100), include_lowest = TRUE,
                     labels = c("0", "1-40", "41-80", "81-99", "100")))) %>%
  mutate(across(all_of(data_completion_level), ~replace_na(.x, "0")))

summary_table_completion_level <- summary_table_base_build(opt_factors = NULL, data = workshop_engagement_cut, columns_to_summarise = data_completion_level,
                                                           replace = "rp.contact.field.w_",
                                                           replace_after = "_completion_level")
summary_table_completion_level <- summary_table_completion_level %>%
  purrr::map(~ {
    df <- .x %>% dplyr::mutate(across(everything(), ~ replace(., is.na(.), 0)))
    if (ncol(df) >= 2) df <- janitor::adorn_totals(df, "col", where(is.numeric))
    df
  })

for (i in 1:length(summary_table_completion_level)){
  if (!"100" %in% names(summary_table_completion_level[[i]])){
    summary_table_completion_level[[i]]$`100` <- 0
  }
}
select_items <- c("n_started", "perc_started", "n_completed", "perc_completed")

relative_perc_completed <- imap(summary_table_completion_level, ~.x %>%
                                  mutate(n_started = Total + `0` - `0`,
                                         perc_started = round(n_started/(Total + `0`) * 100, 1),
                                         perc_completed = round(`100`/n_started*100, 1),
                                         n_completed = `100`) %>%
                                  select(select_items))
relative_perc_completed <- plyr::ldply(relative_perc_completed, `.id` = "Workshop")


all_tracked_activities <- plyr::ldply(activites_tracked, .id = "module")
relative_perc_completed_n_started <- relative_perc_completed %>% dplyr::select(c("Workshop", "n_started"))
names_tracked_modules <- levels(all_tracked_activities$module)
relative_perc_completed_n_started <- relative_perc_completed_n_started %>%
  mutate(module = names_tracked_modules)
all_tracked_activities <- full_join(all_tracked_activities, relative_perc_completed_n_started)


# add into STRUCTURE DATA the number of started. 

# STEP 1: Structure data
structure_df <- all_tracked_activities
structure_df$rows.completed_field <- paste0("rp.contact.field.", structure_df$rows.completed_field)

# STEP 2: Read in main data containing the actual variables
data_df <- plhdata_org_clean

# STEP 3: Calculate proportion TRUE for each variable
completion_proportions <- structure_df %>%
  mutate(n_completed = map_dbl(rows.completed_field, ~ {
    varname <- .x
    if (varname %in% colnames(data_df)) {
      sum(data_df[[varname]] == "true", na.rm = TRUE)
    } else {
      NA_real_
    }
  })) %>%
  mutate(prop_completed = n_completed/n_started * 100)

completion_proportions$rows.id <- forcats::as_factor(completion_proportions$rows.id)

# STEP 4: Make it wide for heatmap
# heatmap_df <- completion_proportions %>%
#   pivot_wider(id_cols = c("rows.id"), names_from = Workshop, values_from = prop_completed)

# STEP 5: Turn into long format for ggplot
# Fill in all possible (rows.id x module) combinations
heatmap_long <- completion_proportions %>%
  dplyr::select(rows.id, Workshop, prop_completed) %>%
  tidyr::complete(rows.id, Workshop) %>%  # Adds missing combinations with NA prop_completed
  dplyr::mutate(prop_completed = round(prop_completed, 1))  # Optional: round


##################################
##################################
### Notification Data Analysis ###
##################################
##################################
app_id <- plhdata_org_clean$app_user_id
# download push notification data
if (study != "WASH"){
  nf_data <- get_nf_data(site = plh_con,
                         filter = TRUE,
                         filter_variable = "app_user_id",
                         filter_variable_value = app_id
  )
}


