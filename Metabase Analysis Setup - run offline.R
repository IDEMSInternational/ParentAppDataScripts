#nrow(plhdata_org_clean)

#rio::export(x = nf_data1, file = "data/nf_data1.xlsx")
#rio::export(x = plhdata_org_clean, file = "data/plhdata_org_offline_TZ_Opt.xlsx")
study <- "Optimisation"

if (study == "Optimisation"){
  plhdata_org_clean <- rio::import(file = here("data/plhdata_org_offline_TZ_Opt.xlsx"))
} else {
  plhdata_org_clean <- rio::import(file = here("data/plhdata_org_offline_TZ_Pilot.xlsx"))
}
nf_data1 <- rio::import(file = here("data/nf_data1.xlsx"))

plhdata_org_clean <- plhdata_org_clean %>%
  mutate(Cluster = opt_cluster)
#plhdata_org_clean <- rio::import(file = here("data/plhdata_org_offline_TZ_Pilot.xlsx"))

# Sorting Name Changes --------------------------------------------------
old_names <- c("a_1_final", "a_2_final", "a_3_final", "a_4_final", "a_5_part_1_final", "a_5_part_2_final", "a_6_final", "a_7_part_1_final")
new_names <- c("ppf", "ppp", "ps", "cme", "fs", "fi", "cmp", "cs")
df_names <- data.frame(old_names, new_names)
for (v in c("v0.16.2", "v0.16.3", "v0.16.4")){
  for (i in 1:nrow(df_names)){
    old_name = df_names[i,1]
    new_name = df_names[i,2]
    plhdata_org_clean <- plhdata_org_clean %>%
      map_df(.x = v, #c("v0.16.2", "v0.16.3", "v0.16.4"),
             .f = ~version_variables_rename(old_name = old_name, new_name = new_name, new_name_v = .x, old_system_replacement = TRUE))
    # todo: doesn't work for v?? Should explore that. But for now, in this extra loop
  }
}

for (v in c("v0.16.4")){ # and other versions?
  for (i in 1:nrow(df_names)){
    old_name = df_names[i,1]
    new_name = df_names[i,2]
    plhdata_org_clean <- plhdata_org_clean %>%
      map_df(.x = v, #c("v0.16.2", "v0.16.3", "v0.16.4"),
             .f = ~version_variables_rename(old_name = old_name, new_name = new_name, new_name_v = .x, old_system_replacement = TRUE, survey = "final"))
    # todo: doesn't work for v?? Should explore that. But for now, in this extra loop
  }
}

# We first rename the _v system into the old system of a_1 etc
# We then rename into the new naming system, where it can be directly renamed. However, some cannot.

# Which cannot:
# rp.contact.field.survey_welcome_a_3_final, rp.contact.field.survey_welcome_a_4_final, rp.contact.field.survey_welcome_a_6_final, rp.contact.field.survey_welcome_a_7_part_1_final
# any after a7p1?

# Next steps here:
# did the question change for any of these. Need to check.
# p7a2, a3, 8, 9 - what were those questions before? what are they now? Check and update.
# what to do where the question changed?
# display differently for optimisation than for pilot if optimisation has different questions? Discuss.

plhdata_org_clean <- plhdata_org_clean %>%
  mutate(rp.contact.field.survey_welcome_ppf = ifelse(!is.na(rp.contact.field.survey_welcome_ppf), rp.contact.field.survey_welcome_ppf, rp.contact.field.survey_welcome_a_1_final),
         rp.contact.field.survey_welcome_ppp = ifelse(!is.na(rp.contact.field.survey_welcome_ppp), rp.contact.field.survey_welcome_ppp, rp.contact.field.survey_welcome_a_2_final),
         rp.contact.field.survey_welcome_fin_s = ifelse(!is.na(rp.contact.field.survey_welcome_fin_s), rp.contact.field.survey_welcome_fin_s, rp.contact.field.survey_welcome_a_5_part_1_final),
         rp.contact.field.survey_welcome_fin_fi = ifelse(!is.na(rp.contact.field.survey_welcome_fin_fi), rp.contact.field.survey_welcome_fin_fi, rp.contact.field.survey_welcome_a_5_part_2_final)) %>%
  mutate(rp.contact.field.survey_final_ppf = ifelse(!is.na(rp.contact.field.survey_final_ppf), rp.contact.field.survey_final_ppf, rp.contact.field.survey_final_a_1_final),
         rp.contact.field.survey_final_ppp = ifelse(!is.na(rp.contact.field.survey_final_ppp), rp.contact.field.survey_final_ppp, rp.contact.field.survey_final_a_2_final),
         rp.contact.field.survey_final_fin_s = ifelse(!is.na(rp.contact.field.survey_final_fin_s), rp.contact.field.survey_final_fin_s, rp.contact.field.survey_final_a_5_part_1_final),
         rp.contact.field.survey_final_fin_fi = ifelse(!is.na(rp.contact.field.survey_final_fin_fi), rp.contact.field.survey_final_fin_fi, rp.contact.field.survey_final_a_5_part_2_final))



# todo: following not working
#plhdata_org_clean <- plhdata_org_clean %>%
#  map2_df(.x = c("a_1_final", "a_2_final", "a_3_final"),
#          .y = c("ppf", "ppp", "ps"),
#          .f = ~version_variables_rename(old_name = .x, new_name = .y))

##plhdata_org_clean$rp.contact.field.survey_welcome_a_1_final[281:290]
##plhdata_org_clean$rp.contact.field.survey_welcome_ppf_v0.16.2[281:290]
#
#plhdata_org_clean$rp.contact.field.survey_welcome_a_2_final[281:290]
#plhdata_org_clean$rp.contact.field.survey_welcome_ppp_v0.16.2[281:290]##
#
#plhdata_org_clean$rp.contact.field.survey_welcome_a_3_final[281:290]
#plhdata_org_clean$rp.contact.field.survey_welcome_ps_v0.16.2[281:290]

# Tidying up for together/individual and modular/workshop skins
if (study == "Optimisation"){
  plhdata_org_clean_mod <- plhdata_org_clean %>% filter(rp.contact.field._app_skin == "modular")
  data_completion_level <- c("rp.contact.field.w_self_care_completion_level", "rp.contact.field.w_1on1_completion_level",  "rp.contact.field.w_praise_completion_level",
                             "rp.contact.field.w_instruct_completion_level",  "rp.contact.field.w_stress_completion_level",
                             "rp.contact.field.w_money_completion_level",  "rp.contact.field.w_rules_completion_level", #you have "safe_completion" under rules. Is this right?
                             "rp.contact.field.w_consequence_completion_level",  "rp.contact.field.w_solve_completion_level",  "rp.contact.field.w_safe_completion_level",
                             "rp.contact.field.w_crisis_completion_level",  "rp.contact.field.w_celebrate_completion_level")
  # Esmee - what is the definition of completion at the moment for workshop skin?
  # which rows.id do they have to have == true in in these to say they've completed?
  total_completed_ind <- NULL
  total_completed_tog <- NULL
  j = 0
  for (i in c("self_care", "1on1", "praise", "instruct", "stress", "money", "rules", "consequence", "solve", "safe", "crisis", "celebrate")){
    # which variables to select?
    
    json_data <- data.frame(jsonlite::fromJSON(paste0("~/GitHub/parenting-app-ui/packages/app-data/sheets/data_list/generated/w_", i, "_task_gs.json")))
    # rows.id, rows.individual, rows.together, rows.completed_field
    json_data <- json_data %>% dplyr::select(c(rows.id, rows.individual, rows.together, rows.completed_field)) %>%
      filter(!rows.id %in% c("home_practice", "hp_review"))
    json_data_ind <- json_data %>% filter(rows.individual == TRUE)
    completed_rows_ind <- paste0("rp.contact.field.", json_data_ind$rows.completed_field)
    json_data_tog <- json_data %>% filter(rows.together == TRUE)
    completed_rows_tog <- paste0("rp.contact.field.", json_data_tog$rows.completed_field)
    
    plhdata_org_clean_mod_inds <- plhdata_org_clean_mod %>%
      filter(rp.contact.field.workshop_path != "together") %>%
      dplyr::select(completed_rows_ind) %>%
      dplyr::mutate(across(everything(), ~as.numeric(as.logical(.)))) 
    plhdata_org_clean_mod_inds <- plhdata_org_clean_mod_inds %>%
      dplyr::mutate(total_completed := rowSums(., na.rm = TRUE)/length(.) * 100)
    
    plhdata_org_clean_mod_tog <- plhdata_org_clean_mod %>%
      filter(rp.contact.field.workshop_path == "together") %>%
      dplyr::select(completed_rows_tog) %>%
      dplyr::mutate(across(everything(), ~as.numeric(as.logical(.))))
    plhdata_org_clean_mod_tog <- plhdata_org_clean_mod_tog %>%
      dplyr::mutate(total_completed := rowSums(., na.rm = TRUE)/length(.) * 100)
    j = j + 1
    total_completed_ind[[j]] <- plhdata_org_clean_mod_inds$total_completed
    total_completed_tog[[j]] <- plhdata_org_clean_mod_tog$total_completed
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
                                                    rp.contact.field.w_money_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_money_completion_level.mod, rp.contact.field.w_money_completion_level),
                                                    rp.contact.field.w_rules_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_rules_completion_level.mod, rp.contact.field.w_rules_completion_level),
                                                    rp.contact.field.w_consequence_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_consequence_completion_level.mod, rp.contact.field.w_consequence_completion_level),
                                                    rp.contact.field.w_solve_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_solve_completion_level.mod, rp.contact.field.w_solve_completion_level),
                                                    rp.contact.field.w_safe_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_safe_completion_level.mod, rp.contact.field.w_safe_completion_level),
                                                    rp.contact.field.w_crisis_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_crisis_completion_level.mod, rp.contact.field.w_crisis_completion_level),
                                                    rp.contact.field.w_celebrate_completion_level = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_celebrate_completion_level.mod, rp.contact.field.w_celebrate_completion_level))
}

#head(plhdata_org_clean$rp.contact.field._app_skin)
#head(plhdata_org_clean$rp.contact.field.w_self_care_completion_level)
#head(xx$rp.contact.field.w_self_care_completion_level)


# More cleaning
# TODO: Add here any to make numeric. check with Esmee about w_self_care_diff_started_completed stored
plhdata_org_clean$rp.contact.field.survey_welcome_and_setup_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.survey_welcome_and_setup_completion_level)
plhdata_org_clean$rp.contact.field.user_age <- as.numeric(plhdata_org_clean$rp.contact.field.user_age)
plhdata_org_clean$rp.contact.field.household_adults <- as.numeric(plhdata_org_clean$rp.contact.field.household_adults)
plhdata_org_clean$rp.contact.field.household_teens <- as.numeric(plhdata_org_clean$rp.contact.field.household_teens)
plhdata_org_clean$rp.contact.field.household_babies <- as.numeric(plhdata_org_clean$rp.contact.field.household_babies)
plhdata_org_clean$rp.contact.field.household_children <- as.numeric(plhdata_org_clean$rp.contact.field.household_children)
plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed)

plhdata_org_clean$rp.contact.field.parent_point_count_relax_w_self_care <- as.numeric(plhdata_org_clean$rp.contact.field.parent_point_count_relax_w_self_care)

plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed)
plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(ends_with("_completion_level"), ~as.numeric(.)))
plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(starts_with("rp.contact.field.parent_point"), ~as.numeric(.)))
plhdata_org_clean$rp.contact.field.app_launch_count <- as.numeric(plhdata_org_clean$rp.contact.field.app_launch_count)

plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed)

plhdata_org_clean$rp.contact.field.first_app_open <- as.Date(plhdata_org_clean$rp.contact.field.first_app_open)
plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(starts_with("rp.contact.field.app_launch_count"), ~as.numeric(.)))



# Write clean data back -------------------------------------------------------

# Analysis - tables - separate for different groups.
# summary_table(columns_to_summarise = app_version, display_table = FALSE)


## Data Analysis ## --------------------------------------------------------
data_baseline_survey <- c("rp.contact.field.survey_welcome_completed", "rp.contact.field.user_gender",
                          "rp.contact.field.user_age", "rp.contact.field.household_adults",
                          "rp.contact.field.household_teens", "rp.contact.field.household_babies",
                          "rp.contact.field.household_children", "rp.contact.field._app_language", "app_version", "rp.contact.field.workshop_path")



#survey ------------------------------------------------------------------------------------

# workshop_path edits ----------
plhdata_org_clean <- plhdata_org_clean %>%
  mutate(rp.contact.field.workshop_path = ifelse(is.na(rp.contact.field.workshop_path_user_choice),
                                                 rp.contact.field.workshop_path,
                                                 ifelse(rp.contact.field.workshop_path_user_choice == "false",
                                                        "default",
                                                        rp.contact.field.workshop_path)))

# Variables Set up ---------------------------------------

# Tab 2 -------
data_completion_level <- c("rp.contact.field.w_self_care_completion_level", "rp.contact.field.w_1on1_completion_level",  "rp.contact.field.w_praise_completion_level",
                           "rp.contact.field.w_instruct_completion_level",  "rp.contact.field.w_stress_completion_level",
                           "rp.contact.field.w_money_completion_level",  "rp.contact.field.w_rules_completion_level", #you have "safe_completion" under rules. Is this right?
                           "rp.contact.field.w_consequence_completion_level",  "rp.contact.field.w_solve_completion_level",  "rp.contact.field.w_safe_completion_level",
                           "rp.contact.field.w_crisis_completion_level",  "rp.contact.field.w_celebrate_completion_level")
completion_vars <- c("Self Care", "One-on-one Time", "Praise", "Positive Instructions", "Managing Stress", "Family Budgets", "Rules", "Calm Consequences", "Problem Solving", "Teen Safety", "Dealing with Crisis","Celebration & Next Steps")



# TODO: add summary plot completion level in
#summary_plot_completion_level <- multiple_plot_output(columns_to_summarise = data_completion_level,
#                                                      replace = "rp.contact.field.w_", replace_after = "_completion_level")
#names(summary_plot_completion_level) <- completion_vars
#summary_plot_completion_level$`One-on-one Time`

# TAB 3 - PP -------------------------
# HABITS by workshop week ------------------------------------------------------------------------------------
data_habit_parent_points_all <- c("rp.contact.field.parent_point_count_relax", "rp.contact.field.parent_point_count_treat_yourself",
                                  "rp.contact.field.parent_point_count_praise_yourself", "rp.contact.field.parent_point_count_spend_time",
                                  "rp.contact.field.parent_point_count_praise_teen", "rp.contact.field.parent_point_count_instruct_positively", "rp.contact.field.parent_point_count_breathe",
                                  "rp.contact.field.parent_point_count_money", "rp.contact.field.parent_point_count_consequence", "rp.contact.field.parent_point_count_safe")

data_habit_parent_points_w_self_care <- c("rp.contact.field.parent_point_count_relax_w_self_care", "rp.contact.field.parent_point_count_treat_yourself_w_self_care", "rp.contact.field.parent_point_count_praise_yourself_w_self_care",
                                          "rp.contact.field.parent_point_count_spend_time_w_self_care", "rp.contact.field.parent_point_count_praise_teen_w_self_care",
                                          "rp.contact.field.parent_point_count_breathe_w_self_care", "rp.contact.field.parent_point_count_money_w_self_care",
                                          "rp.contact.field.parent_point_count_consequence_w_self_care", "rp.contact.field.parent_point_count_safe_w_self_care", "rp.contact.field.parent_point_count_instruct_positively_w_self_care")

data_habit_parent_points_w_1on1 <- c("rp.contact.field.parent_point_count_relax_w_1on1", "rp.contact.field.parent_point_count_treat_yourself_w_1on1",
                                     "rp.contact.field.parent_point_count_praise_yourself_w_1on1", "rp.contact.field.parent_point_count_spend_time_w_1on1",
                                     "rp.contact.field.parent_point_count_praise_teen_w_1on1", "rp.contact.field.parent_point_count_breathe_w_1on1",
                                     "rp.contact.field.parent_point_count_money_w_1on1", "rp.contact.field.parent_point_count_consequence_w_1on1",
                                     "rp.contact.field.parent_point_count_safe_w_1on1", "rp.contact.field.parent_point_count_instruct_positively_w_1on1")

data_habit_parent_points_w_praise <- c("rp.contact.field.parent_point_count_relax_w_praise", "rp.contact.field.parent_point_count_treat_yourself_w_praise",
                                       "rp.contact.field.parent_point_count_praise_yourself_w_praise", "rp.contact.field.parent_point_count_spend_time_w_praise",
                                       "rp.contact.field.parent_point_count_praise_teen_w_praise", "rp.contact.field.parent_point_count_breathe_w_praise",
                                       "rp.contact.field.parent_point_count_money_w_praise", "rp.contact.field.parent_point_count_consequence_w_praise",
                                       "rp.contact.field.parent_point_count_safe_w_praise", "rp.contact.field.parent_point_count_instruct_positively_w_praise")

data_habit_parent_points_w_instruct <- c("rp.contact.field.parent_point_count_relax_w_instruct", "rp.contact.field.parent_point_count_treat_yourself_w_instruct",
                                         "rp.contact.field.parent_point_count_praise_yourself_w_instruct", "rp.contact.field.parent_point_count_spend_time_w_instruct",
                                         "rp.contact.field.parent_point_count_praise_teen_w_instruct", "rp.contact.field.parent_point_count_breathe_w_instruct",
                                         "rp.contact.field.parent_point_count_money_w_instruct", "rp.contact.field.parent_point_count_consequence_w_instruct",
                                         "rp.contact.field.parent_point_count_safe_w_instruct", "rp.contact.field.parent_point_count_instruct_positively_w_instruct")

data_habit_parent_points_w_stress <- c("rp.contact.field.parent_point_count_relax_w_stress", "rp.contact.field.parent_point_count_treat_yourself_w_stress",
                                       "rp.contact.field.parent_point_count_praise_yourself_w_stress", "rp.contact.field.parent_point_count_spend_time_w_stress",
                                       "rp.contact.field.parent_point_count_praise_teen_w_stress", "rp.contact.field.parent_point_count_breathe_w_stress",
                                       "rp.contact.field.parent_point_count_money_w_stress", "rp.contact.field.parent_point_count_consequence_w_stress",
                                       "rp.contact.field.parent_point_count_safe_w_stress", "rp.contact.field.parent_point_count_instruct_positively_w_stress")

data_habit_parent_points_w_money <- c("rp.contact.field.parent_point_count_relax_w_money", "rp.contact.field.parent_point_count_treat_yourself_w_money",
                                      "rp.contact.field.parent_point_count_praise_yourself_w_money", "rp.contact.field.parent_point_count_spend_time_w_money",
                                      "rp.contact.field.parent_point_count_praise_teen_w_money", "rp.contact.field.parent_point_count_breathe_w_money",
                                      "rp.contact.field.parent_point_count_money_w_money", "rp.contact.field.parent_point_count_consequence_w_money",
                                      "rp.contact.field.parent_point_count_safe_w_money", "rp.contact.field.parent_point_count_instruct_positively_w_stress")

data_habit_parent_points_w_rules <- c("rp.contact.field.parent_point_count_relax_w_rules", "rp.contact.field.parent_point_count_treat_yourself_w_rules",
                                      "rp.contact.field.parent_point_count_praise_yourself_w_rules", "rp.contact.field.parent_point_count_spend_time_w_rules",
                                      "rp.contact.field.parent_point_count_praise_teen_w_rules", "rp.contact.field.parent_point_count_breathe_w_rules",
                                      "rp.contact.field.parent_point_count_money_w_rules", #"rp.contact.field.parent_point_count_consequence_w_rules",
                                      "rp.contact.field.parent_point_count_safe_w_rules", "rp.contact.field.parent_point_count_instruct_positively_w_rules")

data_habit_parent_points_w_consequence <- c("rp.contact.field.parent_point_count_relax_w_consequence", "rp.contact.field.parent_point_count_treat_yourself_w_consequence",
                                            "rp.contact.field.parent_point_count_praise_yourself_w_consequence", "rp.contact.field.parent_point_count_spend_time_w_consequence",
                                            "rp.contact.field.parent_point_count_praise_teen_w_consequence", "rp.contact.field.parent_point_count_breathe_w_consequence",
                                            #"rp.contact.field.parent_point_count_money_w_consequence",
                                            "rp.contact.field.parent_point_count_consequence_w_consequence",
                                            "rp.contact.field.parent_point_count_safe_w_consequence", "rp.contact.field.parent_point_count_instruct_positively_w_consequence")

data_habit_parent_points_w_solve <- c("rp.contact.field.parent_point_count_relax_w_solve", "rp.contact.field.parent_point_count_treat_yourself_w_solve",
                                      "rp.contact.field.parent_point_count_praise_yourself_w_solve", "rp.contact.field.parent_point_count_spend_time_w_solve",
                                      "rp.contact.field.parent_point_count_praise_teen_w_solve", "rp.contact.field.parent_point_count_breathe_w_solve",
                                      "rp.contact.field.parent_point_count_money_w_solve", "rp.contact.field.parent_point_count_consequence_w_solve",
                                      "rp.contact.field.parent_point_count_safe_w_solve", "rp.contact.field.parent_point_count_instruct_positively_w_solve")

data_habit_parent_points_w_safe <- c("rp.contact.field.parent_point_count_relax_w_safe", "rp.contact.field.parent_point_count_treat_yourself_w_safe",
                                     "rp.contact.field.parent_point_count_praise_yourself_w_safe", "rp.contact.field.parent_point_count_spend_time_w_safe",
                                     "rp.contact.field.parent_point_count_praise_teen_w_safe", "rp.contact.field.parent_point_count_breathe_w_safe",
                                     "rp.contact.field.parent_point_count_money_w_safe", "rp.contact.field.parent_point_count_consequence_w_safe",
                                     "rp.contact.field.parent_point_count_safe_w_safe", "rp.contact.field.parent_point_count_instruct_positively_w_safe")

data_habit_parent_points_w_crisis <- c("rp.contact.field.parent_point_count_relax_w_crisis", "rp.contact.field.parent_point_count_treat_yourself_w_crisis",
                                       "rp.contact.field.parent_point_count_praise_yourself_w_crisis", "rp.contact.field.parent_point_count_spend_time_w_crisis",
                                       "rp.contact.field.parent_point_count_praise_teen_w_crisis", "rp.contact.field.parent_point_count_breathe_w_crisis",
                                       #"rp.contact.field.parent_point_count_money_w_crisis", "rp.contact.field.parent_point_count_consequence_w_crisis",
                                       "rp.contact.field.parent_point_count_safe_w_crisis", "rp.contact.field.parent_point_count_instruct_positively_w_crisis")

data_habit_parent_points_w_celebrate <- c("rp.contact.field.parent_point_count_relax_w_celebrate", "rp.contact.field.parent_point_count_treat_yourself_w_celebrate",
                                          "rp.contact.field.parent_point_count_praise_yourself_w_celebrate", "rp.contact.field.parent_point_count_spend_time_w_celebrate",
                                          "rp.contact.field.parent_point_count_praise_teen_w_celebrate", "rp.contact.field.parent_point_count_breathe_w_celebrate",
                                          "rp.contact.field.parent_point_count_money_w_celebrate", "rp.contact.field.parent_point_count_consequence_w_celebrate",
                                          "rp.contact.field.parent_point_count_safe_w_celebrate", "rp.contact.field.parent_point_count_instruct_positively_w_celebrate")
#summary_plot_habits_all <- multiple_plot_output(columns_to_summarise = data_habit_parent_points_all, replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")
#summary_plot_habits_self_care <- multiple_plot_output(columns_to_summarise = data_habit_parent_points_all, replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")
#summary_plot_habits_1on1 <- multiple_plot_output(columns_to_summarise = data_habit_parent_points_w_1on1, replace = "rp.contact.field.parent_point_count_", replace_after = "w_1on1", plot_type = "boxplot")


##Priority 20
#App-opens
#Number of app opens within a workshop week for each user
data_app_opens <- c("rp.contact.field.app_launch_count","rp.contact.field.app_launch_count_w_self_care", "rp.contact.field.app_launch_count_w_1on1",
                    "rp.contact.field.app_launch_count_w_praise","rp.contact.field.app_launch_count_w_instruct",
                    "rp.contact.field.app_launch_count_w_stress", "rp.contact.field.app_launch_count_w_money",
                    "rp.contact.field.app_launch_count_w_rules", "rp.contact.field.app_launch_count_w_consequence",
                    "rp.contact.field.app_launch_count_w_solve", "rp.contact.field.app_launch_count_w_safe",
                    "rp.contact.field.app_launch_count_w_crisis", "rp.contact.field.app_launch_count_w_celebrate")

data_app_opens_neat <- c("Total", "Welcome and Self care(1)", "One-on-one time(2)", "Praise (3)", "Positive Instructions(4)",
                         "Managing Stress(5)", "Family Budget(6)","Rules(7)", "Calm Consequences(8)",  
                         "Problem Solving(9)", "Teen Safety(10)", "Crisis(11)", "Celebration & Next Steps(12)")


# Tab ?? ----------
#Define workshop week order
week_order <- c("Self care", "1on1", "Praise", "Instruct", "Stress", "Money", "Rules", "Consequence", "Solve", "Safe",
                "Crisis", "Celebrate" )

#Each habit across workshop weeks
#relax points in each week
relax_workshop_vars <- c( "rp.contact.field.parent_point_count_relax_w_self_care", "rp.contact.field.parent_point_count_relax_w_1on1",
                          "rp.contact.field.parent_point_count_relax_w_praise", "rp.contact.field.parent_point_count_relax_w_instruct",
                          "rp.contact.field.parent_point_count_relax_w_stress", "rp.contact.field.parent_point_count_relax_w_money",
                          "rp.contact.field.parent_point_count_relax_w_rules", "rp.contact.field.parent_point_count_relax_w_consequence",
                          "rp.contact.field.parent_point_count_relax_w_solve", "rp.contact.field.parent_point_count_relax_w_safe",
                          "rp.contact.field.parent_point_count_relax_w_crisis","rp.contact.field.parent_point_count_relax_w_celebrate")
# treat_yourself points in each week
treat_yourself_workshop_vars <- c( "rp.contact.field.parent_point_count_treat_yourself_w_self_care", "rp.contact.field.parent_point_count_treat_yourself_w_1on1",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_praise", "rp.contact.field.parent_point_count_treat_yourself_w_instruct",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_stress", "rp.contact.field.parent_point_count_treat_yourself_w_money",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_rules", "rp.contact.field.parent_point_count_treat_yourself_w_consequence",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_solve", "rp.contact.field.parent_point_count_treat_yourself_w_safe",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_crisis","rp.contact.field.parent_point_count_treat_yourself_w_celebrate")
# praise_yourself points in each week
praise_yourself_workshop_vars <- c( "rp.contact.field.parent_point_count_praise_yourself_w_self_care", "rp.contact.field.parent_point_count_praise_yourself_w_1on1",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_praise", "rp.contact.field.parent_point_count_praise_yourself_w_instruct",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_stress", "rp.contact.field.parent_point_count_praise_yourself_w_money",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_rules", "rp.contact.field.parent_point_count_praise_yourself_w_consequence",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_solve", "rp.contact.field.parent_point_count_praise_yourself_w_safe",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_crisis","rp.contact.field.parent_point_count_praise_yourself_w_celebrate")
# spend_time points in each week
spend_time_workshop_vars <- c( "rp.contact.field.parent_point_count_spend_time_w_self_care", "rp.contact.field.parent_point_count_spend_time_w_1on1",
                               "rp.contact.field.parent_point_count_spend_time_w_praise", "rp.contact.field.parent_point_count_spend_time_w_instruct",
                               "rp.contact.field.parent_point_count_spend_time_w_stress", "rp.contact.field.parent_point_count_spend_time_w_money",
                               "rp.contact.field.parent_point_count_spend_time_w_rules", "rp.contact.field.parent_point_count_spend_time_w_consequence",
                               "rp.contact.field.parent_point_count_spend_time_w_solve", "rp.contact.field.parent_point_count_spend_time_w_safe",
                               "rp.contact.field.parent_point_count_spend_time_w_crisis","rp.contact.field.parent_point_count_spend_time_w_celebrate")
# praise_teen in each week
praise_teen_workshop_vars <- c( "rp.contact.field.parent_point_count_praise_teen_w_self_care", "rp.contact.field.parent_point_count_praise_teen_w_1on1",
                                "rp.contact.field.parent_point_count_praise_teen_w_praise", "rp.contact.field.parent_point_count_praise_teen_w_instruct",
                                "rp.contact.field.parent_point_count_praise_teen_w_stress", "rp.contact.field.parent_point_count_praise_teen_w_money",
                                "rp.contact.field.parent_point_count_praise_teen_w_rules", "rp.contact.field.parent_point_count_praise_teen_w_consequence",
                                "rp.contact.field.parent_point_count_praise_teen_w_solve", "rp.contact.field.parent_point_count_praise_teen_w_safe",
                                "rp.contact.field.parent_point_count_praise_teen_w_crisis","rp.contact.field.parent_point_count_praise_teen_w_celebrate")
# instruct_positively points in each week
instruct_positively_workshop_vars <- c( "rp.contact.field.parent_point_count_instruct_positively_w_self_care", "rp.contact.field.parent_point_count_instruct_positively_w_1on1",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_praise", "rp.contact.field.parent_point_count_instruct_positively_w_instruct",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_stress", "rp.contact.field.parent_point_count_instruct_positively_w_money",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_rules", "rp.contact.field.parent_point_count_instruct_positively_w_consequence",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_solve", "rp.contact.field.parent_point_count_instruct_positively_w_safe",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_crisis","rp.contact.field.parent_point_count_instruct_positively_w_celebrate")
# breathe points in each week
breathe_workshop_vars <- c( "rp.contact.field.parent_point_count_breathe_w_self_care", "rp.contact.field.parent_point_count_breathe_w_1on1",
                            "rp.contact.field.parent_point_count_breathe_w_praise", "rp.contact.field.parent_point_count_breathe_w_instruct",
                            "rp.contact.field.parent_point_count_breathe_w_stress", "rp.contact.field.parent_point_count_breathe_w_money",
                            "rp.contact.field.parent_point_count_breathe_w_rules", "rp.contact.field.parent_point_count_breathe_w_consequence",
                            "rp.contact.field.parent_point_count_breathe_w_solve", "rp.contact.field.parent_point_count_breathe_w_safe",
                            "rp.contact.field.parent_point_count_breathe_w_crisis","rp.contact.field.parent_point_count_breathe_w_celebrate")
# money points in each week
money_workshop_vars <- c( "rp.contact.field.parent_point_count_money_w_self_care", "rp.contact.field.parent_point_count_money_w_1on1",
                          "rp.contact.field.parent_point_count_money_w_praise", "rp.contact.field.parent_point_count_money_w_instruct",
                          "rp.contact.field.parent_point_count_money_w_stress", "rp.contact.field.parent_point_count_money_w_money",
                          "rp.contact.field.parent_point_count_money_w_rules", #"rp.contact.field.parent_point_count_money_w_consequence",
                          "rp.contact.field.parent_point_count_money_w_solve", "rp.contact.field.parent_point_count_money_w_safe",
                          #"rp.contact.field.parent_point_count_money_w_crisis",
                          "rp.contact.field.parent_point_count_money_w_celebrate")
# consequence points in each week
consequence_workshop_vars <- c( "rp.contact.field.parent_point_count_consequence_w_self_care", "rp.contact.field.parent_point_count_consequence_w_1on1",
                                "rp.contact.field.parent_point_count_consequence_w_praise", "rp.contact.field.parent_point_count_consequence_w_instruct",
                                "rp.contact.field.parent_point_count_consequence_w_stress", "rp.contact.field.parent_point_count_consequence_w_money",
                                #"rp.contact.field.parent_point_count_consequence_w_rules", "rp.contact.field.parent_point_count_consequence_w_crisis",
                                "rp.contact.field.parent_point_count_consequence_w_consequence",
                                "rp.contact.field.parent_point_count_consequence_w_solve", "rp.contact.field.parent_point_count_consequence_w_safe",
                                "rp.contact.field.parent_point_count_consequence_w_celebrate")
# safe points in each week
safe_workshop_vars <- c( "rp.contact.field.parent_point_count_safe_w_self_care", "rp.contact.field.parent_point_count_safe_w_1on1",
                         "rp.contact.field.parent_point_count_safe_w_praise", "rp.contact.field.parent_point_count_safe_w_instruct",
                         "rp.contact.field.parent_point_count_safe_w_stress", "rp.contact.field.parent_point_count_safe_w_money",
                         "rp.contact.field.parent_point_count_safe_w_rules", "rp.contact.field.parent_point_count_safe_w_consequence",
                         "rp.contact.field.parent_point_count_safe_w_solve", "rp.contact.field.parent_point_count_safe_w_safe",
                         "rp.contact.field.parent_point_count_safe_w_crisis","rp.contact.field.parent_point_count_safe_w_celebrate")


data_all_weeks_pp_relax_neat <- naming_conventions(relax_workshop_vars, replace = "rp.contact.field.parent_point_count_relax_w_")
data_all_weeks_pp_treat_yourself_neat <- naming_conventions(treat_yourself_workshop_vars, replace = "rp.contact.field.parent_point_count_treat_yourself_w_")
data_all_weeks_pp_praise_yourself_neat <- naming_conventions(praise_yourself_workshop_vars, replace = "rp.contact.field.parent_point_count_praise_yourself_w_")
data_all_weeks_pp_spend_time_neat <- naming_conventions(spend_time_workshop_vars, replace = "rp.contact.field.parent_point_count_spend_time_w_")
data_all_weeks_pp_praise_teen_neat <- naming_conventions(praise_teen_workshop_vars, replace = "rp.contact.field.parent_point_count_praise_teen_w_")
data_all_weeks_pp_instruct_positively_neat <- naming_conventions(instruct_positively_workshop_vars, replace = "rp.contact.field.parent_point_count_instruct_positively_w_")
data_all_weeks_pp_breathe_neat <- naming_conventions(breathe_workshop_vars, replace = "rp.contact.field.parent_point_count_breathe_w_")
data_all_weeks_pp_money_neat <- naming_conventions(money_workshop_vars, replace = "rp.contact.field.parent_point_count_money_w_")
data_all_weeks_pp_consequence_neat <- naming_conventions(consequence_workshop_vars, replace = "rp.contact.field.parent_point_count_consequence_w_")
data_all_weeks_pp_safe_neat <- naming_conventions(safe_workshop_vars, replace = "rp.contact.field.parent_point_count_safe_w_")


# TODO: home practice needs to be moved into shinyapp like the other methods
## Home Practice ------------------------------------------------------------------

# home practice labels, NB two home practices for stress workshop (breathe and talk), but separate fields only exist for done and mood, not for started, challenges or completed
# completed field not included in analysis as it's a bit redundant (HP is completed when started, done, mood, and challenge are completed)

## create neat labels for HP variables - not used because replace and replace.after are sufficent to create labels and removes need for dummy when one workshop doesn't have the value
# hp_vars_done_mood <- c("One-on-one Time", "Praise", "Positive Instructions", "Stress - Breathe", "Stress - Talk", "Family Budgets", "Rules", "Calm Consequences", "Problem Solving", "Teen Safety", "Dealing with Crisis","Celebration & Next Steps")
# hp_vars_started_chall <- c("One-on-one Time", "Praise", "Positive Instructions", "Stress", "Family Budgets", "Rules", "Calm Consequences", "Problem Solving", "Teen Safety", "Dealing with Crisis","Celebration & Next Steps")

data_hp_started <- c("rp.contact.field.w_1on1_hp_review_started",  "rp.contact.field.w_praise_hp_review_started",
                     "rp.contact.field.w_instruct_hp_review_started",  "rp.contact.field.w_stress_hp_review_started",
                     "rp.contact.field.w_money_hp_review_started",  "rp.contact.field.w_rules_hp_review_started",
                     "rp.contact.field.w_consequence_hp_review_started",  "rp.contact.field.w_solve_hp_review_started",  "rp.contact.field.w_safe_hp_review_started",
                     "rp.contact.field.w_crisis_hp_review_started")

data_hp_done <- c("rp.contact.field.w_1on1_hp_done", "rp.contact.field.w_praise_hp_done", "rp.contact.field.w_instruct_hp_done", "rp.contact.field.w_stress_hp_breathe_done", "rp.contact.field.w_stress_hp_talk_done",
                  "rp.contact.field.w_money_hp_done", "rp.contact.field.w_rules_hp_done", "rp.contact.field.w_consequence_hp_done",
                  "rp.contact.field.w_solve_hp_done", "rp.contact.field.w_safe_hp_done", "rp.contact.field.w_crisis_hp_done")

# NB No mood 'review' for week 3 home practice (praise)
data_hp_mood <- c("rp.contact.field.w_1on1_hp_mood", "rp.contact.field.w_instruct_hp_mood", "rp.contact.field.w_stress_hp_breathe_mood", "rp.contact.field.w_stress_hp_talk_mood",
                  "rp.contact.field.w_money_hp_mood", "rp.contact.field.w_rules_hp_mood", "rp.contact.field.w_consequence_hp_mood",
                  "rp.contact.field.w_solve_hp_mood", "rp.contact.field.w_safe_hp_mood", "rp.contact.field.w_crisis_hp_mood") 

# TODO: this should work in function
plhdata_org_clean <- add_na_variable(variable = data_hp_started)
plhdata_org_clean <- add_na_variable(variable = data_hp_done)
plhdata_org_clean <- add_na_variable(variable = data_hp_mood)

challenge_vars <- c("rp.contact.field.w_1on1_hp_challenge_list", "rp.contact.field.w_instruct_hp_challenge_list",
                    "rp.contact.field.w_stress_hp_challenge_list", "rp.contact.field.w_money_hp_challenge_list",
                    "rp.contact.field.w_rules_hp_challenge_list", "rp.contact.field.w_consequence_hp_challenge_list", 
                    "rp.contact.field.w_solve_hp_challenge_list", "rp.contact.field.w_safe_hp_challenge_list", 
                    "rp.contact.field.w_crisis_hp_challenge_list")
chall_ap_vars <- c("rp.contact.field.w_1on1_hp_challenge", "rp.contact.field.w_instruct_hp_challenge",
                   "rp.contact.field.w_stress_hp_challenge", "rp.contact.field.w_money_hp_challenge",
                   "rp.contact.field.w_rules_hp_challenge", "rp.contact.field.w_consequence_hp_challenge", 
                   "rp.contact.field.w_solve_hp_challenge", "rp.contact.field.w_safe_hp_challenge", 
                   "rp.contact.field.w_crisis_hp_challenge")

# NB No challenge for week 3 home practice (praise)
data_hp_chall <- c("hp_list_challenges_1on1", "hp_list_challenges_instruct", "hp_list_challenges_stress", "hp_list_challenges_money", "hp_list_challenges_rules",
                   "hp_list_challenges_consequence", "hp_list_challenges_solve", "hp_list_challenges_safe", "hp_list_challenges_crisis")

# overview table for home practice review started: number of users per home practice who reached first screen, i.e. only "true" not "false" or NA
data_hp_started_neat <- naming_conventions(data_hp_started, replace = "rp.contact.field.w_", replace_after = "_review_started")
summary_table_hp_started <- multiple_table_output(plhdata_org_clean, data_hp_started) 
names(summary_table_hp_started) <- data_hp_started_neat

table_hp_started_long <- plyr::ldply(summary_table_hp_started) #could be the table used for teh plot to show true, false and NA for each HP review
if (!"true" %in% colnames(table_hp_started_long)) { table_hp_started_long$true <- 0 }
table_hp_started <- table_hp_started_long %>% pivot_wider(id_cols = Org, names_from = .id, values_from = c(true))
#how to call in Rshiny: summary_table_hp_started$`1on1 hp` etc


# home practice review - challenges selected for each workshop
#summary_table_hp_chall <- multiple_table_output(plhdata_org_clean, data_hp_chall, replace = "hp_list_")
# summary_table_hp_chall$___

# parent library ------------------------------------------------------------------
data_library <- c("rp.contact.field.click_hs_parent_centre_count", "rp.contact.field.click_pc_help_count",
                  "rp.contact.field.click_pc_my_tips_count", "rp.contact.field.click_pc_essential_tools_count",
                  "rp.contact.field.click_pc_covid_count", "rp.contact.field.click_pc_customisation_count",
                  "rp.contact.field.click_pc_relax_and_activities_count", "rp.contact.field.click_pc_support_contacts_count",
                  "rp.contact.field.click_pc_evidence_base_count", "rp.contact.field.click_pc_technical_support_count",
                  "rp.contact.field.click_pc_message_archive_count","rp.contact.field.click_pc_bereavement_count")




#Test 2 Priority 22 (how to interpret data?)
#Number of in-app message clicks per workshop week.Per quick start button, per workshop week 
#Workshops
plhdata_org_clean$hsqsclickedws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care)


data_weekly_workshops <- c("rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care",
                           "hsqsclickedws1", "rp.contact.field.click_hs_weekly_workshops_quick_start_w_1on1",
                           "rp.contact.field.click_hs_weekly_workshops_quick_start_w_praise", "rp.contact.field.click_hs_weekly_workshops_quick_start_w_instruct",
                           "rp.contact.field.click_hs_weekly_workshops_quick_start_w_stress", "rp.contact.field.click_hs_weekly_workshops_quick_start_w_money",
                           "rp.contact.field.click_hs_weekly_workshops_quick_start_w_rules", "rp.contact.field.click_hs_weekly_workshops_quick_start_w_consequence", 
                           "rp.contact.field.click_hs_weekly_workshops_quick_start_w_solve", "rp.contact.field.click_hs_weekly_workshops_quick_start_w_safe",
                           "rp.contact.field.click_hs_weekly_workshops_quick_start_w_crisis", "rp.contact.field.click_hs_weekly_workshops_quick_start_w_celebrate",
                           "rp.contact.field.click_hs_parent_centre_quick_start_w_self_care", "rp.contact.field.click_hs_parent_centre_quick_start_w_1on1",
                           "rp.contact.field.click_hs_parent_points_quick_start_w_self_care", "rp.contact.field.click_hs_parent_points_quick_start_w_1on1")

data_weekly_workshops_neat <- c("Self care", "In app reminders", "One-on-one time", "Praise", "Positive instructions", "Stress",
                                "Family Budgets", "Rules", "Calm consequence", "Problem solving", "Teen safety", "Dealing with crisis",
                                "Celebration and Next Steps", "Parent center - Self care", "Parent center - One-on-one time", "Parent points - Self care", "Parent points - One-on-one time")

summary_tableweekly_workshops <- multiple_table_output(columns_to_summarise = data_weekly_workshops)
names(summary_tableweekly_workshops) <- data_weekly_workshops_neat
# summary_tableweekly_workshops$Stress


#In-app reminders(Number of in-app message clicks per workshop week),Per quick start button, per workshop week  -------------------------
# TODO: hsqsclickedws1, hsqsclickedws2 is defined twice, differently each time. Should have different names - is this intentional?
plhdata_org_clean$hsqsclickedws1<-is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care)
plhdata_org_clean$hsqsclickedws2<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_1on1)

# summary_table(columns_to_summarise = hsqsclickedws1)
# summary_table(columns_to_summarise = hsqsclickedws2)

#Test 1
#plhdata_org_clean$hsqsclickedws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care)
#plhdata_org_clean$hsqsclickedcountws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care)
#plhdata_org_clean$hsqsclickedws2<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_1on1)
# summary_table(columns_to_summarise = rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care, replace = "rp.contact.field.click_hs_")
# user_id_print("rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care")

# summary_table(columns_to_summarise = hsqsclickedws1)
# summary_table(columns_to_summarise = hsqsclickedcountws1)
# summary_table(columns_to_summarise = hsqsclickedws2)

#Priority 23 
#In-app reminders
#Total number of in-app message clicks.By User
#Weekly workshops
# summary_table(columns_to_summarise = rp.contact.field.click_hs_weekly_workshops_quick_start_count, replace = "rp.contact.field.click_hs_")
# 
# #Parent center
# summary_table(columns_to_summarise = rp.contact.field.click_hs_parent_centre_quick_start_count, replace = "rp.contact.field.click_hs_")
# 
# #Parent points
# summary_table(columns_to_summarise = rp.contact.field.click_hs_parent_points_quick_start_count, replace = "rp.contact.field.click_hs_")


#Priority 19
#App-opens
#Total number of app-opens for each user(cumulative)
#app_open_summary <- plhdata_org_clean %>%
#  group_by(Org) %>%
#  summarise(sum(rp.contact.field.app_launch_count))
# user_id_print("rp.contact.field.app_launch_count")

##Priority 21
#App-opens
#Maximum time between app-opens - for each user.Time in full days 
plhdata_org_clean$rp.contact.field.max_days_between_app_launches <- as.numeric(plhdata_org_clean$rp.contact.field.max_days_between_app_launches)
# summary_table(columns_to_summarise = rp.contact.field.max_days_between_app_launches, replace = "rp.contact.field.")

#Priority 36 --------------------------------------------------------------------------------
#Emotional Check-in
#Rate of users who respond "happy" ,"sad" & "ok"
data_emotional_check_in <- c("rp.contact.field.w_self_care_welcome_individual_a_final", "rp.contact.field.w_1on1_welcome_individual_a_final", 
                             "rp.contact.field.w_praise_welcome_individual_a_final", "rp.contact.field.w_instruct_welcome_individual_a_final", 
                             "rp.contact.field.w_stress_welcome_individual_a_final", "rp.contact.field.w_money_welcome_individual_a_final", 
                             "rp.contact.field.w_rules_welcome_individual_a_final")
data_emotional_check_in_neat <- c("Self care", "One-on-one time", "Praise", "Positive instructions", "Managing stress", "Family budgets", "Rules")
tables_emotional_check_in <- multiple_table_output(columns_to_summarise = data_emotional_check_in)
names(tables_emotional_check_in) <- data_emotional_check_in_neat
# tables_emotional_check_in$`Self care`


# Completion rate of introductory session(Workshop 1:Selfcare) ------------------------------------------------
# plhdata_org_clean %>%
#   split(.$Org) %>%
#   map(~summary_table(data = .x, factor = NULL, columns_to_summarise = rp.contact.field.w_self_care_completed, replace = "rp.contact.field.survey"))
#plhdata_org_clean %>% group_by(Org) %>% select('app_user_id', "rp.contact.field.w_self_care_completed")
#plhdata_org_clean %>% group_by(Org) %>% select('app_user_id', "rp.contact.field.w_self_care_started")
#plhdata_org_clean %>% group_by(Org) %>% select('app_user_id', "rp.contact.field.w_1on1_started")

# Completion status of baseline survey ------------------------------------------------
# plhdata_org_clean %>%
#   split(.$Org) %>%
#   map(~summary_table(data = .x, factor = NULL, columns_to_summarise = rp.contact.field.survey_welcome_complppplheted, replace = "rp.contact.field.survey"))

#Error as rp.contact.field.parent_point_count_praise_teen_w_celebrate does not exist
#Error as Column `rp.contact.field.parent_point_count_money_w_crisis` doesn't exist.
#NB error as Column `rp.contact.field.parent_point_count_consequence_w_money` doesn't exist.
#the tables seems to require all values to exist at least once in order to create the table for any of the parent points in that week.
#false: more likely issue is that R truncated some long contact field names and now cannot find them...
# for now:  data_habit_parent_points_w_money <- data_habit_parent_points_w_money[-8]
#NB error as Column `parent_point_count_consequence_w_rules` doesn't exist.
# for now:  data_habit_parent_points_w_rules <- data_habit_parent_points_w_rules[-8]
#Error as Column `rp.contact.field.parent_point_count_money_w_consequence` doesn't exist.


#data_survey_past_week <- c("rp.contact.field.survey_welcome_a_1_final",  "rp.contact.field.survey_welcome_a_2_final",
#                           "rp.contact.field.survey_welcome_a_3_final",  "rp.contact.field.survey_welcome_a_4_final",
#                           "rp.contact.field.survey_welcome_a_5_part_1_final",  "rp.contact.field.survey_welcome_a_5_part_2_final",
#                           "rp.contact.field.survey_welcome_a_6_final",  "rp.contact.field.survey_welcome_a_7_part_1_final",
#                           "rp.contact.field.survey_welcome_a_7_part_2_final",  "rp.contact.field.survey_welcome_a_7_part_3_final",
#                           "rp.contact.field.survey_welcome_a_8_final", "rp.contact.field.survey_welcome_a_9_final")
#survey_vars <- c("Attention", "Praise", "Stress", "Shouting", "Money worries", "Summary", "Hitting", "Teen activity", "Lockdown?", "Knowledge of teen activity in non-lockdown week",
#                 "Sexual safety talk", "Teen COVID safe")
#summary_table_survey_past_week <- plhdata_org_clean %>%
#  map(.x = data_survey_past_week, .f = ~summary_table(columns_to_summarise = .x, display = FALSE, include_margins = TRUE))
#names(summary_table_survey_past_week) <- survey_vars


# Descriptive Statistics ------------------------------------------------------------------------------------------
# Gender of App Users
#gender_table <- plhdata_org_clean %>%
#  split(.$Org) %>%
#  map(~summary_table(data = .x, factor = NULL, columns_to_summarise = rp.contact.field.user_gender))
#names(gender_table) <- levels(plhdata_org_clean$Org)
# gender_table$Nontobeko
# gender_table$Amathuba
# gender_table


# Age of App Users
# summary_table(columns_to_summarise = rp.contact.field.user_age, summaries = "mean")


#Trials-----------------
# plhdata_org_clean %>% select('app_user_id', "rp.contact.field.user_age")
# plhdata_org_clean %>% select('app_user_id', "rp.contact.field.user_gender")
# plhdata_org_clean %>% filter(Org == "Amathuba") %>% select('app_user_id', "rp.contact.field.user_gender")
# mean(x=as.numeric(plhdata_org_clean$rp.contact.field.user_age), na.rm=TRUE)


# Survey - past week  ----------------------------------------------------------------------------
r_variables_names <- readxl::read_excel("r_variables_names.xlsx")
data_survey_past_week_all <- r_variables_names %>% filter(location_ID == "survey_past_week")

##################################
##################################
### Notification Data Analysis ###
##################################
##################################

# download push notification data
# TODO: add fuzzy join to get_nf_data function
#nf_data <- get_nf_data(site = plh_con) #, UIC_Tracker = UIC.Tracker)
# 
nf_data <- nf_data1 #get_nf_data(site = plh_con) #, UIC_Tracker = UIC.Tracker)

# # what variables do we want in the nf data - org, sex, - add a few in.
data_baseline_nf <-
  c(
    "Org",
    "rp.contact.field.survey_welcome_completed",
    "rp.contact.field.user_gender",
    "rp.contact.field.user_age",
    "rp.contact.field.household_adults",
    "rp.contact.field.household_teens",
    "rp.contact.field.household_babies",
    "rp.contact.field.household_children",
    "rp.contact.field._app_language",
    "app_version",
    "rp.contact.field.workshop_path"
  )
plhdata_org_clean_select <- plhdata_org_clean %>%
  dplyr::select(c(app_user_id, data_baseline_nf))


# Notification Data ---------------------------
# link nf data to user data by app_user_id
# use inner_join: remove from nf anyone not in plhdata_org
nf_data_join <- inner_join(nf_data, plhdata_org_clean_select)

#pn_summary_count <- nf_data_join %>%
#  group_by(app_user_id, Org, rp.contact.field._app_language) %>%
#  summarise(
#    number_received = max(app_user_record_id),
#    number_responded = sum(!is.na(action_id)),
#    percentage_responded = number_responded / number_received *
#      100
#  )

# If we were to use all of the nf_data (except the "temp_" rows)
# nf_data_summary <- nf_data %>%
#   filter(!grepl("temp", app_user_id)) %>% # remove the "temps"
#   group_by(app_user_id) %>%
#   summarise(
#     number_received = max(app_user_record_id),
#     number_responded = sum(!is.na(action_id)),
#     percentage_responded = number_responded / number_received *
#       100
#   )

