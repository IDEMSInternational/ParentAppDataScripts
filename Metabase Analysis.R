
### extract data ----------------------------------------------------------------------
plhdata_org <- get_user_data(merge_check = FALSE) # select 1 if you want to merge in changes (yes)

## Data Cleaning ## --------------------------------------------------------

## Tidy up "Organisation" Variable:
# replace missing values in Organisation and rp.contact.field.organisation_code by Miss so that it is a factor level
plhdata_org$Organisation <- forcats::as_factor(tidyr::replace_na(plhdata_org$Organisation, "Miss"))


# Question: What about "null"?
plhdata_org$rp.contact.field.organisation_code <- forcats::as_factor(tidyr::replace_na(plhdata_org$rp.contact.field.organisation_code, "Miss"))

# look and Recode Factor organisation_full to just the main levels
plhdata_org$rp.contact.field.organisation_code<-as_factor(replace_na(plhdata_org$rp.contact.field.organisation_code, "Miss"))

# Combine Factors Organisation and rp.contact.field.organisation_code 
plhdata_org$organisation_full <- interaction(x=list(plhdata_org$Organisation,
                                                    plhdata_org$rp.contact.field.organisation_code), drop=TRUE)

# look and Recode Factor organisation_full to just the main levels
#sjmisc::frq(x=plhdata_org$organisation_full, out="txt")
plhdata_org$Org <- plyr::revalue(x=plhdata_org$organisation_full, 
                                 replace=c(`Miss.Miss` =  "Other", `Miss.baba` = "Other", `Miss.w` = "Other", `Miss.idems` = "Other",  `Miss.hillcrest` = "Other", `Miss.aqujhk,jafvh` = "Other", `Miss.ParentApp_dev` = "Other", `Miss.CWBSA` = "Other",
                                           `Miss.idems Margherita` = "Other", `Miss.IDEMS Ohad` = "Other", `Miss.983aba50330cf24c` ="Other", `Miss.sdfds`="Other",  `Miss.friend` ="Other", `Miss.myself` ="Other", `Miss.undefined` ="Other",
                                           `Miss.other` ="Other", `Miss.zlto` ="Other", `Miss.hpccc` ="Other", `Miss.seven_passes` ="Other", `Miss.Hillcrest facilitator` ="Other", `Miss.Hillcrest Facilitator ` ="Other", `Miss.a00af0c3b3887330` ="Other",
                                           `Nontobeko.Miss` = "Nontobeko", `Nontobeko.Nontobeko M` = "Nontobeko", `Nontobeko.bbe9ca70c78f7384` = "Nontobeko",  `Nontobeko.nontobekoM` = "Nontobeko",
                                           `Nontobeko.NontobekoM` = "Nontobeko", `Nontobeko.null` ="Nontobeko", `Miss.NontobekoM` = "Nontobeko", 
                                           `Joy.Miss` = "Joy", `Joy.c9097349f34b364c` ="Joy", `Joy.null` ="Joy",
                                           `Dlalanathi.Miss` = "Dlalanathi",  `Dlalanathi.null` = "Dlalanathi", `Miss.dlalanathiThandeka` = "Dlalanathi",  `Dlalanathi.dlanathiThandeka` ="Dlalanathi",
                                           `Dlalanathi.dlalanathThandeka` ="Dlalanathi", `Dlalanathi.dlalanathiThandeka` ="Dlalanathi", `Dlalanathi.dlalanathi` ="Dlalanathi", `Dlalanathi.dlalanithi Thandeka` ="Dlalanathi", 
                                           `Amathuba Collective.Miss` ="Amathuba", `Miss.Amathuba Mzi` ="Amathuba", `Miss.Amathuba Mzi ` ="Amathuba", `Miss.amathuba` ="Amathuba", `Miss.dlalanathi`="Dlalanathi" ))

# so do the Miss. to Other first: [no longer commented out 14 March '22 by Margherita]
# plhdata_org <- plhdata_org %>%
#  mutate(Org = ifelse(Organisation == "Miss", "Other",
#                      ifelse(Organisation == "Dlalanathi", "Dlalanathi",
#                             ifelse(rp.contact.field.organisation_code == "dlalanathiThandeka", "Dlalanathi",
#                                    ifelse(Organisation == "Nontobeko", "Nontobeko",
#                                           ifelse(Organisation == "Joy", "Joy",
#                                                  ifelse(Organisation == "Amathuba Collective", "Amathuba",
#                                                         ifelse(rp.contact.field.organisation_code == "Amathuba Mzi ", "Amathuba",
#                                                                ifelse(rp.contact.field.organisation_code == "Amathuba Mzi", "Amathuba",
#                                                                       ifelse(rp.contact.field.organisation_code == "amathuba", "Amathuba",
#                                                                              paste(Organisation, rp.contact.field.organisation_code, sep = ".")))))))))))

# Look at the organisation data
sjmisc::frq(x=plhdata_org$Org, out="txt")

#####Create a subset for cleaned organisations ####
# TODO: none are called Miss in "Org" due to how you defined it
plhdata_org_clean <- plhdata_org %>%
  filter(Org != "Other")%>%
  mutate(Org = factor(Org))

# Create subsets of the data based on valid app user ID's
plhdata_org_clean <- plhdata_org_clean %>%
  dplyr::filter(!is.na(app_version))

# Look at the numbers per organisation from clear data 
sjmisc::frq(x=plhdata_org_clean$Org, out="txt")

# More cleaning
# TODO: Add here any to make numeric. check with Esmee about w_self_care_diff_started_completed stored
plhdata_org_clean$rp.contact.field.user_age <- as.numeric(plhdata_org_clean$rp.contact.field.user_age)
plhdata_org_clean$rp.contact.field.household_adults <- as.numeric(plhdata_org_clean$rp.contact.field.household_adults)
plhdata_org_clean$rp.contact.field.household_teens <- as.numeric(plhdata_org_clean$rp.contact.field.household_teens)
plhdata_org_clean$rp.contact.field.household_babies <- as.numeric(plhdata_org_clean$rp.contact.field.household_babies)
plhdata_org_clean$rp.contact.field.household_children <- as.numeric(plhdata_org_clean$rp.contact.field.household_children)
plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed)

plhdata_org_clean$rp.contact.field.parent_point_count_relax_w_self_care <- as.numeric(plhdata_org_clean$rp.contact.field.parent_point_count_relax_w_self_care)

plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed)
plhdata_org_clean$rp.contact.field.w_self_care_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_self_care_completion_level)
plhdata_org_clean$rp.contact.field.w_1on1_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_1on1_completion_level)
plhdata_org_clean$rp.contact.field.w_praise_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_praise_completion_level)
plhdata_org_clean$rp.contact.field.w_instruct_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_instruct_completion_level)
plhdata_org_clean$rp.contact.field.w_stress_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_stress_completion_level)
plhdata_org_clean$rp.contact.field.w_money_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_money_completion_level)
plhdata_org_clean$rp.contact.field.w_rules_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_rules_completion_level)
plhdata_org_clean$rp.contact.field.w_consequence_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_consequence_completion_level)
plhdata_org_clean$rp.contact.field.w_solve_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_solve_completion_level)
plhdata_org_clean$rp.contact.field.w_safe_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_safe_completion_level)
plhdata_org_clean$rp.contact.field.w_crisis_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_crisis_completion_level)
plhdata_org_clean$rp.contact.field.w_celebrate_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_celebrate_completion_level)

plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(ends_with("_completion_level"), ~as.numeric(.)))
plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(starts_with("rp.contact.field.parent_point"), ~as.numeric(.)))
plhdata_org_clean$rp.contact.field.app_launch_count <- as.numeric(plhdata_org_clean$rp.contact.field.app_launch_count)

plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed)


# Write clean data back -------------------------------------------------------

# Analysis - tables - separate for different groups.
summary_table(columns_to_summarise = app_version, display_table = FALSE)


## Data Analysis ## --------------------------------------------------------

# Summary tables of started/completed things
# Show the summary of Self care workshop started(1st Workshop)                            
summary_table(columns_to_summarise = rp.contact.field.w_self_care_started)

#plhdata_org_clean%>%dplyr::filter(Nontobeko,Joy,Amathuba,Dlalanathi)%>%summary_table(columns_to_summarise = rp.contact.field.w_self_care_started)

# Show the summary of Self care workshop completion(1st Workshop)
summary_table(columns_to_summarise = rp.contact.field.w_self_care_completed)

# Show the summary of One-on-One Time started(2nd Workshop)
summary_table(columns_to_summarise = rp.contact.field.w_1on1_started)

# Show the summary of  One-on-One Time completion(2nd Workshop)
summary_table(columns_to_summarise = rp.contact.field.w_1on1_completed)

# Show the summary of Praise started(3rd workshop) 
summary_table(columns_to_summarise = rp.contact.field.w_praise_started)

# Show the summary of Praise(3rd Workshop)
summary_table(columns_to_summarise = rp.contact.field.w_praise_completed)

## Show the summary of Positive Instructions(4th workshop)
summary_table(columns_to_summarise = rp.contact.field.w_instruct_started)

# Show the summary of Positive Instructions(4th workshop)
summary_table(columns_to_summarise = rp.contact.field.w_instruct_completed)

# Show the summary of Managing Stress(5th workshop)
summary_table(columns_to_summarise = rp.contact.field.w_stress_started)

# Show the summary of Managing Stress(5th workshop)
summary_table(columns_to_summarise = rp.contact.field.w_stress_completed)

# Show the summary of Family Budgets(6th workshop)
summary_table(columns_to_summarise = rp.contact.field.w_money_started)

# Show the summary of Family Budgets(6th workshop)
summary_table(columns_to_summarise = rp.contact.field.w_money_completed)

#survey ------------------------------------------------------------------------------------
data_baseline_survey <- c("rp.contact.field.survey_welcome_completed", "rp.contact.field.user_gender",
                          "rp.contact.field.user_age", "rp.contact.field.household_adults",
                          "rp.contact.field.household_teens", "rp.contact.field.household_babies",
                          "rp.contact.field.household_children", "rp.contact.field._app_language", "app_version", "rp.contact.field.do_workshops_together")
baseline_names_neat <- naming_conventions(data_baseline_survey, replace = "rp.contact.field.")

#TO DO: replace "NA" with "unknown" for nicer display in Shiny

summary_table_baseline <- plhdata_org_clean %>%
  map(.x = data_baseline_survey, .f = ~replace_na(.x, "unknown"))  %>%
  map(.x = data_baseline_survey, .f = ~summary_table(columns_to_summarise = .x, display = FALSE, include_margins = TRUE, summaries = "frequencies"))
names(summary_table_baseline) <- baseline_names_neat
summary_table_baseline$`Household babies`
summary_table_baseline$` app language`
summary_table_baseline$`App version`
summary_table_baseline$`Do workshops together`
summary_table_baseline$`Household adults`

#summary_table_baseline$`User gender`  %>% filter(Org %in% c(("Dlalanathi"))) %>%
#  pivot_wider(names_from = `User gender`, values_from = N)

summary_plot_baseline <- plhdata_org_clean %>%
  map(.x = data_baseline_survey, .f = ~summary_plot(columns_to_summarise = .x))
names(summary_plot_baseline) <- baseline_names_neat
summary_plot_baseline$`Survey welcome completed`


###Completion status of baseline survey
# Show the summary of baseline survey completion(Organisaton-wise)

summary_table(plhdata_org_clean %>% filter(Org == "Nontobeko"),
              factor = NULL,
              columns_to_summarise = rp.contact.field.survey_welcome_completed)

# Show the summary of user ids on baseline survey completion (Organisaton-wise)
user_id_print("rp.contact.field.survey_welcome_completed")


#Time spent on the workshops -----------------------------------------------------------------------
# TODO: sort NAs in these:
#Self care workshop
summary_table(columns_to_summarise = rp.contact.field.w_self_care_diff_started_completed)

#One on one time workshop
summary_table(columns_to_summarise = rp.contact.field.w_1on1_diff_started_completed)

# app language --------------------------------------------------
summary_table(columns_to_summarise = rp.contact.field._app_language, replace = "rp.contact.field.")
user_id_print("rp.contact.field._app_language")
# TODO: factor levels? that should be done in cleaning step.

# HABITS ------------------------------------------------------------------------------------
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
                                      "rp.contact.field.parent_point_count_money_w_rules", "rp.contact.field.parent_point_count_consequence_w_rules",
                                      "rp.contact.field.parent_point_count_safe_w_rules", "rp.contact.field.parent_point_count_instruct_positively_w_rules")

data_habit_parent_points_w_consequence <- c("rp.contact.field.parent_point_count_relax_w_consequence", "rp.contact.field.parent_point_count_treat_yourself_w_consequence",
                                            "rp.contact.field.parent_point_count_praise_yourself_w_consequence", "rp.contact.field.parent_point_count_spend_time_w_consequence",
                                            "rp.contact.field.parent_point_count_praise_teen_w_consequence", "rp.contact.field.parent_point_count_breathe_w_consequence",
                                            "rp.contact.field.parent_point_count_money_w_consequence", "rp.contact.field.parent_point_count_consequence_w_consequence",
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
                                       "rp.contact.field.parent_point_count_money_w_crisis", "rp.contact.field.parent_point_count_consequence_w_crisis",
                                       "rp.contact.field.parent_point_count_safe_w_crisis", "rp.contact.field.parent_point_count_instruct_positively_w_crisis")

data_habit_parent_points_w_celebrate <- c("rp.contact.field.parent_point_count_relax_w_celebrate", "rp.contact.field.parent_point_count_treat_yourself_w_celebrate",
                                          "rp.contact.field.parent_point_count_praise_yourself_w_celebrate", "rp.contact.field.parent_point_count_spend_time_w_celebrate",
                                          "rp.contact.field.parent_point_count_praise_teen_w_celebrate", "rp.contact.field.parent_point_count_breathe_w_celebrate",
                                          "rp.contact.field.parent_point_count_money_w_celebrate", "rp.contact.field.parent_point_count_consequence_w_celebrate",
                                          "rp.contact.field.parent_point_count_safe_w_celebrate", "rp.contact.field.parent_point_count_instruct_positively_w_celebrate")


data_habit_parent_points_all_neat <- naming_conventions(data_habit_parent_points_all, replace = "rp.contact.field.parent_point_count_")
data_habit_parent_points_w_self_care_neat <- naming_conventions(data_habit_parent_points_w_self_care, replace = "rp.contact.field.parent_point_count_", replace_after = "_w_self_care")
data_habit_parent_points_w_1on1_neat <- naming_conventions(data_habit_parent_points_w_1on1, replace = "rp.contact.field.parent_point_count_", replace_after = "w_1on1")
data_habit_parent_points_w_praise_neat <- naming_conventions(data_habit_parent_points_w_praise, replace = "rp.contact.field.parent_point_count_", replace_after = "w_praise")
data_habit_parent_points_w_instruct_neat <- naming_conventions(data_habit_parent_points_w_instruct, replace = "rp.contact.field.parent_point_count_", replace_after = "w_instruct")
data_habit_parent_points_w_stress_neat <- naming_conventions(data_habit_parent_points_w_stress, replace = "rp.contact.field.parent_point_count_", replace_after = "w_stress")
data_habit_parent_points_w_money_neat <- naming_conventions(data_habit_parent_points_w_money, replace = "rp.contact.field.parent_point_count_", replace_after = "w_money")
data_habit_parent_points_w_rules_neat <- naming_conventions(data_habit_parent_points_w_rules, replace = "rp.contact.field.parent_point_count_", replace_after = "w_rules")
data_habit_parent_points_w_consequence_neat <- naming_conventions(data_habit_parent_points_w_consequence, replace = "rp.contact.field.parent_point_count_", replace_after = "w_consequence")
data_habit_parent_points_w_solve_neat <- naming_conventions(data_habit_parent_points_w_solve, replace = "rp.contact.field.parent_point_count_", replace_after = "w_solve")
data_habit_parent_points_w_safe_neat <- naming_conventions(data_habit_parent_points_w_safe, replace = "rp.contact.field.parent_point_count_", replace_after = "w_safe")
data_habit_parent_points_w_crisis_neat <- naming_conventions(data_habit_parent_points_w_crisis, replace = "rp.contact.field.parent_point_count_", replace_after = "w_crisis")
data_habit_parent_points_w_celebrate_neat <- naming_conventions(data_habit_parent_points_w_celebrate, replace = "rp.contact.field.parent_point_count_", replace_after = "w_celebrate")

#summary_table(columns_to_summarise = "rp.contact.field.parent_point_count_relax", display = TRUE, include_margins = TRUE)

summary_table_habits_all <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_all, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_all) <- data_habit_parent_points_all_neat
summary_table_habits_all$`Relax`
summary_table_habits_all$`Treat yourself`
summary_table_habits_all$`Praise yourself`
summary_table_habits_all$`Spend time`
summary_table_habits_all$`Praise teen`
summary_table_habits_all$`Instruct positively`
summary_table_habits_all$`Breathe`
summary_table_habits_all$`Money`
summary_table_habits_all$`Consequence`
summary_table_habits_all$`Safe`

summary_plot_habits_all <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_all, .f = ~summary_plot(columns_to_summarise = .x, replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot"))
names(summary_plot_habits_all) <- data_habit_parent_points_all_neat
summary_plot_habits_all$Relax

summary_table_habits_self_care <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_self_care, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_self_care) <- data_habit_parent_points_w_self_care_neat
summary_table_habits_self_care$`Relax`
summary_table_habits_self_care$`Treat yourself`
summary_table_habits_self_care$`Praise yourself`
summary_table_habits_self_care$`Spend time`
summary_table_habits_self_care$`Praise teen`
summary_table_habits_self_care$`Instruct positively`
summary_table_habits_self_care$`Breathe`
summary_table_habits_self_care$`Money`
summary_table_habits_self_care$`Consequence`
summary_table_habits_self_care$`Safe`

summary_table_habits_1on1 <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_1on1, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_1on1) <- data_habit_parent_points_w_1on1_neat
summary_table_habits_1on1$`Relax`
summary_table_habits_1on1$`Treat yourself`
summary_table_habits_1on1$`Praise yourself`
summary_table_habits_1on1$`Spend time`
summary_table_habits_1on1$`Praise teen`
summary_table_habits_1on1$`Instruct positively`
summary_table_habits_1on1$`Breathe`
summary_table_habits_1on1$`Money`
summary_table_habits_1on1$`Consequence`
summary_table_habits_1on1$`Safe`

summary_table_habits_praise <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_praise, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_praise) <- data_habit_parent_points_w_praise_neat
summary_table_habits_praise$`Relax`
summary_table_habits_praise$`Treat yourself`
summary_table_habits_praise$`Praise yourself`
summary_table_habits_praise$`Spend time`
summary_table_habits_praise$`Praise teen`
summary_table_habits_praise$`Instruct positively`
summary_table_habits_praise$`Breathe`
summary_table_habits_praise$`Money`
summary_table_habits_praise$`Consequence`
summary_table_habits_praise$`Safe`

summary_table_habits_instruct <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_instruct, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_instruct) <- data_habit_parent_points_w_instruct_neat
summary_table_habits_instruct$`Relax`
summary_table_habits_instruct$`Treat yourself`
summary_table_habits_instruct$`Praise yourself`
summary_table_habits_instruct$`Spend time`
summary_table_habits_instruct$`Praise teen`
summary_table_habits_instruct$`Instruct positively`
summary_table_habits_instruct$`Breathe`
summary_table_habits_instruct$`Money`
summary_table_habits_instruct$`Consequence`
summary_table_habits_instruct$`Safe`

summary_table_habits_stress <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_stress, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_stress) <- data_habit_parent_points_w_stress_neat
summary_table_habits_stress$`Relax`
summary_table_habits_stress$`Treat yourself`
summary_table_habits_stress$`Praise yourself`
summary_table_habits_stress$`Spend time`
summary_table_habits_stress$`Praise teen`
summary_table_habits_stress$`Instruct positively`
summary_table_habits_stress$`Breathe`
summary_table_habits_stress$`Money`
summary_table_habits_stress$`Consequence`
summary_table_habits_stress$`Safe`

#NB error as Column `rp.contact.field.parent_point_count_consequence_w_money` doesn't exist.
#the tables seems to require all values to exist at least once in order to create the table for any of the parent points in that week.
#false: more likely issue is that R truncated some long contact field names and now cannot find them...
# for now:  data_habit_parent_points_w_money <- data_habit_parent_points_w_money[-8]
summary_table_habits_money <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_money, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_money) <- data_habit_parent_points_w_money_neat
# summary_table_habits_money$`Relax`
# summary_table_habits_money$`Treat yourself`
# summary_table_habits_money$`Praise yourself`
# summary_table_habits_money$`Spend time`
# summary_table_habits_money$`Praise teen`
# summary_table_habits_money$`Instruct positively`
# summary_table_habits_money$`Breathe`
# summary_table_habits_money$`Money`
# summary_table_habits_money$`Consequence`
# summary_table_habits_money$`Safe

#NB error as Column `parent_point_count_consequence_w_rules` doesn't exist.
# for now:  data_habit_parent_points_w_rules <- data_habit_parent_points_w_rules[-8]
summary_table_habits_rules <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_rules, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_rules) <- data_habit_parent_points_w_rules_neat
summary_table_habits_rules$`Relax`
summary_table_habits_rules$`Treat yourself`
summary_table_habits_rules$`Praise yourself`
summary_table_habits_rules$`Spend time`
summary_table_habits_rules$`Praise teen`
summary_table_habits_rules$`Instruct positively`
summary_table_habits_rules$`Breathe`
summary_table_habits_rules$`Money`
summary_table_habits_rules$`Consequence`
summary_table_habits_rules$`Safe`

#Error as Column `rp.contact.field.parent_point_count_money_w_consequence` doesn't exist.
summary_table_habits_consequence <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_consequence, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_consequence) <- data_habit_parent_points_w_consequence_neat
summary_table_habits_consequence$`Relax`
summary_table_habits_consequence$`Treat yourself`
summary_table_habits_consequence$`Praise yourself`
summary_table_habits_consequence$`Spend time`
summary_table_habits_consequence$`Praise teen`
summary_table_habits_consequence$`Instruct positively`
summary_table_habits_consequence$`Breathe`
summary_table_habits_consequence$`Money`
summary_table_habits_consequence$`Consequence`
summary_table_habits_consequence$`Safe`

summary_table_habits_solve <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_solve, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_solve) <- data_habit_parent_points_w_solve_neat
summary_table_habits_solve$`Relax`
summary_table_habits_solve$`Treat yourself`
summary_table_habits_solve$`Praise yourself`
summary_table_habits_solve$`Spend time`
summary_table_habits_solve$`Praise teen`
summary_table_habits_solve$`Instruct positively`
summary_table_habits_solve$`Breathe`
summary_table_habits_solve$`Money`
summary_table_habits_solve$`Consequence`
summary_table_habits_solve$`Safe`

summary_table_habits_safe <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_safe, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_safe) <- data_habit_parent_points_w_safe_neat
summary_table_habits_safe$`Relax`
summary_table_habits_safe$`Treat yourself`
summary_table_habits_safe$`Praise yourself`
summary_table_habits_safe$`Spend time`
summary_table_habits_safe$`Praise teen`
summary_table_habits_safe$`Instruct positively`
summary_table_habits_safe$`Breathe`
summary_table_habits_safe$`Money`
summary_table_habits_safe$`Consequence`
summary_table_habits_safe$`Safe`

#Error as Column `rp.contact.field.parent_point_count_money_w_crisis` doesn't exist.
summary_table_habits_crisis <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_crisis, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_crisis) <- data_habit_parent_points_w_crisis_neat
summary_table_habits_crisis$`Relax`
summary_table_habits_crisis$`Treat yourself`
summary_table_habits_crisis$`Praise yourself`
summary_table_habits_crisis$`Spend time`
summary_table_habits_crisis$`Praise teen`
summary_table_habits_crisis$`Instruct positively`
summary_table_habits_crisis$`Breathe`
summary_table_habits_crisis$`Money`
summary_table_habits_crisis$`Consequence`
summary_table_habits_crisis$`Safe`

#Error as rp.contact.field.parent_point_count_praise_teen_w_celebrate does not exist
summary_table_habits_celebrate <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_celebrate, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_celebrate) <- data_habit_parent_points_w_celebrate_neat
summary_table_habits_celebrate$`Relax`
summary_table_habits_celebrate$`Treat yourself`
summary_table_habits_celebrate$`Praise yourself`
summary_table_habits_celebrate$`Spend time`
summary_table_habits_celebrate$`Praise teen`
summary_table_habits_celebrate$`Instruct positively`
summary_table_habits_celebrate$`Breathe`
summary_table_habits_celebrate$`Money`
summary_table_habits_celebrate$`Consequence`
summary_table_habits_celebrate$`Safe`

#mean average number of parent points given per org

summary_mean_habits <- plhdata_org_clean %>%
  group_by(Org)  %>%
  summarise(across(data_habit_parent_points_all, mean, na.rm = TRUE))

colnames(summary_mean_habits) <- naming_conventions(colnames(summary_mean_habits), "rp.contact.field.parent_point_count_")

summary_mean_habits

# Completion Level ----------------------------------------------------------------------------
data_completion_level <- c("rp.contact.field.w_self_care_completion_level", "rp.contact.field.w_1on1_completion_level",  "rp.contact.field.w_praise_completion_level",
                           "rp.contact.field.w_instruct_completion_level",  "rp.contact.field.w_stress_completion_level",
                           "rp.contact.field.w_money_completion_level",  "rp.contact.field.w_rules_completion_level", #you have "safe_completion" under rules. Is this right?
                           "rp.contact.field.w_consequence_completion_level",  "rp.contact.field.w_solve_completion_level",  "rp.contact.field.w_safe_completion_level",
                           "rp.contact.field.w_crisis_completion_level",  "rp.contact.field.w_celebrate_completion_level")
completion_vars <- c("Self Care", "One-on-one Time", "Praise", "Positive Instructions", "Managing Stress", "Family Budgets", "Rules", "Calm Consequences", "Problem Solving", "Teen Safety", "Dealing with Crisis","Celebration & Next Steps")
summary_table_completion_level <- plhdata_org_clean %>%
  map(.x = data_completion_level, .f = ~summary_table(columns_to_summarise = .x, display = FALSE, include_margins = TRUE))
names(summary_table_completion_level) <- completion_vars

summary_plot_completion_level <- plhdata_org_clean %>%
  map(.x = data_completion_level, .f = ~summary_plot(columns_to_summarise = .x))
names(summary_plot_completion_level) <- completion_vars

# then to access a table:
summary_table_completion_level$`Self Care`
summary_table_completion_level$`One-on-one Time`
summary_table_completion_level$Praise
summary_table_completion_level$`Positive Instructions`
summary_table_completion_level$`Managing Stress`
summary_table_completion_level$`Family Budgets`
summary_table_completion_level$Rules
summary_table_completion_level$`Calm Consequences`
summary_table_completion_level$`Problem Solving`
summary_table_completion_level$`Teen Safety`
summary_table_completion_level$`Dealing with Crisis`
summary_table_completion_level$`Celebration & Next Steps`
# etc.


# or to get all tables:
summary_table_completion_level

#mean average completion level per org
summary_mean_completion_level <- plhdata_org_clean %>%
  group_by(Org)  %>%
  summarise(across(data_completion_level, mean, na.rm = TRUE))

colnames(summary_mean_completion_level) <- naming_conventions(colnames(summary_mean_completion_level), "rp.contact.field.w_", "_completion_level")

#Issue: "rules" workshop data was missing - needed to be added in list creation for "data_completion_level"
summary_mean_completion_level



# Survey - past week  ----------------------------------------------------------------------------
data_survey_past_week <- c("rp.contact.field.survey_welcome_a_1_final",  "rp.contact.field.survey_welcome_a_2_final",
                           "rp.contact.field.survey_welcome_a_3_final",  "rp.contact.field.survey_welcome_a_4_final",
                           "rp.contact.field.survey_welcome_a_5_part_1_final",  "rp.contact.field.survey_welcome_a_5_part_2_final",
                           "rp.contact.field.survey_welcome_a_6_final",  "rp.contact.field.survey_welcome_a_7_part_1_final",
                           "rp.contact.field.survey_welcome_a_7_part_2_final",  "rp.contact.field.survey_welcome_a_7_part_3_final",
                           "rp.contact.field.survey_welcome_a_8_final", "rp.contact.field.survey_welcome_a_9_final")
survey_vars <- c("Attention", "Praise", "Stress", "Shouting", "Money worries", "Summary", "Hitting", "Teen activity", "Lockdown?", "Knowledge of teen activity in non-lockdown week",
                 "Sexual safety talk", "Teen COVID safe")
summary_table_survey_past_week <- plhdata_org_clean %>%
  map(.x = data_survey_past_week, .f = ~summary_table(columns_to_summarise = .x, display = FALSE, include_margins = TRUE))
names(summary_table_survey_past_week) <- survey_vars

# then to access a table:
summary_table_survey_past_week$Attention
summary_table_survey_past_week$Praise
summary_table_survey_past_week$Stress
summary_table_survey_past_week$Shouting
summary_table_survey_past_week$`Money worries`
summary_table_survey_past_week$Summary
summary_table_survey_past_week$Hitting
summary_table_survey_past_week$`Teen activity`
summary_table_survey_past_week$`Lockdown?`
summary_table_survey_past_week$`Knowledge of teen activity in non-lockdown week`
summary_table_survey_past_week$`Sexual safety talk`
summary_table_survey_past_week$`Teen COVID safe`

#TODO iff "7" to 7.1? - TODO - what do they mean by this?


# parent library ------------------------------------------------------------------
data_click <- c("rp.contact.field.click_hs_parent_centre_count", "rp.contact.field.click_pc_help_count",
                "rp.contact.field.click_pc_my_tips_count", "rp.contact.field.click_pc_essential_tools_count",
                "rp.contact.field.click_pc_covid_count", "rp.contact.field.click_pc_customisation_count",
                "rp.contact.field.click_pc_relax_and_activities_count", "rp.contact.field.click_pc_support_contacts_count",
                "rp.contact.field.click_pc_evidence_base_count", "rp.contact.field.click_pc_technical_support_count",
                "rp.contact.field.click_pc_message_archive_count")

data_click_neat <- naming_conventions(data_click, replace = "rp.contact.field.click_pc_", replace_after = "count")
data_click_neat <- naming_conventions(data_click_neat, replace = "Rp.contact.field.click hs")
summary_table_click <- plhdata_org_clean %>%
  map(.x = data_click, .f = ~summary_table(columns_to_summarise = .x))
names(summary_table_click) <- data_click_neat

# then to access a table:
summary_table_click$`Help `

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
summary_tableweekly_workshops <- plhdata_org_clean %>%
  map(.x = data_weekly_workshops, .f = ~summary_table(columns_to_summarise = .x))
names(summary_tableweekly_workshops) <- data_weekly_workshops_neat
summary_tableweekly_workshops$Stress


#In-app reminders(Number of in-app message clicks per workshop week),Per quick start button, per workshop week  -------------------------
# TODO: hsqsclickedws1, hsqsclickedws2 is defined twice, differently each time. Should have different names - is this intentional?
plhdata_org_clean$hsqsclickedws1<-is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care)
plhdata_org_clean$hsqsclickedws2<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_1on1)

summary_table(columns_to_summarise = hsqsclickedws1)
summary_table(columns_to_summarise = hsqsclickedws2)

#Test 1
plhdata_org_clean$hsqsclickedws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care)
plhdata_org_clean$hsqsclickedcountws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care)
plhdata_org_clean$hsqsclickedws2<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_1on1)
summary_table(columns_to_summarise = rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care, replace = "rp.contact.field.click_hs_")
user_id_print("rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care")

summary_table(columns_to_summarise = hsqsclickedws1)
summary_table(columns_to_summarise = hsqsclickedcountws1)
summary_table(columns_to_summarise = hsqsclickedws2)

#Priority 23 
#In-app reminders
#Total number of in-app message clicks.By User
#Weekly workshops
summary_table(columns_to_summarise = rp.contact.field.click_hs_weekly_workshops_quick_start_count, replace = "rp.contact.field.click_hs_")

#Parent center
summary_table(columns_to_summarise = rp.contact.field.click_hs_parent_centre_quick_start_count, replace = "rp.contact.field.click_hs_")

#Parent points
summary_table(columns_to_summarise = rp.contact.field.click_hs_parent_points_quick_start_count, replace = "rp.contact.field.click_hs_")


#Priority 19
#App-opens
#Total number of app-opens for each user(cumulative)
#app_open_summary <- plhdata_org_clean %>%
#  group_by(Org) %>%
#  summarise(sum(rp.contact.field.app_launch_count))
user_id_print("rp.contact.field.app_launch_count")

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

tables_app_opens <- plhdata_org_clean %>%
  map(.x = data_app_opens, .f = ~summary_table(columns_to_summarise = .x))
names(tables_app_opens) <- data_app_opens_neat
tables_app_opens$`Total`
tables_app_opens$`Welcome and Self care`
tables_app_opens$`One-on-one time`
tables_app_opens$`Praise`
tables_app_opens$`Positive Instructions`
tables_app_opens$`Managing Stress`
tables_app_opens$`Family Budget`
tables_app_opens$`Rules`
tables_app_opens$`Calm Consequences`
tables_app_opens$`Problem Solving`
tables_app_opens$`Teen Safety`
tables_app_opens$`Crisis`
tables_app_opens$`Celebration & Next Steps`

##Priority 21
#App-opens
#Maximum time between app-opens - for each user.Time in full days 
plhdata_org_clean$rp.contact.field.max_days_between_app_launches <- as.numeric(plhdata_org_clean$rp.contact.field.max_days_between_app_launches)
summary_table(columns_to_summarise = rp.contact.field.max_days_between_app_launches, replace = "rp.contact.field.")

#Priority 36 --------------------------------------------------------------------------------
#Emotional Check-in
#Rate of users who respond "happy" ,"sad" & "ok"
data_emotional_check_in <- c("rp.contact.field.w_self_care_welcome_individual_a_final", "rp.contact.field.w_1on1_welcome_individual_a_final", 
                             "rp.contact.field.w_praise_welcome_individual_a_final", "rp.contact.field.w_instruct_welcome_individual_a_final", 
                             "rp.contact.field.w_stress_welcome_individual_a_final", "rp.contact.field.w_money_welcome_individual_a_final", 
                             "rp.contact.field.w_rules_welcome_individual_a_final")
data_emotional_check_in_neat <- c("Self care", "One-on-one time", "Praise", "Positive instructions", "Managing stress", "Family budgets", "Rules")
tables_emotional_check_in <- plhdata_org_clean %>%
  map(.x = data_emotional_check_in, .f = ~summary_table(columns_to_summarise = .x))
names(tables_emotional_check_in) <- data_emotional_check_in_neat
tables_emotional_check_in$`Self care`

# Completion rate of introductory session(Workshop 1:Selfcare) ------------------------------------------------
plhdata_org_clean %>%
  split(.$Org) %>%
  map(~summary_table(data = .x, factor = NULL, columns_to_summarise = rp.contact.field.w_self_care_completed, replace = "rp.contact.field.survey"))
#plhdata_org_clean %>% group_by(Org) %>% select('app_user_id', "rp.contact.field.w_self_care_completed")
#plhdata_org_clean %>% group_by(Org) %>% select('app_user_id', "rp.contact.field.w_self_care_started")
#plhdata_org_clean %>% group_by(Org) %>% select('app_user_id', "rp.contact.field.w_1on1_started")

# Completion status of baseline survey ------------------------------------------------
plhdata_org_clean %>%
  split(.$Org) %>%
  map(~summary_table(data = .x, factor = NULL, columns_to_summarise = rp.contact.field.survey_welcome_completed, replace = "rp.contact.field.survey"))

# Descriptive Statistics ------------------------------------------------------------------------------------------
# Gender of App Users
gender_table <- plhdata_org_clean %>%
  split(.$Org) %>%
  map(~summary_table(data = .x, factor = NULL, columns_to_summarise = rp.contact.field.user_gender))
names(gender_table) <- levels(plhdata_org_clean$Org)
gender_table$Nontobeko
gender_table$Amathuba
gender_table

# Age of App Users
summary_table(columns_to_summarise = rp.contact.field.user_age, summaries = "mean")


#Trials-----------------
plhdata_org_clean %>% select('app_user_id', "rp.contact.field.user_age")
plhdata_org_clean %>% select('app_user_id', "rp.contact.field.user_gender")
plhdata_org_clean %>% filter(Org == "Amathuba") %>% select('app_user_id', "rp.contact.field.user_gender")
mean(x=as.numeric(plhdata_org_clean$rp.contact.field.user_age), na.rm=TRUE)
