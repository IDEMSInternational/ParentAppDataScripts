
### extract data ----------------------------------------------------------------------
df <- get_metabase_data()
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
                                           `Amathuba Collective.Miss` ="Amathuba", `Miss.Amathuba Mzi` ="Amathuba", `Miss.Amathuba Mzi ` ="Amathuba", `Miss.amathuba` ="Amathuba"))

# so do the Miss. to Other first:
#plhdata_org <- plhdata_org %>%
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
  filter(Org != "Miss")%>%
  mutate(Org = factor(Org))

# Create subsets of the data based on valid app user ID's
plhdata_org_clean <- plhdata_org_clean %>%
  dplyr::filter(!is.na(app_version))

# Look at the numbers per organisation from clear data 
sjmisc::frq(x=plhdata_org_clean$Org, out="txt")

# More cleaning
# TODO: Add here any to make numeric.
plhdata_org_clean$rp.contact.field.user_age <- as.numeric(plhdata_org_clean$rp.contact.field.user_age)
plhdata_org_clean$rp.contact.field.household_adults <- as.numeric(plhdata_org_clean$rp.contact.field.household_adults)
plhdata_org_clean$rp.contact.field.household_teens <- as.numeric(plhdata_org_clean$rp.contact.field.household_teens)
plhdata_org_clean$rp.contact.field.household_babies <- as.numeric(plhdata_org_clean$rp.contact.field.household_babies)
plhdata_org_clean$rp.contact.field.household_children <- as.numeric(plhdata_org_clean$rp.contact.field.household_children)
plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed)
plhdata_org_clean$rp.contact.field.w_self_care_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_self_care_completion_level)
plhdata_org_clean$rp.contact.field.parent_point_count_relax_w_self_care <- as.numeric(plhdata_org_clean$rp.contact.field.parent_point_count_relax_w_self_care)
plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed <- as.Date(plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed)

# Write clean data back -------------------------------------------------------

# Analysis - tables - separate for different groups.
plhdata_org_clean_2 <- plhdata_org_clean %>% filter(!is.na(app_version))

for (i in levels(plhdata_org_clean$Org)){
  summary_PT(data = plhdata_org_clean_2, summary_var = app_version, denominator = Org,
             denominator_level = i, together = TRUE, naming_convention = FALSE)
}


#get_app_user_IDs(data = plhdata_org_clean_2, Org, "Nontobeko", show_invalid = FALSE)


## Data Analysis ## --------------------------------------------------------

# Summary tables of started/completed things
# Show the summary of Self care workshop started(1st Workshop)                            
two_way_table(row_var = rp.contact.field.w_self_care_started)

#plhdata_org_clean%>%dplyr::filter(Nontobeko,Joy,Amathuba,Dlalanathi)%>%two_way_table(row_var = rp.contact.field.w_self_care_started)

# Show the summary of Self care workshop completion(1st Workshop)
two_way_table(row_var = rp.contact.field.w_self_care_completed)

# Show the summary of One-on-One Time started(2nd Workshop)
two_way_table(row_var = rp.contact.field.w_1on1_started)

# Show the summary of  One-on-One Time completion(2nd Workshop)
two_way_table(row_var = rp.contact.field.w_1on1_completed)

# Show the summary of Praise started(3rd workshop) 
two_way_table(row_var = rp.contact.field.w_praise_started)

# Show the summary of Praise(3rd Workshop)
two_way_table(row_var = rp.contact.field.w_praise_completed)

## Show the summary of Positive Instructions(4th workshop)
two_way_table(row_var = rp.contact.field.w_instruct_started)

# Show the summary of Positive Instructions(4th workshop)
two_way_table(row_var = rp.contact.field.w_instruct_completed)

# Show the summary of Managing Stress(5th workshop)
two_way_table(row_var = rp.contact.field.w_stress_started)

# Show the summary of Managing Stress(5th workshop)
two_way_table(row_var = rp.contact.field.w_stress_completed)

# Show the summary of Family Budgets(6th workshop)
two_way_table(row_var = rp.contact.field.w_money_started)

# Show the summary of Family Budgets(6th workshop)
two_way_table(row_var = rp.contact.field.w_money_completed)

#Baseline survey ------------------------------------------------------------------------------------
data_baseline_survey <- with(plhdata_org_clean, data.frame(app_user_id,
                                                           Org,
                                                           rp.contact.field.survey_welcome_completed, rp.contact.field.user_gender,
                                                           rp.contact.field.user_age, rp.contact.field.household_adults, 
                                                           rp.contact.field.household_teens, rp.contact.field.household_babies,
                                                           rp.contact.field.household_children))

colnames(data_baseline_survey) <- naming_conventions(colnames(data_baseline_survey), replace = "rp.contact.field.")
two_way_table_baseline <- NULL
# to run all the "Habits" tables individually:
for (i in 3:length(data_baseline_survey)){
  two_way_table_baseline[[(i-2)]] <- two_way_table(data = data_baseline_survey,
                                                 row_var = names(data_baseline_survey)[i])
  
  # to get user IDs
  # user_id_print(names(data_baseline_survey)[i])
}
names(two_way_table_baseline) <- names(data_baseline_survey)[3:length(data_baseline_survey)]
# then to access a table:
two_way_table_baseline$`Household babies`


###Completion status of baseline survey
# Show the summary of baseline survey completion(Organisaton-wise)

summary_PT(data = plhdata_org_clean, summary_var = rp.contact.field.survey_welcome_completed, denominator = Org,
           denominator_level = "Nontobeko", together = TRUE, naming_convention = FALSE)
summary_PT(data = plhdata_org_clean, summary_var = rp.contact.field.survey_welcome_completed, denominator = Org,
           denominator_level = "Joy", together = TRUE, naming_convention = FALSE)
summary_PT(data = plhdata_org_clean, summary_var = rp.contact.field.survey_welcome_completed, denominator = Org,
           denominator_level = "Dlalanthi", together = TRUE, naming_convention = FALSE)
#summary_PT(data = plhdata_org_clean, summary_var = rp.contact.field.survey_welcome_completed, denominator = Org,
#           denominator_level = "Hillcrest", together = TRUE, naming_convention = FALSE)

# Show the summary of user ids on baseline survey completion (Organisaton-wise)
user_id_print("rp.contact.field.survey_welcome_completed")




#Time spent on the workshops ------------------------------------
# TODO: sort NAs in these:
#Self care workshop
two_way_table(row_var = rp.contact.field.w_self_care_diff_started_completed)

#One on one time workshop
two_way_table(row_var = rp.contact.field.w_1on1_diff_started_completed)

# app language --------------------------------------------------
two_way_table(row_var = rp.contact.field._app_language, replace = "rp.contact.field.")
user_id_print("rp.contact.field._app_language")



# TODO: factor levels? that should be done in cleaning step.

# HABITS ------------------------------------------------------------------------------------
data_habit_parent_points <- with(plhdata_org_clean, data.frame(app_user_id = app_user_id,
                                                               Org,
                                                                rp.contact.field.parent_point_count_relax, rp.contact.field.parent_point_count_treat_yourself,
                                                                rp.contact.field.parent_point_count_praise_yourself, rp.contact.field.parent_point_count_spend_time,
                                                                rp.contact.field.parent_point_count_praise_teen, rp.contact.field.parent_point_count_instruct_positively, rp.contact.field.parent_point_count_breathe,
                                                                rp.contact.field.parent_point_count_money, rp.contact.field.parent_point_count_consequence, rp.contact.field.parent_point_count_safe,
                                                                rp.contact.field.parent_point_count_relax_w_self_care, rp.contact.field.parent_point_count_treat_yourself_w_self_care, rp.contact.field.parent_point_count_praise_yourself_w_self_care,
                                                                rp.contact.field.parent_point_count_spend_time_w_self_care, rp.contact.field.parent_point_count_praise_teen_w_self_care, 
                                                                rp.contact.field.parent_point_count_breathe_w_self_care, rp.contact.field.parent_point_count_money_w_self_care, 
                                                                rp.contact.field.parent_point_count_consequence_w_self_care, rp.contact.field.parent_point_count_safe_w_self_care, 
                                                                rp.contact.field.parent_point_count_relax_w_1on1, rp.contact.field.parent_point_count_treat_yourself_w_1on1,
                                                                rp.contact.field.parent_point_count_praise_yourself_w_1on1, rp.contact.field.parent_point_count_spend_time_w_1on1,
                                                                rp.contact.field.parent_point_count_praise_teen_w_1on1, rp.contact.field.parent_point_count_breathe_w_1on1, 
                                                                rp.contact.field.parent_point_count_money_w_1on1, rp.contact.field.parent_point_count_consequence_w_1on1, 
                                                                rp.contact.field.parent_point_count_safe_w_1on1))

colnames(data_habit_parent_points) <- naming_conventions(colnames(data_habit_parent_points), replace = "rp.contact.field.parent_point_count_")
two_way_table_habits <- NULL
# to run all the "Habits" tables individually:
for (i in 3:length(data_habit_parent_points)){
  data_habit_parent_points[[i]] <- as.numeric(data_habit_parent_points[[i]])
  
  two_way_table_habits[[(i-2)]] <- two_way_table(data = data_habit_parent_points,
                row_var = names(data_habit_parent_points)[i])
  
  # to get user IDs
  # user_id_print(names(data_habit_parent_points)[i])
}
names(two_way_table_habits) <- names(data_habit_parent_points)[3:length(data_habit_parent_points)]
# then to access a table:
two_way_table_habits$`Instruct positively`
# etc.

# Completion Level ----------------------------------------------------------------------------
# Not habits - what are these? ------------------------------------------------------------------
data_completion_level <- with(plhdata_org_clean, data.frame(app_user_id,
                                                            Org,
                                                            rp.contact.field.w_1on1_completion_level, rp.contact.field.w_praise_completion_level, rp.contact.field.w_instruct_completion_level, rp.contact.field.w_stress_completion_level,
                                                            rp.contact.field.w_money_completion_level, rp.contact.field.w_safe_completion_level, #you have "safe_completion" under rules. Is this right?
                                                            rp.contact.field.w_consequence_completion_level, rp.contact.field.w_solve_completion_level, rp.contact.field.w_safe_completion_level,
                                                            rp.contact.field.w_crisis_completion_level))

completion_vars <- c("One-on-one Time", "Praise", "Positive Instructions", "Managing Stress", "Family Budgets", "Rules", "Calm Consequences", "Problem Solving", "Teen Safety", "Dealing with Crisis")
names(data_completion_level) <- c("app_user_id", "Org", completion_vars)

two_way_table_completion_level <- NULL
for (i in 3:length(data_completion_level)){
  data_completion_level[[i]] <- as.numeric(data_completion_level[[i]])
  
  two_way_table_completion_level[[(i-2)]] <- (two_way_table(data = data_completion_level,
                row_var = names(data_completion_level)[i]))
  
  # to get user IDs
  # user_id_print(names(data_completion_level)[i])
}
names(two_way_table_completion_level) <- completion_vars

# then to access a table:
two_way_table_completion_level$Praise
# etc.

# or to get all tables:
two_way_table_completion_level

# Survey - past week  ----------------------------------------------------------------------------
data_survey_past_week <- with(plhdata_org_clean, data.frame(app_user_id,
                                                            Org,
                                                            rp.contact.field.survey_welcome_a_1_final, rp.contact.field.survey_welcome_a_2_final,
                                                            rp.contact.field.survey_welcome_a_3_final, rp.contact.field.survey_welcome_a_4_final,
                                                            rp.contact.field.survey_welcome_a_5_part_1_final, rp.contact.field.survey_welcome_a_5_part_2_final,
                                                            rp.contact.field.survey_welcome_a_6_final, rp.contact.field.survey_welcome_a_7_part_1_final,
                                                            rp.contact.field.survey_welcome_a_7_part_2_final, rp.contact.field.survey_welcome_a_7_part_3_final,
                                                            rp.contact.field.survey_welcome_a_8_final,rp.contact.field.survey_welcome_a_9_final))

survey_vars <- c("Attention", "Praise", "Stress", "Shouting", "Money worries", "Summary", "Hitting", "Teen activity", "Lockdown?", "Knowledge of teen activity in non-lockdown week",
                 "Sexual safety talk", "Teen COVID safe")
names(data_survey_past_week) <- c("app_user_id", "Org", survey_vars)

two_way_table_survey_past_week <- NULL
for (i in 3:length(data_survey_past_week)){
  data_survey_past_week[[i]] <- as.numeric(data_survey_past_week[[i]])
  
  two_way_table_survey_past_week[[(i-2)]] <- (two_way_table(data = data_survey_past_week,
                                                            row_var = names(data_survey_past_week)[i]))
  
  # to get user IDs
  # user_id_print(names(data_survey_past_week)[i])
}
names(two_way_table_survey_past_week) <- survey_vars

# then to access a table:
two_way_table_survey_past_week$Hitting
# etc.

#TODO iff "7" to 7.1? - TODO - what do they mean by this?

# then survey past week BY Org:
org_tables_survey_past_week <- NULL
for (i in 3:length(data_survey_past_week)){
  org_tables_survey_past_week[[(i-2)]] <- multiple_summary_PT(data = data_survey_past_week,
                                                          summary_var = names(data_survey_past_week)[i])
}
names(org_tables_survey_past_week) <- colnames(data_survey_past_week)[3:length(data_survey_past_week)]
org_tables_survey_past_week$`Attention`$Joy


# parent library ------------------------------------------------------------------
data_click <- with(plhdata_org_clean, data.frame(app_user_id,
                                                 Org,
                                                 rp.contact.field.click_hs_parent_centre_count, rp.contact.field.click_pc_help_count,
                                                 rp.contact.field.click_pc_my_tips_count, rp.contact.field.click_pc_essential_tools_count,
                                                 rp.contact.field.click_pc_covid_count, rp.contact.field.click_pc_customisation_count,
                                                 rp.contact.field.click_pc_relax_and_activities_count, rp.contact.field.click_pc_support_contacts_count,
                                                 rp.contact.field.click_pc_evidence_base_count, rp.contact.field.click_pc_technical_support_count,
                                                 rp.contact.field.click_pc_message_archive_count))

colnames(data_click) <- naming_conventions(colnames(data_click), replace = "rp.contact.field.click_pc_", replace_after = "count")
colnames(data_click) <- naming_conventions(colnames(data_click), replace = "Rp.contact.field.click hs")

click_vars <- names(data_click)[3:length(data_click)]
two_way_table_click <- NULL
for (i in 3:length(data_click)){
  data_click[[i]] <- as.numeric(data_click[[i]])
  
  two_way_table_click[[(i-2)]] <- (two_way_table(data = data_click,
                                                 row_var = names(data_click)[i]))
  
  # to get user IDs
  # user_id_print(names(data_click)[i])
}
names(two_way_table_click) <- click_vars

# then to access a table:
two_way_table_click$`Help `


# 

#Test 2 Priority 22 (how to interpret data?)
#Number of in-app message clicks per workshop week.Per quick start button, per workshop week 
#Workshops
plhdata_org_clean$hsqsclickedws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care)

data_weekly_workshops <- with(plhdata_org_clean, data.frame(app_user_id, Org,
                                                            rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care,
                                                            hsqsclickedws1, rp.contact.field.click_hs_weekly_workshops_quick_start_w_1on1,
                                                            rp.contact.field.click_hs_weekly_workshops_quick_start_w_praise, rp.contact.field.click_hs_weekly_workshops_quick_start_w_instruct,
                                                            rp.contact.field.click_hs_weekly_workshops_quick_start_w_stress, rp.contact.field.click_hs_weekly_workshops_quick_start_w_money,
                                                            rp.contact.field.click_hs_weekly_workshops_quick_start_w_rules, rp.contact.field.click_hs_weekly_workshops_quick_start_w_consequence, 
                                                            rp.contact.field.click_hs_weekly_workshops_quick_start_w_solve, rp.contact.field.click_hs_weekly_workshops_quick_start_w_safe,
                                                            rp.contact.field.click_hs_weekly_workshops_quick_start_w_crisis, rp.contact.field.click_hs_weekly_workshops_quick_start_w_celebrate,
                                                            rp.contact.field.click_hs_parent_centre_quick_start_w_self_care, rp.contact.field.click_hs_parent_centre_quick_start_w_1on1,
                                                            rp.contact.field.click_hs_parent_points_quick_start_w_self_care, rp.contact.field.click_hs_parent_points_quick_start_w_1on1))

colnames(data_weekly_workshops)[3:length(data_weekly_workshops)] <- c("Self care", "In app reminders", "One-on-one time", "Praise", "Positive instructions", "Stress",
                                                                      "Family Budgets", "Rules", "Calm consequence", "Problem solving", "Teen safety", "Dealing with crisis",
                                                                      "Celebration and Next Steps", "Parent center - Self care", "Parent center - One-on-one time", "Parent points - Self care", "Parent points - One-on-one time")
two_way_tableweekly_workshops <- NULL
for (i in 3:length(data_weekly_workshops)){
  two_way_tableweekly_workshops[[(i-2)]] <- two_way_table(data = data_weekly_workshops,
                                                          row_var = names(data_weekly_workshops)[i])
  
  # to get user IDs
  # user_id_print(names(data_weekly_workshops)[i])
}
names(two_way_tableweekly_workshops) <- names(data_weekly_workshops)[3:length(data_weekly_workshops)]
# then to access a table:
two_way_tableweekly_workshops$Stress


#In-app reminders(Number of in-app message clicks per workshop week),Per quick start button, per workshop week  -------------------------
# TODO: hsqsclickedws1, hsqsclickedws2 is defined twice, differently each time. Should have different names - is this intentional?
plhdata_org_clean$hsqsclickedws1<-is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care)
plhdata_org_clean$hsqsclickedws2<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_1on1)

two_way_table(row_var = hsqsclickedws1)
two_way_table(row_var = hsqsclickedws2)

#Test 1
plhdata_org_clean$hsqsclickedws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care)
plhdata_org_clean$hsqsclickedcountws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care)
plhdata_org_clean$hsqsclickedws2<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_1on1)
two_way_table(row_var = rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care, replace = "rp.contact.field.click_hs_")
user_id_print("rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care")


two_way_table(row_var = hsqsclickedws1)
two_way_table(row_var = hsqsclickedcountws1)
two_way_table(row_var = hsqsclickedws2)

#Priority 23 
#In-app reminders
#Total number of in-app message clicks.By User
#Weekly workshops
two_way_table(row_var = rp.contact.field.click_hs_weekly_workshops_quick_start_count, replace = "rp.contact.field.click_hs_")

#Parent center
two_way_table(row_var = rp.contact.field.click_hs_parent_centre_quick_start_count, replace = "rp.contact.field.click_hs_")

#Parent points
two_way_table(row_var = rp.contact.field.click_hs_parent_points_quick_start_count, replace = "rp.contact.field.click_hs_")


#Priority 19
#App-opens
#Total number of app-opens for each user(cumulative)
plhdata_org_clean$rp.contact.field.app_launch_count <- as.numeric(plhdata_org_clean$rp.contact.field.app_launch_count)
two_way_table(row_var = rp.contact.field.app_launch_count, replace = "rp.contact.field.")
user_id_print("rp.contact.field.app_launch_count")

##Priority 20
#App-opens
#Number of app opens within a workshop week for each user
data_app_opens <- with(plhdata_org_clean, data.frame(app_user_id, Org,
                                                     rp.contact.field.app_launch_count_w_self_care, rp.contact.field.app_launch_count_w_1on1,
                                                     rp.contact.field.app_launch_count_w_rules, rp.contact.field.app_launch_count_w_consequence))
colnames(data_app_opens)[3:length(data_app_opens)] <- c("Self care", "One-on-one time", "Rules (w/s 7)", "Calm consequences (w/s 8)")
head(data_app_opens)
# have it all as a data set.
tables_app_opens <- NULL
for (i in 3:length(data_app_opens)){
  tables_app_opens[[(i-2)]] <- summary_PT(data = data_app_opens,
                                          summary_var = names(data_app_opens)[i],
                                          together = TRUE)
}
names(tables_app_opens) <- colnames(data_app_opens)[3:length(data_app_opens)]
tables_app_opens$`Self care`

##Priority 21
#App-opens
#Maximum time between app-opens - for each user.Time in full days 
plhdata_org_clean$rp.contact.field.max_days_between_app_launches <- as.numeric(plhdata_org_clean$rp.contact.field.max_days_between_app_launches)
two_way_table(row_var = rp.contact.field.max_days_between_app_launches, replace = "rp.contact.field.")

#Priority 36 --------------------------------------------------------------------------------
#Emotional Check-in
#Rate of users who respond "happy" ,"sad" & "ok"
data_emotional_check_in <- with(plhdata_org_clean, data.frame(app_user_id, Org,
                                                              rp.contact.field.w_self_care_welcome_individual_a_final, rp.contact.field.w_1on1_welcome_individual_a_final, 
                                                              rp.contact.field.w_praise_welcome_individual_a_final, rp.contact.field.w_instruct_welcome_individual_a_final, 
                                                              rp.contact.field.w_stress_welcome_individual_a_final, rp.contact.field.w_money_welcome_individual_a_final, 
                                                              rp.contact.field.w_rules_welcome_individual_a_final))
colnames(data_emotional_check_in)[3:length(data_emotional_check_in)] <- c("Self care", "One-on-one time", "Praise", "Positive instructions", "Managing stress", "Family budgets", "Rules")
head(data_emotional_check_in)
# have it all as a data set.
tables_emotional_check_in <- NULL
for (i in 3:length(data_emotional_check_in)){
  tables_emotional_check_in[[(i-2)]] <- summary_PT(data = data_emotional_check_in,
                                                   summary_var = names(data_emotional_check_in)[i],
                                                   together = TRUE)
}
names(tables_emotional_check_in) <- colnames(data_emotional_check_in)[3:length(data_emotional_check_in)]
tables_emotional_check_in$`Self care`

# Completion rate of introductory session(Workshop 1:Selfcare) ------------------------------------------------
multiple_summary_PT(summary_var = rp.contact.field.w_self_care_completed)
#plhdata_org_clean %>% group_by(Org) %>% select('app_user_id', "rp.contact.field.w_self_care_completed")
#plhdata_org_clean %>% group_by(Org) %>% select('app_user_id', "rp.contact.field.w_self_care_started")
#plhdata_org_clean %>% group_by(Org) %>% select('app_user_id', "rp.contact.field.w_1on1_started")

# Completion status of baseline survey ------------------------------------------------
multiple_summary_PT(summary_var = rp.contact.field.survey_welcome_completed,
                    replace = "rp.contact.field.survey")

# Descriptive Statistics ------------------------------------------------------------------------------------------
# Gender of App Users
gender_table <- multiple_summary_PT(summary_var = rp.contact.field.user_gender)
gender_table$Nontobeko
gender_table$Amathuba
gender_table


# Age of App Users
age_table <- multiple_summary_PT(summary_var = rp.contact.field.user_age)
age_table$Nontobeko

#Trials-----------------
plhdata_org_clean %>% select('app_user_id', "rp.contact.field.user_age")
plhdata_org_clean %>% select('app_user_id', "rp.contact.field.user_gender")
plhdata_org_clean %>% filter(Org == "Amathuba") %>% select('app_user_id', "rp.contact.field.user_gender")
mean(x=as.numeric(plhdata_org_clean$rp.contact.field.user_age), na.rm=TRUE)
