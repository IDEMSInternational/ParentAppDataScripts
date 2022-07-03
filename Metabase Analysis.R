##################################
##################################
####### User Data Analysis #######
##################################
##################################

### extract data ----------------------------------------------------------------------
# to get user data
plhdata_org <- get_user_data(merge_check = FALSE) # select 1 if you want to merge in changes (yes)
# to get notification data
nf_data <- get_nf_data()

## Data Cleaning - User Data ## --------------------------------------------------------

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
                                           `Amathuba Collective.Miss` ="Amathuba", `Miss.Amathuba Mzi` ="Amathuba", `Miss.Amathuba Mzi ` ="Amathuba", `Miss.amathuba` ="Amathuba", `Miss.dlalanathi`="Dlalanathi",
                                           `Miss.organisation_1` = "Other", `Miss.organisation_2` = "Other",`Miss.organisation_6` = "Other"))

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

# add in country variable
plhdata_org_clean <- plhdata_org_clean %>%
  mutate(country = ifelse(Org %in% c("Amathuba", "Joy", "Dlalanathi", "Nontobeko"), "South Africa",
                          ifelse(Org %in% c("ICS"), "Tanzania",
                                 "Other")))

# Look at the numbers per organisation from clear data 
sjmisc::frq(x=plhdata_org_clean$Org, out="txt")

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

# Summary tables of started/completed things
# Show the summary of Self care workshop started(1st Workshop)                            
# summary_table(columns_to_summarise = rp.contact.field.w_self_care_started)

#plhdata_org_clean%>%dplyr::filter(Nontobeko,Joy,Amathuba,Dlalanathi)%>%summary_table(columns_to_summarise = rp.contact.field.w_self_care_started)

# Show the summary of Self care workshop completion(1st Workshop)
#summary_table(columns_to_summarise = rp.contact.field.w_self_care_completed)

# Show the summary of One-on-One Time started(2nd Workshop)
#summary_table(columns_to_summarise = rp.contact.field.w_1on1_started)

# Show the summary of  One-on-One Time completion(2nd Workshop)
#summary_table(columns_to_summarise = rp.contact.field.w_1on1_completed)

# Show the summary of Praise started(3rd workshop) 
#summary_table(columns_to_summarise = rp.contact.field.w_praise_started)

# Show the summary of Praise(3rd Workshop)
#summary_table(columns_to_summarise = rp.contact.field.w_praise_completed)

## Show the summary of Positive Instructions(4th workshop)
#summary_table(columns_to_summarise = rp.contact.field.w_instruct_started)

# Show the summary of Positive Instructions(4th workshop)
#summary_table(columns_to_summarise = rp.contact.field.w_instruct_completed)

# Show the summary of Managing Stress(5th workshop)
#summary_table(columns_to_summarise = rp.contact.field.w_stress_started)

# Show the summary of Managing Stress(5th workshop)
#summary_table(columns_to_summarise = rp.contact.field.w_stress_completed)

# Show the summary of Family Budgets(6th workshop)
#summary_table(columns_to_summarise = rp.contact.field.w_money_started)

# Show the summary of Family Budgets(6th workshop)
#summary_table(columns_to_summarise = rp.contact.field.w_money_completed)

#survey ------------------------------------------------------------------------------------
# workshop_together variable
#plhdata_org_clean <- plhdata_org_clean %>%
#  mutate(rp.contact.field.workshop_path = ifelse(is.na(rp.contact.field.workshop_path),
#                                                         rp.contact.field.do_workshops_together,
#                                                 rp.contact.field.workshop_path))
  
data_baseline_survey <- c("rp.contact.field.survey_welcome_completed", "rp.contact.field.user_gender",
                          "rp.contact.field.user_age", "rp.contact.field.household_adults",
                          "rp.contact.field.household_teens", "rp.contact.field.household_babies",
                          "rp.contact.field.household_children", "rp.contact.field._app_language", "app_version", "rp.contact.field.workshop_path")
baseline_names_neat <- naming_conventions(data_baseline_survey, replace = "rp.contact.field.")

#TO DO: replace "NA" with "unknown" for nicer display in Shiny
summary_table_baseline <- plhdata_org_clean %>%
  map(.x = data_baseline_survey, .f = ~replace_na(.x, "unknown"))  %>%
  map(.x = data_baseline_survey, .f = ~summary_table(columns_to_summarise = .x,
                                                     display = FALSE,
                                                     include_margins = TRUE,
                                                     summaries = "frequencies"))
names(summary_table_baseline) <- baseline_names_neat
# summary_table_baseline$`Household babies`
# summary_table_baseline$` app language`
# summary_table_baseline$`App version`
# summary_table_baseline$`Do workshops together`
# summary_table_baseline$`Household adults`

#summary_table_baseline$`User gender`  %>% filter(Org %in% c(("Dlalanathi"))) %>%
#  pivot_wider(names_from = `User gender`, values_from = N)

summary_plot_baseline <- plhdata_org_clean %>%
  map(.x = data_baseline_survey, .f = ~summary_plot(columns_to_summarise = .x))
names(summary_plot_baseline) <- baseline_names_neat
# summary_plot_baseline$`Survey welcome completed`

###Completion status of baseline survey
# Show the summary of baseline survey completion(Organisaton-wise)

# summary_table(plhdata_org_clean %>% filter(Org == "Nontobeko"),
#               factor = NULL,
#               columns_to_summarise = rp.contact.field.survey_welcome_completed)

# Show the summary of user ids on baseline survey completion (Organisaton-wise)
# user_id_print("rp.contact.field.survey_welcome_completed")


#Time spent on the workshops -----------------------------------------------------------------------
# TODO: sort NAs in these:
#Self care workshop
# summary_table(columns_to_summarise = rp.contact.field.w_self_care_diff_started_completed)

#One on one time workshop
# summary_table(columns_to_summarise = rp.contact.field.w_1on1_diff_started_completed)

# app language --------------------------------------------------
# summary_table(columns_to_summarise = rp.contact.field._app_language, replace = "rp.contact.field.")
# user_id_print("rp.contact.field._app_language")
# TODO: factor levels? that should be done in cleaning step.

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

#parent points in each week sorted by parent point (not necessary to code as we already have the number of PPs in each ws week below)

#Average relax parent points pp1
summary_relax_workshop <- plhdata_org_clean %>%
  group_by(Org) %>%
  summarise(across(relax_workshop_vars, mean, na.rm = TRUE))
colnames(summary_relax_workshop) <- naming_conventions(colnames(summary_relax_workshop), "rp.contact.field.parent_point_count_relax_w_")
# summary_relax_workshop

# Make the table longer so that it is in a format for use in ggplot
summary_relax_workshop_long <- summary_relax_workshop %>%
  pivot_longer(cols = !Org) %>%
  mutate(name = fct_relevel(name, week_order))   # set the order of variables
# summary_relax_workshop_long

# Run the plot (#Code moved to RSHiny file in order to be able to filter by Org)
# ggplot(summary_relax_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
#   geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   geom_line() + labs(x = "Workshop week", y = "Number of points")

#Average treat_yourself parent points pp2
summary_treat_yourself_workshop <- plhdata_org_clean %>%
  group_by(Org) %>%
  summarise(across(treat_yourself_workshop_vars, mean, na.rm = TRUE))
colnames(summary_treat_yourself_workshop) <- naming_conventions(colnames(summary_treat_yourself_workshop), "rp.contact.field.parent_point_count_treat_yourself_w_")
# summary_treat_yourself_workshop

# Make the table longer so that it is in a format for use in ggplot
summary_treat_yourself_workshop_long <- summary_treat_yourself_workshop %>%
  pivot_longer(cols = !Org) %>%
  mutate(name = fct_relevel(name, week_order))   # set the order of variables
# summary_treat_yourself_workshop_long

# Run the plot (#Code moved to RSHiny file in order to be able to filter by Org)
# ggplot(summary_treat_yourself_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
#   geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   geom_line() + labs(x = "Workshop week", y = "Number of points")

#Average praise_yourself parent points pp3
summary_praise_yourself_workshop <- plhdata_org_clean %>%
  group_by(Org) %>%
  summarise(across(praise_yourself_workshop_vars, mean, na.rm = TRUE))
colnames(summary_praise_yourself_workshop) <- naming_conventions(colnames(summary_praise_yourself_workshop), "rp.contact.field.parent_point_count_praise_yourself_w_")
# summary_praise_yourself_workshop

# Make the table longer so that it is in a format for use in ggplot
summary_praise_yourself_workshop_long <- summary_praise_yourself_workshop %>%
  pivot_longer(cols = !Org) %>%
  mutate(name = fct_relevel(name, week_order))   # set the order of variables
# summary_praise_yourself_workshop_long

#Average spend_time parent points pp4
summary_spend_time_workshop <- plhdata_org_clean %>%
  group_by(Org) %>%
  summarise(across(spend_time_workshop_vars, mean, na.rm = TRUE))
colnames(summary_spend_time_workshop) <- naming_conventions(colnames(summary_spend_time_workshop), "rp.contact.field.parent_point_count_spend_time_w_")
# summary_spend_time_workshop

# Make the table longer so that it is in a format for use in ggplot
summary_spend_time_workshop_long <- summary_spend_time_workshop %>%
  pivot_longer(cols = !Org) %>%
  mutate(name = fct_relevel(name, week_order))   # set the order of variables
# summary_spend_time_workshop_long

#Average praise_teen parent points pp5
summary_praise_teen_workshop <- plhdata_org_clean %>%
  group_by(Org) %>%
  summarise(across(praise_teen_workshop_vars, mean, na.rm = TRUE))
colnames(summary_praise_teen_workshop) <- naming_conventions(colnames(summary_praise_teen_workshop), "rp.contact.field.parent_point_count_praise_teen_w_")
# summary_praise_teen_workshop

# Make the table longer so that it is in a format for use in ggplot
summary_praise_teen_workshop_long <- summary_praise_teen_workshop %>%
  pivot_longer(cols = !Org) %>%
  mutate(name = fct_relevel(name, week_order))   # set the order of variables
# summary_praise_teen_workshop_long

#Average instruct_positively parent points pp6
summary_instruct_positively_workshop <- plhdata_org_clean %>%
  group_by(Org) %>%
  summarise(across(instruct_positively_workshop_vars, mean, na.rm = TRUE))
colnames(summary_instruct_positively_workshop) <- naming_conventions(colnames(summary_instruct_positively_workshop), "rp.contact.field.parent_point_count_instruct_positively_w_")
# summary_instruct_positively_workshop

# Make the table longer so that it is in a format for use in ggplot
summary_instruct_positively_workshop_long <- summary_instruct_positively_workshop %>%
  pivot_longer(cols = !Org) %>%
  mutate(name = fct_relevel(name, week_order))   # set the order of variables
# summary_instruct_positively_workshop_long

#Average breathe parent points pp7
summary_breathe_workshop <- plhdata_org_clean %>%
  group_by(Org) %>%
  summarise(across(breathe_workshop_vars, mean, na.rm = TRUE))
colnames(summary_breathe_workshop) <- naming_conventions(colnames(summary_breathe_workshop), "rp.contact.field.parent_point_count_breathe_w_")
# summary_breathe_workshop

# Make the table longer so that it is in a format for use in ggplot
summary_breathe_workshop_long <- summary_breathe_workshop %>%
  pivot_longer(cols = !Org) %>%
  mutate(name = fct_relevel(name, week_order))   # set the order of variables
# summary_breathe_workshop_long

#Average spend_time parent points pp8
summary_money_workshop <- plhdata_org_clean %>%
  group_by(Org) %>%
  summarise(across(money_workshop_vars, mean, na.rm = TRUE))
colnames(summary_money_workshop) <- naming_conventions(colnames(summary_money_workshop), "rp.contact.field.parent_point_count_money_w_")
# summary_money_workshop

# Make the table longer so that it is in a format for use in ggplot
summary_money_workshop_long <- summary_money_workshop %>%
  pivot_longer(cols = !Org) %>%
  mutate(name = fct_relevel(name, week_order))   # set the order of variables
# summary_money_workshop_long

#Average consequence parent points pp9
summary_consequence_workshop <- plhdata_org_clean %>%
  group_by(Org) %>%
  summarise(across(consequence_workshop_vars, mean, na.rm = TRUE))
colnames(summary_consequence_workshop) <- naming_conventions(colnames(summary_consequence_workshop), "rp.contact.field.parent_point_count_consequence_w_")
# summary_consequence_workshop

# Make the table longer so that it is in a format for use in ggplot
summary_consequence_workshop_long <- summary_consequence_workshop %>%
  pivot_longer(cols = !Org) %>%
  mutate(name = fct_relevel(name, week_order))   # set the order of variables
# summary_consequence_workshop_long

#Average safe parent points pp10
summary_safe_workshop <- plhdata_org_clean %>%
  group_by(Org) %>%
  summarise(across(safe_workshop_vars, mean, na.rm = TRUE))
colnames(summary_safe_workshop) <- naming_conventions(colnames(summary_safe_workshop), "rp.contact.field.parent_point_count_safe_w_")
# summary_safe_workshop

# Make the table longer so that it is in a format for use in ggplot
summary_safe_workshop_long <- summary_safe_workshop %>%
  pivot_longer(cols = !Org) %>%
  mutate(name = fct_relevel(name, week_order))   # set the order of variables
# summary_safe_workshop_long

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
# summary_table_habits_all$`Relax`
# summary_table_habits_all$`Treat yourself`
# summary_table_habits_all$`Praise yourself`
# summary_table_habits_all$`Spend time`
# summary_table_habits_all$`Praise teen`
# summary_table_habits_all$`Instruct positively`
# summary_table_habits_all$`Breathe`
# summary_table_habits_all$`Money`
# summary_table_habits_all$`Consequence`
# summary_table_habits_all$`Safe`

summary_plot_habits_all <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_all, .f = ~summary_plot(columns_to_summarise = .x, replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot"))
names(summary_plot_habits_all) <- data_habit_parent_points_all_neat
# summary_plot_habits_all$Relax

summary_table_habits_self_care <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_self_care, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_self_care) <- data_habit_parent_points_w_self_care_neat
# summary_table_habits_self_care$`Relax`
# summary_table_habits_self_care$`Treat yourself`
# summary_table_habits_self_care$`Praise yourself`
# summary_table_habits_self_care$`Spend time`
# summary_table_habits_self_care$`Praise teen`
# summary_table_habits_self_care$`Instruct positively`
# summary_table_habits_self_care$`Breathe`
# summary_table_habits_self_care$`Money`
# summary_table_habits_self_care$`Consequence`
# summary_table_habits_self_care$`Safe`

summary_plot_habits_self_care <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_self_care, .f = ~summary_plot(columns_to_summarise = .x, replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot"))
names(summary_plot_habits_self_care) <- data_habit_parent_points_w_self_care_neat
# summary_plot_habits_self_care$Relax

summary_table_habits_1on1 <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_1on1, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_1on1) <- data_habit_parent_points_w_1on1_neat
# summary_table_habits_1on1$`Relax`
# summary_table_habits_1on1$`Treat yourself`
# summary_table_habits_1on1$`Praise yourself`
# summary_table_habits_1on1$`Spend time`
# summary_table_habits_1on1$`Praise teen`
# summary_table_habits_1on1$`Instruct positively`
# summary_table_habits_1on1$`Breathe`
# summary_table_habits_1on1$`Money`
# summary_table_habits_1on1$`Consequence`
# summary_table_habits_1on1$`Safe`

summary_table_habits_praise <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_praise, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_praise) <- data_habit_parent_points_w_praise_neat
# summary_table_habits_praise$`Relax`
# summary_table_habits_praise$`Treat yourself`
# summary_table_habits_praise$`Praise yourself`
# summary_table_habits_praise$`Spend time`
# summary_table_habits_praise$`Praise teen`
# summary_table_habits_praise$`Instruct positively`
# summary_table_habits_praise$`Breathe`
# summary_table_habits_praise$`Money`
# summary_table_habits_praise$`Consequence`
# summary_table_habits_praise$`Safe`

summary_table_habits_instruct <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_instruct, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_instruct) <- data_habit_parent_points_w_instruct_neat
# summary_table_habits_instruct$`Relax`
# summary_table_habits_instruct$`Treat yourself`
# summary_table_habits_instruct$`Praise yourself`
# summary_table_habits_instruct$`Spend time`
# summary_table_habits_instruct$`Praise teen`
# summary_table_habits_instruct$`Instruct positively`
# summary_table_habits_instruct$`Breathe`
# summary_table_habits_instruct$`Money`
# summary_table_habits_instruct$`Consequence`
# summary_table_habits_instruct$`Safe`

summary_table_habits_stress <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_stress, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_stress) <- data_habit_parent_points_w_stress_neat
# summary_table_habits_stress$`Relax`
# summary_table_habits_stress$`Treat yourself`
# summary_table_habits_stress$`Praise yourself`
# summary_table_habits_stress$`Spend time`
# summary_table_habits_stress$`Praise teen`
# summary_table_habits_stress$`Instruct positively`
# summary_table_habits_stress$`Breathe`
# summary_table_habits_stress$`Money`
# summary_table_habits_stress$`Consequence`
# summary_table_habits_stress$`Safe`

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
# summary_table_habits_rules$`Relax`
# summary_table_habits_rules$`Treat yourself`
# summary_table_habits_rules$`Praise yourself`
# summary_table_habits_rules$`Spend time`
# summary_table_habits_rules$`Praise teen`
# summary_table_habits_rules$`Instruct positively`
# summary_table_habits_rules$`Breathe`
# summary_table_habits_rules$`Money`
# summary_table_habits_rules$`Consequence`
# summary_table_habits_rules$`Safe`

#Error as Column `rp.contact.field.parent_point_count_money_w_consequence` doesn't exist.
summary_table_habits_consequence <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_consequence, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_consequence) <- data_habit_parent_points_w_consequence_neat
# summary_table_habits_consequence$`Relax`
# summary_table_habits_consequence$`Treat yourself`
# summary_table_habits_consequence$`Praise yourself`
# summary_table_habits_consequence$`Spend time`
# summary_table_habits_consequence$`Praise teen`
# summary_table_habits_consequence$`Instruct positively`
# summary_table_habits_consequence$`Breathe`
# summary_table_habits_consequence$`Money`
# summary_table_habits_consequence$`Consequence`
# summary_table_habits_consequence$`Safe`

summary_table_habits_solve <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_solve, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_solve) <- data_habit_parent_points_w_solve_neat
# summary_table_habits_solve$`Relax`
# summary_table_habits_solve$`Treat yourself`
# summary_table_habits_solve$`Praise yourself`
# summary_table_habits_solve$`Spend time`
# summary_table_habits_solve$`Praise teen`
# summary_table_habits_solve$`Instruct positively`
# summary_table_habits_solve$`Breathe`
# summary_table_habits_solve$`Money`
# summary_table_habits_solve$`Consequence`
# summary_table_habits_solve$`Safe`

summary_table_habits_safe <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_safe, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_safe) <- data_habit_parent_points_w_safe_neat
# summary_table_habits_safe$`Relax`
# summary_table_habits_safe$`Treat yourself`
# summary_table_habits_safe$`Praise yourself`
# summary_table_habits_safe$`Spend time`
# summary_table_habits_safe$`Praise teen`
# summary_table_habits_safe$`Instruct positively`
# summary_table_habits_safe$`Breathe`
# summary_table_habits_safe$`Money`
# summary_table_habits_safe$`Consequence`
# summary_table_habits_safe$`Safe`

#Error as Column `rp.contact.field.parent_point_count_money_w_crisis` doesn't exist.
summary_table_habits_crisis <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_crisis, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_crisis) <- data_habit_parent_points_w_crisis_neat
# summary_table_habits_crisis$`Relax`
# summary_table_habits_crisis$`Treat yourself`
# summary_table_habits_crisis$`Praise yourself`
# summary_table_habits_crisis$`Spend time`
# summary_table_habits_crisis$`Praise teen`
# summary_table_habits_crisis$`Instruct positively`
# summary_table_habits_crisis$`Breathe`
# summary_table_habits_crisis$`Money`
# summary_table_habits_crisis$`Consequence`
# summary_table_habits_crisis$`Safe`

#Error as rp.contact.field.parent_point_count_praise_teen_w_celebrate does not exist
summary_table_habits_celebrate <- plhdata_org_clean %>%
  map(.x = data_habit_parent_points_w_celebrate, .f = ~summary_table(columns_to_summarise = .x, wider_table = TRUE, include_margins = TRUE))
names(summary_table_habits_celebrate) <- data_habit_parent_points_w_celebrate_neat
# summary_table_habits_celebrate$`Relax`
# summary_table_habits_celebrate$`Treat yourself`
# summary_table_habits_celebrate$`Praise yourself`
# summary_table_habits_celebrate$`Spend time`
# summary_table_habits_celebrate$`Praise teen`
# summary_table_habits_celebrate$`Instruct positively`
# summary_table_habits_celebrate$`Breathe`
# summary_table_habits_celebrate$`Money`
# summary_table_habits_celebrate$`Consequence`
# summary_table_habits_celebrate$`Safe`

#mean average number of parent points given per org
summary_mean_habits <- plhdata_org_clean %>%
  group_by(Org)  %>%
  summarise(across(data_habit_parent_points_all, mean, na.rm = TRUE))
colnames(summary_mean_habits) <- naming_conventions(colnames(summary_mean_habits), "rp.contact.field.parent_point_count_")
# summary_mean_habits

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
# summary_table_completion_level$`Baseline Survey`
# summary_table_completion_level$`Self Care`
# summary_table_completion_level$`One-on-one Time`
# summary_table_completion_level$Praise
# summary_table_completion_level$`Positive Instructions`
# summary_table_completion_level$`Managing Stress`
# summary_table_completion_level$`Family Budgets`
# summary_table_completion_level$Rules
# summary_table_completion_level$`Calm Consequences`
# summary_table_completion_level$`Problem Solving`
# summary_table_completion_level$`Teen Safety`
# summary_table_completion_level$`Dealing with Crisis`
# summary_table_completion_level$`Celebration & Next Steps`
# etc.


# or to get all tables:
# summary_table_completion_level

#mean average completion level per org
summary_mean_completion_level <- plhdata_org_clean %>%
  group_by(Org)  %>%
  summarise(across(data_completion_level, mean, na.rm = TRUE))
colnames(summary_mean_completion_level) <- naming_conventions(colnames(summary_mean_completion_level), "rp.contact.field.w_", "_completion_level")
# summary_mean_completion_level

# Percentage of users who completed a workshop out of those who started it
# nrow(plhdata_org_clean %>% filter(rp.contact.field.w_money_completion_level == 100)) / nrow(plhdata_org_clean %>% filter(rp.contact.field.w_money_started == "true"))
# nrow(plhdata_org_clean %>% filter(rp.contact.field.w_money_completion_level == 100)) / nrow(plhdata_org_clean %>% filter(rp.contact.field.w_money_completion_level > 0))

for (i in 1:length(summary_table_completion_level)){
  if (!"100" %in% names(summary_table_completion_level[[i]])){
    summary_table_completion_level[[i]]$`100` <- 0
  }
}

relative_perc_completed <- imap(summary_table_completion_level, ~.x %>%
                               mutate(started = Total - `0` - `NA`,
                                      perc_completed = `100`/started*100) %>%
                               select(c(Org, started, perc_completed)))
table_perc_completed <- plyr::ldply(relative_perc_completed) %>%
  pivot_wider(id_cols = Org, names_from = .id, values_from = perc_completed)
  
table_ws_started <- plyr::ldply(relative_perc_completed) %>%
  pivot_wider(id_cols = Org, names_from = .id, values_from = started)

# Survey - past week  ----------------------------------------------------------------------------
r_variables_names <- readxl::read_excel("r_variables_names.xlsx")
data_survey_past_week_all <- r_variables_names %>% filter(location_ID == "survey_past_week")
summary_table_survey_past_week <- tabulate_with_metadata(location_ID = "survey_past_week")

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

# then to access a table:
# summary_table_survey_past_week$Attention
# summary_table_survey_past_week$Praise
# summary_table_survey_past_week$Stress
# summary_table_survey_past_week$Shouting
# summary_table_survey_past_week$`Money worries`
# summary_table_survey_past_week$Summary
# summary_table_survey_past_week$Hitting
# summary_table_survey_past_week$`Teen activity`
# summary_table_survey_past_week$`Lockdown?`
# summary_table_survey_past_week$`Knowledge of teen activity in non-lockdown week`
# summary_table_survey_past_week$`Sexual safety talk`
# summary_table_survey_past_week$`Teen COVID safe`

#TODO iff "7" to 7.1? - TODO - what do they mean by this?

# Home Practice ------------------------------------------------------------------

# rp.contact.field.w_1on1_hp_done
# rp.contact.field.w_1on1_hp_mood
# rp-contact-field.w_1on1_hp_challenge_list #doesn't include latest challenge rp.contact.field.w_1on1_hp_challenge
# 
# rp.contact.field.w_1on1_hp_done
# rp.contact.field.w_1on1_hp_mood
# rp-contact-field.w_1on1_hp_challenge_list #does't include latest challenge rp.contact.field.w_1on1_hp_challenge



# parent library ------------------------------------------------------------------
data_library <- c("rp.contact.field.click_hs_parent_centre_count", "rp.contact.field.click_pc_help_count",
                "rp.contact.field.click_pc_my_tips_count", "rp.contact.field.click_pc_essential_tools_count",
                "rp.contact.field.click_pc_covid_count", "rp.contact.field.click_pc_customisation_count",
                "rp.contact.field.click_pc_relax_and_activities_count", "rp.contact.field.click_pc_support_contacts_count",
                "rp.contact.field.click_pc_evidence_base_count", "rp.contact.field.click_pc_technical_support_count",
                "rp.contact.field.click_pc_message_archive_count","rp.contact.field.click_pc_bereavement_count")

data_library_neat <- naming_conventions(data_library, replace = "rp.contact.field.click_pc_", replace_after = "count")
data_library_neat <- naming_conventions(data_library_neat, replace = "Rp.contact.field.click hs")
summary_table_library <- plhdata_org_clean %>%
  map(.x = data_library, .f = ~summary_table(columns_to_summarise = .x, include_margins = TRUE))
names(summary_table_library) <- data_library_neat

# then to access a table:
# summary_table_library$`My tips `
# summary_table_library$` parent centre `
# summary_table_library$`Essential tools `
# summary_table_library$`Relax and activities `
# summary_table_library$`Customisation `
# summary_table_library$`Help `
# summary_table_library$`Technical support `
# summary_table_library$`Covid `
# summary_table_library$`Bereavement `

#mean library clicks (button type per organisation)
#mean library clicks per workshop week is not stored to my knowledge

summary_library_mean <- plhdata_org_clean %>%
  group_by(Org)  %>%
  summarise(across(data_library, mean, na.rm = TRUE))
colnames(summary_library_mean) <- naming_conventions(colnames(summary_library_mean), "rp.contact.field.click_", "_count")
# summary_library_mean



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
# tables_app_opens$`Total`
# tables_app_opens$`Welcome and Self care`
# tables_app_opens$`One-on-one time`
# tables_app_opens$`Praise`
# tables_app_opens$`Positive Instructions`
# tables_app_opens$`Managing Stress`
# tables_app_opens$`Family Budget`
# tables_app_opens$`Rules`
# tables_app_opens$`Calm Consequences`
# tables_app_opens$`Problem Solving`
# tables_app_opens$`Teen Safety`
# tables_app_opens$`Crisis`
# tables_app_opens$`Celebration & Next Steps`

#Average app opens per ws week
summary_mean_appopens <- plhdata_org_clean %>%
  group_by(Org) %>%
  summarise(across(data_app_opens, mean, na.rm = TRUE))
colnames(summary_mean_appopens)[2:length(summary_mean_appopens)] <- data_app_opens_neat
# summary_mean_appopens

# Make the table longer so that it is in a format for use in ggplot
summary_mean_appopens_long <- summary_mean_appopens %>%
  pivot_longer(cols = !Org) %>%
  mutate(name = fct_relevel(name, data_app_opens_neat))   # set the order of variables
# summary_mean_appopens_long

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
tables_emotional_check_in <- plhdata_org_clean %>%
  map(.x = data_emotional_check_in, .f = ~summary_table(columns_to_summarise = .x))
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

summary_table_survey_completion <- plhdata_org_clean %>%
  summary_table(columns_to_summarise = "rp.contact.field.survey_welcome_and_setup_completion_level", display = FALSE, include_margins = TRUE)
# summary_table_survey_completion

# Descriptive Statistics ------------------------------------------------------------------------------------------
# Gender of App Users
gender_table <- plhdata_org_clean %>%
  split(.$Org) %>%
  map(~summary_table(data = .x, factor = NULL, columns_to_summarise = rp.contact.field.user_gender))
names(gender_table) <- levels(plhdata_org_clean$Org)
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

##################################
##################################
### Notification Data Analysis ###
##################################
##################################

# download push notification data
# TODO: add fuzzy join to get_nf_data function
 nf_data <- get_nf_data()
# 
# # what variables do we want in the nf data - org, sex, - add a few in.
 data_baseline_survey <-
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
   dplyr::select(c(app_user_id, data_baseline_survey))
 
 # link nf data to user data by app_user_id
 # use inner_join: remove from nf anyone not in plhdata_org
 nf_data_join <- inner_join(nf_data, plhdata_org_clean_select)
 
 # Only 8 rows. This is because we have filtered out a lot of the plhdata_org_clean users
 # since their org is NA.
 # e.g.:
 #plhdata_org %>% filter(app_user_id == "73d882bf9283163d") %>% select(rp.contact.field.organisation_code)
 # we're throwing away a lot of data over this missing organisation. I think we need to reconsider
 # how to handle these?
 # Additionally surely TZ has only one organisation?
 
 pn_summary_count <- nf_data_join %>%
   group_by(app_user_id, Org, rp.contact.field._app_language) %>%
   summarise(
     number_received = max(app_user_record_id),
     number_responded = sum(!is.na(action_id)),
     percentage_responded = number_responded / number_received *
       100
   )
 #pn_summary_count
 
 # pn_summary_means <-
 #pn_summary_count %>%
 #  dplyr::summarise(dplyr::across(2:4, mean))
 
 # If we were to use all of the nf_data (except the "temp_" rows)
 nf_data_summary <- nf_data %>%
   filter(!grepl("temp", app_user_id)) %>% # remove the "temps"
   group_by(app_user_id) %>%
   summarise(
     number_received = max(app_user_record_id),
     number_responded = sum(!is.na(action_id)),
     percentage_responded = number_responded / number_received *
       100
   )
 
 #nf_data_summary %>%
 #  dplyr::summarise(dplyr::across(2:4, mean))