# Script to update data ---------------------------------------
setwd("C:/Users/lclem/OneDrive/Documents/GitHub/ParentAppDataScripts")

update_data <- function(old_data_file = "plhdata_org_clean_dat.RDS"){
  old_data <- readRDS(old_data_file)
  var_names <- names(old_data)
  
  # rp.contact.field.parent_point_count_money_w_consequence, 
  # parent_point_count_money_w_crisis
  # rp.contact.field.parent_point_count_money_w_celebrate
  #"rp.contact.field.parent_point_count_consequence_w_rules", "rp.contact.field.parent_point_count_consequence_w_crisis",
  # dont exist. 
  plhdata_org <- get_user_data(merge_check = FALSE) # select 1 if you want to merge in changes (yes)
  
  plhdata_org <- plhdata_org %>%
    filter(as.POSIXlt(plhdata_org$rp.contact.field._server_sync_latest, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT") >= (Sys.time() - 60*60*24))
  plhdata_org <- plhdata_org %>%
    filter(as.POSIXlt(plhdata_org$rp.contact.field._server_sync_latest, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT") <= (Sys.time()))
  
  if (nrow(plhdata_org) == 0){
    print("No update required")
    saveRDS(plhdata_org, "plhdata_org_clean_dat.Rdata")
  } else {
    
    # data cleaning -----------------------------
    plhdata_org$Organisation <- forcats::as_factor(tidyr::replace_na(plhdata_org$Organisation, "Miss"))
    plhdata_org$rp.contact.field.organisation_code <- forcats::as_factor(tidyr::replace_na(plhdata_org$rp.contact.field.organisation_code, "Miss"))
    plhdata_org$rp.contact.field.organisation_code<-as_factor(replace_na(plhdata_org$rp.contact.field.organisation_code, "Miss"))
    plhdata_org$organisation_full <- interaction(x=list(plhdata_org$Organisation,
                                                        plhdata_org$rp.contact.field.organisation_code), drop=TRUE)
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
    plhdata_org_clean <- plhdata_org %>%
      filter(Org != "Other")%>%
      mutate(Org = factor(Org))
    plhdata_org_clean <- plhdata_org_clean %>%
      dplyr::filter(!is.na(app_version))
    
    # make vars numeric
    plhdata_org_clean$rp.contact.field.user_age <- as.numeric(plhdata_org_clean$rp.contact.field.user_age)
    plhdata_org_clean$rp.contact.field.household_adults <- as.numeric(plhdata_org_clean$rp.contact.field.household_adults)
    plhdata_org_clean$rp.contact.field.household_teens <- as.numeric(plhdata_org_clean$rp.contact.field.household_teens)
    plhdata_org_clean$rp.contact.field.household_babies <- as.numeric(plhdata_org_clean$rp.contact.field.household_babies)
    plhdata_org_clean$rp.contact.field.household_children <- as.numeric(plhdata_org_clean$rp.contact.field.household_children)
    plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed)
    plhdata_org_clean$rp.contact.field.parent_point_count_relax_w_self_care <- as.numeric(plhdata_org_clean$rp.contact.field.parent_point_count_relax_w_self_care)
    plhdata_org_clean <- plhdata_org_clean %>%
      mutate(across(ends_with("_completion_level"), ~as.numeric(.)))
    plhdata_org_clean <- plhdata_org_clean %>%
      mutate(across(starts_with("rp.contact.field.parent_point"), ~as.numeric(.)))
    plhdata_org_clean$rp.contact.field.app_launch_count <- as.numeric(plhdata_org_clean$rp.contact.field.app_launch_count)
    plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed)
    plhdata_org_clean$rp.contact.field.first_app_open <- as.Date(plhdata_org_clean$rp.contact.field.first_app_open)
    plhdata_org_clean$hsqsclickedws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care)
    plhdata_org_clean$rp.contact.field.max_days_between_app_launches <- as.numeric(plhdata_org_clean$rp.contact.field.max_days_between_app_launches)
    
    # select variables 
    var_names <- var_names[-length(var_names)]
    plhdata_org_clean_dat <- plhdata_org_clean %>%
      dplyr::select(c(var_names))
    
    # add timestamp
    if (nrow(plhdata_org_clean_dat) > 0){
      plhdata_org_clean_dat$R_timestamp <- date()
      
      # then we bind the old data with this updated data
      plhdata_org_clean_dat2 <- bind_rows(old_data, plhdata_org_clean_dat) 
      saveRDS(plhdata_org_clean_dat2, "plhdata_org_clean_dat.RDS")
      
    } else {
      print("No update required")
      saveRDS(plhdata_org, "plhdata_org_clean_dat.RDS")
    }
  }
}

update_data()

# Data frame for first time --------------------------------------------------------------------------
# creating for first time
plhdata_org <- get_user_data(merge_check = FALSE) # select 1 if you want to merge in changes (yes)
plhdata_org$Organisation <- forcats::as_factor(tidyr::replace_na(plhdata_org$Organisation, "Miss"))
plhdata_org$rp.contact.field.organisation_code <- forcats::as_factor(tidyr::replace_na(plhdata_org$rp.contact.field.organisation_code, "Miss"))
plhdata_org$rp.contact.field.organisation_code<-as_factor(replace_na(plhdata_org$rp.contact.field.organisation_code, "Miss"))
plhdata_org$organisation_full <- interaction(x=list(plhdata_org$Organisation,
                                                    plhdata_org$rp.contact.field.organisation_code), drop=TRUE)
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
plhdata_org_clean <- plhdata_org %>%
  filter(Org != "Other")%>%
  mutate(Org = factor(Org))
plhdata_org_clean <- plhdata_org_clean %>%
  dplyr::filter(!is.na(app_version))
# make vars numeric
plhdata_org_clean$rp.contact.field.user_age <- as.numeric(plhdata_org_clean$rp.contact.field.user_age)
plhdata_org_clean$rp.contact.field.household_adults <- as.numeric(plhdata_org_clean$rp.contact.field.household_adults)
plhdata_org_clean$rp.contact.field.household_teens <- as.numeric(plhdata_org_clean$rp.contact.field.household_teens)
plhdata_org_clean$rp.contact.field.household_babies <- as.numeric(plhdata_org_clean$rp.contact.field.household_babies)
plhdata_org_clean$rp.contact.field.household_children <- as.numeric(plhdata_org_clean$rp.contact.field.household_children)
plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed)
plhdata_org_clean$rp.contact.field.parent_point_count_relax_w_self_care <- as.numeric(plhdata_org_clean$rp.contact.field.parent_point_count_relax_w_self_care)
plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(ends_with("_completion_level"), ~as.numeric(.)))
plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(starts_with("rp.contact.field.parent_point"), ~as.numeric(.)))
plhdata_org_clean$rp.contact.field.app_launch_count <- as.numeric(plhdata_org_clean$rp.contact.field.app_launch_count)
plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed)
plhdata_org_clean$rp.contact.field.first_app_open <- as.Date(plhdata_org_clean$rp.contact.field.first_app_open)
plhdata_org_clean$hsqsclickedws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care)
plhdata_org_clean$rp.contact.field.max_days_between_app_launches <- as.numeric(plhdata_org_clean$rp.contact.field.max_days_between_app_launches)

# select variables (put them into groups here)
data_baseline_survey <- c("rp.contact.field.survey_welcome_completed", "rp.contact.field.user_gender",
                          "rp.contact.field.user_age", "rp.contact.field.household_adults",
                          "rp.contact.field.household_teens", "rp.contact.field.household_babies",
                          "rp.contact.field.household_children", "rp.contact.field._app_language", "app_version", "rp.contact.field.do_workshops_together")
relax_workshop_vars <- c( "rp.contact.field.parent_point_count_relax_w_self_care", "rp.contact.field.parent_point_count_relax_w_1on1",
                          "rp.contact.field.parent_point_count_relax_w_praise", "rp.contact.field.parent_point_count_relax_w_instruct",
                          "rp.contact.field.parent_point_count_relax_w_stress", "rp.contact.field.parent_point_count_relax_w_money",
                          "rp.contact.field.parent_point_count_relax_w_rules", "rp.contact.field.parent_point_count_relax_w_consequence",
                          "rp.contact.field.parent_point_count_relax_w_solve", "rp.contact.field.parent_point_count_relax_w_safe",
                          "rp.contact.field.parent_point_count_relax_w_crisis","rp.contact.field.parent_point_count_relax_w_celebrate")
treat_yourself_workshop_vars <- c( "rp.contact.field.parent_point_count_treat_yourself_w_self_care", "rp.contact.field.parent_point_count_treat_yourself_w_1on1",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_praise", "rp.contact.field.parent_point_count_treat_yourself_w_instruct",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_stress", "rp.contact.field.parent_point_count_treat_yourself_w_money",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_rules", "rp.contact.field.parent_point_count_treat_yourself_w_consequence",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_solve", "rp.contact.field.parent_point_count_treat_yourself_w_safe",
                                   "rp.contact.field.parent_point_count_treat_yourself_w_crisis","rp.contact.field.parent_point_count_treat_yourself_w_celebrate")
praise_yourself_workshop_vars <- c( "rp.contact.field.parent_point_count_praise_yourself_w_self_care", "rp.contact.field.parent_point_count_praise_yourself_w_1on1",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_praise", "rp.contact.field.parent_point_count_praise_yourself_w_instruct",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_stress", "rp.contact.field.parent_point_count_praise_yourself_w_money",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_rules", "rp.contact.field.parent_point_count_praise_yourself_w_consequence",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_solve", "rp.contact.field.parent_point_count_praise_yourself_w_safe",
                                    "rp.contact.field.parent_point_count_praise_yourself_w_crisis","rp.contact.field.parent_point_count_praise_yourself_w_celebrate")
spend_time_workshop_vars <- c( "rp.contact.field.parent_point_count_spend_time_w_self_care", "rp.contact.field.parent_point_count_spend_time_w_1on1",
                               "rp.contact.field.parent_point_count_spend_time_w_praise", "rp.contact.field.parent_point_count_spend_time_w_instruct",
                               "rp.contact.field.parent_point_count_spend_time_w_stress", "rp.contact.field.parent_point_count_spend_time_w_money",
                               "rp.contact.field.parent_point_count_spend_time_w_rules", "rp.contact.field.parent_point_count_spend_time_w_consequence",
                               "rp.contact.field.parent_point_count_spend_time_w_solve", "rp.contact.field.parent_point_count_spend_time_w_safe",
                               "rp.contact.field.parent_point_count_spend_time_w_crisis","rp.contact.field.parent_point_count_spend_time_w_celebrate")
praise_teen_workshop_vars <- c( "rp.contact.field.parent_point_count_praise_teen_w_self_care", "rp.contact.field.parent_point_count_praise_teen_w_1on1",
                                "rp.contact.field.parent_point_count_praise_teen_w_praise", "rp.contact.field.parent_point_count_praise_teen_w_instruct",
                                "rp.contact.field.parent_point_count_praise_teen_w_stress", "rp.contact.field.parent_point_count_praise_teen_w_money",
                                "rp.contact.field.parent_point_count_praise_teen_w_rules", "rp.contact.field.parent_point_count_praise_teen_w_consequence",
                                "rp.contact.field.parent_point_count_praise_teen_w_solve", "rp.contact.field.parent_point_count_praise_teen_w_safe",
                                "rp.contact.field.parent_point_count_praise_teen_w_crisis","rp.contact.field.parent_point_count_praise_teen_w_celebrate")
instruct_positively_workshop_vars <- c( "rp.contact.field.parent_point_count_instruct_positively_w_self_care", "rp.contact.field.parent_point_count_instruct_positively_w_1on1",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_praise", "rp.contact.field.parent_point_count_instruct_positively_w_instruct",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_stress", "rp.contact.field.parent_point_count_instruct_positively_w_money",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_rules", "rp.contact.field.parent_point_count_instruct_positively_w_consequence",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_solve", "rp.contact.field.parent_point_count_instruct_positively_w_safe",
                                        "rp.contact.field.parent_point_count_instruct_positively_w_crisis","rp.contact.field.parent_point_count_instruct_positively_w_celebrate")
breathe_workshop_vars <- c( "rp.contact.field.parent_point_count_breathe_w_self_care", "rp.contact.field.parent_point_count_breathe_w_1on1",
                            "rp.contact.field.parent_point_count_breathe_w_praise", "rp.contact.field.parent_point_count_breathe_w_instruct",
                            "rp.contact.field.parent_point_count_breathe_w_stress", "rp.contact.field.parent_point_count_breathe_w_money",
                            "rp.contact.field.parent_point_count_breathe_w_rules", "rp.contact.field.parent_point_count_breathe_w_consequence",
                            "rp.contact.field.parent_point_count_breathe_w_solve", "rp.contact.field.parent_point_count_breathe_w_safe",
                            "rp.contact.field.parent_point_count_breathe_w_crisis","rp.contact.field.parent_point_count_breathe_w_celebrate")
money_workshop_vars <- c( "rp.contact.field.parent_point_count_money_w_self_care", "rp.contact.field.parent_point_count_money_w_1on1",
                          "rp.contact.field.parent_point_count_money_w_praise", "rp.contact.field.parent_point_count_money_w_instruct",
                          "rp.contact.field.parent_point_count_money_w_stress", "rp.contact.field.parent_point_count_money_w_money",
                          "rp.contact.field.parent_point_count_money_w_rules", #"rp.contact.field.parent_point_count_money_w_consequence",
                          "rp.contact.field.parent_point_count_money_w_solve", "rp.contact.field.parent_point_count_money_w_safe")
#"rp.contact.field.parent_point_count_money_w_crisis","rp.contact.field.parent_point_count_money_w_celebrate")
consequence_workshop_vars <- c( "rp.contact.field.parent_point_count_consequence_w_self_care", "rp.contact.field.parent_point_count_consequence_w_1on1",
                                "rp.contact.field.parent_point_count_consequence_w_praise", "rp.contact.field.parent_point_count_consequence_w_instruct",
                                "rp.contact.field.parent_point_count_consequence_w_stress", "rp.contact.field.parent_point_count_consequence_w_money",
                                #"rp.contact.field.parent_point_count_consequence_w_rules", "rp.contact.field.parent_point_count_consequence_w_crisis",
                                "rp.contact.field.parent_point_count_consequence_w_consequence",
                                "rp.contact.field.parent_point_count_consequence_w_solve", "rp.contact.field.parent_point_count_consequence_w_safe",
                                "rp.contact.field.parent_point_count_consequence_w_celebrate")
safe_workshop_vars <- c( "rp.contact.field.parent_point_count_safe_w_self_care", "rp.contact.field.parent_point_count_safe_w_1on1",
                         "rp.contact.field.parent_point_count_safe_w_praise", "rp.contact.field.parent_point_count_safe_w_instruct",
                         "rp.contact.field.parent_point_count_safe_w_stress", "rp.contact.field.parent_point_count_safe_w_money",
                         "rp.contact.field.parent_point_count_safe_w_rules", "rp.contact.field.parent_point_count_safe_w_consequence",
                         "rp.contact.field.parent_point_count_safe_w_solve", "rp.contact.field.parent_point_count_safe_w_safe",
                         "rp.contact.field.parent_point_count_safe_w_crisis","rp.contact.field.parent_point_count_safe_w_celebrate")
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
                                            "rp.contact.field.parent_point_count_consequence_w_consequence", #"rp.contact.field.parent_point_count_money_w_consequence", 
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
                                          "rp.contact.field.parent_point_count_consequence_w_celebrate",#"rp.contact.field.parent_point_count_money_w_celebrate", 
                                          "rp.contact.field.parent_point_count_safe_w_celebrate", "rp.contact.field.parent_point_count_instruct_positively_w_celebrate")
data_completion_level <- c("rp.contact.field.survey_welcome_and_setup_completion_level", "rp.contact.field.w_self_care_completion_level", "rp.contact.field.w_1on1_completion_level",  "rp.contact.field.w_praise_completion_level",
                           "rp.contact.field.w_instruct_completion_level",  "rp.contact.field.w_stress_completion_level",
                           "rp.contact.field.w_money_completion_level",  "rp.contact.field.w_rules_completion_level", #you have "safe_completion" under rules. Is this right?
                           "rp.contact.field.w_consequence_completion_level",  "rp.contact.field.w_solve_completion_level",  "rp.contact.field.w_safe_completion_level",
                           "rp.contact.field.w_crisis_completion_level",  "rp.contact.field.w_celebrate_completion_level")
data_survey_past_week <- c("rp.contact.field.survey_welcome_a_1_final",  "rp.contact.field.survey_welcome_a_2_final",
                           "rp.contact.field.survey_welcome_a_3_final",  "rp.contact.field.survey_welcome_a_4_final",
                           "rp.contact.field.survey_welcome_a_5_part_1_final",  "rp.contact.field.survey_welcome_a_5_part_2_final",
                           "rp.contact.field.survey_welcome_a_6_final",  "rp.contact.field.survey_welcome_a_7_part_1_final",
                           "rp.contact.field.survey_welcome_a_7_part_2_final",  "rp.contact.field.survey_welcome_a_7_part_3_final",
                           "rp.contact.field.survey_welcome_a_8_final", "rp.contact.field.survey_welcome_a_9_final")
data_click <- c("rp.contact.field.click_hs_parent_centre_count", "rp.contact.field.click_pc_help_count",
                "rp.contact.field.click_pc_my_tips_count", "rp.contact.field.click_pc_essential_tools_count",
                "rp.contact.field.click_pc_covid_count", "rp.contact.field.click_pc_customisation_count",
                "rp.contact.field.click_pc_relax_and_activities_count", "rp.contact.field.click_pc_support_contacts_count",
                "rp.contact.field.click_pc_evidence_base_count", "rp.contact.field.click_pc_technical_support_count",
                "rp.contact.field.click_pc_message_archive_count")
data_weekly_workshops <- c("rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care",
                           "hsqsclickedws1", "rp.contact.field.click_hs_weekly_workshops_quick_start_w_1on1",
                           "rp.contact.field.click_hs_weekly_workshops_quick_start_w_praise", "rp.contact.field.click_hs_weekly_workshops_quick_start_w_instruct",
                           "rp.contact.field.click_hs_weekly_workshops_quick_start_w_stress", "rp.contact.field.click_hs_weekly_workshops_quick_start_w_money",
                           "rp.contact.field.click_hs_weekly_workshops_quick_start_w_rules", "rp.contact.field.click_hs_weekly_workshops_quick_start_w_consequence", 
                           "rp.contact.field.click_hs_weekly_workshops_quick_start_w_solve", "rp.contact.field.click_hs_weekly_workshops_quick_start_w_safe",
                           "rp.contact.field.click_hs_weekly_workshops_quick_start_w_crisis", "rp.contact.field.click_hs_weekly_workshops_quick_start_w_celebrate",
                           "rp.contact.field.click_hs_parent_centre_quick_start_w_self_care", "rp.contact.field.click_hs_parent_centre_quick_start_w_1on1",
                           "rp.contact.field.click_hs_parent_points_quick_start_w_self_care", "rp.contact.field.click_hs_parent_points_quick_start_w_1on1")
data_app_opens <- c("rp.contact.field.app_launch_count","rp.contact.field.app_launch_count_w_self_care", "rp.contact.field.app_launch_count_w_1on1",
                    "rp.contact.field.app_launch_count_w_praise","rp.contact.field.app_launch_count_w_instruct",
                    "rp.contact.field.app_launch_count_w_stress", "rp.contact.field.app_launch_count_w_money",
                    "rp.contact.field.app_launch_count_w_rules", "rp.contact.field.app_launch_count_w_consequence",
                    "rp.contact.field.app_launch_count_w_solve", "rp.contact.field.app_launch_count_w_safe",
                    "rp.contact.field.app_launch_count_w_crisis", "rp.contact.field.app_launch_count_w_celebrate")
data_emotional_check_in <- c("rp.contact.field.w_self_care_welcome_individual_a_final", "rp.contact.field.w_1on1_welcome_individual_a_final", 
                             "rp.contact.field.w_praise_welcome_individual_a_final", "rp.contact.field.w_instruct_welcome_individual_a_final", 
                             "rp.contact.field.w_stress_welcome_individual_a_final", "rp.contact.field.w_money_welcome_individual_a_final", 
                             "rp.contact.field.w_rules_welcome_individual_a_final")
plhdata_org_clean$hsqsclickedws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care)

plhdata_org_clean <- plhdata_org_clean %>%
  select(c(id, createdAt, updatedAt, deletedAt, version, app_user_id, app_version, app_deployment_name,
           rp.contact.field._server_sync_latest, data_baseline_survey,
           relax_workshop_vars, treat_yourself_workshop_vars, praise_yourself_workshop_vars,
           spend_time_workshop_vars, praise_teen_workshop_vars, instruct_positively_workshop_vars,
           breathe_workshop_vars, money_workshop_vars, consequence_workshop_vars, safe_workshop_vars,
           data_habit_parent_points_all, data_habit_parent_points_w_self_care, data_habit_parent_points_w_1on1,
           data_habit_parent_points_w_praise, data_habit_parent_points_w_instruct,
           data_habit_parent_points_w_stress, data_habit_parent_points_w_money, data_habit_parent_points_w_rules,
           data_habit_parent_points_w_consequence, data_habit_parent_points_w_solve,
           data_habit_parent_points_w_safe, data_habit_parent_points_w_crisis, data_habit_parent_points_w_celebrate,
           data_completion_level, data_survey_past_week, data_click, data_weekly_workshops, data_app_opens,
           data_emotional_check_in))
plhdata_org_clean$R_timestamp <- date()

#saveRDS(plhdata_org_clean, "plhdata_org_clean_dat.RDS")
#plhdata_org_clean <- readRDS("plhdata_org_clean_dat.RDS")
