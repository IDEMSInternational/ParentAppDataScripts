# in app pre-test surveys and demographics data for O

# taken from Metabase Analysis Setup.R
country <- "Tanzania"
study <- "Optimisation"
### extract data ----------------------------------------------------------------------
# to get user data
plhdata_org <- get_user_data(site = plh_con, merge_check = FALSE, UIC_Tracker = UIC.Tracker) # select 1 if you want to merge in changes (yes)
#plhdata_org <- plhdata_org1
## Data Cleaning - User Data ## --------------------------------------------------------
## Tidy up "Organisation" Variable:
# replace missing values in Organisation and rp.contact.field.organisation_code by Miss so that it is a factor level
plhdata_org$Organisation <- forcats::as_factor(tidyr::replace_na(plhdata_org$Organisation, "Miss"))

# look and Recode Factor organisation_full to just the main levels # Question: What about "null"?
plhdata_org$rp.contact.field.organisation_code <- as_factor(replace_na(plhdata_org$rp.contact.field.organisation_code, "Miss"))

# Combine Factors Organisation and rp.contact.field.organisation_code 
plhdata_org$organisation_full <- interaction(x=list(plhdata_org$Organisation,
                                                    plhdata_org$rp.contact.field.organisation_code), drop=TRUE)

plhdata_org <- plhdata_org %>%
  mutate(organisation_full = ifelse((rp.contact.field.organisation_code == "organisation_1") & (app_deployment_name %in% c("plh_tz", "PLH TZ")),
                                    "ICS",
                                    as.character(organisation_full)))

# Create Optimisation Group Data for Optimisation Study - Tanzania
valid_ids <- UIC_Tracker_Tanzania %>%
  filter(complete.cases(YourParentAppCode))  %>%
  filter(Study == "Optimisation") %>%
  select(c(YourParentAppCode, opt_cluster, experimental_condition))

plhdata_org_opt_fuzzy <- fuzzyjoin::stringdist_full_join(x = plhdata_org, y = valid_ids, by = c("app_user_id" = "YourParentAppCode"), max_dist = 5)
# get the app user IDs where we have a code from the optimisation group.
valid_app_user_id_TZ <- (plhdata_org_opt_fuzzy %>% filter(!is.na(YourParentAppCode)))$app_user_id
plhdata_org <- plhdata_org %>% 
  mutate(valid_ics_1 = ifelse(app_user_id %in% valid_app_user_id_TZ, TRUE, FALSE)) %>%
  mutate(organisation_full = ifelse(valid_ics_1, "Optimisation Study", organisation_full))

plhdata_org_opt_fuzzy_opt_cluster <- plhdata_org_opt_fuzzy %>% dplyr::select(c(app_user_id, opt_cluster, experimental_condition))
plhdata_org <- full_join(plhdata_org, plhdata_org_opt_fuzzy_opt_cluster, by = c("app_user_id" = "app_user_id"))

# Create Pilot Group Data for Pilot Study - Tanzania
valid_ids <- UIC_Tracker_Tanzania %>%
  filter(complete.cases(YourParentAppCode))  %>%
  filter(Study == "Pilot") %>%
  select(YourParentAppCode)

plhdata_org_ics_fuzzy <- fuzzyjoin::stringdist_full_join(x = plhdata_org, y = valid_ids, by = c("app_user_id" = "YourParentAppCode"), max_dist = 5)

valid_app_user_id_TZ <- (plhdata_org_ics_fuzzy %>% filter(organisation_full == "ICS") %>% filter(!is.na(YourParentAppCode)))$app_user_id
plhdata_org <- plhdata_org %>% 
  mutate(valid_ics = ifelse(organisation_full != "ICS", TRUE,
                            ifelse(app_user_id %in% valid_app_user_id_TZ, TRUE, FALSE))) %>%
  filter(valid_ics)
plhdata_org <- plhdata_org %>%
  mutate(organisation_full = ifelse(app_user_id %in% c("2c5bfeb1c97cffdf", "0e5824bd19aae8c4",
                                                       "48621962b0612b7c", "d5faa072c966ea8d",
                                                       "df1088af5f3d4c87", "5b2ba92c32c6a3e2",
                                                       "f3aff268263b1d62", "a05a0fe6cd3cb52d",
                                                       "7f56c4c0a8a2f36f"),
                                    "ICS",
                                    as.character(organisation_full)))

# add in new row containing ICS, and app_user_id  -  08/09/22
#fab4ne58ac03f920
#oe5824bd19aa8c4
plhdata_org[(nrow(plhdata_org)+1):(nrow(plhdata_org)+2),] <- NA
plhdata_org$app_user_id[(last(nrow(plhdata_org))-1):last(nrow(plhdata_org))] <- c("fab4ne58ac03f920", "oe5824bd19aa8c4")
plhdata_org$organisation_full[(last(nrow(plhdata_org))-1):last(nrow(plhdata_org))] <- c("ICS", "ICS")
plhdata_org$app_version[(last(nrow(plhdata_org))-1):last(nrow(plhdata_org))] <- c("0.0", "0.0")

# TO CHECK:
#plhdata_org_ics_fuzzy %>% filter(!is.na(YourParentAppCode)) %>% dplyr::select(organisation_full, app_user_id, YourParentAppCode)
# Note: "2c5bfeb1c97cffdf" "oe5824bd19aa8c4" are in "Miss.Miss"
plhdata_org$Org <- plyr::revalue(x=plhdata_org$organisation_full, 
                                 replace=c(`ICS` = "ICS", `Optimisation Study` = "Optimisation Study", `Miss.Miss` =  "Other", `Miss.baba` = "Other", `Miss.w` = "Other", `Miss.idems` = "Other",  `Miss.hillcrest` = "Other", `Miss.aqujhk,jafvh` = "Other", `Miss.ParentApp_dev` = "Other", `Miss.CWBSA` = "Other",
                                           `Miss.idems Margherita` = "Other", `Miss.IDEMS Ohad` = "Other", `Miss.983aba50330cf24c` ="Other", `Miss.sdfds`="Other",  `Miss.friend` ="Other", `Miss.myself` ="Other", `Miss.undefined` ="Other",
                                           `Miss.other` ="Other", `Miss.zlto` ="Other", `Miss.hpccc` ="Other", `Miss.seven_passes` ="Other", `Miss.Hillcrest facilitator` ="Other", `Miss.Hillcrest Facilitator ` ="Other", `Miss.a00af0c3b3887330` ="Other",
                                           `Nontobeko.Miss` = "Nontobeko", `Nontobeko.Nontobeko M` = "Nontobeko", `Nontobeko.bbe9ca70c78f7384` = "Nontobeko",  `Nontobeko.nontobekoM` = "Nontobeko",
                                           `Nontobeko.NontobekoM` = "Nontobeko", `Nontobeko.null` ="Nontobeko", `Miss.NontobekoM` = "Nontobeko", 
                                           `Joy.Miss` = "Joy", `Joy.c9097349f34b364c` ="Joy", `Joy.null` ="Joy",
                                           `Dlalanathi.Miss` = "Dlalanathi",  `Dlalanathi.null` = "Dlalanathi", `Miss.dlalanathiThandeka` = "Dlalanathi",  `Dlalanathi.dlanathiThandeka` ="Dlalanathi",
                                           `Dlalanathi.dlalanathThandeka` ="Dlalanathi", `Dlalanathi.dlalanathiThandeka` ="Dlalanathi", `Dlalanathi.dlalanathi` ="Dlalanathi", `Dlalanathi.dlalanithi Thandeka` ="Dlalanathi", 
                                           `Amathuba Collective.Miss` ="Amathuba", `Miss.Amathuba Mzi` ="Amathuba", `Miss.Amathuba Mzi ` ="Amathuba", `Miss.amathuba` ="Amathuba", `Miss.dlalanathi`="Dlalanathi",
                                           `Miss.organisation_1` = "Other", `Miss.organisation_2` = "Other",`Miss.organisation_6` = "Other"))

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
                          ifelse(Org %in% c("ICS", "Optimisation Study"), "Tanzania",
                                 "Other")))

print(country)
if (country == "Tanzania"){
  if (study == "Optimisation"){
    plhdata_org_clean <- plhdata_org_clean %>% filter(Org == "Optimisation Study")
  } else {
    plhdata_org_clean <- plhdata_org_clean %>% filter(Org == "ICS")
  }
} else if (country == "South Africa"){
  plhdata_org_clean <- plhdata_org_clean %>% filter(Org %in% c("Amathuba", "Joy", "Dlalanathi", "Nontobeko"))
}

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
#json_data <- NULL
#for (i in c("self_care", "1on1", "praise", "instruct", "stress", "money", "rules", "consequence", "solve", "safe", "crisis", "celebrate")){
#  # which variables to select?
#  json_data[[i]] <- data.frame(jsonlite::fromJSON(paste0("~/GitHub/parenting-app-ui/packages/app-data/sheets/data_list/generated/w_", i, "_task_gs.json")))
#}
#saveRDS(json_data, file = "data/json_data.RDS")
json_data <- readRDS(file = "data/json_data.RDS")

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
    
    json_data_i <- json_data[[i]]
    # rows.id, rows.individual, rows.together, rows.completed_field
    json_data_i <- json_data_i %>% dplyr::select(c(rows.id, rows.individual, rows.together, rows.completed_field)) %>%
      filter(!rows.id %in% c("home_practice", "hp_review"))
    json_data_i_ind <- json_data_i %>% filter(rows.individual == TRUE)
    completed_rows_ind <- paste0("rp.contact.field.", json_data_i_ind$rows.completed_field)
    json_data_i_tog <- json_data_i %>% filter(rows.together == TRUE)
    completed_rows_tog <- paste0("rp.contact.field.", json_data_i_tog$rows.completed_field)
    
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


# Info wanted ----------------------------------------------------------------

# Baseline Survey
data_baseline_survey <- c("rp.contact.field.survey_welcome_completed", "rp.contact.field.user_gender",
                          "rp.contact.field.user_age", "rp.contact.field.household_adults",
                          "rp.contact.field.household_teens", "rp.contact.field.household_babies",
                          "rp.contact.field.household_children", "rp.contact.field._app_language", "app_version", "rp.contact.field.workshop_path")
r_variables_names <- readxl::read_excel("r_variables_names.xlsx")
data_survey_past_week_all <- r_variables_names %>% filter(location_ID == "survey_initial_1")
data_welcome_survey <- data_survey_past_week_all$metabase_ID
data_welcome_survey_names <- data_survey_past_week_all$display_name

plhdata_org_clean_1 <- plhdata_org_clean %>% dplyr::select(c(id, app_user_id, createdAt, opt_cluster, experimental_condition,
                                                           data_baseline_survey, data_welcome_survey))
names(plhdata_org_clean_1) <- c("ID", "App user ID", "Created at", "Optimisation Cluster", "Experimental Condition",
                              "Welcome survey", "Gender", "Age", "Household adults", "Household teens", "Household babies",
                              "Household children", "App language", "App version", "Workshop path",
                              data_welcome_survey_names)

plhdata_org_clean_1 <- plhdata_org_clean_1 %>% arrange(`Created at`)
plhdata_org_clean_select <- plhdata_org_clean_1[1:100,]

#writexl::write_xlsx(plhdata_org_clean_select, path = "TZ_Optimisation_100_20221207.xlsx")



# Completion Level ---------------------------------------------------
# Tab 2 -------
data_completion_level <- c("rp.contact.field.w_self_care_completion_level", "rp.contact.field.w_1on1_completion_level",  "rp.contact.field.w_praise_completion_level",
                           "rp.contact.field.w_instruct_completion_level",  "rp.contact.field.w_stress_completion_level",
                           "rp.contact.field.w_money_completion_level",  "rp.contact.field.w_rules_completion_level", #you have "safe_completion" under rules. Is this right?
                           "rp.contact.field.w_consequence_completion_level",  "rp.contact.field.w_solve_completion_level",  "rp.contact.field.w_safe_completion_level",
                           "rp.contact.field.w_crisis_completion_level",  "rp.contact.field.w_celebrate_completion_level")
completion_vars <- c("Self Care Level", "One-on-one Time Level", "Praise Level", "Positive Instructions Level", "Managing Stress Level", "Family Budgets Level", "RulesLevel", "Calm Consequences Level", "Problem Solving Level", "Teen Safety Level", "Dealing with Crisis Level", "Celebration & Next Steps Level")

plhdata_org_clean_CL <- plhdata_org_clean %>%
  dplyr::mutate(across(data_completion_level, ~ifelse(. == 100, "true", "false"))) %>%
  dplyr::select(app_user_id, data_completion_level)
colnames(plhdata_org_clean_CL) <- gsub("_completion_level", "_completed", colnames(plhdata_org_clean_CL))
data_completion_vars <- colnames(plhdata_org_clean_CL)[-1]
completion_total_vars <- c("Self Care Complete", "One-on-one Time Complete", "Praise Complete", "Positive Instructions Complete", "Managing Stress Complete", "Family Budgets Complete", "RulesComplete", "Calm Consequences Complete", "Problem Solving Complete", "Teen Safety Complete", "Dealing with Crisis Complete", "Celebration & Next Steps Complete")

plhdata_org_clean_CL <- full_join(plhdata_org_clean, plhdata_org_clean_CL, by = "app_user_id", suffix = c("", ".mod"))
plhdata_org_clean_CL <- plhdata_org_clean_CL %>%
  mutate(rp.contact.field.w_self_care_completed = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_self_care_completed.mod, rp.contact.field.w_self_care_completed),
                                                    rp.contact.field.w_1on1_completed = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_1on1_completed.mod, rp.contact.field.w_1on1_completed),
                                                    rp.contact.field.w_praise_completed = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_praise_completed.mod, rp.contact.field.w_praise_completed),
                                                    rp.contact.field.w_instruct_completed = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_instruct_completed.mod, rp.contact.field.w_instruct_completed),
                                                    rp.contact.field.w_stress_completed = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_stress_completed.mod, rp.contact.field.w_stress_completed),
                                                    rp.contact.field.w_money_completed = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_money_completed.mod, rp.contact.field.w_money_completed),
                                                    rp.contact.field.w_rules_completed = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_rules_completed.mod, rp.contact.field.w_rules_completed),
                                                    rp.contact.field.w_consequence_completed = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_consequence_completed.mod, rp.contact.field.w_consequence_completed),
                                                    rp.contact.field.w_solve_completed = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_solve_completed.mod, rp.contact.field.w_solve_completed),
                                                    rp.contact.field.w_safe_completed = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_safe_completed.mod, rp.contact.field.w_safe_completed),
                                                    rp.contact.field.w_crisis_completed = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_crisis_completed.mod, rp.contact.field.w_crisis_completed),
                                                    rp.contact.field.w_celebrate_completed = ifelse(rp.contact.field._app_skin == "modular", rp.contact.field.w_celebrate_completed.mod, rp.contact.field.w_celebrate_completed))

plhdata_org_clean_CL <- plhdata_org_clean_CL %>%
  dplyr::select(c(id, app_user_id, createdAt, opt_cluster,
                  experimental_condition, data_completion_level,
                  data_completion_vars, rp.contact.field._app_skin))
names(plhdata_org_clean_CL) <- c("ID", "App user ID", "Created at",
                                "Optimisation Cluster", "Experimental Condition",
                                completion_vars, completion_total_vars, "Skin_PA_var")



# Survey completion ------------------------------------------
plhdata_org_clean_survey <- plhdata_org_clean %>%
  dplyr::select(app_user_id, rp.contact.field.survey_welcome_completed, rp.contact.field.survey_final_completed)

names(plhdata_org_clean_survey) <- c("App user ID", "Welcome Survey Completion", "Final Survey Completion")


# Bang 'em together -------------------------------------------
plhdata_org_clean_all <- full_join(plhdata_org_clean_CL, plhdata_org_clean_survey, by = "App user ID")

plhdata_org_clean_all <- plhdata_org_clean_all %>%
  mutate(Support = ifelse(`Experimental Condition` < 5, "Self-guided", "WhatsApp"),
         Skin = ifelse(`Experimental Condition` %in% c(1, 2, 5, 6), "Module", "Workshop"),
         `Digital Literacy` = ifelse(`Experimental Condition` %in% c(1, 3, 5, 7), "On", "Off"))

plhdata_org_clean_all %>% group_by(Skin, Skin_PA_var) %>% dplyr::summarise(n())
plhdata_org_clean_mis <- plhdata_org_clean_all %>% 
  filter((Skin == "Module" & Skin_PA_var == "weekly_workshop") |
           (Skin == "Workshop" & Skin_PA_var == "modular"))


writexl::write_xlsx(plhdata_org_clean_all, path = "TZ_data_20230117.xlsx")



View(plhdata_org_clean_all)
