##################################
##################################
####### User Data Analysis #######
##################################
##################################

country <- "Tanzania"
study <- "RCT"

### Set up UIC data

# add in start dates for clusters
# create a data frame with the lookup values
lookup_df <- data.frame(opt_cluster = c(1, 3, 4, 6, 7, 11, 13, 14),
                        whatsapp_start_date = as.Date(c("2022-11-27", "2022-12-17", "2023-01-10",
                                                        "2022-11-24", "2022-11-26", "2023-01-10",
                                                        "2022-12-24", "2022-12-24")))

# use left_join to add the lookup values to the main data frame
UIC_Tracker_Tanzania <- UIC_Tracker_Tanzania %>%
  left_join(lookup_df, by = "opt_cluster")


### extract data ----------------------------------------------------------------------
# to get user data

if (study %in% c("Optimisation", "Pilot")){
  UIC_Tracker_Use = UIC_Tracker_Tanzania
} else {
  UIC_Tracker_Use = UIC_Tracker_RCT
}

plhdata_org <- get_user_data(site = plh_con, merge_check = FALSE, filter = TRUE,
                              UIC_Tracker = UIC_Tracker_Use,
                                     country = country, study = study)
names(plhdata_org) <- gsub(x = names(plhdata_org), pattern = "\\-", replacement = ".")  

#plhdata_org <- readRDS("plhdata_org_RCT_20230525.RDS")

if (nrow(plhdata_org) == 0){
  plhdata_org <- get_user_data(site = plh_con, merge_check = FALSE, filter = TRUE, UIC_Tracker = UIC_Tracker_Tanzania,
                               country = country, study = "Pilot")
  names(plhdata_org) <- gsub(x = names(plhdata_org), pattern = "\\-", replacement = ".")
  plhdata_org <- plhdata_org %>% mutate(across(everything(), as.numeric))
  plhdata_org <- naniar::replace_with_na_all(plhdata_org, ~.x)
  plhdata_org <- plhdata_org[0,]
}

# Create Optimisation Group Data for Optimisation Study - Tanzania
if (study == "Optimisation"){
  valid_ids <- UIC_Tracker_Tanzania %>%
    filter(complete.cases(YourParentAppCode))  %>%
    filter(Study == study) %>%
    select(c(YourParentAppCode, opt_cluster, experimental_condition, OnboardingDate))
  
  plhdata_org <- full_join(plhdata_org, valid_ids, by = c("app_user_id" = "YourParentAppCode"))
  plhdata_org <- plhdata_org %>%
    mutate(Cluster = opt_cluster,
           Support = ifelse(experimental_condition < 5, "Self-guided", "WhatsApp"),
           Skin = ifelse(experimental_condition %in% c(1, 2, 5, 6), "Module", "Workshop"),
           `Digital Literacy` = ifelse(experimental_condition %in% c(1, 3, 5, 7), "On", "Off"))
} else if (study == "Pilot") {
  valid_ids <- UIC_Tracker_Tanzania %>%
    filter(complete.cases(YourParentAppCode))  %>%
    filter(Study == "Pilot") %>%
    select(c(YourParentAppCode, PilotSite))
  plhdata_org <- fuzzyjoin::stringdist_full_join(x = plhdata_org, y = valid_ids, by = c("app_user_id" = "YourParentAppCode"), max_dist = 5)
  
  #plhdata_org_ics_fuzzy <- fuzzyjoin::stringdist_full_join(x = plhdata_org, y = valid_ids, by = c("app_user_id" = "YourParentAppCode"), max_dist = 5)
  # valid_app_user_id_TZ <- (plhdata_org_ics_fuzzy %>% filter(organisation_full == "ICS") %>% filter(!is.na(YourParentAppCode)))$app_user_id
  # plhdata_org <- plhdata_org %>% 
  #   mutate(valid_ics = ifelse(organisation_full != "ICS", TRUE,
  #                             ifelse(app_user_id %in% valid_app_user_id_TZ, TRUE, FALSE))) %>%
  #   filter(valid_ics)
  # plhdata_org <- plhdata_org %>%
  #   mutate(organisation_full = ifelse(app_user_id %in% c("2c5bfeb1c97cffdf", "0e5824bd19aae8c4",
  #                                                        "48621962b0612b7c", "d5faa072c966ea8d",
  #                                                        "df1088af5f3d4c87", "5b2ba92c32c6a3e2",
  #                                                        "f3aff268263b1d62", "a05a0fe6cd3cb52d",
  #                                                        "7f56c4c0a8a2f36f", "fab4ae58ac03f920"),
  #                                     "ICS",
  #                                     as.character(organisation_full)))
  
  # Create Pilot Group Data for Pilot Study - Tanzania
  
  # add in new row containing ICS, and app_user_id  -  08/09/22
  #fab4ne58ac03f920
  #oe5824bd19aa8c4
  #plhdata_org[(nrow(plhdata_org)+1):(nrow(plhdata_org)+2),] <- NA
  #plhdata_org$app_user_id[(last(nrow(plhdata_org))-1):last(nrow(plhdata_org))] <- c("fab4ne58ac03f920", "oe5824bd19aa8c4")
  #plhdata_org$organisation_full[(last(nrow(plhdata_org))-1):last(nrow(plhdata_org))] <- c("ICS", "ICS")
  #plhdata_org$app_version[(last(nrow(plhdata_org))-1):last(nrow(plhdata_org))] <- c("0.0", "0.0")
  
  # get unique cases only
  #View(plhdata_org_ics_fuzzy %>% filter(app_user_id == "a60b902a430aaec2"))
  # plhdata_org_ics_fuzzy <- unique(plhdata_org_ics_fuzzy %>% dplyr::select(-c("YourParentAppCode")))
  # plhdata_org_pilot_site <- plhdata_org_ics_fuzzy %>% dplyr::select(c(app_user_id, PilotSite)) %>% filter(!is.na(PilotSite))
  # nrow((plhdata_org_pilot_site))
  # plhdata_org <- full_join(plhdata_org, plhdata_org_pilot_site, by = c("app_user_id" = "app_user_id"))
} else if (study == "RCT") {
  valid_ids <- UIC_Tracker_Use %>%
    filter(complete.cases(YourParentAppCode))  %>%
    filter(Study == "RCT") %>%
    select(c(YourParentAppCode, Ward, ClusterName, OnboardingDateShort))
  plhdata_org <- fuzzyjoin::stringdist_full_join(x = plhdata_org, y = valid_ids, by = c("app_user_id" = "YourParentAppCode"), max_dist = 5)
}

# For SA:
if (country == "South Africa"){
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
  #plhdata_org_ics_fuzzy %>% filter(!is.na(YourParentAppCode)) %>% dplyr::select(organisation_full, app_user_id, YourParentAppCode)
  # Note: "2c5bfeb1c97cffdf" "oe5824bd19aa8c4" are in "Miss.Miss"  
}

#####Create a subset for cleaned organisations ####
plhdata_org_clean <- plhdata_org # %>% filter(Org != "Other")%>% mutate(Org = factor(Org))

# RCT TODO HERE
# plhdata_org_clean <- plhdata_org_clean %>%
#   dplyr::mutate(rp.contact.field.user_age = as.numeric(rp.contact.field.user_age)) %>%
#   dplyr::mutate(rp.contact.field.user_age = replace(rp.contact.field.user_age,
#                                                     rp.contact.field.user_age %in% c(-29, 2, 1794, 5655),
#                                                     NA)) %>%
#   dplyr::mutate(rp.contact.field.user_age = ifelse(rp.contact.field.user_age > 1960,
#                                                    2023 - rp.contact.field.user_age, #todo: fix more pernamently
#                                                    ifelse(rp.contact.field.user_age < 0, NA, rp.contact.field.user_age)))

# Create subsets of the data based on valid app user ID's
#plhdata_org_clean <- plhdata_org_clean %>% dplyr::filter(!is.na(app_version))

# add in country variable
plhdata_org_clean <- plhdata_org_clean %>% mutate(country = country)


if (country == "Tanzania"){
  if (study == "Optimisation"){
    plhdata_org_clean <- plhdata_org_clean #%>% filter(Org == "Optimisation Study")
  } else if (study == "Pilot") {
    plhdata_org_clean <- plhdata_org_clean %>% mutate(PilotSite = replace_na(PilotSite, "Unknown"))
#    plhdata_org_clean <- plhdata_org_clean %>% filter(Org == "ICS")
  } else {
    plhdata_org_clean <- plhdata_org_clean %>% mutate(ClusterName = replace_na(ClusterName, "Unknown"))
  }
} else if (country == "South Africa"){
  plhdata_org_clean <- plhdata_org_clean %>% filter(Org %in% c("Amathuba", "Joy", "Dlalanathi", "Nontobeko"))
}

# Sorting Name Changes --------------------------------------------------

# v_all <- c()
# if (length(grep("0.16.2", unique(plhdata_org_clean$app_version))) !=0) { v_all <- c(v_all, "0.16.2")}
# if (length(grep("0.16.3", unique(plhdata_org_clean$app_version))) !=0) { v_all <- c(v_all, "0.16.3")}
# if (length(grep("0.16.4", unique(plhdata_org_clean$app_version))) !=0) { v_all <- c(v_all, "0.16.4")}
# if (length(v_all) > 0){
#   old_names <- c("a_1_final", "a_2_final", "a_3_final", "a_4_final", "a_5_part_1_final", "a_5_part_2_final", "a_6_final", "a_7_part_1_final")
#   new_names <- c("ppf", "ppp", "ps", "cme", "fs", "fi", "cmp", "cs")
#   df_names <- data.frame(old_names, new_names)
#   for (v in v_all){
#     for (i in 1:nrow(df_names)){
#       old_name = df_names[i,1]
#       new_name = df_names[i,2]
#       plhdata_org_clean <- plhdata_org_clean %>%
#         map_df(.x = v, #c("v0.16.2", "v0.16.3", "v0.16.4"),
#                .f = ~version_variables_rename(old_name = old_name, new_name = new_name, new_name_v = .x, old_system_replacement = TRUE))
#       # todo: doesn't work for v?? Should explore that. But for now, in this extra loop
#     }
#   }
#   
#   for (v in c("v0.16.4")){ # and other versions?
#     for (i in 1:nrow(df_names)){
#       old_name = df_names[i,1]
#       new_name = df_names[i,2]
#       plhdata_org_clean <- plhdata_org_clean %>%
#         map_df(.x = v, #c("v0.16.2", "v0.16.3", "v0.16.4"),
#                .f = ~version_variables_rename(old_name = old_name, new_name = new_name, new_name_v = .x, old_system_replacement = TRUE, survey = "final"))
#       # todo: doesn't work for v?? Should explore that. But for now, in this extra loop
#     }
#   }
# }


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
# json_data <- NULL
# for (i in c("self_care", "1on1", "praise", "instruct", "stress", "money", "rules", "consequence", "solve", "safe", "crisis", "celebrate")){
#  # which variables to select?
#  json_data[[i]] <- data.frame(jsonlite::fromJSON(paste0("~/GitHub/parenting-app-ui/packages/app-data/sheets/data_list/generated/w_", i, "_task_gs.json")))
# }
# saveRDS(json_data, file = "data/json_data.RDS")
json_data <- readRDS(file = "data/json_data.RDS")

if (study %in% c("Optimisation", "RCT")){
  plhdata_org_clean_mod <- plhdata_org_clean %>% filter(rp.contact.field._app_skin %in% c("modular", "default"))
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
    if (i == "self_care" && study == "RCT") {
      json_data_i$rows.individual[which(json_data_i$rows.completed_field %in% c("task_gp_w_self_care_welcome_individual_completed", "task_gp_w_self_care_survey_completed"))] <- FALSE
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
# RCT TODO here

if (study != "RCT"){
  plhdata_org_clean$rp.contact.field.survey_welcome_and_setup_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.survey_welcome_and_setup_completion_level)
  plhdata_org_clean$rp.contact.field.user_age <- as.numeric(plhdata_org_clean$rp.contact.field.user_age)
  plhdata_org_clean$rp.contact.field.household_adults <- as.numeric(plhdata_org_clean$rp.contact.field.household_adults)
  plhdata_org_clean$rp.contact.field.household_teens <- as.numeric(plhdata_org_clean$rp.contact.field.household_teens)
  plhdata_org_clean$rp.contact.field.household_babies <- as.numeric(plhdata_org_clean$rp.contact.field.household_babies)
  plhdata_org_clean$rp.contact.field.household_children <- as.numeric(plhdata_org_clean$rp.contact.field.household_children)
  plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed)
  plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_self_care_diff_started_completed)
}

plhdata_org_clean$rp.contact.field.parent_point_count_relax_w_self_care <- as.numeric(plhdata_org_clean$rp.contact.field.parent_point_count_relax_w_self_care)

plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(ends_with("_completion_level"), ~as.numeric(.)))
plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(starts_with("rp.contact.field.parent_point"), ~as.numeric(.)))
plhdata_org_clean$rp.contact.field.app_launch_count <- as.numeric(plhdata_org_clean$rp.contact.field.app_launch_count)


# plhdata_org_clean$rp.contact.field.first_app_open <- as.Date(plhdata_org_clean$rp.contact.field.first_app_open)
plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(starts_with("rp.contact.field.app_launch_count"), ~as.numeric(.)))


## Data Analysis ## --------------------------------------------------------

# workshop_path edits ----------
plhdata_org_clean <- plhdata_org_clean %>%
  mutate(rp.contact.field.workshop_path = ifelse(is.na(rp.contact.field.workshop_path_user_choice),
                                                 rp.contact.field.workshop_path,
                                                 ifelse(rp.contact.field.workshop_path_user_choice == "false",
                                                        "default",
                                                        rp.contact.field.workshop_path)))

# Variables Set up ---------------------------------------
#completion_vars <- c("Self Care", "One-on-one Time", "Praise", "Positive Instructions", "Managing Stress", "Family Budgets", "Rules", "Calm Consequences", "Problem Solving", "Teen Safety", "Dealing with Crisis","Celebration & Next Steps")
# TODO: add summary plot completion level in
#summary_plot_completion_level <- multiple_plot_output(columns_to_summarise = data_completion_level,
#                                                      replace = "rp.contact.field.w_", replace_after = "_completion_level")
#names(summary_plot_completion_level) <- completion_vars
#summary_plot_completion_level$`One-on-one Time`

# TAB 3 - PP -------------------------
# HABITS by workshop week ------------------------------------------------------------------------------------



##Priority 20
#App-opens
data_app_opens <- c("rp.contact.field.app_launch_count","rp.contact.field.app_launch_count_w_1on1", "rp.contact.field.app_launch_count_w_self_care",
                    "rp.contact.field.app_launch_count_w_praise","rp.contact.field.app_launch_count_w_instruct",
                    "rp.contact.field.app_launch_count_w_stress", "rp.contact.field.app_launch_count_w_money",
                    "rp.contact.field.app_launch_count_w_rules", "rp.contact.field.app_launch_count_w_consequence",
                    "rp.contact.field.app_launch_count_w_solve", "rp.contact.field.app_launch_count_w_safe",
                    "rp.contact.field.app_launch_count_w_crisis", "rp.contact.field.app_launch_count_w_celebrate")

data_app_opens_neat <- c("Overall", "1on1 (2)", "Self Care (1)", "Praise (3)", "Positive Instructions(4)",
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

## Home Practice ------------------------------------------------------------------
data_hp_started <- c("rp.contact.field.w_1on1_hp_review_started",  "rp.contact.field.w_praise_hp_review_started",
                     "rp.contact.field.w_instruct_hp_review_started",  "rp.contact.field.w_stress_hp_review_started",
                     "rp.contact.field.w_money_hp_review_started",  "rp.contact.field.w_rules_hp_review_started",
                     "rp.contact.field.w_consequence_hp_review_started",  "rp.contact.field.w_solve_hp_review_started",  "rp.contact.field.w_safe_hp_review_started",
                     "rp.contact.field.w_crisis_hp_review_started")

# RCT TODO
# plhdata_org_clean <- plhdata_org_clean %>%
#   dplyr::mutate(rp.contact.field.w_stress_hp_done = ifelse(rp.contact.field.w_stress_hp_talk_done == "yes" & 
#                                                              rp.contact.field.w_stress_hp_breathe_done == "yes",
#                                                            "yes",
#                                                            "no"))

# data_hp_done <- c("rp.contact.field.w_1on1_hp_done", "rp.contact.field.w_praise_hp_done", "rp.contact.field.w_instruct_hp_done", "rp.contact.field.w_stress_hp_talk_done",
#                   "rp.contact.field.w_stress_hp_breathe_done", "rp.contact.field.w_money_hp_done", "rp.contact.field.w_rules_hp_done", "rp.contact.field.w_consequence_hp_done",
#                   "rp.contact.field.w_solve_hp_done", "rp.contact.field.w_safe_hp_done", "rp.contact.field.w_crisis_hp_done")

# NB No mood 'review' for week 3 home practice (praise)
data_hp_mood <- c("rp.contact.field.w_1on1_hp_mood", "rp.contact.field.w_instruct_hp_mood", "rp.contact.field.w_stress_hp_breathe_mood", "rp.contact.field.w_stress_hp_talk_mood",
                  "rp.contact.field.w_money_hp_mood", "rp.contact.field.w_rules_hp_mood", "rp.contact.field.w_consequence_hp_mood",
                  "rp.contact.field.w_solve_hp_mood", "rp.contact.field.w_safe_hp_mood", "rp.contact.field.w_crisis_hp_mood") 

# TODO: this should work in function
# plhdata_org_clean <- add_na_variable(variable = data_hp_started)
# plhdata_org_clean <- add_na_variable(variable = data_hp_done)
# plhdata_org_clean <- add_na_variable(variable = data_hp_mood)

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

# parent library ------------------------------------------------------------------
data_library <- c("rp.contact.field.click_hs_parent_centre_count", "rp.contact.field.click_pc_help_count",
                  "rp.contact.field.click_pc_my_tips_count", "rp.contact.field.click_pc_essential_tools_count",
                  "rp.contact.field.click_pc_covid_count", "rp.contact.field.click_pc_customisation_count",
                  "rp.contact.field.click_pc_relax_and_activities_count", "rp.contact.field.click_pc_support_contacts_count",
                  "rp.contact.field.click_pc_evidence_base_count", "rp.contact.field.click_pc_technical_support_count",
                  "rp.contact.field.click_pc_message_archive_count","rp.contact.field.click_pc_bereavement_count")

# Down to data_library

# Completion status of baseline survey ------------------------------------------------
# plhdata_org_clean %>%
#   split(.$Org) %>%
#   map(~summary_table(data = .x, factor = NULL, columns_to_summarise = rp.contact.field.survey_welcome_complppplheted, replace = "rp.contact.field.survey"))

##################################
##################################
### Notification Data Analysis ###
##################################
##################################

# download push notification data
  nf_data <- get_nf_data(site = plh_con, UIC_Tracker = UIC_Tracker_Use, filter = TRUE,
                         study = study, country = country, app_user_id = "app_user_id")

