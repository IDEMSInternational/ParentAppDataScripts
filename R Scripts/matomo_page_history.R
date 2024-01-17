#
plhdata_org_clean$time_diff <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(plhdata_org_clean$updatedAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours")

plhdata_group_ids <- plhdata_org_clean %>% select(c('app_user_id', opt_cluster = "ClusterName", "createdAt", "time_diff", all_of(data_completion_level)))

plhdata_group_ids_group_1 <- threshhold_function(data = plhdata_group_ids, threshhold = 0)
plhdata_group_ids_group_1 <- plhdata_group_ids_group_1 %>%
  mutate(engagement_total = self_care_started + `1on1_started` + praise_started + 
           instruct_started + stress_started + money_started + rules_started + consequence_started + 
           solve_started + safe_started + crisis_started + celebrate_started)
#View(plhdata_group_ids_group_1)
plhdata_group_ids_group_1 <- plhdata_group_ids_group_1 %>%
  mutate(createdAt = as.Date(createdAt, "%y %m %d", tz = "utc"), # can calculate by UIC tracker
         curr_date = as.Date(Sys.Date(), "%y %m %d")) %>%
  mutate(diff_in_days = curr_date - createdAt) %>%
  mutate(hours_since_sync = time_diff) %>%
  mutate(week_number = floor(as.numeric(diff_in_days/7))) %>%
  mutate(week_number = ifelse(week_number > 12, 12, week_number)) %>%
  mutate(prop_complete = engagement_total / week_number) %>%
  mutate(engagement_level = ifelse(prop_complete == 0, "not engaged",
                                   ifelse(prop_complete <= 0.33, "low",
                                          ifelse(prop_complete <= 0.67, "moderate",
                                                 ifelse(prop_complete <= 1, "high",
                                                        "else"))))) %>%
  dplyr::select(-c(diff_in_days, curr_date, prop_complete))

plhdata_group_ids_group_1 <- plhdata_group_ids_group_1 %>% dplyr::select(-c(self_care_started, `1on1_started`, praise_started, 
                                                                            instruct_started, stress_started, money_started, rules_started, consequence_started, 
                                                                            solve_started, safe_started, crisis_started, celebrate_started))
names(plhdata_group_ids_group_1) <- naming_conventions(names(plhdata_group_ids_group_1), replace = "rp.contact.field.w_")
plhdata_group_ids_group_1 <- plhdata_group_ids_group_1 %>%
  dplyr::select(c("App user id", "Opt cluster", "Engagement total", "Week number", "Engagement level",
                  "CreatedAt", "Hours since sync", "1 Self care completion level" = "Self care completion level",
                  "2 1on1 completion level" = "1on1 completion level", "3 Praise completion level" = "Praise completion level",
                  "4 Instruct completion level" = "Instruct completion level", "5 Stress completion level" = "Stress completion level",
                  "6 Money completion level" = "Money completion level", "7 Rules completion level" = "Rules completion level",
                  "8 Consequence completion level" = "Consequence completion level", "9 Solve completion level" = "Solve completion level",
                  "10 Safe completion level" = "Safe completion level", "11 Crisis completion level" = "Crisis completion level", 
                  "12 Celebrate completion level" = "Celebrate completion level"))


View(plhdata_group_ids_group_1)


aa <- plhdata_group_ids_group_1 %>%
  filter(`1 Self care completion level` == 0)
aa$`App user id`


aa <- plhdata_group_ids_group_1 %>%
  filter(`Hours since sync` > 336)
View(aa)
#aa$`App user id`



# TODO:
uuid_interest <- c("e87fddcba08c3589", "b685052194626ce2", "d6eac9de408a762a", "4e84b3cd2ae20d2b", "9f5a0210d5e16493",
                   "730f1a47e748e7e2", "273b8219e1718cdb", "b81e6360184d1bd7", "0c0a27fee584e408", "a67e25dbec7b8553",
                   "ad70bef587bab9ec", "bcc2ff87e2cd893b", "0440ebdd456a5e30", "0f7e3fbedf1485cf", "41fc85f73c3644dd",
                   "787713c906881483", "5a380bc0ca2871f5", "1524a2a5631dd7ba", "37f94e4d3b69ec3b", "ecdaacf3f3164abc",
                   "1107f9e0853c8d61", "5b09e2cfe2f8df06", "9957b5aeb8af11dd")




rct_codes <- UIC_Tracker_RCT %>%
  dplyr::filter(Condition == "Intervention") %>%
  dplyr::pull(YourParentAppCode)
last_visits_id_date <- NULL
dates_from <- seq(from = as.Date("2023-05-01"), as.Date("2023-06-13"), by = 1) 
for (i in 38:length(dates_from)){
  j <- as.Date(dates_from[i]) 
  json_file <- paste0("https://apps-server.idems.international/analytics/index.php?date=", dates_from[i], ",", j, "&expanded=1&filter_limit=-1&force_api_session=1&format=JSON&idSite=1&method=Live.getLastVisitsDetails&module=API&period=range&token_auth=a30e42045e15641b75a74ecf210c26b6")
  json_data <- jsonlite::fromJSON(txt=json_file, flatten = TRUE)
  if (length(json_data) == 0) { 
    print(i)
  } else {
    json_data <- json_data %>% filter(userId %in% rct_codes)
    
    # for each row 
    last_visits_id <- NULL
    if (nrow(json_data) == 0) {
      last_visits_id_date[[i]] <- data.frame(`.id` = NA, url = NA, timeSpent = NA)
    } else {
      for (j in 1:nrow(json_data)){
        last_visits <- plyr::ldply(json_data[j,]$actionDetails) # get all the json_data
        last_visits$url <- sub("\\?.*", "", last_visits$url)
        last_visits_id[[j]] <- last_visits
      }
      names(last_visits_id) <- json_data$userId
      last_visits_id_date[[i]] <- plyr::ldply(last_visits_id)
    }
    print(i)
  }
}
#upto i = 21
# check after i = 21
#saveRDS( last_visits_id_date, "last_visits_id_date_to21.RDS")
#saveRDS( last_visits_id_date, "last_visits_id_date_to37.RDS")
#last_visits_id_date1 <- last_visits_id_date
#last_visits_id_date

last_visits_id_date <- plyr::ldply(last_visits_id_date)#, .id = "date_set")

last_visits_id_date <- last_visits_id_date %>% filter(`.id` %in% uuid_interest)

last_visits_id_date <- last_visits_id_date %>%
  dplyr::mutate(url = gsub(".*/", "", url))

id_0c0a27fee584e408 <- last_visits_id_date %>% filter(`.id` == "0c0a27fee584e408")
View(id_0c0a27fee584e408)

#ph_0c0a27fee584e408 <- plhdata_org_clean %>% filter(app_user_id == "0c0a27fee584e408")
# ph_0c0a27fee584e408$rp.contact.field.w_self_care_completion_level

ph_0c0a27fee584e408$rp.contact.field.task_gp_w_self_care_welcome_individual_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_self_care_relax_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_self_care_intro_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_self_care_tools_activity_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_self_care_ending_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_self_care_think_completed

#i in c("self_care", "1on1", "praise", "instruct", "stress", "money", "rules", "consequence", "solve", "safe", "crisis", "celebrate")){
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_1on1_welcome_individual_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_1on1_relax_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_1on1_intro_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_1on1_tools_activity_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_1on1_ending_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_1on1_think_completed

ph_0c0a27fee584e408$rp.contact.field.task_gp_w_celebrate_welcome_individual_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_celebrate_relax_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_celebrate_intro_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_celebrate_tools_activity_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_celebrate_ending_completed
ph_0c0a27fee584e408$rp.contact.field.task_gp_w_celebrate_think_completed


#dat_s <- (plhdata_org %>% filter(app_user_id == "0c0a27fee584e408"))
#dat_s$`rp-contact-field.task_gp_w_celebrate_relax_completed`

###### TODO: run below for visitor ID: 6bcf8d746ce5909b
###### do we have all the details we expect?
for (i in 1:length(dates_from)){
  j <- as.Date(dates_from[i]) 
  json_file <- paste0("https://apps-server.idems.international/analytics/index.php?date=", i, ",", j, "&module=API$action=index&visitorId=6bcf8d746ce5909b&idSite=1&period=range&expanded=1&method=Live.getVisitorProfile&filter_limit=-1&format=JSON&idSite=1&module=API&period=range&token_auth=a30e42045e15641b75a74ecf210c26b6")
  json_data <- jsonlite::fromJSON(txt=json_file, flatten = TRUE)
  if (length(json_data) == 0) { 
    print(i)
  } else {
    json_data <- json_data %>% filter(userId %in% rct_codes)
    
    # for each row 
    last_visits_id <- NULL
    if (nrow(json_data) == 0) {
      last_visits_id_date[[i]] <- data.frame(`.id` = NA, url = NA, timeSpent = NA)
    } else {
      for (j in 1:nrow(json_data)){
        last_visits <- plyr::ldply(json_data[j,]$actionDetails) # get all the json_data
        last_visits$url <- sub("\\?.*", "", last_visits$url)
        last_visits_id[[j]] <- last_visits
      }
      names(last_visits_id) <- json_data$userId
      last_visits_id_date[[i]] <- plyr::ldply(last_visits_id)
    }
    print(i)
  }
}


json_data <- jsonlite::fromJSON(txt=json_file, flatten = TRUE)

View(json_data$lastVisits)


json_file <- paste0("https://apps-server.idems.international/analytics/index.php?date=", "2023-04-01", ",", "2023-06-13", "&module=API$action=index&visitorId=6bcf8d746ce5909b&idSite=1&period=range&expanded=1&method=Live.getVisitorProfile&filter_limit=-1&format=JSON&idSite=1&module=API&period=range&token_auth=a30e42045e15641b75a74ecf210c26b6")


# TODO: we should have that they've visited loads of websites - like, all of them. the 5th workshop and stuff
# do we have that?
# if yes, why is it not in json_data where I download all (perhaps because I get "last visit" and I do over 3 days or so)


plhdata_org_clean %>% filter(app_user_id == "0c0a27fee584e408") %>%
  dplyr::select(createdAt, updatedAt, time_diff)

json_data$lastVisit


