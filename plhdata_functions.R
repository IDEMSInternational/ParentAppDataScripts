
# Creating a subset for an organisation------------- 
Organization_Table<-function(data,Org_name){
  plhdata_org_Filter <- filter(data, Org == {{Org_name}})
  
  # Show any app user ids that are invalid and do not come from the app.
  plhdata_org_Filter %>% filter(is.na(plhdata_org_Filter$'app_version')) %>% select('app_user_id')
  
  # Show any app user ids that are invalid and do not come from the app.
  plhdata_org_Filter %>% select('app_user_id', 'Org', 'app_version')
  
  # Create subsets of the data based on valid DLALANATHI app user ID's
  plhdata_org_Filter <- filter(plhdata_org_Filter, !is.na(plhdata_org_Filter$'app_version'))
  
  # Show the summary of app versions
  sjmisc::frq(x=plhdata_org_Filter$'app_version', out="txt")
  
}
Organization_Table(data=plhdata_org_clean, Org_name = "Joy")

head(plhdata_org$Organisation)



# Sampled UIC's for Qual Interviews: Completion levels of workshops(Plhorg data)-----------------------
# do it for clean data, then you dont have to do it each time for the diffeernt organisation data
plhdata_org$rp.contact.field.w_self_care_completion_level <- as.numeric(as.character(plhdata_org$rp.contact.field.w_self_care_completion_level))
plhdata_org$rp.contact.field.w_1on1_completion_level <- as.numeric(as.character(plhdata_org$rp.contact.field.w_1on1_completion_level))
plhdata_org$rp.contact.field.w_praise_completion_level <- as.numeric(as.character(plhdata_org$rp.contact.field.w_praise_completion_level))
plhdata_org$rp.contact.field.w_instruct_completion_level <- as.numeric(as.character(plhdata_org$rp.contact.field.w_instruct_completion_level))
plhdata_org$rp.contact.field.w_stress_completion_level <- as.numeric(as.character(plhdata_org$rp.contact.field.w_stress_completion_level))
plhdata_org$rp.contact.field.w_money_completion_level <- as.numeric(as.character(plhdata_org$rp.contact.field.w_money_completion_level))
plhdata_org$rp.contact.field.w_rules_completion_level <- as.numeric(as.character(plhdata_org$rp.contact.field.w_rules_completion_level))



#All groups
plhdata_group_ids <- plhdata_org %>%  select('app_user_id','Organisation', "rp.contact.field._app_language", "rp.contact.field.w_self_care_completion_level", 
                                             "rp.contact.field.w_1on1_completion_level", "rp.contact.field.w_praise_completion_level",
                                             "rp.contact.field.w_instruct_completion_level", "rp.contact.field.w_stress_completion_level" ,
                                             "rp.contact.field.w_money_completion_level", "rp.contact.field.w_rules_completion_level") %>%
  mutate(group_4 = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50 &
                            rp.contact.field.w_1on1_completion_level> 50 & rp.contact.field.w_stress_completion_level>50 &
                            rp.contact.field.w_instruct_completion_level>50 & rp.contact.field.w_money_completion_level>50 & 
                            rp.contact.field.w_rules_completion_level>50, 
                          1,
                          0)) %>%  
  mutate(group_3 = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50 &
                            rp.contact.field.w_1on1_completion_level> 50 & rp.contact.field.w_stress_completion_level>50 &
                            rp.contact.field.w_instruct_completion_level>50,
                          1,
                          0)) %>%
  mutate(group_2bplus = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50  &
                                 rp.contact.field.w_1on1_completion_level> 50,
                               1,
                               0)) %>%
  mutate(group_2b = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50  &
                             rp.contact.field.w_1on1_completion_level> 50 & rp.contact.field.w_stress_completion_level< 1 &
                             rp.contact.field.w_instruct_completion_level < 1,
                           1,
                           0)) %>%
  mutate(group_2aplus = ifelse(rp.contact.field.w_self_care_completion_level > 50,
                               1,
                               0)) %>%
  mutate(group_2a = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level < 1  &
                             rp.contact.field.w_1on1_completion_level< 1 & rp.contact.field.w_stress_completion_level< 1 &
                             rp.contact.field.w_instruct_completion_level < 1,
                           1,
                           0)) %>% rowwise() %>% 
  mutate(group_2c = ifelse(mean(c(rp.contact.field.w_self_care_completion_level, rp.contact.field.w_praise_completion_level,
                                  rp.contact.field.w_1on1_completion_level, rp.contact.field.w_stress_completion_level,
                                  rp.contact.field.w_instruct_completion_level))> 50,
                           1,
                           0)) %>% 
  
  filter(Organisation != 'Miss')

View(plhdata_group_ids)
plhdata_group_ids %>% filter(group_4 == 1) %>%  select('app_user_id',  'Organisation', 'rp.contact.field._app_language', 'group_4' ,'group_3', 'group_2c', 'group_2b', 'group_2a')
plhdata_group_ids %>% filter(group_3 == 1) %>%  select('app_user_id',  'Organisation' ,'rp.contact.field._app_language','group_3', 'group_2c', 'group_2b', 'group_2a')
plhdata_group_ids %>% filter(group_2c == 1) %>%  select('app_user_id',  'Organisation' ,'rp.contact.field._app_language','group_3', 'group_2c', 'group_2b', 'group_2a')
plhdata_group_ids %>% filter(group_2b == 1) %>%  select('app_user_id',  'Organisation','rp.contact.field._app_language','group_3', 'group_2c', 'group_2b', 'group_2a')
plhdata_group_ids %>% filter(group_2a == 1) %>%  select('app_user_id',  'Organisation','rp.contact.field._app_language','group_3', 'group_2c', 'group_2b', 'group_2a')
plhdata_group_ids %>% filter(group_2aplus == 1) %>%  select('app_user_id',  'Organisation','rp.contact.field._app_language','group_3', 'group_2c', 'group_2b', 'group_2a')



# Sampled UIC's for Qual Interviews: Completion levels of workshops(Plhdata_clean)-----------------------
# do it for clean data, then you dont have to do it each time for the different organisation data
plhdata_org_clean$rp.contact.field.w_self_care_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_self_care_completion_level))
plhdata_org_clean$rp.contact.field.w_1on1_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_1on1_completion_level))
plhdata_org_clean$rp.contact.field.w_praise_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_praise_completion_level))
plhdata_org_clean$rp.contact.field.w_instruct_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_instruct_completion_level))
plhdata_org_clean$rp.contact.field.w_stress_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_stress_completion_level))
plhdata_org_clean$rp.contact.field.w_money_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_money_completion_level))
plhdata_org_clean$rp.contact.field.w_rules_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_rules_completion_level))



#All groups
plhdata_group_ids <- plhdata_org_clean %>%  select('app_user_id','Org', "rp.contact.field._app_language", "rp.contact.field.w_self_care_completion_level", 
                                                   "rp.contact.field.w_1on1_completion_level", "rp.contact.field.w_praise_completion_level",
                                                   "rp.contact.field.w_instruct_completion_level", "rp.contact.field.w_stress_completion_level" ,
                                                   "rp.contact.field.w_money_completion_level", "rp.contact.field.w_rules_completion_level") %>%
  mutate(group_4 = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50 &
                            rp.contact.field.w_1on1_completion_level> 50 & rp.contact.field.w_stress_completion_level>50 &
                            rp.contact.field.w_instruct_completion_level>50 & rp.contact.field.w_money_completion_level>50 & 
                            rp.contact.field.w_rules_completion_level>50, 
                          1,
                          0)) %>%  
  mutate(group_3 = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50 &
                            rp.contact.field.w_1on1_completion_level> 50 & rp.contact.field.w_stress_completion_level>50 &
                            rp.contact.field.w_instruct_completion_level>50,
                          1,
                          0)) %>%
  mutate(group_2bplus = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50  &
                                 rp.contact.field.w_1on1_completion_level> 50,
                               1,
                               0)) %>%
  mutate(group_2b = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50  &
                             rp.contact.field.w_1on1_completion_level> 50 & rp.contact.field.w_stress_completion_level< 1 &
                             rp.contact.field.w_instruct_completion_level < 1,
                           1,
                           0)) %>%
  mutate(group_2aplus = ifelse(rp.contact.field.w_self_care_completion_level > 50,
                               1,
                               0)) %>%
  mutate(group_2a = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level < 1  &
                             rp.contact.field.w_1on1_completion_level< 1 & rp.contact.field.w_stress_completion_level< 1 &
                             rp.contact.field.w_instruct_completion_level < 1,
                           1,
                           0)) %>% rowwise() %>% 
  mutate(group_2c = ifelse(mean(c(rp.contact.field.w_self_care_completion_level, rp.contact.field.w_praise_completion_level,
                                  rp.contact.field.w_1on1_completion_level, rp.contact.field.w_stress_completion_level,
                                  rp.contact.field.w_instruct_completion_level))> 50,
                           1,
                           0)) %>% 
  
  filter(Org != 'Miss')

View(plhdata_group_ids)
plhdata_group_ids %>% filter(group_4 == 1) %>%  select('app_user_id', 'Org', 'rp.contact.field._app_language', 'group_4' ,'group_3', 'group_2c', 'group_2b', 'group_2a')
plhdata_group_ids %>% filter(group_3 == 1) %>%  select('app_user_id',  'Org' ,'rp.contact.field._app_language','group_3', 'group_2c', 'group_2b', 'group_2a')
plhdata_group_ids %>% filter(group_2c == 1) %>%  select('app_user_id','Org' ,'rp.contact.field._app_language','group_3', 'group_2c', 'group_2b', 'group_2a')
plhdata_group_ids %>% filter(group_2b == 1) %>%  select('app_user_id','Org','rp.contact.field._app_language','group_3', 'group_2c', 'group_2b', 'group_2a')
plhdata_group_ids %>% filter(group_2a == 1) %>%  select('app_user_id','Org','rp.contact.field._app_language','group_3', 'group_2c', 'group_2b', 'group_2a')
plhdata_group_ids %>% filter(group_2aplus == 1) %>%  select('app_user_id','Org','rp.contact.field._app_language','group_3', 'group_2c', 'group_2b', 'group_2a')



#Summary tables
#Workshop started & completed analysis
#Show the summary of  baseline survey completion
#Show the summary of user gender

Workshop_started_tables<-function(data=plhdata_org_clean, grouping_names){
plhdata_org_Filter<-dplyr::filter(data, Org != "Miss")

#userid_data<-plhdata_org_Filter %>% dplyr::select("app_user_id", across({{grouping_names}}))

summary_table <- plhdata_org_Filter%>% dplyr::group_by(across({{grouping_names}})) %>% dplyr::summarise(n()) 
pivot_wider(summary_table, names_from = grouping_names[2], values_from = `n()`)
}

Workshop_started_tables(grouping_names = c("Org","rp.contact.field.w_self_care_started"))

#Show the summary of user age
#show the summary  household adults
#show the summary of household teens
#show the summary of household babies
#show the summary of household children

#convert variable  to numeric

plhdata_org_clean$rp.contact.field.user_age <- as.numeric(plhdata_org_clean$rp.contact.field.user_age)

User_age_tables<-function(data=plhdata_org_clean, grouping_names){
  plhdata_org_Filter<-dplyr::filter(data, Org != "Miss")
  
  #userid_data<-plhdata_org_Filter %>% dplyr::select("app_user_id", across({{grouping_names}}))
  
  summary_table <- plhdata_org_Filter%>% dplyr::group_by(across({{grouping_names}})) %>% dplyr::summarise(n()) 
  pivot_wider(summary_table, names_from = grouping_names[2], values_from = `n()`)
}

User_age_tables(grouping_names = c("Org","rp.contact.field.user_age"))


#show the summary of user languages
#Parent Points(Show summary  for each Parent Point, all time number of clicks - for each user)
#Show the summary for each parent point and each week, number of clicks in that week for each user.
user_id_print <- function(field) {
  for (o in orgs_list) {
    # print organisation first
    print(o)
    # print filtered data
    print(
      plhdata_org_clean %>%
        filter(Org == o) %>%
        select('app_user_id', field)
    )
  }
}

user_id_print("rp.contact.field._app_language")
user_id_print("rp.contact.field.parent_point_count_relax")

User_language_tables<-function(data=plhdata_org_clean, grouping_names){
  plhdata_org_Filter<-dplyr::filter(data, Org != "Miss")
  
  #userid_data<-plhdata_org_Filter %>% dplyr::select("app_user_id", across({{grouping_names}}))
  
  summary_table <- plhdata_org_Filter%>% dplyr::group_by(across({{grouping_names}})) %>% dplyr::summarise(n())
  pivot_wider(summary_table, names_from = grouping_names[2], values_from = `n()`) 
}

User_language_tables(grouping_names = c("Org","rp.contact.field._app_language"))



#Single user information on variables of interest for the SA Qual Interviews----------------------
#To do:create a function
User_key_values<-function(data=plhdata_org_clean, user_id){ 
  plhdata_org_clean<-data

  plhdata_org_clean$rp.contact.field.w_self_care_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_self_care_completion_level))
  plhdata_org_clean$rp.contact.field.w_1on1_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_1on1_completion_level))
  plhdata_org_clean$rp.contact.field.w_praise_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_praise_completion_level))
  plhdata_org_clean$rp.contact.field.w_instruct_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_instruct_completion_level))
  plhdata_org_clean$rp.contact.field.w_stress_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_stress_completion_level))
  plhdata_org_clean$rp.contact.field.w_money_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_money_completion_level))
  plhdata_org_clean$rp.contact.field.w_rules_completion_level <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.w_rules_completion_level))


  plhdata_org_clean$rp.contact.field.parent_point_count_safe <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.parent_point_count_safe))
  plhdata_org_clean$rp.contact.field.parent_point_count_relax <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.parent_point_count_relax))
  plhdata_org_clean$rp.contact.field.parent_point_count_treat_yourself <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.parent_point_count_treat_yourself))
  plhdata_org_clean$rp.contact.field.parent_point_count_praise_yourself <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.parent_point_count_praise_yourself))
  plhdata_org_clean$rp.contact.field.parent_point_count_spend_time <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.parent_point_count_spend_time))
  plhdata_org_clean$rp.contact.field.parent_point_count_instruct_positively <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.parent_point_count_instruct_positively))
  plhdata_org_clean$rp.contact.field.parent_point_count_breathe <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.parent_point_count_breathe))
  plhdata_org_clean$rp.contact.field.parent_point_count_money <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.parent_point_count_money))
  plhdata_org_clean$rp.contact.field.parent_point_count_consequence <- as.numeric(as.character(plhdata_org_clean$rp.contact.field.parent_point_count_safe))


  #All groups
  plhdata_group_ids <- plhdata_org_clean %>%  select('app_user_id','Org','app_version','rp.contact.field.app_launch_count',"rp.contact.field._app_language","createdAt",'rp.contact.field._server_sync_latest', 'rp.contact.field.user_age','rp.contact.field.household_children','rp.contact.field.survey_welcome_completed','rp.contact.field.do_workshops_together','rp.contact.field.click_hs_weekly_workshops_quick_start_count', 'rp.contact.field.click_hs_parent_centre_quick_start_count','rp.contact.field.click_hs_parent_points_quick_start_count','rp.contact.field.click_hs_parent_centre_count',"rp.contact.field.w_self_care_completion_level",'rp.contact.field.parent_point_count_safe',"rp.contact.field.parent_point_count_relax", "rp.contact.field.parent_point_count_treat_yourself", "rp.contact.field.parent_point_count_praise_yourself", "rp.contact.field.parent_point_count_spend_time", "rp.contact.field.parent_point_count_instruct_positively","rp.contact.field.parent_point_count_breathe","rp.contact.field.parent_point_count_money","rp.contact.field.parent_point_count_consequence", 
                                                     "rp.contact.field.w_1on1_completion_level", "rp.contact.field.w_praise_completion_level",
                                                     "rp.contact.field.w_instruct_completion_level", "rp.contact.field.w_stress_completion_level" ,
                                                     "rp.contact.field.w_money_completion_level", "rp.contact.field.w_rules_completion_level",
                                                     "rp.contact.field.w_consequence_completion_level","rp.contact.field.w_solve_completion_level","rp.contact.field.w_safe_completion_level","rp.contact.field.w_crisis_completion_level","rp.contact.field.w_celebrate_completion_level") %>%
    mutate(group_4 = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50 &
                              rp.contact.field.w_1on1_completion_level> 50 & rp.contact.field.w_stress_completion_level>50 &
                              rp.contact.field.w_instruct_completion_level>50 & rp.contact.field.w_money_completion_level>50 & 
                              rp.contact.field.w_rules_completion_level>50, 
                            1,
                            0)) %>%  
    mutate(group_3 = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50 &
                              rp.contact.field.w_1on1_completion_level> 50 & rp.contact.field.w_stress_completion_level>50 &
                              rp.contact.field.w_instruct_completion_level>50,
                            1,
                            0)) %>%
    mutate(group_2bplus = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50  &
                                   rp.contact.field.w_1on1_completion_level> 50,
                                 1,
                                 0)) %>%
    mutate(group_2b = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50  &
                               rp.contact.field.w_1on1_completion_level> 50 & rp.contact.field.w_stress_completion_level< 1 &
                               rp.contact.field.w_instruct_completion_level < 1,
                             1,
                             0)) %>%
    mutate(group_2aplus = ifelse(rp.contact.field.w_self_care_completion_level > 50,
                                 1,
                                 0)) %>%
    mutate(group_2a = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level < 1  &
                               rp.contact.field.w_1on1_completion_level< 1 & rp.contact.field.w_stress_completion_level< 1 &
                               rp.contact.field.w_instruct_completion_level < 1,
                             1,
                             0)) %>% rowwise() %>% 
    mutate(group_2c = ifelse(mean(c(rp.contact.field.w_self_care_completion_level, rp.contact.field.w_praise_completion_level,
                                    rp.contact.field.w_1on1_completion_level, rp.contact.field.w_stress_completion_level,
                                    rp.contact.field.w_instruct_completion_level))> 50,
                             1,
                             0)) %>% 

    filter(Org != 'Miss')

  View(plhdata_group_ids)

  #plhdata_group_ids$createdAt<-as.Date(plhdata_group_ids$createdAt)

  Single_user<-plhdata_group_ids %>% filter(app_user_id== user_id ) %>%  
    select('app_user_id', 'Org', 'app_version','rp.contact.field.app_launch_count','rp.contact.field._app_language','rp.contact.field.user_age',"createdAt",'rp.contact.field._server_sync_latest','rp.contact.field.household_children','rp.contact.field.survey_welcome_completed' ,'rp.contact.field.w_self_care_completion_level','rp.contact.field.w_1on1_completion_level','rp.contact.field.w_instruct_completion_level','rp.contact.field.w_praise_completion_level','rp.contact.field.w_stress_completion_level','rp.contact.field.w_money_completion_level','rp.contact.field.w_rules_completion_level','rp.contact.field.w_consequence_completion_level','rp.contact.field.w_solve_completion_level','rp.contact.field.w_safe_completion_level','rp.contact.field.w_crisis_completion_level','rp.contact.field.w_celebrate_completion_level','rp.contact.field.do_workshops_together', 'rp.contact.field.click_hs_parent_centre_count','rp.contact.field.click_hs_weekly_workshops_quick_start_count','rp.contact.field.click_hs_parent_centre_quick_start_count','rp.contact.field.click_hs_parent_points_quick_start_count', 'rp.contact.field.parent_point_count_safe','rp.contact.field.parent_point_count_relax', "rp.contact.field.parent_point_count_treat_yourself", "rp.contact.field.parent_point_count_praise_yourself", "rp.contact.field.parent_point_count_spend_time", "rp.contact.field.parent_point_count_instruct_positively","rp.contact.field.parent_point_count_breathe","rp.contact.field.parent_point_count_money","rp.contact.field.parent_point_count_consequence")%>%
    rowwise() %>%  dplyr::mutate(Parentpoints_cumulative = sum(c(rp.contact.field.parent_point_count_relax,rp.contact.field.parent_point_count_safe, rp.contact.field.parent_point_count_treat_yourself, rp.contact.field.parent_point_count_praise_yourself, rp.contact.field.parent_point_count_spend_time, rp.contact.field.parent_point_count_instruct_positively,rp.contact.field.parent_point_count_breathe,rp.contact.field.parent_point_count_money,rp.contact.field.parent_point_count_consequence)) ) %>%
    gather('User Id', value, -'app_user_id')%>%
    spread('app_user_id', value)

  Single_user[Single_user$'User Id'=='Org',]$'User Id'<-'Organisation'
  Single_user[Single_user$'User Id'=='rp.contact.field._app_language',]$'User Id'<-'Language used'
  Single_user[Single_user$'User Id'=='app_version',]$'User Id'<-'App Version'
  Single_user[Single_user$'User Id'=='rp.contact.field._server_sync_latest',]$'User Id'<-'Data last synched'
  Single_user[Single_user$'User Id'=='rp.contact.field.user_age',]$'User Id'<-'User age'
  Single_user[Single_user$'User Id'=='rp.contact.field.household_children',]$'User Id'<-'Number of children'
  Single_user[Single_user$'User Id'=='rp.contact.field.survey_welcome_completed',]$'User Id'<-'Baseline survey completed'
  Single_user[Single_user$'User Id'=='rp.contact.field.click_hs_parent_centre_quick_start_count',]$'User Id'<-'Quickstart buttons ever used(parentcentre)'
  Single_user[Single_user$'User Id'=='rp.contact.field.click_hs_parent_points_quick_start_count',]$'User Id'<-'Quickstart buttons ever used(parentpoints)'
  Single_user[Single_user$'User Id'=='rp.contact.field.click_hs_weekly_workshops_quick_start_count',]$'User Id'<-'Quickstart buttons ever used(weekly workshops)'
  Single_user[Single_user$'User Id'=='rp.contact.field.do_workshops_together',]$'User Id'<-'Used in a group/as individual'
  Single_user[Single_user$'User Id'=='rp.contact.field.app_launch_count',]$'User Id'<-'Number of app opens'
  Single_user[Single_user$'User Id'=='rp.contact.field.parent_point_count_breathe',]$'User Id'<-'parent_point_count_breathe'
  Single_user[Single_user$'User Id'=='rp.contact.field.parent_point_count_consequence',]$'User Id'<-'parent_point_count_consequence'
  Single_user[Single_user$'User Id'=='rp.contact.field.parent_point_count_instruct_positively',]$'User Id'<-'parent_point_count_instruct_positively'
  Single_user[Single_user$'User Id'=='rp.contact.field.parent_point_count_money',]$'User Id'<-'parent_point_count_money'
  Single_user[Single_user$'User Id'=='rp.contact.field.parent_point_count_praise_yourself',]$'User Id'<-'parent_point_count_praise_yourself'
  Single_user[Single_user$'User Id'=='rp.contact.field.parent_point_count_relax',]$'User Id'<-'parent_point_count_relax'
  Single_user[Single_user$'User Id'=='rp.contact.field.parent_point_count_safe',]$'User Id'<-'parent_point_count_safe'
  Single_user[Single_user$'User Id'=='rp.contact.field.parent_point_count_spend_time',]$'User Id'<-'parent_point_count_spend_time'
  Single_user[Single_user$'User Id'=='rp.contact.field.parent_point_count_treat_yourself',]$'User Id'<-'parent_point_count_treat_yourself'
  Single_user[Single_user$'User Id'=='rp.contact.field.click_hs_parent_centre_count',]$'User Id'<-'Parent library overall clicks on homescreen'
  Single_user[Single_user$'User Id'=='rp.contact.field.w_self_care_completion_level',]$'User Id'<-'Welcome & self care workshop'
  Single_user[Single_user$'User Id'=='rp.contact.field.w_1on1_completion_level',]$'User Id'<-'One-on-one time workshop'
  Single_user[Single_user$'User Id'=='rp.contact.field.w_praise_completion_level',]$'User Id'<-'Praise workshop'
  Single_user[Single_user$'User Id'=='rp.contact.field.w_instruct_completion_level',]$'User Id'<-'Positive instructions workshop'
  Single_user[Single_user$'User Id'=='rp.contact.field.w_stress_completion_level',]$'User Id'<-'Managing stress workshop'
  Single_user[Single_user$'User Id'=='rp.contact.field.w_money_completion_level',]$'User Id'<-'Family budgets workshop'
  Single_user[Single_user$'User Id'=='rp.contact.field.w_rules_completion_level',]$'User Id'<-'Rules workshop'
  Single_user[Single_user$'User Id'=='rp.contact.field.w_consequence_completion_level',]$'User Id'<-'Calm consequences workshop'
  Single_user[Single_user$'User Id'=='rp.contact.field.w_solve_completion_level',]$'User Id'<-'Problem solving workshop'
  Single_user[Single_user$'User Id'=='rp.contact.field.w_safe_completion_level',]$'User Id'<-'Teen safety workshop'
  Single_user[Single_user$'User Id'=='rp.contact.field.w_crisis_completion_level',]$'User Id'<-'Crisis workshop'
  Single_user[Single_user$'User Id'=='rp.contact.field.w_celebrate_completion_level',]$'User Id'<-'Celebrate & next steps workshop'

  View(Single_user)

}

User_key_values(plhdata_org_clean, "954751197453e59b")




