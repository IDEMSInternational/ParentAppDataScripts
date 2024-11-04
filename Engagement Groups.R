# Sampled UIC's for Qual Interviews: Completion levels of workshops

# Use "UpdatedAt"

# Group 1: Non engagers: Users who have not engaged with a workshop.
# Group 2: Low – medium engagers: Users who have engaged with OR completed
#          less than 75% of the number of workshops based on their programme
#          start date.
#          This would include users who have started but not completed
#          workshops (e.g. only engaged with a smaller % of the steppers)
#          and users who are ‘behind’ where they should be in terms of
#          overall progress on the number of workshops accessed.

# Group 3: High engagers: Users who have completed 75% or more of the number
#          of workshops to at least 80% completion per workshop based on
#          their programme start date.
#          This would include users are progressing as expected in terms of
#          the number of workshops based on when they started the programme
#          and are engaging with at least 80% of the workshop content/steppers.

plhdata_org_clean <- plhdata_org_clean %>%
  mutate(across(ends_with("_completion_level"), ~as.numeric(.)))

# Function for threshold ----
threshhold_function <- function(data, threshhold, sign = "gt"){
  if (sign == "gt"){
    plhdata_org_clean_engagement <- data %>%
      mutate(self_care_started = ifelse(rp.contact.field.w_self_care_completion_level > threshhold, 1, 0),
             one_on_one_started = ifelse(rp.contact.field.w_1on1_completion_level > threshhold, 1, 0),
             praise_started = ifelse(rp.contact.field.w_praise_completion_level > threshhold, 1, 0),
             instruct_started = ifelse(rp.contact.field.w_instruct_completion_level > threshhold, 1, 0),
             stress_started = ifelse(rp.contact.field.w_stress_completion_level > threshhold, 1, 0),
             money_started = ifelse(rp.contact.field.w_money_completion_level > threshhold, 1, 0),
             rules_started = ifelse(rp.contact.field.w_rules_completion_level > threshhold, 1, 0),
             consequence_started = ifelse(rp.contact.field.w_consequence_completion_level > threshhold, 1, 0),
             solve_started = ifelse(rp.contact.field.w_solve_completion_level > threshhold, 1, 0),
             safe_started = ifelse(rp.contact.field.w_safe_completion_level > threshhold, 1, 0),
             crisis_started = ifelse(rp.contact.field.w_crisis_completion_level > threshhold, 1, 0),
             celebrate_started = ifelse(rp.contact.field.w_celebrate_completion_level > threshhold, 1, 0))
  } else if (sign == "lt"){
    plhdata_org_clean_engagement <- data %>%
      mutate(self_care_started = ifelse(rp.contact.field.w_self_care_completion_level < threshhold, 1, 0),
             one_on_one_started = ifelse(rp.contact.field.w_1on1_completion_level < threshhold, 1, 0),
             praise_started = ifelse(rp.contact.field.w_praise_completion_level < threshhold, 1, 0),
             instruct_started = ifelse(rp.contact.field.w_instruct_completion_level < threshhold, 1, 0),
             stress_started = ifelse(rp.contact.field.w_stress_completion_level < threshhold, 1, 0),
             money_started = ifelse(rp.contact.field.w_money_completion_level < threshhold, 1, 0),
             rules_started = ifelse(rp.contact.field.w_rules_completion_level < threshhold, 1, 0),
             consequence_started = ifelse(rp.contact.field.w_consequence_completion_level < threshhold, 1, 0),
             solve_started = ifelse(rp.contact.field.w_solve_completion_level < threshhold, 1, 0),
             safe_started = ifelse(rp.contact.field.w_safe_completion_level < threshhold, 1, 0),
             crisis_started = ifelse(rp.contact.field.w_crisis_completion_level < threshhold, 1, 0),
             celebrate_started = ifelse(rp.contact.field.w_celebrate_completion_level < threshhold, 1, 0))
  } else {
    stop("sign must be one of gt or lt")
  }
  return(plhdata_org_clean_engagement)
}

# All groups
plhdata_group_ids <- plhdata_org_clean %>%  select('app_user_id','Org', "createdAt", "rp.contact.field.w_self_care_completion_level", 
                                                   "rp.contact.field.w_1on1_completion_level", "rp.contact.field.w_praise_completion_level",
                                                   "rp.contact.field.w_instruct_completion_level", "rp.contact.field.w_stress_completion_level" ,
                                                   "rp.contact.field.w_money_completion_level", "rp.contact.field.w_rules_completion_level" , 
                                                   "rp.contact.field.w_consequence_completion_level" , "rp.contact.field.w_solve_completion_level", 
                                                   "rp.contact.field.w_safe_completion_level" , "rp.contact.field.w_crisis_completion_level" , 
                                                   "rp.contact.field.w_celebrate_completion_level" )

# Creating Group 1
# Group 1: Non engagers: Users who have not engaged with a workshop.
plhdata_group_ids_group_1 <- threshhold_function(data = plhdata_group_ids, threshhold = 0)
plhdata_group_ids_group_1 <- plhdata_group_ids_group_1 %>%
  mutate(engagement_total = self_care_started + one_on_one_started + praise_started + 
           instruct_started + stress_started + money_started + rules_started + consequence_started + 
           solve_started + safe_started + crisis_started + celebrate_started) %>%
  filter(engagement_total == 0)
plhdata_group_ids_group_1$app_user_id

# Creating Group 2
# Group 2: Low – medium engagers: Users who have engaged with OR completed
#          less than 75% of the number of workshops based on their programme
#          start date.
#          This would include users who have started but not completed
#          workshops (e.g. only engaged with a smaller % of the steppers)
#          and users who are ‘behind’ where they should be in terms of
#          overall progress on the number of workshops accessed.
plhdata_group_ids <- plhdata_group_ids %>%
  mutate(createdAt = as.Date(createdAt, "%y %m %d", tz = "utc"), # can calculate by UIC tracker
         curr_date = as.Date(Sys.Date(), "%y %m %d")) %>%
  mutate(diff_in_days = curr_date - createdAt) %>%
  mutate(week_number = floor(as.numeric(diff_in_days/7))) %>%
  mutate(week_number = ifelse(week_number > 12, 12, week_number))
# 1. find workshops engaged with/completed
plhdata_group_ids_group_2 <- threshhold_function(data = plhdata_group_ids, threshhold = 0)
# 2. filter to those who have that this on <75% of workshops
plhdata_group_ids_group_2 <- plhdata_group_ids_group_2 %>%
  mutate(engagement_total = self_care_started + one_on_one_started + praise_started +
           instruct_started + stress_started + money_started + rules_started + consequence_started +
           solve_started + safe_started + crisis_started + celebrate_started) %>%
  filter(engagement_total < ceiling(week_number * 0.75)) %>%
  filter(engagement_total > 0)
plhdata_group_ids_group_2$app_user_id


# Creating Group 3
# Group 3: High engagers: Users who have completed 75% or more of the number
#          of workshops to at least 80% completion per workshop based on
#          their programme start date.
#          This would include users are progressing as expected in terms of
#          the number of workshops based on when they started the programme
#          and are engaging with at least 80% of the workshop content/steppers.
plhdata_group_ids <- plhdata_group_ids %>%
  mutate(createdAt = as.Date(createdAt, "%y %m %d", tz = "UTC"),
         curr_date = as.Date(Sys.Date(), "%y %m %d")) %>%
  mutate(diff_in_days = curr_date - createdAt) %>%
  mutate(week_number = floor(as.numeric(diff_in_days/7)))
# 1. find workshops engaged with at least 80%/completed
plhdata_group_ids_group_3 <- threshhold_function(data = plhdata_group_ids, threshhold = 79.9)
# 2. filter to those who have that 80%+ on >= 75% of workshops
plhdata_group_ids_group_3 <- plhdata_group_ids_group_3 %>%
  mutate(engagement_total = self_care_started + one_on_one_started + praise_started + 
           instruct_started + stress_started + money_started + rules_started + consequence_started + 
           solve_started + safe_started + crisis_started + celebrate_started) %>%
  mutate(week_number = ifelse(week_number > 12, 12, week_number)) %>% # check this worked
  filter(engagement_total >= week_number * 0.75)
plhdata_group_ids_group_3$app_user_id


intersect(plhdata_group_ids_group_1$app_user_id, plhdata_group_ids_group_2$app_user_id)
intersect(plhdata_group_ids_group_1$app_user_id, plhdata_group_ids_group_3$app_user_id)
intersect(plhdata_group_ids_group_2$app_user_id, plhdata_group_ids_group_1$app_user_id)
intersect(plhdata_group_ids_group_2$app_user_id, plhdata_group_ids_group_3$app_user_id)
intersect(plhdata_group_ids_group_3$app_user_id, plhdata_group_ids_group_1$app_user_id)

nrow(plhdata_group_ids_group_1)
nrow(plhdata_group_ids_group_2)
nrow(plhdata_group_ids_group_3)

# who isn't in any?

ungroup_ppl <- setdiff(plhdata_org_clean$app_user_id, plhdata_group_ids_group_1$app_user_id)
ungroup_ppl <- setdiff(ungroup_ppl, plhdata_group_ids_group_2$app_user_id)
ungroup_ppl <- setdiff(ungroup_ppl, plhdata_group_ids_group_3$app_user_id)

# yay
# ungroup_ppl

plhdata_group_ids_group_ungroup <- threshhold_function(data = plhdata_group_ids, threshhold = 79.9)
View(plhdata_group_ids_group_ungroup %>% filter(app_user_id %in% ungroup_ppl))




#################
## New definition
# ow engagement (completing ≤ 4 workshops or modules)
# moderate engagement (completing ≥ 5 and ≤ 8 workshops or modules)
# high engagement (completing ≥ 9 workshops or modules).

# Creating Group 1
# Group 1: Non engagers: Users who have not engaged with a workshop.
plhdata_group_ids_group_eng <- threshhold_function(data = plhdata_group_ids, threshhold = 99.9)
plhdata_group_ids_group_eng <- plhdata_group_ids_group_eng %>%
  mutate(engagement_total = self_care_started + one_on_one_started + praise_started + 
           instruct_started + stress_started + money_started + rules_started + consequence_started + 
           solve_started + safe_started + crisis_started + celebrate_started)

#head(plhdata_group_ids_group_eng)

plhdata_group_ids_group_eng <- plhdata_group_ids_group_eng %>%
  mutate(engagement_level = ifelse(engagement_total <= 4, "low",
                                   ifelse(engagement_total <= 8, "moderate",
                                          ifelse(engagement_total >= 9, "high",
                                                 "else")))) %>%
  dplyr::select(-c( self_care_started,one_on_one_started,praise_started,
                     instruct_started,stress_started,money_started,rules_started,consequence_started,
                     solve_started,safe_started,crisis_started,celebrate_started))

writexl::write_xlsx(plhdata_group_ids_group_eng, "optimisation_engagement_20230126.xlsx")



###############

# Could you just include the non-engaging category 
# the onboarding date/start date for each participant
# their gender.

# Non engagers: All participants who completed ≤ 1 workshops/modules
# Low engagers: All participants who completed between ≥ 2 and ≤ 4 workshops/modules
# Moderate engagers: All participants who completed between ≥ 5 and  ≤  8 workshops/modules
# High engagers: All participants who completed ≥ 9  workshops/modules

#IDEMS to pull list of ParentApp codes for no workshop engagement, first one, adn first two engaged
plhdata_group_ids <- plhdata_org_clean %>%  select('app_user_id','Support', 'Skin', 'Digital Literacy', "createdAt", "opt_cluster",
                                                   "experimental_condition", "rp.contact.field.user_gender",
                                                   "OnboardingDate",
                                                   "rp.contact.field.w_self_care_completion_level", 
                                                   "rp.contact.field.w_1on1_completion_level", "rp.contact.field.w_praise_completion_level",
                                                   "rp.contact.field.w_instruct_completion_level", "rp.contact.field.w_stress_completion_level" ,
                                                   "rp.contact.field.w_money_completion_level", "rp.contact.field.w_rules_completion_level" , 
                                                   "rp.contact.field.w_consequence_completion_level" , "rp.contact.field.w_solve_completion_level", 
                                                   "rp.contact.field.w_safe_completion_level" , "rp.contact.field.w_crisis_completion_level" , 
                                                   "rp.contact.field.w_celebrate_completion_level" )

plhdata_group_ids_group_1 <- threshhold_function(data = plhdata_group_ids, threshhold = 0)
plhdata_group_ids_group_1 <- plhdata_group_ids_group_1 %>%
  mutate(engagement_total = self_care_started + one_on_one_started + praise_started + 
           instruct_started + stress_started + money_started + rules_started + consequence_started + 
           solve_started + safe_started + crisis_started + celebrate_started) 
#View(plhdata_group_ids_group_1)
plhdata_group_ids_group_1 <- plhdata_group_ids_group_1 %>%
  mutate(engagement_level = ifelse(engagement_total <= 1, "non",
                                   ifelse(engagement_total <= 4, "low",
                                          ifelse(engagement_total <= 8, "moderate",
                                                 ifelse(engagement_total <= 12, "high",
                                                        "else")))))
  
#two_engagement <- plhdata_group_ids_group_1 %>%
#  filter(engagement_total == 2) %>%
#  filter(self_care_started == 1) %>%
#  filter(one_on_one_started == 1)

#no_engagement <- no_engagement %>% dplyr::select(c(app_user_id, Skin, Support, `Digital Literacy`, `Self care completion` = rp.contact.field.w_self_care_completion_level, `1on1 completion` = rp.contact.field.w_1on1_completion_level, engagement_total))
#one_engagement <- one_engagement %>% dplyr::select(c(app_user_id, Skin, Support, `Digital Literacy`, `Self care completion` = rp.contact.field.w_self_care_completion_level, `1on1 completion` = rp.contact.field.w_1on1_completion_level, engagement_total))
#two_engagement <- two_engagement %>% dplyr::select(c(app_user_id, Skin, Support, `Digital Literacy`, `Self care completion` = rp.contact.field.w_self_care_completion_level, `1on1 completion` = rp.contact.field.w_1on1_completion_level, engagement_total))

#low_engagers <- rbind(no_engagement, one_engagement, two_engagement)

#View(low_engagers)

#low_engagers %>% group_by(engagement_total, Skin, Support, `Digital Literacy`) %>%
#  summarise(n()) %>% View()

writexl::write_xlsx(plhdata_group_ids_group_1, "plhdata_optimisation_engagement_20230130.xlsx")
