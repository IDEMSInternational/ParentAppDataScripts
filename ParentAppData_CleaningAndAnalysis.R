# run libraries
library(tidyverse)
library(here)

#Source the personal setup for data
source(here("config", "Personal Setup.R"))

#Get data from excel
UIC.Tracker <- rio::import(file = here("data", "UIC Tracker.xlsx"), which = "UIC Tracker 211014")

#Get the List of PLH Tables and data from server
plh_tables <- dbListTables(plh_con)
df <- dbReadTable(plh_con, plh_tables[2])

#Source the personal setup for data
#Manuel step source("Personal Setup Script.R")

#Source the data cleaning and setup
source(here("ParentAppData_setupclean.R"))

#Write clean data back to Metabase
dbWriteTable(parent_app_con, "Cleaned PLH data", select(plhdata_org_clean,!(contact_fields)), overwrite=TRUE)

# Define list of organisations
orgs_list <- levels(plhdata_org_clean$Org)

# Show the summary of app versions
sjPlot::sjtab(data=plhdata_org_clean, Org, app_version, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Subset the data
#####Create a subset for an organisation NONTOBEKO####

# Create subsets of the data based on organisation NONTOBEKO

Organization_Table(data=plhdata_org, Org_name = "Nontobeko")

#plhdata_org_NONTOBEKO <- filter(plhdata_org, Org == "Nontobeko")

# Show any app user ids that are invalid and do not come from the app.
#plhdata_org_NONTOBEKO %>% filter(is.na(plhdata_org_NONTOBEKO$'app_version')) %>% select('app_user_id')

# Create subsets of the data based on valid NONTOBEKOM app user ID's
#plhdata_org_NONTOBEKO <- filter(plhdata_org_NONTOBEKO, !is.na(plhdata_org_NONTOBEKO$'app_version'))

# Show the summary of app versions
#sjmisc::frq(x=plhdata_org_NONTOBEKO$'app_version', out="txt")

# Show any app user ids that have only synced initial data.
plhdata_org_NONTOBEKO %>% filter(is.na(plhdata_org_NONTOBEKO$rp.contact.field.first_app_open)) %>% select('app_user_id')


#####Create a subset for an organisation DLALANATHI####
# Create subsets of the data based on organisation DLALANATHI
Organization_Table(data=plhdata_org, Org_name = "Dlalanathi")


# Show any app user ids that have only synced initial data [done for each Org under subsetting but also with for loop further below]
plhdata_org_DLALANATHI %>% filter(is.na(plhdata_org_DLALANATHI$rp.contact.field.first_app_open)) %>% select('app_user_id')


#####Create a subset for an organisation HILLCREST####
Organization_Table(data=plhdata_org, Org_name = "Hillcrest")

#plhdata_org_HILLCREST <- filter(plhdata_org, Org == "Hillcrest")

# Show any app user ids that are invalid and do not come from the app.
#plhdata_org_HILLCREST %>% filter(is.na(plhdata_org_HILLCREST$'app_version')) %>% select('app_user_id')

# Create subsets of the data based on valid HILLCREST app user ID's
#plhdata_org_HILLCREST <- filter(plhdata_org_HILLCREST, !is.na(plhdata_org_HILLCREST$'app_version'))

# Show the summary of app versions
#sjmisc::frq(x=plhdata_org_HILLCREST$'app_version', out="txt")

# Show any app user ids that have only synced initial data.
plhdata_org_HILLCREST %>% filter(is.na(plhdata_org_HILLCREST$rp.contact.field.first_app_open)) %>% select('app_user_id')


#####Create a subset for an organisation Joy####

Organization_Table(data=plhdata_org, Org_name = "Joy")

# Create subsets of the data based on organisation Joy
#plhdata_org_Joy <- filter(plhdata_org, Org == "Joy")

# Show any app user ids that are invalid and do not come from the app.
#plhdata_org_Joy %>% filter(is.na(plhdata_org_Joy$'app_version')) %>% select('app_user_id')

# Create subsets of the data based on valid Joy app user ID's
#plhdata_org_Joy <- filter(plhdata_org_Joy, !is.na(plhdata_org_Joy$'app_version'))

# Show the summary of app versions
#sjmisc::frq(x=plhdata_org_Joy$'app_version', out="txt")

# Show any app user ids that have only synced initial data.
plhdata_org_Joy %>% filter(is.na(plhdata_org_Joy$rp.contact.field.first_app_open)) %>% select('app_user_id')



#####Create a subset for an organisation Amathuba####

Organization_Table(data=plhdata_org, Org_name = "Amathuba")

# Create subsets of the data based on organisation Amathuba
#plhdata_org_Amathuba <- filter(plhdata_org, Org == "Amathuba")

# Show any app user ids that are invalid and do not come from the app.
#plhdata_org_Amathuba %>% filter(is.na(plhdata_org_Amathuba$'app_version')) %>% select('app_user_id')

# Create subsets of the data based on valid Joy app user ID's
#plhdata_org_Amathuba <- filter(plhdata_org_Amathuba, !is.na(plhdata_org_Amathuba$'app_version'))

# Show the summary of app versions
#sjmisc::frq(x=plhdata_org_Amathuba$'app_version', out="txt")

# Show any app user ids that have only synced initial data.
plhdata_org_Amathuba %>% filter(is.na(plhdata_org_Amathuba$rp.contact.field.first_app_open)) %>% select('app_user_id')



#Workshop started & completed analysis

# Show the summary of Self care workshop started(1st Workshop)                            
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_self_care_started, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of Self care workshop completion(1st Workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_self_care_completed  , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of One-on-One Time started(2nd Workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_1on1_started  , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of  One-on-One Time completion(2nd Workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_1on1_completed , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of Praise started(3rd workshop) 
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_praise_started , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of Praise(3rd Workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_praise_completed , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

## Show the summary of Positive Instructions(4th workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_instruct_started , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of Positive Instructions(4th workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_instruct_completed , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of Managing Stress(5th workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_stress_started , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of Managing Stress(5th workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_stress_completed , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of Family Budgets(6th workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_money_started , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of Family Budgets(6th workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_money_completed , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")




#Baseline survey
# Show the summary of  baseline survey completion
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_completed , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
 

#Descriptive statistics
#Show the summary of user gender
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.user_gender, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Show the summary of user age
plhdata_org_clean$rp.contact.field.user_age <- as.numeric(plhdata_org_clean$rp.contact.field.user_age)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.user_age, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")


#show the summary , household adults
plhdata_org_clean$rp.contact.field.household_adults <- as.numeric(plhdata_org_clean$rp.contact.field.household_adults)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.household_adults, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#show the summary of , household teens
plhdata_org_clean$rp.contact.field.household_teens <- as.numeric(plhdata_org_clean$rp.contact.field.household_teens)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.household_teens, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#show the summary of , household babies
plhdata_org_clean$rp.contact.field.household_babies <- as.numeric(plhdata_org_clean$rp.contact.field.household_babies)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.household_babies, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#show the summary of , household children
plhdata_org_clean$rp.contact.field.household_children <- as.numeric(plhdata_org_clean$rp.contact.field.household_children)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.household_children, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#show the summary of app versions, language
user_id_print <- function(field) {
  for (o in orgs_list) {
    # print organisation first
    print(o)
    # print filtered data
    print(
      plhdata_org%>%
        filter(Org == o) %>%
        select('app_user_id', field)
    )
  }
}

sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field._app_language, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field._app_language")



#Time spent on the workshops.
#Self care workshop
plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed <- as.numeric(plhdata_org_clean$rp.contact.field.w_1on1_diff_started_completed)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_self_care_diff_started_completed, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")


#One on one time workshop

sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_1on1_diff_started_completed, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")


#Parent Points

##Show summary  for each Parent Point, all time number of clicks - for each user
user_id_print <- function(field) {
  for (o in orgs_list) {
    # print organisation first
    print(o)
    # print filtered data
    print(
      plhdata_org %>%
        filter(Org == o) %>%
        select('app_user_id', field)
    )
  }
}
#HABIT: Relax
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_relax, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_relax")

# Replaced by above
# plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax")
# plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax")
# plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax")
# plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax")


#HABIT: Treat yourself well
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_treat_yourself, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_treat_yourself")

#HABIT: Praise yourself
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_praise_yourself, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_praise_yourself")


#HABIT:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_spend_time, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_spend_time")


#HABIT:Praise your teen
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_praise_teen, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_praise_teen")


#HABIT:Get Positive
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_instruct_positively, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_instruct_positively")


#HABIT:Breathe not yell
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_breathe, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_breathe")


#HABIT:Good money choice
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_money, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_money")


#HABIT:Calm consequence
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_consequence, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_consequence")


#HABIT:Safe
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_safe, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_safe")


##Show the summary for each parent point and each week, number of clicks in that week for each user.

#HABIT:Relax  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_relax_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_relax_w_self_care")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax_w_self_care")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax_w_self_care")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax_w_self_care")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax_w_self_care")


#HABIT:Treat yourself well  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_treat_yourself_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_treat_yourself_w_self_care")


#HABIT:Praise yourself well  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_praise_yourself_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_praise_yourself_w_self_care")

#HABIT:One-on-one time  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_spend_time_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_spend_time_w_self_care")

#HABIT:Praise your teen  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_praise_teen_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_praise_teen_w_self_care")

#HABIT:Breathe not yell  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_breathe_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_breathe_w_self_care")

#HABIT:Good Money Choice  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_money_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_money_w_self_care")

#HABIT:Calm Consequence  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_consequence_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_consequence_w_self_care")

#HABIT:Safe  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_safe_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_safe_w_self_care")

#HABIT:Relax  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_relax_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_relax_w_1on1")

#HABIT:Treat yourself well  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_treat_yourself_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_treat_yourself_w_1on1")


#HABIT:Praise yourself well  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_praise_yourself_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_treat_yourself_w_1on1")

#HABIT:One-on-one time  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_spend_time_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_spend_time_w_1on1")


#HABIT:Praise your teen  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_praise_teen_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_praise_teen_w_1on1")


#HABIT:Breathe not yell  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_breathe_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_breathe_w_1on1")


#HABIT:Good Money Choice  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_money_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_money_w_1on1")


#HABIT:Calm Consequence  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_consequence_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_consequence_w_1on1")


#HABIT:Safe  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_safe_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_safe_w_1on1")


## Show any app user ids that have only synced initial data [Not working as expected]
# sjPlot::sjtab(data=plhdata_org_clean, Org, is.na(rp.contact.field.first_app_open), show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
# user_id_print("rp.contact.field.first_app_open")


#Workshop completion level
#show the summary of total number of activities viewed out of total number of workshop activities (steppers)#####
#percentage corresponding to the proportion the proportion out of the 9 items of the stepper. For example, 4/9  =0.4444444 = 45.
#which is the best way to identify the number of steppers for each workshop?###

user_id_print <- function(field) {
  for (o in orgs_list) {
    # print organisation first
    print(o)
    # print filtered data
    print(
      plhdata_org %>%
        filter(Org == o) %>%
        select('app_user_id', field)
    )
  }
}

#(Welcome and Self-Care Workshop)
#change column names to numeric
plhdata_org_clean$rp.contact.field.w_self_care_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_self_care_completion_level)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_self_care_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_self_care_completion_level")




#Replaced by above
#Show app user ids and  their completion levels(Selfcare):NONTOBEKO
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.w_self_care_completion_level") 

#Show app user ids and whether their completion levels(Selfcare):DLALANATHI
#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.w_self_care_completion_level")

#Show app user ids and whether their completion levels(Selfcare):JOY
#plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.w_self_care_completion_level")



#One-on-One Time
plhdata_org_clean$rp.contact.field.w_1on1_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_1on1_completion_level)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_1on1_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_1on1_completion_level")

#Praise
plhdata_org_clean$rp.contact.field.w_praise_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_praise_completion_level)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_praise_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_praise_completion_level")

#Positive Instructions
plhdata_org_clean$rp.contact.field.w_instruct_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_instruct_completion_level)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_instruct_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_instruct_completion_level")

#Managing Stress
plhdata_org_clean$rp.contact.field.w_stress_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_stress_completion_level)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_stress_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_stress_completion_level")

#Family Budgets
plhdata_org_clean$rp.contact.field.w_money_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_money_completion_level)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_money_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_money_completion_level")

#Rules
plhdata_org_clean$rp.contact.field.w_safe_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_safe_completion_level)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_safe_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_safe_completion_level")

#Calm Consequences
plhdata_org_clean$rp.contact.field.w_consequence_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_consequence_completion_level)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_consequence_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_consequence_completion_level")

#Problem Solving
plhdata_org_clean$rp.contact.field.w_solve_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_solve_completion_level)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_solve_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_solve_completion_level")

#Teen Safety
plhdata_org_clean$rp.contact.field.w_safe_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_safe_completion_level)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_safe_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_safe_completion_level")

#Dealing with Crisis
plhdata_org_clean$rp.contact.field.w_crisis_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_crisis_completion_level)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_crisis_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_crisis_completion_level")

#Celebration and Next Steps
plhdata_org_clean$rp.contact.field.w_celebrate_completion_level <- as.numeric(plhdata_org_clean$rp.contact.field.w_celebrate_completion_level)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_celebrate_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_celebrate_completion_level")


##Response to each survey question (respond, skip, exit)
#Question: how to find average number "no value" responses to survey questions per org
#Days attention past week
# Show the summary of survey_welcome_a_1_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_1_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.survey_welcome_a_1_final")

# Days of praise past week
# Show the summary of survey_welcome_a_2_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_2_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.survey_welcome_a_2_final")

#Days of stress past week
# Show the summary of survey_welcome_a_3_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_3_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.survey_welcome_a_3_final")

#Days of shouting past week
# Show the summary of survey_welcome_a_4_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_4_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.survey_welcome_a_4_final")


#Days of money worry past week
# Show the summary of survey_welcome_a_5_part_1_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_5_part_1_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.survey_welcome_a_5_part_1_final")

# Show the summary of survey_welcome_a_5_part_2_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_5_part_2_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.survey_welcome_a_5_part_2_final")

#Days of hitting past week
# Show the summary of survey_welcome_a_6_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_6_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.survey_welcome_a_6_final")

# Knowledge of teen activity past week
# Show the summary of survey_welcome_a_7_part_1_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_7_part_1_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.survey_welcome_a_7_part_1_final")

# iff "7" to 7.1: Lockdown? yes/no
# Show the summary of survey_welcome_a_7_part_2_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_7_part_2_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.survey_welcome_a_7_part_2_final")

#iff "7" to 7.1: Knowledge of teen activity in non-lockdown week
# Show the summary of survey_welcome_a_7_part_3_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_7_part_3_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.survey_welcome_a_7_part_3_final")

#Days of sexual safety talk past month
# Show the summary of survey_welcome_a_8_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_8_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.survey_welcome_a_8_final")

#Days of teen COVID safe past week
# Show the summary of survey_welcome_a_9_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_9_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.survey_welcome_a_9_final")


##Parent Library###
#Show the summary of number of times users access the Parent Library(Number of clicks on the Parent Library tile on the home screen) 
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_parent_centre_count, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.click_hs_parent_centre_count")


#Show the summary of type of content accessed (Number of clicks on any button on the Parent Library page)   
#Help
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_pc_help_count, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.click_pc_help_count")

#My Tips
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_pc_my_tips_count, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.click_pc_my_tips_count")

#Essential Tools
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_pc_essential_tools_count, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.click_pc_esential_tools_count")

#COVID
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_pc_covid_count, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.click_pc_covid_count")

#Customise
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_pc_customisation_count, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.click_pc_customisation_count")

#Relax and Activities
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_pc_relax_and_activities_count, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.click_pc_relax_and_activities_count")

#Support Contacts
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_pc_support_contacts_count, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.click_pc_support_contacts_count")

#Evidence Base
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_evidence_base_count, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.click_evidence_base_count")

#Technical Support
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_pc_technical_support_count, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.click_pc_technical_support_count")

#Message Archive
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_pc_message_archive_count, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.click_pc_message_archive_count")


###Completion status of baseline survey
# Show the summary of baseline survey completion(Organisaton-wise)

sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_completed, out="txt")
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_completed, out="txt")
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_completed, out="txt")
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_completed, out="txt")

# Show the summary of user ids on baseline survey completion (Organisaton-wise)
user_id_print("rp.contact.field.survey_welcome_completed")



#In-app reminders(Number of in-app message clicks per workshop week),Per quick start button, per workshop week 

user_id_print <- function(data = plhdata_org_clean, field) {
  for (o in unique(data$Org)) {
    # print organisation first
    print(o)
    # print filtered data
    print(
      data %>%
        filter(Org == o) %>%
        select('app_user_id', field)
    )
  }
}



plhdata_org_clean$hsqsclickedws1<-is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care)

sjPlot::sjtab(data=plhdata_org_clean, Org, hsqsclickedws1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

user_id_print(data =plhdata_org_DLALANATHI, field = "rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care")



plhdata_org_clean$hsqsclickedws2<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_1on1)


sjPlot::sjtab(data=plhdata_org_clean, Org, hsqsclickedws2, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")


#Test 1

user_id_print <- function(field) {
  for (o in orgs_list) {
    # print organisation first
    print(o)
    # print filtered data
    print(
      plhdata_org %>%
        filter(Org == o) %>%
        select('app_user_id', field)
    )
  }
}

#plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care)

sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

user_id_print("rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care")

user_id_print("Org")

#plhdata_org_clean$hsqsclickedws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care)
plhdata_org_clean$hsqsclickedcountws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care)

#sjPlot::sjtab(data=plhdata_org_clean, Org, hsqsclickedws1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
sjPlot::sjtab(data=plhdata_org_clean, Org,hsqsclickedcountws1 , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

user_id_print("rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care")

plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care")
                                                                   

plhdata_org_clean$hsqsclickedws2<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_1on1)


sjPlot::sjtab(data=plhdata_org_clean, Org, hsqsclickedws2, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")


#Test 2 Priority 22 (how to interpret data?)
#Number of in-app message clicks per workshop week.Per quick start button, per workshop week 
#Workshops
#Self Care workshop
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#replaced by above(contact field is non-existent-removed count)
#sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_count_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#In-app reminders(Number of in-app message clicks per workshop week),Per quick start button, per workshop week 
plhdata_org_clean$hsqsclickedws1<-!is.na(plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_self_care)
sjPlot::sjtab(data=plhdata_org_clean, Org, hsqsclickedws1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#One-on-one time workshop
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Praise
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_w_praise, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Positive Instructions
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_w_instruct, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Managing Stress
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_w_stress, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Family Budgets
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_w_money, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Rules
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_w_rules, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Calm Consequence
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_w_consequence, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Problem Solving
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_w_solve, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Teen Safety
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_w_safe, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Dealing with crisis(how to interpret:could mean that people have not interacted with the app)
# sjPlot::sjtab(data=plhdata_org_clean, 
#               Org,
#               rp.contact.field.click_hs_weekly_workshops_quick_start_w_crisis, 
#               show.summary=FALSE, 
#               digits=0, fun="xtab",
#               title="",
#               string,
#               total="Total")
table(plhdata_org_clean$Org,plhdata_org_clean$rp.contact.field.click_hs_weekly_workshops_quick_start_w_crisis)




#Celebration and Next Steps
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_w_celebrate, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")



#Parent center
#Self Care workshop
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_parent_centre_quick_start_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#One-on-one time workshop
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_parent_centre_quick_start_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")



#Parent points
#Self Care workshop
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_parent_points_quick_start_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#One-on-one time workshop
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_parent_points_quick_start_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")


#Priority 23 
#In-app reminders
#Total number of in-app message clicks.By User
#Weekly workshops
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_weekly_workshops_quick_start_count, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Parent center
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_parent_centre_quick_start_count, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Parent points
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_hs_parent_points_quick_start_count, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")


#Priority 19
#App-opens
#Total number of app-opens for each user(cumulative)

#cabrine <- plhdata_org_clean


user_id_print <- function(field) {
  for (o in orgs_list) {
    # print organisation first
    print(o)
    # print filtered data
    print(
      plhdata_org %>%
        filter(Org == o) %>%
        select('app_user_id', field)
    )
  }
}

sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.app_launch_count, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

user_id_print("rp.contact.field.app_launch_count")

##Priority 20
#App-opens
#Number of app opens within a workshop week for each user

#Self Care Workshop
user_id_print <- function(field) {
  for (o in orgs_list) {
    # print organisation first
    print(o)
    # print filtered data
    print(
      plhdata_org %>%
        filter(Org == o) %>%
        select('app_user_id', field)
    )
  }
}

sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.app_launch_count_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

user_id_print("rp.contact.field.app_launch_count_w_self_care")

#One-on-one time workshop

user_id_print <- function(field) {
  for (o in orgs_list) {
    # print organisation first
    print(o)
    # print filtered data
    print(
      plhdata_org %>%
        filter(Org == o) %>%
        select('app_user_id', field)
    )
  }
}

sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.app_launch_count_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

user_id_print("rp.contact.field.app_launch_count_w_1on1")

##Priority 21
#App-opens
#Maximum time between app-opens - for each user.Time in full days 

sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.max_days_between_app_launches, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Priority 36
#Emotional Check-in
#Rate of users who respond “happy” ,"sad" & "ok"

#Self Care Workshop
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_self_care_welcome_individual_a_final, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")


#One on one time workshop

user_id_print <- function(data = plhdata_org_clean, field) {
  for (o in unique(data$Org)) {
    # print organisation first
    print(o)
    # print filtered data
    print(
      data %>%
        filter(Org == o) %>%
        select('app_user_id', field)
    )
  }
}


sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_1on1_welcome_individual_a_final, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

user_id_print(data =plhdata_org_DLALANATHI, field = "rp.contact.field.w_1on1_welcome_individual_a_final")


#Praise Workshop
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_praise_welcome_individual_a_final, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Positive Instructions Workshop
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_instruct_welcome_individual_a_final, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Managing Stress Workshop
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_stress_welcome_individual_a_final, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Family Budgets
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_money_welcome_individual_a_final, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")

#Rules
sjPlot::sjtab(data=plhdata_org_clean, Organisation, rp.contact.field.w_rules_welcome_individual_a_final, show.summary=FALSE, digits=0, fun="xtab", title="",string.total="Total")





# Show app user ids and whether they have completed selfcare.
plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have started selfcare.
plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.w_self_care_started")

# Show app user ids and whether they have started One-on-One time
plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.w_1on1_started")




#####Completion rate of introductory session(Workshop 1:Selfcare)####
# Show the summary of self care completion
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.w_self_care_completed, out="txt")

# Show app user ids and whether they have completed selfcare.
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have started selfcare.
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.w_self_care_started")

# Show app user ids and whether they have started one-on-one time
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.w_1on1_started")





#TODO
#####Completion status of baseline survey####

# Show the summary of baseline survey completion(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_completed, out="txt")

# Show app user ids and whether they have completed the baseline survey.
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.survey_welcome_completed")

# Show the summary of baseline survey completion(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_completed, out="txt")

# Show app user ids and whether they have completed the baseline survey
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.survey_welcome_completed")


# Show the summary of baseline survey completion(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_completed, out="txt")

# Show app user ids and whether they have completed the baseline survey
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.survey_welcome_completed")

# Show the summary of baseline survey completion(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_completed, out="txt")

# Show app user ids and whether they have completed the baseline survey
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.survey_welcome_completed")



####Response to each survey question (respond, skip, exit)#####

# Days attention past week
# Show the summary of survey_welcome_a_1(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_1
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_1_final")


# Show the summary of survey_welcome_a_1(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_1_final, out="txt")

# Show app user ids and their responses to  survey_welcome_a_1
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_1_final")

# Show the summary of survey_welcome_a_1(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_1
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_1_final")

# Show the summary of survey_welcome_a_1(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_1
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_1_final")



# Days of praise past week
# Show the summary of survey_welcome_a_2_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_2_final
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_2_final")

# Show the summary of survey_welcome_a_2_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_2_final
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_2_final")

# Show the summary of survey_welcome_a_2_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_2_final, out="txt")

# Show app user ids and their responses to survey_final_a_2_final
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_2_final")

# Show the summary of survey_welcome_a_2_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_2_final
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_2_final")


#Days of stress past week
# Show the summary of survey_welcome_a_3_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_3_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_3_final
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.survey_final_a_3_final")

# Show the summary of survey_welcome_a_3_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_3_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_3_final
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_3_final")

# Show the summary of survey_welcome_a_3_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_3_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_3_final
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.survey_final_a_3_final")

# Show the summary of survey_welcome_a_3_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_3_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_3_final
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_3_final")



#Days of shouting past week
# Show the summary of survey_welcome_a_4_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_4_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_4_final
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_4_final")

# Show the summary of survey_welcome_a_4_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_4_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_4_final
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_4_final")

# Show the summary of survey_welcome_a_4_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_4_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_4_final
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_4_final")

# Show the summary of survey_welcome_a_4_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCRESTrp.contact.field.survey_welcome_a_4_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_4_final
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_4_final")


#Days of money worry past week
# Show the summary of survey_welcome_a_5_part_1_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_5_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_1_final
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_4_final")

# Show the summary of survey_welcome_a_5_part_1_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_4_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_1_final
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_5_part_1_final")

# Show the summary of survey_welcome_a_5_part_1_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_5_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_1_final
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_5_part_1_final")

# Show the summary of survey_welcome_a_5_part_1_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_5_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_1_final
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_5_part_1_final")


#Days out of money for food past month 
# Show the summary of survey_welcome_a_5_part_2_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_5_part_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_2_final
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_5_part_2_final")

# Show the summary of survey_welcome_a_5_part_2_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_5_part_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_2_final
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_5_part_2_final")

# Show the summary of survey_welcome_a_5_part_2_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_5_part_2_final, out="txt")

# Show app user ids and their responses survey_welcome_a_5_part_2_final
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_5_part_2_final")

# Show the summary of survey_welcome_a_5_part_2_final
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_5_part_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_2_final
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_5_part_2_final")


#Days of hitting past week
# Show the summary of survey_welcome_a_6_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_6_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_6_final
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_6_final")

# Show the summary of survey_welcome_a_6_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_6_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_6_final
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_6_final")

# Show the summary of survey_welcome_a_6_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_6_final, out="txt")

# Show app user ids and their survey_welcome_a_6_final
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_6_final")

# Show the summary of survey_welcome_a_6_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_6_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_6_final
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_6_final")


# Knowledge of teen activity past week
# Show the summary of survey_welcome_a_7_part_1_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_7_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_1_final
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_7_part_1_final")

# Show the summary of survey_welcome_a_7_part_1_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_7_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_1_final
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_7_part_1_final")

# Show the summary of survey_welcome_a_7_part_1_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_7_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_1_final
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_7_part_1_final")

# Show the summary of survey_welcome_a_7_part_1_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_7_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_1_final
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_7_part_1_final")



# iff "7" to 7.1: Lockdown? yes/no
# Show the summary of survey_welcome_a_7_part_2_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_7_part_2_final, out="txt")

# Show app user ids and their responses to rp.contact.field.survey_welcome_a_7_part_2_final
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_7_part_2_final")

# Show the summary of survey_welcome_a_7_part_2_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_7_part_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_2_final
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_7_part_2_final")

# Show the summary of survey_welcome_a_7_part_2_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_7_part_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_2_final
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_7_part_2_final")

# Show the summary osurvey_welcome_a_7_part_2_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_7_part_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_2_final
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_7_part_2_final")


#iff "7" to 7.1: Knowledge of teen activity in non-lockdown week
# Show the summary of survey_welcome_a_7_part_3_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_7_part_3_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_3_final
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_7_part_3_final")

# Show the summary of survey_welcome_a_7_part_3_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_7_part_3_final, out="txt")

# Show app user ids and their responses torp.contact.field.survey_welcome_a_7_part_3_final
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_7_part_3_final")

# Show the summary of rp.contact.field.survey_welcome_a_7_part_3_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_7_part_3_final, out="txt")

# Show app user ids and their responses to rp.contact.field.survey_welcome_a_7_part_3_final
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_7_part_3_final")

# Show the summary of rp.contact.field.survey_welcome_a_7_part_3_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_7_part_2_final, out="txt")

# Show app user ids and their responses to rp.contact.field.survey_welcome_a_7_part_3_final
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_7_part_3_final")


#Days of sexual safety talk past month
# Show the summary of survey_welcome_a_8_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_8_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_8_final
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_8_final")

# Show the summary of survey_welcome_a_8_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_8_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_8_final
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_8_final")

# Show the summary of survey_welcome_a_8_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_8_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_8_final
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_8_final")

# Show the summary of survey_welcome_a_8_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_8_final, out="txt")

# Show app user ids and their responses survey_welcome_a_8_final
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_8_final")


#Days of teen COVID safe past week
# Show the summary of survey_welcome_a_9_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_9_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_9_final
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_9_final")

# Show the summary of survey_welcome_a_9_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_9_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_9_final
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_9_final")

# Show the summary of survey_welcome_a_9_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_9_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_9_final
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_9_final")

# Show the summary of survey_welcome_a_9_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_9_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_9_final
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.survey_welcome_a_9_final")


##Descriptive Statistics##
######Gender of App Users#########

# Show the summary of user gender(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.user_gender, out="txt")

# Show app user ids and their user gender
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.user_gender")

# Show the summary of user_gender(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.user_gender, out="txt")

# Show app user ids and user_gender
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.user_gender")

# Show the summary of user_gender (DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.user_gender, out="txt")

# Show app user ids their user_gender
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.user_gender")

# Show the summary of user_gender(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.user_gender, out="txt")

# Show app user ids and their user_gender
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.user_gender")


###Age of App Users####

# Show the summary of user age(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.user_age, out="txt")

# Show app user ids and their user age
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.user_age")

# Show the summary of user age(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.user_age, out="txt")

# Show app user ids and user age
plhdata_org_JOY %>%  select('app_user_id', "rp.contact.field.user_age")

# Show the summary of user age (DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.user_age, out="txt")

# Show app user ids their user age
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.user_age")

# Show the summary of user age(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.user_age, out="txt")

# Show app user ids and their user age
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.user_age")





#####Completion rate of introductory session(Workshop 1:Selfcare)####
# Show the summary of self care completion
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.w_self_care_completed, out="txt")

# Show the summary of One on One completion
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.w_1on1_completed, out="txt")

# Show app user ids and whether they have completed selfcare workshop
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have completed One on One workshop
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.w_1on1_completed")

# Show app user ids and whether they have started selfcare workshop
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.w_self_care_started")

# Show app user ids and whether they have started one on one time
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.w_1on1_started")

# Show app user ids and whether they have started 
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.w_1on1_started")

# Show app user ids and whether they have completed  workshop
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have completed Praise workshop
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.w_praise_completed")

# Show app user ids and whether they have completed Positive Instructions workshop
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.w_instruct_completed")

# Show app user ids and whether they have completed 5 workshops------------------
plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.w_self_care_completed", "rp.contact.field.w_1on1_completed", "rp.contact.field.w_praise_completed", "rp.contact.field.w_instruct_completed", "rp.contact.field.w_stress_completed")


#####Completion rate of introductory session(Workshop 1:Selfcare)####
# Show the summary of self care completion
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.w_self_care_completed, out="txt")

# Show app user ids and whether they have completed selfcare.
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have started selfcare.
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.w_self_care_started")

## Show app user ids and whether they have started one-one-time
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.w_1on1_started")


#####Completion rate of introductory session(Workshop 1:Selfcare)####
# Show the summary of self care completion
sjmisc::frq(x=plhdata_org_Amathuba$rp.contact.field.w_self_care_completed, out="txt")

# Show app user ids and whether they have completed selfcare.
plhdata_org_Amathuba %>%  select('app_user_id', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have started selfcare.
plhdata_org_Amathuba %>%  select('app_user_id', "rp.contact.field.w_self_care_started")

## Show app user ids and whether they have started one-one-time
plhdata_org_Amathuba %>%  select('app_user_id', "rp.contact.field.w_1on1_started")

# Show app user ids and whether they have completed 5 workshops--------------
plhdata_org_Amathuba %>%  select('app_user_id', "rp.contact.field.w_self_care_completed", "rp.contact.field.w_1on1_completed", "rp.contact.field.w_praise_completed", "rp.contact.field.w_instruct_completed", "rp.contact.field.w_stress_completed")


#####Completion rate of introductory session(Workshop 1:Selfcare)####
# Show the summary of self care completion
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.w_self_care_completed, out="txt")

# Show app user ids and whether they have completed selfcare.
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have started selfcare.
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.w_self_care_started")

## Show app user ids and whether they have started one-one-time
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.w_1on1_started")

# Show app user ids and whether they have completed 5 workshops------------------
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.w_self_care_completed", "rp.contact.field.w_1on1_completed", "rp.contact.field.w_praise_completed", "rp.contact.field.w_instruct_completed", "rp.contact.field.w_stress_completed")

#####Completion rate of introductory session(Workshop 1:Selfcare)####
# Show the summary of self care completion
sjmisc::frq(x=plhdata_org_Joy$rp.contact.field.w_self_care_completed, out="txt")

# Show app user ids and whether they have completed selfcare.
plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have started selfcare.
plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.w_self_care_started")

## Show app user ids and whether they have started one-one-time
plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.w_1on1_started")





#Group 3: Highly engaged,completed >50 of first five workshops--------------
#Joy
temp <- plhdata_org %>%  select('app_user_id',  'Organisation', "rp.contact.field.w_self_care_completion_level", 
                            "rp.contact.field.w_1on1_completion_level", "rp.contact.field.w_praise_completion_level",
                            "rp.contact.field.w_instruct_completion_level", "rp.contact.field.w_stress_completion_level") %>%
  rowwise() %>%
  mutate(conditional = ifelse(mean(c(rp.contact.field.w_self_care_completion_level, rp.contact.field.w_praise_completion_level,
                                     rp.contact.field.w_1on1_completion_level, rp.contact.field.w_stress_completion_level,
                                     rp.contact.field.w_instruct_completion_level))> 50,
                              1,
                              0)) %>% # creates a new column in the data set
  filter(conditional == 1)

#Group 2b: completed >50 of first 3 workshops

plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.w_self_care_completion_level", "rp.contact.field.w_praise_completion_level" ,
                            "rp.contact.field.w_1on1_completion_level")%>%
  mutate(conditional = ifelse(rp.contact.field.w_self_care_completion_level > 50 & 
                                rp.contact.field.w_1on1_completion_level> 50 & rp.contact.field.w_praise_completion_level >50,
                              1,
                              0)) %>% # creates a new column in the data set
  filter(conditional == 1)

#Alternative trial
#Group 2b: completed >50 of first 3 workshops
plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.w_self_care_completion_level", 
                            "rp.contact.field.w_1on1_completion_level", "rp.contact.field.w_praise_completion_level",
                            "rp.contact.field.w_instruct_completion_level", "rp.contact.field.w_stress_completion_level")%>%
  mutate(conditional = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50  &
                                rp.contact.field.w_1on1_completion_level> 50 & rp.contact.field.w_stress_completion_level< 1 &
                                rp.contact.field.w_instruct_completion_level < 1,
                              1,
                              0)) %>% # creates a new column in the data set
  filter(conditional == 1 )



#Group 2a: completed >50 of first workshop

plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.w_self_care_completion_level" 
                           )%>%
  mutate(conditional = ifelse(rp.contact.field.w_self_care_completion_level > 50,  
                              1,
                              0)) %>% # creates a new column in the data set
  filter(conditional == 1)

#Alternative Trial
#Group 2a: completed >50 of first workshop
plhdata_org_Amathuba %>%  select('app_user_id', "rp.contact.field.w_self_care_completion_level", 
                                 "rp.contact.field.w_1on1_completion_level", "rp.contact.field.w_praise_completion_level",
                                 "rp.contact.field.w_instruct_completion_level", "rp.contact.field.w_stress_completion_level")%>%
  mutate(conditional = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level < 50  &
                                rp.contact.field.w_1on1_completion_level < 50 & rp.contact.field.w_stress_completion_level< 50 &
                                rp.contact.field.w_instruct_completion_level < 50,
                              1,
                              0)) %>% # creates a new column in the data set
  filter(conditional == 1 )

#Trial 
#Group 2b: completed >50 of first 3 workshops
plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.w_self_care_completion_level", 
                            "rp.contact.field.w_1on1_completion_level", "rp.contact.field.w_praise_completion_level",
                            "rp.contact.field.w_instruct_completion_level", "rp.contact.field.w_stress_completion_level")%>%
  mutate(conditional = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level > 50  &
                                rp.contact.field.w_1on1_completion_level> 50 & rp.contact.field.w_stress_completion_level< 1 &
                                rp.contact.field.w_instruct_completion_level < 1,
                              1,
                              0)) %>% # creates a new column in the data set
  filter(conditional == 1 )


#Group 2a: completed >50 of first workshop
plhdata_org_Amathuba %>%  select('app_user_id', "rp.contact.field.w_self_care_completion_level", 
                                 "rp.contact.field.w_1on1_completion_level", "rp.contact.field.w_praise_completion_level",
                                 "rp.contact.field.w_instruct_completion_level", "rp.contact.field.w_stress_completion_level")%>%
  mutate(conditional = ifelse(rp.contact.field.w_self_care_completion_level > 50 & rp.contact.field.w_praise_completion_level < 50  &
                                rp.contact.field.w_1on1_completion_level < 50 & rp.contact.field.w_stress_completion_level< 50 &
                                rp.contact.field.w_instruct_completion_level < 50,
                              1,
                              0)) %>% # creates a new column in the data set
  filter(conditional == 1 )









#Trials-----------------
plhdata_org_clean%>% select('app_user_id', "rp.contact.field.user_age")
plhdata_org_clean%>% select('app_user_id', "rp.contact.field.user_gender")
plhdata_org_Amathuba%>% select('app_user_id', "rp.contact.field.user_gender")
mean(x=as.numeric(plhdata_org_clean$rp.contact.field.user_age), na.rm=TRUE)
