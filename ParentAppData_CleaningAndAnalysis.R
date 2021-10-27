# run libraries
library(tidyverse)

#Source the personal setup for data
#Manuel step source("Personal Setup Data.R")

#Get data from excel

UIC.Tracker <- rio::import(file="Data/UIC Tracker.xlsx", which="UIC Tracker 211014")

#Get the List of PLH Tables and data from server
plh_tables <- dbListTables(plh_con)
df <- dbReadTable(plh_con, plh_tables[2])

#Source the personal setup for data
#Manuel step source("Personal Setup Script.R")

#Source the data cleaning and setup
source("ParentAppData_setupclean.R")

# Define list of organisations
orgs_list <- levels(plhdata_org_clean$Org)

# Show the summary of app versions

sjPlot::sjtab(data=plhdata_org_clean, Org, app_version, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of app versions, self care workshop started(1st Workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_self_care_started, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of app versions, self care workshop completion(1st Workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_self_care_completed  , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of app versions, One-on-One Time started(2nd Workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_1on1_started  , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of app versions, One-on-One Time completion(2nd Workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_1on1_completed , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of app versions, Praise(3rd Workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_praise_completed , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of app versions, baseline survey 
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_completed , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Show the summary of app versions,user gender
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.user_gender, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Show the summary of app versions,user age
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.user_age, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#show the summary of app versions, household adults
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.household_adults, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#show the summary of app versions, household teens
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.household_teens, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#show the summary of app versions, household babies
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.household_babies, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#show the summary of app versions, household children
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.household_children, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")


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
# 
# plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax")
# 
# plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax")
# 
# plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax")


#HABIT: Treat yourself well
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_treat_yourself, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_treat_yourself")

# Replaced by above
# plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_treat_yourself")
# 
# plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_treat_yourself")
# 
# plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_treat_yourself")
# 
# plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_treat_yourself")


#HABIT: Praise yourself
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_praise_yourself, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_praise_yourself")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_yourself")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_yourself")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_yourself")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_yourself")


#HABIT:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_spend_time, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_spend_time")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_spend_time")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_spend_time")

#lhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_spend_time")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_spend_time")


#HABIT:Praise your teen
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_praise_teen, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_praise_teen")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_teen")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_teen")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_teen")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_teen")


#HABIT:Get Positive
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_instruct_positively, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_instruct_positively")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_instruct_positively")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_instruct_positively")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_instruct_positively")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_instruct_positively")


#HABIT:Breathe not yell
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_breathe, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_breathe")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_breathe")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_breathe")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_breathe")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_breathe")


#HABIT:Good money choice
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_money, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_money")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_money")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_money")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_money")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_money")


#HABIT:Calm consequence
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_consequence, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_consequence")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_consequence")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_consequence")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_consequence")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_consequence")


#HABIT:Safe
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_safe, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_safe")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_safe")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_safe")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_safe")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_safe")






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

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_treat_yourself_w_self_care")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_treat_yourself_w_self_care")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_treat_yourself_w_self_care")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_treat_yourself_w_self_care")


#HABIT:Praise yourself well  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_praise_yourself_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_praise_yourself_w_self_care")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_yourself_w_self_care")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_yourself_w_self_care")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_yourself_w_self_care")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_yourself_w_self_care")


#HABIT:One-on-one time  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_spend_time_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_spend_time_w_self_care")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_spend_time_w_self_care")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_spend_time_w_self_care")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_spend_time_w_self_care")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_spend_time_w_self_care")


#HABIT:Praise your teen  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_praise_teen_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_praise_teen_w_self_care")

#Relaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_teen_w_self_care")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_teen_w_self_care")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_teen_w_self_care")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_teen_w_self_care")


#HABIT:Breathe not yell  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_breathe_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_breathe_w_self_care")

#Replace by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_breathe_w_self_care")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_breathe_w_self_care")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_breathe_w_self_care")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_breathe_w_self_care")


#HABIT:Good Money Choice  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_money_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_money_w_self_care")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_money_w_self_care")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_money_w_self_care")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_money_w_self_care")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_money_w_self_care")


#HABIT:Calm Consequence  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_consequence_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_consequence_w_self_care")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_consequence_w_self_care")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_consequence_w_self_care")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_consequence_w_self_care")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_consequence_w_self_care")

#HABIT:Safe  WORKSHOP:Self Care
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_safe_w_self_care, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_safe_w_self_care")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_safe_w_self_care")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_safe_w_self_care")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_safe_w_self_care")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_safe_w_self_care")



#HABIT:Relax  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_relax_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_relax_w_1on1")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax_w_1on1")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax_w_1on1")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax_w_1on1")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_relax_w_1on1")


#HABIT:Treat yourself well  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_treat_yourself_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_treat_yourself_w_1on1")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_treat_yourself_w_1on1")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_treat_yourself_w_self_w_1on1")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_treat_yourself_w_1on1")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_treat_yourself_w_1on1")


#HABIT:Praise yourself well  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_praise_yourself_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_treat_yourself_w_1on1")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_yourself_w_1on1")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_yourself_w_1on1")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_yourself_w_1on1")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_yourself_w_1on1")


#HABIT:One-on-one time  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_spend_time_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_spend_time_w_1on1")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_spend_time_w_1on1")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_spend_time_w_1on1")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_spend_time_w_1on1")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_spend_time_w_1on1")


#HABIT:Praise your teen  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_praise_teen_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_praise_teen_w_1on1")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_teen_w_1on1")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_teen_w_1on1")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_teen_w_1on1")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_praise_teen_w_1on1")


#HABIT:Breathe not yell  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_breathe_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_breathe_w_1on1")

#Repalced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_breathe_w_1on1")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_breathe_w_1on1")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_breathe_w_1on1")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_breathe_w_1on1")


#HABIT:Good Money Choice  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_money_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_money_w_1on1")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_money_w_1on1")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_money_w_1on1")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_money_w_1on1")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_money_w_1on1")


#HABIT:Calm Consequence  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_consequence_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_consequence_w_1on1")

#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_consequence_w_1on1")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_consequence_w_1on1")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_consequence_w_1on1")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_consequence_w_1on1")

#HABIT:Safe  WORKSHOP:One-on-one time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.parent_point_count_safe_w_1on1, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.parent_point_count_safe_w_1on1")


#Replaced by above
#plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.parent_point_count_safe_w_1on1")

#plhdata_org_DLALANATHI %>%  select('app_user_id', "rp.contact.field.parent_point_count_safe_w_1on1")

#plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.parent_point_count_safe_w_1on1")

#plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.parent_point_count_safe_w_1on1")



#####show the summary of total number of activities viewed out of total number of workshop activities (steppers)#####
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
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_1on1_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_1on1_completion_level")

#Praise
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_praise_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_praise_completion_level")

#Positive Instructions
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_instruct_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_instruct_completion_level")

#Managing Stress
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_stress_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_stress_completion_level")

#Family Budgets
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_money_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_money_completion_level")

#Rules
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_safe_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_safe_completion_level")

#Calm Consequences
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_consequence_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_consequence_completion_level")

#Problem Solving
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_solve_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_solve_completion_level")

#Teen Safety
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_safe_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_safe_completion_level")

#Dealing with Crisis
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_crisis_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
user_id_print("rp.contact.field.w_crisis_completion_level")

#Celebration and Next Steps
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
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.click_pc_esential_tools_count, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")
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
sjmisc::frq(x=plhdata_org$rp.contact.field.click_hs_weekly_workshops_quick_start, out="txt")

#####Create a subset for an organisation Joy####

# Create subsets of the data based on organisation Joy
plhdata_org_Joy <- filter(plhdata_org, Org == "Joy")

# Show any app user ids that are invalid and do not come from the app.
plhdata_org_Joy %>% filter(is.na(plhdata_org_Joy$'app_version')) %>% select('app_user_id')

# Create subsets of the data based on valid Joy app user ID's
plhdata_org_Joy <- filter(plhdata_org_Joy, !is.na(plhdata_org_Joy$'app_version'))

# Show the summary of app versions
sjmisc::frq(x=plhdata_org_Joy$'app_version', out="txt")

#####Completion rate of introductory session(Workshop 1:Selfcare)####
# Show the summary of self care completion
sjmisc::frq(x=plhdata_org_Joy$rp.contact.field.w_self_care_completed, out="txt")

# Show app user ids and whether they have completed selfcare.
plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have started selfcare.
plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.w_self_care_started")

# Show app user ids and whether they have started One-on-One time
plhdata_org_Joy %>%  select('app_user_id', "rp.contact.field.w_1on1_started")



#####Create a subset for an organisation NONTOBEKO####

# Create subsets of the data based on organisation NONTOBEKO
plhdata_org_NONTOBEKO <- filter(plhdata_org, Org == "Nontobeko")

# Show any app user ids that are invalid and do not come from the app.
plhdata_org_NONTOBEKO %>% filter(is.na(plhdata_org_NONTOBEKO$'app_version')) %>% select('app_user_id')

# Create subsets of the data based on valid NONTOBEKOM app user ID's
plhdata_org_NONTOBEKO <- filter(plhdata_org_NONTOBEKO, !is.na(plhdata_org_NONTOBEKO$'app_version'))

# Show the summary of app versions
sjmisc::frq(x=plhdata_org_NONTOBEKO$'app_version', out="txt")

#####Completion rate of introductory session(Workshop 1:Selfcare)####
# Show the summary of self care completion
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.w_self_care_completed, out="txt")

# Show app user ids and whether they have completed selfcare.
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have completed selfcare.
plhdata_org_NONTOBEKO %>%  select('app_user_id', "rp.contact.field.w_self_care_started")

# Show app user ids and whether they have started selfcare.
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



#####Create a subset for an organisation DLALANATHI####
# Create subsets of the data based on organisation DLALANATHI
plhdata_org_DLALANATHI <- filter(plhdata_org, Org == "Dlalanathi")

# Show any app user ids that are invalid and do not come from the app.
plhdata_org_DLALANATHI %>% filter(is.na(plhdata_org_DLALANATHI$'app_version')) %>% select('app_user_id')

# Show any app user ids that are invalid and do not come from the app.
plhdata_org_DLALANATHI %>% select('app_user_id', Organisation, 'app_version')

# Create subsets of the data based on valid DLALANATHI app user ID's
plhdata_org_DLALANATHI <- filter(plhdata_org_DLALANATHI, !is.na(plhdata_org_DLALANATHI$'app_version'))

# Show the summary of app versions
sjmisc::frq(x=plhdata_org_DLALANATHI$'app_version', out="txt")

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


#####Create a subset for an organisation HILLCREST####

# Create subsets of the data based on organisation HILLCREST
plhdata_org_HILLCREST <- filter(plhdata_org, Org == "Hillcrest")

# Show any app user ids that are invalid and do not come from the app.
plhdata_org_HILLCREST %>% filter(is.na(plhdata_org_HILLCREST$'app_version')) %>% select('app_user_id')

# Create subsets of the data based on valid HILLCREST app user ID's
plhdata_org_HILLCREST <- filter(plhdata_org_HILLCREST, !is.na(plhdata_org_HILLCREST$'app_version'))

# Show the summary of app versions
sjmisc::frq(x=plhdata_org_HILLCREST$'app_version', out="txt")

#####Completion rate of introductory session(Workshop 1:Selfcare)####
# Show the summary of self care completion
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.w_self_care_completed, out="txt")

# Show app user ids and whether they have completed selfcare.
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have started selfcare.
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.w_self_care_started")

## Show app user ids and whether they have started one-one-time
plhdata_org_HILLCREST %>%  select('app_user_id', "rp.contact.field.w_1on1_started")

