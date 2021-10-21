##-- reshape json file -- #####

# run libraries
library(tidyverse)
UIC.Tracker <- rio::import(file="Data/UIC Tracker.xlsx", which="UIC Tracker 211014")

#Get the List of PLH Tables
plh_tables <- dbListTables(plh_con)
df <- dbReadTable(plh_con, plh_tables[2])

# create empty list to store the data frames
appdata_df <- list()

# looping through a column 
for (i in 1:nrow(df)) {
  appdata_df[[i]] <- data.frame(jsonlite::fromJSON(df$contact_fields[i],  flatten = TRUE))
}

# combine the list into a data frame 
appdata <- plyr::ldply(appdata_df)

#merge the two dataframes
plhdata <- bind_cols(df,appdata)

#merge with the uic tracker data 
plhdata_org <- dplyr::full_join(x=plhdata, y=UIC.Tracker, by=c("app_user_id"="UIC"))

#'This could be a fuzzy join'
plhdata_org_fuzzy <- fuzzyjoin::stringdist_full_join(x=plhdata, y=UIC.Tracker, by=c("app_user_id"="UIC"))

#check the fuzzy matches 
plhdata_org_fuzzy_comp <- plhdata_org_fuzzy %>% 
  filter(!is.na(plhdata_org_fuzzy$UIC)) %>% 
  filter(app_user_id!=UIC) %>% 
  select(app_user_id, UIC)
View(plhdata_org_fuzzy_comp)

#Accept the fuzzy matches
plhdata_org<- plhdata_org_fuzzy

# View it all
# View(plhdata_org)

# save data as a csv file
#write.csv(plhdata_org, 'plhdata_org.csv')

# look at and convert Organisation and rp.contact.field.organisation_code to factor after replacing missing values by Miss so that it is a factor level
sjmisc::frq(x=plhdata_org$Organisation, out="txt")
plhdata_org$Organisation<-as_factor(replace_na(plhdata_org$Organisation, "Miss"))
sjmisc::frq(x=plhdata_org$rp.contact.field.organisation_code, out="txt")
plhdata_org$rp.contact.field.organisation_code<-as_factor(replace_na(plhdata_org$rp.contact.field.organisation_code, "Miss"))

# Combine Factors Organisation and rp.contact.field.organisation_code 
plhdata_org$organisation_full <- interaction(x=list(plhdata_org$Organisation,plhdata_org$rp.contact.field.organisation_code), drop=TRUE)

# look and Recode Factor organisation_full to just the main levels
sjmisc::frq(x=plhdata_org$organisation_full, out="txt")
plhdata_org$Org <- plyr::revalue(x=plhdata_org$organisation_full, replace=c(`Miss.Miss` =  "Miss", `Nontobeko.Miss` = "Nontobeko", `Joy.Miss` = "Joy", `Dlalanathi.Miss` = "Dlalanathi", `Miss.baba` = "Other", `Miss.` = "Other", `Miss.w` = "Other", `Miss.idems` = "Other", `Miss.Hillcrest facilitator` = "Hillcrest", `Miss.hillcrest` = "Hillcrest", `Miss.aqujhk,jafvh` = "Other", `Miss.ParentApp_dev` = "Other", `Miss.CWBSA` = "Other", `Dlalanathi.null` = "Dlalanathi", `Nontobeko.Nontobeko M` = "Nontobeko", `Nontobeko.bbe9ca70c78f7384` = "Nontobeko", `Miss.idems Margherita` = "Other", `Miss.NontobekoM` = "Nontobeko", `Nontobeko.NontobekoM` = "Nontobeko", `Miss.dlanathiThandeka` = "Dlalanathi", `Miss.dlalanathiThandeka` = "Dlalanathi", `Miss.dlalanathi` = "Dlalanathi", `Miss.Hillcrest Facilitator ` = "Hillcrest", `Miss.Hillcrest Facilitator` = "Hillcrest", `Miss.IDEMS Ohad` = "Other", `Nontobeko.nontobekoM` = "Nontobeko", `Miss.dlalanathThandeka`= "Dlalanathi", `Miss.dlalanithi Thandeka`="Dlalanathi",`Miss.Research team `="Other",`Miss.983aba50330cf24c` ="Other", `Miss.sdfds`="Other"))

# Look at the organisation data
sjmisc::frq(x=plhdata_org$Org, out="txt")

#Source the personal setup
#Manuel step source("Personal Setup.R")

#Read in the tracker file

#####Create a subset for cleaned organisations ####
#plhdata_org_clean<-filter(plhdata_org,Org!="Miss")
plhdata_org_clean<-filter(plhdata_org,Org!="Other")

# Create subsets of the data based on valid app user ID's
plhdata_org_clean <- filter(plhdata_org_clean, !is.na(plhdata_org_clean$app_version))

# Create subsets of the data based on valid app version
#plhdata_org_clean <- filter(plhdata_org_clean, 'App Version'== "0.11.3" || 'App Version'== "0.11.2")

#write.csv(plhdata_org_clean, 'plhdata_org_clean.csv')


# Show the summary of app versions
sjPlot::sjtab(data=plhdata_org_clean, Org, app_version, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of app versions, self care workshop started(1st Workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_self_care_started  , show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")


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


#####show the summary of total number of activities viewed out of total number of workshop activities (steppers)#####
#percentage corresponding to the proportion the proportion out of the 9 items of the stepper. For example, 4/9  =0.4444444 = 45.

#(Welcome and Self-Care Workshop)
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_self_care_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#One-on-One Time
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_1on1_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Praise
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_praise_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Positive Instructions
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_instruct_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Managing Stress
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_stress_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Family Budgets
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_money_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Rules
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_safe_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Calm Consequences
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_consequence_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Problem Solving
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_solve_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Teen Safety
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_safe_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Dealing with Crisis
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_crisis_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Celebration and Next Steps
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.w_celebrate_completion_level, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")



# Days attention past week
# Show the summary of survey_welcome_a_1_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_1_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Days of praise past week
# Show the summary of survey_welcome_a_2_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_2_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Days of stress past week
# Show the summary of survey_welcome_a_3_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_3_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Days of shouting past week
# Show the summary of survey_welcome_a_4_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_4_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Days of money worry past week
# Show the summary of survey_welcome_a_5_part_1_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_5_part_1_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Show the summary of survey_welcome_a_5_part_2_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_5_part_2_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Days of hitting past week
# Show the summary of survey_welcome_a_6_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_6_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# Knowledge of teen activity past week
# Show the summary of survey_welcome_a_7_part_1_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_7_part_1_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

# iff "7" to 7.1: Lockdown? yes/no
# Show the summary of survey_welcome_a_7_part_2_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_7_part_2_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#iff "7" to 7.1: Knowledge of teen activity in non-lockdown week
# Show the summary of survey_welcome_a_7_part_3_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_7_part_3_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Days of sexual safety talk past month
# Show the summary of survey_welcome_a_8_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_8_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")

#Days of teen COVID safe past week
# Show the summary of survey_welcome_a_9_final
sjPlot::sjtab(data=plhdata_org_clean, Org, rp.contact.field.survey_welcome_a_9_final, show.summary=FALSE, digits=0, fun="xtab", title="", string.total="Total")



#####Create a subset for an organisation Joy####

# Create subsets of the data based on organisation Joy
plhdata_org_Joy <- filter(plhdata_org, Org == "Joy")

# Show any app user ids that are invalid and do not come from the app.
plhdata_org_Joy %>% filter(is.na(plhdata_org_Joy$'App Version')) %>% select('App User ID')

# Create subsets of the data based on valid Joy app user ID's
plhdata_org_Joy <- filter(plhdata_org_Joy, !is.na(plhdata_org_Joy$'App Version'))

# Show the summary of app versions
sjmisc::frq(x=plhdata_org_Joy$'App Version', out="txt")

#####Completion rate of introductory session(Workshop 1:Selfcare)####
# Show the summary of self care completion
sjmisc::frq(x=plhdata_org_Joy$rp.contact.field.w_self_care_completed, out="txt")

# Show app user ids and whether they have completed selfcare.
plhdata_org_Joy %>%  select('App User ID', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have started selfcare.
plhdata_org_Joy %>%  select('App User ID', "rp.contact.field.w_self_care_started")

# Show app user ids and whether they have started One-on-One time
plhdata_org_Joy %>%  select('App User ID', "rp.contact.field.w_1on1_started")



#####Create a subset for an organisation NONTOBEKO####

# Create subsets of the data based on organisation NONTOBEKO
plhdata_org_NONTOBEKO <- filter(plhdata_org, Org == "Nontobeko")

# Show any app user ids that are invalid and do not come from the app.
plhdata_org_NONTOBEKO %>% filter(is.na(plhdata_org_NONTOBEKO$'App Version')) %>% select('App User ID')

# Create subsets of the data based on valid NONTOBEKOM app user ID's
plhdata_org_NONTOBEKO <- filter(plhdata_org_NONTOBEKO, !is.na(plhdata_org_NONTOBEKO$'App Version'))

# Show the summary of app versions
sjmisc::frq(x=plhdata_org_NONTOBEKO$'App Version', out="txt")

#####Completion rate of introductory session(Workshop 1:Selfcare)####
# Show the summary of self care completion
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.w_self_care_completed, out="txt")

# Show app user ids and whether they have completed selfcare.
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have completed selfcare.
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.w_self_care_started")

# Show app user ids and whether they have started selfcare.
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.w_1on1_started")





#TODO
#####Completion status of baseline survey####

# Show the summary of baseline survey completion(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_completed, out="txt")

# Show app user ids and whether they have completed the baseline survey.
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.survey_welcome_completed")

# Show the summary of baseline survey completion(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_completed, out="txt")

# Show app user ids and whether they have completed the baseline survey
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.survey_welcome_completed")


# Show the summary of baseline survey completion(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_completed, out="txt")

# Show app user ids and whether they have completed the baseline survey
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.survey_welcome_completed")

# Show the summary of baseline survey completion(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_completed, out="txt")

# Show app user ids and whether they have completed the baseline survey
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.survey_welcome_completed")



####Response to each survey question (respond, skip, exit)#####

# Days attention past week
# Show the summary of survey_welcome_a_1(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_1
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.survey_welcome_a_1_final")


# Show the summary of survey_welcome_a_1(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_1_final, out="txt")

# Show app user ids and their responses to  survey_welcome_a_1
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.survey_welcome_a_1_final")

# Show the summary of survey_welcome_a_1(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_1
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.survey_welcome_a_1_final")

# Show the summary of survey_welcome_a_1(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_1
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.survey_welcome_a_1_final")



# Days of praise past week
# Show the summary of survey_welcome_a_2_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_2_final
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.survey_welcome_a_2_final")

# Show the summary of survey_welcome_a_2_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_2_final
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.survey_welcome_a_2_final")

# Show the summary of survey_welcome_a_2_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_2_final, out="txt")

# Show app user ids and their responses to survey_final_a_2_final
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.survey_welcome_a_2_final")

# Show the summary of survey_welcome_a_2_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_2_final
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.survey_welcome_a_2_final")


#Days of stress past week
# Show the summary of survey_welcome_a_3_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_3_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_3_final
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.survey_final_a_3_final")

# Show the summary of survey_welcome_a_3_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_3_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_3_final
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.survey_welcome_a_3_final")

# Show the summary of survey_welcome_a_3_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_3_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_3_final
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.survey_final_a_3_final")

# Show the summary of survey_welcome_a_3_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_3_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_3_final
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.survey_welcome_a_3_final")



#Days of shouting past week
# Show the summary of survey_welcome_a_4_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_4_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_4_final
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.survey_welcome_a_4_final")

# Show the summary of survey_welcome_a_4_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_4_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_4_final
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.survey_welcome_a_4_final")

# Show the summary of survey_welcome_a_4_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_4_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_4_final
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.survey_welcome_a_4_final")

# Show the summary of survey_welcome_a_4_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCRESTrp.contact.field.survey_welcome_a_4_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_4_final
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.survey_welcome_a_4_final")


#Days of money worry past week
# Show the summary of survey_welcome_a_5_part_1_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_5_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_1_final
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.survey_welcome_a_4_final")

# Show the summary of survey_welcome_a_5_part_1_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_4_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_1_final
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.survey_welcome_a_5_part_1_final")

# Show the summary of survey_welcome_a_5_part_1_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_5_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_1_final
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.survey_welcome_a_5_part_1_final")

# Show the summary of survey_welcome_a_5_part_1_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_5_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_1_final
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.survey_welcome_a_5_part_1_final")


#Days out of money for food past month 
# Show the summary of survey_welcome_a_5_part_2_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_5_part_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_2_final
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.survey_welcome_a_5_part_2_final")

# Show the summary of survey_welcome_a_5_part_2_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_5_part_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_2_final
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.survey_welcome_a_5_part_2_final")

# Show the summary of survey_welcome_a_5_part_2_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_5_part_2_final, out="txt")

# Show app user ids and their responses survey_welcome_a_5_part_2_final
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.survey_welcome_a_5_part_2_final")

# Show the summary of survey_welcome_a_5_part_2_final
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_5_part_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_5_part_2_final
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.survey_welcome_a_5_part_2_final")


#Days of hitting past week
# Show the summary of survey_welcome_a_6_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_6_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_6_final
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.survey_welcome_a_6_final")

# Show the summary of survey_welcome_a_6_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_6_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_6_final
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.survey_welcome_a_6_final")

# Show the summary of survey_welcome_a_6_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_6_final, out="txt")

# Show app user ids and their survey_welcome_a_6_final
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.survey_welcome_a_6_final")

# Show the summary of survey_welcome_a_6_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_6_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_6_final
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.survey_welcome_a_6_final")


# Knowledge of teen activity past week
# Show the summary of survey_welcome_a_7_part_1_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_7_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_1_final
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.survey_welcome_a_7_part_1_final")

# Show the summary of survey_welcome_a_7_part_1_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_7_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_1_final
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.survey_welcome_a_7_part_1_final")

# Show the summary of survey_welcome_a_7_part_1_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_7_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_1_final
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.survey_welcome_a_7_part_1_final")

# Show the summary of survey_welcome_a_7_part_1_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_7_part_1_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_1_final
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.survey_welcome_a_7_part_1_final")



# iff "7" to 7.1: Lockdown? yes/no
# Show the summary of survey_welcome_a_7_part_2_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_7_part_2_final, out="txt")

# Show app user ids and their responses to rp.contact.field.survey_welcome_a_7_part_2_final
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.survey_welcome_a_7_part_2_final")

# Show the summary of survey_welcome_a_7_part_2_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_7_part_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_2_final
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.survey_welcome_a_7_part_2_final")

# Show the summary of survey_welcome_a_7_part_2_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_7_part_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_2_final
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.survey_welcome_a_7_part_2_final")

# Show the summary osurvey_welcome_a_7_part_2_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_7_part_2_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_2_final
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.survey_welcome_a_7_part_2_final")


#iff "7" to 7.1: Knowledge of teen activity in non-lockdown week
# Show the summary of survey_welcome_a_7_part_3_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_7_part_3_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_7_part_3_final
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.survey_welcome_a_7_part_3_final")

# Show the summary of survey_welcome_a_7_part_3_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_7_part_3_final, out="txt")

# Show app user ids and their responses torp.contact.field.survey_welcome_a_7_part_3_final
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.survey_welcome_a_7_part_3_final")

# Show the summary of rp.contact.field.survey_welcome_a_7_part_3_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_7_part_3_final, out="txt")

# Show app user ids and their responses to rp.contact.field.survey_welcome_a_7_part_3_final
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.survey_welcome_a_7_part_3_final")

# Show the summary of rp.contact.field.survey_welcome_a_7_part_3_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_7_part_2_final, out="txt")

# Show app user ids and their responses to rp.contact.field.survey_welcome_a_7_part_3_final
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.survey_welcome_a_7_part_3_final")


#Days of sexual safety talk past month
# Show the summary of survey_welcome_a_8_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_8_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_8_final
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.survey_welcome_a_8_final")

# Show the summary of survey_welcome_a_8_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_8_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_8_final
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.survey_welcome_a_8_final")

# Show the summary of survey_welcome_a_8_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_8_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_8_final
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.survey_welcome_a_8_final")

# Show the summary of survey_welcome_a_8_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_8_final, out="txt")

# Show app user ids and their responses survey_welcome_a_8_final
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.survey_welcome_a_8_final")


#Days of teen COVID safe past week
# Show the summary of survey_welcome_a_9_final(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.survey_welcome_a_9_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_9_final
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.survey_welcome_a_9_final")

# Show the summary of survey_welcome_a_9_final(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.survey_welcome_a_9_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_9_final
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.survey_welcome_a_9_final")

# Show the summary of survey_welcome_a_9_final(DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.survey_welcome_a_9_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_9_final
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.survey_welcome_a_9_final")

# Show the summary of survey_welcome_a_9_final(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.survey_welcome_a_9_final, out="txt")

# Show app user ids and their responses to survey_welcome_a_9_final
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.survey_welcome_a_9_final")


##Descriptive Statistics##
######Gender of App Users#########

# Show the summary of user gender(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.user_gender, out="txt")

# Show app user ids and their user gender
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.user_gender")

# Show the summary of user_gender(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.user_gender, out="txt")

# Show app user ids and user_gender
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.user_gender")

# Show the summary of user_gender (DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.user_gender, out="txt")

# Show app user ids their user_gender
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.user_gender")

# Show the summary of user_gender(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.user_gender, out="txt")

# Show app user ids and their user_gender
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.user_gender")


###Age of App Users####

# Show the summary of user age(NONTEBEKOM)
sjmisc::frq(x=plhdata_org_NONTOBEKO$rp.contact.field.user_age, out="txt")

# Show app user ids and their user age
plhdata_org_NONTOBEKO %>%  select('App User ID', "rp.contact.field.user_age")

# Show the summary of user age(JOYN)
sjmisc::frq(x=plhdata_org_JOY$rp.contact.field.user_age, out="txt")

# Show app user ids and user age
plhdata_org_JOY %>%  select('App User ID', "rp.contact.field.user_age")

# Show the summary of user age (DLALANATHI)
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.user_age, out="txt")

# Show app user ids their user age
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.user_age")

# Show the summary of user age(HILLCREST)
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.user_age, out="txt")

# Show app user ids and their user age
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.user_age")



#####Create a subset for an organisation DLALANATHI####
# Create subsets of the data based on organisation DLALANATHI
plhdata_org_DLALANATHI <- filter(plhdata_org, Org == "Dlalanathi")

# Show any app user ids that are invalid and do not come from the app.
plhdata_org_DLALANATHI %>% filter(is.na(plhdata_org_DLALANATHI$'App Version')) %>% select('App User ID')

# Show any app user ids that are invalid and do not come from the app.
plhdata_org_DLALANATHI %>% select('App User ID', Organisation, 'App Version')

# Create subsets of the data based on valid DLALANATHI app user ID's
plhdata_org_DLALANATHI <- filter(plhdata_org_DLALANATHI, !is.na(plhdata_org_DLALANATHI$'App Version'))

# Show the summary of app versions
sjmisc::frq(x=plhdata_org_DLALANATHI$'App Version', out="txt")

#####Completion rate of introductory session(Workshop 1:Selfcare)####
# Show the summary of self care completion
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.w_self_care_completed, out="txt")

# Show the summary of One on One completion
sjmisc::frq(x=plhdata_org_DLALANATHI$rp.contact.field.w_1on1_completed, out="txt")

# Show app user ids and whether they have completed selfcare workshop
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have completed One on One workshop
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.w_1on1_completed")

# Show app user ids and whether they have started selfcare workshop
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.w_self_care_started")

# Show app user ids and whether they have started one on one time
plhdata_org_DLALANATHI %>%  select('App User ID', "rp.contact.field.w_1on1_started")


#####Create a subset for an organisation HILLCREST####

# Create subsets of the data based on organisation HILLCREST
plhdata_org_HILLCREST <- filter(plhdata_org, Org == "Hillcrest")

# Show any app user ids that are invalid and do not come from the app.
plhdata_org_HILLCREST %>% filter(is.na(plhdata_org_HILLCREST$'App Version')) %>% select('App User ID')

# Create subsets of the data based on valid HILLCREST app user ID's
plhdata_org_HILLCREST <- filter(plhdata_org_HILLCREST, !is.na(plhdata_org_HILLCREST$'App Version'))

# Show the summary of app versions
sjmisc::frq(x=plhdata_org_HILLCREST$'App Version', out="txt")

#####Completion rate of introductory session(Workshop 1:Selfcare)####
# Show the summary of self care completion
sjmisc::frq(x=plhdata_org_HILLCREST$rp.contact.field.w_self_care_completed, out="txt")

# Show app user ids and whether they have completed selfcare.
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.w_self_care_completed")

# Show app user ids and whether they have started selfcare.
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.w_self_care_started")

## Show app user ids and whether they have started one-one-time
plhdata_org_HILLCREST %>%  select('App User ID', "rp.contact.field.w_1on1_started")

