
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
plhdata_org_fuzzy <- fuzzyjoin::stringdist_full_join(x=plhdata, y=UIC.Tracker, by=c("app_user_id"="UIC"), max_dist = 5)

#check the fuzzy matches 
plhdata_org_fuzzy_comp <- plhdata_org_fuzzy %>% 
  filter(!is.na(plhdata_org_fuzzy$UIC)) %>% 
  filter(app_user_id!=UIC | is.na(app_user_id)) %>% 
  select(app_user_id, UIC)
View(plhdata_org_fuzzy_comp)

plhdata_org_no_match <- plhdata_org_fuzzy %>% 
  filter(!is.na(plhdata_org_fuzzy$UIC)) %>% 
  filter(is.na(app_user_id)) %>% 
  select(app_user_id, UIC)
View(plhdata_org_no_match)


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
plhdata_org$Org <- plyr::revalue(x=plhdata_org$organisation_full, replace=c(`Miss.Miss` =  "Other", `Nontobeko.Miss` = "Nontobeko", `Joy.Miss` = "Joy", `Dlalanathi.Miss` = "Dlalanathi", `Miss.baba` = "Other", `Miss.w` = "Other", `Miss.idems` = "Other",  `Miss.hillcrest` = "Other", `Miss.aqujhk,jafvh` = "Other", `Miss.ParentApp_dev` = "Other", `Miss.CWBSA` = "Other", `Dlalanathi.null` = "Dlalanathi", `Nontobeko.Nontobeko M` = "Nontobeko", `Nontobeko.bbe9ca70c78f7384` = "Nontobeko", `Miss.idems Margherita` = "Other",  `Nontobeko.NontobekoM` = "Nontobeko", `Miss.dlalanathiThandeka` = "Dlalanathi",  `Miss.IDEMS Ohad` = "Other", `Nontobeko.nontobekoM` = "Nontobeko",`Miss.Research team `="Other",`Miss.983aba50330cf24c` ="Other", `Miss.sdfds`="Other", `Joy.c9097349f34b364c` ="Joy", `Nontobeko.null` ="Nontobeko", `Joy.null` ="Joy", `Miss.friend` ="Other", `Miss.myself` ="Other", `Miss.undefined` ="Other", `Miss.other` ="Other", `Amathuba Collective.Miss` ="Amathuba", `Dlalanathi.dlanathiThandeka` ="Dlalanathi", `Dlalanathi.dlalanathThandeka` ="Dlalanathi", `Dlalanathi.dlalanathiThandeka` ="Dlalanathi", `Dlalanathi.dlalanathi` ="Dlalanathi", `Dlalanathi.dlalanithi Thandeka` ="Dlalanathi", `Miss.zlto` ="Other", `Miss.hpccc` ="Other", `Miss.Amathuba Mzi` ="Amathuba", `Miss.Amathuba Mzi ` ="Amathuba", `Miss.seven_passes` ="Other", `Miss.amathuba` ="Amathuba", `Miss.Hillcrest facilitator` ="Other", `Miss.Hillcrest Facilitator ` ="Other"))
                                                                            
# Look at the organisation data
sjmisc::frq(x=plhdata_org$Org, out="txt")

#Read in the tracker file

#####Create a subset for cleaned organisations ####
#plhdata_org_clean<-filter(plhdata_org,Org!="Miss")
plhdata_org_clean<-filter(plhdata_org,Org!="Other")

plhdata_org$Org <- factor(plhdata_org$Org, levels = unique(plhdata_org$Org))

# Create subsets of the data based on valid app user ID's
plhdata_org_clean <- filter(plhdata_org_clean, !is.na(plhdata_org_clean$app_version))

# Create subsets of the data based on valid app version
#plhdata_org_clean <- filter(plhdata_org_clean, 'App Version'== "0.11.3" || 'App Version'== "0.11.2")

#write.csv(plhdata_org_clean, 'plhdata_org_clean.csv')


# Look at the numbers per organisation from clear data 
sjmisc::frq(x=plhdata_org_clean$Org, out="txt")

nrow(filter(plhdata_org_clean,( 'App Version'== "0.11.3" | 'App Version'== "0.11.2")&('Org'=="Nontobeko")  & is.na(plhdata_org_clean$rp.contact.field.w_1on1_completion_level)))

