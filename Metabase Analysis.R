
### extract data ----------------------------------------------------------------------
df <- get_metabase_data()
plhdata_org <- get_user_data(merge_check = FALSE) # select 1 if you want to merge in changes (yes)

### clean data ----------------------------------------------------------------------

#plhdata_org1 <- plhdata_org
#plhdata_org <- plhdata_org1
## Tidy up "Organisation" Variable:
# replace missing values in Organisation and rp.contact.field.organisation_code by Miss so that it is a factor level
#sjmisc::frq(x=plhdata_org$Organisation, out="txt")
plhdata_org$Organisation <- forcats::as_factor(tidyr::replace_na(plhdata_org$Organisation, "Miss"))

#sjmisc::frq(x=plhdata_org$rp.contact.field.organisation_code, out="txt")
# Question: What about "null"?
plhdata_org$rp.contact.field.organisation_code <- forcats::as_factor(tidyr::replace_na(plhdata_org$rp.contact.field.organisation_code, "Miss"))

# look and Recode Factor organisation_full to just the main levels
#sjmisc::frq(x=plhdata_org$organisation_full, out="txt")


plhdata_org$rp.contact.field.organisation_code<-as_factor(replace_na(plhdata_org$rp.contact.field.organisation_code, "Miss"))

# Combine Factors Organisation and rp.contact.field.organisation_code 
plhdata_org$organisation_full <- interaction(x=list(plhdata_org$Organisation,plhdata_org$rp.contact.field.organisation_code), drop=TRUE)

# look and Recode Factor organisation_full to just the main levels
sjmisc::frq(x=plhdata_org$organisation_full, out="txt")
plhdata_org$Org <- plyr::revalue(x=plhdata_org$organisation_full, 
                                 replace=c(`Miss.Miss` =  "Other", `Miss.baba` = "Other", `Miss.w` = "Other", `Miss.idems` = "Other",  `Miss.hillcrest` = "Other", `Miss.aqujhk,jafvh` = "Other", `Miss.ParentApp_dev` = "Other", `Miss.CWBSA` = "Other",
                                           `Miss.idems Margherita` = "Other", `Miss.IDEMS Ohad` = "Other", `Miss.983aba50330cf24c` ="Other", `Miss.sdfds`="Other",  `Miss.friend` ="Other", `Miss.myself` ="Other", `Miss.undefined` ="Other",
                                           `Miss.other` ="Other", `Miss.zlto` ="Other", `Miss.hpccc` ="Other", `Miss.seven_passes` ="Other", `Miss.Hillcrest facilitator` ="Other", `Miss.Hillcrest Facilitator ` ="Other", `Miss.a00af0c3b3887330` ="Other",
                                           `Nontobeko.Miss` = "Nontobeko", `Nontobeko.Nontobeko M` = "Nontobeko", `Nontobeko.bbe9ca70c78f7384` = "Nontobeko",  `Nontobeko.NontobekoM` = "Nontobeko",
                                           `Nontobeko.NontobekoM` = "Nontobeko", `Nontobeko.null` ="Nontobeko", 
                                           `Joy.Miss` = "Joy", `Joy.c9097349f34b364c` ="Joy", `Joy.null` ="Joy",
                                           `Dlalanathi.Miss` = "Dlalanathi",  `Dlalanathi.null` = "Dlalanathi", `Miss.dlalanathiThandeka` = "Dlalanathi",  `Dlalanathi.dlanathiThandeka` ="Dlalanathi",
                                           `Dlalanathi.dlalanathThandeka` ="Dlalanathi", `Dlalanathi.dlalanathiThandeka` ="Dlalanathi", `Dlalanathi.dlalanathi` ="Dlalanathi", `Dlalanathi.dlalanithi Thandeka` ="Dlalanathi", 
                                           `Amathuba Collective.Miss` ="Amathuba", `Miss.Amathuba Mzi` ="Amathuba", `Miss.Amathuba Mzi ` ="Amathuba", `Miss.amathuba` ="Amathuba"))

# so do the Miss. to Other first:
#plhdata_org <- plhdata_org %>%
#  mutate(Org = ifelse(Organisation == "Miss", "Other",
#                      ifelse(Organisation == "Dlalanathi", "Dlalanathi",
#                             ifelse(rp.contact.field.organisation_code == "dlalanathiThandeka", "Dlalanathi",
#                                    ifelse(Organisation == "Nontobeko", "Nontobeko",
#                                           ifelse(Organisation == "Joy", "Joy",
#                                                  ifelse(Organisation == "Amathuba Collective", "Amathuba",
#                                                         ifelse(rp.contact.field.organisation_code == "Amathuba Mzi ", "Amathuba",
#                                                                ifelse(rp.contact.field.organisation_code == "Amathuba Mzi", "Amathuba",
#                                                                       ifelse(rp.contact.field.organisation_code == "amathuba", "Amathuba",
#                                                                              paste(Organisation, rp.contact.field.organisation_code, sep = ".")))))))))))

# Look at the organisation data
sjmisc::frq(x=plhdata_org$Org, out="txt")


#####Create a subset for cleaned organisations ####
# TODO: none are called Miss in "Org" due to how you defined it
plhdata_org_clean <- plhdata_org %>% filter(Org != "Miss")

plhdata_org_clean <- plhdata_org_clean %>%
  mutate(Org = factor(Org))

# Create subsets of the data based on valid app user ID's
plhdata_org_clean <- plhdata_org_clean %>%
  dplyr::filter(!is.na(app_version))

# Look at the numbers per organisation from clear data 
sjmisc::frq(x=plhdata_org_clean$Org, out="txt")


# Write clean data back -------------------------------------------------------


# Analysis - tables
plhdata_org_clean_2 <- plhdata_org_clean %>% filter(!is.na(app_version))

summary_PT(data = plhdata_org_clean_2, summary_var = app_version, denominator = Org, denominator_level = "Nontobeko", together = TRUE, naming_convention = FALSE)
summary_PT(data = plhdata_org_clean_2, summary_var = app_version, denominator = Org, denominator_level = "Dlalanathi", together = TRUE, naming_convention = FALSE)
summary_PT(data = plhdata_org_clean_2, summary_var = app_version, denominator = Org, denominator_level = "Joy", together = TRUE, naming_convention = FALSE)
summary_PT(data = plhdata_org_clean_2, summary_var = app_version, denominator = Org, denominator_level = "Nontobeko", together = TRUE, naming_convention = FALSE)

#summary_PT(data = plhdata_org_clean_2, summary_var = c(app_version, Org), naming_convention = TRUE)

#mmtable2::mmtable(x, cells = value) +
#  header_top(app_version) +
#  header_left(Org) +
#  header_left_top(name)

#get_app_user_IDs(data = plhdata_org_clean_2, Org, "Nontobeko", show_invalid = FALSE)






