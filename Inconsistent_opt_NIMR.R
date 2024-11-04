library(tidyverse)

# Load opt data
optdata <- readxl::read_excel("optimisation_data_20230321.xlsx")

# Load screening survey gender info
#screening_gender <- readxl::read_excel("C:/Users/lclem/Downloads/optimisation_screening_gender_20230323.xlsx")

# Rename unique ID variable to match metabase variable
optdata <- optdata %>% dplyr::rename(app_user_id = `App user ID`)
screening_gender <- screening_gender %>% dplyr::rename(app_user_id = parentapp_code)

# checks
length(unique(screening_gender$app_user_id))
length(unique(optdata$app_user_id))

# Joining the two dataframes using the "app_user_id" variable
optdata_new <- inner_join(screening_gender, optdata, by = "app_user_id")

# The resulting dataframe "optdata_new" will only have app_user_id codes that are present in both optdata and screening_gender
# This procedure was only able to match 610 ids - two ids are missing

# Lets explore them ----------------------------------------------------------------
# which are in optdata and not in screening_gender 
optdata_id <- optdata %>%
  mutate(match = ifelse(app_user_id %in% screening_gender$app_user_id, 0, 1)) %>%
  filter(match == 1)
optdata_id$app_user_id
# 813ecf9e57ee9a3d
# 75bbbdc21741f155

# which are in screening_gender and not in  optdata
screening_gender_id <- screening_gender %>%
  mutate(match = ifelse(app_user_id %in% optdata$app_user_id, 0, 1)) %>%
  filter(match == 1)
screening_gender_id$app_user_id

# I'm not sure where some have come from - the only valid looking ones are
# 0ffbe037f056ccb8
# 852f0841bd8aa738
# 75bbdc21741f155

# so one of them matches quite clearly - 75bbbdc21741f155 from optdata with 75bbdc21741f155 from screening_gender
# let's fix that one
screening_gender <- screening_gender %>%
  mutate(app_user_id = ifelse(app_user_id == "75bbdc21741f155", "75bbbdc21741f155", app_user_id))
optdata_new <- inner_join(screening_gender, optdata, by = "app_user_id")
nrow(optdata_new) # got 611/612.

# Let's explore these final ones ----------------------------------
# 813ecf9e57ee9a3d in optdata
# 0ffbe037f056ccb8 in screening data
# 852f0841bd8aa738 in screening data
# which are in optdata and not in screening_gender 
optdata %>%
  mutate(match = ifelse(app_user_id %in% screening_gender$app_user_id, 0, 1)) %>%
  filter(match == 1)
# this individual in optdata did the initial survey, but not much else
# all modules have completion FALSE


# I then loaded in the whole data (snapshot, master) that we took on the 21st March 2023, and found that 
# these two IDs are in there.
# See data attached to email for data on them :)