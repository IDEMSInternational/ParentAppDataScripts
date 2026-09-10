# token: a30e42045e15641b75a74ecf210c26b6
#https://apps-server.idems.international/analytics/index.php?date=2022-12-01,2023-03-21&expanded=1&filter_limit=-1&format=TSV&idSite=1&language=en&method=UserId.getUsers&module=API&period=range&segment=&token_auth=a30e42045e15641b75a74ecf210c26b6&translateColumnNames=1

#X <- read.csv(url("https://apps-server.idems.international/analytics/index.php?date=2022-12-01,2023-03-21&expanded=1&filter_limit=-1&format=TSV&idSite=1&language=en&method=UserId.getUsers&module=API&period=range&segment=&token_auth=a30e42045e15641b75a74ecf210c26b6&translateColumnNames=1"))

#' Visitors > User IDs > date range > import
#' 
#' UUID? Check.
#' Visits: If a vistor comes for the first time, or if they visit a page more than 30 minutes after their last page view, this is a new visit
#' Actions: The number of actions performed (page views, site searches, downloads, outlinks)
#' Actions per visit: average number of actions per visits
#' avg time on website: Average duration of a visit (seconds)
#' Bounces: number of visits that only had a single page view (so the visitor left the website directly from the entrance page)
#' Bounce proportion: proportion of visits that only had a single page view (so the visitor left the website directly from the entrance page)
#' Time on Website: total time spent by visitor (in seconds) over period. - note that I calculated this so it can be out by about 10 seconds.


#' Unique visitors (daily sum)
#' Users (daily sum)

#' Conversion rate: 

library(rjson)

#json_file <- "https://apps-server.idems.international/analytics/index.php?apiAction=getUsers&apiModule=UserId&date=2022-10-01,2023-03-22&expanded=1&filter_limit=-1&format=JSON&idSite=1&method=API.getProcessedReport&module=API&period=range&segment=&token_auth=a30e42045e15641b75a74ecf210c26b6"
json_file <- "https://apps-server.idems.international/analytics/index.php?apiAction=getUsers&apiModule=UserId&date=2023-09-27,2023-10-25&expanded=1&filter_limit=-1&format=JSON&idSite=1&method=API.getProcessedReport&module=API&period=range&segment=&token_auth=a30e42045e15641b75a74ecf210c26b6"
#json_file <- "https://apps-server.idems.international/analytics/index.php?apiAction=getUsers&apiModule=UserId&date=2022-03-31,2023-06-15&expanded=1&filter_limit=-1&format=JSON&idSite=1&method=API.getProcessedReport&module=API&period=range&segment=&token_auth=a30e42045e15641b75a74ecf210c26b6"
json_data <- jsonlite::fromJSON(txt=json_file, flatten = TRUE)

our_data <- json_data$reportData
names(our_data) <- c("UUID", "Visits", "Actions", "C", "D", "Actions per visit", "Avg. Time on Website", "Bounce Rate")
our_data <- our_data %>% dplyr::select(-c("C", "D"))
our_data$`Bounce proportion` <- as.numeric(as.character(stringr::str_split(our_data$`Bounce Rate`, "%", simplify = TRUE)[,1]))/100
our_data <- our_data %>% mutate(Bounce = round(`Bounce proportion` * Visits, 0)) %>% dplyr::select(-c("Bounce Rate"))
our_data$`Avg. Time on Website` <- period_to_seconds(hms(x = our_data$`Avg. Time on Website`, format = "%H:%M:%S"))[1:length(our_data$`Avg. Time on Website`)]
our_data$`Time on Website` <- our_data$`Avg. Time on Website` * our_data$Visits # this is calculated so can be out by 10 seconds or so

valid_uuid <- plhdata_org_clean$app_user_id
plhdata_org_clean_1 <- plhdata_org_clean %>% dplyr::select(c(UUID = app_user_id))
our_data <- our_data %>% filter(UUID %in% valid_uuid)

our_data <- full_join(plhdata_org_clean_1, our_data)

writexl::write_xlsx(our_data, "optimisation_matomo_data_20230322.xlsx")

# ce1d324f22d37fba, 7e7d8c6b7c38d442
# never launched app


#writexl::write_xlsx(our_data, path = "optimisation_matomo_20230321.xlsx")




check_uuid <- (plhdata_org_clean_1 %>%
                 mutate(match = ifelse(UUID %in% our_data$UUID, 0, 1)) %>%
                 filter(match == 1))$UUID
check_uuid



# device info



filter_nas <- plhdata_org_clean %>% filter(app_user_id %in% check_uuid)
appdata_df <- list()
for (i in 1:nrow(filter_nas)) {
  appdata_df[[i]] <- data.frame(jsonlite::fromJSON(filter_nas$device_info[i]))
}
appdata_df <- plyr::ldply(appdata_df)
other_names <- names(filter_nas[6:length(filter_nas)])
top_names <- names(filter_nas[1:5])
filter_nas <- bind_cols(filter_nas, appdata_df)
filter_nas <- filter_nas %>% dplyr::select(top_names, names(appdata_df), other_names)

filter_nas <- filter_nas[,colSums(is.na(filter_nas))<nrow(filter_nas)]

write.csv(filter_nas, "optimisation_11_not_in_matomo.csv")


# one user with value not equal to NA
