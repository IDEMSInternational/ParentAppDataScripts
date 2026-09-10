#' Yes, I’m thinking about all the:
#' “Take a Pause” and “Listen and Relax” and “Do and Relax” exercises
#' Not necessarily the other activities.
#' 
#' Simple data like:                                                   - got # clicks onto relax and activities page
#'  Whether the user has clicked on it or not, 
#'  How much time a user spends on the page of the exercise,
#'  If they listen to the entire voice note or not,
#'  The rate of completion                                             - got if they have completed it
#'  if any of that is possible.
#'  
#'  Also very open to other ideas!

# other name for our data
relax_link_data <- readxl::read_xlsx("data/relaxation_matomo_link.xlsx")

relax_comp <- NULL; relax_comp_name <- NULL; relax_link <- NULL
for (i in 1:24){
  relax_comp[i] <- paste0("rp.contact.field.relax_", i, "_completed")
  relax_comp_name[i] <- paste0("relax_", i)
} 
relax_link_id <- relax_link_data[1:12,]$id
for (i in relax_link_id) relax_link[i] <- paste0("rp.contact.field.task_gp_", i, "_relax_completed")


relax_comp_data <- plhdata_org_clean %>%
  dplyr::select(app_user_id,
                rp.contact.field.click_pc_relax_and_activities_count,
                all_of(relax_link),
                all_of(relax_comp))
names(relax_comp_data) <- c("ID", "relax_and_activities_count", relax_link_id, relax_comp_name)

for (i in relax_link_id){
  i_code <- relax_link_data[which(relax_link_data == i),]$relax_code
  i_id <- relax_link_data[which(relax_link_data == i),]$id
  
  relax_comp_data <- relax_comp_data %>% mutate(!!i_code := paste0(get(i_code), get(i_id))) %>%
    mutate(!!i_code := grepl("true", get(i_code), fixed = TRUE))
}
relax_comp_data <- relax_comp_data %>%
  dplyr::select(-c(relax_link_id)) %>%
  mutate(across(all_of(relax_comp_name), ~ifelse(.x %in% c("true", "TRUE"),
                                                 "TRUE",
                                                 ifelse(is.na(.x), "FALSE", .x))))
# rename variables
# match name of relax_comp_data with activity_name
#names(relax_comp_data) <- 
#relax_link_data <- relax_link_data %>% arrange(as.numeric(sub("relax_", "", relax_link_data$relax_code)))

relax_comp_data1 <- relax_comp_data

var_list <- match(names(relax_comp_data)[3:length(relax_comp_data)], relax_link_data$relax_code)
names(relax_comp_data)[3:length(relax_comp_data)] <- relax_link_data$activity_name[var_list]
#sanity_check <- data.frame(a = relax_comp_data1$relax_5, b = relax_comp_data$`In and out`)
#sanity_check %>% filter(a != b)

# Now to get Matomo data ---------------------------------------------
# Currently, for a set of dates: (must be a better way!)
rct_codes <- UIC_Tracker_RCT %>%
  dplyr::filter(Condition == "Intervention") %>%
  dplyr::pull(YourParentAppCode)
last_visits_id_date <- NULL
relax_link_name_get <- paste0(relax_link_id, "_relax")
relax_link_name_get <- stringr::str_c(relax_link_name_get, collapse = "|")
dates_from <- seq(from = as.Date("2023-05-01"), as.Date("2023-06-09"), by = 3) 
# TODO: in future, save until yesterday, and then bind with previously saved data!
# ATM I have not done. I saved to today, which means I only have half of today and I can't untangle that!
for (i in 1:length(dates_from)){
  j <- as.Date(dates_from[i]) + 2
  #json_file <- "https://apps-server.idems.international/analytics/index.php?date=2023-05-25,2023-05-31&expanded=1&filter_limit=-1&force_api_session=1&format=JSON&idSite=1&method=Live.getLastVisitsDetails&module=API&period=range&token_auth=a30e42045e15641b75a74ecf210c26b6"
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
        last_visits <- json_data[j,]$actionDetails[[1]]
        last_visits$url <- sub("\\?.*", "", last_visits$url)
        # filter to if it starts with activity_relax_ page, or "w_self_care_relax" etc etc. 
        last_visits_1 <- last_visits %>% filter(grepl("activity_relax_", url))
        last_visits_2 <- last_visits %>% filter(grepl(relax_link_name_get, url))
        last_visits <- dplyr::bind_rows(last_visits_1, last_visits_2)
        
        # TODO: would be quicker to do this bit before we bind_rows
        if ("timeSpent" %in% names(last_visits)){
          last_visits <- last_visits %>% dplyr::select(c(url, timeSpent))
        } else {
          last_visits <- last_visits %>% mutate(timeSpent = NA) %>% dplyr::select(c(url, timeSpent))
        }
        last_visits_id[[j]] <- last_visits
      }
      names(last_visits_id) <- json_data$userId
      last_visits_id_date[[i]] <- plyr::ldply(last_visits_id)
    }
    print(i)
  }
}
#last_visits_id_date1 <- last_visits_id_date
#last_visits_id_date

#last_visits_id_date1$url[1] <- "https://plh-tz.web.app/template/w_self_care_relax"
last_visits_id_date <- plyr::ldply(last_visits_id_date)#, .id = "date_set")
last_visits_id_date <- last_visits_id_date %>%
  dplyr::mutate(url = gsub(".*/", "", url)) %>%
  dplyr::mutate(url = gsub(".*activity_", "", url)) %>%
  dplyr::mutate(url = gsub("_relax", "", url))

# rename variables
relax_link_data_long <- relax_link_data %>% pivot_longer(cols = c(id, relax_code), values_to = "url")
last_visits_id_date <- full_join(relax_link_data_long %>% dplyr::select(url, activity_name), last_visits_id_date)

last_visits_id_date_summary <- last_visits_id_date %>%
  filter(complete.cases(last_visits_id_date)) %>%
  dplyr::group_by(activity_name, `.id`) %>%
  dplyr::mutate(timeSpent = as.numeric(timeSpent)) %>%
  dplyr::summarise(time_spent = sum(timeSpent, na.rm = TRUE),
                   page_count = n())

last_visits_id_date2 <- last_visits_id_date_summary %>% filter(`.id` %in% rct_codes)
###

writexl::write_xlsx(last_visits_id_date_summary, "RCT_relaxation_summaries_20230608.xlsx")
writexl::write_xlsx(relax_comp_data, "RCT_relaxation_completed_20230608.xlsx")


################################################################################################



# for just one day:
#json_file <- "https://apps-server.idems.international/analytics/index.php?date=yesterday&expanded=1&filter_limit=-1&force_api_session=1&format=JSON&idSite=1&method=Live.getLastVisitsDetails&module=API&period=day&token_auth=a30e42045e15641b75a74ecf210c26b6"

json_file <- "https://apps-server.idems.international/analytics/index.php?date=2023-05-29,2023-05-31&expanded=1&filter_limit=-1&force_api_session=1&format=JSON&idSite=1&method=Live.getLastVisitsDetails&module=API&period=range&token_auth=a30e42045e15641b75a74ecf210c26b6"
json_data <- jsonlite::fromJSON(txt=json_file, flatten = TRUE)
json_data <- json_data %>% filter(userId %in% (UIC_Tracker_RCT %>% filter(study == "RCT") %>% pull(YourParentAppCode)))

# for each row 
j = 1
last_visits_id <- NULL

for (j in 1:nrow(json_data)){
  last_visits <- json_data[j,]$actionDetails[[1]]
  last_visits$url <- sub("\\?.*", "", last_visits$url)
  last_visits <- last_visits %>% filter(grepl("activity_relax_", url))
  
  if ("timeSpent" %in% names(last_visits)){
    last_visits <- last_visits %>% dplyr::select(c(url, timeSpent))
  } else {
    last_visits <- last_visits %>% mutate(timeSpent = NA) %>% dplyr::select(c(url, timeSpent))
  }
  last_visits_id[[j]] <- last_visits
}
names(last_visits_id) <- json_data$userId
last_visits_id <- plyr::ldply(last_visits_id)

# count page counts per person, and total time on each of them. 
last_visits_id <- last_visits_id %>%
  dplyr::group_by(url, `.id`) %>%
  dplyr::mutate(timeSpent = as.numeric(timeSpent)) %>%
  dplyr::summarise(time_spent = sum(timeSpent, na.rm = TRUE),
                   page_count = n())


####





# then merge that with visited_pages_all and last_visits_id
# visited_pages_all <- dplyr::full_join(visited_pages, last_visits_id)
# names(visited_pages_all[[j]]) <- json_data$visitorId




# view number of times some pages were visited - can filter here to our ones we are interested in

# TODO: repeat to run for the list of IDs in the tracker
# 
# visited_pages_all <- NULL
# # FOR EACH PERSON (HERE FOR jth PERSON)
# visited_pages <- json_data$visitedPages
# visited_pages$url <- sub("\\?.*", "", visited_pages$url)
# visited_pages <- visited_pages %>% filter(grepl("activity_relax_", url))
# # or put as a list and
# last_visits_id <- NULL
# for (i in 1:length(json_data$lastVisits)){
#   last_visits <- json_data$lastVisits[i,]$actionDetails[[1]]
#   last_visits$url <- sub("\\?.*", "", last_visits$url)
#   last_visits <- last_visits %>% filter(grepl("activity_relax_", url))
#   last_visits <- last_visits %>% dplyr::select(c(url, timeSpent))
#   last_visits_id[[i]] <- last_visits
# }
# last_visits_id <- plyr::ldply(last_visits_id) %>%
#   dplyr::group_by(url) %>%
#   dplyr::summarise(time_spent = sum(timeSpent, na.rm = TRUE))
# # then merge that with visited_pages_all and last_visits_id
# visited_pages_all <- dplyr::full_join(visited_pages, last_visits_id)
# names(visited_pages_all[[j]]) <- json_data$visitorId




# go through all visitor IDs on this way:


# here we have url to filter
# and can look at time spent for the time spent in seconds
# URL does not change if they watch the video - but this will be in the time spend in seconds
# "&popup_child=relax_1_text&popup_parent=content_box&popup_parent_triggered_by=button_1" - if they click on for script.

#
# c995f0e3bb363a0e is my ID
# look for  type action
# activity_relax_1 in URL
# can count # times it occurs
# can see timeSpent in seconds
