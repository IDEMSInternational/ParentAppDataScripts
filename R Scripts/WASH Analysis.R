### FUNCTIONS
# get names
get_var_names <- function(data, field){
  var_names <- names(data %>% dplyr::select(starts_with(paste0("rp.contact.field.", field))))
  return(var_names)
}
# started a module?
started_module <- function(data, var_list = "introduction"){
  #var_list <- paste0(var, "_all_started")
  data <- data %>%
    dplyr::select(c(app_user_id, all_of(var_list))) %>%
    mutate(x = ifelse(rowSums(!is.na(across(var_list))) == 0, NA, 1))
  return(data %>% dplyr::pull(x))
}

### DATA 
# this bit will eventually be in a data manipulation code file
#our_data <- readRDS(file="C:/Users/lclem/OneDrive/Documents/GitHub/ParentAppDataScripts/WASHapp20230710.RDS")
our_data$rp.contact.field.app_launch_count <- as.numeric(our_data$rp.contact.field.app_launch_count)
our_data$rp.contact.field.max_days_between_app_launches <- as.numeric(our_data$rp.contact.field.max_days_between_app_launches)



################# Click Times ##########################

module_names <- c("introduction", "handwashing_with_soap", "when_to_wash_your_hands",
                  "safe_drinking_water",
                  "waste", "celebration", "safe_food",  "bathing", "healthy_families",
                  "clean_toilets", "healthy_homes", "how_to_wash_your_hands")

module_names_click_history <- paste0("rp.contact.field.", module_names, "_card_click_history")

module_names_click_history

our_data_click_history <- our_data %>% dplyr::select(c("YourParentAppCode", module_names_click_history))


our_data_click_history_1 <- our_data_click_history[1,]



card_click_history <- purrr::map(.x = module_names_click_history,
                                 .f = ~ stringr::str_split(our_data_click_history[[.x]], " ;  "))
names(card_click_history) <- module_names
for (i in 1:12){
  names(card_click_history[[i]]) <- our_data_click_history[["YourParentAppCode"]]
}
card_click_history <- purrr::map(.x = module_names,
                                 .f = ~ data.frame(unlist(card_click_history[[.x]])))
names(card_click_history) <- module_names
for (i in 1:12){
  card_click_history[[i]]$UUID <- rownames(card_click_history[[i]])
}
card_click_history <- plyr::ldply(card_click_history)
names(card_click_history) <- c("Module", "Time of click", "UUID")
card_click_history$UUID <- substr(card_click_history$UUID, 1, 16) 

card_click_history$`Time of click` <- as_datetime(card_click_history$`Time of click`)
card_click_history$Module <- forcats::as_factor(card_click_history$Module)
# card_click_history1 <- card_click_history %>% filter(UUID == "4c558b055d709423")
# ggplot(card_click_history1, aes(x = Module, y = `Time of click`)) + geom_point()

# look at first click and last click of each one
# card_click_history1 <- card_click_history1 %>%
#   group_by(Module, UUID) %>%
#   summarise(first_click = dplyr::first(`Time of click`),
#             last_click = dplyr::last(`Time of click`))
# card_click_history1 <- card_click_history1 %>%
#   pivot_longer(cols = c("first_click", "last_click"), names_to = "Click type", values_to = "Time")
# ggplot(card_click_history1, aes(x = Module, y = `Time`)) + geom_point()


card_click_history <- card_click_history %>% filter(!is.na(`Time of click`))
card_click_history$Date <- as.Date(card_click_history$`Time of click`)
card_click_history$Week <- lubridate::week(ymd(card_click_history$Date)) - lubridate::week(ymd(min(card_click_history$Date))) + 1

card_click_history1 <- card_click_history %>%
  #filter(UUID == "4c558b055d709423") %>%
  group_by(Module, UUID) %>%
  summarise(first_click = dplyr::first(`Time of click`),
            last_click = dplyr::last(`Time of click`),
            first_week = dplyr::first(Week),
            last_week = dplyr::last(Week))
card_click_history1 <- card_click_history1 %>%
  #  pivot_longer(cols = c("first_click", "last_click"), names_to = "Click type", values_to = "Time")
  pivot_longer(cols = c("first_week", "last_week"), names_to = "Click type", values_to = "Week")
#ggplot(card_click_history1, aes(x = Module, y = `Time`, colour = `Click type`, group = `Click type`)) + geom_point() +
#  geom_line()
card_click_history1 <- card_click_history1 %>%
  mutate(`Click type` = ifelse(`Click type` == "first_week", "First", "Last"))
ggplot(card_click_history1, aes(x = Module, y = `Week`, colour = `Click type`, group = `Click type`)) + geom_point() +
  geom_line() +
  facet_wrap(vars(UUID)) +
  ggthemes::scale_colour_colorblind()



# Number of clicks
card_click_history1 <- card_click_history %>%
  group_by(Module, UUID, Week) %>%
  summarise(Number_of_clicks = n())
#ggplot(card_click_history1, aes(x = Module, y = `Time`, colour = `Click type`, group = `Click type`)) + geom_point() +
#  geom_line()
ggplot(card_click_history1, aes(x = Module, y = `Week`, size = Number_of_clicks, colour = Number_of_clicks)) + geom_point() +
  facet_wrap(vars(UUID)) +
  scale_colour_gradient(low = "Orange", high = "Black")
