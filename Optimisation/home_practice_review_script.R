
optdata$hp_started_1on1
optdata$hp_started_1on1_num




# Define the pairs of .x and .y
library(dplyr)
library(purrr)
vars <- c("hp_started_1on1_num", "hp_started_praise_num", "hp_started_instruct_num", "hp_started_stress_num", "hp_started_money_num", "hp_started_rules_num", "hp_started_consequence_num", "hp_started_solve_num", "hp_started_safe_num", "hp_started_crisis_num")

result <- purrr::map2(.x = c("hp_started_1on1_num", "hp_started_praise_num", "hp_started_instruct_num", "hp_started_stress_num", "hp_started_money_num", "hp_started_rules_num", "hp_started_consequence_num", "hp_started_solve_num", "hp_started_safe_num", "hp_started_crisis_num"),
                      .y =  c("hp_done_1on1", "hp_done_praise", "hp_done_instruct", "hp_done_stress_hp_breathe_done", "hp_done_money", "hp_done_rules", "hp_done_consequence", "hp_done_solve", "hp_done_safe", "hp_done_crisis"),
                      .f = ~ optdata %>%
                        filter(!!sym(.x) == 1) %>% # filtering for people who started
                        group_by(skin, !!sym(.y)) %>%
                        summarise(n()) %>%
                        dplyr::select(c(skin, hp_done = .y, count = `n()`)))

names(result) <- vars

result2 <- plyr::ldply(result)

result2 <- result2 %>% group_by(skin, `.id`) %>%
  mutate(total_count = sum(count)) %>%
  mutate(prop = count / total_count * 100)

result2$`.id` <- gsub("hp_started_", "", result2$`.id`)
result2$`.id` <- gsub("_num", "", result2$`.id`)
ggplot(result2, aes(x = `.id`, y = count, colour = skin, group = skin)) + 
  geom_point(aes(size = total_count)) +
  geom_line() +
  facet_grid(cols = vars(hp_done), scales = "free") +
  ggthemes::scale_colour_colorblind() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.2))







result2 %>% group_by(hp_done) %>%
  summarise(total_count = sum(count))
