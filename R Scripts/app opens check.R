# Stress showing up as 2 variables for users who completed stress home practice
# we think there are two activities and would sum them to indicate completion of the home practice
summary_table_baseline_build <- summary_table_base_build(opt_factors = "Skin",
                                                         columns_to_summarise = data_hp_done,
                                                         replace = "rp.contact.field.w_")

#rp.contact.field.w_stress_hp_breathe_done
#rp.contact.field.w_stress_hp_talk_done

# App opens check
data_app_opens1 <- data_app_opens[-1]
plhdata_org_clean$rp.contact.field.app_launch_count <- as.numeric(plhdata_org_clean$rp.contact.field.app_launch_count)
plhdata_org_clean_1 <- plhdata_org_clean %>%
  dplyr::select(app_user_id, data_app_opens) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(total = rowSums(across(data_app_opens1))) %>%
  dplyr::select(app_user_id, rp.contact.field.app_launch_count, total)
plhdata_org_clean_1 %>% filter(rp.contact.field.app_launch_count != total)
#View(plhdata_org_clean_1)



#

which(min(plhdata_org_clean$rp.contact.field.user_age, na.rm = TRUE))

plhdata_org_clean_1 <- plhdata_org_clean %>% select(c(app_user_id, rp.contact.field.user_age))
View(plhdata_org_clean_1)

# In Optimisation
d892b1f3740a8154 01*
  2cb7dfdb424a40f9 1970
a7d06956c51fc205 1974
b6bcf11fa2576c65 1975
e526f52402024c71 56-
  813ecf9e57ee9a3d 5655
005fe4160844434d 60--
  afad31776569e40f <NAME> 52

"9a1fd2a40c6ce593" "58e3896bae31ec2a" "edcc6da11856826d" "4ba48c4f51b3ff13" "d3f2842891376867"
[6] "ce1d324f22d37fba" "fb7b5790c237a5ad" "347915109106ca43" "6926a8bc261be790" "5f2187568e0662cd"
[11] "e6c105d84818e033" "02ce1c82ef4017a4" "8d18693979f3b879" "2513602e6b9e2e4c" "e127687fc92e8302"
[16] "7e7d8c6b7c38d442" "0e1ff0abd3e1f872" "2ee9e0c2fb35260b" "1a05fd775c4be5ee" "746f6b293cbae011"
[21] "d8bbe92768f62838" "23b407df36f37a06" "7c3573436195d5cd" "a053f4398ba52499"

# Ages in Pilot:
cab216e825ba6e53 -29
9ba1b013b38a7714 ;;;N,,,#,,,##))//-- 0 0054 5 *
c15690727cc1335a 1794
2820e1b0a3a4863a 777-
  # NA ages:
  fab4ae58ac03f920
d151b6ae8d96e590
214462c0560cf125
086250600b410646
0e5824bd19aae8c4
0411d3ba86824f34
a9ecb7774e4b769b






# nrow(plhdata_org_clean %>% filter(rp.contact.field.w_money_completion_level == 100)) / nrow(plhdata_org_clean %>% filter(rp.contact.field.w_money_started == "true"))
summary_mean_completion_level <- summary_table(columns_to_summarise = data_completion_level,
                                               replace = "rp.contact.field.w_",
                                               replace_after = "_completion_level",
                                               summaries = "mean",
                                               factors = Skin)

plhdata_org_clean %>%
  filter(Skin == "Module") %>%
  group_by(rp.contact.field.w_1on1_completion_level) %>%
  summarise(n())
summarise(    min(rp.contact.field.w_self_care_completion_level, na.rm = TRUE),
              max(rp.contact.field.w_self_care_completion_level, na.rm = TRUE),
              mean(rp.contact.field.w_self_care_completion_level, na.rm = TRUE),
              n())
