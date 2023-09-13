##### For the Modules Completion Rate ####
MCR <- calculate_module_completion_percentage()
#View(MCR)
MCP <- calculate_module_click_percentage()
CDF <- calculate_module_completion_percentage()

plot_completion_rate_grouped <- ggplot(MCR, aes(x = Module_Name, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Module Completion Rate",
       x = "Module Name",
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c"))

#print(plot_completion_rate_grouped)


#### Section Completion Rate You need to declare which module, here module 4 is picked ####
SCP4 <- calculate_section_completion_percentage(module_id = 4)
#View(SCP4)
# To visualize it

plot_section_completion_rate_grouped <- ggplot(SCP4, aes(x = Section_Name, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Section Completion Rate for Module 4",
       x = "Section Name",
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c"))

#print(plot_section_completion_rate_grouped)

##### Users Engagement using the card Click History ####
SCLP4 <- calculate_section_click_percentage(module_id = 4)
#View(SCLP4)


#### Toggle Usage Percentage for Modules ####
## toggle_type can be "started" or "completed"
## toggle_status can be "toggle" or "not_toggle"

TP <- calculate_toggle_percentage(toggle_type = "started", toggle_status = "toggle")

#View(TP)

# Grouped bar chart for Module Toggle Usage
plot_toggle_usage_grouped <- ggplot(TP, aes(x = Module_Name, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Module Toggle Usage",
       x = "Module Name",
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  scale_fill_manual(values = c("#1f78b4", "#e31a1c"))

#print(plot_toggle_usage_grouped)


