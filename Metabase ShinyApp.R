print("shinyapp")
study = "RCT"
country = "Tanzania"
print("shinyapp done")
# 1. Functions -------------------------------------------------------------------------
# 3. Define UI -----------------------------------------------------------------------------
parentapp_shiny <- function(country, study){
  # Define UI
  ui <- dashboardPage(
    header = dashboardHeader(title = paste(country, study, "ParentApp Dashboard")),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem("Overview and Demographics", tabName = "demographics", icon = icon("users")),
        menuItem("Workshop Engagement", tabName = "workshops", icon = icon("lightbulb")),
        menuItem("Additional Modules", tabName = "additionalinfo", icon = icon("plus")),
        menuItem("Parent Points", tabName = "parentpoints", icon = icon("star")),
        menuItem("In-week Engagement", tabName = "xtraengagement", icon = icon("user-check")),
        menuItem("Parent Library", tabName = "library", icon = icon("book-reader")),
        menuItem("Download", tabName = "download", icon = icon("download"))
      )), #closes sidebarMenu and dashboardSidebar
    
    dashboardBody(# Boxes need to be put in a row (or column)
      #top_boxes(country = country), #closes fluidRow
      fluidRow(
        shinydashboard::valueBoxOutput("total_n", width=6), 
        shinydashboard::valueBoxOutput("total_users", width=6), 
        #shinydashboard::valueBoxOutput("myvaluebox1", width=3), 
        shinydashboard::valueBoxOutput("myvaluebox2", width=4),
        shinydashboard::valueBoxOutput("myvaluebox3", width=4),
        shinydashboard::valueBoxOutput("myvaluebox4", width=4)
      ),
      fluidRow(checkbox_input(inputId = "Dem", country = country, study = study)), #closes fluidRow
      tabItems(
        # First tab content layout
        tabItem(tabName = "demographics",
                fluidRow(
                  column(12, align = "centre",
                         # splitLayout gets two boxes side by side.
                         # in this case, it is just the header (h2), and an icon
                         # we want 80% of the width to be the header (h2) and 20% the icon (hence cellWidths = ...)
                         box(splitLayout(h2("User Overview and Demographics"), icon("users", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 15,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = "light-blue", #Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                             height = "95px")
                  ) #closes box
                ), #closes fluid row
                
                fluidRow(
                  box(width = 12,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "App first downloaded",
                      status = "primary",  
                      #background = "orange",
                      plotlyOutput(outputId = "plot_app_downloaded", height = "240"),
                      shiny::tableOutput("table_app_downloaded")
                  )#closes box
                ), #closes fluid row
                
                fluidRow(
                  box(width = 12,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "App last synced",
                      status = "primary",  
                      #background = "orange",
                      plotlyOutput(outputId = "plot_app_launch", height = "240"),
                      shiny::tableOutput("table_app_launch")
                  )#closes box
                ), #closes fluid row
                
                fluidRow(
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Last Sync by Week",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_engagement_weeks", height = "240"), #generates graph
                      shiny::tableOutput("table_engagement_weeks")  #generates table
                  ), #closes box
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Language",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_app_language", height = "240"), #generates graph
                      shiny::tableOutput("table_app_language")  #generates table
                  ) #closes box
                ), #closes box, fluid row
                
                fluidRow(
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "App version",
                      status = "primary", # primary, success, info, warning, danger
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_app_version", height = "240"),
                      shiny::tableOutput("table_app_version")),
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "User gender",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_parent_gender", height = "240"), #generates graph
                      shiny::tableOutput("table_parent_gender")  #generates table
                      ) #closes box
                ), #closes fluidRow
                
                demographics_fluid_row(study = study)
                
        ),#closes first tabItem
        
        # Second tab content layout
        tabItem(tabName = "workshops",
                fluidRow(
                  column(12, align = "centre",
                         # splitLayout gets two boxes side by side.
                         # in this case, it is just the header (h2), and an icon
                         # we want 80% of the width to be the header (h2) and 20% the icon (hence cellWidths = ...)
                         box(splitLayout(h2("Workshop Engagement"), icon("lightbulb", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 15,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = "aqua",
                             height = "95px")
                  ) #closes box
                ), #closes fluid row
                
                #fluidRow(checkbox_input(inputId = "WS", country = country)), #closes fluidRow
                
                tabsetPanel(type = "tabs",
                            tabPanel("Overview",
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Number of users who have started a workshop",
                                           status = "info",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_ws_started", height = "240"),
                                           shiny::tableOutput("table_ws_started")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Average workshop completion level",
                                           status = "info",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_ws_totals", height = "240"),
                                           shiny::tableOutput("table_ws_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Percentage of starters who completed a workshop",
                                           status = "info",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_ws_rel_completed", height = "240"),
                                           shiny::tableOutput("table_ws_rel_completed")
                                       )#closes box
                                     ) #closes fluid row
                            ), # closes Overview tabPanel
                            
                            tabPanel("Additional Insights",
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshops started compared to week number",
                                           h5("Not started, <33% (low), 33-67% (moderate), 67-100% (high), workshops exceed number of weeks (ahead)"),
                                           status = "info",
                                           style='width:100%;overflow-x: scroll;',
                                           shiny::tableOutput("week_engagement")
                                       )),
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 1: Welcome and Self-Care",
                                           status = "info",
                                           h5("As individual: percentage out of 9 steppers;"), h5("As group: not an option for first workshop"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_self_care", height = "240"),
                                           shiny::tableOutput("table_w_self_care")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 2: One-on-One Time",
                                           status = "info",  
                                           h5("As individual: percentage out of 7 steppers;"), h5("As group: percentage out of 9 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_1on1", height = "240"),
                                           shiny::tableOutput("table_w_1on1")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 3: Praise",
                                           status = "info",  
                                           h5("As individual: percentage out of 9 steppers"), h5("As group: percentage out of 11 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_praise", height = "240"),
                                           shiny::tableOutput("table_w_praise")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 4: Positive Instructions",
                                           status = "info",  
                                           h5("As individual: percentage out of 11 steppers"), h5("As group: percentage out of 13 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_instruct", height = "240"),
                                           shiny::tableOutput("table_w_instruct")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 5: Managing Stress",
                                           status = "info",
                                           h5("As individual: percentage out of 10 steppers"), h5("As group: percentage out of 11 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_stress", height = "240"),
                                           shiny::tableOutput("table_w_stress")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 6: Problem Solving",
                                           status = "info",  
                                           h5("As individual: percentage out of 10 steppers"), h5("As group: npercentage out of 12 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_solve", height = "240"),
                                           shiny::tableOutput("table_w_solve")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 7: Family Budgets",
                                           status = "info",  
                                           h5("As individual: percentage out of 18 steppers"), h5("As group: percentage out of 18 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_money", height = "240"),
                                           shiny::tableOutput("table_w_money")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 8: Rules",
                                           status = "info",  
                                           h5("As individual: percentage out of 10 steppers"), h5("As group: npercentage out of 11 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_rules", height = "240"),
                                           shiny::tableOutput("table_w_rules")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,                                           
                                           title = "Workshop 9: Calm Consequences",
                                           status = "info",  
                                           h5("As individual: percentage out of 12 steppers"), h5("As group: npercentage out of 14 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_consequence", height = "240"),
                                           shiny::tableOutput("table_w_consequence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 10: Teen Safety",
                                           status = "info",  
                                           h5("As individual: percentage out of 11 steppers"), h5("As group: npercentage out of 12 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_safe", height = "240"),
                                           shiny::tableOutput("table_w_safe")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 11: Dealing with Crisis",
                                           status = "info",  
                                           h5("As individual: percentage out of 11 steppers"), h5("As group: npercentage out of 12 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_crisis", height = "240"),
                                           shiny::tableOutput("table_w_crisis")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 12: Celebration and Next Steps",
                                           status = "info",  
                                           h5("As individual: percentage out of 6 steppers"), h5("As group: npercentage out of 9 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_celebrate", height = "240"),
                                           shiny::tableOutput("table_w_celebrate")
                                       ) #closes box
                                     ) #closes fluid row
                            ) # closes Additional Insights
                ) #closes tabsetPanel for workshop
        ), #closes tabItem
        
        # NEW third tab content layout
        tabItem(tabName = "additionalinfo",
                
                fluidRow(
                  column(12, align = "centre",
                         # splitLayout gets two boxes side by side.
                         # in this case, it is just the header (h2), and an icon
                         # we want 80% of the width to be the header (h2) and 20% the icon (hence cellWidths = ...)
                         box(splitLayout(h2("Additional Modules"), icon("plus", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 15,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = "aqua",
                             height = "95px")
                  ) #closes column
                ), #closes fluid row
                
                fluidRow(
                  shinydashboard::valueBoxOutput("additional_total", width=6), 
                  shinydashboard::valueBoxOutput("additional_starter", width=6)
                ),
                tabsetPanel(type = "tabs",
                            tabPanel("Overview",
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "User Engagement (in last X days) for those who accessed the additional content",
                                           status = "info",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_engagement_additional_content", height = "240"),
                                           shiny::tableOutput("table_engagement_additional_content")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Number of users who have started a workshop",
                                           status = "info",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "additional_plot_ws_started", height = "240"),
                                           shiny::tableOutput("additional_table_ws_started")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Average workshop completion level",
                                           status = "info",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "additional_plot_ws_totals", height = "240"),
                                           shiny::tableOutput("additional_table_ws_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Percentage of starters who completed a workshop",
                                           status = "info",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "additional_plot_ws_rel_completed", height = "240"),
                                           shiny::tableOutput("additional_table_ws_rel_completed")
                                       )#closes box
                                     ) #closes fluid row
                            ), # closes Overview tabPanel
                            
                            tabPanel("Additional Insights",
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Learn",
                                           status = "info",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_learn", height = "240"),
                                           shiny::tableOutput("table_w_learn")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "SVP",
                                           status = "info",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_svp", height = "240"),
                                           shiny::tableOutput("table_w_svp")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Grief",
                                           status = "info",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_grief", height = "240"),
                                           shiny::tableOutput("table_w_grief")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "SRH",
                                           status = "info",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_srh", height = "240"),
                                           shiny::tableOutput("table_w_srh")
                                       ) #closes box
                                     ) #closes fluid row
                            ), # closes Additional Insights
                            tabPanel("Activities",
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Number of users who have started an activity",
                                           status = "info",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "activity_plot_started", height = "240"),
                                           shiny::tableOutput("activity_table_started")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Number of users who have self-reported trying the activity",
                                           status = "info",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "activity_plot_done", height = "240"),
                                           shiny::tableOutput("activity_table_done")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Number of repeat users for an activity",
                                           status = "info",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "activity_plot_repeat", height = "240"),
                                           shiny::tableOutput("activity_table_repeat")
                                       )#closes box
                                     ), #closes fluid row
                                     # 
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Number of activities started (for users who have done at least one)",
                                           status = "info",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "activity_plot_total", height = "240"),
                                           shiny::tableOutput("activity_table_total")
                                       )#closes box
                                     )
                            )
                ) #closes tabsetPanel for additional insights
        ), #closes tabItem
        
        # Third tab content layout
        tabItem(tabName = "parentpoints",
                
                fluidRow(
                  column(12, align = "centre",
                         # splitLayout gets two boxes side by side.
                         # in this case, it is just the header (h2), and an icon
                         # we want 80% of the width to be the header (h2) and 20% the icon (hence cellWidths = ...)
                         box(splitLayout(h2("Parent points"), icon("star", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 15,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = "yellow",
                             height = "95px")
                  ) #closes column
                ), #closes fluid row
                
                fluidRow(#checkbox_input(inputId = "PP", country = country),
                  box(width = 4,
                      checkboxGroupInput(inputId = "PpPP",
                                         label = "Parent points to show:",
                                         choices = c("Relax" = "relax",
                                                     "Treat yourself well" = "treat_yourself",
                                                     "Praise yourself" = "praise_yourself", 
                                                     "One-on-one time" =  "spend_time",
                                                     "Praise your teen" = "praise_teen",
                                                     "Get positive" = "instruct_positively",
                                                     "Breathe not yell" = "breathe",
                                                     "Good money choice" = "money",
                                                     "Calm consequence" = "consequence",
                                                     "Safe" = "safe"),
                                         selected = c("relax", "treat_yourself", "praise_yourself", 
                                                      "spend_time", "praise_teen",
                                                      "instruct_positively","breathe","money", "consequence", "safe"),
                                         inline = TRUE
                      ))
                ), #closes fluidRow
                
                tabsetPanel(type = "tabs",
                            tabPanel("Cumulative Parent Points",
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent points summary",
                                           status = "warning",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pp_totals", height = "240"),
                                           shiny::tableOutput("table_pp_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent points summary",
                                           status = "warning",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pp_means", height = "240"),
                                           shiny::tableOutput("table_pp_means")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent points: Relax",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax", height = "240"),
                                           shiny::tableOutput("table_pp_relax")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent points: Treat yourself well",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent points: Praise yourself",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent points: One-on-one Time",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent points: Praise your teen",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent points: Get positive",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent points: Breathe not yell",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe", height = "240"),
                                           shiny::tableOutput("table_pp_breathe")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent points: Good money choice",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money", height = "240"),
                                           shiny::tableOutput("table_pp_money")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent points: Calm consequence",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_consequence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Parent points: Safe",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe", height = "240"),
                                           shiny::tableOutput("table_pp_safe")
                                       ) #closes box
                                     ) #closes fluid row
                            ), #closes and tab panel
                            
                            tabPanel("Relax", #pp1
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Average Relax Points per Workshop Week",
                                           status = "warning",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pp_relax_ws_totals", height = "240"),
                                           shiny::tableOutput("table_pp_relax_ws_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 1: Welcome and Self-Care",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_self_care", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_self_care")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 2: One-on-One Time",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_1on1", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_1on1")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 3: Praise",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_praise", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_praise")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 4: Positive Instructions",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_instruct", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_instruct")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 5: Managing Stress",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_stress", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_stress")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 6: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_solve")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 7: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_money")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 8: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_rules")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 9: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_consequence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 10: Teen Safety",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_safe", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_safe")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 11: Dealing with Crisis",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_crisis", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_crisis")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 12: Celebration and Next Steps",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_celebrate", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_celebrate")
                                       ) #closes box
                                     ) #closes fluid row
                                     
                            ), #closes tab panel pp1 relax
                            
                            tabPanel("Treat Self", #pp2
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Average Treat Yourself Points per Workshop Week",
                                           status = "warning",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_ws_totals", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_ws_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 1: Welcome and Self-Care",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_self_care", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_self_care")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 2: One-on-One Time",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_1on1", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_1on1")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 3: Praise",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_praise", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_praise")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 4: Positive Instructions",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_instruct", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_instruct")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 5: Managing Stress",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_stress", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_stress")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 6: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_solve")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 7: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_money")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 8: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_rules")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 9: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_consequence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 10: Teen Safety",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_safe", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_safe")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 11: Dealing with Crisis",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_crisis", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_crisis")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 12: Celebration and Next Steps",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_celebrate", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_celebrate")
                                       ) #closes box
                                     ) #closes fluid row
                                     
                            ), #closes tab panel pp2 treat yourself well
                            
                            tabPanel("Praise Self", #pp3 praise yourself
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Average Praise Yourself Points per Workshop Week",
                                           status = "warning",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_ws_totals", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_ws_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 1: Welcome and Self-Care",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_self_care", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_self_care")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 2: One-on-One Time",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_1on1", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_1on1")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 3: Praise",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_praise", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_praise")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 4: Positive Instructions",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_instruct", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_instruct")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 5: Managing Stress",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_stress", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_stress")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 6: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_solve")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 7: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_money")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 8: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_rules")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 9: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_consequence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 10: Teen Safety",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_safe", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_safe")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 11: Dealing with Crisis",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_crisis", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_crisis")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 12: Celebration and Next Steps",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_celebrate", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_celebrate")
                                       ) #closes box
                                     ) #closes fluid row
                            ), #closes tab panel pp3 praise yourself
                            
                            tabPanel("1-on-1",  #pp4 spend time together
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Average 1-on-1 Time Points per Workshop Week",
                                           status = "warning",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pp_spend_time_ws_totals", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_ws_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 1: Welcome and Self-Care",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_self_care", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_self_care")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 2: One-on-One Time",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_1on1", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_1on1")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 3: Praise",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_praise", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_praise")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 4: Positive Instructions",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_instruct", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_instruct")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 5: Managing Stress",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_stress", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_stress")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 6: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_solve")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 7: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_money")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 8: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_rules")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 9: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_consequence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 10: Teen Safety",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_safe", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_safe")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 11: Dealing with Crisis",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_crisis", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_crisis")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 12: Celebration and Next Steps",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_celebrate", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_celebrate")
                                       ) #closes box
                                     ) #closes fluid row  
                            ), #closes tab panel pp4 spend time together
                            
                            tabPanel("Praise Teen", #pp5
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Average Praise Teen Points per Workshop Week",
                                           status = "warning",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pp_praise_teen_ws_totals", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_ws_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 1: Welcome and Self-Care",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_self_care", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_self_care")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 2: One-on-One Time",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_1on1", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_1on1")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 3: Praise",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_praise", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_praise")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 4: Positive Instructions",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_instruct", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_instruct")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 5: Managing Stress",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_stress", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_stress")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 6: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_solve")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 7: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_money")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 8: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_rules")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 9: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_consequence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 10: Teen Safety",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_safe", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_safe")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 11: Dealing with Crisis",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_crisis", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_crisis")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 12: Celebration and Next Steps",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_celebrate", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_celebrate")
                                       ) #closes box
                                     ) #closes fluid row      
                            ), #closes tab panel pp5 praise teen
                            
                            tabPanel("Positive", #pp6 get positive
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Average Get Positive Points per Workshop Week",
                                           status = "warning",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_ws_totals", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_ws_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 1: Welcome and Self-Care",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_self_care", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_self_care")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 2: One-on-One Time",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_1on1", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_1on1")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 3: Praise",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_praise", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_praise")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 4: Positive Instructions",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_instruct", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_instruct")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 5: Managing Stress",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_stress", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_stress")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 6: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_solve")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 7: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_money")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 8: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_rules")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 9: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_consequence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 10: Teen Safety",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_safe", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_safe")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 11: Dealing with Crisis",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_crisis", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_crisis")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 12: Celebration and Next Steps",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_celebrate", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_celebrate")
                                       ) #closes box
                                     ) #closes fluid row
                            ), #closes tab panel pp6 get positive
                            
                            tabPanel("Breathe", #pp7 breathe not yell
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Average Breathe Points per Workshop Week",
                                           status = "warning",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pp_breathe_ws_totals", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_ws_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 1: Welcome and Self-Care",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_self_care", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_self_care")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 2: One-on-One Time",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_1on1", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_1on1")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 3: Praise",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_praise", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_praise")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 4: Positive Instructions",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_instruct", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_instruct")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 5: Managing Stress",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_stress", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_stress")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 6: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_solve")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 7: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_money")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 8: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_rules")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 9: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_consequence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 10: Teen Safety",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_safe", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_safe")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 11: Dealing with Crisis",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_crisis", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_crisis")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 12: Celebration and Next Steps",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_celebrate", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_celebrate")
                                       ) #closes box
                                     ) #closes fluid row
                            ), #closes tab panel pp7 breathe not yell
                            
                            tabPanel("Money", #pp8 good money choice
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Average Good Money Choice Points per Workshop Week",
                                           status = "warning",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pp_money_ws_totals", height = "240"),
                                           shiny::tableOutput("table_pp_money_ws_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 1: Welcome and Self-Care",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_self_care", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_self_care")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 2: One-on-One Time",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_1on1", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_1on1")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 3: Praise",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_praise", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_praise")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 4: Positive Instructions",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_instruct", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_instruct")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 5: Managing Stress",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_stress", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_stress")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 6: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_solve")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 7: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_money")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 8: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_rules")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 9: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_consequence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 10: Teen Safety",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_safe", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_safe")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 11: Dealing with Crisis",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_crisis", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_crisis")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 12: Celebration and Next Steps",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_celebrate", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_celebrate")
                                       ) #closes box
                                     ) #closes fluid row
                            ), #closes tab panel pp8 good money choice
                            
                            tabPanel("Consequence", #pp9 calm consequence
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Average Calm Consequence Points per Workshop Week",
                                           status = "warning",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pp_consequence_ws_totals", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_ws_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 1: Welcome and Self-Care",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_self_care", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_self_care")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 2: One-on-One Time",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_1on1", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_1on1")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 3: Praise",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_praise", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_praise")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 4: Positive Instructions",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_instruct", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_instruct")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 5: Managing Stress",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_stress", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_stress")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 6: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_solve")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 7: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_money")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 8: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_rules")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 9: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_consequence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 10: Teen Safety",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_safe", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_safe")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 11: Dealing with Crisis",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_crisis", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_crisis")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 12: Celebration and Next Steps",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_celebrate", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_celebrate")
                                       ) #closes box
                                     ) #closes fluid row
                            ), #closes tab panel pp9 calm consequence
                            
                            tabPanel("Safe",  #pp10 safe
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Average Safe Points per Workshop Week",
                                           status = "warning",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pp_safe_ws_totals", height = "240"),
                                           shiny::tableOutput("table_pp_safe_ws_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 1: Welcome and Self-Care",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_self_care", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_self_care")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 2: One-on-One Time",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_1on1", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_1on1")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 3: Praise",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_praise", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_praise")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 4: Positive Instructions",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_instruct", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_instruct")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 5: Managing Stress",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_stress", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_stress")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 6: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_solve")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 7: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_money")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 8: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_rules")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 9: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_consequence")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 10: Teen Safety",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_safe", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_safe")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 11: Dealing with Crisis",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_crisis", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_crisis")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 12: Celebration and Next Steps",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_celebrate", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_celebrate")
                                       ) #closes box
                                     ) #closes fluid row
                            ) #closes tab panel pp10 safe
                ) #closes tabset panel         
        ), # closes third tabItem
        
        #FOURTH tab content
        tabItem(tabName = "xtraengagement",
                
                fluidRow(
                  column(12, align = "centre",
                         # splitLayout gets two boxes side by side.
                         # in this case, it is just the header (h2), and an icon
                         # we want 80% of the width to be the header (h2) and 20% the icon (hence cellWidths = ...)
                         box(splitLayout(h2("In-week Engagement"), icon("user-check", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 15,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = "green",
                             height = "95px")
                  ) #closes box
                ), #closes fluid row
                
                fluidRow(#checkbox_input(inputId = "XE", country = country),
                  box(width = 4,
                      checkboxGroupInput(inputId = "WsXE",
                                         label = "Workshop weeks to show:",
                                         choices = c("1 Welcome and Self-Care"= "w_self_care",
                                                     "2 One-on-One Time" = "w_1on1",
                                                     "3 Praise" = "w_praise",
                                                     "4 Positive Instructions" = "w_instruct",
                                                     "5 Managing Stress" = "w_stress",
                                                     "6 Problem Solving" = "w_solve",
                                                     "7 Family Budgets" = "w_money",
                                                     "8 Rules" = "w_rules",
                                                     "9 Calm Consequences" = "w_consequence" ,
                                                     "10	Teen Safety" = "w_safe",
                                                     "11 Dealing with Crisis" = "w_crisis",
                                                     "12 Celebration and Next Steps" = "w_celebrate"),
                                         selected = c("w_self_care", "w_1on1", "w_praise", "w_instruct","w_stress", "w_solve", "w_money", "w_rules", "w_consequence",  "w_safe", "w_praise", "w_crisis", "w_celebrate"),
                                         inline = TRUE)
                  )), #closes box and fluid row
                
                tabsetPanel(type = "tabs",
                            tabPanel("App Opens",
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Cumulative app opens",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_totals", height = "240"),
                                           shiny::tableOutput("table_appopen_totals"),
                                           shiny::tableOutput("table_appopen_summary")
                                       ) #closes box
                                     ), #closes fluidrow
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Mean app opens per workshop week",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_mean_week", height = "240"),
                                           shiny::tableOutput("table_appopen_mean_week")
                                       )#closes box
                                     ), #closes fluid row
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           status = "success",  
                                           title = "App Opens for Week 1: Welcome and Self Care",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_self_care", height = "240"),
                                           shiny::tableOutput("table_appopen_self_care")
                                       ) #closes box
                                     ), #closes fluidrow
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           status = "success", 
                                           title = "App Opens for Week 2: One-on-One Time",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_1on1", height = "240"),
                                           shiny::tableOutput("table_appopen_1on1")
                                       ) #closes box
                                     ), #closes fluidrow
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "App Opens for Week 3: Praise",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_praise", height = "240"),
                                           shiny::tableOutput("table_appopen_praise")
                                       ) #closes box
                                     ), #closes fluidrow
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "App Opens for Week 4: Positive Instructions",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_instructions", height = "240"),
                                           shiny::tableOutput("table_appopen_instructions")
                                       ) #closes box
                                     ), #closes fluidrow
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "App Opens for Week 5: Stress",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_stress", height = "240"),
                                           shiny::tableOutput("table_appopen_stress")
                                       ) #closes box
                                     ), #closes fluidrow
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "App Opens for Week 6: Problem Solving",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_problem_solving", height = "240"),
                                           shiny::tableOutput("table_appopen_problem_solving")
                                       ) #closes box
                                     ), #closes fluidrow
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "App Opens for Week 7: Budgeting",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_budget", height = "240"),
                                           shiny::tableOutput("table_appopen_budget")
                                       ) #closes box
                                     ), #closes fluidrow
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "App Opens for Week 8: Rules",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_rules", height = "240"),
                                           shiny::tableOutput("table_appopen_rules")
                                       ) #closes box
                                     ), #closes fluidrow
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "App Opens for Week 9: Consequences",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_consequences", height = "240"),
                                           shiny::tableOutput("table_appopen_consequences")
                                       ) #closes box
                                     ), #closes fluidrow
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "App Opens for Week 10: Teen Safety",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_teen_safety", height = "240"),
                                           shiny::tableOutput("table_appopen_teen_safety")
                                       ) #closes box
                                     ), #closes fluidrow
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "App Opens for Week 11: Crisis",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_crisis", height = "240"),
                                           shiny::tableOutput("table_appopen_crisis")
                                       ) #closes box
                                     ), #closes fluidrow
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "App Opens for Week 12: Celebration and Next Steps",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_appopen_celebration", height = "240"),
                                           shiny::tableOutput("table_appopen_celebration")
                                       ) #closes box
                                     ) #closes fluidrow
                            ), #closes tabPanel App Opens
                            
                            tabPanel("Push Notifications",
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Push notification clicks overview",
                                           status = "success",  
                                           #background = "orange",
                                           shiny::tableOutput("table_pushn_totals")
                                       )#closes box
                                     ), #closes fluidrow
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Push notification types sent",
                                           status = "success",
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pushn_mean", height = "240"), #needs to be renames
                                           shiny::tableOutput("table_pushn_mean")
                                       ) #closes box
                                     ) #closes fluid row
                            ), #closes tabPanel Push Notifications
                            
                            tabPanel("Home Practice",
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Number of users starting each home practice review",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_hp_started", height = "240"),
                                           shiny::tableOutput("table_hp_started")
                                       )#closes box
                                     ), #closes fluid row  
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Proportion of users who completed each home practice review, out of those who started",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_hp_done", height = "240"),
                                           shiny::tableOutput("table_hp_done")
                                       )#closes box
                                     ) #closes fluid row  
                ) #closes tabset panel for In-week engagement
                )
        ), # closes fourth tabItem
        
        # SIXTH tab content
        tabItem(tabName = "library",
                fluidRow(
                  column(12, align = "centre",
                         # splitLayout gets two boxes side by side.
                         # in this case, it is just the header (h2), and an icon
                         # we want 80% of the width to be the header (h2) and 20% the icon (hence cellWidths = ...)
                         box(splitLayout(h2("Parent Library"), icon("book-reader", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 15,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = "aqua",
                             height = "95px")
                  ) #closes box
                ), #closes fluid row
                
                #fluidRow(checkbox_input(inputId = "LB", country = country)), #closes fluidRow
                
                fluidRow(
                  box(width = 12,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Parent Library average clicks per workshop week",
                      status = "primary",  
                      #background = "orange",
                      plotlyOutput(outputId = "plot_library_mean", height = "240"), #generates graph
                      shiny::tableOutput("table_library_mean")  #generates table
                  )), #closes box and fluidRow
                
                fluidRow(
                  box(width = 12,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Parent Library access",
                      status = "primary",  
                      #background = "orange",
                      plotlyOutput(outputId = "plot_library_totals", height = "240"), #generates graph
                      shiny::tableOutput("table_library_totals")  #generates table
                  )), #closes box and fluidRow
                
                fluidRow(
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "My Tips (based on customisation)",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_lib_tips", height = "240"), #generates graph
                      shiny::tableOutput("table_lib_tips")  #generates table
                  ), #closes box
                  
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Essential Tools (from each workshop week)",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_lib_tools", height = "240"),
                      shiny::tableOutput("table_lib_tools")
                  ) #closes box
                ), #closes fluidRow
                
                fluidRow(
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Relax and Activities (fun with teen)",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_lib_activities", height = "240"), #generates graph
                      shiny::tableOutput("table_lib_activities")  #generates table
                  ), #closes box
                  
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Customise ParentApp",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_lib_custom", height = "240"),
                      shiny::tableOutput("table_lib_custom")
                  ) #closes box
                ), #closes fluidRow
                
                fluidRow(
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Help (quick parenting tips)",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_lib_help", height = "240"), #generates graph
                      shiny::tableOutput("table_lib_help")  #generates table
                  ), #closes box
                  
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Technical Support",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_lib_tech", height = "240"),
                      shiny::tableOutput("table_lib_tech")
                  ) #closes box
                ), #closes fluidRow
                
                fluidRow(
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Coping COVID",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_lib_covid", height = "240"), #generates graph
                      shiny::tableOutput("table_lib_covid")  #generates table
                  ), #closes box
                  
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Coping with Grief",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_lib_grief", height = "240"), #generates graph
                      shiny::tableOutput("table_lib_grief")  #generates table
                  ) #closes box
                ) #closes fluidRow
        ), #closes sixth tab item
        
        
        tabItem(tabName = "download",
                fluidRow(
                  column(12, align = "centre",
                         # splitLayout gets two boxes side by side.
                         # in this case, it is just the header (h2), and an icon
                         # we want 80% of the width to be the header (h2) and 20% the icon (hence cellWidths = ...)
                         box(splitLayout(h2("Download"), icon("download", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 15,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = "aqua",
                             height = "95px")
                  ) #closes box
                ), #closes fluid row
                useShinyjs(),
                shinyauthr::loginUI("login"),
                uiOutput("build_download")
        ) # closes download tab item
      ) # closes tabItems
    ) # closes dashboardBody
  )# closes dashboardPage
  
  # 4. Define Server -----------------------------------------------------------------------------
  server <- function(input, output, session) {
    
    observe({
      #if (study == "RCT"){
      source(here("Metabase Analysis Setup.R")) # approx 17 secs # so what's the rest of the time? # How long does it take overall,
      #source(here("Metabase Analysis Setup RCT.R")) # approx 17 secs # so what's the rest of the time? # How long does it take overall,
      #} else {
         # approx 17 secs # so what's the rest of the time? # How long does it take overall,
      #}
    })
    
    # if (country == "Tanzania" && study == "Optimisation"){
    #   output$opt_chk_support <- renderUI({
    #     checkboxGroupInput(inputId = "opt_support",
    #                                          label = "Support level",
    #                                          c("Self-guided" = "Self-guided",
    #                                            "WhatsApp" = "WhatsApp"),
    #                                          selected = c("Self-guided", "WhatsApp"))
    #   })
    #   observeEvent(input$chk_support, {
    #     if (input$chk_support) shinyjs::enable(id="opt_support")  
    #     else shinyjs::disable(id="opt_support")
    #   })
    # }
    
    # If Checkbox  
    if (country == "Tanzania" & study %in% c("RCT", "Optimisation")){
      observe({
        if(input$select_cluster){
          shinyjs::disable("opt_cluster")
        } else {
          shinyjs::enable("opt_cluster")
        }
      })
    }
    
    print("0")
    
    if (country == "Tanzania" & study == "Optimisation"){
      selected_data_dem <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
        print("1")
        if(input$select_cluster){
          opt_cluster_vals <- 1:16
        } else {
          opt_cluster_vals <- extract(input$opt_cluster)
        }
        plhdata_checkgroup <- plhdata_org_clean %>%
          dplyr::filter(Cluster %in% c(opt_cluster_vals))
        if (!is.null(input$opt_support)) {
          plhdata_checkgroup <- plhdata_checkgroup %>%
            dplyr::filter(Support %in% c(input$opt_support))
        }
        if (!is.null(input$opt_skin)) {
          plhdata_checkgroup <- plhdata_checkgroup %>%
            dplyr::filter(Skin %in% c(input$opt_skin))
        }
        if (!is.null(input$opt_diglit)) {
          plhdata_checkgroup <- plhdata_checkgroup %>%
            dplyr::filter(`Digital Literacy` %in% c(input$opt_diglit))
        }
        return(plhdata_checkgroup)
      })
    } else {
      selected_data_dem <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
        if (country == "Tanzania"){
          if (study == "Pilot"){
            plhdata_checkgroup <- plhdata_org_clean %>%
              dplyr::filter(PilotSite %in% c(input$OrgDem))
          } else if (study == "RCT") {
            if(input$select_cluster){
              opt_cluster_vals <- unique(UIC_Tracker_Use$ClusterName)
            } else {
              opt_cluster_vals <- extract(input$opt_cluster, as.numeric = FALSE)
            }
            plhdata_checkgroup <- plhdata_org_clean %>%
              dplyr::filter(ClusterName %in% c(opt_cluster_vals))
          } else {
            plhdata_checkgroup <- plhdata_org_clean
          }
        } else {
          plhdata_checkgroup <- plhdata_org_clean %>% dplyr::filter(Org %in% c((input$OrgDem)))
        }
        return(plhdata_checkgroup)
      })
    }
  
    last_sync <- reactive({
      if (country == "Tanzania"){
        time_diff <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(selected_data_dem()$updatedAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours")
        return(time_diff)
      }
    })
    
    last_sync_cat <- reactive({
      if (country == "Tanzania"){
        time_diff <- last_sync()
        last_sync_cat <- ifelse(is.na(time_diff), "5",
                                ifelse(time_diff > 60*24, "4",
                                       ifelse(time_diff > 30*24, "3",
                                              ifelse(time_diff > 14*24, "2", "1"))))
      }
    })
    
    # last_sync_upto12weeks <- reactive({
    #   if (country == "Tanzania"){
    #     our_data <- selected_data_dem() %>% filter(`Weeks completed` <= 12)
    #     time_diff <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(selected_data_dem()$updatedAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours")
    #     last_sync_cat <- ifelse(is.na(time_diff), "5",
    #                             ifelse(time_diff > 60*24, "4",
    #                                    ifelse(time_diff > 30*24, "3",
    #                                           ifelse(time_diff > 14*24, "2",
    #                                                  ifelse(time_diff > 7*24, "1", "0")))))
    #     return(last_sync_cat)
    #   }
    # })
    # 
    #SUMMARY STATS HEADER displays (same for all tabs)
    #if (country == "Tanzania"){
    output$total_n <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(nrow(selected_data_dem()), subtitle = "total users", icon = icon("people"),
                               color = "aqua")})
    output$total_users <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(nrow(selected_data_dem() %>% filter(createdAt > as.Date(lubridate::now(tzone = "UTC")) - 7)), subtitle = "trial users joined in last 7 days", icon = icon("clock"),
                               color = "yellow")})
      # output$myvaluebox1 <- shinydashboard::renderValueBox({
      #   shinydashboard::valueBox(nrow(data_engagement_weeks_all() %>% filter(last_sync_cat == "Last synced less than 14 days ago")), subtitle = "Last synced less than 14 days ago", icon = icon("user"),
      #                            color = "green")})
      
      # if (study != "RCT"){
      output$myvaluebox2 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(nrow(data_engagement_weeks_all() %>% filter(last_sync_cat == "Last synced 14-29 days ago")), subtitle = "Last synced 14-29 days ago", icon = icon("user"),
                                 color = "fuchsia")})
      output$myvaluebox3 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(nrow(data_engagement_weeks_all() %>% filter(last_sync_cat == "Last synced 30-59 days ago")), subtitle = "Last synced 30-59 days ago", icon = icon("user"),
                                 color = "purple")})
      output$myvaluebox4 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(nrow(data_engagement_weeks_all() %>% filter(last_sync_cat == "Last synced over 60 days ago")), subtitle = "not synced in over 60 days", icon = icon("user"),
                                 color = "orange")})
      # } else {
      #   output$myvaluebox2 <- shinydashboard::renderValueBox({
      #     shinydashboard::valueBox(nrow(plhdata_org_allusers_count), subtitle = "All time users from Tanzania (matomo)", icon = icon("user"),
      #                              color = "fuchsia")})
      #   output$myvaluebox3 <- shinydashboard::renderValueBox({
      #     shinydashboard::valueBox(nrow(plhdata_org_allusers_count %>% filter(as.Date(createdAt) > date_from)),
      #                              subtitle = "New users in the last 30 days from Tanzania (matomo)", icon = icon("user"),
      #                              color = "purple")})
      #   output$myvaluebox4 <- shinydashboard::renderValueBox({
      #     shinydashboard::valueBox(nrow(plhdata_org %>% filter(as.Date(updatedAt) > date_from)),
      #                              subtitle = "opened the app in the last 30 days", icon = icon("user"),
      #                              color = "orange")})
      # }
      
      output$additional_total <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(nrow(accessed_new_content()), subtitle = "users have accessed the additional modules",
                                 icon = icon("person-running"),
                                 color = "light-blue")})
      output$additional_starter <- shinydashboard::renderValueBox({
        total_count <- total_count()
        total_count$number_started <- apply(!is.na(total_count), 1, sum) - 1
        shinydashboard::valueBox(nrow(total_count %>% filter(number_started != 0)), subtitle = "users have started at least one activity",
                                 icon = icon("play"),
                                 color = "light-blue")
        })

    #   } else {
    #     output$myvaluebox1 <- shinydashboard::renderValueBox({
    #       shinydashboard::valueBox(nrow(plhdata_org_clean), subtitle = "Enrolled", icon = icon("user"),
    #                                color = "aqua")})
    #     output$myvaluebox2 <- shinydashboard::renderValueBox({
    #       shinydashboard::valueBox( nrow(plhdata_org_clean %>% filter(Org == "Amathuba")), subtitle = "Amathuba", icon = icon("user"),
    #                                 color = "navy")})
    #     output$myvaluebox3 <- shinydashboard::renderValueBox({
    #       shinydashboard::valueBox( nrow(plhdata_org_clean %>% filter(Org == "Dlalanathi")), subtitle = "Dlalanathi", icon = icon("user"),
    #                                 color = "navy")})
    #     output$myvaluebox4 <- shinydashboard::renderValueBox({
    #       shinydashboard::valueBox( nrow(plhdata_org_clean %>% filter(Org == "Joy")), subtitle = "Joy", icon = icon("user"),
    #                                 color = "navy")})
    #     output$myvaluebox5 <- shinydashboard::renderValueBox({
    #       shinydashboard::valueBox( nrow(plhdata_org_clean %>% filter(Org == "Nontobeko")), subtitle = "Nontobeko", icon = icon("user"),
    #                                 color = "navy")})
    #   }
    #   if (country == "all"){
    #     output$myvaluebox6 <- shinydashboard::renderValueBox({
    #       shinydashboard::valueBox( nrow(plhdata_org_clean %>% filter(Org == "ICS")), subtitle = "ICS", icon = icon("user"),
    #                                 color = "navy")}) 
    #   }
    # }
    
    opt_factors <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      print("2")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          opt_factors <- c("PilotSite")
        } else if (study == "Optimisation"){
          opt_factors <- c()
          if (!is.null(input$opt_support)){
            opt_factors <- c(opt_factors, "Support")
          }
          if (!is.null(input$opt_skin)){
            opt_factors <- c(opt_factors, "Skin")
          }
          if (!is.null(input$opt_diglit)){
            opt_factors <- c(opt_factors, "Digital Literacy")
          }
          if (length(opt_factors) == 0){
            opt_factors <- c("Org")
          }
        } else if (study == "RCT"){
          opt_factors <- c("ClusterName")
        } else {
          opt_factors <- c("Org")
        }
      } else {
        opt_factors <- c("Org")
      }
      return(opt_factors)
    })
    
    # summary_table_filter <- function(summary_workshop, add_totals = TRUE){
    #   print("3")
    #   if (country == "Tanzania"){
    #     if (study == "Pilot"){
    #       summary_workshop <- summary_workshop %>%
    #         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
    #         mutate(PilotSite = as.character(PilotSite))
    #       if (add_totals){
    #         summary_workshop <- summary_workshop %>%
    #         janitor::adorn_totals(c("row", "col"))
    #       }
    #     } else if (study == "Optimisation"){
    #       if (!is.null(input$opt_support)){
    #         summary_workshop <- summary_workshop %>%
    #           dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
    #           mutate(Support = as.character(Support))
    #       }
    #       if (!is.null(input$opt_skin)){
    #         summary_workshop <- summary_workshop %>%
    #           dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
    #           mutate(Skin = as.character(Skin))
    #       }
    #       if (!is.null(input$opt_diglit)){
    #         summary_workshop <- summary_workshop %>%
    #           dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
    #           mutate(`Digital Literacy` = as.character(`Digital Literacy`))
    #       }
    #       if (add_totals){
    #         summary_workshop <- summary_workshop %>%
    #           janitor::adorn_totals(c("row", "col"))
    #       } else {
    #         summary_workshop <- summary_workshop
    #       }
    #     } else {
    #       summary_workshop <- summary_workshop %>%
    #         dplyr::filter(Org %in% unique(selected_data_dem()$Org))
    #     }
    #   } else {
    #     summary_workshop <- summary_workshop %>%
    #       dplyr::filter(Org %in% unique(selected_data_dem()$Org)) #%>%
    #     #janitor::adorn_totals(c("row", "col")))
    #   }
    #   return(summary_workshop)
    # }
    
    mult_summary_table_filter <- function(summary_table = summary_table_baseline_build){
      print("4")
      # if (country == "Tanzania"){
      #   if (study == "Pilot"){
      #     summary_table <- summary_table %>% 
      #       purrr::map(.f =~.x %>%
      #                    dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
      #                    mutate(PilotSite = as.character(PilotSite)) %>%
      #                    janitor::adorn_totals(c("row", "col")))
      #   } else if (study == "Optimisation"){
      # if (!is.null(input$opt_support)){
      #   summary_table <- summary_table %>% 
      #     purrr::map(.f =~.x %>%
      #                  dplyr::filter(Support %in% c(selected_data_dem()$Support))%>%
      #                  mutate(Support = as.character(Support)))
      # }
      # if (!is.null(input$opt_skin)){
      #   summary_table <- summary_table %>% 
      #     purrr::map(.f =~.x %>%
      #                  dplyr::filter(Skin %in% c(selected_data_dem()$Skin))%>%
      #                  mutate(Skin = as.character(Skin)))
      # }
      # if (!is.null(input$opt_diglit)){
      #   summary_table <- summary_table %>% 
      #     purrr::map(.f =~.x %>%
      #                  dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`))%>%
      #                  mutate(`Digital Literacy` = as.character(`Digital Literacy`)))
      # }
      summary_table <- summary_table %>% 
        purrr::map(.f =~.x %>%
                     janitor::adorn_totals(c("row", "col")))
      #   } else {
      #     summary_table <- summary_table %>% 
      #       purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
      #   }
      # } else {
      #   summary_table <- summary_table %>% 
      #     purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
      #   #janitor::adorn_totals(c("row", "col")))
      # }
      return(summary_table) 
    }
    
    # Demographics ---------------------------------------------------
    summary_table_baseline <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      if (study == "RCT"){
        summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(),
                                 columns_to_summarise = data_baseline_survey$metabase_ID,
                                 include_perc = TRUE) %>%
          purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      } else {      
        summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(), columns_to_summarise = data_baseline_survey$metabase_ID)
        summary_table_baseline_build %>%
          purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0))) %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
      }
    })
    
    plot_app_downloaded  <- reactive({ # last sync
      ggplot(data = selected_data_dem(), aes(x = as.POSIXct(createdAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))) +
        geom_freqpoly(bins = 30) +
        labs(x = "Created Profile (createdAt)", y = "Count")
    }) 
    output$plot_app_downloaded <- renderPlotly({plot_app_downloaded()})
    
    #table_app_launch <- reactive({}) 
    plot_app_launch  <- reactive({ # last sync
      ggplot(data = selected_data_dem(), aes(x = as.POSIXct(updatedAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))) +
        geom_freqpoly(bins = 30) +
        labs(x = "Last sync (updatedAt)", y = "Count")
    }) 
    output$plot_app_launch <- renderPlotly({plot_app_launch()})
    
    #Overview and Demographics plot and table
    display_sheet_table <- function(n = "language", j = 1){
      return(output[[paste0("table_", n)]] <- shiny::renderTable({(summary_table_baseline()[[j]])}, striped = TRUE))
    }
    display_sheet_plot <- function(n = "language", j = 1){
      return(output[[paste0("plot_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(),
                                                                        columns_to_summarise = j,
                                                                        replace = "rp.contact.field.",
                                                                        values = "percentage",
                                                                        fill_colour = "#4882BE")) # shades of blue
    )} # plottype = histogram
    
    # run our table_baselines and plot_baselines # TODO: in PLHr function, replace for loop with map like this.
    map2(data_baseline_survey$display_name, data_baseline_survey$object_name, .f = ~ display_sheet_table(n = .y, j = .x))
    map2(data_baseline_survey$metabase_ID, data_baseline_survey$object_name, .f = ~ display_sheet_plot(n = .y, j = .x))
    # map2(data_baseline_survey$display_name, data_baseline_survey$object_name, .f = ~ display_sheet_table(n = .y, j = .x))
    
    
    # bit different for age
    # RCT TODO HERE - not called age i nRCT
    #output$table_parent_age <- shiny::renderTable({(selected_data_dem() %>% summary_table(columns_to_summarise = rp.contact.field.user_age, factors = opt_factors(), summaries = "mmm"))}, striped = TRUE)
    #output$plot_parent_age <- renderPlotly({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.user_age", replace = "rp.contact.field.", plot_type = "histogram")})
    
    #App version
    plot_app_version  <- reactive({
      summary_plot(data = selected_data_dem(),
                   columns_to_summarise = "app_version",
                   replace = "rp.contact.field.",
                   values = "percentage",
                   fill_colour = "#4882BE")
      #summary_plot(plhdata_org_clean, app_version)
    })
    output$plot_app_version <- renderPlotly({plot_app_version()})
    
    # this one is used in additional content and in demographics
    data_engagement_weeks_all <- reactive({
      last_sync_data <- selected_data_dem() %>%
        mutate(group_since_sync = last_sync_cat()) %>%
        mutate(last_sync_cat = ifelse(group_since_sync == "4", "Last synced over 60 days ago",
                                      ifelse(group_since_sync == "3", "Last synced 30-59 days ago",
                                             ifelse(group_since_sync == "2", "Last synced 14-29 days ago",
                                                    ifelse(group_since_sync == "1", "Last synced less than 14 days ago", "0"))))) %>%
        mutate(last_sync_cat = fct_relevel(last_sync_cat, c("Last synced less than 14 days ago", "Last synced 14-29 days ago", "Last synced 30-59 days ago", "Last synced over 60 days ago"))) %>%
        dplyr::select(c(ClusterName, last_sync_cat, rp.contact.field.post_rct_access))
      return(last_sync_data)
    })
    
    data_engagement_weeks <- reactive({
      UIC_onboarding_dates_1_14 <- UIC_onboarding_dates %>% filter(`Weeks completed` <= 14) %>% pull(ClusterName)
      last_sync_data <- data_engagement_weeks_all() %>%
        mutate(Week = ifelse(ClusterName %in% UIC_onboarding_dates_1_14, "1-14", "15+"))
      
      last_sync_data_1 <- last_sync_data %>%
        group_by(Week, last_sync_cat) %>%
        summarise(last_sync = n()) %>%
        filter(last_sync_cat != 0)
      # last_sync_data_2 <- last_sync_data %>%
      #   group_by(Week) %>%
      #   summarise(last_sync = n()) %>%
      #   mutate(last_sync_cat = "Total")
      last_sync_data_3 <- last_sync_data %>%
        group_by(last_sync_cat) %>%
        summarise(last_sync = n()) %>%
        filter(last_sync_cat != 0) %>%
        mutate(Week = "Total")
      
      last_sync_data <- bind_rows(last_sync_data_1, last_sync_data_3)
      return(last_sync_data)
    })
    
    plot_engagement_weeks  <- reactive({
      ggplot(data_engagement_weeks(), aes(x = last_sync_cat, y = last_sync, fill = Week)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_brewer(palette = "Blues", direction = -1) +
        scale_x_discrete(labels = c("7 days", "14 days", "30 days", "60 days", "Total")) +
        labs(x = "Last sync", y = "Frequency")
      })
    output$plot_engagement_weeks <- renderPlotly({plot_engagement_weeks()})
    
    table_engagement_weeks  <- reactive({
      data_engagement_weeks() %>% pivot_wider(id_cols = "last_sync_cat", names_from = Week, values_from = last_sync, values_fill = 0)
    })
    output$table_engagement_weeks <- shiny::renderTable({table_engagement_weeks()}, striped = TRUE)
    
    #SECOND Tab Workshop Engagement Data
    
    # Workshop Engagement ---------------------------------------------------
    output$week_engagement <- shiny::renderTable({download_data_start() %>%
        group_by(engagement_level) %>% summarise(Number = n(), Percentage = n()/nrow(.) * 100)}, striped = TRUE)
    
    workshop_engagement_cut <- reactive({
      data <- selected_data_dem() %>%
        mutate(across(all_of(data_completion_level),
                      ~cut(.x, breaks = c(0, 1, 40, 80, 99, 100), include_lowest = TRUE,
                           labels = c("0", "1-40", "41-80", "81-99", "100")))) %>%
        mutate(across(all_of(data_completion_level), ~replace_na(.x, "0")))
      return(data)
    })
    
    summary_table_completion_level <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = workshop_engagement_cut(), columns_to_summarise = data_completion_level,
                                                               replace = "rp.contact.field.w_",
                                                               replace_after = "_completion_level")
      summary_table_baseline_build <- summary_table_baseline_build %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    relative_perc_completed <- reactive({
      summary_table_completion_level <- summary_table_completion_level()
      for (i in 1:length(summary_table_completion_level)){
        if (!"100" %in% names(summary_table_completion_level[[i]])){
          summary_table_completion_level[[i]]$`100` <- 0
        }
      }
      select_items <- c(opt_factors(), "n_started", "perc_started", "n_completed", "perc_completed")
      
      relative_perc_completed <- imap(summary_table_completion_level, ~.x %>%
                                        mutate(n_started = Total - `0`,
                                               perc_started = round(n_started/Total * 100, 1),
                                               perc_completed = round(`100`/n_started*100, 1),
                                               n_completed = `100`) %>%
                                        select(select_items))
      relative_perc_completed <- plyr::ldply(relative_perc_completed, `.id` = "Workshop")
      return(relative_perc_completed)   
    })
    
    # Started Workshop
    table_ws_started <- reactive({
      table_ws_started <- relative_perc_completed() %>%
        mutate(perc_started = paste0(n_started, " (", perc_started, "%)")) %>%
        pivot_wider(id_cols = opt_factors(), names_from = Workshop, values_from = perc_started)
      if (study == "RCT"){
        table_ws_started <- full_join(UIC_onboarding_dates, table_ws_started, multiple = "all")
        # table_ws_started$`Weeks completed`[length(table_ws_started$`Weeks completed`)] <- mean(UIC_onboarding_dates$`Weeks completed`, na.rm = TRUE)
      }
      return(table_ws_started)
    })
    plot_ws_started <- reactive({
      summary_mean_completion_level_long <- relative_perc_completed()
      if (country == "Tanzania"){
        if (study == "Optimisation"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>%
            tidyr::unite(col = "Org", opt_factors())
        } else if (study == "Pilot"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>%
            filter(PilotSite != "Total") %>% mutate(Org = PilotSite)
        } else if (study == "RCT"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>%
            filter(ClusterName == "Total")# %>% mutate(Org = ClusterName)
        }
      } else {
        summary_mean_completion_level_long <- summary_mean_completion_level_long %>% filter(Org != "Total")
      }
      
      plot <- ggplot(summary_mean_completion_level_long, aes(x = Workshop, y = perc_started))#, fill = Org)) # removing fill by ClusterName
      plot + geom_bar(stat = "identity", position = "dodge", fill = "#AAE8E7") +
        scale_x_discrete(guide = guide_axis(angle = 90), limits = week_order) +
        #viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "Workshop", y = "Started (%)")
    }) 
    output$table_ws_started <- shiny::renderTable({(table_ws_started())}, striped = TRUE)
    output$plot_ws_started <- renderPlotly({plot_ws_started()})
    
    # Completed Workshop
    table_ws_rel_completed <- reactive({
      table_perc_completed <- relative_perc_completed() %>%
        mutate(perc_completed = paste0(n_completed, " (", perc_completed, "%)")) %>%
        pivot_wider(id_cols = opt_factors(), names_from = Workshop, values_from = perc_completed)
      if (study == "RCT"){
        table_perc_completed <- full_join(UIC_onboarding_dates, table_perc_completed, multiple = "all")
        # table_perc_completed$`Weeks completed`[length(table_perc_completed$`Weeks completed`)] <- round(mean(UIC_onboarding_dates$`Weeks completed`, na.rm = TRUE), 0)
      }
      return(table_perc_completed)
    })
    plot_ws_rel_completed  <- reactive({
      summary_mean_completion_level_long <- relative_perc_completed()
      if (country == "Tanzania"){
        if (study == "Optimisation"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>%
            tidyr::unite(col = "Org", opt_factors())
        } else if (study == "Pilot"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>% filter(PilotSite != "Total") %>% mutate(Org = PilotSite)
        } else if (study == "RCT"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>% filter(ClusterName == "Total")# %>% mutate(Org = ClusterName)
        }
      } else {
        summary_mean_completion_level_long <- summary_mean_completion_level_long %>% filter(Org != "Total")
      }
      plot <- ggplot(summary_mean_completion_level_long, aes(x = `Workshop`, y = perc_completed))#, fill = Org)) # removing fill by ClusterName
      plot + geom_bar(stat = "identity", position = "dodge", fill = "#AAE8E7") +
        scale_x_discrete(guide = guide_axis(angle = 90), limits = week_order) +
        viridis::scale_fill_viridis(discrete = TRUE)+
        labs(x = "Workshop", y = "Completed (%)")
    }) 
    output$table_ws_rel_completed <- shiny::renderTable({(table_ws_rel_completed())}, striped = TRUE)
    output$plot_ws_rel_completed <- renderPlotly({plot_ws_rel_completed()})
    
    # Completion Level
    table_ws_totals <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      #mean average completion level per org
      # Percentage of users who completed a workshop out of those who started it
      # nrow(plhdata_org_clean %>% filter(rp.contact.field.w_money_completion_level == 100)) / nrow(plhdata_org_clean %>% filter(rp.contact.field.w_money_started == "true"))
      summary_mean_completion_level <- summary_table(data = selected_data_dem(),
                                                     columns_to_summarise = data_completion_level,
                                                     replace = "rp.contact.field.w_",
                                                     replace_after = "_completion_level",
                                                     summaries = "mean",
                                                     factors = opt_factors(),
                                                     include_margins = TRUE)
      if (study == "RCT"){
        summary_mean_completion_level <- full_join(UIC_onboarding_dates, summary_mean_completion_level, multiple = "all")
        # summary_mean_completion_level$`Weeks completed`[length(summary_mean_completion_level$`Weeks completed`)] <- round(mean(UIC_onboarding_dates$`Weeks completed`, na.rm = TRUE), 0)
      }
    })
    
    plot_ws_totals  <- reactive({
      if (study == "RCT"){
        table_ws_totals <- table_ws_totals() %>% filter(ClusterName == "Total") 
      } else {
        table_ws_totals <- table_ws_totals() 
      }
      hp_mood_plot(data = table_ws_totals, factors = opt_factors(), 
                   limits = week_order, xlab = NULL, manipulation = "longer", fill = FALSE,
                   fill_colour = "#AAE8E7")
    }) 
    output$table_ws_totals <- shiny::renderTable({(table_ws_totals())}, striped = TRUE)
    output$plot_ws_totals <- renderPlotly({plot_ws_totals()})
    
    #fill = "#19D1CD"
    
    # Additional Insights - Enaggement ------------------
    #Workshop plot and table
    ws_completion_table <- function(n, j = 1){
      return(output[[paste0("table_", n)]] <-  shiny::renderTable({(summary_table_completion_level()[[j]])}, striped = TRUE))
    }
    ws_completion_plot <- function(n, j = 1){
      return(output[[paste0("plot_", n)]] <-  renderPlotly(summary_plot(data = workshop_engagement_cut(),
                                                                        columns_to_summarise = j,
                                                                        replace = "rp.contact.field.w_",
                                                                        fill_colour = "#AAE8E7")))
    } # plottype = histogram
    
    # run our table_baselines and plot_baselines # TODO: in PLHr function, replace for loop with map like this.
    map2(data_completion_level_data$display_name, data_completion_level_data$object_name, .f = ~ ws_completion_table(n = .y, j = .x))
    map2(data_completion_level_data$metabase_ID, data_completion_level_data$object_name, .f = ~ ws_completion_plot(n = .y, j = .x))
    
    
    # New additional insights tab ---------------------------------------------
    data_engagement_additional_content <- reactive({
      engagement <- data_engagement_weeks_all() %>%
        filter(rp.contact.field.post_rct_access == "true") %>%
        group_by(last_sync_cat) %>%
        summarise(last_sync = n()) %>%
        filter(last_sync_cat != 0)
      return(engagement)
    })
    output$table_engagement_additional_content <- shiny::renderTable({data_engagement_additional_content()}, striped = TRUE)
    plot_engagement_additional_content  <- reactive({
      ggplot(data_engagement_additional_content(), aes(x = last_sync_cat, y = last_sync)) +
        geom_bar(stat = "identity", position = "dodge", fill = "#AAE8E7") +
        scale_x_discrete(labels = c("7 days", "14 days", "30 days", "60 days")) +
        labs(x = "Last sync", y = "Frequency")
    })
    output$plot_engagement_additional_content <- renderPlotly({plot_engagement_additional_content()})
    
    accessed_new_content <- reactive({
      selected_data_dem() %>% filter(rp.contact.field.post_rct_access == "true")
      })

    accessed_new_content_column <- reactive({
      accessed_new_content() %>%
        group_by(ClusterName) %>%
        summarise(`Number accessed` = n())
    })
    
    additional_engagement_cut <- reactive({
      data <- accessed_new_content() %>%
        mutate(across(all_of(new_modules_completion_level),
                      ~cut(.x, breaks = c(0, 1, 40, 80, 99, 100), include_lowest = TRUE,
                           labels = c("0", "1-40", "41-80", "81-99", "100")))) %>%
        mutate(across(all_of(new_modules_completion_level), ~replace_na(.x, "0")))
      return(data)
    })
    
    additional_summary_table_completion_level <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = additional_engagement_cut(), columns_to_summarise = new_modules_completion_level,
                                                               replace = "rp.contact.field.w_",
                                                               replace_after = "_completion_level")
      summary_table_baseline_build <- summary_table_baseline_build %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    additional_relative_perc_completed <- reactive({
      additional_summary_table_completion_level <- additional_summary_table_completion_level()
      for (i in 1:length(additional_summary_table_completion_level)){
        if (!"100" %in% names(additional_summary_table_completion_level[[i]])){
          additional_summary_table_completion_level[[i]]$`100` <- 0
        }
      }
      select_items <- c(opt_factors(), "n_started", "perc_started", "n_completed", "perc_completed")
      
      additional_relative_perc_completed <- imap(additional_summary_table_completion_level, ~.x %>%
                                                   mutate(n_started = Total - `0`,
                                                          perc_started = round(n_started/Total * 100, 1),
                                                          perc_completed = round(`100`/n_started*100, 1),
                                                          n_completed = `100`) %>%
                                                   select(all_of(select_items)))
      additional_relative_perc_completed <- plyr::ldply(additional_relative_perc_completed, `.id` = "Workshop")
      return(additional_relative_perc_completed)   
    })
    
    # Started Workshop
    additional_table_ws_started <- reactive({
      additional_table_ws_started <- additional_relative_perc_completed() %>%
        mutate(perc_started = paste0(n_started, " (", perc_started, "%)")) %>%
        pivot_wider(id_cols = opt_factors(), names_from = Workshop, values_from = perc_started)
      
      # this should be done elsewhere - adding the week # into the plhdata.
      if (study == "RCT"){
        additional_table_ws_started <- full_join(accessed_new_content_column(), additional_table_ws_started, multiple = "all")
        additional_table_ws_started <- full_join(UIC_onboarding_dates, additional_table_ws_started, multiple = "all") %>%
          mutate(`Number accessed` = replace_na(`Number accessed`, 0)) %>%
          dplyr::select(c("ClusterName", "Number accessed", "Weeks completed", "Srh", "Svp", "Grief", "Learn"))
        additional_table_ws_started$`Number accessed`[nrow(additional_table_ws_started)] <- sum(additional_table_ws_started$`Number accessed`)
        # additional_table_ws_started$`Weeks completed`[length(additional_table_ws_started$`Weeks completed`)] <- round(mean(UIC_onboarding_dates$`Weeks completed`, na.rm = TRUE), 0)
      }
      return(additional_table_ws_started)
    })
    additional_plot_ws_started <- reactive({
      summary_mean_completion_level_long <- additional_relative_perc_completed()
      if (country == "Tanzania"){
        if (study == "Optimisation"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>%
            tidyr::unite(col = "Org", opt_factors())
        } else if (study == "Pilot"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>%
            filter(PilotSite != "Total") %>% mutate(Org = PilotSite)
        } else if (study == "RCT"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>%
            filter(ClusterName == "Total")# %>% mutate(Org = ClusterName)
        }
      } else {
        summary_mean_completion_level_long <- summary_mean_completion_level_long %>% filter(Org != "Total")
      }
      
      plot <- ggplot(summary_mean_completion_level_long, aes(x = Workshop, y = perc_started))#, fill = Org)) # removing fill by ClusterName
      plot + geom_bar(stat = "identity", position = "dodge", fill = "#AAE8E7") +
        scale_x_discrete(guide = guide_axis(angle = 90), limits = additional_week_order) +
        viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "Workshop", y = "Started (%)")
    }) 
    output$additional_table_ws_started <- shiny::renderTable({(additional_table_ws_started())}, striped = TRUE)
    output$additional_plot_ws_started <- renderPlotly({additional_plot_ws_started()})
    
    # Completed Workshop
    additional_table_ws_rel_completed <- reactive({
      table_perc_completed <- additional_relative_perc_completed() %>%
        mutate(perc_completed = paste0(n_completed, " (", perc_completed, "%)")) %>%
        pivot_wider(id_cols = opt_factors(), names_from = Workshop, values_from = perc_completed)
      if (study == "RCT"){
        table_perc_completed <- full_join(accessed_new_content_column(), table_perc_completed, multiple = "all")
        table_perc_completed <- full_join(UIC_onboarding_dates, table_perc_completed, multiple = "all") %>%
          mutate(`Number accessed` = replace_na(`Number accessed`, 0)) %>%
          dplyr::select(c("ClusterName", "Number accessed", "Weeks completed", "Srh", "Svp", "Grief", "Learn"))
        table_perc_completed$`Number accessed`[nrow(table_perc_completed)] <- sum(table_perc_completed$`Number accessed`)
      }
      return(table_perc_completed)
    })
    additional_plot_ws_rel_completed  <- reactive({
      summary_mean_completion_level_long <- additional_relative_perc_completed()
      summary_mean_completion_level_long <- summary_mean_completion_level_long %>% filter(ClusterName == "Total")# %>% mutate(Org = ClusterName)
      plot <- ggplot(summary_mean_completion_level_long, aes(x = `Workshop`, y = perc_completed))#, fill = Org)) # removing fill by ClusterName
      plot + geom_bar(stat = "identity", position = "dodge", fill = "#AAE8E7") +
        scale_x_discrete(guide = guide_axis(angle = 90), limits = additional_week_order) +
        viridis::scale_fill_viridis(discrete = TRUE)+
        labs(x = "Workshop", y = "Completed (%)")
    }) 
    output$additional_table_ws_rel_completed <- shiny::renderTable({(additional_table_ws_rel_completed())}, striped = TRUE)
    output$additional_plot_ws_rel_completed <- renderPlotly({additional_plot_ws_rel_completed()})
    
    # Completion Level
    additional_table_ws_totals <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_mean_completion_level <- summary_table(data = accessed_new_content(),
                                                     columns_to_summarise = new_modules_completion_level,
                                                     replace = "rp.contact.field.w_",
                                                     replace_after = "_completion_level",
                                                     summaries = "mean",
                                                     factors = opt_factors(),
                                                     include_margins = TRUE)
      
      if (study == "RCT"){
        summary_mean_completion_level <- full_join(accessed_new_content_column(), summary_mean_completion_level, multiple = "all")
        summary_mean_completion_level <- full_join(UIC_onboarding_dates, summary_mean_completion_level, multiple = "all") %>%
          mutate(`Number accessed` = replace_na(`Number accessed`, 0)) %>%
          dplyr::select(c("ClusterName", "Number accessed", "Weeks completed", "Srh", "Svp", "Grief", "Learn"))
        summary_mean_completion_level$`Number accessed`[nrow(summary_mean_completion_level)] <- sum(summary_mean_completion_level$`Number accessed`)
      }
      return(summary_mean_completion_level)
    })
    
    additional_plot_ws_totals  <- reactive({
      additional_table_ws_totals <- additional_table_ws_totals() %>% filter(ClusterName == "Total")  %>% dplyr::select(-c("Weeks completed"))
      hp_mood_plot(data = additional_table_ws_totals, factors = opt_factors(), 
                   limits = additional_week_order, xlab = NULL, manipulation = "longer", fill = FALSE,
                   fill_colour = "#AAE8E7")
    }) 
    output$additional_table_ws_totals <- shiny::renderTable({(additional_table_ws_totals())}, striped = TRUE)
    output$additional_plot_ws_totals <- renderPlotly({additional_plot_ws_totals()})
    
    #Workshop plot and table
    additional_ws_completion_table <- function(n, j = 1){
      return(output[[paste0("table_", n)]] <-  shiny::renderTable({(additional_summary_table_completion_level()[[j]])}, striped = TRUE))
    }
    additional_ws_completion_plot <- function(n, j = 1){
      return(output[[paste0("plot_", n)]] <-  renderPlotly(summary_plot(data = additional_engagement_cut(), columns_to_summarise = j, replace = "rp.contact.field.w_", fill_colour = "#AAE8E7")))
    } # plottype = histogram
    
    # run our table_baselines and plot_baselines # TODO: in PLHr function, replace for loop with map like this.
    map2(c("Srh", "Svp", "Grief", "Learn"), c("w_srh", "w_svp", "w_grief", "w_learn"), .f = ~ additional_ws_completion_table(n = .y, j = .x))
    map2(c("rp.contact.field.w_learn_completion_level", "rp.contact.field.w_svp_completion_level", "rp.contact.field.w_grief_completion_level", "rp.contact.field.w_srh_completion_level"), c("w_learn", "w_svp", "w_grief", "w_srh"), .f = ~ additional_ws_completion_plot(n = .y, j = .x))
    
    # ACTIVITES - ADDITIONAL ----------
    
    # Number Started - at least one click:
    ltp_activities <- reactive({
      accessed_new_content() %>%
      #plhdata_org_clean %>% dplyr::filter(rp.contact.field.post_rct_access == "true") %>%
        dplyr::select(c(ClusterName, starts_with("rp.contact.field.ltp_"))) %>%
        dplyr::select(c(ClusterName, ends_with("click_history")))
    })
    
    # Total count
    total_count <- reactive({
      ltp_activities() %>%
      mutate(across(ends_with("click_history"), ~ stringr::str_count(.x, "T")))
    })
    
    # Calculation and Table of number started
    activity_table_started <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      number_started <- total_count() %>% 
        group_by(ClusterName) %>%
        summarise(across(ends_with("click_history"), ~ sum(!is.na(.x))))
      names(number_started) <- naming_conventions(names(number_started), replace = "rp.contact.field.ltp_activity_", replace_after = "_click_history")
      number_started <- number_started %>% janitor::adorn_totals(c("row", "col"))
    })
    
    # Plot of number started
    activity_plot_started  <- reactive({
      number_started_total <- activity_table_started() %>% filter(ClusterName == "Total") %>%
        pivot_longer(cols = !ClusterName) %>%
        filter(name != "Total")
      ltp_activites_names <- ltp_activites_name[ltp_activites_name %in% number_started_total$name]
      ggplot(number_started_total, aes(x = as_factor(name), y = value, fill = value)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none") +
        labs(x = "Activity", y = "Number started") +
        scale_fill_gradient(low = "#cc0000", high = "#00cc44") +
        scale_x_discrete(limits = ltp_activites_names)
    }) 
    output$activity_table_started <- shiny::renderTable({(activity_table_started())}, striped = TRUE)
    output$activity_plot_started <- renderPlotly({activity_plot_started()})
    
    
    # Calculation and Table of number started
    activity_table_total <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      total_count <- total_count()
      total_count$number_started <- apply(!is.na(total_count), 1, sum) - 1
      total_count %>%
        filter(number_started != 0) %>%
        group_by(ClusterName) %>%
        summarise(Mean = mean(number_started),
                  Max = max(number_started),
                  SD = sd(number_started))
    })
    
    # Plot of number started
    activity_plot_total  <- reactive({
      total_count <- total_count()
      total_count$number_started <- apply(!is.na(total_count), 1, sum) - 1
      ggplot(total_count %>% filter(number_started != 0), aes(x = number_started)) + geom_bar() + labs(x = "Number of activities started", y = "Number of users") +
        scale_fill_gradient(low = "#cc0000", high = "#00cc44")
    }) 
    output$activity_table_total <- shiny::renderTable({(activity_table_total())}, striped = TRUE)
    output$activity_plot_total <- renderPlotly({activity_plot_total()})
    
    
    # activity_plot_repeat
    # activity_table_repeat
    activity_table_repeat <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      ltp_activities_summary <- ltp_activities() %>%
      mutate(across(ends_with("_click_history"), ~stringr::str_detect(.x, ";"))) %>%
      pivot_longer(cols = !"ClusterName", names_to = "Activity") %>%
      group_by(Activity) %>%
      mutate(value = ifelse(value == TRUE, 1, 0)) %>%
      summarise(`Number of users` = sum(value, na.rm = TRUE)) %>%
      filter(`Number of users` != "0")
    ltp_activities_summary$Activity <- 
      naming_conventions(ltp_activities_summary$Activity, "rp.contact.field.ltp_activity_", "_click_history")
    return(ltp_activities_summary)
    })
    activity_plot_repeat  <- reactive({
      ltp_activities_summary <- activity_table_repeat()
      ltp_activites_name <- ltp_activites_name[ltp_activites_name %in% ltp_activities_summary$Activity]
      ggplot(ltp_activities_summary, aes(x = Activity, y = `Number of users`, fill = `Number of users`)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none") +
        scale_fill_gradient(low = "#cc0000", high = "#00cc44") +
        scale_x_discrete(limits = ltp_activites_name)
    }) 
    output$activity_table_repeat <- shiny::renderTable({(activity_table_repeat())}, striped = TRUE)
    output$activity_plot_repeat <- renderPlotly({activity_plot_repeat()})
    
    
    # # for each activity - if interested
    # naming_ltp_activites <- paste0("rp.contact.field.ltp_activity_", ltp_activites, "_click_history")
    # ltp_complete <- purrr::map(.x = naming_ltp_activites,
    #                            .f = ~ total_count %>%
    #                              dplyr::select(ClusterName, .x) %>%
    #                              dplyr::filter(!is.na(get(.x))) %>%
    #                              group_by(ClusterName, get(.x)) %>%
    #                              summarise(n()) %>%
    #                              pivot_wider(names_from = `get(.x)`, values_from = `n()`, values_fill = 0))
    # # 

    # Self reported - done
    ltp_activities_done <- reactive({
      accessed_new_content() %>%
        dplyr::select(c(ClusterName, starts_with("rp.contact.field.ltp_"))) %>%
        dplyr::select(c(ClusterName, ends_with("_hp_done")))
    })
    
    # self reported = yes' ?
    activity_table_done <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      ltp_activities_done <- ltp_activities_done() %>%
        dplyr::mutate(across(ends_with("_hp_done"), ~str_count(.x, "yes"))) %>% # count number of yes'
        group_by(ClusterName) %>%
        summarise(across(ends_with("_hp_done"), ~sum(.x, na.rm = TRUE)))
      names(ltp_activities_done) <- naming_conventions(names(ltp_activities_done), replace = "rp.contact.field.ltp_activity_", replace_after = "_hp_done")
      ltp_activities_done <- ltp_activities_done %>% janitor::adorn_totals(c("row", "col"))
      return(ltp_activities_done)
    })
    
    activity_plot_done  <- reactive({
      ltp_activities_done_plot <- activity_table_done() %>% filter(ClusterName == "Total") %>%
        pivot_longer(cols = !ClusterName) %>%
        filter(name != "Total")
      ltp_activites_names <- ltp_activites_name[ltp_activites_name %in% ltp_activities_done_plot$name]
      ggplot(ltp_activities_done_plot, aes(x = as_factor(name), y = value, fill = value)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none") +
        labs(x = "Activity", y = "Number completed (self reported)") +
        scale_fill_gradient(low = "#cc0000", high = "#00cc44") +
        scale_x_discrete(limits = ltp_activites_names)
    }) 
    output$activity_table_done <- shiny::renderTable({(activity_table_done())}, striped = TRUE)
    output$activity_plot_done <- renderPlotly({activity_plot_done()})
    
    # Parent Points Tab -----------------------------------------------
    values <- reactiveValues(total = 0)
    observeEvent(input$OrgDem, { values$total <- values$total + 1 })
    
    summary_table_habits_all <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(), columns_to_summarise = data_habit_parent_points_all,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "_completion_level") %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    #Table of averages
    table_pp_totals <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_total_habits <- summary_table(data = selected_data_dem(),
                                            columns_to_summarise = data_habit_parent_points_all,
                                            replace = "rp.contact.field.parent_point_count_",
                                            summaries = "sum",
                                            factors = opt_factors())
      summary_total_habits %>% janitor::adorn_totals(c("row", "col"))
    })
    plot_pp_totals  <- reactive({
      summary_mean_completion_level_long <- pivot_longer(table_pp_totals(), cols = !opt_factors(), names_to = "Parent Points", values_to = "Value") %>%
        filter(`Parent Points` != "Total")
      if (country == "Tanzania"){
        if (study == "Optimisation"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>%
            tidyr::unite(col = "Org", opt_factors())
          # no not to string
        } else if (study == "Pilot"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>% mutate(Org = opt_factors())
        } else if (study == "RCT"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>% mutate(Org = ClusterName)
        }
      } else {
        summary_mean_completion_level_long <- summary_mean_completion_level_long %>% filter(Org != "Total")
      }
      ggplot(summary_mean_completion_level_long, aes(x = Org, y = Value, fill = `Parent Points`)) + 
        geom_bar(stat = "identity", position = "fill") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "Organisation", title = "Proportion of parent points given in each category within an organisation")
    }) 
    output$table_pp_totals <- shiny::renderTable({(table_pp_totals())}, striped = TRUE, caption = "Total parent points for each category split by organisation")
    output$plot_pp_totals <- renderPlotly({plot_pp_totals()})
    
    #Table of averages
    table_pp_means <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_mean_habits <- summary_table(data = selected_data_dem(),
                                           columns_to_summarise = data_habit_parent_points_all,
                                           replace = "rp.contact.field.parent_point_count_",
                                           summaries = "mean",
                                           factors = opt_factors())
      summary_mean_habits %>% janitor::adorn_totals(c("row", "col"))
    }) 
    plot_pp_means  <- reactive({
      summary_mean_completion_level_long <- pivot_longer(table_pp_means(), cols = !opt_factors(), names_to = "Parent Points", values_to = "Value") %>%
        filter(`Parent Points` != "Total")
      if (country == "Tanzania"){
        if (study == "Optimisation"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>%
            tidyr::unite(col = "Org", opt_factors())
        } else if (study == "Pilot"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>% mutate(Org = opt_factors())
        } else if (study == "RCT"){
          summary_mean_completion_level_long <- summary_mean_completion_level_long %>% mutate(Org = ClusterName)
        }
      } else {
        summary_mean_completion_level_long <- summary_mean_completion_level_long %>% filter(Org != "Total")
      }
      ggplot(summary_mean_completion_level_long, aes(x = Org, y = Value, fill = `Parent Points`)) + 
        geom_bar(stat = "identity", position = "fill") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "Organisation", title = "Proportion of parent points given in each category within an organisation")
    }) 
    output$table_pp_means <- shiny::renderTable({(table_pp_means())}, striped = TRUE, caption = "Mean parent points for each category split by organisation")
    output$plot_pp_means <- renderPlotly({plot_pp_means()})
    
    # pp_ table and plots --------------------
    pp_completion_table <- function(n, j = 1){
      return(output[[paste0("table_pp_", n)]] <-  shiny::renderTable({(summary_table_habits_all()[[j]])}, striped = TRUE))
    }
    pp_completion_plot <- function(n, j){
      return(output[[paste0("plot_pp_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(), columns_to_summarise = j, replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")))
    }
    map2(data_habit_parent_points_data$display_name, data_habit_parent_points_data$object_name, .f = ~ pp_completion_table(n = .y, j = .x))
    map2(data_habit_parent_points_data$metabase_ID, data_habit_parent_points_data$object_name, .f = ~ pp_completion_plot(n = .y, j = .x))
    
    #Parent Point sub tab Relax points pp1 ----------------------------------------------------------
    table_pp_relax_ws_totals <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      selected_data_relax <- add_na_variable(data = selected_data_dem(), variable = relax_workshop_vars)
      summary_relax_workshop <- summary_table(data = selected_data_relax,
                                              factors = opt_factors(),
                                              columns_to_summarise = relax_workshop_vars,
                                              summaries = c("mean"),
                                              replace = "rp.contact.field.parent_point_count_relax_w_")
      summary_relax_workshop <- summary_relax_workshop %>%
        mutate_all(~replace(., is.na(.), 0))
      
      summary_relax_workshop %>% janitor::adorn_totals(c("row", "col"))
    })
    output$table_pp_relax_ws_totals <- shiny::renderTable({table_pp_relax_ws_totals()})
    plot_pp_relax_ws_totals <- reactive({
      plot_totals_function(table_pp_relax_ws_totals(), factors = opt_factors())
    })
    output$plot_pp_relax_ws_totals <- renderPlotly({plot_pp_relax_ws_totals()})
    
    summary_table_habits_relax <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(),
                                                               columns_to_summarise = pp_metabase_ID$Relax, replace = "rp.contact.field.parent_point_count_relax_w_") %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      
      return(summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col"))))
    })
    
    summary_table_habits_treat_yourself <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(),
                                                               columns_to_summarise = pp_metabase_ID$`Treat yourself`,
                                                               replace = "rp.contact.field.parent_point_count_treat_yourself_w_") %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    summary_table_habits_praise_yourself <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(),
                                                               columns_to_summarise = pp_metabase_ID$`Praise yourself`,
                                                               replace = "rp.contact.field.parent_point_count_praise_yourself_w_") %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    summary_table_habits_spend_time <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(),
                                                               columns_to_summarise = pp_metabase_ID$`Spend time`,
                                                               replace = "rp.contact.field.parent_point_count_spend_time_w_") %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    summary_table_habits_praise_teen <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(),
                                                               columns_to_summarise = pp_metabase_ID$`Praise teen`,
                                                               replace = "rp.contact.field.parent_point_count_praise_teen_w_") %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    summary_table_habits_instruct <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(),
                                                               columns_to_summarise = pp_metabase_ID$`Instruct positively`,
                                                               replace = "rp.contact.field.parent_point_count_instruct_positively_w_") %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    summary_table_habits_breathe <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(),
                                                               columns_to_summarise = pp_metabase_ID$`Breathe`,
                                                               replace = "rp.contact.field.parent_point_count_breathe_w_") %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    summary_table_habits_consequence <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(), 
                                                               columns_to_summarise = pp_metabase_ID$`Consequence`,
                                                               replace = "rp.contact.field.parent_point_count_consequence_w_") %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    summary_table_habits_safe <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(), 
                                                               columns_to_summarise = pp_metabase_ID$`Safe`,
                                                               replace = "rp.contact.field.parent_point_count_safe_w_") %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    #Workshop plot and table
    # TODO: want to iterate this to avoid repeating code
    pp_table_relax <- function(n, j = 1){ return(output[[paste0("table_pp_", n)]] <- shiny::renderTable({(summary_table_habits_relax()[[j]])}, striped = TRUE))}
    pp_table_treat_yourself <- function(n, j = 1){return(output[[paste0("table_pp_", n)]] <- shiny::renderTable({(summary_table_habits_treat_yourself()[[j]])}, striped = TRUE))}
    pp_table_praise_yourself <- function(n, j = 1){return(output[[paste0("table_pp_", n)]] <- shiny::renderTable({(summary_table_habits_praise_yourself()[[j]])}, striped = TRUE))}
    pp_table_praise_teen <- function(n, j = 1){return(output[[paste0("table_pp_", n)]] <- shiny::renderTable({(summary_table_habits_praise_teen()[[j]])}, striped = TRUE))}
    pp_table_spend_time <- function(n, j = 1){return(output[[paste0("table_pp_", n)]] <- shiny::renderTable({(summary_table_habits_spend_time()[[j]])}, striped = TRUE))}
    pp_table_instruct <- function(n, j = 1){return(output[[paste0("table_pp_", n)]] <- shiny::renderTable({(summary_table_habits_instruct()[[j]])}, striped = TRUE))}
    pp_table_breathe <- function(n, j = 1){return(output[[paste0("table_pp_", n)]] <- shiny::renderTable({(summary_table_habits_breathe()[[j]])}, striped = TRUE))}
    pp_table_money <- function(n, j = 1){return(output[[paste0("table_pp_", n)]] <- shiny::renderTable({(summary_table_habits_money()[[j]])}, striped = TRUE))}
    pp_table_consequence <- function(n, j = 1){return(output[[paste0("table_pp_", n)]] <- shiny::renderTable({(summary_table_habits_consequence()[[j]])}, striped = TRUE))}
    pp_table_safe <- function(n, j = 1){return(output[[paste0("table_pp_", n)]] <- shiny::renderTable({(summary_table_habits_safe()[[j]])}, striped = TRUE))}
    pp_plot_relax <- function(n, j = 1){ return(output[[paste0("plot_pp_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(), columns_to_summarise = j, replace = "rp.contact.field.parent_point_count_relax_w_", plot_type = "boxplot")))} # plottype = histogram
    pp_plot_treat_yourself <- function(n, j = 1){ return(output[[paste0("plot_pp_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(), columns_to_summarise = j, replace = "rp.contact.field.parent_point_count_treat_yourself_w_", plot_type = "boxplot")))} # plottype = histogram
    pp_plot_praise_yourself <- function(n, j = 1){ return(output[[paste0("plot_pp_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(), columns_to_summarise = j, replace = "rp.contact.field.parent_point_count_praise_yourself_w_", plot_type = "boxplot")))} # plottype = histogram
    pp_plot_praise_teen <- function(n, j = 1){ return(output[[paste0("plot_pp_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(), columns_to_summarise = j, replace = "rp.contact.field.parent_point_count_praise_teen_w_", plot_type = "boxplot")))} # plottype = histogram
    pp_plot_spend_time <- function(n, j = 1){ return(output[[paste0("plot_pp_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(), columns_to_summarise = j, replace = "rp.contact.field.parent_point_count_spend_time_w_", plot_type = "boxplot")))} # plottype = histogram
    pp_plot_instruct <- function(n, j = 1){ return(output[[paste0("plot_pp_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(), columns_to_summarise = j, replace = "rp.contact.field.parent_point_count_instruct_w_", plot_type = "boxplot")))} # plottype = histogram
    pp_plot_breathe <- function(n, j = 1){ return(output[[paste0("plot_pp_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(), columns_to_summarise = j, replace = "rp.contact.field.parent_point_count_breathe_w_", plot_type = "boxplot")))} # plottype = histogram
    pp_plot_money <- function(n, j = 1){ return(output[[paste0("plot_pp_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(), columns_to_summarise = j, replace = "rp.contact.field.parent_point_count_money_w_", plot_type = "boxplot")))} # plottype = histogram
    pp_plot_consequence <- function(n, j = 1){ return(output[[paste0("plot_pp_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(), columns_to_summarise = j, replace = "rp.contact.field.parent_point_count_consequence_w_", plot_type = "boxplot")))} # plottype = histogram
    pp_plot_safe <- function(n, j = 1){ return(output[[paste0("plot_pp_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(), columns_to_summarise = j, replace = "rp.contact.field.parent_point_count_safe_w_", plot_type = "boxplot")))} # plottype = histogram
    
    add_per_tab <- c("_w_self_care", "_w_1on1", "_w_praise", "_w_instruct", "_w_stress", "_w_solve", "_w_money", "_w_rules", "_w_consequence", "_w_safe", "_w_crisis", "_w_celebrate")
    name_per_tab <- c("Self care", "1on1", "Praise", "Instruct", "Stress", "Solve", "Money", "Rules", "Consequence", "Safe", "Crisis", "Celebrate")
    pp_object_names <- map(data_habit_parent_points_data$object_name, ~ paste0(.x, add_per_tab))
    pp_metabase_ID <- map(data_habit_parent_points_data$metabase_ID, ~ paste0(.x, add_per_tab))
    names(pp_object_names) <- data_habit_parent_points_data$display_name
    names(pp_metabase_ID) <- data_habit_parent_points_data$display_name
    map2(pp_object_names$Relax, name_per_tab, .f = ~ pp_table_relax(n = .x, j = .y))
    map2(pp_object_names$`Treat yourself`, name_per_tab, .f = ~ pp_table_treat_yourself(n = .x, j = .y))
    map2(pp_object_names$`Praise yourself`, name_per_tab, .f = ~ pp_table_praise_yourself(n = .x, j = .y))
    map2(pp_object_names$`Praise teen`, name_per_tab, .f = ~ pp_table_praise_teen(n = .x, j = .y))
    map2(pp_object_names$`Spend time`, name_per_tab, .f = ~ pp_table_spend_time(n = .x, j = .y))
    map2(pp_object_names$`Instruct positively`, name_per_tab, .f = ~ pp_table_instruct(n = .x, j = .y))
    map2(pp_object_names$Breathe, name_per_tab, .f = ~ pp_table_breathe(n = .x, j = .y))
    map2(pp_object_names$Money, name_per_tab, .f = ~ pp_table_money(n = .x, j = .y))
    map2(pp_object_names$Consequence, name_per_tab, .f = ~ pp_table_consequence(n = .x, j = .y))
    map2(pp_object_names$Safe, name_per_tab, .f = ~ pp_table_safe(n = .x, j = .y))
    map2(pp_object_names$Relax, pp_metabase_ID$Relax, .f = ~ pp_plot_relax(n = .x, j = .y))
    map2(pp_object_names$`Treat yourself`, pp_metabase_ID$`Treat yourself`, .f = ~ pp_plot_treat_yourself(n = .x, j = .y))
    map2(pp_object_names$`Praise yourself`, pp_metabase_ID$`Praise yourself`, .f = ~ pp_plot_praise_yourself(n = .x, j = .y))
    map2(pp_object_names$`Praise teen`, pp_metabase_ID$`Praise teen`, .f = ~ pp_plot_praise_teen(n = .x, j = .y))
    map2(pp_object_names$`Spend time`, pp_metabase_ID$`Spend time`, .f = ~ pp_plot_spend_time(n = .x, j = .y))
    map2(pp_object_names$`Instruct positively`, pp_metabase_ID$`Instruct positively`, .f = ~ pp_plot_instruct(n = .x, j = .y))
    map2(pp_object_names$Breathe, pp_metabase_ID$Breathe, .f = ~ pp_plot_breathe(n = .x, j = .y))
    map2(pp_object_names$Money, pp_metabase_ID$Money, .f = ~ pp_plot_money(n = .x, j = .y))
    map2(pp_object_names$Consequence, pp_metabase_ID$Consequence, .f = ~ pp_plot_consequence(n = .x, j = .y))
    map2(pp_object_names$Safe, pp_metabase_ID$Safe, .f = ~ pp_plot_safe(n = .x, j = .y))
    
    # This runs on open, etc.
    # for (i in c("_w_self_care", "_w_1on1", "_w_praise", "_w_instruct", "_w_stress", "_w_money", "_w_rules", "_w_consequence", "_w_solve", "_w_safe", "_w_crisis", "_w_celebrate")){
    #   print(i)
    #   map2(paste0(data_habit_parent_points_data$metabase_ID, i), paste0(data_habit_parent_points_data$object_name, i), .f = ~ pp_plot_self_care(n = .y, j = .x))
    # }
    
    # Treat Yourself Tab ----------------------------------------------------------------------------
    #Parent Point sub tab Treat Yourself points pp2
    table_pp_treat_yourself_ws_totals <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      selected_data_treat <- add_na_variable(data = selected_data_dem(), variable = treat_yourself_workshop_vars)
      summary_treat_yourself_workshop <- summary_table(data = selected_data_treat,
                                                       factors = opt_factors(),
                                                       columns_to_summarise = treat_yourself_workshop_vars,
                                                       summaries = c("mean"),
                                                       replace = "rp.contact.field.parent_point_count_treat_yourself_w_") %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      summary_treat_yourself_workshop %>% janitor::adorn_totals(c("row", "col"))
    })
    plot_pp_treat_yourself_ws_totals <- reactive({
      plot_totals_function(table_pp_treat_yourself_ws_totals(), factors = opt_factors())
    })
    output$table_pp_treat_yourself_ws_totals <- shiny::renderTable({table_pp_treat_yourself_ws_totals()})
    output$plot_pp_treat_yourself_ws_totals <- renderPlotly({plot_pp_treat_yourself_ws_totals()})
    
    # Parent Point sub tab Praise Yourself points pp3
    table_pp_praise_yourself_ws_totals <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      selected_data_praise <- add_na_variable(data = selected_data_dem(), variable = praise_yourself_workshop_vars)
      summary_praise_yourself_workshop <- summary_table(data = selected_data_praise,
                                                        factors = opt_factors(),
                                                        columns_to_summarise = praise_yourself_workshop_vars,
                                                        summaries = c("mean"),
                                                        replace = "rp.contact.field.parent_point_count_praise_yourself_w_")
      summary_praise_yourself_workshop %>% janitor::adorn_totals(c("row", "col"))
    })
    plot_pp_praise_yourself_ws_totals <- reactive({
      plot_totals_function(table_pp_praise_yourself_ws_totals(), factors = opt_factors())
    })
    output$table_pp_praise_yourself_ws_totals <- shiny::renderTable({table_pp_praise_yourself_ws_totals()})
    output$plot_pp_praise_yourself_ws_totals <- renderPlotly({plot_pp_praise_yourself_ws_totals()})
    
    # Parent Point sub tab Spend Time points pp4
    table_pp_spend_time_ws_totals <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      selected_data_time <- add_na_variable(data = selected_data_dem(), variable = spend_time_workshop_vars)
      summary_spend_time_workshop <- summary_table(data = selected_data_time,
                                                   factors = opt_factors(),
                                                   columns_to_summarise = spend_time_workshop_vars,
                                                   summaries = c("mean"),
                                                   replace = "rp.contact.field.parent_point_count_spend_time_w_")
      summary_spend_time_workshop %>% janitor::adorn_totals(c("row", "col"))
    })
    plot_pp_spend_time_ws_totals <- reactive({
      plot_totals_function(table_pp_spend_time_ws_totals(), factors = opt_factors())
    })
    output$table_pp_spend_time_ws_totals <- shiny::renderTable({table_pp_spend_time_ws_totals()})
    output$plot_pp_spend_time_ws_totals <- renderPlotly({plot_pp_spend_time_ws_totals()})
    
    
    # Parent Point sub tab Praise Teen points pp5
    table_pp_praise_teen_ws_totals <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      selected_data_praise <- add_na_variable(data = selected_data_dem(), variable = praise_teen_workshop_vars)
      summary_praise_teen_workshop <- summary_table(data = selected_data_praise,
                                                    factors = opt_factors(),
                                                    columns_to_summarise = praise_teen_workshop_vars,
                                                    summaries = c("mean"),
                                                    replace = "rp.contact.field.parent_point_count_praise_teen_w_")
      summary_praise_teen_workshop %>% janitor::adorn_totals(c("row", "col"))
    })
    plot_pp_praise_teen_ws_totals <- reactive({
      plot_totals_function(table_pp_praise_teen_ws_totals(), factors = opt_factors())
    })
    output$table_pp_praise_teen_ws_totals <- shiny::renderTable({table_pp_praise_teen_ws_totals()})
    output$plot_pp_praise_teen_ws_totals <- renderPlotly({plot_pp_praise_teen_ws_totals()})
    
    
    # Parent Point sub tab Instruct Positively points pp6
    table_pp_instruct_positively_ws_totals <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      selected_data_ip <- add_na_variable(data = selected_data_dem(), variable = instruct_positively_workshop_vars)
      summary_instruct_positively_workshop <- summary_table(data = selected_data_ip,
                                                            factors = opt_factors(),
                                                            columns_to_summarise = instruct_positively_workshop_vars,
                                                            summaries = c("mean"),
                                                            replace = "rp.contact.field.parent_point_count_instruct_positively_w_")
      summary_instruct_positively_workshop %>% janitor::adorn_totals(c("row", "col"))
    })
    plot_pp_instruct_positively_ws_totals <- reactive({
      plot_totals_function(table_pp_instruct_positively_ws_totals(), factors = opt_factors())
    })
    output$table_pp_instruct_positively_ws_totals <- shiny::renderTable({table_pp_instruct_positively_ws_totals()})
    output$plot_pp_instruct_positively_ws_totals <- renderPlotly({plot_pp_instruct_positively_ws_totals()})
    
    
    # Parent Point sub tab Breathe points pp7
    table_pp_breathe_ws_totals <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      selected_data_breathe <- add_na_variable(data = selected_data_dem(), variable = breathe_workshop_vars)
      summary_breathe_workshop <- summary_table(data = selected_data_breathe,
                                                factors = opt_factors(),
                                                columns_to_summarise = breathe_workshop_vars,
                                                summaries = c("mean"),
                                                replace = "rp.contact.field.parent_point_count_breathe_w_")
      summary_breathe_workshop %>% janitor::adorn_totals(c("row", "col"))
    })
    plot_pp_breathe_ws_totals <- reactive({
      plot_totals_function(table_pp_breathe_ws_totals(), factors = opt_factors())
    })
    output$table_pp_breathe_ws_totals <- shiny::renderTable({table_pp_breathe_ws_totals()})
    output$plot_pp_breathe_ws_totals <- renderPlotly({plot_pp_breathe_ws_totals()})
    
    
    # Parent Point sub tab Money points pp8
    table_pp_money_ws_totals <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      selected_data_cons <- add_na_variable(data = selected_data_dem(), variable = consequence_workshop_vars)
      summary_consequence_workshop <- summary_table(data = selected_data_cons,
                                                    factors = opt_factors(),
                                                    columns_to_summarise = consequence_workshop_vars,
                                                    summaries = c("mean"),
                                                    replace = "rp.contact.field.parent_point_count_consequence_w_")
      summary_consequence_workshop %>% janitor::adorn_totals(c("row", "col"))
    })
    plot_pp_consequence_ws_totals <- reactive({
      plot_totals_function(table_pp_consequence_ws_totals(), factors = opt_factors())
    })
    output$table_pp_consequence_ws_totals <- shiny::renderTable({table_pp_consequence_ws_totals()})
    output$plot_pp_consequence_ws_totals <- renderPlotly({plot_pp_consequence_ws_totals()})
    
    
    # Parent Point sub tab Safe points pp10
    table_pp_safe_ws_totals <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      selected_data_safe <- add_na_variable(data = selected_data_dem(), variable = safe_workshop_vars)
      summary_safe_workshop <- summary_table(data = selected_data_safe,
                                             factors = opt_factors(),
                                             columns_to_summarise = safe_workshop_vars,
                                             summaries = c("mean"),
                                             replace = "rp.contact.field.parent_point_count_safe_w_")
      summary_safe_workshop %>% janitor::adorn_totals(c("row", "col"))
    })
    plot_pp_safe_ws_totals <- reactive({
      plot_totals_function(table_pp_safe_ws_totals(), factors = opt_factors())
    })
    output$table_pp_safe_ws_totals <- shiny::renderTable({table_pp_safe_ws_totals()})
    output$plot_pp_safe_ws_totals <- renderPlotly({plot_pp_safe_ws_totals()})
    
    #FOURTH Tab In-week Engagement ---------------------------
    tables_app_opens <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(),
                                                               data = selected_data_dem(),
                                                               columns_to_summarise = data_app_opens,
                                                               replace = "rp.contact.field.app_launch_count",
                                                               retain_names_for_refactor = TRUE)
      names(summary_table_baseline_build) <- data_app_opens_neat
      print(summary_table_baseline_build[[1]])
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    # summary app opens table
    table_appopen_summary <- reactive({
      selected_data_dem() %>%
        group_by(across(opt_factors())) %>%
        summarise(Min = min(rp.contact.field.app_launch_count, na.rm = TRUE),
                  Mean = mean(rp.contact.field.app_launch_count, na.rm = TRUE),
                  Max = max(rp.contact.field.app_launch_count, na.rm = TRUE),
                  SD = sd(rp.contact.field.app_launch_count, na.rm = TRUE))
    }) 
    output$table_appopen_summary <- shiny::renderTable({(table_appopen_summary())}, striped = TRUE)
    
    #App Opens tab 4.1
    table_appopen_totals <- reactive({
      print(tables_app_opens()$`Overall`)
      tables_app_opens()$`Overall`
    }) 
    plot_appopen_totals <- reactive({
      plot_fn <- plot_totals_function(table_appopen_totals(), opt_factors())
      v <- as.numeric(as.character(plot_fn$data$name))
      plot_fn + labs(x = "App opens", y = "Frequency") + scale_y_continuous(limits = c(0, max(plot_fn$data$value))) +
        scale_x_discrete(limits = c(min(v, na.rm = TRUE):max(v, na.rm = TRUE)))
    }) 
    output$table_appopen_totals <- shiny::renderTable({(table_appopen_totals())}, striped = TRUE)
    output$plot_appopen_totals <- renderPlotly({plot_appopen_totals()})
    
    table_appopen_mean_week <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      #Average app opens per ws week
      selected_data_ao <- add_na_variable(data = selected_data_dem(), variable = data_app_opens)
      summary_mean_appopens <- summary_table(data = selected_data_ao, factors = opt_factors(),
                                             summaries = "mean", columns_to_summarise = data_app_opens,
                                             replace = "rp.contact.field.app_launch_count_w_")
      summary_mean_appopens <- rename(summary_mean_appopens, "Total" = `Rp.contact.field.app launch count`)
      summary_mean_appopens %>% janitor::adorn_totals(c("row", "col"))
    }) 
    plot_appopen_mean_week <- reactive({
      plot_fn <- plot_totals_function(table_appopen_mean_week(), opt_factors())
      plot_fn + labs(y = "Mean App Opens")
    }) 
    output$table_appopen_mean_week <- shiny::renderTable({(table_appopen_mean_week())}, striped = TRUE)
    output$plot_appopen_mean_week <- renderPlotly({plot_appopen_mean_week()})
    
    table_appopen_self_care <- reactive({
      tables_app_opens()$`Self Care (1)`
    }) 
    plot_appopen_self_care <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.app_launch_count_w_self_care", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot", group = "ClusterName")
    })
    output$table_appopen_self_care <- shiny::renderTable({(table_appopen_self_care())}, striped = TRUE)
    output$plot_appopen_self_care <- renderPlotly({plot_appopen_self_care()})
    
    table_appopen_1on1 <- reactive({
      tables_app_opens()$`1on1 (2)`
    }) 
    plot_appopen_1on1 <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.app_launch_count_w_1on1", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot", group = "ClusterName")
    })
    output$table_appopen_1on1 <- shiny::renderTable({(table_appopen_1on1())}, striped = TRUE)
    output$plot_appopen_1on1 <- renderPlotly({plot_appopen_1on1()})
    
    table_appopen_praise <- reactive({
      tables_app_opens()$`Praise (3)`
    }) 
    plot_appopen_praise <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.app_launch_count_w_praise", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot", group = "ClusterName")
    })
    output$table_appopen_praise <- shiny::renderTable({(table_appopen_praise())}, striped = TRUE)
    output$plot_appopen_praise <- renderPlotly({plot_appopen_praise()})
    
    
    table_appopen_instructions <- reactive({
      tables_app_opens()$`Positive Instructions(4)`
    }) 
    plot_appopen_instructions <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.app_launch_count_w_instruct", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot", group = "ClusterName")
    })
    output$table_appopen_instructions <- shiny::renderTable({(table_appopen_instructions())}, striped = TRUE)
    output$plot_appopen_instructions <- renderPlotly({plot_appopen_instructions()})
    
    
    table_appopen_stress <- reactive({
      tables_app_opens()$`Managing Stress(5)`
    }) 
    plot_appopen_stress <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.app_launch_count_w_stress", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot", group = "ClusterName")
    })
    output$table_appopen_stress <- shiny::renderTable({(table_appopen_stress())}, striped = TRUE)
    output$plot_appopen_stress <- renderPlotly({plot_appopen_stress()})
    
    
    table_appopen_budget <- reactive({
      tables_app_opens()$`Family Budget(6)`
    }) 
    plot_appopen_budget <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.app_launch_count_w_money", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot", group = "ClusterName")
    })
    output$table_appopen_budget <- shiny::renderTable({(table_appopen_budget())}, striped = TRUE)
    output$plot_appopen_budget <- renderPlotly({plot_appopen_budget()})
    
    
    table_appopen_rules <- reactive({
      tables_app_opens()$`Rules(7)`
    }) 
    plot_appopen_rules <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.app_launch_count_w_rules", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot", group = "ClusterName")
    })
    output$table_appopen_rules <- shiny::renderTable({(table_appopen_rules())}, striped = TRUE)
    output$plot_appopen_rules <- renderPlotly({plot_appopen_rules()})
    
    
    table_appopen_consequences <- reactive({
      tables_app_opens()$`Calm Consequences(8)`
    }) 
    plot_appopen_consequences <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.app_launch_count_w_consequence", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot", group = "ClusterName")
    })
    output$table_appopen_consequences <- shiny::renderTable({(table_appopen_consequences())}, striped = TRUE)
    output$plot_appopen_consequences <- renderPlotly({plot_appopen_consequences()})
    
    
    table_appopen_problem_solving <- reactive({
      tables_app_opens()$`Problem Solving(6)`
    }) 
    plot_appopen_problem_solving <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.app_launch_count_w_solve", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot", group = "ClusterName")
    })
    output$table_appopen_problem_solving <- shiny::renderTable({(table_appopen_problem_solving())}, striped = TRUE)
    output$plot_appopen_problem_solving <- renderPlotly({plot_appopen_problem_solving()})
    
    
    table_appopen_teen_safety <- reactive({
      tables_app_opens()$`Teen Safety(10)`
    }) 
    plot_appopen_teen_safety <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.app_launch_count_w_safe", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot", group = "ClusterName")
    })
    output$table_appopen_teen_safety <- shiny::renderTable({(table_appopen_teen_safety())}, striped = TRUE)
    output$plot_appopen_teen_safety <- renderPlotly({plot_appopen_teen_safety()})
    
    
    table_appopen_crisis <- reactive({
      tables_app_opens()$`Crisis(11)`
    }) 
    plot_appopen_crisis <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.app_launch_count_w_crisis", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot", group = "ClusterName")
    })
    output$table_appopen_crisis <- shiny::renderTable({(table_appopen_crisis())}, striped = TRUE)
    output$plot_appopen_crisis <- renderPlotly({plot_appopen_crisis()})
    
    
    table_appopen_celebration <- reactive({
      tables_app_opens()$`Celebration & Next Steps(12)`
    }) 
    plot_appopen_celebration <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.app_launch_count_w_celebrate", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot", group = "ClusterName")
    })
    output$table_appopen_celebration <- shiny::renderTable({(table_appopen_celebration())}, striped = TRUE)
    output$plot_appopen_celebration <- renderPlotly({plot_appopen_celebration()})
    
    nf_data_join <- reactive({
      if (country == "Tanzania"){
        if (study == "Optimisation"){
          plhorg_nf <- selected_data_dem() %>%
            dplyr::select(c("app_user_id", "Support", "Skin", "Digital Literacy"))
        } else if (study == "Pilot"){
          plhorg_nf <- selected_data_dem() %>%
            dplyr::select(c("app_user_id", "PilotSite"))
        } else if (study == "RCT"){
          plhorg_nf <- selected_data_dem() %>%
            dplyr::select(c("app_user_id", "ClusterName"))
        }
      }
      # link nf data to user data by app_user_id
      # use inner_join: remove from nf anyone not in plhdata_org
      inner_join(nf_data, plhorg_nf)
    })
    
    # Push notifications tab 4.2
    table_pushn_totals <- reactive({notification_summary(nf_data_join(), factors = opt_factors()) %>%
        mutate(`Notifications responded to` = paste0(round(replied / received * 100, 1), "% (", replied, "/", received, ")")) %>%
        dplyr::select(-c("replied", "received"))}) 
    output$table_pushn_totals <- shiny::renderTable({(table_pushn_totals())}, striped = TRUE)
    
    table_pushn_mean <- reactive({
      notification_summary(nf_data_join(), factors = c(opt_factors(), "campaign_id")) %>%
        mutate(`Notifications responded to` = paste0(round(replied / received * 100, 1), "% (", replied, "/", received, ")")) %>%
        dplyr::select(-c("replied", "received"))
    })
    plot_pushn_mean <- reactive({
      notif <- notification_summary(nf_data_join(), factors = c(opt_factors(), "campaign_id")) %>%
        mutate(perc_received = replied/received)
      
      if (country == "Tanzania"){
        if (study == "Optimisation"){
          notif <- notif %>%
            tidyr::unite(col = "Org", opt_factors())
        } else if (study %in% c("Pilot", "RCT")){
          notif <- notif %>% mutate(Org = opt_factors())
        }
      }
      plot <- ggplot(notif, aes(x = campaign_id, y = perc_received)) #, fill = Org)) # removing fill by ClusterName
      plot + geom_bar(stat = "identity", position = "dodge", fill = "#78D473") +
        scale_x_discrete(guide = guide_axis(angle = 90)) +
        viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "Campaign ID", y = "Percentage responded")
    })
    output$table_pushn_mean <- shiny::renderTable({(table_pushn_mean())}, striped = TRUE)
    output$plot_pushn_mean <- renderPlotly({plot_pushn_mean()})
    
    
    # Home Practice tab 4.3
    #NB no home practice for worshops 1 and 12 (welcome and celebration)
    
    #### HP review started per week ############################################################
    # Engagement sub tab: HP -------------------------
    summary_table_hp_totals <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(),
                                                               data = selected_data_dem(),
                                                               columns_to_summarise = data_hp_started,
                                                               replace = "rp.contact.field.w_",
                                                               replace_after = "_hp_review_started")
      summary_table_baseline_build %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0))) %>%
        purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    relative_hp_started <- reactive({
      relative_hp_started <- summary_table_hp_totals() %>%
        purrr::map(~.x %>% mutate_all(~replace(., is.na(.), 0)) %>%
                     dplyr::select(opt_factors(), True))
      return(relative_hp_started)   
    })
    table_hp_started <- reactive({
      relative_hp_started() %>%
        list_rbind(names_to = "id") %>%
        dplyr::select(id, opt_factors(), True) %>%
        pivot_wider(names_from = id, values_from = True) %>%
        dplyr::rename_all(~gsub("_", ".", .))
    })
    plot_hp_started  <- reactive({
      relative_hp_started <- imap(relative_hp_started(), ~.x %>%
                                    rename(value = True))
      hp_mood_plot(relative_hp_started, opt_factors(), manipulation = "ldply",
                   limits =  c("Praise", "Instruct", "Stress", "Solve", "Money", "Rules", "Consequence", "Safe", "Crisis"),
                   xlab = "Workshop week", fill = FALSE, fill_colour = "#78D473")
    }) 
    output$table_hp_started <- shiny::renderTable({(table_hp_started())}, striped = TRUE)
    output$plot_hp_started <- renderPlotly({plot_hp_started()})
    
    # home practice review - user claims they had a chance to do the hp
    
    summary_table_hp_done <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(),
                                                               data = selected_data_dem(),
                                                               columns_to_summarise = data_hp_done,
                                                               replace = "rp.contact.field.w_",
                                                               replace_after = "_hp_done")
      summary_table_baseline_build %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0))) %>%
        purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    relative_hp_done <- reactive({
      summary_table_hp_done <- summary_table_hp_done()
      for (i in 1:length(summary_table_hp_done)){
        if (!"Yes" %in% colnames(summary_table_hp_done[[i]])) {
          summary_table_hp_done[[i]]$Yes <- 0
        }
      }
      select_items <- c(opt_factors(), "Yes")
      relative_hp_done <- imap(summary_table_hp_done, ~.x %>%
                                 select(select_items))
      return(relative_hp_done)
    })
    table_perc_long <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      relative_hp_done_stress <- summary_table_base_build(opt_factors = opt_factors(),
                                                          data = selected_data_dem(),
                                                          columns_to_summarise = "rp.contact.field.w_stress_hp_done",
                                                          replace = "rp.contact.field.w_",
                                                          replace_after = "_hp_done")
      relative_hp_done_stress <- relative_hp_done_stress %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      relative_hp_done_stress <- relative_hp_done_stress %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
      
      relative_hp_done_stress <- plyr::ldply(relative_hp_done_stress)
      hp_started_long <- plyr::ldply(relative_hp_started())
      hp_done_long <- plyr::ldply(relative_hp_done())
      
      hp_done_long <- full_join(relative_hp_done_stress, hp_done_long)
      table_perc_long <- full_join(hp_started_long, hp_done_long) %>%
        mutate(perc_complete = Yes/True)
      return(table_perc_long)
    })
    table_hp_done <- reactive({
      table_hp_done <- table_perc_long() %>%
        pivot_wider(id_cols = opt_factors(), names_from = .id, values_from = perc_complete)
      return(table_hp_done)
    })
    plot_hp_done  <- reactive({
      summary_mean_completion_level_long <- table_perc_long() %>%
        filter(ClusterName == "Total") %>% rename(value = perc_complete) %>% rename(name = `.id`)
      hp_mood_plot(summary_mean_completion_level_long, opt_factors(), manipulation = "none", limits = c("1on1", "Praise", "Instruct", "Stress", "Solve", "Money", "Rules", "Consequence", "Safe", "Crisis"),
                   xlab = "Workshop week", fill = FALSE, fill_colour = "#78D473")
    }) 
    output$table_hp_done <- shiny::renderTable({(table_hp_done())}, striped = TRUE)
    output$plot_hp_done <- renderPlotly({plot_hp_done()})
  
    #SIXTH Tab Parent Library
    summary_table_library <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(), columns_to_summarise = data_library,
                                                               replace = "rp.contact.field.click_pc_",
                                                               replace_after = "count")
      data_library_neat <- naming_conventions(names(summary_table_library), replace = "Rp.contact.field.click hs")
      names(summary_table_baseline_build) <- data_library_neat
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    #average clicks on parent library (mean per org)
    table_library_mean <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      #mean library clicks (button type per organisation)
      #mean library clicks per workshop week is not stored to my knowledge
      table_library_data <- selected_data_dem() %>%
        mutate(across(data_library, ~as.numeric(.x)))
      
      summary_mean_library_data <- summary_table(data = table_library_data,
                                                 columns_to_summarise = data_library,
                                                 replace = "rp.contact.field.click_",
                                                 replace_after = "_count",
                                                 summaries = "mean",
                                                 factors = opt_factors())
      means_total <- NULL
      for (i in 1:length(summary_mean_library_data)){
        if(is.numeric(summary_mean_library_data[[i]])){
          means_total[i] <- round(mean(summary_mean_library_data[[i]], na.rm = TRUE), 1)
        } else {
          means_total[i] <- NA
        }
      }
      means_total <- data.frame(t(plyr::ldply(means_total)))
      names(means_total) <- names(summary_mean_library_data)
      j <- 1
      for (i in 1:length(means_total)){
        if (is.na(means_total[[i]])){
          if (j == 1) {
            means_total[[i]][length(means_total[[i]])] <- "Total"
            j <- j + 1
          } else {
            means_total[[i]][length(means_total[[i]])] <- "-"
          }
        }
      }
      rbind(summary_mean_library_data, means_total)
      
    }) 
    
    plot_library_mean  <- reactive({
      summary_library_mean_long <- pivot_longer(table_library_mean(),
                                                cols = !Org,
                                                names_to = "Library", values_to = "Clicks")
      ggplot(summary_library_mean_long, aes(x = Library , y = Clicks)) + #, fill = Org)) # removing fill by ClusterName
        geom_bar(stat = "identity", position = "dodge") +
        # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        scale_x_discrete(guide = guide_axis(angle = 90), limits = week_order) +
        viridis::scale_fill_viridis(discrete = TRUE) 
      
    })
    output$table_library_mean <- shiny::renderTable({(table_library_mean())}, striped = TRUE)
    output$plot_library_mean <- renderPlotly({plot_library_mean()})
    
    #total clicks on parent library from homescreen
    table_library_totals <- reactive({summary_table_library()$` parent centre `  })
    plot_library_totals  <- reactive({summary_plot(selected_data_dem(), "rp.contact.field.click_hs_parent_centre_count", replace = "rp.contact.field.click_hs_")})
    output$table_library_totals <- shiny::renderTable({(table_library_totals())}, striped = TRUE)
    output$plot_library_totals <- renderPlotly({plot_library_totals()})
    
    #my tips
    table_lib_tips <- reactive({summary_table_library()$`My tips `  }) 
    plot_lib_tips  <- reactive({summary_plot(data= selected_data_dem(), "rp.contact.field.click_pc_my_tips_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_tips <- shiny::renderTable({(table_lib_tips())}, striped = TRUE)
    output$plot_lib_tips <- renderPlotly({plot_lib_tips()})
    
    #essential tools
    table_lib_tools <- reactive({summary_table_library()$`Essential tools `  }) 
    plot_lib_tools  <- reactive({summary_plot(selected_data_dem(), "rp.contact.field.click_pc_essential_tools_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_tools <- shiny::renderTable({(table_lib_tools())}, striped = TRUE)
    output$plot_lib_tools <- renderPlotly({plot_lib_tools()})
    
    #relax and activities
    table_lib_activities <- reactive({summary_table_library()$`Relax and activities `  }) 
    plot_lib_activities  <- reactive({summary_plot(selected_data_dem(), "rp.contact.field.click_pc_relax_and_activities_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_activities <- shiny::renderTable({(table_lib_activities())}, striped = TRUE)
    output$plot_lib_activities <- renderPlotly({plot_lib_activities()})
    
    #customisse ParentApp
    table_lib_custom <- reactive({summary_table_library()$`Customisation `  }) 
    plot_lib_custom  <- reactive({summary_plot(selected_data_dem(), "rp.contact.field.click_pc_customisation_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_custom <- shiny::renderTable({(table_lib_custom())}, striped = TRUE)
    output$plot_lib_custom <- renderPlotly({plot_lib_custom()})
    
    #help - quick parenting tips
    table_lib_help <- reactive({summary_table_library()$`Help `  }) 
    plot_lib_help  <- reactive({summary_plot(selected_data_dem(), "rp.contact.field.click_pc_help_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_help <- shiny::renderTable({(table_lib_help())}, striped = TRUE)
    output$plot_lib_help <- renderPlotly({plot_lib_help()})
    
    #technical support
    table_lib_tech <- reactive({summary_table_library()$`Technical support `  }) 
    plot_lib_tech  <- reactive({summary_plot(selected_data_dem(), "rp.contact.field.click_pc_technical_support_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_tech <- shiny::renderTable({(table_lib_tech())}, striped = TRUE)
    output$plot_lib_tech <- renderPlotly({plot_lib_tech()})
    
    #coping with COVID 
    table_lib_covid <- reactive({summary_table_library()$`Covid `  }) 
    plot_lib_covid  <- reactive({summary_plot(selected_data_dem(), "rp.contact.field.click_pc_covid_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_covid <- shiny::renderTable({(table_lib_covid())}, striped = TRUE)
    output$plot_lib_covid <- renderPlotly({plot_lib_covid()})
    
    #coping with grief / bereavement
    table_lib_grief <- reactive({summary_table_library()$`Bereavement `  }) 
    plot_lib_grief  <- reactive({summary_plot(selected_data_dem(), "rp.contact.field.click_pc_bereavement_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_grief <- shiny::renderTable({(table_lib_grief())}, striped = TRUE)
    output$plot_lib_grief <- renderPlotly({plot_lib_grief()})
    
    ## Download sheet -----------------------------------------
    download_data_start <- reactive({
      if (study == "Optimisation"){
        plhdata_group_ids <- selected_data_dem() %>% select(c('app_user_id', "opt_cluster", "createdAt", all_of(data_completion_level)))
      } else if (study == "RCT"){
        plhdata_group_ids <- selected_data_dem() %>% select(c('app_user_id', "updated_id", opt_cluster = "ClusterName", "createdAt", "updatedAt", `App version` = "app_version", all_of(data_completion_level)))
      }
      plhdata_group_ids_group_1 <- threshhold_function(data = plhdata_group_ids, threshhold = 0)
      plhdata_group_ids_group_1 <- plhdata_group_ids_group_1 %>%
        mutate(engagement_total = self_care_started + `1on1_started` + praise_started + 
                 instruct_started + stress_started + money_started + rules_started + consequence_started + 
                 solve_started + safe_started + crisis_started + celebrate_started)
      #View(plhdata_group_ids_group_1)
      plhdata_group_ids_group_1 <- plhdata_group_ids_group_1 %>%
        mutate(createdAt = as.Date(createdAt, "%y %m %d", tz = "utc"), # can calculate by UIC tracker
               curr_date = as.Date(Sys.Date(), "%y %m %d")) %>%
        mutate(diff_in_days = curr_date - createdAt) %>%
        mutate(hours_since_sync = last_sync()) %>%
        mutate(group_since_sync = last_sync_cat()) %>%
        mutate(week_number = floor(as.numeric(diff_in_days/7))) %>%
        mutate(week_number = ifelse(week_number > 12, 12, week_number)) %>%
        mutate(prop_complete = engagement_total / week_number) %>%
        mutate(engagement_level = ifelse(prop_complete == 0, "not engaged",
                                         ifelse(prop_complete <= 0.33, "low",
                                                ifelse(prop_complete <= 0.67, "moderate",
                                                       ifelse(prop_complete <= 1, "high",
                                                              ifelse(prop_complete > 1, "ahead",
                                                                     "else")))))) %>%
        dplyr::select(-c(diff_in_days, curr_date, prop_complete))
      
      return(plhdata_group_ids_group_1)
    })
    
    large_engagement_sheet <- reactive({
      if (study == "RCT"){
        # data_completion_level
        
        # data to provide
        # completion level and completion varibales
        completion_vars <- paste0(data_completion_level_names, " Level")
        completion_total_vars <- paste0(data_completion_level_names, " Complete")
        
        plhdata_org_clean <- selected_data_dem()
        plhdata_org_clean_CL <- plhdata_org_clean %>%
          dplyr::mutate(across(data_completion_level, ~ifelse(. == 100, TRUE, FALSE))) %>%
          dplyr::select(app_user_id, updated_id, data_completion_level)
        colnames(plhdata_org_clean_CL) <- gsub("_completion_level", "_completed", colnames(plhdata_org_clean_CL))
        
        plhdata_org_clean_1 <- plhdata_org_clean %>%
          dplyr::select(c(app_user_id, ClusterName, app_version, data_completion_level))
        plhdata_org_clean_CL <- full_join(plhdata_org_clean_1, plhdata_org_clean_CL)
        
        # number of app opens -------------------
        plhdata_org_clean_appopens <- add_na_variable(plhdata_org_clean, data_app_opens)
        plhdata_org_clean_appopens <- plhdata_org_clean_appopens %>%
          dplyr::select(app_user_id, updated_id, data_app_opens)
        
        names(plhdata_org_clean_appopens) <- paste0("app_launch_", naming_conventions(names(plhdata_org_clean_appopens),
                                                                                      replace = "rp.contact.field.app_launch_count_w_"))
        names(plhdata_org_clean_appopens)[[1]] <- c("app_user_id")
        names(plhdata_org_clean_appopens)[[2]] <- c("app_launch_count")
        
        # home practice activity response (yes/no) for each home practice activity
        plhdata_org_clean_hp_response <- plhdata_org_clean %>%
          dplyr::select(app_user_id, updated_id, data_hp_started)
        names(plhdata_org_clean_hp_response) <- paste0("hp_started_", naming_conventions(names(plhdata_org_clean_hp_response),
                                                                                         replace = "rp.contact.field.w_",
                                                                                         replace_after = "_hp_review_started"))
        names(plhdata_org_clean_hp_response)[[1]] <- c("app_user_id")
        
        # data_hp_done
        plhdata_org_clean_hp_done <- add_na_variable(plhdata_org_clean, data_hp_done)
        plhdata_org_clean_hp_done <- plhdata_org_clean_hp_done %>%
          dplyr::select(app_user_id, updated_id, data_hp_done)
        names(plhdata_org_clean_hp_done) <- paste0("hp_done_", naming_conventions(names(plhdata_org_clean_hp_done),
                                                                                  replace = "rp.contact.field.w_",
                                                                                  replace_after = "_hp_done"))
        names(plhdata_org_clean_hp_done)[[1]] <- c("app_user_id")
        
        # data hp mood
        plhdata_org_clean_hp_mood <- add_na_variable(plhdata_org_clean, data_hp_mood)
        plhdata_org_clean_hp_mood <- plhdata_org_clean_hp_mood %>%
          dplyr::select(app_user_id, updated_id, data_hp_mood)
        names(plhdata_org_clean_hp_mood) <- paste0("hp_mood_", naming_conventions(names(plhdata_org_clean_hp_mood),
                                                                                  replace = "rp.contact.field.w_",
                                                                                  replace_after = "_hp_mood"))
        names(plhdata_org_clean_hp_mood)[[1]] <- c("app_user_id")
        
        # parent points
        plhdata_org_clean_pp <- plhdata_org_clean %>%
          dplyr::select(app_user_id, updated_id, data_habit_parent_points_all)
        names(plhdata_org_clean_pp) <- paste0("parent_point_", naming_conventions(names(plhdata_org_clean_pp),
                                                                                  replace = "rp.contact.field.parent_point_count_"))
        names(plhdata_org_clean_pp)[[1]] <- c("app_user_id")
        
        # Bang 'em together -------------------------------------------
        plhdata_org_clean_all <- full_join(plhdata_org_clean_CL, plhdata_org_clean_appopens)
        plhdata_org_clean_all <- full_join(plhdata_org_clean_all, plhdata_org_clean_hp_response)
        plhdata_org_clean_all <- full_join(plhdata_org_clean_all, plhdata_org_clean_hp_done)
        plhdata_org_clean_all <- full_join(plhdata_org_clean_all, plhdata_org_clean_hp_mood)
        plhdata_org_clean_all <- full_join(plhdata_org_clean_all, plhdata_org_clean_pp)
        
        plhdata_org_clean_all <- unique(plhdata_org_clean_all)
        return(plhdata_org_clean_all)
      }
    })
    
    engagement_download <- reactive({
      plhdata_group_ids_group_1 <- download_data_start() %>% dplyr::select(-c(self_care_started, `1on1_started`, praise_started, 
                                                                              instruct_started, stress_started, money_started, rules_started, consequence_started, 
                                                                              solve_started, safe_started, crisis_started, celebrate_started))
      names(plhdata_group_ids_group_1) <- naming_conventions(names(plhdata_group_ids_group_1), replace = "rp.contact.field.w_")
      plhdata_group_ids_group_1 <- plhdata_group_ids_group_1 %>%
        dplyr::select(c("App user id", "Updated id", "Opt cluster", "Engagement total", "Week number", "Engagement level",
                        "CreatedAt", "App version", "Hours since sync", "1 Self care completion level" = "Self care completion level",
                        "2 1on1 completion level" = "1on1 completion level", "3 Praise completion level" = "Praise completion level",
                        "4 Instruct completion level" = "Instruct completion level", "5 Stress completion level" = "Stress completion level",
                        "6 Solve completion level" = "Solve completion level",
                        "7 Money completion level" = "Money completion level", "8 Rules completion level" = "Rules completion level",
                        "9 Consequence completion level" = "Consequence completion level", 
                        "10 Safe completion level" = "Safe completion level", "11 Crisis completion level" = "Crisis completion level", 
                        "12 Celebrate completion level" = "Celebrate completion level"))
      return(plhdata_group_ids_group_1)
    })
    
    # TODO: fix.
    summary_download <- reactive({
      data <- download_data_start()
      last_sync_data <- data %>%
        mutate(last_sync_cat = ifelse(group_since_sync == "4", "Last synced over 60 days ago",
                                      ifelse(group_since_sync == "3", "Last synced over 30 days ago",
                                             ifelse(group_since_sync == "2", "Last synced over 14 days ago",
                                                    ifelse(group_since_sync == "1", "Last synced over 7 days ago", "0"))))) %>%  group_by(opt_cluster, last_sync_cat) %>%
        summarise(opt_cluster, last_sync_cat) %>%
        summarise(last_sync = n()) %>%
        filter(last_sync_cat != 0) %>%
        pivot_wider(id_cols = opt_cluster, names_from = last_sync_cat, values_from = last_sync, values_fill = 0)
      
      plhdata_group_ids_group_1 <- data %>%
        group_by(opt_cluster) %>%
        summarise(`Average weeks started` = mean(engagement_total, na.rm = TRUE),
                  `Week number (?)` = mean(week_number, na.rm = TRUE),
                  `Total participants` = n())
      
      plhdata_group_ids_group_1 <- full_join(plhdata_group_ids_group_1, last_sync_data)
      # started_vars <- c("self_care_started", "1on1_started", "praise_started", "instruct_started", "stress_started", 
      #                   "money_started", "rules_started", "consequence_started", "solve_started", "safe_started", "crisis_started", "celebrate_started")
      # df_list <- plhdata_group_ids_group_1() %>%
      #   map2(.x = started_vars, .y = data_completion_level,
      #        .f = ~eng_summary_vars(data = download_data_start(), started = .x, completion = .y))
      # for (i in 1:length(df_list)){
      #   plhdata_group_ids_group_1 <- merge(plhdata_group_ids_group_1, df_list[[i]])
      # }
      plhdata_group_ids_group_1 <- plhdata_group_ids_group_1 %>%
        mutate(across(where(is.numeric), ~round(.x, digits = 1)))
      return(plhdata_group_ids_group_1)
    })
    
    summary_download_workshop <- reactive({
      table_ws_started <- relative_perc_completed()
      table_ws_started_additional <- additional_relative_perc_completed()
      table_ws_totals1 <- table_ws_totals() %>%
        pivot_longer(cols = !"ClusterName",
                     names_to = "Workshop",
                     values_to = "Average completion level")
      table_ws_totals1_additional <- additional_table_ws_totals() %>%
        pivot_longer(cols = !"ClusterName",
                     names_to = "Workshop",
                     values_to = "Average completion level")
      
      table_ws_totals1 <- full_join(table_ws_totals1, table_ws_started) %>%
        relocate("Average completion level", .after = last_col())
      table_ws_totals1_additional <- full_join(table_ws_totals1_additional, table_ws_started_additional) %>%
        relocate("Average completion level", .after = last_col())
      table_ws_totals1 <- bind_rows(table_ws_totals1, table_ws_totals1_additional)
      return(table_ws_totals1 %>% dplyr::filter(!Workshop %in% c("Weeks completed", "Number accessed")))
    })
    
    rct_access_data <- reactive({
      rct_access <- selected_data_dem() %>%
        dplyr::mutate(days_since_last_sync = as.Date(Sys.Date()) - as.Date(rp.contact.field._server_sync_latest)) %>%
        dplyr::select(c(app_user_id, ClusterName, RCT_access=rp.contact.field.post_rct_access,
                        last_sync = rp.contact.field._server_sync_latest,
                        days_since_last_sync))
    })
    
    credentials <- shinyauthr::loginServer(
      id = "login",
      data = credentials_data,
      user_col = user,
      pwd_col = password)
    
    output$build_download <- renderUI({
      req(credentials()$user_auth)
      if (credentials()$info$user == "admin"){
        tagList(fluidRow(
          box(width = 6, 
              selectInput("dataset", "Choose a dataset:", choices = c("Engagement Data",
                                                                      "Engagement Data (Large)",
                                                                      "Summary Data",
                                                                      "Summary Data by Workshop",
                                                                      "RCT Access")),
              # Button
              downloadButton("downloadData", "Download"))),
          fluidRow(box(width = 12,
                       dataTableOutput("download_table"),
                       style='width:100%;overflow-x: scroll;')))
      } else {
        tagList(fluidRow(
          box(width = 6, 
              selectInput("dataset", "Choose a dataset:", choices = c("Summary Data", "Summary Data by Workshop", "RCT Access")),
              # Button
              downloadButton("downloadData", "Download"))),
          fluidRow(box(width = 12,
                       dataTableOutput("download_table"),
                       style='width:100%;overflow-x: scroll;')))
      }
      
    })
    
    datasetInput <- reactive({
      switch(input$dataset,
             "Engagement Data" = engagement_download(),
             "Engagement Data (Large)" = large_engagement_sheet(),
             "Summary Data" = summary_download(),
             "Summary Data by Workshop" = summary_download_workshop(),
             "RCT Access" = rct_access_data())
    })
    
    # Table of selected dataset ----
    output$download_table <- renderDataTable({
      return(datasetInput())
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
    print("last")
    print(Sys.time())
  } #close server
  shinyApp(ui = ui, server = server)
} # close function
