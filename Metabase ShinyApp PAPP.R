study = "PAPP"
country = "Tanzania"

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
    
    dashboardBody(
      fluidRow(
        shinydashboard::valueBoxOutput("total_n", width=6),
        shinydashboard::valueBoxOutput("total_users", width=6),
        #shinydashboard::valueBoxOutput("myvaluebox1", width=3),
        shinydashboard::valueBoxOutput("myvaluebox2", width=4),
        shinydashboard::valueBoxOutput("myvaluebox3", width=4),
        shinydashboard::valueBoxOutput("myvaluebox4", width=4)
      ),
      tabItems(
        # First tab content layout
        tabItem(tabName = "demographics",
                fluidRow(
                  column(12, align = "centre",
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
                      title = "Language",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_app_language", height = "240"), #generates graph
                      shiny::tableOutput("table_app_language")  #generates table
                  ), #closes box
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Referral Source",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_referral_source", height = "240"), #generates graph
                      shiny::tableOutput("table_referral_source")  #generates table
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
                                     ), #closes fluid row
                                     
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Which Activities have been completed (of those started)",
                                           footer = "Of all individuals who have started that module, what proportion of individuals have completed the specific activity",
                                           status = "info",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_which_completed", height = "480"),
                                           shiny::tableOutput("table_which_completed")
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
                                           shiny::tableOutput("table_pp_means")
                                           ), #closes and tab panel
                                       )
                ),

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
    
    # observe({
    # })

    last_sync <- reactive({
      if (country == "Tanzania"){
        time_diff <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(plhdata_org_clean$updatedAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours")
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
    
    #SUMMARY STATS HEADER displays (same for all tabs)
    output$total_n <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(nrow(plhdata_org_clean), subtitle = "total users", icon = icon("people"),
                               color = "aqua")})
    output$total_users <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(nrow(plhdata_org_clean %>% filter(createdAt > as.Date(lubridate::now(tzone = "UTC")) - 7)), subtitle = "trial users joined in last 7 days", icon = icon("clock"),
                               color = "yellow")})
      output$myvaluebox2 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(nrow(data_engagement_weeks_all() %>% filter(last_sync_cat == "Last synced 14-29 days ago")), subtitle = "Last synced 14-29 days ago", icon = icon("user"),
                                 color = "fuchsia")})
      output$myvaluebox3 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(nrow(data_engagement_weeks_all() %>% filter(last_sync_cat == "Last synced 30-59 days ago")), subtitle = "Last synced 30-59 days ago", icon = icon("user"),
                                 color = "purple")})
      output$myvaluebox4 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(nrow(data_engagement_weeks_all() %>% filter(last_sync_cat == "Last synced over 60 days ago")), subtitle = "not synced in over 60 days", icon = icon("user"),
                                 color = "orange")})

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
  
    mult_summary_table_filter <- function(summary_table = summary_table_baseline_build){
      print("4")
      summary_table <- summary_table %>% 
        purrr::map(.f =~.x %>%
                     janitor::adorn_totals(c("row", "col")))
       return(summary_table) 
    }
    
    # Demographics ---------------------------------------------------
    summary_table_baseline <- reactive({ #eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
        summary_table_baseline_build <- summary_table_base_build(opt_factors = NULL, data = plhdata_org_clean, columns_to_summarise = data_baseline_survey$metabase_ID)
        summary_table_baseline_build %>%
          purrr::map(~ {
            df <- .x %>% dplyr::mutate(across(everything(), ~ replace(., is.na(.), 0)))
            if (ncol(df) >= 2) df <- janitor::adorn_totals(df, "col", where(is.numeric))
            df
          })
    })
    
    plot_app_downloaded  <- reactive({ # last sync
      ggplot(data = plhdata_org_clean, aes(x = as.POSIXct(createdAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
                                           colour = `Referral source`)) +
        geom_density(linewidth = 0.6) +
        labs(x = "Created Profile (createdAt)", y = "Density")
    }) 
    output$plot_app_downloaded <- renderPlotly({plot_app_downloaded()})
    
    table_app_downloaded <- reactive({
      plhdata_org_clean$createdAt <- as.POSIXct(plhdata_org_clean$createdAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC")
      plhdata_org_clean$createdAt <- as.Date(plhdata_org_clean$createdAt)
      
      table_app_downloaded <- plhdata_org_clean %>%
        group_by(createdAt, `Referral source`) %>%
        count() %>%
        pivot_wider(names_from = `Referral source`, values_from = n, values_fill = 0) %>%
        janitor::adorn_totals(where = c("row", "col"), where(is.numeric))
      return(table_app_downloaded)
    })
    output$table_app_downloaded <- shiny::renderTable({(table_app_downloaded())}, striped = TRUE)
    
    #table_app_launch <- reactive({}) 
    plot_app_launch  <- reactive({ # last sync
      ggplot(data = plhdata_org_clean, aes(x = as.POSIXct(updatedAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))) +
        geom_freqpoly(bins = 30) +
        labs(x = "Last sync (updatedAt)", y = "Count")
    }) 
    output$plot_app_launch <- renderPlotly({plot_app_launch()})
    
    #Overview and Demographics plot and table
    display_sheet_table <- function(n = "language", j = 1){
      return(output[[paste0("table_", n)]] <- shiny::renderTable({(summary_table_baseline()[[j]])}, striped = TRUE))
    }
    display_sheet_plot <- function(n = "language", j = 1){
      return(output[[paste0("plot_", n)]] <-  renderPlotly(summary_plot(data = plhdata_org_clean,
                                                                        columns_to_summarise = j,
                                                                        replace = "rp.contact.field.",
                                                                        values = "percentage",
                                                                        fill_colour = "#4882BE")) # shades of blue
    )} # plottype = histogram
    
    # run our table_baselines and plot_baselines # TODO: in PLHr function, replace for loop with map like this.
    map2(data_baseline_survey$display_name, data_baseline_survey$object_name, .f = ~ display_sheet_table(n = .y, j = .x))
    map2(data_baseline_survey$metabase_ID, data_baseline_survey$object_name, .f = ~ display_sheet_plot(n = .y, j = .x))
    # map2(data_baseline_survey$display_name, data_baseline_survey$object_name, .f = ~ display_sheet_table(n = .y, j = .x))
    
    # plot_referral_source and table_referral_source
    # Started Workshop
    table_referral_source <- reactive({
      table_referral_source <- summary_table_base_build(opt_factors = NULL,
                                                        data = plhdata_org_clean,
                                                        columns_to_summarise = "rp.contact.field.user_referral_source") %>%
        janitor::adorn_totals("col", where(is.numeric))
      return(table_referral_source$`User referral source`)
    })
    plot_referral_source <- reactive({
      plhdata_org_clean$rp.contact.field.user_referral_source <- naming_conventions(plhdata_org_clean$rp.contact.field.user_referral_source)
      
      plot <- ggplot(plhdata_org_clean, aes(x = rp.contact.field.user_referral_source))
      plot + geom_bar(position = "dodge", fill = "#AAE8E7") +
        scale_x_discrete(guide = guide_axis(angle = 45)) +
        #viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "Referral Source", y = "Number of users")
    }) 
    output$table_referral_source <- shiny::renderTable({(table_referral_source())}, striped = TRUE)
    output$plot_referral_source <- renderPlotly({plot_referral_source()})
    
    # bit different for age
    # RCT TODO HERE - not called age i nRCT
    #output$table_parent_age <- shiny::renderTable({(plhdata_org_clean %>% summary_table(columns_to_summarise = rp.contact.field.user_age, factors = NULL, summaries = "mmm"))}, striped = TRUE)
    #output$plot_parent_age <- renderPlotly({summary_plot(data = plhdata_org_clean, columns_to_summarise = "rp.contact.field.user_age", replace = "rp.contact.field.", plot_type = "histogram")})
    
    #App version
    plot_app_version  <- reactive({
      summary_plot(data = plhdata_org_clean,
                   columns_to_summarise = "app_version",
                   replace = "rp.contact.field.",
                   values = "percentage",
                   fill_colour = "#4882BE")
      #summary_plot(plhdata_org_clean, app_version)
    })
    output$plot_app_version <- renderPlotly({plot_app_version()})
    
    # this one is used in additional content and in demographics
    data_engagement_weeks_all <- reactive({
      last_sync_data <- plhdata_org_clean %>%
        mutate(group_since_sync = last_sync_cat()) %>%
        mutate(last_sync_cat = ifelse(group_since_sync == "4", "Last synced over 60 days ago",
                                      ifelse(group_since_sync == "3", "Last synced 30-59 days ago",
                                             ifelse(group_since_sync == "2", "Last synced 14-29 days ago",
                                                    ifelse(group_since_sync == "1", "Last synced less than 14 days ago", "0"))))) %>%
        mutate(last_sync_cat = fct_relevel(last_sync_cat, c("Last synced less than 14 days ago", "Last synced 14-29 days ago", "Last synced 30-59 days ago", "Last synced over 60 days ago"))) %>%
        dplyr::select(c(last_sync_cat, rp.contact.field.post_rct_access))
      return(last_sync_data)
    })
    
    #SECOND Tab Workshop Engagement Data
    
    # Workshop Engagement ---------------------------------------------------
    output$week_engagement <- shiny::renderTable({download_data_start() %>%
        group_by(engagement_level) %>% summarise(Number = n(), Percentage = n()/nrow(.) * 100)}, striped = TRUE)
    
    # Started Workshop
    table_ws_started <- reactive({
      table_ws_started <- relative_perc_completed %>%
        mutate(perc_started = paste0(n_started, " (", perc_started, "%)")) %>%
        dplyr::select(c("Workshop", "perc_started")) %>%
        tidyr::pivot_wider(names_from = "Workshop", values_from = "perc_started")
      return(table_ws_started)
    })
    plot_ws_started <- reactive({
      summary_mean_completion_level_long <- relative_perc_completed
      plot <- ggplot(summary_mean_completion_level_long, aes(x = Workshop, y = perc_started))
      plot + geom_bar(stat = "identity", position = "dodge", fill = "#AAE8E7") +
        scale_x_discrete(guide = guide_axis(angle = 90)) +
        #viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "Workshop", y = "Started (%)")
    }) 
    output$table_ws_started <- shiny::renderTable({(table_ws_started())}, striped = TRUE)
    output$plot_ws_started <- renderPlotly({plot_ws_started()})
    
    # Completed Workshop
    table_ws_rel_completed <- reactive({
      table_perc_completed <- relative_perc_completed %>%
        mutate(perc_completed = paste0(n_completed, " (", perc_completed, "%)")) %>%
        dplyr::select(c("Workshop", "perc_completed")) %>%
        tidyr::pivot_wider(names_from = "Workshop", values_from = c("perc_completed"))
      
      table_ws_started <- relative_perc_completed %>%
        dplyr::select(c("Workshop", "n_started")) %>%
        dplyr::mutate(n_started = as.character(n_started)) %>%
        tidyr::pivot_wider(names_from = "Workshop", values_from = "n_started")
      
      table_perc_completed <- full_join(table_perc_completed, table_ws_started)
      table_perc_completed <- data.frame(Guide = c("Completed (Percentage)", "Started"), table_perc_completed)
      return(table_perc_completed)
    })
    plot_ws_rel_completed  <- reactive({
      summary_mean_completion_level_long <- relative_perc_completed
      plot <- ggplot(summary_mean_completion_level_long, aes(x = `Workshop`, y = perc_completed))#, fill = Org)) # removing fill by ClusterName
      plot + geom_bar(stat = "identity", position = "dodge", fill = "#AAE8E7") +
        scale_x_discrete(guide = guide_axis(angle = 90), limits = week_order) +
        viridis::scale_fill_viridis(discrete = TRUE)+
        labs(x = "Workshop", y = "Completed (%)")
    }) 
    output$table_ws_rel_completed <- shiny::renderTable({(table_ws_rel_completed())}, striped = TRUE)
    output$plot_ws_rel_completed <- renderPlotly({plot_ws_rel_completed()})
    
    
    plot_which_completed <- reactive({
      ggplot(heatmap_long, aes(x = Workshop, y = rows.id, fill = prop_completed)) +
        geom_tile(color = "white") +
        geom_text(aes(label = prop_completed), size = 3, na.rm = TRUE) +
        scale_fill_gradient(
          low = "white",
          high = "darkblue",
          name = "Completion %",
          na.value = "grey80"  # show NAs as grey
        ) +
        theme_minimal() +
        scale_y_discrete(limits = rev) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(
          title = "Module Completion Heatmap",
          x = "Module",
          y = "Row ID"
        )
    }) 
    table_which_completed <- reactive({
      table_ws_started <- relative_perc_completed %>%
        dplyr::select(c("Workshop", "n_started")) %>%
        tidyr::pivot_wider(names_from = "Workshop", values_from = "n_started")
      return(table_ws_started)
    })
    output$table_which_completed <- shiny::renderTable({(table_which_completed())}, striped = TRUE)
    output$plot_which_completed <- renderPlotly({plot_which_completed()})
    
    # Completion Level
    table_ws_totals <- reactive({ # eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      #mean average completion level per org
      # Percentage of users who completed a workshop out of those who started it
      # nrow(plhdata_org_clean %>% filter(rp.contact.field.w_money_completion_level == 100)) / nrow(plhdata_org_clean %>% filter(rp.contact.field.w_money_started == "true"))
      summary_mean_completion_level <- summary_table(data = plhdata_org_clean,
                                                     columns_to_summarise = data_completion_level,
                                                     replace = "rp.contact.field.w_",
                                                     replace_after = "_completion_level",
                                                     summaries = "mean",
                                                     factors = NULL,
                                                     include_margins = TRUE)
    })
    
    plot_ws_totals  <- reactive({
      table_ws_totals <- table_ws_totals() 
      hp_mood_plot(data = table_ws_totals, factors = NULL, 
                   limits = week_order, xlab = NULL, manipulation = "longer", fill = FALSE,
                   fill_colour = "#AAE8E7")
    }) 
    output$table_ws_totals <- shiny::renderTable({(table_ws_totals())}, striped = TRUE)
    output$plot_ws_totals <- renderPlotly({plot_ws_totals()})
    
    #fill = "#19D1CD"
    
    # Additional Insights - Enaggement ------------------
    #Workshop plot and table
    ws_completion_table <- function(n, j = 1){
      return(output[[paste0("table_", n)]] <-  shiny::renderTable({(summary_table_completion_level[[j]])}, striped = TRUE))
    }
    ws_completion_plot <- function(n, j = 1){
      return(output[[paste0("plot_", n)]] <-  renderPlotly(summary_plot(data = workshop_engagement_cut,
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
      plhdata_org_clean %>% filter(rp.contact.field.post_rct_access == "true")
      })

    accessed_new_content_column <- reactive({
      accessed_new_content() %>%
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
    
    additional_summary_table_completion_level <- reactive({ #eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = NULL, data = additional_engagement_cut(), columns_to_summarise = new_modules_completion_level,
                                                               replace = "rp.contact.field.w_",
                                                               replace_after = "_completion_level")
      summary_table_baseline_build %>%
        purrr::map(~ {
          df <- .x %>% dplyr::mutate(across(everything(), ~ replace(., is.na(.), 0)))
          if (ncol(df) >= 2) df <- janitor::adorn_totals(df, "col", where(is.numeric))
          df
        })
    })
    
    additional_relative_perc_completed <- reactive({
      additional_summary_table_completion_level <- additional_summary_table_completion_level()
      for (i in 1:length(additional_summary_table_completion_level)){
        if (!"100" %in% names(additional_summary_table_completion_level[[i]])){
          additional_summary_table_completion_level[[i]]$`100` <- 0
        }
      }
      select_items <- c("n_started", "perc_started", "n_completed", "perc_completed")
      
      additional_relative_perc_completed <- imap(additional_summary_table_completion_level, ~.x %>%
                                                   mutate(n_started = Total + `0` - `0`,
                                                          perc_started = round(n_started/(Total + `0`) * 100, 1),
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
        pivot_wider(names_from = Workshop, values_from = perc_started)
      
      # this should be done elsewhere - adding the week # into the plhdata.
      if (study == "RCT"){
        additional_table_ws_started <- full_join(accessed_new_content_column(), additional_table_ws_started, multiple = "all")
          mutate(`Number accessed` = replace_na(`Number accessed`, 0)) %>%
          dplyr::select(c("Number accessed", "Srh", "Svp", "Grief", "Learn"))
        additional_table_ws_started$`Number accessed`[nrow(additional_table_ws_started)] <- sum(additional_table_ws_started$`Number accessed`)
        # additional_table_ws_started$`Weeks completed`[length(additional_table_ws_started$`Weeks completed`)] <- round(mean(UIC_onboarding_dates$`Weeks completed`, na.rm = TRUE), 0)
      }
      return(additional_table_ws_started)
    })
    additional_plot_ws_started <- reactive({
      summary_mean_completion_level_long <- additional_relative_perc_completed()

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
        pivot_wider(names_from = Workshop, values_from = perc_completed)
      if (study == "RCT"){
        table_perc_completed <- full_join(accessed_new_content_column(), table_perc_completed, multiple = "all")
          mutate(`Number accessed` = replace_na(`Number accessed`, 0)) %>%
          dplyr::select(c("Number accessed", "Srh", "Svp", "Grief", "Learn"))
        table_perc_completed$`Number accessed`[nrow(table_perc_completed)] <- sum(table_perc_completed$`Number accessed`)
      }
      return(table_perc_completed)
    })
    additional_plot_ws_rel_completed  <- reactive({
      summary_mean_completion_level_long <- additional_relative_perc_completed()
      plot <- ggplot(summary_mean_completion_level_long, aes(x = `Workshop`, y = perc_completed))#, fill = Org)) # removing fill by ClusterName
      plot + geom_bar(stat = "identity", position = "dodge", fill = "#AAE8E7") +
        scale_x_discrete(guide = guide_axis(angle = 90), limits = additional_week_order) +
        viridis::scale_fill_viridis(discrete = TRUE)+
        labs(x = "Workshop", y = "Completed (%)")
    }) 
    output$additional_table_ws_rel_completed <- shiny::renderTable({(additional_table_ws_rel_completed())}, striped = TRUE)
    output$additional_plot_ws_rel_completed <- renderPlotly({additional_plot_ws_rel_completed()})
    
    # Completion Level
    additional_table_ws_totals <- reactive({ # eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_mean_completion_level <- summary_table(data = accessed_new_content(),
                                                     columns_to_summarise = new_modules_completion_level,
                                                     replace = "rp.contact.field.w_",
                                                     replace_after = "_completion_level",
                                                     summaries = "mean",
                                                     factors = NULL,
                                                     include_margins = TRUE)
      
      if (study == "RCT"){
        summary_mean_completion_level <- full_join(accessed_new_content_column(), summary_mean_completion_level, multiple = "all")
          mutate(`Number accessed` = replace_na(`Number accessed`, 0)) %>%
          dplyr::select(c("Number accessed", "Srh", "Svp", "Grief", "Learn"))
        summary_mean_completion_level$`Number accessed`[nrow(summary_mean_completion_level)] <- sum(summary_mean_completion_level$`Number accessed`)
      }
      return(summary_mean_completion_level)
    })
    
    additional_plot_ws_totals  <- reactive({
      additional_table_ws_totals <- additional_table_ws_totals()
      hp_mood_plot(data = additional_table_ws_totals, factors = NULL, 
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
        dplyr::select(c(starts_with("rp.contact.field.ltp_"))) %>%
        dplyr::select(c(ends_with("click_history")))
    })
    
    # Total count
    total_count <- reactive({
      ltp_activities() %>%
      mutate(across(ends_with("click_history"), ~ stringr::str_count(.x, "T")))
    })
    
    # Calculation and Table of number started
    activity_table_started <- reactive({ # eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      number_started <- total_count() %>% 
        summarise(across(ends_with("click_history"), ~ sum(!is.na(.x))))
      names(number_started) <- naming_conventions(names(number_started), replace = "rp.contact.field.ltp_activity_", replace_after = "_click_history")
      number_started <- number_started %>% janitor::adorn_totals(c("row", "col"))
    })
    
    # Plot of number started
    activity_plot_started  <- reactive({
      number_started_total <- activity_table_started() %>%
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
    activity_table_total <- reactive({ # eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      total_count <- total_count()
      total_count$number_started <- apply(!is.na(total_count), 1, sum) - 1
      total_count %>%
        filter(number_started != 0) %>%
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
    activity_table_repeat <- reactive({ # eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      ltp_activities_summary <- ltp_activities() %>%
      mutate(across(ends_with("_click_history"), ~stringr::str_detect(.x, ";"))) %>%
      pivot_longer(cols = everything(), names_to = "Activity") %>%
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
        dplyr::select(c(starts_with("rp.contact.field.ltp_"))) %>%
        dplyr::select(c(ends_with("_hp_done")))
    })
    
    # self reported = yes' ?
    activity_table_done <- reactive({ # eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      ltp_activities_done <- ltp_activities_done() %>%
        dplyr::mutate(across(ends_with("_hp_done"), ~str_count(.x, "yes"))) %>% # count number of yes'
        summarise(across(ends_with("_hp_done"), ~sum(.x, na.rm = TRUE)))
      names(ltp_activities_done) <- naming_conventions(names(ltp_activities_done), replace = "rp.contact.field.ltp_activity_", replace_after = "_hp_done")
      ltp_activities_done <- ltp_activities_done %>% janitor::adorn_totals(c("row", "col"))
      return(ltp_activities_done)
    })
    
    activity_plot_done  <- reactive({
      ltp_activities_done_plot <- activity_table_done() %>%
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
    
    summary_table_habits_all <- reactive({ #eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = NULL, data = plhdata_org_clean, columns_to_summarise = data_habit_parent_points_all,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "_completion_level") %>%
        purrr::map(~ {
          df <- .x %>% dplyr::mutate(across(everything(), ~ replace(., is.na(.), 0)))
          df
        })
    })
    
    #Table of averages
    table_pp_totals <- reactive({#eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_total_habits <- summary_table(data = plhdata_org_clean,
                                            columns_to_summarise = data_habit_parent_points_all,
                                            replace = "rp.contact.field.parent_point_count_",
                                            summaries = "sum",
                                            factors = NULL)
      summary_total_habits %>% janitor::adorn_totals(c("col"))
    })
    plot_pp_totals  <- reactive({
      summary_mean_completion_level_long <- pivot_longer(table_pp_totals(), cols = everything(), names_to = "Parent Points", values_to = "Value") %>%
        filter(`Parent Points` != "Total")
      ggplot(summary_mean_completion_level_long, aes(x = `Parent Points`, y = Value, fill = `Parent Points`)) + 
        geom_bar(stat = "identity") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "Organisation", title = "Parent points given in each category")
    }) 
    output$table_pp_totals <- shiny::renderTable({(table_pp_totals())}, striped = TRUE, caption = "Total parent points for each category")
    output$plot_pp_totals <- renderPlotly({plot_pp_totals()})
    
    #Table of averages
    table_pp_means <- reactive({ #eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_mean_habits <- summary_table(data = plhdata_org_clean,
                                           columns_to_summarise = data_habit_parent_points_all,
                                           replace = "rp.contact.field.parent_point_count_",
                                           summaries = "mean",
                                           factors = NULL)
     summary_mean_habits %>% janitor::adorn_totals(c("col"))
    }) 
    plot_pp_means  <- reactive({
      summary_mean_completion_level_long <- pivot_longer(table_pp_means(), cols = everything(), names_to = "Parent Points", values_to = "Value") %>%
        filter(`Parent Points` != "Total")
      ggplot(summary_mean_completion_level_long, aes(x = `Parent Points`, y = Value, fill = `Parent Points`)) + 
        geom_bar(stat = "identity") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "Organisation", title = "Mean Parent points given in each category")
    }) 
    output$table_pp_means <- shiny::renderTable({(table_pp_means())}, striped = TRUE, caption = "Mean parent points for each category")
    output$plot_pp_means <- renderPlotly({plot_pp_means()})
    
    # pp_ table and plots --------------------
    pp_completion_table <- function(n, j = 1){
      return(output[[paste0("table_pp_", n)]] <-  shiny::renderTable({(summary_table_habits_all()[[j]])}, striped = TRUE))
    }
    pp_completion_plot <- function(n, j){
      return(output[[paste0("plot_pp_", n)]] <-  renderPlotly(summary_plot(data = plhdata_org_clean, columns_to_summarise = j, replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")))
    }
    map2(data_habit_parent_points_data$display_name, data_habit_parent_points_data$object_name, .f = ~ pp_completion_table(n = .y, j = .x))
    map2(data_habit_parent_points_data$metabase_ID, data_habit_parent_points_data$object_name, .f = ~ pp_completion_plot(n = .y, j = .x))
    
    #FOURTH Tab In-week Engagement ---------------------------
    tables_app_opens <- #reactive({ #(ifelse(input$goButton == 0, 1, input$goButton), {
      #summary_table_baseline_build <-
      summary_table_base_build(opt_factors = NULL,
                               data = plhdata_org_clean,
                               columns_to_summarise = data_app_opens,
                               replace = "rp.contact.field.app_launch_count",
                               retain_names_for_refactor = TRUE)
    names(tables_app_opens) <- data_app_opens_neat
    # Replace NA names with "0"
    tables_app_opens <- purrr::map(tables_app_opens, function(df) {
      names(df)[names(df) == "NA"] <- "0"
      df
    })
    
    # summary app opens table
    table_appopen_summary <- reactive({
      plhdata_org_clean %>%
        summarise(Min = min(rp.contact.field.app_launch_count, na.rm = TRUE),
                  Mean = mean(rp.contact.field.app_launch_count, na.rm = TRUE),
                  Max = max(rp.contact.field.app_launch_count, na.rm = TRUE),
                  SD = sd(rp.contact.field.app_launch_count, na.rm = TRUE))
    }) 
    output$table_appopen_summary <- shiny::renderTable({(table_appopen_summary())}, striped = TRUE)
    
    #App Opens tab 4.1
    table_appopen_totals <- reactive({
      tables_app_opens$`Overall`
    }) 
    plot_appopen_totals <- reactive({
      tables_app_opens_all <- bind_rows(tables_app_opens, .id = "Module")
      table_appopen_totals <- pivot_longer(tables_app_opens_all, cols = !Module, names_to = "App Opens")
      plot_fn <- ggplot(table_appopen_totals, aes(x = Module, y = value, fill = `App Opens`)) +
        geom_bar(stat = "identity") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "Module", y = "Frequency")
      plot_fn
    }) 
    output$table_appopen_totals <- shiny::renderTable({(table_appopen_totals())}, striped = TRUE)
    output$plot_appopen_totals <- renderPlotly({plot_appopen_totals()})
    
    table_appopen_mean_week <- reactive({ #(ifelse(input$goButton == 0, 1, input$goButton), {
      #Average app opens per ws week
      selected_data_ao <- add_na_variable(data = plhdata_org_clean, variable = data_app_opens)
      summary_mean_appopens <- summary_table(data = selected_data_ao, factors = NULL,
                                             summaries = "mean", columns_to_summarise = data_app_opens,
                                             replace = "rp.contact.field.app_launch_count_w_")
      summary_mean_appopens <- rename(summary_mean_appopens, "Total" = `Rp.contact.field.app launch count`)
      summary_mean_appopens %>% mutate_all(~replace(., is.na(.), 0))
    }) 
    plot_appopen_mean_week <- reactive({
      table_appopen_mean_week <- pivot_longer(table_appopen_mean_week(), cols = everything())
      plot_fn <- ggplot(table_appopen_mean_week, aes(x = name, y = value, fill = name)) +
        geom_bar(stat = "identity") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "Number of Opens", y = "Mean App Opens")
      plot_fn
    }) 
    output$table_appopen_mean_week <- shiny::renderTable({(table_appopen_mean_week())}, striped = TRUE)
    output$plot_appopen_mean_week <- renderPlotly({plot_appopen_mean_week()})
    
    table_appopen_self_care <- reactive({
      tables_app_opens$`Self Care (1)`
    }) 
    plot_appopen_self_care <- reactive({
      summary_plot(plhdata_org_clean, "rp.contact.field.app_launch_count_w_self_care", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot")
    })
    output$table_appopen_self_care <- shiny::renderTable({(table_appopen_self_care())}, striped = TRUE)
    output$plot_appopen_self_care <- renderPlotly({plot_appopen_self_care()})
    
    table_appopen_1on1 <- reactive({
      tables_app_opens$`1on1 (2)`
    }) 
    plot_appopen_1on1 <- reactive({
      summary_plot(plhdata_org_clean, "rp.contact.field.app_launch_count_w_1on1", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot")
    })
    output$table_appopen_1on1 <- shiny::renderTable({(table_appopen_1on1())}, striped = TRUE)
    output$plot_appopen_1on1 <- renderPlotly({plot_appopen_1on1()})
    
    table_appopen_praise <- reactive({
      tables_app_opens$`Praise (3)`
    }) 
    plot_appopen_praise <- reactive({
      summary_plot(plhdata_org_clean, "rp.contact.field.app_launch_count_w_praise", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot")
    })
    output$table_appopen_praise <- shiny::renderTable({(table_appopen_praise())}, striped = TRUE)
    output$plot_appopen_praise <- renderPlotly({plot_appopen_praise()})
    
    
    table_appopen_instructions <- reactive({
      tables_app_opens$`Positive Instructions(4)`
    }) 
    plot_appopen_instructions <- reactive({
      summary_plot(plhdata_org_clean, "rp.contact.field.app_launch_count_w_instruct", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot")
    })
    output$table_appopen_instructions <- shiny::renderTable({(table_appopen_instructions())}, striped = TRUE)
    output$plot_appopen_instructions <- renderPlotly({plot_appopen_instructions()})
    
    
    table_appopen_stress <- reactive({
      tables_app_opens$`Managing Stress(5)`
    }) 
    plot_appopen_stress <- reactive({
      summary_plot(plhdata_org_clean, "rp.contact.field.app_launch_count_w_stress", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot")
    })
    output$table_appopen_stress <- shiny::renderTable({(table_appopen_stress())}, striped = TRUE)
    output$plot_appopen_stress <- renderPlotly({plot_appopen_stress()})
    
    
    table_appopen_budget <- reactive({
      tables_app_opens$`Family Budget(6)`
    }) 
    plot_appopen_budget <- reactive({
      summary_plot(plhdata_org_clean, "rp.contact.field.app_launch_count_w_money", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot")
    })
    output$table_appopen_budget <- shiny::renderTable({(table_appopen_budget())}, striped = TRUE)
    output$plot_appopen_budget <- renderPlotly({plot_appopen_budget()})
    
    
    table_appopen_rules <- reactive({
      tables_app_opens$`Rules(7)`
    }) 
    plot_appopen_rules <- reactive({
      summary_plot(plhdata_org_clean, "rp.contact.field.app_launch_count_w_rules", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot")
    })
    output$table_appopen_rules <- shiny::renderTable({(table_appopen_rules())}, striped = TRUE)
    output$plot_appopen_rules <- renderPlotly({plot_appopen_rules()})
    
    
    table_appopen_consequences <- reactive({
      tables_app_opens$`Calm Consequences(8)`
    }) 
    plot_appopen_consequences <- reactive({
      summary_plot(plhdata_org_clean, "rp.contact.field.app_launch_count_w_consequence", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot")
    })
    output$table_appopen_consequences <- shiny::renderTable({(table_appopen_consequences())}, striped = TRUE)
    output$plot_appopen_consequences <- renderPlotly({plot_appopen_consequences()})
    
    
    table_appopen_problem_solving <- reactive({
      tables_app_opens$`Problem Solving(6)`
    }) 
    plot_appopen_problem_solving <- reactive({
      summary_plot(plhdata_org_clean, "rp.contact.field.app_launch_count_w_solve", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot")
    })
    output$table_appopen_problem_solving <- shiny::renderTable({(table_appopen_problem_solving())}, striped = TRUE)
    output$plot_appopen_problem_solving <- renderPlotly({plot_appopen_problem_solving()})
    
    
    table_appopen_teen_safety <- reactive({
      tables_app_opens$`Teen Safety(10)`
    }) 
    plot_appopen_teen_safety <- reactive({
      summary_plot(plhdata_org_clean, "rp.contact.field.app_launch_count_w_safe", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot")
    })
    output$table_appopen_teen_safety <- shiny::renderTable({(table_appopen_teen_safety())}, striped = TRUE)
    output$plot_appopen_teen_safety <- renderPlotly({plot_appopen_teen_safety()})
    
    
    table_appopen_crisis <- reactive({
      tables_app_opens$`Crisis(11)`
    }) 
    plot_appopen_crisis <- reactive({
      summary_plot(plhdata_org_clean, "rp.contact.field.app_launch_count_w_crisis", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot")
    })
    output$table_appopen_crisis <- shiny::renderTable({(table_appopen_crisis())}, striped = TRUE)
    output$plot_appopen_crisis <- renderPlotly({plot_appopen_crisis()})
    
    
    table_appopen_celebration <- reactive({
      tables_app_opens$`Celebration & Next Steps(12)`
    }) 
    plot_appopen_celebration <- reactive({
      summary_plot(plhdata_org_clean, "rp.contact.field.app_launch_count_w_celebrate", replace = "rp.contact.field.app_launch_count_w_", plot_type = "boxplot")
    })
    output$table_appopen_celebration <- shiny::renderTable({(table_appopen_celebration())}, striped = TRUE)
    output$plot_appopen_celebration <- renderPlotly({plot_appopen_celebration()})
    
    nf_data_join <- reactive({
      plhorg_nf <- plhdata_org_clean %>%
            dplyr::select(c("app_user_id"))
      # link nf data to user data by app_user_id
      # use inner_join: remove from nf anyone not in plhdata_org
      inner_join(nf_data, plhorg_nf)
    })
    
    # Push notifications tab 4.2
    table_pushn_totals <- reactive({notification_summary(nf_data_join(), factors = NULL) %>%
        mutate(`Notifications responded to` = paste0(round(replied / received * 100, 1), "% (", replied, "/", received, ")")) %>%
        dplyr::select(-c("replied", "received"))}) 
    output$table_pushn_totals <- shiny::renderTable({(table_pushn_totals())}, striped = TRUE)
    
    table_pushn_mean <- reactive({
      notification_summary(nf_data_join(), factors = c("campaign_id")) %>%
        mutate(`Notifications responded to` = paste0(round(replied / received * 100, 1), "% (", replied, "/", received, ")")) %>%
        dplyr::select(-c("replied", "received"))
    })
    plot_pushn_mean <- reactive({
      notif <- notification_summary(nf_data_join(), factors = c("campaign_id")) %>%
        mutate(perc_received = replied/received)
      
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
    summary_table_hp_totals <- reactive({ #(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = NULL,
                                                               data = plhdata_org_clean,
                                                               columns_to_summarise = data_hp_started,
                                                               replace = "rp.contact.field.w_",
                                                               replace_after = "_hp_review_started")
      summary_table_baseline_build %>%
        purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0))) %>%
        purrr::map(.f =~.x %>% janitor::adorn_totals(c("col")))
    })
    
    relative_hp_started <- reactive({
      relative_hp_started <- summary_table_hp_totals() %>%
        purrr::map(~ .x %>%
                     mutate_all(~ replace(., is.na(.), 0)) %>%
                     { if (!"True" %in% names(.)) mutate(., True = 0) else . } %>%
                     select(True)
        )
      return(relative_hp_started)   
    })
    table_hp_started <- reactive({
      relative_hp_started() %>%
        list_rbind(names_to = "id") %>%
        dplyr::select(id, True) %>%
        pivot_wider(names_from = id, values_from = True) %>%
        dplyr::rename_all(~gsub("_", ".", .))
    })
    plot_hp_started  <- reactive({
      relative_hp_started <- imap(relative_hp_started(), ~.x %>%
                                    rename(value = True))
      hp_mood_plot(relative_hp_started, factors = NULL, manipulation = "ldply",
                   limits =  c("Praise", "Instruct", "Stress", "Solve", "Money", "Rules", "Consequence", "Safe", "Crisis"),
                   xlab = "Workshop week", fill = FALSE, fill_colour = "#78D473")
    }) 
    output$table_hp_started <- shiny::renderTable({(table_hp_started())}, striped = TRUE)
    output$plot_hp_started <- renderPlotly({plot_hp_started()})
    
    # home practice review - user claims they had a chance to do the hp
    
    summary_table_hp_done <- reactive({ #(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = NULL,
                                                               data = plhdata_org_clean,
                                                               columns_to_summarise = data_hp_done,
                                                               replace = "rp.contact.field.w_",
                                                               replace_after = "_hp_done")
      summary_table_baseline_build %>%
        purrr::map(~ {
          df <- .x %>% dplyr::mutate(across(everything(), ~ replace(., is.na(.), 0)))
          if (ncol(df) >= 2) df <- janitor::adorn_totals(df, "col", where(is.numeric))
          df
        })
    })
    
    relative_hp_done <- reactive({
      summary_table_hp_done <- summary_table_hp_done()
      for (i in 1:length(summary_table_hp_done)){
        if (!"Yes" %in% colnames(summary_table_hp_done[[i]])) {
          summary_table_hp_done[[i]]$Yes <- 0
        }
      }
      select_items <- c("Yes")
      relative_hp_done <- imap(summary_table_hp_done, ~.x %>%
                                 select(select_items))
      return(relative_hp_done)
    })
    table_perc_long <- reactive({ #(ifelse(input$goButton == 0, 1, input$goButton), {
      # relative_hp_done_stress <- summary_table_base_build(opt_factors = NULL,
      #                                                     data = plhdata_org_clean,
      #                                                     columns_to_summarise = "rp.contact.field.w_stress_hp_done",
      #                                                     replace = "rp.contact.field.w_",
      #                                                     replace_after = "_hp_done")
      # relative_hp_done_stress <- relative_hp_done_stress %>%
      #   purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      # relative_hp_done_stress <- relative_hp_done_stress %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
      # 
      #relative_hp_done_stress <- plyr::ldply(relative_hp_done_stress)
      hp_started_long <- plyr::ldply(relative_hp_started())
      hp_done_long <- plyr::ldply(relative_hp_done())
      
      hp_done_long <- full_join(hp_started_long, hp_done_long)
      table_perc_long <- hp_done_long %>%
        rename(Started = "value") %>%
        rename(Completed = "Yes") %>%
        mutate(Completed = ifelse(is.na(Completed), 0, Completed)) %>%
        mutate(Started = ifelse(is.na(Started), 0, Started)) %>%
        mutate(perc_complete = Completed / Started) %>%
        mutate(perc_complete = ifelse(is.na(perc_complete), 0, perc_complete))
        
      return(table_perc_long)
    })
    table_hp_done <- reactive({
      table_hp_done <- table_perc_long() %>%
        pivot_wider(names_from = .id, values_from = perc_complete) %>%
        mutate(dplyr::across(everything(), ~ifelse(is.na(.), 0, .)))
      return(table_hp_done)
    })
    plot_hp_done  <- reactive({
      summary_mean_completion_level_long <- table_perc_long() %>%
        rename(value = perc_complete) %>% rename(name = `.id`)
      hp_mood_plot(summary_mean_completion_level_long, factors = NULL, manipulation = "none", limits = c("1on1", "Praise", "Instruct", "Stress", "Solve", "Money", "Rules", "Consequence", "Safe", "Crisis"),
                   xlab = "Workshop week", fill = FALSE, fill_colour = "#78D473")
    }) 
    output$table_hp_done <- shiny::renderTable({(table_hp_done())}, striped = TRUE)
    output$plot_hp_done <- renderPlotly({plot_hp_done()})
  
    #SIXTH Tab Parent Library
    summary_table_library <- reactive({ #(ifelse(input$goButton == 0, 1, input$goButton), {
      summary_table_baseline_build <- summary_table_base_build(opt_factors = NULL, data = plhdata_org_clean, columns_to_summarise = data_library,
                                                               replace = "rp.contact.field.click_pc_",
                                                               replace_after = "count")
      data_library_neat <- naming_conventions(names(summary_table_library), replace = "Rp.contact.field.click hs")
      names(summary_table_baseline_build) <- data_library_neat
      summary_table_baseline_build %>%
        purrr::map(~ {
          df <- .x %>% dplyr::mutate(across(everything(), ~ replace(., is.na(.), 0)))
          if (ncol(df) >= 2) df <- janitor::adorn_totals(df, "col", where(is.numeric))
          df
        })
      })
    
    #average clicks on parent library (mean per org)
    table_library_mean <- reactive({ #(ifelse(input$goButton == 0, 1, input$goButton), {
      #mean library clicks (button type per organisation)
      #mean library clicks per workshop week is not stored to my knowledge
      table_library_data <- plhdata_org_clean %>%
        mutate(across(data_library, ~as.numeric(.x)))
      
      summary_mean_library_data <- summary_table(data = table_library_data,
                                                 columns_to_summarise = data_library,
                                                 replace = "rp.contact.field.click_",
                                                 replace_after = "_count",
                                                 summaries = "mean",
                                                 factors = NULL)
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
                                                cols = everything(),
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
    plot_library_totals  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_hs_parent_centre_count", replace = "rp.contact.field.click_hs_")})
    output$table_library_totals <- shiny::renderTable({(table_library_totals())}, striped = TRUE)
    output$plot_library_totals <- renderPlotly({plot_library_totals()})
    
    #my tips
    table_lib_tips <- reactive({summary_table_library()$`My tips `  }) 
    plot_lib_tips  <- reactive({summary_plot(data= plhdata_org_clean, "rp.contact.field.click_pc_my_tips_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_tips <- shiny::renderTable({(table_lib_tips())}, striped = TRUE)
    output$plot_lib_tips <- renderPlotly({plot_lib_tips()})
    
    #essential tools
    table_lib_tools <- reactive({summary_table_library()$`Essential tools `  }) 
    plot_lib_tools  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_essential_tools_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_tools <- shiny::renderTable({(table_lib_tools())}, striped = TRUE)
    output$plot_lib_tools <- renderPlotly({plot_lib_tools()})
    
    #relax and activities
    table_lib_activities <- reactive({summary_table_library()$`Relax and activities `  }) 
    plot_lib_activities  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_relax_and_activities_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_activities <- shiny::renderTable({(table_lib_activities())}, striped = TRUE)
    output$plot_lib_activities <- renderPlotly({plot_lib_activities()})
    
    #customisse ParentApp
    table_lib_custom <- reactive({summary_table_library()$`Customisation `  }) 
    plot_lib_custom  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_customisation_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_custom <- shiny::renderTable({(table_lib_custom())}, striped = TRUE)
    output$plot_lib_custom <- renderPlotly({plot_lib_custom()})
    
    #help - quick parenting tips
    table_lib_help <- reactive({summary_table_library()$`Help `  }) 
    plot_lib_help  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_help_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_help <- shiny::renderTable({(table_lib_help())}, striped = TRUE)
    output$plot_lib_help <- renderPlotly({plot_lib_help()})
    
    #technical support
    table_lib_tech <- reactive({summary_table_library()$`Technical support `  }) 
    plot_lib_tech  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_technical_support_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_tech <- shiny::renderTable({(table_lib_tech())}, striped = TRUE)
    output$plot_lib_tech <- renderPlotly({plot_lib_tech()})
    
    #coping with COVID 
    table_lib_covid <- reactive({summary_table_library()$`Covid `  }) 
    plot_lib_covid  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_covid_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_covid <- shiny::renderTable({(table_lib_covid())}, striped = TRUE)
    output$plot_lib_covid <- renderPlotly({plot_lib_covid()})
    
    #coping with grief / bereavement
    table_lib_grief <- reactive({summary_table_library()$`Bereavement `  }) 
    plot_lib_grief  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_bereavement_count", replace = "rp.contact.field.click_pc_")})
    output$table_lib_grief <- shiny::renderTable({(table_lib_grief())}, striped = TRUE)
    output$plot_lib_grief <- renderPlotly({plot_lib_grief()})
    
    ## Download sheet -----------------------------------------
    download_data_start <- reactive({
      plhdata_group_ids <- plhdata_org_clean %>% select(c('app_user_id', "createdAt", all_of(data_completion_level)))
      plhdata_group_ids_group_1 <- threshhold_function(data = plhdata_group_ids, threshhold = 0)
      plhdata_group_ids_group_1 <- plhdata_group_ids_group_1 %>%
        mutate(engagement_total = self_care_started + `1on1_started` + praise_started + 
                 instruct_started + stress_started + money_started + rules_started + consequence_started + 
                 solve_started + safe_started + crisis_started + celebrate_started)
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
        
        plhdata_org_clean <- plhdata_org_clean
        plhdata_org_clean_CL <- plhdata_org_clean %>%
          dplyr::mutate(across(data_completion_level, ~ifelse(. == 100, TRUE, FALSE))) %>%
          dplyr::select(app_user_id, updated_id, data_completion_level)
        colnames(plhdata_org_clean_CL) <- gsub("_completion_level", "_completed", colnames(plhdata_org_clean_CL))
        
        plhdata_org_clean_1 <- plhdata_org_clean %>%
          dplyr::select(c(app_user_id, app_version, data_completion_level))
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
                        "CreatedAt", "App version", "Hours since sync", "1 relative_perc_completed completion level" = "Self care completion level",
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
      table_ws_started <- relative_perc_completed
      table_ws_started_additional <- additional_relative_perc_completed()
      table_ws_totals1 <- table_ws_totals() %>%
        pivot_longer(cols = everything(),
                     names_to = "Workshop",
                     values_to = "Average completion level")
      table_ws_totals1_additional <- additional_table_ws_totals() %>%
        pivot_longer(cols = everything(),
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
      rct_access <- plhdata_org_clean %>%
        dplyr::mutate(days_since_last_sync = as.Date(Sys.Date()) - as.Date(rp.contact.field._server_sync_latest)) %>%
        dplyr::select(c(app_user_id, RCT_access=rp.contact.field.post_rct_access,
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
