
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
        menuItem("Parent Points", tabName = "parentpoints", icon = icon("star")),
        menuItem("In-week Engagement", tabName = "xtraengagement", icon = icon("user-check")),
        menuItem("Surveys", tabName = "surveys", icon = icon("question")),
        menuItem("Parent Library", tabName = "library", icon = icon("book-reader"))
      )), #closes sidebarMenu and dashboardSidebar
    
    dashboardBody(# Boxes need to be put in a row (or column)
      top_boxes(country = country), #closes fluidRow
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
                      title = "App first downloaded and opened",
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
                      plotlyOutput(outputId = "plot_language", height = "240"), #generates graph
                      shiny::tableOutput("table_language")  #generates table
                  ), #closes box
                  
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Workshop format",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_ws_format", height = "240"),
                      shiny::tableOutput("table_ws_format")
                  )), #closes box, fluid row
                  
                fluidRow(
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "App version",
                      status = "primary", # primary, success, info, warning, danger
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_app_version", height = "240"),
                      shiny::tableOutput("table_app_version")
                  ), #closes box
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Parent gender",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_parent_gender", height = "240"), #generates graph
                      shiny::tableOutput("table_parent_gender")  #generates table
                  ) #closes box
                ), #closes fluidRow

                fluidRow(
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Parent age",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_parent_age", height = "240"),
                      shiny::tableOutput("table_parent_age")
                  ), #closes box
                  
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Adults in household",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_household_adults", height = "240"),
                      shiny::tableOutput("table_household_adults")
                  ) #closes box
                ), #closes fluidRow
                
                fluidRow(
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Teens in household",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_household_teens", height = "240"),
                      shiny::tableOutput("table_household_teens")
                  ), #closes box
                  
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Children in household",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_household_children", height = "240"),
                      shiny::tableOutput("table_household_children")
                  ) #closes box
                ), # closes fluidrow
                  
                fluidRow(
                  box(width = 6,
                      collapsible = TRUE,
                      solidHeader = TRUE,
                      title = "Babies in household",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_household_babies", height = "240"),
                      shiny::tableOutput("table_household_babies")
                  ) #closes box
                ) #closes fluid row
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
                                           title = "Average workshop completion",
                                           status = "info",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_ws_totals", height = "240"),
                                           shiny::tableOutput("table_ws_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
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
                                           title = "Workshop 6: Family Budgets",
                                           status = "info",  
                                           h5("As individual: percentage out of 18 steppers"), h5("As group: percentage out of 18 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_money", height = "240"),
                                           shiny::tableOutput("table_w_money")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 7: Rules",
                                           status = "info",  
                                           h5("As individual: percentage out of 10 steppers"), h5("As group: npercentage out of 11 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_rules", height = "240"),
                                           shiny::tableOutput("table_w_rules")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 8: Calm Consequences",
                                           status = "info",  
                                           h5("As individual: percentage out of 12 steppers"), h5("As group: npercentage out of 14 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_consequence", height = "240"),
                                           shiny::tableOutput("table_w_consequence")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Workshop 9: Problem Solving",
                                           status = "info",  
                                           h5("As individual: percentage out of 10 steppers"), h5("As group: npercentage out of 12 steppers"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_w_solve", height = "240"),
                                           shiny::tableOutput("table_w_solve")
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
                            ), # closes Overview tabPanel
                            
                            tabPanel("Additional Insights",
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Number of users who have started a workshop",
                                           status = "info",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_ws_started", height = "240"),
                                           shiny::tableOutput("table_ws_started")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Percentage of starters who completed a workshop",
                                           status = "info",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_ws_rel_completed", height = "240"),
                                           shiny::tableOutput("table_ws_rel_completed")
                                       )#closes box
                                     ) #closes fluid row
                                     
                            ) # closes Additional Insights
                ) #closes tabsetPanel for workshop
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
                                           title = "Relax Points in Workshop 6: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_money")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 7: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_rules")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 8: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_consequence")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Relax Points in Workshop 9: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_relax_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_relax_w_solve")
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
                                           title = "Treat Yourself Points in Workshop 6: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_money")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 7: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_rules")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 8: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_consequence")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Treat Yourself Points in Workshop 9: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_treat_yourself_w_solve")
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
                                           title = "Praise Yourself Points in Workshop 6: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_money")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 7: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_rules")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 8: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_consequence")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Yourself Points in Workshop 9: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_praise_yourself_w_solve")
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
                                           title = "1-on-1 Time Points in Workshop 6: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_money")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 7: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_rules")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 8: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_consequence")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "1-on-1 Time Points in Workshop 9: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_spend_time_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_spend_time_w_solve")
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
                                           title = "Praise Teen Points in Workshop 6: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_money")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 7: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_rules")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 8: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_consequence")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Praise Teen Points in Workshop 9: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_praise_teen_w_solve")
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
                                           title = "Get Positive Points in Workshop 6: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_money")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 7: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_rules")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 8: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_consequence")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Get Positive Points in Workshop 9: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_instruct_positively_w_solve")
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
                                           title = "Breathe Points in Workshop 6: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_money")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 7: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_rules")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 8: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_consequence")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Breathe Points in Workshop 9: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_breathe_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_breathe_w_solve")
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
                                           title = "Good Money Choice Points in Workshop 6: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_money")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 7: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_rules")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 8: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_consequence")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Good Money Choice Points in Workshop 9: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_money_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_money_w_solve")
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
                                           title = "Calm Consequence Points in Workshop 6: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_money")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 7: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_rules")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 8: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_consequence")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Calm Consequence Points in Workshop 9: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_consequence_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_consequence_w_solve")
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
                                           title = "Safe Points in Workshop 6: Family Budgets",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_money", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_money")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 7: Rules",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_rules", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_rules")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 8: Calm Consequences",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_consequence", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_consequence")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Safe Points in Workshop 9: Problem Solving",
                                           status = "warning",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_pp_safe_w_solve", height = "240"),
                                           shiny::tableOutput("table_pp_safe_w_solve")
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
                                                     "6 Family Budgets" = "w_money",
                                                     "7 Rules" = "w_rules",
                                                     "8 Calm Consequences" = "w_consequence" ,
                                                     "9 Problem Solving" = "w_solve",
                                                     "10	Teen Safety" = "w_safe",
                                                     "11 Dealing with Crisis" = "w_crisis",
                                                     "12 Celebration and Next Steps" = "w_celebrate"),
                                         selected = c("w_self_care", "w_1on1", "w_praise", "w_instruct","w_stress","w_money", "w_rules", "w_consequence", "w_solve", "w_safe", "w_praise", "w_crisis", "w_celebrate"),
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
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_appopen_totals", height = "240"),
                                           shiny::tableOutput("table_appopen_totals")
                                       ) #closes box
                                     ), #closes fluidrow
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Mean app opens per workshop week",
                                           status = "success",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_appopen_mean_week", height = "240"),
                                           shiny::tableOutput("table_appopen_mean_week")
                                       )#closes box
                                     ) #closes fluid row
                            ), #closes tabPanel App Opens
                            
                            tabPanel("Push Notifications",
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Push notification clicks overview",
                                           status = "success",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_pushn_totals", height = "240"),
                                           shiny::tableOutput("table_pushn_totals")
                                       )#closes box
                                       # ), #closes fluidrow
                                       #          
                                       #          fluidRow(
                                       #            box(width = 12,
                                       #                collapsible = TRUE,
                                       #                solidHeader = TRUE,
                                       #                title = "Push notification types sent",
                                       #                status = "success",  
                                       #                #background = "orange",
                                       #                plotlyOutput(outputId = "plot_pushn_mean", height = "240"), #needs to be renames
                                       #                shiny::tableOutput("table_pushn_mean")
                                       #            ) #closes box
                                     ) #closes fluid row
                            ), #closes tabPanel Push Notifications
                            
                            tabPanel("Home Practice",
                                     # fluidRow(
                                     #   box(width = 12,
                                     #       collapsible = TRUE,
                                     #       solidHeader = TRUE,
                                     #       title = "Number of users starting each home practice (currently not collected)",
                                     #       status = "success",  
                                     #       #background = "orange",
                                     #       plotlyOutput(outputId = "plot_homep_start", height = "240"),
                                     #       shiny::tableOutput("table_homep_start")
                                     #   ) #closes box
                                     # ), #closes fluidrow
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Number of users starting each home practice review (value is true rather than false or missing)",
                                           status = "success",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_hp_rev_totals", height = "240"),
                                           shiny::tableOutput("table_hp_rev_totals")
                                       )#closes box
                                     ), #closes fluid row  
                                     
                                     fluidRow(
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 2: One-on-One Time - completion", #NB this is the reported completion of HP itself, not ther Review of the HP (stored under variable rp-contact-field.w_stress_hp_review_completed)
                                           status = "success",
                                           h5("Question for this column phrased as 'Did you have a chance to do this?'"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_hpdone_1on1", height = "240"),
                                           shiny::tableOutput("table_hpdone_1on1")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 2: One-on-One Time - how it went",
                                           h5("Question for this column only asked if completion question answered with 'Yes'; phrased as 'How did it go?'"),
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_mood_1on1", height = "240"),
                                           shiny::tableOutput("table_mood_1on1")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 2: One-on-One Time - challenges selected",
                                           status = "success",  
                                           h5("Question for this column phrased as 'What challenge did you have?' and asked irespective of previous answers."),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_chall_1on1", height = "240"),
                                           shiny::tableOutput("table_chall_1on1")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 3: Praise - completion",
                                           status = "success",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_hpdone_praise", height = "240"),
                                           shiny::tableOutput("table_hpdone_praise")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "No 'review' for week 3 home practice",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_mood_praise", height = "240"),
                                           shiny::tableOutput("table_mood_praise")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "No challenges for week 3 home practice",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_chall_praise", height = "240"),
                                           shiny::tableOutput("table_chall_praise")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 4: Positive Instructions - completion",
                                           status = "success",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_hpdone_instruct", height = "240"),
                                           shiny::tableOutput("table_hpdone_instruct")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 4: Positive Instructions - how it went",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_mood_instruct", height = "240"),
                                           shiny::tableOutput("table_mood_instruct")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 4: Positive Instructions - challenges selected",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_chall_instruct", height = "240"),
                                           shiny::tableOutput("table_chall_instruct")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 5: Managing Stress (breathe) - completion",
                                           status = "success",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_hpdone_stress_br", height = "240"),
                                           shiny::tableOutput("table_hpdone_stress_br")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 5: Managing Stress (breathe) - how it went",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_mood_stress_br", height = "240"),
                                           shiny::tableOutput("table_mood_stress_br")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 5: Managing Stress (breathe & talk) - challenges selected",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_chall_stress_br", height = "240"),
                                           shiny::tableOutput("table_chall_stress_br")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 5: Managing Stress (talk) - completion",
                                           status = "success",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_hpdone_stress_tk", height = "240"),
                                           shiny::tableOutput("table_hpdone_stress_tk")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 5: Managing Stress (talk) - how it went",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_mood_stress_tk", height = "240"),
                                           shiny::tableOutput("table_mood_stress_tk")
                                           # ), #closes box
                                           # 
                                           # box(width = 4,
                                           #     collapsible = TRUE,
                                           #     solidHeader = TRUE,
                                           #     title = "Home Practice Week 5: Managing Stress (talk) - challenges selected",
                                           #     status = "success",  
                                           #     style='width:100%;overflow-x: scroll;',
                                           #     plotlyOutput(outputId = "plot_chall_stress_tk", height = "240"),
                                           #     shiny::tableOutput("table_chall_stress_tk")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 6: Budgets - completion",
                                           status = "success",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_hpdone_money", height = "240"),
                                           shiny::tableOutput("table_hpdone_money")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 6: Budgets - how it went",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_mood_money", height = "240"),
                                           shiny::tableOutput("table_mood_money")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 6: Budgets - challenges selected",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_chall_money", height = "240"),
                                           shiny::tableOutput("table_chall_money")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 7: Rules - completion",
                                           status = "success",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_hpdone_rules", height = "240"),
                                           shiny::tableOutput("table_hpdone_rules")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 7: Rules - how it went",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_mood_rules", height = "240"),
                                           shiny::tableOutput("table_mood_rules")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 7: Rules - challenges selected",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_chall_rules", height = "240"),
                                           shiny::tableOutput("table_chall_rules")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 8: Calm Consequences - completion",
                                           status = "success",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_hpdone_consequence", height = "240"),
                                           shiny::tableOutput("table_hpdone_consequence")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 8: Calm Consequences - how it went",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_mood_consequence", height = "240"),
                                           shiny::tableOutput("table_mood_consequence")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 8: Calm Consequences - challenges selected",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_chall_consequence", height = "240"),
                                           shiny::tableOutput("table_chall_consequence")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 9: Problem Solving - completion",
                                           status = "success",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_hpdone_solve", height = "240"),
                                           shiny::tableOutput("table_hpdone_solve")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 9: Problem Solving - how it went",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_mood_solve", height = "240"),
                                           shiny::tableOutput("table_mood_solve")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 9: Problem Solving - challenges selected",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_chall_solve", height = "240"),
                                           shiny::tableOutput("table_chall_solve")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 10: Teen Safety - completion",
                                           status = "success",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_hpdone_safe", height = "240"),
                                           shiny::tableOutput("table_hpdone_safe")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 10: Teen Safety - how it went",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_mood_safe", height = "240"),
                                           shiny::tableOutput("table_mood_safe")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 10: Teen Safety - challenges selected",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_chall_safe", height = "240"),
                                           shiny::tableOutput("table_chall_safe")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 11: Dealing with Crisis - completion",
                                           status = "success",
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_hpdone_crisis", height = "240"),
                                           shiny::tableOutput("table_hpdone_crisis")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 11: Dealing with Crisis - how it went",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_mood_crisis", height = "240"),
                                           shiny::tableOutput("table_mood_crisis")
                                       ), #closes box
                                       
                                       box(width = 4,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Home Practice Week 11: Dealing with Crisis - challenges selected",
                                           status = "success",  
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_chall_crisis", height = "240"),
                                           shiny::tableOutput("table_chall_crisis")
                                       ) #closes box
                                     ) #closes fluid row         
                            ), #closes tabPanel Home practice
                            
                            tabPanel("Quick Start Buttons",
                                     
                            ), #closes tabPanel Quick Start Buttons
                            
                            tabPanel("Time Spent",
                                     
                            ) #closes tabPanel Time spent
                ) #closes tabset panel for In-week engagement
        ), # closes fourth tabItem
        
        #FIFTH tab content
        tabItem(tabName = "surveys",
                
                fluidRow(
                  column(12, align = "centre",
                         # splitLayout gets two boxes side by side.
                         # in this case, it is just the header (h2), and an icon
                         # we want 80% of the width to be the header (h2) and 20% the icon (hence cellWidths = ...)
                         box(splitLayout(h2("Surveys"), icon("question", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 15,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = "red",
                             height = "95px")
                  ) #closes box
                ), #closes fluid row
                
                tabsetPanel(type = "tabs",
                            tabPanel("Baseline survey",
                                     
                                     #fluidRow(checkbox_input(inputId = "SV1", country = country)), #closes fluidRow
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Baseline survey completion",
                                           status = "danger", 
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_sv1_totals", height = "240"),
                                           shiny::tableOutput("table_sv1_totals")
                                       )#closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of attention",
                                           status = "danger",  
                                           h5("How many days in the past week were you able to give them your attention and do something that they enjoyed with them? [0-7],
                                                              NB the question was changed for the Tanzania rollout to: How many days in the past week were you able to do something fun together?"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv1_attention", height = "240"),
                                           shiny::tableOutput("table_sv1_attention")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of praise",
                                           status = "danger",
                                           h5("How many days in the past week have you praised your teen? [0-7]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv1_praise", height = "240"),
                                           shiny::tableOutput("table_sv1_praise")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of stress",
                                           status = "danger",  
                                           h5("How many days in the past week did you feel very stressed as a parent/caregiver? [0-7]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv1_stress", height = "240"),
                                           shiny::tableOutput("table_sv1_stress")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of shouting",
                                           status = "danger",
                                           h5("How many days in the past week did you shout, scream or yell at your teen? [0-7]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv1_shout", height = "240"),
                                           shiny::tableOutput("table_sv1_shout")
                                       ) #closes box
                                     ),#closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of money worries",
                                           status = "danger",  
                                           h5("How many days in the past week have you worried or felt anxious about money? [0-7]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv1_money", height = "240"),
                                           shiny::tableOutput("table_sv1_money")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days out of food money (last month)",
                                           status = "danger",  
                                           h5("How many days in the past month (30 days) did you run out of money to pay for food? [0-30]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv1_food_money", height = "240"),
                                           shiny::tableOutput("table_sv1_food_money")
                                       ) #closes box
                                     ),#closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of hitting",
                                           status = "danger", 
                                           h5("In the past week, did you physically discipline your children by hitting, spanking, or slapping with your hand or an object like a stick or a belt? [0-7]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv1_hitting", height = "240"),
                                           shiny::tableOutput("table_sv1_hitting")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days with knowledge of teen activity",
                                           status = "danger", 
                                           h5("How many days in the past week did you know what your teen was doing and who they were with? [0-7] NB this question was removed for the Tanzania rollout."),
                                           #h7("NB next question only asked if this one is answered with '7'"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv1_week_teen_activity", height = "240"),
                                           shiny::tableOutput("table_sv1_week_teen_activity")
                                       ) #closes box
                                     ),#closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Lockdown",
                                           status = "danger",
                                           h5("Were you in lockdown this week? [Yes/No], NB next question only asked if this one is answered with 'Yes', NB this question was removed for the Tanzania rollout."),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv1_lockdown", height = "240"),
                                           shiny::tableOutput("table_sv1_lockdown")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Non-lockdown knowledge of teen activity",
                                           status = "danger",
                                           h5("How many days in a typical non-lockdown week do you know what your teen is doing and who they are with? [0-7],
                                                              NB this question is only asked if the parent said they had knowledge 7/7 days and then indicated that it was a lockdown week,
                                                              NB this question was removed for the Tanzania rollout."),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv1_reg_teen_activity", height = "240"),
                                           shiny::tableOutput("table_sv1_reg_teen_activity")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of sexual safety talk (last month)",
                                           status = "danger",  
                                           h5("How many days in the past month (30 days) did you talk with your teen about keeping safe from sexual violence online or offline? This could be when they are going out with friends, or talking about the websites they use. [0-30, steps of 5],
                                                           NB the question was changed for the Tanzania rollout to:
                                                          [In the past month, did you talk with your teen about keeping safe from sexual violence online or offline? This could be about going out with friends or about the websites and apps they use.]
                                                              How many days have you had a talk like this?"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv1_sex_talk", height = "240"),
                                           shiny::tableOutput("table_sv1_sex_talk")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of COVID safe teenager behaviour",
                                           status = "danger", 
                                           h5("How many days last week did your teenager stay safe through wearing a mask, keeping a distance from people and keeping away from crowded places? [0-7]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv1_covid_safe", height = "240"),
                                           shiny::tableOutput("table_sv1_covid_safe")
                                       ) #closes box
                                     )#closes fluid row
                            ), #closes baseline tab panel
                            
                            
                            tabPanel("Endline survey",
                                     #fluidRow(checkbox_input(inputId = "SV2", country = country)), #closes fluidRow
                                     
                                     fluidRow(
                                       box(width = 12,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Endline survey completion",
                                           status = "danger",  
                                           #background = "orange",
                                           plotlyOutput(outputId = "plot_sv2_totals", height = "240"),
                                           shiny::tableOutput("table_sv2_totals")
                                       )#closes box
                                     ), # closes fluidRow
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of attention",
                                           status = "danger",  
                                           h5("How many days in the past week were you able to give them your attention and do something that they enjoyed with them? [0-7],
                                                              NB the question was changed for the Tanzania rollout to: How many days in the past week were you able to do something fun together?"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv2_attention", height = "240"),
                                           shiny::tableOutput("table_sv2_attention")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of praise",
                                           status = "danger",
                                           h5("How many days in the past week have you praised your teen? [0-7]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv2_praise", height = "240"),
                                           shiny::tableOutput("table_sv2_praise")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of stress",
                                           status = "danger",  
                                           h5("How many days in the past week did you feel very stressed as a parent/caregiver? [0-7]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv2_stress", height = "240"),
                                           shiny::tableOutput("table_sv2_stress")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of shouting",
                                           status = "danger",
                                           h5("How many days in the past week did you shout, scream or yell at your teen? [0-7]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv2_shout", height = "240"),
                                           shiny::tableOutput("table_sv2_shout")
                                       ) #closes box
                                     ),#closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of money worries",
                                           status = "danger",  
                                           h5("How many days in the past week have you worried or felt anxious about money? [0-7]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv2_money", height = "240"),
                                           shiny::tableOutput("table_sv2_money")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days out of food money (last month)",
                                           status = "danger",  
                                           h5("How many days in the past month (30 days) did you run out of money to pay for food? [0-30]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv2_food_money", height = "240"),
                                           shiny::tableOutput("table_sv2_food_money")
                                       ) #closes box
                                     ),#closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of hitting",
                                           status = "danger", 
                                           h5("In the past week, did you physically discipline your children by hitting, spanking, or slapping with your hand or an object like a stick or a belt? [0-7]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv2_hitting", height = "240"),
                                           shiny::tableOutput("table_sv2_hitting")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days with knowledge of teen activity",
                                           status = "danger", 
                                           h5("How many days in the past week did you know what your teen was doing and who they were with? [0-7] NB this question was removed for the Tanzania rollout."),
                                           #h7("NB next question only asked if this one is answered with '7'"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv2_week_teen_activity", height = "240"),
                                           shiny::tableOutput("table_sv2_week_teen_activity")
                                       ) #closes box
                                     ),#closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Lockdown",
                                           status = "danger",
                                           h5("Were you in lockdown this week? [Yes/No], NB next question only asked if this one is answered with 'Yes', NB this question was removed for the Tanzania rollout."),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv2_lockdown", height = "240"),
                                           shiny::tableOutput("table_sv2_lockdown")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Non-lockdown knowledge of teen activity",
                                           status = "danger",
                                           h5("How many days in a typical non-lockdown week do you know what your teen is doing and who they are with? [0-7],
                                                              NB this question is only asked if the parent said they had knowledge 7/7 days and then indicated that it was a lockdown week,
                                                              NB this question was removed for the Tanzania rollout."),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv2_reg_teen_activity", height = "240"),
                                           shiny::tableOutput("table_sv2_reg_teen_activity")
                                       ) #closes box
                                     ), #closes fluid row
                                     
                                     fluidRow(
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of sexual safety talk (last month)",
                                           status = "danger",  
                                           h5("How many days in the past month (30 days) did you talk with your teen about keeping safe from sexual violence online or offline? This could be when they are going out with friends, or talking about the websites they use. [0-30, steps of 5],
                                                           NB the question was changed for the Tanzania rollout to:
                                                          [In the past month, did you talk with your teen about keeping safe from sexual violence online or offline? This could be about going out with friends or about the websites and apps they use.]
                                                              How many days have you had a talk like this?"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv2_sex_talk", height = "240"),
                                           shiny::tableOutput("table_sv2_sex_talk")
                                       ), #closes box
                                       
                                       box(width = 6,
                                           collapsible = TRUE,
                                           solidHeader = TRUE,
                                           title = "Days of COVID safe teenager behaviour",
                                           status = "danger", 
                                           h5("How many days last week did your teenager stay safe through wearing a mask, keeping a distance from people and keeping away from crowded places? [0-7]"),
                                           style='width:100%;overflow-x: scroll;',
                                           plotlyOutput(outputId = "plot_sv2_covid_safe", height = "240"),
                                           shiny::tableOutput("table_sv2_covid_safe")
                                       ) #closes box
                                     )#closes fluid row
                            ) #closes midline tab panel
                )), # closes tabset panel and fifth tabItem 
        
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
        ) #closes sixth tab item
      ) # closes tabItems
    ) # closes dashboardBody
  )# closes dashboardPage
  
  # 4. Define Server -----------------------------------------------------------------------------
  server <- function(input, output) {
    
    # General Set Up ---------------------------------------------------
    #autoRefresh <- reactiveTimer(6 * 60 * 60 * 1000)
    
    observe({
      #autoRefresh()
      #source(here("Metabase Analysis Setup - run offline.R"))
      source(here("Metabase Analysis Setup.R"))
    })
    
    #SUMMARY STATS HEADER displays (same for all tabs)
    if (country != "Tanzania"){
      output$myvaluebox1 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(nrow(plhdata_org_clean), subtitle = "Enrolled", icon = icon("user"),
                                 color = "aqua")})
      output$myvaluebox2 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox( nrow(plhdata_org_clean %>% filter(Org == "Amathuba")), subtitle = "Amathuba", icon = icon("user"),
                                  color = "navy")})
      output$myvaluebox3 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox( nrow(plhdata_org_clean %>% filter(Org == "Dlalanathi")), subtitle = "Dlalanathi", icon = icon("user"),
                                  color = "navy")})
      output$myvaluebox4 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox( nrow(plhdata_org_clean %>% filter(Org == "Joy")), subtitle = "Joy", icon = icon("user"),
                                  color = "navy")})
      output$myvaluebox5 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox( nrow(plhdata_org_clean %>% filter(Org == "Nontobeko")), subtitle = "Nontobeko", icon = icon("user"),
                                  color = "navy")})
    }
    if (country == "all"){
      output$myvaluebox6 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox( nrow(plhdata_org_clean %>% filter(Org == "ICS")), subtitle = "ICS", icon = icon("user"),
                                  color = "navy")}) 
    }
    
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
    
    selected_data_dem <- reactive({
      if (country == "Tanzania"){
        if (study == "Pilot"){
          plhdata_checkgroup <- plhdata_org_clean %>% dplyr::filter(PilotSite %in% c(input$OrgDem))
        } else if (study == "Optimisation"){
          plhdata_checkgroup <- plhdata_org_clean %>%
            dplyr::filter(Support %in% c(input$opt_support)) %>%
            dplyr::filter(Skin %in% c(input$opt_skin)) %>%
            dplyr::filter(`Digital Literacy` %in% c(input$opt_diglit))
        } else {
          plhdata_checkgroup <- plhdata_org_clean
        }
      } else {
        plhdata_checkgroup <- plhdata_org_clean %>% dplyr::filter(Org %in% c((input$OrgDem)))
      }
      return(plhdata_checkgroup)
    })
    
    # Demographics ---------------------------------------------------
    summary_table_baseline <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_baseline_survey)
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    table_app_launch <- reactive({}) 
    plot_app_launch  <- reactive({
      first_app_plot <- ggplot(data = selected_data_dem(), aes(x = rp.contact.field.first_app_open)) +
        geom_freqpoly() +
        labs(x = "Date app first opened", y = "Count")
      first_app_plot
    }) 
    output$table_app_launch <- shiny::renderTable({(table_app_launch())}, striped = TRUE)
    output$plot_app_launch <- renderPlotly({plot_app_launch()})
    
    #Languages plot and table
    table_language <- reactive({
      summary_table_baseline()$` app language`
    }) 
    plot_language  <- reactive({
      summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field._app_language", replace = "rp.contact.field._")
    }) 
    output$table_language <- shiny::renderTable({(table_language())}, striped = TRUE)
    output$plot_language <- renderPlotly({plot_language()})
    
    #Workshop format plot and table
    table_ws_format <- reactive({
      summary_table_baseline()$`Workshop path`
    }) 
    plot_ws_format  <- reactive({
      # plhdata_org_clean$rp.contact.field.workshop_path is automatically combined from plhdata_org_clean$rp.contact.field.workshop_path and .do_workshops_together
      # we add in a "default" field from rp.contact.field.workshop_path_user_choice
      summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.workshop_path", replace = "rp.contact.field.")
    }) 
    output$table_ws_format <- shiny::renderTable({(table_ws_format())}, striped = TRUE, style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
    output$plot_ws_format <- renderPlotly({plot_ws_format()})
    
    #App version
    table_app_version <- reactive({
      summary_table_baseline()$`App version`
    }) 
    plot_app_version  <- reactive({
      plhdata_org_clean_1 <- selected_data_dem()
      ggplot(plhdata_org_clean_1, aes(x = app_version, fill = Org)) +
        geom_bar(position = "dodge") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "App version")
      #summary_plot(plhdata_org_clean, app_version)
    }) 
    output$table_app_version <- shiny::renderTable({(table_app_version())}, striped = TRUE, options = list(scrollX = TRUE))
    output$plot_app_version <- renderPlotly({plot_app_version()})
    
    #Parent gender plot and tabled
    table_parent_gender <- reactive({
      summary_table_baseline()$`User gender`
    }) 
    plot_parent_gender  <- reactive({
      summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.user_gender", replace = "rp.contact.field.", plot_type = "histogram")
    }) 
    output$table_parent_gender <- shiny::renderTable({(table_parent_gender())}, striped = TRUE)
    output$plot_parent_gender <- renderPlotly({plot_parent_gender()})
    
    #Parent age plot and table
    table_parent_age <- reactive({
      table_parent_age <- selected_data_dem() %>% summary_table(columns_to_summarise = rp.contact.field.user_age, summaries = "mean")
      table_parent_age <- table_parent_age %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))
      return(table_parent_age)
    }) 
    ##previously, but with new contact field summarise(`mean age`=(mean(rp.contact.field.user_gender, na.rm = TRUE)))
    plot_parent_age <- reactive({
      summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.user_age", replace = "rp.contact.field.", plot_type = "histogram")
    })
    output$table_parent_age <- shiny::renderTable({(table_parent_age())}, striped = TRUE)
    output$plot_parent_age <- renderPlotly({plot_parent_age()})
    
    #Adults in household plot and table
    table_household_adults <- reactive({
      summary_table_baseline()$`Household adults`
    }) 
    plot_household_adults  <- reactive({
      summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.household_adults", replace = "rp.contact.field.", plot_type = "histogram")
      #plhdata_org_clean_1 <- plhdata_org_clean %>% filter(Org %in% c((input$OrgDem)))
      #summary_plot(plhdata_org_clean_1, rp.contact.field.household_adults)
    })
    output$table_household_adults <- shiny::renderTable({( table_household_adults())}, striped = TRUE)
    output$plot_household_adults <- renderPlotly({plot_household_adults()})
    
    #Teens in household plot and table
    table_household_teens <- reactive({
      summary_table_baseline()$`Household teens`
    }) 
    plot_household_teens  <- reactive({
      summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.household_teens", replace = "rp.contact.field.", plot_type = "histogram")
    })
    output$table_household_teens <- shiny::renderTable({( table_household_teens())}, striped = TRUE)
    output$plot_household_teens <- renderPlotly({plot_household_teens()})
    
    #Children in household plot and table
    table_household_children <- reactive({
      summary_table_baseline()$`Household children`
    }) 
    plot_household_children  <- reactive({
      summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.household_children", replace = "rp.contact.field.", plot_type = "histogram")
    })
    output$table_household_children <- shiny::renderTable({( table_household_children())}, striped = TRUE)
    output$plot_household_children <- renderPlotly({plot_household_children()})
    
    #Babies in household plot and table
    table_household_babies <- reactive({
      summary_table_baseline()$`Household babies`
    }) 
    plot_household_babies  <- reactive({
      summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.household_babies", replace = "rp.contact.field.", plot_type = "histogram")
    })
    output$table_household_babies <- shiny::renderTable({( table_household_babies())}, striped = TRUE)
    output$plot_household_babies <- renderPlotly({plot_household_babies()})
    
    ### Former format with testdata
    # table_child_type <- reactive({
    #   testdata %>% 
    #     group_by(challenging_type) %>% summarise(n())
    # }) #closes child behaviour table
    # 
    # plot_child_type  <- reactive({
    #   ggplot(testdata, aes(x = challenging_type)) +
    #     geom_histogram(stat = "count") +
    #     viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
    #     labs(x = "child behaviour", y = "Count") +
    #     theme_classic() +
    #     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    # }) #closes child behaviour plot
    # 
    # output$table_child_type <- shiny::renderTable({(table_child_type())}, striped = TRUE)
    # output$plot_child_type <- renderPlotly({plot_child_type()})
    
    #SECOND Tab Workshop Engagement Data
    
    # Workshop Engagement ---------------------------------------------------
    summary_table_completion_level <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_completion_level,
                                                               replace = "rp.contact.field.w_",
                                                               replace_after = "_completion_level")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    #Table of averages
    table_ws_totals <- reactive({
      #mean average completion level per org
      # Percentage of users who completed a workshop out of those who started it
      # nrow(plhdata_org_clean %>% filter(rp.contact.field.w_money_completion_level == 100)) / nrow(plhdata_org_clean %>% filter(rp.contact.field.w_money_started == "true"))
      summary_mean_completion_level <- summary_table(data = selected_data_dem(),
                                                     columns_to_summarise = data_completion_level,
                                                     replace = "rp.contact.field.w_",
                                                     replace_after = "_completion_level",
                                                     summaries = "mean")
      summary_mean_completion_level <- summary_mean_completion_level %>%
        dplyr::filter(Org %in% unique(selected_data_dem()$Org))
      #dplyr::filter(Org %in% unique(selected_data_dem()$Org))
      return(summary_mean_completion_level)
    }) 
    
    plot_ws_totals  <- reactive({
      summary_mean_completion_level_long <- pivot_longer(table_ws_totals(), cols = !Org, names_to = "Workshop", values_to = "Value")
      ggplot(summary_mean_completion_level_long, aes(x = Workshop, y = Value, fill = Org)) + 
        geom_bar(stat = "identity", position = "dodge") +
        # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        scale_x_discrete(guide = guide_axis(angle = 90), limits = week_order) +
        viridis::scale_fill_viridis(discrete = TRUE) 
      #if needed: + flip axis coord_flip()
    }) 
    output$table_ws_totals <- shiny::renderTable({(table_ws_totals())}, striped = TRUE)
    output$plot_ws_totals <- renderPlotly({plot_ws_totals()})
    
    #Workshop tables with filter by Org
    table_w_self_care <- reactive({
      summary_table_completion_level()$`Self care`
    })
    plot_w_self_care <- reactive({ summary_plot(selected_data_dem(), "rp.contact.field.w_self_care_completion_level", replace = "rp.contact.field.w_") }) 
    output$table_w_self_care <- shiny::renderTable({(table_w_self_care())}, striped = TRUE)
    output$plot_w_self_care <- renderPlotly({plot_w_self_care()})
    
    #2
    table_w_1on1 <- reactive({
      summary_table_completion_level()$`1on1`
    })
    plot_w_1on1<- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.w_1on1_completion_level", replace = "rp.contact.field.w_")
    }) 
    output$table_w_1on1 <- shiny::renderTable({(table_w_1on1())}, striped = TRUE)
    output$plot_w_1on1 <- renderPlotly({plot_w_1on1()})
    #3
    table_w_praise <- reactive({
      summary_table_completion_level()$Praise
    })
    plot_w_praise<- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.w_praise_completion_level", replace = "rp.contact.field.w_")
    }) 
    output$table_w_praise <- shiny::renderTable({(table_w_praise())}, striped = TRUE)
    output$plot_w_praise <- renderPlotly({plot_w_praise()})
    #4
    table_w_instruct <- reactive({
      summary_table_completion_level()$`Instruct`
    })
    plot_w_instruct<- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.w_instruct_completion_level", replace = "rp.contact.field.w_")
    }) 
    output$table_w_instruct <- shiny::renderTable({(table_w_instruct())}, striped = TRUE)
    output$plot_w_instruct <- renderPlotly({plot_w_instruct()})
    #5
    table_w_stress <- reactive({
      summary_table_completion_level()$`Stress`
    })
    plot_w_stress<- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.w_stress_completion_level", replace = "rp.contact.field.w_")
    }) 
    output$table_w_stress <- shiny::renderTable({(table_w_stress())}, striped = TRUE)
    output$plot_w_stress <- renderPlotly({plot_w_stress()})
    #6
    table_w_money <- reactive({
      summary_table_completion_level()$`Money`
    })
    plot_w_money<- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.w_money_completion_level", replace = "rp.contact.field.w_")
    }) 
    output$table_w_money <- shiny::renderTable({(table_w_money())}, striped = TRUE)
    output$plot_w_money <- renderPlotly({plot_w_money()})
    #7
    table_w_rules <- reactive({
      summary_table_completion_level()$Rules
    })
    plot_w_rules<- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.w_rules_completion_level", replace = "rp.contact.field.w_")
    }) 
    output$table_w_rules <- shiny::renderTable({(table_w_rules())}, striped = TRUE)
    output$plot_w_rules <- renderPlotly({plot_w_rules()})
    #8
    table_w_consequence <- reactive({
      summary_table_completion_level()$`Consequence`
    })
    plot_w_consequence <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.w_consequence_completion_level", replace = "rp.contact.field.w_")
    }) 
    output$table_w_consequence <- shiny::renderTable({(table_w_consequence())}, striped = TRUE)
    output$plot_w_consequence <- renderPlotly({plot_w_consequence()})
    #9
    table_w_solve <- reactive({
      summary_table_completion_level()$`Solve`
    })
    plot_w_solve<- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.w_solve_completion_level", replace = "rp.contact.field.w_")
    }) 
    output$table_w_solve <- shiny::renderTable({(table_w_solve())}, striped = TRUE)
    output$plot_w_solve <- renderPlotly({plot_w_solve()})
    #10
    table_w_safe <- reactive({
      summary_table_completion_level()$`Safe`
    })
    plot_w_safe<- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.w_safe_completion_level", replace = "rp.contact.field.w_")
    }) 
    output$table_w_safe <- shiny::renderTable({(table_w_safe())}, striped = TRUE)
    output$plot_w_safe <- renderPlotly({plot_w_safe()})
    #11
    table_w_crisis <- reactive({
      summary_table_completion_level()$`Crisis`
    })
    plot_w_crisis<- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.w_crisis_completion_level", replace = "rp.contact.field.w_")
    }) 
    output$table_w_crisis <- shiny::renderTable({(table_w_crisis())}, striped = TRUE)
    output$plot_w_crisis <- renderPlotly({plot_w_crisis()})
    #12
    table_w_celebrate <- reactive({
      summary_table_completion_level()$`Celebrate`
    })
    plot_w_celebrate<- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.w_celebrate_completion_level", replace = "rp.contact.field.w_")
    }) 
    output$table_w_celebrate <- shiny::renderTable({(table_w_celebrate())}, striped = TRUE)
    output$plot_w_celebrate <- renderPlotly({plot_w_celebrate()})
    
    #Workshop engagement sub tab: additional insights -------------------------
    relative_perc_completed <- reactive({
      summary_table_completion_level <- summary_table_completion_level()
      for (i in 1:length(summary_table_completion_level)){
        if (!"100" %in% names(summary_table_completion_level[[i]])){
          summary_table_completion_level[[i]]$`100` <- 0
        }
      }
      
      relative_perc_completed <- imap(summary_table_completion_level, ~.x %>%
                                        mutate(started = Total - `0` - `NA`,
                                               perc_completed = `100`/started*100) %>%
                                        select(c(Org, started, perc_completed)))
      return(relative_perc_completed)
    })
    table_ws_started <- reactive({
      table_ws_started <- plyr::ldply(relative_perc_completed()) %>%
        pivot_wider(id_cols = Org, names_from = .id, values_from = started)
      return(table_ws_started)
    })
    plot_ws_started <- reactive({}) 
    output$table_ws_started <- shiny::renderTable({(table_ws_started())}, striped = TRUE)
    output$plot_ws_started <- renderPlotly({plot_ws_started()})
    
    table_ws_rel_completed <- reactive({
      table_perc_completed <- plyr::ldply(relative_perc_completed()) %>%
        pivot_wider(id_cols = Org, names_from = .id, values_from = perc_completed)
      return(table_perc_completed)
    })
    plot_ws_rel_completed <- reactive({}) 
    output$table_ws_rel_completed <- shiny::renderTable({(table_ws_rel_completed())}, striped = TRUE)
    output$plot_ws_rel_completed <- renderPlotly({plot_ws_rel_completed()})
    
    # Parent Points Tab -----------------------------------------------
    values <- reactiveValues(total = 0)
    
    observeEvent(input$OrgDem, {
      values$total <- values$total + 1
    })
    
    summary_table_habits_all <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_habit_parent_points_all,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "_completion_level")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    #Table of averages
    table_pp_totals <- reactive({
      summary_mean_habits <- summary_table(data = selected_data_dem(),
                                           columns_to_summarise = data_habit_parent_points_all,
                                           replace = "rp.contact.field.parent_point_count_",
                                           summaries = "mean")
      summary_mean_habits <- summary_mean_habits %>%
        dplyr::filter(Org %in% unique(selected_data_dem()$Org))
      return(summary_mean_habits)
    }) 
    plot_pp_totals  <- reactive({
      table_to_plot <- selected_data_dem() %>%
        group_by(Org) %>%
        summarise(across(data_habit_parent_points_all, sum, na.rm = TRUE))
      colnames(table_to_plot) <- naming_conventions(colnames(table_to_plot), "rp.contact.field.parent_point_count_")
      table_to_plot <- pivot_longer(table_to_plot, cols = !Org, names_to = "Parent Points", values_to = "Value")
      ggplot(table_to_plot, aes(x = Org, y = Value, fill = `Parent Points`)) +
        geom_bar(stat = "identity", position = "fill") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "Organisation", title = "Proportion of parent points given in each category within an organisation")
      
      # summary_mean_habits_long <- pivot_longer(summary_mean_habits, cols = !Org, names_to = "Parent Points", values_to = "Value")
      # ggplot(summary_mean_habits_long, aes(x = Org, y = Value, fill = `Parent Points`)) +
      #   geom_bar(stat = "identity", position = "stack") +
      #   viridis::scale_fill_viridis(discrete = TRUE)
    }) 
    output$table_pp_totals <- shiny::renderTable({(table_pp_totals())}, striped = TRUE, caption = "Mean parent points for each category split by organisation")
    output$plot_pp_totals <- renderPlotly({plot_pp_totals()})
    
    # pp_relax
    table_pp_relax <- reactive({
      summary_table_habits_all()$`Relax`}) 
    plot_pp_relax  <- reactive({
      # old method of just calling a static plot from analysis file - abandoned because does not allow filtering out sub-sets of data
      # summary_plot_habits_all$`Relax`
      # UPDATE: TODO: Work on summary_plot_habits_all()`Relax` to work here now. 
      summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_relax", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")
    }) 
    output$table_pp_relax <- shiny::renderTable({(table_pp_relax())}, striped = TRUE)
    output$plot_pp_relax <- renderPlotly({plot_pp_relax()})
    
    # pp_treat_yourself
    table_pp_treat_yourself <- reactive({summary_table_habits_all()$`Treat yourself`}) 
    plot_pp_treat_yourself  <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_treat_yourself", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")}) 
    output$table_pp_treat_yourself <- shiny::renderTable({(table_pp_treat_yourself())}, striped = TRUE)
    output$plot_pp_treat_yourself <- renderPlotly({plot_pp_treat_yourself()})
    
    # pp_praise_yourself
    table_pp_praise_yourself <- reactive({summary_table_habits_all()$`Praise yourself`}) 
    plot_pp_praise_yourself  <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")}) 
    output$table_pp_praise_yourself <- shiny::renderTable({(table_pp_praise_yourself())}, striped = TRUE)
    output$plot_pp_praise_yourself <- renderPlotly({plot_pp_praise_yourself()})
    
    # pp_spend_time
    table_pp_spend_time <- reactive({summary_table_habits_all()$`Spend time`}) 
    plot_pp_spend_time  <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")}) 
    output$table_pp_spend_time <- shiny::renderTable({(table_pp_spend_time())}, striped = TRUE)
    output$plot_pp_spend_time <- renderPlotly({plot_pp_spend_time()})
    
    # pp_praise_teen
    table_pp_praise_teen <- reactive({summary_table_habits_all()$`Praise teen`}) 
    plot_pp_praise_teen  <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")}) 
    output$table_pp_praise_teen <- shiny::renderTable({(table_pp_praise_teen())}, striped = TRUE)
    output$plot_pp_praise_teen <- renderPlotly({plot_pp_praise_teen()})
    
    # pp_instruct_positively
    table_pp_instruct_positively <- reactive({summary_table_habits_all()$`Instruct positively`}) 
    plot_pp_instruct_positively  <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")}) 
    output$table_pp_instruct_positively <- shiny::renderTable({(table_pp_instruct_positively())}, striped = TRUE)
    output$plot_pp_instruct_positively <- renderPlotly({plot_pp_instruct_positively()})
    
    # pp_breathe
    table_pp_breathe <- reactive({summary_table_habits_all()$`Breathe`}) 
    plot_pp_breathe  <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")}) 
    output$table_pp_breathe <- shiny::renderTable({(table_pp_breathe())}, striped = TRUE)
    output$plot_pp_breathe <- renderPlotly({plot_pp_breathe()})
    
    # pp_money
    table_pp_money <- reactive({summary_table_habits_all()$`Money`}) 
    plot_pp_money  <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_money", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")}) 
    output$table_pp_money <- shiny::renderTable({(table_pp_money())}, striped = TRUE)
    output$plot_pp_money <- renderPlotly({plot_pp_money()})
    
    # pp_consequence
    table_pp_consequence <- reactive({summary_table_habits_all()$`Consequence`}) 
    plot_pp_consequence  <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")}) 
    output$table_pp_consequence <- shiny::renderTable({(table_pp_consequence())}, striped = TRUE)
    output$plot_pp_consequence <- renderPlotly({plot_pp_consequence()})
    
    # pp_safe
    table_pp_safe <- reactive({summary_table_habits_all()$`Safe`}) 
    plot_pp_safe  <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_safe", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")}) 
    output$table_pp_safe <- shiny::renderTable({(table_pp_safe())}, striped = TRUE)
    output$plot_pp_safe <- renderPlotly({plot_pp_safe()})
    
    #Parent Point sub tab Relax points pp1
    table_pp_relax_ws_totals <- reactive({
      #Average relax parent points pp1
      #summary_relax_workshop <- multiple_table_output(columns_to_summarise = relax_workshop_vars, replace = "rp.contact.field.parent_point_count_relax_w_")
      summary_relax_workshop <- selected_data_dem() %>%
        group_by(Org) %>%
        summarise(across(all_of(relax_workshop_vars), mean, na.rm = TRUE))
      colnames(summary_relax_workshop) <- naming_conventions(colnames(summary_relax_workshop), "rp.contact.field.parent_point_count_relax_w_")
      if (country != "Tanzania"){
        summary_relax_workshop <- summary_relax_workshop %>%
          dplyr::filter(Org %in% c((input$OrgDem)))
      }
      return(summary_relax_workshop)
    })
    output$table_pp_relax_ws_totals <- shiny::renderTable({table_pp_relax_ws_totals()})
    plot_pp_relax_ws_totals <- reactive({
      # Make the table longer so that it is in a format for use in ggplot
      summary_relax_workshop_long <- table_pp_relax_ws_totals() %>%
        pivot_longer(cols = !Org) %>%
        mutate(name = fct_relevel(name, week_order))   # set the order of variables
      ggplot(summary_relax_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
        geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        geom_line() + labs(x = "Workshop week", y = "Number of points")
    })
    output$plot_pp_relax_ws_totals <- renderPlotly({plot_pp_relax_ws_totals()})
    
    summary_table_habits_self_care <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_habit_parent_points_w_self_care,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "_w_self_care")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    summary_table_habits_1on1 <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_habit_parent_points_w_1on1,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "w_1on1")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    summary_table_habits_praise <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_habit_parent_points_w_praise,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "w_praise")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    summary_table_habits_instruct <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_habit_parent_points_w_instruct,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "w_instruct")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    summary_table_habits_stress <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_habit_parent_points_w_stress,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "w_stress")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    summary_table_habits_money <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_habit_parent_points_w_money,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "w_money")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    summary_table_habits_rules <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_habit_parent_points_w_rules,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "w_rules")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    summary_table_habits_consequence <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_habit_parent_points_w_consequence,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "w_consequence")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    summary_table_habits_solve <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_habit_parent_points_w_solve,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "w_solve")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    summary_table_habits_safe <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_habit_parent_points_w_safe,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "w_safe")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    summary_table_habits_crisis <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_habit_parent_points_w_crisis,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "w_crisis")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    summary_table_habits_celebrate <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_habit_parent_points_w_celebrate,
                                                               replace = "rp.contact.field.parent_point_count_",
                                                               replace_after = "w_celebrate")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    table_pp_relax_w_self_care <- reactive({
      summary_table_habits_self_care()$`Relax`
    })
    plot_pp_relax_w_self_care <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_relax_w_self_care", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")})
    output$table_pp_relax_w_self_care <- shiny::renderTable({(table_pp_relax_w_self_care())}, striped = TRUE)
    output$plot_pp_relax_w_self_care <- renderPlotly({plot_pp_relax_w_self_care()})
    
    table_pp_relax_w_1on1 <- reactive({summary_table_habits_1on1()$`Relax`})
    plot_pp_relax_w_1on1 <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_relax_w_1on1", replace = "rp.contact.field.parent_point_count_relax_w_", plot_type = "boxplot")})
    output$table_pp_relax_w_1on1 <- shiny::renderTable({(table_pp_relax_w_1on1())}, striped = TRUE)
    output$plot_pp_relax_w_1on1 <- renderPlotly({plot_pp_relax_w_1on1()})
    
    table_pp_relax_w_praise <- reactive({summary_table_habits_praise()$`Relax`})
    plot_pp_relax_w_praise <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_relax_w_praise", replace = "rp.contact.field.parent_point_count_relax_w_", plot_type = "boxplot")})
    output$table_pp_relax_w_praise <- shiny::renderTable({(table_pp_relax_w_praise())}, striped = TRUE)
    output$plot_pp_relax_w_praise <- renderPlotly({plot_pp_relax_w_praise()})
    
    table_pp_relax_w_instruct <- reactive({summary_table_habits_instruct()$`Relax`})
    plot_pp_relax_w_instruct <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_relax_w_instruct", replace = "rp.contact.field.parent_point_count_relax_w_", plot_type = "boxplot")})
    output$table_pp_relax_w_instruct <- shiny::renderTable({(table_pp_relax_w_instruct())}, striped = TRUE)
    output$plot_pp_relax_w_instruct <- renderPlotly({plot_pp_relax_w_instruct()})
    
    table_pp_relax_w_stress <- reactive({summary_table_habits_stress()$`Relax`%>% filter(Org %in% c((input$OrgDem)))})
    plot_pp_relax_w_stress <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_relax_w_stress", replace = "rp.contact.field.parent_point_count_relax_w_", plot_type = "boxplot")})
    output$table_pp_relax_w_stress <- shiny::renderTable({(table_pp_relax_w_stress())}, striped = TRUE)
    output$plot_pp_relax_w_stress <- renderPlotly({plot_pp_relax_w_stress()})
    
    table_pp_relax_w_money <- reactive({summary_table_habits_money()$`Relax`})
    plot_pp_relax_w_money <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_relax_w_money", replace = "rp.contact.field.parent_point_count_relax_w_", plot_type = "boxplot")})
    output$table_pp_relax_w_money <- shiny::renderTable({(table_pp_relax_w_money())}, striped = TRUE)
    output$plot_pp_relax_w_money <- renderPlotly({plot_pp_relax_w_money()})
    
    table_pp_relax_w_rules <- reactive({summary_table_habits_rules()$`Relax`})
    plot_pp_relax_w_rules <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_relax_w_rules", replace = "rp.contact.field.parent_point_count_relax_w_", plot_type = "boxplot")})
    output$table_pp_relax_w_rules <- shiny::renderTable({(table_pp_relax_w_rules())}, striped = TRUE)
    output$plot_pp_relax_w_rules <- renderPlotly({plot_pp_relax_w_rules()})
    
    table_pp_relax_w_consequence <- reactive({summary_table_habits_consequence()$`Relax`})
    plot_pp_relax_w_consequence <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_relax_w_consequence", replace = "rp.contact.field.parent_point_count_relax_w_", plot_type = "boxplot")})
    output$table_pp_relax_w_consequence <- shiny::renderTable({(table_pp_relax_w_consequence())}, striped = TRUE)
    output$plot_pp_relax_w_consequence <- renderPlotly({plot_pp_relax_w_consequence()})
    
    table_pp_relax_w_solve <- reactive({summary_table_habits_solve()$`Relax`})
    plot_pp_relax_w_solve <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_relax_w_solve", replace = "rp.contact.field.parent_point_count_relax_w_", plot_type = "boxplot")})
    output$table_pp_relax_w_solve <- shiny::renderTable({(table_pp_relax_w_solve())}, striped = TRUE)
    output$plot_pp_relax_w_solve <- renderPlotly({plot_pp_relax_w_solve()})
    
    table_pp_relax_w_safe <- reactive({summary_table_habits_safe()$`Relax`})
    plot_pp_relax_w_safe <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_relax_w_safe", replace = "rp.contact.field.parent_point_count_relax_w_", plot_type = "boxplot")})
    output$table_pp_relax_w_safe <- shiny::renderTable({(table_pp_relax_w_safe())}, striped = TRUE)
    output$plot_pp_relax_w_safe <- renderPlotly({plot_pp_relax_w_safe()})
    
    table_pp_relax_w_crisis <- reactive({summary_table_habits_crisis()$`Relax`})
    plot_pp_relax_w_crisis <- reactive({
      summary_plot(data = selected_data_dem(),
                   columns_to_summarise = "rp.contact.field.parent_point_count_relax_w_crisis",
                   replace = "rp.contact.field.parent_point_count_relax_w_",
                   plot_type = "boxplot")})
    output$table_pp_relax_w_crisis <- shiny::renderTable({(table_pp_relax_w_crisis())}, striped = TRUE)
    output$plot_pp_relax_w_crisis <- renderPlotly({plot_pp_relax_w_crisis()})
    
    table_pp_relax_w_celebrate <- reactive({summary_table_habits_celebrate()$`Relax`})
    plot_pp_relax_w_celebrate <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_relax_w_celebrate", replace = "rp.contact.field.parent_point_count_relax_w_", plot_type = "boxplot")})
    output$table_pp_relax_w_celebrate <- shiny::renderTable({(table_pp_relax_w_celebrate())}, striped = TRUE)
    output$plot_pp_relax_w_celebrate <- renderPlotly({plot_pp_relax_w_celebrate()})
    
    #Parent Point sub tab Treat Yourself points pp2
    table_pp_treat_yourself_ws_totals <- reactive({
      #Average treat_yourself parent points pp2
      summary_treat_yourself_workshop <- selected_data_dem() %>%
        group_by(Org, .drop = TRUE) %>%
        summarise(across(treat_yourself_workshop_vars, mean, na.rm = TRUE))
      colnames(summary_treat_yourself_workshop) <- naming_conventions(colnames(summary_treat_yourself_workshop), "rp.contact.field.parent_point_count_treat_yourself_w_")
      if (country != "Tanzania"){
        summary_treat_yourself_workshop <- summary_treat_yourself_workshop %>%
          dplyr::filter(Org %in% c((input$OrgDem)))
      }
      return(summary_treat_yourself_workshop)
    }) 
    plot_pp_treat_yourself_ws_totals  <- reactive({
      summary_treat_yourself_workshop_long <- table_pp_treat_yourself_ws_totals() %>%
        pivot_longer(cols = !Org) %>%
        mutate(name = fct_relevel(name, week_order))   # set the order of variables
      ggplot(summary_treat_yourself_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
        geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        geom_line() + labs(x = "Workshop week", y = "Number of points")
    }) 
    output$table_pp_treat_yourself_ws_totals <- shiny::renderTable({(table_pp_treat_yourself_ws_totals())}, striped = TRUE)
    output$plot_pp_treat_yourself_ws_totals <- renderPlotly({plot_pp_treat_yourself_ws_totals()})
    
    table_pp_treat_yourself_w_self_care <- reactive({summary_table_habits_self_care()$`Treat yourself`})
    plot_pp_treat_yourself_w_self_care <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_treat_yourself_w_self_care", replace = "rp.contact.field.parent_point_count_treat_yourself_w", plot_type = "boxplot")})
    output$table_pp_treat_yourself_w_self_care <- shiny::renderTable({(table_pp_treat_yourself_w_self_care())}, striped = TRUE)
    output$plot_pp_treat_yourself_w_self_care <- renderPlotly({plot_pp_treat_yourself_w_self_care()})
    
    table_pp_treat_yourself_w_1on1 <- reactive({summary_table_habits_1on1()$`Treat yourself`})
    plot_pp_treat_yourself_w_1on1 <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_treat_yourself_w_1on1", replace = "rp.contact.field.parent_point_count_treat_yourself_w", plot_type = "boxplot")})
    output$table_pp_treat_yourself_w_1on1 <- shiny::renderTable({(table_pp_treat_yourself_w_1on1())}, striped = TRUE)
    output$plot_pp_treat_yourself_w_1on1 <- renderPlotly({plot_pp_treat_yourself_w_1on1()})
    
    table_pp_treat_yourself_w_praise <- reactive({summary_table_habits_praise()$`Treat yourself `})
    plot_pp_treat_yourself_w_praise <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_treat_yourself_w_praise", replace = "rp.contact.field.parent_point_count_treat_yourself_w", plot_type = "boxplot")})
    output$table_pp_treat_yourself_w_praise <- shiny::renderTable({(table_pp_treat_yourself_w_praise())}, striped = TRUE)
    output$plot_pp_treat_yourself_w_praise <- renderPlotly({plot_pp_treat_yourself_w_praise()})
    
    table_pp_treat_yourself_w_instruct <- reactive({summary_table_habits_instruct()$`Treat yourself `})
    plot_pp_treat_yourself_w_instruct <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_treat_yourself_w_instruct", replace = "rp.contact.field.parent_point_count_treat_yourself_w", plot_type = "boxplot")})
    output$table_pp_treat_yourself_w_instruct <- shiny::renderTable({(table_pp_treat_yourself_w_instruct())}, striped = TRUE)
    output$plot_pp_treat_yourself_w_instruct <- renderPlotly({plot_pp_treat_yourself_w_instruct()})
    
    table_pp_treat_yourself_w_stress <- reactive({summary_table_habits_stress()$`Treat yourself `})
    plot_pp_treat_yourself_w_stress <- reactive({})
    output$table_pp_treat_yourself_w_stress <- shiny::renderTable({(table_pp_treat_yourself_w_stress())}, striped = TRUE)
    output$plot_pp_treat_yourself_w_stress <- renderPlotly({plot_pp_treat_yourself_w_stress()})
    
    table_pp_treat_yourself_w_money <- reactive({summary_table_habits_money()$`Treat yourself `})
    plot_pp_treat_yourself_w_money <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_treat_yourself_w_money", replace = "rp.contact.field.parent_point_count_treat_yourself_w", plot_type = "boxplot")})
    output$table_pp_treat_yourself_w_money <- shiny::renderTable({(table_pp_treat_yourself_w_money())}, striped = TRUE)
    output$plot_pp_treat_yourself_w_money <- renderPlotly({plot_pp_treat_yourself_w_money()})
    
    #ERROR
    table_pp_treat_yourself_w_rules <- reactive({summary_table_habits_rules()$`Treat yourself `})
    plot_pp_treat_yourself_w_rules <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_treat_yourself_w_rules", replace = "rp.contact.field.parent_point_count_treat_yourself_w", plot_type = "boxplot")})
    output$table_pp_treat_yourself_w_rules <- shiny::renderTable({(table_pp_treat_yourself_w_rules())}, striped = TRUE)
    output$plot_pp_treat_yourself_w_rules <- renderPlotly({plot_pp_treat_yourself_w_rules()})
    
    table_pp_treat_yourself_w_consequence <- reactive({summary_table_habits_consequence()$`Treat yourself`})
    plot_pp_treat_yourself_w_consequence <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_treat_yourself_w_consequence", replace = "rp.contact.field.parent_point_count_treat_yourself_w", plot_type = "boxplot")})
    output$table_pp_treat_yourself_w_consequence <- shiny::renderTable({(table_pp_treat_yourself_w_consequence())}, striped = TRUE)
    output$plot_pp_treat_yourself_w_consequence <- renderPlotly({plot_pp_treat_yourself_w_consequence()})
    
    table_pp_treat_yourself_w_solve <- reactive({summary_table_habits_solve()$`Treat yourself`})
    plot_pp_treat_yourself_w_solve <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_treat_yourself_w_solve", replace = "rp.contact.field.parent_point_count_treat_yourself_w", plot_type = "boxplot")})
    output$table_pp_treat_yourself_w_solve <- shiny::renderTable({(table_pp_treat_yourself_w_solve())}, striped = TRUE)
    output$plot_pp_treat_yourself_w_solve <- renderPlotly({plot_pp_treat_yourself_w_solve()})
    
    table_pp_treat_yourself_w_safe <- reactive({summary_table_habits_safe()$`Treat yourself`})
    plot_pp_treat_yourself_w_safe <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_treat_yourself_w_safe", replace = "rp.contact.field.parent_point_count_treat_yourself_w", plot_type = "boxplot")})
    output$table_pp_treat_yourself_w_safe <- shiny::renderTable({(table_pp_treat_yourself_w_safe())}, striped = TRUE)
    output$plot_pp_treat_yourself_w_safe <- renderPlotly({plot_pp_treat_yourself_w_safe()})
    
    table_pp_treat_yourself_w_crisis <- reactive({summary_table_habits_crisis()$`Treat yourself`})
    plot_pp_treat_yourself_w_crisis <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_treat_yourself_w_crisis", replace = "rp.contact.field.parent_point_count_treat_yourself_w", plot_type = "boxplot")})
    output$table_pp_treat_yourself_w_crisis <- shiny::renderTable({(table_pp_treat_yourself_w_crisis())}, striped = TRUE)
    output$plot_pp_treat_yourself_w_crisis <- renderPlotly({plot_pp_treat_yourself_w_crisis()})
    
    table_pp_treat_yourself_w_celebrate <- reactive({summary_table_habits_celebrate()$`Treat yourself`})
    plot_pp_treat_yourself_w_celebrate <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_treat_yourself_w_celebrate", replace = "rp.contact.field.parent_point_count_treat_yourself_w", plot_type = "boxplot")})
    output$table_pp_treat_yourself_w_celebrate <- shiny::renderTable({(table_pp_treat_yourself_w_celebrate())}, striped = TRUE)
    output$plot_pp_treat_yourself_w_celebrate <- renderPlotly({plot_pp_treat_yourself_w_celebrate()})
    
    # Parent Point sub tab Praise Yourself points pp3
    table_pp_praise_yourself_ws_totals <- reactive({
      #Average praise_yourself parent points pp3
      summary_praise_yourself_workshop <- selected_data_dem() %>%
        group_by(Org, .drop = TRUE) %>%
        summarise(across(praise_yourself_workshop_vars, mean, na.rm = TRUE))
      colnames(summary_praise_yourself_workshop) <- naming_conventions(colnames(summary_praise_yourself_workshop), "rp.contact.field.parent_point_count_praise_yourself_w_")
      if (country != "Tanzania"){
        summary_praise_yourself_workshop <- summary_praise_yourself_workshop %>%
          dplyr::filter(Org %in% c((input$OrgDem)))
      }
      summary_praise_yourself_workshop
    })
    plot_pp_praise_yourself_ws_totals <- reactive({
      # Make the table longer so that it is in a format for use in ggplot
      summary_praise_yourself_workshop_long <- table_pp_praise_yourself_ws_totals() %>%
        pivot_longer(cols = !Org) %>%
        mutate(name = fct_relevel(name, week_order))   # set the order of variables
      ggplot(summary_praise_yourself_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
        geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        geom_line() + labs(x = "Workshop week", y = "Number of points")
    })
    output$table_pp_praise_yourself_ws_totals <- shiny::renderTable({table_pp_praise_yourself_ws_totals()})
    output$plot_pp_praise_yourself_ws_totals <- renderPlotly({plot_pp_praise_yourself_ws_totals()})
    
    table_pp_praise_yourself_w_self_care <- reactive({summary_table_habits_self_care()$`Praise yourself`})
    plot_pp_praise_yourself_w_self_care <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself_w_self_care", replace = "rp.contact.field.parent_point_count_praise_yourself_w_", plot_type = "boxplot")})
    output$table_pp_praise_yourself_w_self_care <- shiny::renderTable({(table_pp_praise_yourself_w_self_care())}, striped = TRUE)
    output$plot_pp_praise_yourself_w_self_care <- renderPlotly({plot_pp_praise_yourself_w_self_care()})
    
    table_pp_praise_yourself_w_1on1 <- reactive({summary_table_habits_1on1()$`Praise yourself`})
    plot_pp_praise_yourself_w_1on1 <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself_w_1on1", replace = "rp.contact.field.parent_point_count_praise_yourself_w_", plot_type = "boxplot")})
    output$table_pp_praise_yourself_w_1on1 <- shiny::renderTable({(table_pp_praise_yourself_w_1on1())}, striped = TRUE)
    output$plot_pp_praise_yourself_w_1on1 <- renderPlotly({plot_pp_praise_yourself_w_1on1()})
    
    table_pp_praise_yourself_w_praise <- reactive({summary_table_habits_praise()$`Praise yourself `})
    plot_pp_praise_yourself_w_praise <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself_w_praise", replace = "rp.contact.field.parent_point_count_praise_yourself_w_", plot_type = "boxplot")})
    output$table_pp_praise_yourself_w_praise <- shiny::renderTable({(table_pp_praise_yourself_w_praise())}, striped = TRUE)
    output$plot_pp_praise_yourself_w_praise <- renderPlotly({plot_pp_praise_yourself_w_praise()})
    
    table_pp_praise_yourself_w_instruct <- reactive({summary_table_habits_instruct()$`Praise yourself `})
    plot_pp_praise_yourself_w_instruct <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself_w_instruct", replace = "rp.contact.field.parent_point_count_praise_yourself_w_", plot_type = "boxplot")})
    output$table_pp_praise_yourself_w_instruct <- shiny::renderTable({(table_pp_praise_yourself_w_instruct())}, striped = TRUE)
    output$plot_pp_praise_yourself_w_instruct <- renderPlotly({plot_pp_praise_yourself_w_instruct()})
    
    table_pp_praise_yourself_w_stress <- reactive({summary_table_habits_stress()$`Praise yourself `})
    plot_pp_praise_yourself_w_stress <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself_w_stress", replace = "rp.contact.field.parent_point_count_praise_yourself_w_", plot_type = "boxplot")})
    output$table_pp_praise_yourself_w_stress <- shiny::renderTable({(table_pp_praise_yourself_w_stress())}, striped = TRUE)
    output$plot_pp_praise_yourself_w_stress <- renderPlotly({plot_pp_praise_yourself_w_stress()})
    
    table_pp_praise_yourself_w_money <- reactive({summary_table_habits_money()$`Praise yourself `})
    plot_pp_praise_yourself_w_money <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself_w_money", replace = "rp.contact.field.parent_point_count_praise_yourself_w_", plot_type = "boxplot")})
    output$table_pp_praise_yourself_w_money <- shiny::renderTable({(table_pp_praise_yourself_w_money())}, striped = TRUE)
    output$plot_pp_praise_yourself_w_money <- renderPlotly({plot_pp_praise_yourself_w_money()})
    
    table_pp_praise_yourself_w_rules <- reactive({summary_table_habits_rules()$`Praise yourself `})
    plot_pp_praise_yourself_w_rules <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself_w_rules", replace = "rp.contact.field.parent_point_count_praise_yourself_w_", plot_type = "boxplot")})
    output$table_pp_praise_yourself_w_rules <- shiny::renderTable({(table_pp_praise_yourself_w_rules())}, striped = TRUE)
    output$plot_pp_praise_yourself_w_rules <- renderPlotly({plot_pp_praise_yourself_w_rules()})
    
    table_pp_praise_yourself_w_consequence <- reactive({summary_table_habits_consequence()$`Praise yourself`})
    plot_pp_praise_yourself_w_consequence <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself_w_consequence", replace = "rp.contact.field.parent_point_count_praise_yourself_w_", plot_type = "boxplot")})
    output$table_pp_praise_yourself_w_consequence <- shiny::renderTable({(table_pp_praise_yourself_w_consequence())}, striped = TRUE)
    output$plot_pp_praise_yourself_w_consequence <- renderPlotly({plot_pp_praise_yourself_w_consequence()})
    
    table_pp_praise_yourself_w_solve <- reactive({summary_table_habits_solve()$`Praise yourself`})
    plot_pp_praise_yourself_w_solve <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself_w_solve", replace = "rp.contact.field.parent_point_count_praise_yourself_w_", plot_type = "boxplot")})
    output$table_pp_praise_yourself_w_solve <- shiny::renderTable({(table_pp_praise_yourself_w_solve())}, striped = TRUE)
    output$plot_pp_praise_yourself_w_solve <- renderPlotly({plot_pp_praise_yourself_w_solve()})
    
    table_pp_praise_yourself_w_safe <- reactive({summary_table_habits_safe()$`Praise yourself`})
    plot_pp_praise_yourself_w_safe <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself_w_safe", replace = "rp.contact.field.parent_point_count_praise_yourself_w_", plot_type = "boxplot")})
    output$table_pp_praise_yourself_w_safe <- shiny::renderTable({(table_pp_praise_yourself_w_safe())}, striped = TRUE)
    output$plot_pp_praise_yourself_w_safe <- renderPlotly({plot_pp_praise_yourself_w_safe()})
    
    table_pp_praise_yourself_w_crisis <- reactive({summary_table_habits_crisis()$`Praise yourself`})
    plot_pp_praise_yourself_w_crisis <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself_w_crisis", replace = "rp.contact.field.parent_point_count_praise_yourself_w_", plot_type = "boxplot")})
    output$table_pp_praise_yourself_w_crisis <- shiny::renderTable({(table_pp_praise_yourself_w_crisis())}, striped = TRUE)
    output$plot_pp_praise_yourself_w_crisis <- renderPlotly({plot_pp_praise_yourself_w_crisis()})
    
    table_pp_praise_yourself_w_celebrate <- reactive({summary_table_habits_celebrate()$`Praise yourself`})
    plot_pp_praise_yourself_w_celebrate <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself_w_celebrate", replace = "rp.contact.field.parent_point_count_praise_yourself_w_", plot_type = "boxplot")})
    output$table_pp_praise_yourself_w_celebrate <- shiny::renderTable({(table_pp_praise_yourself_w_celebrate())}, striped = TRUE)
    output$plot_pp_praise_yourself_w_celebrate <- renderPlotly({plot_pp_praise_yourself_w_celebrate()})
    
    # Parent Point sub tab Spend Time points pp4
    table_pp_spend_time_ws_totals <- reactive({
      #Average spend_time parent points pp4
      summary_spend_time_workshop <- selected_data_dem() %>%
        group_by(Org) %>%
        summarise(across(spend_time_workshop_vars, mean, na.rm = TRUE))
      colnames(summary_spend_time_workshop) <- naming_conventions(colnames(summary_spend_time_workshop), "rp.contact.field.parent_point_count_spend_time_w_")
      if (country != "Tanzania"){
        summary_spend_time_workshop <- summary_spend_time_workshop %>%
          dplyr::filter(Org %in% c((input$OrgDem)))
      }
      summary_spend_time_workshop
    })
    plot_pp_spend_time_ws_totals <- reactive({
      # Make the table longer so that it is in a format for use in ggplot
      summary_spend_time_workshop_long <- table_pp_spend_time_ws_totals() %>%
        pivot_longer(cols = !Org) %>%
        mutate(name = fct_relevel(name, week_order))   # set the order of variables
      ggplot(summary_spend_time_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
        geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        geom_line() + labs(x = "Workshop week", y = "Number of points")})
    output$table_pp_spend_time_ws_totals <- shiny::renderTable({table_pp_spend_time_ws_totals()})
    output$plot_pp_spend_time_ws_totals <- renderPlotly({plot_pp_spend_time_ws_totals()})
    
    table_pp_spend_time_w_self_care <- reactive({summary_table_habits_self_care()$`Spend time`})
    plot_pp_spend_time_w_self_care <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time_w_self_care", replace = "rp.contact.field.parent_point_count_spend_time_w_", plot_type = "boxplot")})
    output$table_pp_spend_time_w_self_care <- shiny::renderTable({(table_pp_spend_time_w_self_care())}, striped = TRUE)
    output$plot_pp_spend_time_w_self_care <- renderPlotly({plot_pp_spend_time_w_self_care()})
    
    table_pp_spend_time_w_1on1 <- reactive({summary_table_habits_1on1()$`Spend time`})
    plot_pp_spend_time_w_1on1 <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time_w_1on1", replace = "rp.contact.field.parent_point_count_spend_time_w_", plot_type = "boxplot")})
    output$table_pp_spend_time_w_1on1 <- shiny::renderTable({(table_pp_spend_time_w_1on1())}, striped = TRUE)
    output$plot_pp_spend_time_w_1on1 <- renderPlotly({plot_pp_spend_time_w_1on1()})
    
    table_pp_spend_time_w_praise <- reactive({summary_table_habits_praise()$`Spend time `})
    plot_pp_spend_time_w_praise <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time_w_praise", replace = "rp.contact.field.parent_point_count_spend_time_w_", plot_type = "boxplot")})
    output$table_pp_spend_time_w_praise <- shiny::renderTable({(table_pp_spend_time_w_praise())}, striped = TRUE)
    output$plot_pp_spend_time_w_praise <- renderPlotly({plot_pp_spend_time_w_praise()})
    
    table_pp_spend_time_w_instruct <- reactive({summary_table_habits_instruct()$`Spend time `})
    plot_pp_spend_time_w_instruct <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time_w_instruct", replace = "rp.contact.field.parent_point_count_spend_time_w_", plot_type = "boxplot")})
    output$table_pp_spend_time_w_instruct <- shiny::renderTable({(table_pp_spend_time_w_instruct())}, striped = TRUE)
    output$plot_pp_spend_time_w_instruct <- renderPlotly({plot_pp_spend_time_w_instruct()})
    
    table_pp_spend_time_w_stress <- reactive({summary_table_habits_stress()$`Spend time `})
    plot_pp_spend_time_w_stress <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time_w_stress", replace = "rp.contact.field.parent_point_count_spend_time_w_", plot_type = "boxplot")})
    output$table_pp_spend_time_w_stress <- shiny::renderTable({(table_pp_spend_time_w_stress())}, striped = TRUE)
    output$plot_pp_spend_time_w_stress <- renderPlotly({plot_pp_spend_time_w_stress()})
    
    table_pp_spend_time_w_money <- reactive({summary_table_habits_money()$`Spend time `})
    plot_pp_spend_time_w_money <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time_w_money", replace = "rp.contact.field.parent_point_count_spend_time_w_", plot_type = "boxplot")})
    output$table_pp_spend_time_w_money <- shiny::renderTable({(table_pp_spend_time_w_money())}, striped = TRUE)
    output$plot_pp_spend_time_w_money <- renderPlotly({plot_pp_spend_time_w_money()})
    
    table_pp_spend_time_w_rules <- reactive({summary_table_habits_rules()$`Spend time `})
    plot_pp_spend_time_w_rules <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time_w_rules", replace = "rp.contact.field.parent_point_count_spend_time_w_", plot_type = "boxplot")})
    output$table_pp_spend_time_w_rules <- shiny::renderTable({(table_pp_spend_time_w_rules())}, striped = TRUE)
    output$plot_pp_spend_time_w_rules <- renderPlotly({plot_pp_spend_time_w_rules()})
    
    table_pp_spend_time_w_consequence <- reactive({summary_table_habits_consequence()$`Spend time`})
    plot_pp_spend_time_w_consequence <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time_w_consequence", replace = "rp.contact.field.parent_point_count_spend_time_w_", plot_type = "boxplot")})
    output$table_pp_spend_time_w_consequence <- shiny::renderTable({(table_pp_spend_time_w_consequence())}, striped = TRUE)
    output$plot_pp_spend_time_w_consequence <- renderPlotly({plot_pp_spend_time_w_consequence()})
    
    table_pp_spend_time_w_solve <- reactive({summary_table_habits_solve()$`Spend time`})
    plot_pp_spend_time_w_solve <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time_w_solve", replace = "rp.contact.field.parent_point_count_spend_time_w_", plot_type = "boxplot")})
    output$table_pp_spend_time_w_solve <- shiny::renderTable({(table_pp_spend_time_w_solve())}, striped = TRUE)
    output$plot_pp_spend_time_w_solve <- renderPlotly({plot_pp_spend_time_w_solve()})
    
    table_pp_spend_time_w_safe <- reactive({summary_table_habits_safe()$`Spend time`})
    plot_pp_spend_time_w_safe <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time_w_safe", replace = "rp.contact.field.parent_point_count_spend_time_w_", plot_type = "boxplot")})
    output$table_pp_spend_time_w_safe <- shiny::renderTable({(table_pp_spend_time_w_safe())}, striped = TRUE)
    output$plot_pp_spend_time_w_safe <- renderPlotly({plot_pp_spend_time_w_safe()})
    
    table_pp_spend_time_w_crisis <- reactive({summary_table_habits_crisis()$`Spend time`})
    plot_pp_spend_time_w_crisis <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time_w_crisis", replace = "rp.contact.field.parent_point_count_spend_time_w_", plot_type = "boxplot")})
    output$table_pp_spend_time_w_crisis <- shiny::renderTable({(table_pp_spend_time_w_crisis())}, striped = TRUE)
    output$plot_pp_spend_time_w_crisis <- renderPlotly({plot_pp_spend_time_w_crisis()})
    
    table_pp_spend_time_w_celebrate <- reactive({summary_table_habits_celebrate()$`Spend time`})
    plot_pp_spend_time_w_celebrate <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time_w_celebrate", replace = "rp.contact.field.parent_point_count_spend_time_w_", plot_type = "boxplot")})
    output$table_pp_spend_time_w_celebrate <- shiny::renderTable({(table_pp_spend_time_w_celebrate())}, striped = TRUE)
    output$plot_pp_spend_time_w_celebrate <- renderPlotly({plot_pp_spend_time_w_celebrate()})
    
    # Parent Point sub tab Praise Teen points pp5
    table_pp_praise_teen_ws_totals <- reactive({
      #Average praise_teen parent points pp5
      summary_praise_teen_workshop <- selected_data_dem() %>%
        group_by(Org, .drop = TRUE) %>%
        summarise(across(praise_teen_workshop_vars, mean, na.rm = TRUE))
      colnames(summary_praise_teen_workshop) <- naming_conventions(colnames(summary_praise_teen_workshop), "rp.contact.field.parent_point_count_praise_teen_w_")
      if (country != "Tanzania"){
        summary_praise_teen_workshop <- summary_praise_teen_workshop %>%
          dplyr::filter(Org %in% c((input$OrgDem)))
      }
      summary_praise_teen_workshop
    })
    plot_pp_praise_teen_ws_totals <- reactive({
      # Make the table longer so that it is in a format for use in ggplot
      summary_praise_teen_workshop_long <- table_pp_praise_teen_ws_totals() %>%
        pivot_longer(cols = !Org) %>%
        mutate(name = fct_relevel(name, week_order))   # set the order of variables
      # summary_praise_teen_workshop_long
      ggplot(summary_praise_teen_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
        geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        geom_line() + labs(x = "Workshop week", y = "Number of points")})
    output$table_pp_praise_teen_ws_totals <- shiny::renderTable({table_pp_praise_teen_ws_totals()})
    output$plot_pp_praise_teen_ws_totals <- renderPlotly({plot_pp_praise_teen_ws_totals()})
    
    table_pp_praise_teen_w_self_care <- reactive({summary_table_habits_self_care()$`Praise teen`})
    plot_pp_praise_teen_w_self_care <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen_w_self_care", replace = "rp.contact.field.parent_point_count_praise_teen_w_", plot_type = "boxplot")})
    output$table_pp_praise_teen_w_self_care <- shiny::renderTable({(table_pp_praise_teen_w_self_care())}, striped = TRUE)
    output$plot_pp_praise_teen_w_self_care <- renderPlotly({plot_pp_praise_teen_w_self_care()})
    
    table_pp_praise_teen_w_1on1 <- reactive({summary_table_habits_1on1()$`Praise teen`})
    plot_pp_praise_teen_w_1on1 <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen_w_1on1", replace = "rp.contact.field.parent_point_count_praise_teen_w_", plot_type = "boxplot")})
    output$table_pp_praise_teen_w_1on1 <- shiny::renderTable({(table_pp_praise_teen_w_1on1())}, striped = TRUE)
    output$plot_pp_praise_teen_w_1on1 <- renderPlotly({plot_pp_praise_teen_w_1on1()})
    
    table_pp_praise_teen_w_praise <- reactive({summary_table_habits_praise()$`Praise teen `})
    plot_pp_praise_teen_w_praise <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen_w_praise", replace = "rp.contact.field.parent_point_count_praise_teen_w_", plot_type = "boxplot")})
    output$table_pp_praise_teen_w_praise <- shiny::renderTable({(table_pp_praise_teen_w_praise())}, striped = TRUE)
    output$plot_pp_praise_teen_w_praise <- renderPlotly({plot_pp_praise_teen_w_praise()})
    
    table_pp_praise_teen_w_instruct <- reactive({summary_table_habits_instruct()$`Praise teen `})
    plot_pp_praise_teen_w_instruct <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen_w_instruct", replace = "rp.contact.field.parent_point_count_praise_teen_w_", plot_type = "boxplot")})
    output$table_pp_praise_teen_w_instruct <- shiny::renderTable({(table_pp_praise_teen_w_instruct())}, striped = TRUE)
    output$plot_pp_praise_teen_w_instruct <- renderPlotly({plot_pp_praise_teen_w_instruct()})
    
    table_pp_praise_teen_w_stress <- reactive({summary_table_habits_stress()$`Praise teen `})
    plot_pp_praise_teen_w_stress <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen_w_stress", replace = "rp.contact.field.parent_point_count_praise_teen_w_", plot_type = "boxplot")})
    output$table_pp_praise_teen_w_stress <- shiny::renderTable({(table_pp_praise_teen_w_stress())}, striped = TRUE)
    output$plot_pp_praise_teen_w_stress <- renderPlotly({plot_pp_praise_teen_w_stress()})
    
    table_pp_praise_teen_w_money <- reactive({summary_table_habits_money()$`Praise teen `})
    plot_pp_praise_teen_w_money <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen_w_money", replace = "rp.contact.field.parent_point_count_praise_teen_w_", plot_type = "boxplot")})
    output$table_pp_praise_teen_w_money <- shiny::renderTable({(table_pp_praise_teen_w_money())}, striped = TRUE)
    output$plot_pp_praise_teen_w_money <- renderPlotly({plot_pp_praise_teen_w_money()})
    
    table_pp_praise_teen_w_rules <- reactive({summary_table_habits_rules()$`Praise teen `})
    plot_pp_praise_teen_w_rules <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen_w_rules", replace = "rp.contact.field.parent_point_count_praise_teen_w_", plot_type = "boxplot")})
    output$table_pp_praise_teen_w_rules <- shiny::renderTable({(table_pp_praise_teen_w_rules())}, striped = TRUE)
    output$plot_pp_praise_teen_w_rules <- renderPlotly({plot_pp_praise_teen_w_rules()})
    
    table_pp_praise_teen_w_consequence <- reactive({summary_table_habits_consequence()$`Praise teen`})
    plot_pp_praise_teen_w_consequence <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen_w_consequence", replace = "rp.contact.field.parent_point_count_praise_teen_w_", plot_type = "boxplot")})
    output$table_pp_praise_teen_w_consequence <- shiny::renderTable({(table_pp_praise_teen_w_consequence())}, striped = TRUE)
    output$plot_pp_praise_teen_w_consequence <- renderPlotly({plot_pp_praise_teen_w_consequence()})
    
    table_pp_praise_teen_w_solve <- reactive({summary_table_habits_solve()$`Praise teen`})
    plot_pp_praise_teen_w_solve <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen_w_solve", replace = "rp.contact.field.parent_point_count_praise_teen_w_", plot_type = "boxplot")})
    output$table_pp_praise_teen_w_solve <- shiny::renderTable({(table_pp_praise_teen_w_solve())}, striped = TRUE)
    output$plot_pp_praise_teen_w_solve <- renderPlotly({plot_pp_praise_teen_w_solve()})
    
    table_pp_praise_teen_w_safe <- reactive({summary_table_habits_safe()$`Praise teen`})
    plot_pp_praise_teen_w_safe <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen_w_safe", replace = "rp.contact.field.parent_point_count_praise_teen_w_", plot_type = "boxplot")})
    output$table_pp_praise_teen_w_safe <- shiny::renderTable({(table_pp_praise_teen_w_safe())}, striped = TRUE)
    output$plot_pp_praise_teen_w_safe <- renderPlotly({plot_pp_praise_teen_w_safe()})
    
    table_pp_praise_teen_w_crisis <- reactive({summary_table_habits_crisis()$`Praise teen`})
    plot_pp_praise_teen_w_crisis <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen_w_crisis", replace = "rp.contact.field.parent_point_count_praise_teen_w_", plot_type = "boxplot")})
    output$table_pp_praise_teen_w_crisis <- shiny::renderTable({(table_pp_praise_teen_w_crisis())}, striped = TRUE)
    output$plot_pp_praise_teen_w_crisis <- renderPlotly({plot_pp_praise_teen_w_crisis()})
    
    table_pp_praise_teen_w_celebrate <- reactive({summary_table_habits_celebrate()$`Praise teen`})
    plot_pp_praise_teen_w_celebrate <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen_w_celebrate", replace = "rp.contact.field.parent_point_count_praise_teen_w_", plot_type = "boxplot")})
    output$table_pp_praise_teen_w_celebrate <- shiny::renderTable({(table_pp_praise_teen_w_celebrate())}, striped = TRUE)
    output$plot_pp_praise_teen_w_celebrate <- renderPlotly({plot_pp_praise_teen_w_celebrate()})
    
    # Parent Point sub tab Instruct Positively points pp6
    table_pp_instruct_positively_ws_totals <- reactive({
      #Average instruct_positively parent points pp6
      summary_instruct_positively_workshop <- selected_data_dem() %>%
        group_by(Org, .drop = TRUE) %>%
        summarise(across(instruct_positively_workshop_vars, mean, na.rm = TRUE))
      colnames(summary_instruct_positively_workshop) <- naming_conventions(colnames(summary_instruct_positively_workshop), "rp.contact.field.parent_point_count_instruct_positively_w_")
      if (country != "Tanzania"){
        summary_instruct_positively_workshop <- summary_instruct_positively_workshop %>%
          dplyr::filter(Org %in% c((input$OrgDem)))
      }
      summary_instruct_positively_workshop
    })
    plot_pp_instruct_positively_ws_totals <- reactive({
      # Make the table longer so that it is in a format for use in ggplot
      summary_instruct_positively_workshop_long <- table_pp_instruct_positively_ws_totals() %>%
        pivot_longer(cols = !Org) %>%
        mutate(name = fct_relevel(name, week_order))   # set the order of variables
      ggplot(summary_instruct_positively_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
        geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        geom_line() + labs(x = "Workshop week", y = "Number of points")})
    output$table_pp_instruct_positively_ws_totals <- shiny::renderTable({table_pp_instruct_positively_ws_totals()})
    output$plot_pp_instruct_positively_ws_totals <- renderPlotly({plot_pp_instruct_positively_ws_totals()})
    
    table_pp_instruct_positively_w_self_care <- reactive({summary_table_habits_self_care()$`Instruct positively`})
    plot_pp_instruct_positively_w_self_care <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively_w_self_care", replace = "rp.contact.field.parent_point_count_instruct_positively_w", plot_type = "boxplot")})
    output$table_pp_instruct_positively_w_self_care <- shiny::renderTable({(table_pp_instruct_positively_w_self_care())}, striped = TRUE)
    output$plot_pp_instruct_positively_w_self_care <- renderPlotly({plot_pp_instruct_positively_w_self_care()})
    
    table_pp_instruct_positively_w_1on1 <- reactive({summary_table_habits_1on1()$`Instruct positively`})
    plot_pp_instruct_positively_w_1on1 <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively_w_1on1", replace = "rp.contact.field.parent_point_count_instruct_positively_w", plot_type = "boxplot")})
    output$table_pp_instruct_positively_w_1on1 <- shiny::renderTable({(table_pp_instruct_positively_w_1on1())}, striped = TRUE)
    output$plot_pp_instruct_positively_w_1on1 <- renderPlotly({plot_pp_instruct_positively_w_1on1()})
    
    table_pp_instruct_positively_w_praise <- reactive({summary_table_habits_praise()$`Instruct positively `})
    plot_pp_instruct_positively_w_praise <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively_w_praise", replace = "rp.contact.field.parent_point_count_instruct_positively_w", plot_type = "boxplot")})
    output$table_pp_instruct_positively_w_praise <- shiny::renderTable({(table_pp_instruct_positively_w_praise())}, striped = TRUE)
    output$plot_pp_instruct_positively_w_praise <- renderPlotly({plot_pp_instruct_positively_w_praise()})
    
    table_pp_instruct_positively_w_instruct <- reactive({summary_table_habits_instruct()$`Instruct positively `})
    plot_pp_instruct_positively_w_instruct <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively_w_instruct", replace = "rp.contact.field.parent_point_count_instruct_positively_w", plot_type = "boxplot")})
    output$table_pp_instruct_positively_w_instruct <- shiny::renderTable({(table_pp_instruct_positively_w_instruct())}, striped = TRUE)
    output$plot_pp_instruct_positively_w_instruct <- renderPlotly({plot_pp_instruct_positively_w_instruct()})
    
    table_pp_instruct_positively_w_stress <- reactive({summary_table_habits_stress()$`Instruct positively `})
    plot_pp_instruct_positively_w_stress <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively_w_stress", replace = "rp.contact.field.parent_point_count_instruct_positively_w", plot_type = "boxplot")})
    output$table_pp_instruct_positively_w_stress <- shiny::renderTable({(table_pp_instruct_positively_w_stress())}, striped = TRUE)
    output$plot_pp_instruct_positively_w_stress <- renderPlotly({plot_pp_instruct_positively_w_stress()})
    
    table_pp_instruct_positively_w_money <- reactive({summary_table_habits_money()$`Instruct positively`})
    plot_pp_instruct_positively_w_money <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively_w_money", replace = "rp.contact.field.parent_point_count_instruct_positively_w", plot_type = "boxplot")})
    output$table_pp_instruct_positively_w_money <- shiny::renderTable({(table_pp_instruct_positively_w_money())}, striped = TRUE)
    output$plot_pp_instruct_positively_w_money <- renderPlotly({plot_pp_instruct_positively_w_money()})
    
    table_pp_instruct_positively_w_rules <- reactive({summary_table_habits_rules()$`Instruct positively`})
    plot_pp_instruct_positively_w_rules <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively_w_rules", replace = "rp.contact.field.parent_point_count_instruct_positively_w", plot_type = "boxplot")})
    output$table_pp_instruct_positively_w_rules <- shiny::renderTable({(table_pp_instruct_positively_w_rules())}, striped = TRUE)
    output$plot_pp_instruct_positively_w_rules <- renderPlotly({plot_pp_instruct_positively_w_rules()})
    
    table_pp_instruct_positively_w_consequence <- reactive({summary_table_habits_consequence()$`Instruct positively`})
    plot_pp_instruct_positively_w_consequence <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively_w_consequence", replace = "rp.contact.field.parent_point_count_instruct_positively_w", plot_type = "boxplot")})
    output$table_pp_instruct_positively_w_consequence <- shiny::renderTable({(table_pp_instruct_positively_w_consequence())}, striped = TRUE)
    output$plot_pp_instruct_positively_w_consequence <- renderPlotly({plot_pp_instruct_positively_w_consequence()})
    
    table_pp_instruct_positively_w_solve <- reactive({summary_table_habits_solve()$`Instruct positively`})
    plot_pp_instruct_positively_w_solve <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively_w_solve", replace = "rp.contact.field.parent_point_count_instruct_positively_w", plot_type = "boxplot")})
    output$table_pp_instruct_positively_w_solve <- shiny::renderTable({(table_pp_instruct_positively_w_solve())}, striped = TRUE)
    output$plot_pp_instruct_positively_w_solve <- renderPlotly({plot_pp_instruct_positively_w_solve()})
    
    table_pp_instruct_positively_w_safe <- reactive({summary_table_habits_safe()$`Instruct positively`})
    plot_pp_instruct_positively_w_safe <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively_w_safe", replace = "rp.contact.field.parent_point_count_instruct_positively_w", plot_type = "boxplot")})
    output$table_pp_instruct_positively_w_safe <- shiny::renderTable({(table_pp_instruct_positively_w_safe())}, striped = TRUE)
    output$plot_pp_instruct_positively_w_safe <- renderPlotly({plot_pp_instruct_positively_w_safe()})
    
    table_pp_instruct_positively_w_crisis <- reactive({summary_table_habits_crisis()$`Instruct positively`})
    plot_pp_instruct_positively_w_crisis <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively_w_crisis", replace = "rp.contact.field.parent_point_count_instruct_positively_w", plot_type = "boxplot")})
    output$table_pp_instruct_positively_w_crisis <- shiny::renderTable({(table_pp_instruct_positively_w_crisis())}, striped = TRUE)
    output$plot_pp_instruct_positively_w_crisis <- renderPlotly({plot_pp_instruct_positively_w_crisis()})
    
    table_pp_instruct_positively_w_celebrate <- reactive({summary_table_habits_celebrate()$`Instruct positively`})
    plot_pp_instruct_positively_w_celebrate <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively_w_celebrate", replace = "rp.contact.field.parent_point_count_instruct_positively_w", plot_type = "boxplot")})
    output$table_pp_instruct_positively_w_celebrate <- shiny::renderTable({(table_pp_instruct_positively_w_celebrate())}, striped = TRUE)
    output$plot_pp_instruct_positively_w_celebrate <- renderPlotly({plot_pp_instruct_positively_w_celebrate()})
    
    # Parent Point sub tab Breathe points pp7
    table_pp_breathe_ws_totals <- reactive({
      #Average breathe parent points pp7
      summary_breathe_workshop <- selected_data_dem() %>%
        group_by(Org, .drop = TRUE) %>%
        summarise(across(breathe_workshop_vars, mean, na.rm = TRUE))
      colnames(summary_breathe_workshop) <- naming_conventions(colnames(summary_breathe_workshop), "rp.contact.field.parent_point_count_breathe_w_")
      if (country != "Tanzania"){
        summary_breathe_workshop <- summary_breathe_workshop %>%
          dplyr::filter(Org %in% c((input$OrgDem)))
      }
      summary_breathe_workshop
    })
    plot_pp_breathe_ws_totals <- reactive({
      # Make the table longer so that it is in a format for use in ggplot
      summary_breathe_workshop_long <- table_pp_breathe_ws_totals() %>%
        pivot_longer(cols = !Org) %>%
        mutate(name = fct_relevel(name, week_order))   # set the order of variables
      ggplot(summary_breathe_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
        geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        geom_line() + labs(x = "Workshop week", y = "Number of points")})
    output$table_pp_breathe_ws_totals <- shiny::renderTable({table_pp_breathe_ws_totals()})
    output$plot_pp_breathe_ws_totals <- renderPlotly({plot_pp_breathe_ws_totals()})
    
    table_pp_breathe_w_self_care <- reactive({summary_table_habits_self_care()$`Breathe`})
    plot_pp_breathe_w_self_care <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe_w_self_care", replace = "rp.contact.field.parent_point_count_breathe_w_", plot_type = "boxplot")})
    output$table_pp_breathe_w_self_care <- shiny::renderTable({(table_pp_breathe_w_self_care())}, striped = TRUE)
    output$plot_pp_breathe_w_self_care <- renderPlotly({plot_pp_breathe_w_self_care()})
    
    table_pp_breathe_w_1on1 <- reactive({summary_table_habits_1on1()$`Breathe `})
    plot_pp_breathe_w_1on1 <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe_w_1on1", replace = "rp.contact.field.parent_point_count_breathe_w_", plot_type = "boxplot")})
    output$table_pp_breathe_w_1on1 <- shiny::renderTable({(table_pp_breathe_w_1on1())}, striped = TRUE)
    output$plot_pp_breathe_w_1on1 <- renderPlotly({plot_pp_breathe_w_1on1()})
    
    table_pp_breathe_w_praise <- reactive({summary_table_habits_praise()$`Breathe `})
    plot_pp_breathe_w_praise <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe_w_praise", replace = "rp.contact.field.parent_point_count_breathe_w_", plot_type = "boxplot")})
    output$table_pp_breathe_w_praise <- shiny::renderTable({(table_pp_breathe_w_praise())}, striped = TRUE)
    output$plot_pp_breathe_w_praise <- renderPlotly({plot_pp_breathe_w_praise()})
    
    table_pp_breathe_w_instruct <- reactive({summary_table_habits_instruct()$`Breathe `})
    plot_pp_breathe_w_instruct <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe_w_instruct", replace = "rp.contact.field.parent_point_count_breathe_w_", plot_type = "boxplot")})
    output$table_pp_breathe_w_instruct <- shiny::renderTable({(table_pp_breathe_w_instruct())}, striped = TRUE)
    output$plot_pp_breathe_w_instruct <- renderPlotly({plot_pp_breathe_w_instruct()})
    
    table_pp_breathe_w_stress <- reactive({summary_table_habits_stress()$`Breathe `})
    plot_pp_breathe_w_stress <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe_w_stress", replace = "rp.contact.field.parent_point_count_breathe_w_", plot_type = "boxplot")})
    output$table_pp_breathe_w_stress <- shiny::renderTable({(table_pp_breathe_w_stress())}, striped = TRUE)
    output$plot_pp_breathe_w_stress <- renderPlotly({plot_pp_breathe_w_stress()})
    
    table_pp_breathe_w_money <- reactive({summary_table_habits_money()$`Breathe `})
    plot_pp_breathe_w_money <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe_w_money", replace = "rp.contact.field.parent_point_count_breathe_w_", plot_type = "boxplot")})
    output$table_pp_breathe_w_money <- shiny::renderTable({(table_pp_breathe_w_money())}, striped = TRUE)
    output$plot_pp_breathe_w_money <- renderPlotly({plot_pp_breathe_w_money()})
    
    table_pp_breathe_w_rules <- reactive({summary_table_habits_rules()$`Breathe `})
    plot_pp_breathe_w_rules <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe_w_rules", replace = "rp.contact.field.parent_point_count_breathe_w_", plot_type = "boxplot")})
    output$table_pp_breathe_w_rules <- shiny::renderTable({(table_pp_breathe_w_rules())}, striped = TRUE)
    output$plot_pp_breathe_w_rules <- renderPlotly({plot_pp_breathe_w_rules()})
    
    table_pp_breathe_w_consequence <- reactive({summary_table_habits_consequence()$`Breathe`})
    plot_pp_breathe_w_consequence <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe_w_consequence", replace = "rp.contact.field.parent_point_count_breathe_w_", plot_type = "boxplot")})
    output$table_pp_breathe_w_consequence <- shiny::renderTable({(table_pp_breathe_w_consequence())}, striped = TRUE)
    output$plot_pp_breathe_w_consequence <- renderPlotly({plot_pp_breathe_w_consequence()})
    
    table_pp_breathe_w_solve <- reactive({summary_table_habits_solve()$`Breathe`})
    plot_pp_breathe_w_solve <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe_w_solve", replace = "rp.contact.field.parent_point_count_breathe_w_", plot_type = "boxplot")})
    output$table_pp_breathe_w_solve <- shiny::renderTable({(table_pp_breathe_w_solve())}, striped = TRUE)
    output$plot_pp_breathe_w_solve <- renderPlotly({plot_pp_breathe_w_solve()})
    
    table_pp_breathe_w_safe <- reactive({summary_table_habits_safe()$`Breathe`})
    plot_pp_breathe_w_safe <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe_w_safe", replace = "rp.contact.field.parent_point_count_breathe_w_", plot_type = "boxplot")})
    output$table_pp_breathe_w_safe <- shiny::renderTable({(table_pp_breathe_w_safe())}, striped = TRUE)
    output$plot_pp_breathe_w_safe <- renderPlotly({plot_pp_breathe_w_safe()})
    
    table_pp_breathe_w_crisis <- reactive({summary_table_habits_crisis()$`Breathe`})
    plot_pp_breathe_w_crisis <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe_w_crisis", replace = "rp.contact.field.parent_point_count_breathe_w_", plot_type = "boxplot")})
    output$table_pp_breathe_w_crisis <- shiny::renderTable({(table_pp_breathe_w_crisis())}, striped = TRUE)
    output$plot_pp_breathe_w_crisis <- renderPlotly({plot_pp_breathe_w_crisis()})
    
    table_pp_breathe_w_celebrate <- reactive({summary_table_habits_celebrate()$`Breathe`})
    plot_pp_breathe_w_celebrate <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe_w_celebrate", replace = "rp.contact.field.parent_point_count_breathe_w_", plot_type = "boxplot")})
    output$table_pp_breathe_w_celebrate <- shiny::renderTable({(table_pp_breathe_w_celebrate())}, striped = TRUE)
    output$plot_pp_breathe_w_celebrate <- renderPlotly({plot_pp_breathe_w_celebrate()})
    
    # Parent Point sub tab Money points pp8
    table_pp_money_ws_totals <- reactive({
      #Average spend_time parent points pp8
      summary_money_workshop <- selected_data_dem() %>%
        group_by(Org, .drop = TRUE) %>%
        summarise(across(money_workshop_vars, mean, na.rm = TRUE))
      colnames(summary_money_workshop) <- naming_conventions(colnames(summary_money_workshop), "rp.contact.field.parent_point_count_money_w_")
      if (country != "Tanzania"){
        summary_money_workshop <- summary_money_workshop %>%
          dplyr::filter(Org %in% c((input$OrgDem)))
      }
      summary_money_workshop
    })
    plot_pp_money_ws_totals <- reactive({
      summary_money_workshop_long <- table_pp_money_ws_totals() %>%
        pivot_longer(cols = !Org) %>%
        mutate(name = fct_relevel(name, week_order))   # set the order of variables
      ggplot(summary_money_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
        geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        geom_line() + labs(x = "Workshop week", y = "Number of points")})
    output$table_pp_money_ws_totals <- shiny::renderTable({table_pp_money_ws_totals()})
    output$plot_pp_money_ws_totals <- renderPlotly({plot_pp_money_ws_totals()})
    
    table_pp_money_w_self_care <- reactive({summary_table_habits_self_care()$`Money`})
    plot_pp_money_w_self_care <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_money_w_self_care", replace = "rp.contact.field.parent_point_count_money_w_", plot_type = "boxplot")})
    output$table_pp_money_w_self_care <- shiny::renderTable({(table_pp_money_w_self_care())}, striped = TRUE)
    output$plot_pp_money_w_self_care <- renderPlotly({plot_pp_money_w_self_care()})
    
    table_pp_money_w_1on1 <- reactive({summary_table_habits_1on1()$`Money `})
    plot_pp_money_w_1on1 <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_money_w_1on1", replace = "rp.contact.field.parent_point_count_money_w_", plot_type = "boxplot")})
    output$table_pp_money_w_1on1 <- shiny::renderTable({(table_pp_money_w_1on1())}, striped = TRUE)
    output$plot_pp_money_w_1on1 <- renderPlotly({plot_pp_money_w_1on1()})
    
    table_pp_money_w_praise <- reactive({summary_table_habits_praise()$`Money `})
    plot_pp_money_w_praise <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_money_w_praise", replace = "rp.contact.field.parent_point_count_money_w_", plot_type = "boxplot")})
    output$table_pp_money_w_praise <- shiny::renderTable({(table_pp_money_w_praise())}, striped = TRUE)
    output$plot_pp_money_w_praise <- renderPlotly({plot_pp_money_w_praise()})
    
    table_pp_money_w_instruct <- reactive({summary_table_habits_instruct()$`Money `})
    plot_pp_money_w_instruct <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_money_w_instruct", replace = "rp.contact.field.parent_point_count_money_w_", plot_type = "boxplot")})
    output$table_pp_money_w_instruct <- shiny::renderTable({(table_pp_money_w_instruct())}, striped = TRUE)
    output$plot_pp_money_w_instruct <- renderPlotly({plot_pp_money_w_instruct()})
    
    table_pp_money_w_stress <- reactive({summary_table_habits_stress()$`Money `})
    plot_pp_money_w_stress <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_money_w_stress", replace = "rp.contact.field.parent_point_count_money_w_", plot_type = "boxplot")})
    output$table_pp_money_w_stress <- shiny::renderTable({(table_pp_money_w_stress())}, striped = TRUE)
    output$plot_pp_money_w_stress <- renderPlotly({plot_pp_money_w_stress()})
    
    table_pp_money_w_money <- reactive({summary_table_habits_money()$`Money `})
    plot_pp_money_w_money <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_money_w_money", replace = "rp.contact.field.parent_point_count_money_w_", plot_type = "boxplot")})
    output$table_pp_money_w_money <- shiny::renderTable({(table_pp_money_w_money())}, striped = TRUE)
    output$plot_pp_money_w_money <- renderPlotly({plot_pp_money_w_money()})
    
    table_pp_money_w_rules <- reactive({summary_table_habits_rules()$`Money `})
    plot_pp_money_w_rules <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_money_w_rules", replace = "rp.contact.field.parent_point_count_money_w_", plot_type = "boxplot")})
    output$table_pp_money_w_rules <- shiny::renderTable({(table_pp_money_w_rules())}, striped = TRUE)
    output$plot_pp_money_w_rules <- renderPlotly({plot_pp_money_w_rules()})
    
    table_pp_money_w_consequence <- reactive({summary_table_habits_consequence()$`Money`})
    plot_pp_money_w_consequence <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_money_w_consequence", replace = "rp.contact.field.parent_point_count_money_w_", plot_type = "boxplot")})
    output$table_pp_money_w_consequence <- shiny::renderTable({(table_pp_money_w_consequence())}, striped = TRUE)
    output$plot_pp_money_w_consequence <- renderPlotly({plot_pp_money_w_consequence()})
    
    table_pp_money_w_solve <- reactive({summary_table_habits_solve()$`Money`})
    plot_pp_money_w_solve <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_money_w_solve", replace = "rp.contact.field.parent_point_count_money_w_", plot_type = "boxplot")})
    output$table_pp_money_w_solve <- shiny::renderTable({(table_pp_money_w_solve())}, striped = TRUE)
    output$plot_pp_money_w_solve <- renderPlotly({plot_pp_money_w_solve()})
    
    table_pp_money_w_safe <- reactive({summary_table_habits_safe()$`Money`})
    plot_pp_money_w_safe <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_money_w_safe", replace = "rp.contact.field.parent_point_count_money_w_", plot_type = "boxplot")})
    output$table_pp_money_w_safe <- shiny::renderTable({(table_pp_money_w_safe())}, striped = TRUE)
    output$plot_pp_money_w_safe <- renderPlotly({plot_pp_money_w_safe()})
    
    table_pp_money_w_crisis <- reactive({summary_table_habits_crisis()$`Money`})
    plot_pp_money_w_crisis <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_money_w_crisis", replace = "rp.contact.field.parent_point_count_money_w_", plot_type = "boxplot")})
    output$table_pp_money_w_crisis <- shiny::renderTable({(table_pp_money_w_crisis())}, striped = TRUE)
    output$plot_pp_money_w_crisis <- renderPlotly({plot_pp_money_w_crisis()})
    
    table_pp_money_w_celebrate <- reactive({summary_table_habits_celebrate()$`Money`})
    plot_pp_money_w_celebrate <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_money_w_celebrate", replace = "rp.contact.field.parent_point_count_money_w_", plot_type = "boxplot")})
    output$table_pp_money_w_celebrate <- shiny::renderTable({(table_pp_money_w_celebrate())}, striped = TRUE)
    output$plot_pp_money_w_celebrate <- renderPlotly({plot_pp_money_w_celebrate()})
    
    # Parent Point sub tab Consequence points pp9
    table_pp_consequence_ws_totals <- reactive({
      summary_consequence_workshop <- selected_data_dem() %>%
        group_by(Org, .drop = TRUE) %>%
        summarise(across(consequence_workshop_vars, mean, na.rm = TRUE))
      colnames(summary_consequence_workshop) <- naming_conventions(colnames(summary_consequence_workshop), "rp.contact.field.parent_point_count_consequence_w_")
      if (country != "Tanzania"){
        summary_consequence_workshop <- summary_consequence_workshop %>%
          dplyr::filter(Org %in% c((input$OrgDem)))
      }
      summary_consequence_workshop
    })
    plot_pp_consequence_ws_totals <- reactive({
      summary_consequence_workshop_long <- table_pp_consequence_ws_totals() %>%
        pivot_longer(cols = !Org) %>%
        mutate(name = fct_relevel(name, week_order))   # set the order of variables
      ggplot(summary_consequence_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
        geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        geom_line() + labs(x = "Workshop week", y = "Number of points")})
    output$table_pp_consequence_ws_totals <- shiny::renderTable({table_pp_consequence_ws_totals()})
    output$plot_pp_consequence_ws_totals <- renderPlotly({plot_pp_consequence_ws_totals()})
    
    table_pp_consequence_w_self_care <- reactive({summary_table_habits_self_care()$`Consequence`})
    plot_pp_consequence_w_self_care <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence_w_self_care", replace = "rp.contact.field.parent_point_count_consequence_w_", plot_type = "boxplot")})
    output$table_pp_consequence_w_self_care <- shiny::renderTable({(table_pp_consequence_w_self_care())}, striped = TRUE)
    output$plot_pp_consequence_w_self_care <- renderPlotly({plot_pp_consequence_w_self_care()})
    
    table_pp_consequence_w_1on1 <- reactive({summary_table_habits_1on1()$`Consequence `})
    plot_pp_consequence_w_1on1 <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence_w_1on1", replace = "rp.contact.field.parent_point_count_consequence_w_", plot_type = "boxplot")})
    output$table_pp_consequence_w_1on1 <- shiny::renderTable({(table_pp_consequence_w_1on1())}, striped = TRUE)
    output$plot_pp_consequence_w_1on1 <- renderPlotly({plot_pp_consequence_w_1on1()})
    
    table_pp_consequence_w_praise <- reactive({summary_table_habits_praise()$`Consequence `})
    plot_pp_consequence_w_praise <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence_w_praise", replace = "rp.contact.field.parent_point_count_consequence_w_", plot_type = "boxplot")})
    output$table_pp_consequence_w_praise <- shiny::renderTable({(table_pp_consequence_w_praise())}, striped = TRUE)
    output$plot_pp_consequence_w_praise <- renderPlotly({plot_pp_consequence_w_praise()})
    
    table_pp_consequence_w_instruct <- reactive({summary_table_habits_instruct()$`Consequence `})
    plot_pp_consequence_w_instruct <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence_w_instruct", replace = "rp.contact.field.parent_point_count_consequence_w_", plot_type = "boxplot")})
    output$table_pp_consequence_w_instruct <- shiny::renderTable({(table_pp_consequence_w_instruct())}, striped = TRUE)
    output$plot_pp_consequence_w_instruct <- renderPlotly({plot_pp_consequence_w_instruct()})
    
    table_pp_consequence_w_stress <- reactive({summary_table_habits_stress()$`Consequence `})
    plot_pp_consequence_w_stress <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence_w_stress", replace = "rp.contact.field.parent_point_count_consequence_w_", plot_type = "boxplot")})
    output$table_pp_consequence_w_stress <- shiny::renderTable({(table_pp_consequence_w_stress())}, striped = TRUE)
    output$plot_pp_consequence_w_stress <- renderPlotly({plot_pp_consequence_w_stress()})
    
    table_pp_consequence_w_money <- reactive({summary_table_habits_money()$`Consequence `})
    plot_pp_consequence_w_money <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence_w_money", replace = "rp.contact.field.parent_point_count_consequence_w_", plot_type = "boxplot")})
    output$table_pp_consequence_w_money <- shiny::renderTable({(table_pp_consequence_w_money())}, striped = TRUE)
    output$plot_pp_consequence_w_money <- renderPlotly({plot_pp_consequence_w_money()})
    
    table_pp_consequence_w_rules <- reactive({summary_table_habits_rules()$`Consequence `})
    plot_pp_consequence_w_rules <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence_w_rules", replace = "rp.contact.field.parent_point_count_consequence_w_", plot_type = "boxplot")})
    output$table_pp_consequence_w_rules <- shiny::renderTable({(table_pp_consequence_w_rules())}, striped = TRUE)
    output$plot_pp_consequence_w_rules <- renderPlotly({plot_pp_consequence_w_rules()})
    
    table_pp_consequence_w_consequence <- reactive({summary_table_habits_consequence()$`Consequence`})
    plot_pp_consequence_w_consequence <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence_w_consequence", replace = "rp.contact.field.parent_point_count_consequence_w_", plot_type = "boxplot")})
    output$table_pp_consequence_w_consequence <- shiny::renderTable({(table_pp_consequence_w_consequence())}, striped = TRUE)
    output$plot_pp_consequence_w_consequence <- renderPlotly({plot_pp_consequence_w_consequence()})
    
    table_pp_consequence_w_solve <- reactive({summary_table_habits_solve()$`Consequence`})
    plot_pp_consequence_w_solve <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence_w_solve", replace = "rp.contact.field.parent_point_count_consequence_w_", plot_type = "boxplot")})
    output$table_pp_consequence_w_solve <- shiny::renderTable({(table_pp_consequence_w_solve())}, striped = TRUE)
    output$plot_pp_consequence_w_solve <- renderPlotly({plot_pp_consequence_w_solve()})
    
    table_pp_consequence_w_safe <- reactive({summary_table_habits_safe()$`Consequence`})
    plot_pp_consequence_w_safe <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence_w_safe", replace = "rp.contact.field.parent_point_count_consequence_w_", plot_type = "boxplot")})
    output$table_pp_consequence_w_safe <- shiny::renderTable({(table_pp_consequence_w_safe())}, striped = TRUE)
    output$plot_pp_consequence_w_safe <- renderPlotly({plot_pp_consequence_w_safe()})
    
    table_pp_consequence_w_crisis <- reactive({summary_table_habits_crisis()$`Consequence`})
    plot_pp_consequence_w_crisis <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence_w_crisis", replace = "rp.contact.field.parent_point_count_consequence_w_", plot_type = "boxplot")})
    output$table_pp_consequence_w_crisis <- shiny::renderTable({(table_pp_consequence_w_crisis())}, striped = TRUE)
    output$plot_pp_consequence_w_crisis <- renderPlotly({plot_pp_consequence_w_crisis()})
    
    table_pp_consequence_w_celebrate <- reactive({summary_table_habits_celebrate()$`Consequence`})
    plot_pp_consequence_w_celebrate <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence_w_celebrate", replace = "rp.contact.field.parent_point_count_consequence_w_", plot_type = "boxplot")})
    output$table_pp_consequence_w_celebrate <- shiny::renderTable({(table_pp_consequence_w_celebrate())}, striped = TRUE)
    output$plot_pp_consequence_w_celebrate <- renderPlotly({plot_pp_consequence_w_celebrate()})
    
    # Parent Point sub tab Safe points pp10
    table_pp_safe_ws_totals <- reactive({
      summary_safe_workshop <- selected_data_dem() %>%
        group_by(Org, .drop = TRUE) %>%
        summarise(across(safe_workshop_vars, mean, na.rm = TRUE))
      colnames(summary_safe_workshop) <- naming_conventions(colnames(summary_safe_workshop), "rp.contact.field.parent_point_count_safe_w_")
      if (country != "Tanzania"){
        summary_safe_workshop <- summary_safe_workshop %>%
          dplyr::filter(Org %in% c((input$OrgDem)))
      }
      summary_safe_workshop
    })
    plot_pp_safe_ws_totals <- reactive({
      summary_safe_workshop_long <- table_pp_safe_ws_totals() %>%
        pivot_longer(cols = !Org) %>%
        mutate(name = fct_relevel(name, week_order))   # set the order of variables
      ggplot(summary_safe_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
        geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        geom_line() + labs(x = "Workshop week", y = "Number of points")})
    output$table_pp_safe_ws_totals <- shiny::renderTable({table_pp_safe_ws_totals()})
    output$plot_pp_safe_ws_totals <- renderPlotly({plot_pp_safe_ws_totals()})
    
    table_pp_safe_w_self_care <- reactive({summary_table_habits_self_care()$`Safe`})
    plot_pp_safe_w_self_care <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_safe_w_self_care", replace = "rp.contact.field.parent_point_count_safe_w", plot_type = "boxplot")})
    output$table_pp_safe_w_self_care <- shiny::renderTable({(table_pp_safe_w_self_care())}, striped = TRUE)
    output$plot_pp_safe_w_self_care <- renderPlotly({plot_pp_safe_w_self_care()})
    
    table_pp_safe_w_1on1 <- reactive({summary_table_habits_1on1()$`Safe `})
    plot_pp_safe_w_1on1 <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_safe_w_1on1", replace = "rp.contact.field.parent_point_count_safe_w", plot_type = "boxplot")})
    output$table_pp_safe_w_1on1 <- shiny::renderTable({(table_pp_safe_w_1on1())}, striped = TRUE)
    output$plot_pp_safe_w_1on1 <- renderPlotly({plot_pp_safe_w_1on1()})
    
    table_pp_safe_w_praise <- reactive({summary_table_habits_praise()$`Safe `})
    plot_pp_safe_w_praise <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_safe_w_praise", replace = "rp.contact.field.parent_point_count_safe_w", plot_type = "boxplot")})
    output$table_pp_safe_w_praise <- shiny::renderTable({(table_pp_safe_w_praise())}, striped = TRUE)
    output$plot_pp_safe_w_praise <- renderPlotly({plot_pp_safe_w_praise()})
    
    table_pp_safe_w_instruct <- reactive({summary_table_habits_instruct()$`Safe `})
    plot_pp_safe_w_instruct <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_safe_w_instruct", replace = "rp.contact.field.parent_point_count_safe_w", plot_type = "boxplot")})
    output$table_pp_safe_w_instruct <- shiny::renderTable({(table_pp_safe_w_instruct())}, striped = TRUE)
    output$plot_pp_safe_w_instruct <- renderPlotly({plot_pp_safe_w_instruct()})
    
    table_pp_safe_w_stress <- reactive({summary_table_habits_stress()$`Safe `})
    plot_pp_safe_w_stress <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_safe_w_stress", replace = "rp.contact.field.parent_point_count_safe_w", plot_type = "boxplot")})
    output$table_pp_safe_w_stress <- shiny::renderTable({(table_pp_safe_w_stress())}, striped = TRUE)
    output$plot_pp_safe_w_stress <- renderPlotly({plot_pp_safe_w_stress()})
    
    table_pp_safe_w_money <- reactive({summary_table_habits_money()$`Safe `})
    plot_pp_safe_w_money <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_safe_w_money", replace = "rp.contact.field.parent_point_count_safe_w", plot_type = "boxplot")})
    output$table_pp_safe_w_money <- shiny::renderTable({(table_pp_safe_w_money())}, striped = TRUE)
    output$plot_pp_safe_w_money <- renderPlotly({plot_pp_safe_w_money()})
    
    table_pp_safe_w_rules <- reactive({summary_table_habits_rules()$`Safe `})
    plot_pp_safe_w_rules <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_safe_w_rules", replace = "rp.contact.field.parent_point_count_safe_w", plot_type = "boxplot")})
    output$table_pp_safe_w_rules <- shiny::renderTable({(table_pp_safe_w_rules())}, striped = TRUE)
    output$plot_pp_safe_w_rules <- renderPlotly({plot_pp_safe_w_rules()})
    
    table_pp_safe_w_consequence <- reactive({summary_table_habits_consequence()$`Safe`})
    plot_pp_safe_w_consequence <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_safe_w_consequence", replace = "rp.contact.field.parent_point_count_safe_w", plot_type = "boxplot")})
    output$table_pp_safe_w_consequence <- shiny::renderTable({(table_pp_safe_w_consequence())}, striped = TRUE)
    output$plot_pp_safe_w_consequence <- renderPlotly({plot_pp_safe_w_consequence()})
    
    table_pp_safe_w_solve <- reactive({summary_table_habits_solve()$`Safe`})
    plot_pp_safe_w_solve <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_safe_w_solve", replace = "rp.contact.field.parent_point_count_safe_w", plot_type = "boxplot")})
    output$table_pp_safe_w_solve <- shiny::renderTable({(table_pp_safe_w_solve())}, striped = TRUE)
    output$plot_pp_safe_w_solve <- renderPlotly({plot_pp_safe_w_solve()})
    
    table_pp_safe_w_safe <- reactive({summary_table_habits_safe()$`Safe`})
    plot_pp_safe_w_safe <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_safe_w_safe", replace = "rp.contact.field.parent_point_count_safe_w", plot_type = "boxplot")})
    output$table_pp_safe_w_safe <- shiny::renderTable({(table_pp_safe_w_safe())}, striped = TRUE)
    output$plot_pp_safe_w_safe <- renderPlotly({plot_pp_safe_w_safe()})
    
    table_pp_safe_w_crisis <- reactive({summary_table_habits_crisis()$`Safe`})
    plot_pp_safe_w_crisis <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_safe_w_crisis", replace = "rp.contact.field.parent_point_count_safe_w", plot_type = "boxplot")})
    output$table_pp_safe_w_crisis <- shiny::renderTable({(table_pp_safe_w_crisis())}, striped = TRUE)
    output$plot_pp_safe_w_crisis <- renderPlotly({plot_pp_safe_w_crisis()})
    
    table_pp_safe_w_celebrate <- reactive({summary_table_habits_celebrate()$`Safe`})
    plot_pp_safe_w_celebrate <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.parent_point_count_safe_w_celebrate", replace = "rp.contact.field.parent_point_count_safe_w", plot_type = "boxplot")})
    output$table_pp_safe_w_celebrate <- shiny::renderTable({(table_pp_safe_w_celebrate())}, striped = TRUE)
    output$plot_pp_safe_w_celebrate <- renderPlotly({plot_pp_safe_w_celebrate()})
    
    
    #FOURTH Tab In-week Engagement ---------------------------
    tables_app_opens <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_app_opens,
                                                               replace = "rp.contact.field.")
      names(summary_table_baseline_build) <- data_app_opens_neat
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    #App Opens tab 4.1
    table_appopen_totals <- reactive({
      tables_app_opens()$`Total`
    }) 
    plot_appopen_totals <- reactive({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.app_launch_count", replace = "rp.contact.field.")}) 
    output$table_appopen_totals <- shiny::renderTable({(table_appopen_totals())}, striped = TRUE)
    output$plot_appopen_totals <- renderPlotly({plot_appopen_totals()})
    
    table_appopen_mean_week <- reactive({
      #Average app opens per ws week
      summary_mean_appopens <- selected_data_dem() %>%
        group_by(Org) %>%
        summarise(across(data_app_opens, mean, na.rm = TRUE))
      colnames(summary_mean_appopens)[2:length(summary_mean_appopens)] <- data_app_opens_neat
      if (country == "Tanzania"){
        summary_mean_appopens <- summary_mean_appopens
      } else {
        summary_mean_appopens <- summary_mean_appopens %>%
          dplyr::filter(Org %in% c((input$OrgDem)))
      }
      summary_mean_appopens
    }) 
    plot_appopen_mean_week <- reactive({
      # Make the table longer so that it is in a format for use in ggplot
      summary_mean_appopens_long <- table_appopen_mean_week() %>%
        pivot_longer(cols = !Org, names_to = "Workshop Week", values_to = "Value") %>% mutate(`Workshop Week` = fct_relevel(`Workshop Week`, data_app_opens_neat))
      ggplot(summary_mean_appopens_long, aes(x = `Workshop Week`, y = Value, fill = Org)) + 
        geom_bar(stat = "identity", position = "dodge") +
        scale_x_discrete(guide = guide_axis(angle = 90), limits = data_app_opens_neat) +
        viridis::scale_fill_viridis(discrete = TRUE)
    }) 
    output$table_appopen_mean_week <- shiny::renderTable({(table_appopen_mean_week())}, striped = TRUE)
    output$plot_appopen_mean_week <- renderPlotly({plot_appopen_mean_week()})
    
    # Push notifications tab 4.2
    table_pushn_totals <- reactive({pn_summary_count }) 
    plot_pushn_totals <- reactive({}) 
    output$table_pushn_totals <- shiny::renderTable({(table_pushn_totals())}, striped = TRUE)
    output$plot_pushn_totals <- renderPlotly({plot_pushn_totals()})
    
    # table_pushn_mean <- reactive({
    # })
    # plot_pushn_mean <- reactive({
    # })
    # output$table_pushn_mean <- shiny::renderTable({(table_pushn_mean())}, striped = TRUE)
    # output$plot_pushn_mean <- renderPlotly({plot_pushn_mean()})
    
    
    # Home Practice tab 4.3
    #NB no home practice for worshops 1 and 12 (welcome and celebration)
    
    # HP review started per week
    table_hp_rev_totals <- reactive({table_hp_started %>% filter(Org %in% unique(selected_data_dem()$Org))})
    plot_hp_rev_totals <- reactive({})
    output$table_hp_rev_totals <- shiny::renderTable({(table_hp_rev_totals())}, striped = TRUE)
    output$plot_hp_rev_totals <- renderPlotly({plot_hp_rev_totals()})
    
    
    # home practice review - user claims they had a chance to do the hp
    summary_table_hp_done <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_hp_done,
                                                               replace = "rp.contact.field.w_",
                                                               replace_after = "_done")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    # home practice review - user notes how HP went
    summary_table_hp_mood <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_hp_mood,
                                                               replace = "rp.contact.field.w_",
                                                               replace_after = "_mood")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    #HP 2 One on One (NB no HP for WS 1)
    table_hpdone_1on1 <- reactive({summary_table_hp_done()$`1on1 hp` })
    plot_hpdone_1on1 <- reactive({})
    output$table_hpdone_1on1 <- shiny::renderTable({(table_hpdone_1on1())}, striped = TRUE)
    output$plot_hpdone_1on1 <- renderPlotly({plot_hpdone_1on1()})
    
    table_mood_1on1 <- reactive({summary_table_hp_mood()$`1on1 hp` })
    plot_mood_1on1 <- reactive({})
    output$table_mood_1on1 <- shiny::renderTable({(table_mood_1on1())}, striped = TRUE)
    output$plot_mood_1on1 <- renderPlotly({plot_mood_1on1()})
    
    table_chall_1on1 <- reactive({summary_table_hp_chall$`Challenges 1on1` })
    plot_chall_1on1 <- reactive({})
    output$table_chall_1on1 <- shiny::renderTable({(table_chall_1on1())}, striped = TRUE,
                                                  caption = "1 = I don’t have enough time; 2 = My teen does not want to spend time with me; 3 = My teen only wants to watch TV or play on his/her phone; 4 = My teen wants to do things that are not safe or that cost money; 5 = My teen wants to do things that I cannot physically do; 6 = My teen chose a competitive activity. I won and s/he got angry.; 7 = I struggled to end the one-on-one time; 8 = All my children want one-on-one time with me at the same time")
    output$plot_chall_1on1 <- renderPlotly({plot_chall_1on1()})
    
    #HP 3 Praise (no review/ mood and no challenges)
    table_hpdone_praise <- reactive({summary_table_hp_done()$`Praise hp` })
    plot_hpdone_praise <- reactive({})
    output$table_hpdone_praise <- shiny::renderTable({(table_hpdone_praise())}, striped = TRUE)
    output$plot_hpdone_praise <- renderPlotly({plot_hpdone_praise()})
    
    table_mood_praise <- reactive({})
    plot_mood_praise <- reactive({})
    output$table_mood_praise <- shiny::renderTable({(table_mood_praise())}, striped = TRUE)
    output$plot_mood_praise <- renderPlotly({plot_mood_praise()})
    
    table_chall_praise <- reactive({})
    plot_chall_praise <- reactive({})
    output$table_chall_praise <- shiny::renderTable({(table_chall_praise())}, striped = TRUE)
    # caption
    output$plot_chall_praise <- renderPlotly({plot_chall_praise()})
    
    #HP 4 Pos Instr
    table_hpdone_instruct <- reactive({summary_table_hp_done()$`Instruct hp` })
    plot_hpdone_instruct <- reactive({})
    output$table_hpdone_instruct <- shiny::renderTable({(table_hpdone_instruct())}, striped = TRUE)
    output$plot_hpdone_instruct <- renderPlotly({plot_hpdone_instruct()})
    
    table_mood_instruct <- reactive({summary_table_hp_mood()$`Instruct hp`  })
    plot_mood_instruct <- reactive({})
    output$table_mood_instruct <- shiny::renderTable({(table_mood_instruct())}, striped = TRUE)
    output$plot_mood_instruct <- renderPlotly({plot_mood_instruct()})
    
    table_chall_instruct <- reactive({summary_table_hp_chall$`Challenges 1on1` })
    plot_chall_instruct <- reactive({})
    output$table_chall_instruct <- shiny::renderTable({(table_chall_instruct())}, striped = TRUE,
                                                      caption = "1 = My teenager did not want to follow the instruction; 2 = I did not find time to spend one-on-one time with my teen; 3 = I gave a negative instead of a positive instruction; 4 = I shouted at my teen when they behaved negatively, instead of giving them a positive instruction for what they should do")
    output$plot_chall_instruct <- renderPlotly({plot_chall_instruct()})
    
    #HP 5.1 Stress - Breathe
    table_hpdone_stress_br <- reactive({summary_table_hp_done()$`Stress hp breathe` })
    plot_hpdone_stress_br <- reactive({})
    output$table_hpdone_stress_br <- shiny::renderTable({(table_hpdone_stress_br())}, striped = TRUE)
    output$plot_hpdone_stress_br <- renderPlotly({plot_hpdone_stress_br()})
    
    table_mood_stress_br <- reactive({summary_table_hp_mood()$`Stress hp breathe` })
    plot_mood_stress_br <- reactive({})
    output$table_mood_stress_br <- shiny::renderTable({(table_mood_stress_br())}, striped = TRUE)
    output$plot_mood_stress_br <- renderPlotly({plot_mood_stress_br()})
    
    # TODO: for summary_table_hp_chall add filter options
    table_chall_stress_br <- reactive({summary_table_hp_chall$`Challenges 1on1`})
    plot_chall_stress_br <- reactive({})
    output$table_chall_stress_br <- shiny::renderTable({(table_chall_stress_br())}, striped = TRUE,
                                                       caption = "1 = I was afraid my teen would think I was weak; 2 = I felt uncomfortable about naming specific difficult feelings; 3 = My teen felt uncomfortable when I shared my feelings; 4 = When I shared my feelings, my teen asked many questions, which made me uncomfortable; 5 = I was too stressed or angry to try sharing my feelings – I prefer to be alone when I feel like that")
    output$plot_chall_stress_br <- renderPlotly({plot_chall_stress_br()})
    
    #HP 5.2 Stress - Talk
    table_hpdone_stress_tk <- reactive({summary_table_hp_done()$`Stress hp talk` })
    plot_hpdone_stress_tk <- reactive({})
    output$table_hpdone_stress_tk <- shiny::renderTable({(table_hpdone_stress_tk())}, striped = TRUE)
    output$plot_hpdone_stress_tk <- renderPlotly({plot_hpdone_stress_tk()})
    
    table_mood_stress_tk <- reactive({summary_table_hp_mood()$`Stress hp talk` })
    plot_mood_stress_tk <- reactive({})
    output$table_mood_stress_tk <- shiny::renderTable({(table_mood_stress_tk())}, striped = TRUE)
    output$plot_mood_stress_tk <- renderPlotly({plot_mood_stress_tk()})
    
    # table_chall_stress_tk <- reactive({summary_table_hp_chall$`Challenges 1on1` })
    # plot_chall_stress_tk <- reactive({})
    # output$table_chall_stress_tk <- shiny::renderTable({(table_chall_stress_tk())}, striped = TRUE)
    #       #caption = "1 = ; 2 = ; 3 = ; 4 =; 3 = ; 5 = ; 6 = ; 7 = ; 8 = ")
    # output$plot_chall_stress_tk <- renderPlotly({plot_chall_stress_tk()})
    
    #HP 6 Fam Budg
    table_hpdone_money <- reactive({summary_table_hp_done()$`Money hp` })
    plot_hpdone_money <- reactive({})
    output$table_hpdone_money <- shiny::renderTable({(table_hpdone_money())}, striped = TRUE)
    output$plot_hpdone_money <- renderPlotly({plot_hpdone_money()})
    
    table_mood_money <- reactive({summary_table_hp_mood()$`Money hp` })
    plot_mood_money <- reactive({})
    output$table_mood_money <- shiny::renderTable({(table_mood_money())}, striped = TRUE)
    output$plot_mood_money <- renderPlotly({plot_mood_money()})
    
    table_chall_money <- reactive({summary_table_hp_chall$`Challenges 1on1` })
    plot_chall_money <- reactive({})
    output$table_chall_money <- shiny::renderTable({(table_chall_money())}, striped = TRUE,
                                                   caption = "1 = I did not want to tell my family how much I earn; 2 = When budgeting, we could not agree on what should fall under needs and what should fall under wants; 3 = I did not understand what to do; 4 = My teen did not want to do the budgeting with me")
    output$plot_chall_money <- renderPlotly({plot_chall_money()})
    
    #HP 7 Rules
    table_hpdone_rule <- reactive({summary_table_hp_done()$`Rules hp` })
    plot_hpdone_rule <- reactive({})
    output$table_hpdone_rule <- shiny::renderTable({(table_hpdone_rule())}, striped = TRUE)
    output$plot_hpdone_rule <- renderPlotly({plot_hpdone_rule()})
    
    table_mood_rule <- reactive({summary_table_hp_mood()$`Rules hp` })
    plot_mood_rule <- reactive({})
    output$table_mood_rule <- shiny::renderTable({(table_mood_rule())}, striped = TRUE)
    output$plot_mood_rule <- renderPlotly({plot_mood_rule()})
    
    table_chall_rule <- reactive({summary_table_hp_chall$`Challenges 1on1` })
    plot_chall_rule <- reactive({})
    output$table_chall_rule <- shiny::renderTable({(table_chall_rule())}, striped = TRUE,
                                                  caption = "1 = My teen and I could not agree on a rule; 2 = My teen felt it was unfair that they have to follow the rule while I don’t have to follow it; 3 = I felt uncomfortable, because I feel that I should be the one establishing the rule, not my teen; 4 = My partner felt uncomfortable, because they feel that they should be the one establishing the rule, not the teen; 5 = When we tried to set a rule, we got into an argument; 6 = We were not able to stick to the rule")
    output$plot_chall_rule <- renderPlotly({plot_chall_rule()})
    
    #HP 8 Calm Cons
    table_hpdone_consequence <- reactive({summary_table_hp_done()$`Consequence hp` })
    plot_hpdone_consequence <- reactive({})
    output$table_hpdone_consequence <- shiny::renderTable({(table_hpdone_consequence())}, striped = TRUE)
    output$plot_hpdone_consequence <- renderPlotly({plot_hpdone_consequence()})
    
    table_mood_consequence <- reactive({summary_table_hp_mood()$`Consequence hp` })
    plot_mood_consequence <- reactive({})
    output$table_mood_consequence <- shiny::renderTable({(table_mood_consequence())}, striped = TRUE)
    output$plot_mood_consequence <- renderPlotly({plot_mood_consequence()})
    
    table_chall_consequence <- reactive({summary_table_hp_chall$`Challenges 1on1` })
    plot_chall_consequence <- reactive({})
    output$table_chall_consequence <- shiny::renderTable({(table_chall_consequence())}, striped = TRUE,
                                                         caption = "1 = I got very angry when my teen broke the rule; 2 = My teen got very angry with me after I gave the consequence; 3 = I introduced the consequence without first discussing it with my teen; 4 = I forgot to follow through with the consequence; 5 = Even with the consequence, my teen still does not follow the rule; 6 = We only created a negative consequence, not a positive consequence; 7 = My teen suggested being hit as a negative consequence")
    output$plot_chall_consequence <- renderPlotly({plot_chall_consequence()})
    
    #HP 9 Pr Solve
    table_hpdone_solve <- reactive({summary_table_hp_done()$`Solve hp` })
    plot_hpdone_solve <- reactive({})
    output$table_hpdone_solve <- shiny::renderTable({(table_hpdone_solve())}, striped = TRUE)
    output$plot_hpdone_solve <- renderPlotly({plot_hpdone_solve()})
    
    table_mood_solve <- reactive({summary_table_hp_mood()$`Solve hp` })
    plot_mood_solve <- reactive({})
    output$table_mood_solve <- shiny::renderTable({(table_mood_solve())}, striped = TRUE)
    output$plot_mood_solve <- renderPlotly({plot_mood_solve()})
    
    table_chall_solve <- reactive({summary_table_hp_chall$`Challenges 1on1` })
    plot_chall_solve <- reactive({})
    output$table_chall_solve <- shiny::renderTable({(table_chall_solve())}, striped = TRUE,
                                                   caption = "1 = I forgot the steps of problem solving; 2 = I started with the solutions right away; 3 = We could not agree on a solution to try out and we got into an argument; 4 = I got angry when the problem came up and I forgot to use the problem-solving steps; 5 = My teen got angry and did not want to talk about the problem")
    output$plot_chall_solve <- renderPlotly({plot_chall_solve()})
    
    #HP 10 Teen Safe
    table_hpdone_safe <- reactive({summary_table_hp_done()$`Safe hp` })
    plot_hpdone_safe <- reactive({})
    output$table_hpdone_safe <- shiny::renderTable({(table_hpdone_safe())}, striped = TRUE)
    output$plot_hpdone_safe <- renderPlotly({plot_hpdone_safe()})
    
    table_mood_safe <- reactive({summary_table_hp_mood()$`Safe hp` })
    plot_mood_safe <- reactive({})
    output$table_mood_safe <- shiny::renderTable({(table_mood_safe())}, striped = TRUE)
    output$plot_mood_safe <- renderPlotly({plot_mood_safe()})
    
    table_chall_safe <- reactive({summary_table_hp_chall$`Challenges 1on1`})
    plot_chall_safe <- reactive({})
    output$table_chall_safe <- shiny::renderTable({(table_chall_safe())}, striped = TRUE,
                                                  caption = "1 = My teen and I disagreed on which areas and online activities were unsafe; 2 = My teen identified the house of someone I trust as unsafe. I was shocked and did not know what to do; 3 = My teen insisted that the bar (or another place I don’t feel is safe) is safe for them to visit. I don’t know how to convince my teen; 4 = As an adult, I feel responsible to protect - but when I told my teen what is safe and what is not, my teen got angry; 5 = It was hard to identify support resources available in my community, because I don’t know my community so well / there are few services available; 6 = I don’t know much about technology, so I don’t know how to talk about it with my teen")
    output$plot_chall_safe <- renderPlotly({plot_chall_safe()})
    
    #HP 11 D w Crisis
    table_hpdone_crisis <- reactive({summary_table_hp_done()$`Crisis hp`})
    plot_hpdone_crisis <- reactive({})
    output$table_hpdone_crisis <- shiny::renderTable({(table_hpdone_crisis())}, striped = TRUE)
    output$plot_hpdone_crisis <- renderPlotly({plot_hpdone_crisis()})
    
    table_mood_crisis <- reactive({summary_table_hp_mood()$`Crisis hp`})
    plot_mood_crisis <- reactive({})
    output$table_mood_crisis <- shiny::renderTable({(table_mood_crisis())}, striped = TRUE)
    output$plot_mood_crisis <- renderPlotly({plot_mood_crisis()})
    
    table_chall_crisis <- reactive({summary_table_hp_chall$`Challenges 1on1` })
    plot_chall_crisis <- reactive({})
    output$table_chall_crisis <- shiny::renderTable({(table_chall_crisis())}, striped = TRUE,
                                                    caption = "1 = My teen told me that something serious happened to them and I did not know how to handle it; 2 = The conversation made me very uncomfortable because I was reminded of a negative experience I had; 3 = One of us did not feel comfortable")
    output$plot_chall_crisis <- renderPlotly({plot_chall_crisis()})
    
    #FIFTH Tab Surveys
    summary_table_survey_past_week_build <- reactive({
      tabulate_with_metadata(data = selected_data_dem(), location_ID = "survey_past_week")
    })
    summary_table_survey_past_week <- reactive({
      summary_table_survey_past_week_build() %>% 
        purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
    })
    
    # Baseline survey completion levels
    table_sv1_totals <- reactive({
      summary_table_survey_completion <- selected_data_dem() %>%
        summary_table(columns_to_summarise = "rp.contact.field.survey_welcome_and_setup_completion_level", display = FALSE, include_margins = TRUE)
      summary_table_survey_completion <- summary_table_survey_completion %>%
        dplyr::filter(Org %in% unique(selected_data_dem()$Org))
      summary_table_survey_completion
    })
    plot_sv1_totals <- reactive({
      #selected_data_dem()$rp.contact.field.survey_welcome_and_setup_completion_level <- factor(selected_data_dem()$rp.contact.field.survey_welcome_and_setup_completion_level)
      summary_plot(data = selected_data_dem(),
                   columns_to_summarise = "rp.contact.field.survey_welcome_and_setup_completion_level",
                   replace = "rp.contact.field.")
    }) 
    output$table_sv1_totals <- shiny::renderTable({(table_sv1_totals())}, striped = TRUE)
    output$plot_sv1_totals <- renderPlotly({plot_sv1_totals()})
    
    # sv1_attention a_1
    table_sv1_attention <- reactive({
      summary_table_survey_past_week()$Attention  }) 
    output$table_sv1_attention <- shiny::renderTable({(table_sv1_attention())}, striped = TRUE,
                                                     caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    plot_sv1_attention  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_welcome_ppf", replace = "rp.contact.field.")})
    output$plot_sv1_attention <- renderPlotly({plot_sv1_attention()})
    
    # sv1_praise a_2
    table_sv1_praise <- reactive({
      summary_table_survey_past_week()$Praise  }) 
    plot_sv1_praise  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_welcome_ppp", replace = "rp.contact.field.")})
    output$table_sv1_praise <- shiny::renderTable({(table_sv1_praise())}, striped = TRUE,
                                                  caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv1_praise <- renderPlotly({plot_sv1_praise()})
    
    # sv1_stress a_3
    table_sv1_stress <- reactive({
      summary_table_survey_past_week()$Stress  }) 
    plot_sv1_stress  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_welcome_a_3_final", replace = "rp.contact.field.")})
    output$table_sv1_stress <- shiny::renderTable({(table_sv1_stress())}, striped = TRUE,
                                                  caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv1_stress <- renderPlotly({plot_sv1_stress()})
    
    # sv1_shout a_4
    table_sv1_shout <- reactive({
      summary_table_survey_past_week()$Shouting  }) 
    plot_sv1_shout  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_welcome_a_4_final", replace = "rp.contact.field.")})
    output$table_sv1_shout <- shiny::renderTable({(table_sv1_shout())}, striped = TRUE,
                                                 caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv1_shout <- renderPlotly({plot_sv1_shout()})
    
    # sv1_money a_5 p1
    table_sv1_money <- reactive({
      summary_table_survey_past_week()$`Money worries`  }) 
    plot_sv1_money  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_welcome_fin_s", replace = "rp.contact.field.")})
    output$table_sv1_money <- shiny::renderTable({(table_sv1_money())}, striped = TRUE,
                                                 caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv1_money <- renderPlotly({plot_sv1_money()})
    
    # sv1_food_money a_5 
    table_sv1_food_money <- reactive({
      summary_table_survey_past_week()$Summary  }) 
    plot_sv1_food_money  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_welcome_fin_fi", replace = "rp.contact.field.")}) 
    output$table_sv1_food_money <- shiny::renderTable({(table_sv1_food_money())}, striped = TRUE,
                                                      caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv1_food_money <- renderPlotly({plot_sv1_food_money()})
    
    # sv1_hitting a_6
    table_sv1_hitting <- reactive({
      summary_table_survey_past_week()$Hitting  }) 
    plot_sv1_hitting  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_welcome_a_6_final", replace = "rp.contact.field.")}) 
    output$table_sv1_hitting <- shiny::renderTable({(table_sv1_hitting())}, striped = TRUE,
                                                   caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv1_hitting <- renderPlotly({plot_sv1_hitting()})
    
    # sv1_week_teen_activity a_7 p1
    table_sv1_week_teen_activity <- reactive({
      summary_table_survey_past_week()$`Teen activity`  }) 
    plot_sv1_week_teen_activity  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_welcome_a_7_part_1_final", replace = "rp.contact.field.")})
    output$table_sv1_week_teen_activity <- shiny::renderTable({(table_sv1_week_teen_activity())}, striped = TRUE,
                                                              caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv1_week_teen_activity <- renderPlotly({plot_sv1_week_teen_activity()})
    
    # sv1_lockdown a_7 p2
    table_sv1_lockdown <- reactive({
      summary_table_survey_past_week()$`Lockdown?`  }) 
    plot_sv1_lockdown  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_welcome_a_7_part_2_final", replace = "rp.contact.field.")})
    output$table_sv1_lockdown <- shiny::renderTable({(table_sv1_lockdown())}, striped = TRUE,
                                                    caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv1_lockdown <- renderPlotly({plot_sv1_lockdown()})
    
    # sv1_reg_teen_activity a_7 p3
    table_sv1_reg_teen_activity <- reactive({
      summary_table_survey_past_week()$`Knowledge of teen activity in non-lockdown week`  }) 
    plot_sv1_reg_teen_activity  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_welcome_a_7_part_3_final", replace = "rp.contact.field.")}) 
    output$table_sv1_reg_teen_activity <- shiny::renderTable({(table_sv1_reg_teen_activity())}, striped = TRUE,
                                                             caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv1_reg_teen_activity <- renderPlotly({plot_sv1_reg_teen_activity()})
    
    # sv1_sex_talk a_8
    table_sv1_sex_talk <- reactive({
      summary_table_survey_past_week()$`Sexual safety talk`  }) 
    plot_sv1_sex_talk  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_welcome_a_8_final", replace = "rp.contact.field.")})
    output$table_sv1_sex_talk <- shiny::renderTable({(table_sv1_sex_talk())}, striped = TRUE,
                                                    caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv1_sex_talk <- renderPlotly({plot_sv1_sex_talk()})
    
    # sv1_covid_safe a_9
    table_sv1_covid_safe <- reactive({
      summary_table_survey_past_week()$`Teen COVID safe`  }) 
    plot_sv1_covid_safe  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_welcome_a_9_final", replace = "rp.contact.field.")})
    output$table_sv1_covid_safe <- shiny::renderTable({(table_sv1_covid_safe())}, striped = TRUE,
                                                      caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv1_covid_safe <- renderPlotly({plot_sv1_covid_safe()})
    
    # Final pilot survey 
    # rp.contact.field.survey_final_completion_level # is it this variable?
    table_sv2_totals <- reactive({
      summary_table_survey_completion <- selected_data_dem() %>%
        summary_table(columns_to_summarise = "rp.contact.field.survey_final_completion_level", display = FALSE, include_margins = TRUE)
      summary_table_survey_completion <- summary_table_survey_completion %>%
        dplyr::filter(Org %in% unique(selected_data_dem()$Org))
      summary_table_survey_completion
    })
    plot_sv2_totals <- reactive({
      summary_plot(data = selected_data_dem(),
                   columns_to_summarise = "rp.contact.field.survey_final_completion_level",
                   replace = "rp.contact.field.")
    }) 
    output$table_sv2_totals <- shiny::renderTable({(table_sv2_totals())}, striped = TRUE)
    output$plot_sv2_totals <- renderPlotly({plot_sv2_totals()})
    
    summary_table_survey_final_build <- reactive({
      tabulate_with_metadata(data = selected_data_dem(), location_ID = "survey_final")
    })
    
    summary_table_survey_final <- reactive({
      summary_table_survey_final_build() %>% 
        purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
    })
    
    # sv2_attention a_1
    table_sv2_attention <- reactive({
      summary_table_survey_final()$Attention  }) 
    output$table_sv2_attention <- shiny::renderTable({(table_sv2_attention())}, striped = TRUE,
                                                     caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    
    # sv2_praise a_2
    table_sv2_praise <- reactive({
      summary_table_survey_final()$Praise  }) 
    plot_sv2_praise  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_final_ppp", replace = "rp.contact.field.")})
    output$table_sv2_praise <- shiny::renderTable({(table_sv2_praise())}, striped = TRUE,
                                                  caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv2_praise <- renderPlotly({plot_sv2_praise()})
    
    # sv2_stress a_3
    table_sv2_stress <- reactive({
      summary_table_survey_final()$Stress  }) 
    plot_sv2_stress  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_final_a_3_final", replace = "rp.contact.field.")})
    output$table_sv2_stress <- shiny::renderTable({(table_sv2_stress())}, striped = TRUE,
                                                  caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv2_stress <- renderPlotly({plot_sv2_stress()})
    
    # sv2_shout a_4
    table_sv2_shout <- reactive({
      summary_table_survey_final()$Shouting  }) 
    plot_sv2_shout  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_final_a_4_final", replace = "rp.contact.field.")})
    output$table_sv2_shout <- shiny::renderTable({(table_sv2_shout())}, striped = TRUE,
                                                 caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv2_shout <- renderPlotly({plot_sv2_shout()})
    
    # sv2_money a_5 p1
    table_sv2_money <- reactive({
      summary_table_survey_final()$`Money worries`  }) 
    plot_sv2_money  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_final_fin_s", replace = "rp.contact.field.")})
    output$table_sv2_money <- shiny::renderTable({(table_sv2_money())}, striped = TRUE,
                                                 caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv2_money <- renderPlotly({plot_sv2_money()})
    
    # sv2_food_money a_5 
    table_sv2_food_money <- reactive({
      summary_table_survey_final()$Summary  }) 
    plot_sv2_food_money  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_final_fin_fi", replace = "rp.contact.field.")}) 
    output$table_sv2_food_money <- shiny::renderTable({(table_sv2_food_money())}, striped = TRUE,
                                                      caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv2_food_money <- renderPlotly({plot_sv2_food_money()})
    
    # sv2_hitting a_6
    table_sv2_hitting <- reactive({
      summary_table_survey_final()$Hitting  }) 
    plot_sv2_hitting  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_final_a_6_final", replace = "rp.contact.field.")}) 
    output$table_sv2_hitting <- shiny::renderTable({(table_sv2_hitting())}, striped = TRUE,
                                                   caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv2_hitting <- renderPlotly({plot_sv2_hitting()})
    
    # sv2_week_teen_activity a_7 p1
    table_sv2_week_teen_activity <- reactive({
      summary_table_survey_final()$`Teen activity`  }) 
    plot_sv2_week_teen_activity  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_final_a_7_part_1_final", replace = "rp.contact.field.")})
    output$table_sv2_week_teen_activity <- shiny::renderTable({(table_sv2_week_teen_activity())}, striped = TRUE,
                                                              caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv2_week_teen_activity <- renderPlotly({plot_sv2_week_teen_activity()})
    
    # sv2_lockdown a_7 p2
    table_sv2_lockdown <- reactive({
      summary_table_survey_final()$`Lockdown?`  }) 
    plot_sv2_lockdown  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_final_a_7_part_2_final", replace = "rp.contact.field.")})
    output$table_sv2_lockdown <- shiny::renderTable({(table_sv2_lockdown())}, striped = TRUE,
                                                    caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv2_lockdown <- renderPlotly({plot_sv2_lockdown()})
    
    # sv2_reg_teen_activity a_7 p3
    table_sv2_reg_teen_activity <- reactive({
      summary_table_survey_final()$`Knowledge of teen activity in non-lockdown week`  }) 
    plot_sv2_reg_teen_activity  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_final_a_7_part_3_final", replace = "rp.contact.field.")}) 
    output$table_sv2_reg_teen_activity <- shiny::renderTable({(table_sv2_reg_teen_activity())}, striped = TRUE,
                                                             caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv2_reg_teen_activity <- renderPlotly({plot_sv2_reg_teen_activity()})
    
    # sv2_sex_talk a_8
    table_sv2_sex_talk <- reactive({
      summary_table_survey_final()$`Sexual safety talk`  }) 
    plot_sv2_sex_talk  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_final_a_8_final", replace = "rp.contact.field.")})
    output$table_sv2_sex_talk <- shiny::renderTable({(table_sv2_sex_talk())}, striped = TRUE,
                                                    caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv2_sex_talk <- renderPlotly({plot_sv2_sex_talk()})
    
    # sv2_covid_safe a_9
    table_sv2_covid_safe <- reactive({
      summary_table_survey_final()$`Teen COVID safe`  }) 
    plot_sv2_covid_safe  <- reactive({
      summary_plot(selected_data_dem(), "rp.contact.field.survey_final_a_9_final", replace = "rp.contact.field.")})
    output$table_sv2_covid_safe <- shiny::renderTable({(table_sv2_covid_safe())}, striped = TRUE,
                                                      caption = "no_value = selected 'choose not to answer'; null = skipped using the navigation buttons; NA = survey not accessed or data not synced")
    output$plot_sv2_covid_safe <- renderPlotly({plot_sv2_covid_safe()})
    
    #SIXTH Tab Parent Library
    summary_table_library <- reactive({
      summary_table_baseline_build <- summary_table_base_build(columns_to_summarise = data_library,
                                                               replace = "rp.contact.field.click_pc_",
                                                               replace_after = "count")
      data_library_neat <- naming_conventions(names(summary_table_library), replace = "Rp.contact.field.click hs")
      names(summary_table_baseline_build) <- data_library_neat
      if (country == "Tanzania"){
        if (study == "Pilot"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
                         janitor::adorn_totals("row"))
        } else if (study == "Optimisation"){
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>%
                         dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
                         dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
                         dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
                         janitor::adorn_totals("row"))
        } else {
          summary_table_baseline_build %>% 
            purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
        }
      } else {
        summary_table_baseline_build %>% 
          purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
        #janitor::adorn_totals("row"))
      }
    })
    
    #average clicks on parent library (mean per org)
    table_library_mean <- reactive({
      #mean library clicks (button type per organisation)
      #mean library clicks per workshop week is not stored to my knowledge
      summary_library_mean <- selected_data_dem() %>%
        group_by(Org)  %>%
        summarise(across(data_library, mean, na.rm = TRUE))
      colnames(summary_library_mean) <- naming_conventions(colnames(summary_library_mean), "rp.contact.field.click_", "_count")
      summary_library_mean
    })
    
    plot_library_mean  <- reactive({
      summary_library_mean_long <- pivot_longer(table_library_mean(),
                                                cols = !Org,
                                                names_to = "Library", values_to = "Clicks")
      ggplot(summary_library_mean_long, aes(x = Library , y = Clicks, fill = Org)) + 
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
    
  } #close server
  shinyApp(ui = ui, server = server)
} # close function
