# install.packages(c("shiny", "shinythemes", "shinyjs", "plotly", "shinydashboard", "httr", "jsonlite", "tidyverse"))

library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly)
library(shinydashboard)
library(httr)
library(jsonlite)
#library(here)          

# 1. Calling the data ---------------------------------------------------------------------
# not needed - data is just called plhdata_org_clean
data_baseline_survey <- c("rp.contact.field.survey_welcome_completed", "rp.contact.field.user_gender",
                          "rp.contact.field.user_age", "rp.contact.field.household_adults",
                          "rp.contact.field.household_teens", "rp.contact.field.household_babies",
                          "rp.contact.field.household_children", "rp.contact.field._app_language", "app_version", "rp.contact.field.do_workshops_together")
baseline_names_neat <- naming_conventions(data_baseline_survey, replace = "rp.contact.field.")

# 2. Data Cleaning -------------------------------------------------------------------------
# not needed - data is clean

# 3. Define UI -----------------------------------------------------------------------------
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "ParentApp Dashboard"),
                    
                    sidebar = dashboardSidebar(
                      sidebarMenu(
                        menuItem("Overview and Demographics", tabName = "demographics", icon = icon("users")),
                        menuItem("Workshop Engagement", tabName = "workshops", icon = icon("lightbulb")),
                        menuItem("Parent Points", tabName = "parentpoints", icon = icon("star")),
                        menuItem("In-week Engagement", tabName = "xtraengagement", icon = icon("user-check")),
                        menuItem("Surveys", tabName = "surveys", icon = icon("question")),
                        menuItem("Parent Library", tabName = "library", icon = icon("book-reader"))
                      )), #closes sidebarMenu and dashboardSidebar
                    
                    dashboardBody(  # Boxes need to be put in a row (or column)
                      fluidRow(
                        shinydashboard::valueBoxOutput("myvaluebox1", width=2), 
                        shinydashboard::valueBoxOutput("myvaluebox2", width=2),
                        shinydashboard::valueBoxOutput("myvaluebox3", width=2),
                        shinydashboard::valueBoxOutput("myvaluebox4", width=2),
                        shinydashboard::valueBoxOutput("myvaluebox5", width=2)
                      ), #closes fluidRow
                      
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
                                  box(width = 6,
                                      checkboxGroupInput(inputId = "CtryDem",
                                                         label = "Countries to select:",
                                                         choices = c("South Africa" = "plh_za",
                                                                     "Tanzania" = "plh_tz"),
                                                         selected = c("plh_za","plh_tz")
                                      )),
                                  box(width = 6,
                                      checkboxGroupInput(inputId = "OrgDem",
                                                         label = "Organisations to show:",
                                                         choices = c("SA: Amathuba" = "Amathuba",
                                                                     "SA: Dlalanathi" = "Dlalanathi",
                                                                     "SA: Joy" = "Joy",
                                                                     "SA: Nontobeko" = "Nontobeko", "TZ: ICS" ="ICS"),
                                                         selected = c("Amathuba","Dlalanathi",
                                                                      "Joy","Nontobeko", "ICS")
                                      ))), #closes fluidRow
                                
                                fluidRow(
                                  box(width = 12,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "App first downloaded and opened",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_app_launch", height = "240"),
                                      shiny::tableOutput("table_app_launch")
                                  )#closes box
                                ), #closes fluid row
                                
                                fluidRow(
                                  box(width = 4,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Language",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_language", height = "240"), #generates graph
                                      shiny::tableOutput("table_language")  #generates table
                                  ), #closes box
                                  
                                  box(width = 4,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop format",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_ws_format", height = "240"),
                                      shiny::tableOutput("table_ws_format")
                                  ), #closes box
                                  
                                  box(width = 4,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "App version",
                                      status = "primary", # primary, success, info, warning, danger
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_app_version", height = "240"),
                                      shiny::tableOutput("table_app_version")
                                  ) #closes box
                                ), #closes fluidRow
                                
                                fluidRow(
                                  box(width = 4,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Parent gender",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_parent_gender", height = "240"), #generates graph
                                      shiny::tableOutput("table_parent_gender")  #generates table
                                  ), #closes box
                                  
                                  box(width = 4,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Parent age",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_parent_age", height = "240"),
                                      shiny::tableOutput("table_parent_age")
                                  ), #closes box
                                  
                                  box(width = 4,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Adults in household",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_household_adults", height = "240"),
                                      shiny::tableOutput("table_household_adults")
                                  ) #closes box
                                ), #closes fluidRow
                                
                                fluidRow(
                                  box(width = 4,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Teens in household",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_household_teens", height = "240"),
                                      shiny::tableOutput("table_household_teens")
                                  ), #closes box
                                  
                                  box(width = 4,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Children in household",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_household_children", height = "240"),
                                      shiny::tableOutput("table_household_children")
                                  ), #closes box
                                  
                                  box(width = 4,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Babies in household",
                                      status = "primary",  
                                      #background = "orange",
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
                                
                                fluidRow(
                                  box(width = 6,
                                      checkboxGroupInput(inputId = "CtryWS",
                                                         label = "Countries to select:",
                                                         choices = c("South Africa" = "plh_za",
                                                                     "Tanzania" = "plh_tz"),
                                                         selected = c("plh_za","plh_tz")
                                      )),
                                  box(width = 6,
                                      checkboxGroupInput(inputId = "OrgWS",
                                                         label = "Organisations to show:",
                                                         choices = c("SA: Amathuba" = "Amathuba",
                                                                     "SA: Dlalanathi" = "Dlalanathi",
                                                                     "SA: Joy" = "Joy",
                                                                     "SA: Nontobeko" = "Nontobeko", "TZ: ICS" ="ICS"),
                                                         selected = c("Amathuba","Dlalanathi",
                                                                      "Joy","Nontobeko", "ICS")
                                      ))), #closes fluidRow
                                
                                tabsetPanel(type = "tabs",
                                            tabPanel("Overview",
                                
                                fluidRow(
                                  box(width = 12,
                                      collapsible = FALSE,
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
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 1: Welcome and Self-Care",
                                      status = "info",
                                      h5("As individual: percentage out of 9 steppers; <br> As group: not an option for first workshop"),
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_self_care", height = "240"),
                                      shiny::tableOutput("table_w_self_care")
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 2: One-on-One Time",
                                      status = "info",  
                                      h5("As individual: percentage out of 7 steppers; <br> As group: percentage out of 9 steppers"),
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_1on1", height = "240"),
                                      shiny::tableOutput("table_w_1on1")
                                  ) #closes box
                                ), #closes fluid row
                                
                                fluidRow(
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 3: Praise",
                                      status = "info",  
                                      h5("As individual: percentage out of 9 steppers <br> As group: percentage out of 11 steppers"),
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_praise", height = "240"),
                                      shiny::tableOutput("table_w_praise")
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 4: Positive Instructions",
                                      status = "info",  
                                      h5("As individual: percentage out of 11 steppers <br> As group: percentage out of 13 steppers"),
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_instruct", height = "240"),
                                      shiny::tableOutput("table_w_instruct")
                                  ) #closes box
                                ), #closes fluid row
                                
                                fluidRow(
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 5: Managing Stress",
                                      status = "info",
                                      h5("As individual: percentage out of 10 steppers <br> As group: percentage out of 11 steppers"),
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_stress", height = "240"),
                                      shiny::tableOutput("table_w_stress")
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 6: Family Budgets",
                                      status = "info",  
                                      h5("As individual: percentage out of 18 steppers <br> As group: percentage out of 18 steppers"),
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_money", height = "240"),
                                      shiny::tableOutput("table_w_money")
                                  ) #closes box
                                ), #closes fluid row
                                
                                fluidRow(
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 7: Rules",
                                      status = "info",  
                                      h5("As individual: percentage out of 10 steppers <br> As group: npercentage out of 11 steppers"),
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_rules", height = "240"),
                                      shiny::tableOutput("table_w_rules")
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 8: Calm Consequences",
                                      status = "info",  
                                      h5("As individual: percentage out of 12 steppers <br> As group: npercentage out of 14 steppers"),
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_consequence", height = "240"),
                                      shiny::tableOutput("table_w_consequence")
                                  ) #closes box
                                ), #closes fluid row
                                
                                fluidRow(
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 9: Problem Solving",
                                      status = "info",  
                                      h5("As individual: percentage out of 10 steppers <br> As group: npercentage out of 12 steppers"),
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_solve", height = "240"),
                                      shiny::tableOutput("table_w_solve")
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 10: Teen Safety",
                                      status = "info",  
                                      h5("As individual: percentage out of 11 steppers <br> As group: npercentage out of 12 steppers"),
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_safe", height = "240"),
                                      shiny::tableOutput("table_w_safe")
                                  ) #closes box
                                ), #closes fluid row
                                
                                fluidRow(
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 11: Dealing with Crisis",
                                      status = "info",  
                                      h5("As individual: percentage out of 11 steppers <br> As group: npercentage out of 12 steppers"),
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_crisis", height = "240"),
                                      shiny::tableOutput("table_w_crisis")
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 12: Celebration and Next Steps",
                                      status = "info",  
                                      h5("As individual: percentage out of 6 steppers <br> As group: npercentage out of 9 steppers"),
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_celebrate", height = "240"),
                                      shiny::tableOutput("table_w_celebrate")
                                  ) #closes box
                                ) #closes fluid row
                                            ), # closes Overview tabPanel
                                
                                tabPanel("Additional Insights",
                                         
                                         fluidRow(
                                           box(width = 12,
                                               collapsible = FALSE,
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
                                               collapsible = FALSE,
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
                                
                                fluidRow(
                                  box(width = 4,
                                      checkboxGroupInput(inputId = "CtryPP",
                                                         label = "Countries to select:",
                                                         choices = c("South Africa" = "plh_za",
                                                                     "Tanzania" = "plh_tz"),
                                                         selected = c("plh_za","plh_tz")
                                      )),
                                  box(width = 4,
                                      checkboxGroupInput(inputId = "OrgPP",
                                                         label = "Organisations to show:",
                                                         choices = c("SA: Amathuba" = "Amathuba",
                                                                     "SA: Dlalanathi" = "Dlalanathi",
                                                                     "SA: Joy" = "Joy",
                                                                     "SA: Nontobeko" = "Nontobeko", "TZ: ICS" ="ICS"),
                                                         selected = c("Amathuba","Dlalanathi",
                                                                      "Joy","Nontobeko", "ICS")
                                      )),
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
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Parent points: Relax",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_relax", height = "240"),
                                                           shiny::tableOutput("table_pp_relax")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Parent points: Treat yourself well",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_treat_yourself", height = "240"),
                                                           shiny::tableOutput("table_pp_treat_yourself")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Parent points: Praise yourself",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_yourself", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_yourself")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Parent points: One-on-one Time",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_spend_time", height = "240"),
                                                           shiny::tableOutput("table_pp_spend_time")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Parent points: Praise your teen",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_teen", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_teen")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Parent points: Get positive",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_instruct_positively", height = "240"),
                                                           shiny::tableOutput("table_pp_instruct_positively")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Parent points: Breathe not yell",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_breathe", height = "240"),
                                                           shiny::tableOutput("table_pp_breathe")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Parent points: Good money choice",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_money", height = "240"),
                                                           shiny::tableOutput("table_pp_money")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Parent points: Calm consequence",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_consequence", height = "240"),
                                                           shiny::tableOutput("table_pp_consequence")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Parent points: Safe",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_safe", height = "240"),
                                                           shiny::tableOutput("table_pp_safe")
                                                       ) #closes box
                                                     ) #closes fluid row
                                            ), #closes and tab panel
                                            
                                            tabPanel("Relax", #pp1
                                                     
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Relax Points in Workshop 1: Welcome and Self-Care",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_relax_w_self_care", height = "240"),
                                                           shiny::tableOutput("table_pp_relax_w_self_care")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Relax Points in Workshop 2: One-on-One Time",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_relax_w_1on1", height = "240"),
                                                           shiny::tableOutput("table_pp_relax_w_1on1")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Relax Points in Workshop 3: Praise",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_relax_w_praise", height = "240"),
                                                           shiny::tableOutput("table_pp_relax_w_praise")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Relax Points in Workshop 4: Positive Instructions",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_relax_w_instruct", height = "240"),
                                                           shiny::tableOutput("table_pp_relax_w_instruct")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Relax Points in Workshop 5: Managing Stress",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_relax_w_stress", height = "240"),
                                                           shiny::tableOutput("table_pp_relax_w_stress")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Relax Points in Workshop 6: Family Budgets",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_relax_w_money", height = "240"),
                                                           shiny::tableOutput("table_pp_relax_w_money")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Relax Points in Workshop 7: Rules",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_relax_w_rules", height = "240"),
                                                           shiny::tableOutput("table_pp_relax_w_rules")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Relax Points in Workshop 8: Calm Consequences",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_relax_w_consequence", height = "240"),
                                                           shiny::tableOutput("table_pp_relax_w_consequence")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Relax Points in Workshop 9: Problem Solving",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_relax_w_solve", height = "240"),
                                                           shiny::tableOutput("table_pp_relax_w_solve")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Relax Points in Workshop 10: Teen Safety",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_relax_w_safe", height = "240"),
                                                           shiny::tableOutput("table_pp_relax_w_safe")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Relax Points in Workshop 11: Dealing with Crisis",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_relax_w_crisis", height = "240"),
                                                           shiny::tableOutput("table_pp_relax_w_crisis")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Relax Points in Workshop 12: Celebration and Next Steps",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_relax_w_celebrate", height = "240"),
                                                           shiny::tableOutput("table_pp_relax_w_celebrate")
                                                       ) #closes box
                                                     ) #closes fluid row
                                                     
                                            ), #closes tab panel pp1 relax
                                            
                                            tabPanel("Treat Self", #pp2
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Treat Yourself Points in Workshop 1: Welcome and Self-Care",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_self_care", height = "240"),
                                                           shiny::tableOutput("table_pp_treat_yourself_w_self_care")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Treat Yourself Points in Workshop 2: One-on-One Time",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_1on1", height = "240"),
                                                           shiny::tableOutput("table_pp_treat_yourself_w_1on1")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Treat Yourself Points in Workshop 3: Praise",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_praise", height = "240"),
                                                           shiny::tableOutput("table_pp_treat_yourself_w_praise")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Treat Yourself Points in Workshop 4: Positive Instructions",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_instruct", height = "240"),
                                                           shiny::tableOutput("table_pp_treat_yourself_w_instruct")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Treat Yourself Points in Workshop 5: Managing Stress",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_stress", height = "240"),
                                                           shiny::tableOutput("table_pp_treat_yourself_w_stress")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Treat Yourself Points in Workshop 6: Family Budgets",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_money", height = "240"),
                                                           shiny::tableOutput("table_pp_treat_yourself_w_money")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Treat Yourself Points in Workshop 7: Rules",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_rules", height = "240"),
                                                           shiny::tableOutput("table_pp_treat_yourself_w_rules")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Treat Yourself Points in Workshop 8: Calm Consequences",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_consequence", height = "240"),
                                                           shiny::tableOutput("table_pp_treat_yourself_w_consequence")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Treat Yourself Points in Workshop 9: Problem Solving",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_solve", height = "240"),
                                                           shiny::tableOutput("table_pp_treat_yourself_w_solve")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Treat Yourself Points in Workshop 10: Teen Safety",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_safe", height = "240"),
                                                           shiny::tableOutput("table_pp_treat_yourself_w_safe")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Treat Yourself Points in Workshop 11: Dealing with Crisis",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_crisis", height = "240"),
                                                           shiny::tableOutput("table_pp_treat_yourself_w_crisis")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Treat Yourself Points in Workshop 12: Celebration and Next Steps",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_treat_yourself_w_celebrate", height = "240"),
                                                           shiny::tableOutput("table_pp_treat_yourself_w_celebrate")
                                                       ) #closes box
                                                     ) #closes fluid row
                                                     
                                            ), #closes tab panel pp2 treat yourself well
                                            
                                            tabPanel("Praise Self", #pp3 praise yourself
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Yourself Points in Workshop 1: Welcome and Self-Care",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_self_care", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_yourself_w_self_care")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Yourself Points in Workshop 2: One-on-One Time",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_1on1", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_yourself_w_1on1")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Yourself Points in Workshop 3: Praise",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_praise", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_yourself_w_praise")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Yourself Points in Workshop 4: Positive Instructions",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_instruct", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_yourself_w_instruct")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Yourself Points in Workshop 5: Managing Stress",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_stress", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_yourself_w_stress")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Yourself Points in Workshop 6: Family Budgets",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_money", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_yourself_w_money")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Yourself Points in Workshop 7: Rules",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_rules", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_yourself_w_rules")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Yourself Points in Workshop 8: Calm Consequences",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_consequence", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_yourself_w_consequence")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Yourself Points in Workshop 9: Problem Solving",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_solve", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_yourself_w_solve")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Yourself Points in Workshop 10: Teen Safety",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_safe", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_yourself_w_safe")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Yourself Points in Workshop 11: Dealing with Crisis",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_crisis", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_yourself_w_crisis")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Yourself Points in Workshop 12: Celebration and Next Steps",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_yourself_w_celebrate", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_yourself_w_celebrate")
                                                       ) #closes box
                                                     ) #closes fluid row
                                                  ), #closes tab panel pp3 praise yourself
                                            
                                            tabPanel("1-on-1",  #pp4 spend time together
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "1-on-1 Time Points in Workshop 1: Welcome and Self-Care",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_spend_time_w_self_care", height = "240"),
                                                           shiny::tableOutput("table_pp_spend_time_w_self_care")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "1-on-1 Time Points in Workshop 2: One-on-One Time",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_spend_time_w_1on1", height = "240"),
                                                           shiny::tableOutput("table_pp_spend_time_w_1on1")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "1-on-1 Time Points in Workshop 3: Praise",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_spend_time_w_praise", height = "240"),
                                                           shiny::tableOutput("table_pp_spend_time_w_praise")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "1-on-1 Time Points in Workshop 4: Positive Instructions",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_spend_time_w_instruct", height = "240"),
                                                           shiny::tableOutput("table_pp_spend_time_w_instruct")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "1-on-1 Time Points in Workshop 5: Managing Stress",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_spend_time_w_stress", height = "240"),
                                                           shiny::tableOutput("table_pp_spend_time_w_stress")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "1-on-1 Time Points in Workshop 6: Family Budgets",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_spend_time_w_money", height = "240"),
                                                           shiny::tableOutput("table_pp_spend_time_w_money")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "1-on-1 Time Points in Workshop 7: Rules",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_spend_time_w_rules", height = "240"),
                                                           shiny::tableOutput("table_pp_spend_time_w_rules")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "1-on-1 Time Points in Workshop 8: Calm Consequences",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_spend_time_w_consequence", height = "240"),
                                                           shiny::tableOutput("table_pp_spend_time_w_consequence")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "1-on-1 Time Points in Workshop 9: Problem Solving",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_spend_time_w_solve", height = "240"),
                                                           shiny::tableOutput("table_pp_spend_time_w_solve")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "1-on-1 Time Points in Workshop 10: Teen Safety",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_spend_time_w_safe", height = "240"),
                                                           shiny::tableOutput("table_pp_spend_time_w_safe")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "1-on-1 Time Points in Workshop 11: Dealing with Crisis",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_spend_time_w_crisis", height = "240"),
                                                           shiny::tableOutput("table_pp_spend_time_w_crisis")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "1-on-1 Time Points in Workshop 12: Celebration and Next Steps",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_spend_time_w_celebrate", height = "240"),
                                                           shiny::tableOutput("table_pp_spend_time_w_celebrate")
                                                       ) #closes box
                                                     ) #closes fluid row  
                                            ), #closes tab panel pp4 spend time together
                                            
                                            tabPanel("Praise Teen", #pp5
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Teen Points in Workshop 1: Welcome and Self-Care",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_self_care", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_teen_w_self_care")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Teen Points in Workshop 2: One-on-One Time",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_1on1", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_teen_w_1on1")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Teen Points in Workshop 3: Praise",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_praise", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_teen_w_praise")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Teen Points in Workshop 4: Positive Instructions",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_instruct", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_teen_w_instruct")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Teen Points in Workshop 5: Managing Stress",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_stress", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_teen_w_stress")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Teen Points in Workshop 6: Family Budgets",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_money", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_teen_w_money")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Teen Points in Workshop 7: Rules",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_rules", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_teen_w_rules")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Teen Points in Workshop 8: Calm Consequences",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_consequence", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_teen_w_consequence")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Teen Points in Workshop 9: Problem Solving",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_solve", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_teen_w_solve")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Teen Points in Workshop 10: Teen Safety",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_safe", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_teen_w_safe")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Teen Points in Workshop 11: Dealing with Crisis",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_crisis", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_teen_w_crisis")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Praise Teen Points in Workshop 12: Celebration and Next Steps",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_praise_teen_w_celebrate", height = "240"),
                                                           shiny::tableOutput("table_pp_praise_teen_w_celebrate")
                                                       ) #closes box
                                                     ) #closes fluid row      
                                            ), #closes tab panel pp5 praise teen
                                            
                                            tabPanel("Positive", #pp6 get positive
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Get Positive Points in Workshop 1: Welcome and Self-Care",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_self_care", height = "240"),
                                                           shiny::tableOutput("table_pp_instruct_positively_w_self_care")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Get Positive Points in Workshop 2: One-on-One Time",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_1on1", height = "240"),
                                                           shiny::tableOutput("table_pp_instruct_positively_w_1on1")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Get Positive Points in Workshop 3: Praise",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_praise", height = "240"),
                                                           shiny::tableOutput("table_pp_instruct_positively_w_praise")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Get Positive Points in Workshop 4: Positive Instructions",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_instruct", height = "240"),
                                                           shiny::tableOutput("table_pp_instruct_positively_w_instruct")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Get Positive Points in Workshop 5: Managing Stress",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_stress", height = "240"),
                                                           shiny::tableOutput("table_pp_instruct_positively_w_stress")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Get Positive Points in Workshop 6: Family Budgets",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_money", height = "240"),
                                                           shiny::tableOutput("table_pp_instruct_positively_w_money")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Get Positive Points in Workshop 7: Rules",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_rules", height = "240"),
                                                           shiny::tableOutput("table_pp_instruct_positively_w_rules")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Get Positive Points in Workshop 8: Calm Consequences",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_consequence", height = "240"),
                                                           shiny::tableOutput("table_pp_instruct_positively_w_consequence")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Get Positive Points in Workshop 9: Problem Solving",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_solve", height = "240"),
                                                           shiny::tableOutput("table_pp_instruct_positively_w_solve")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Get Positive Points in Workshop 10: Teen Safety",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_safe", height = "240"),
                                                           shiny::tableOutput("table_pp_instruct_positively_w_safe")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Get Positive Points in Workshop 11: Dealing with Crisis",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_crisis", height = "240"),
                                                           shiny::tableOutput("table_pp_instruct_positively_w_crisis")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Get Positive Points in Workshop 12: Celebration and Next Steps",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_instruct_positively_w_celebrate", height = "240"),
                                                           shiny::tableOutput("table_pp_instruct_positively_w_celebrate")
                                                       ) #closes box
                                                     ) #closes fluid row
                                            ), #closes tab panel pp6 get positive
                                            
                                            tabPanel("Breathe", #pp7 breathe not yell
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Breathe Points in Workshop 1: Welcome and Self-Care",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_breathe_w_self_care", height = "240"),
                                                           shiny::tableOutput("table_pp_breathe_w_self_care")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Breathe Points in Workshop 2: One-on-One Time",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_breathe_w_1on1", height = "240"),
                                                           shiny::tableOutput("table_pp_breathe_w_1on1")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Breathe Points in Workshop 3: Praise",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_breathe_w_praise", height = "240"),
                                                           shiny::tableOutput("table_pp_breathe_w_praise")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Breathe Points in Workshop 4: Positive Instructions",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_breathe_w_instruct", height = "240"),
                                                           shiny::tableOutput("table_pp_breathe_w_instruct")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Breathe Points in Workshop 5: Managing Stress",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_breathe_w_stress", height = "240"),
                                                           shiny::tableOutput("table_pp_breathe_w_stress")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Breathe Points in Workshop 6: Family Budgets",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_breathe_w_money", height = "240"),
                                                           shiny::tableOutput("table_pp_breathe_w_money")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Breathe Points in Workshop 7: Rules",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_breathe_w_rules", height = "240"),
                                                           shiny::tableOutput("table_pp_breathe_w_rules")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Breathe Points in Workshop 8: Calm Consequences",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_breathe_w_consequence", height = "240"),
                                                           shiny::tableOutput("table_pp_breathe_w_consequence")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Breathe Points in Workshop 9: Problem Solving",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_breathe_w_solve", height = "240"),
                                                           shiny::tableOutput("table_pp_breathe_w_solve")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Breathe Points in Workshop 10: Teen Safety",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_breathe_w_safe", height = "240"),
                                                           shiny::tableOutput("table_pp_breathe_w_safe")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Breathe Points in Workshop 11: Dealing with Crisis",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_breathe_w_crisis", height = "240"),
                                                           shiny::tableOutput("table_pp_breathe_w_crisis")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Breathe Points in Workshop 12: Celebration and Next Steps",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_breathe_w_celebrate", height = "240"),
                                                           shiny::tableOutput("table_pp_breathe_w_celebrate")
                                                       ) #closes box
                                                     ) #closes fluid row
                                            ), #closes tab panel pp7 breathe not yell
                                            
                                            tabPanel("Money", #pp8 good money choice
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Good Money Choice Points in Workshop 1: Welcome and Self-Care",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_money_w_self_care", height = "240"),
                                                           shiny::tableOutput("table_pp_money_w_self_care")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Good Money Choice Points in Workshop 2: One-on-One Time",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_money_w_1on1", height = "240"),
                                                           shiny::tableOutput("table_pp_money_w_1on1")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Good Money Choice Points in Workshop 3: Praise",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_money_w_praise", height = "240"),
                                                           shiny::tableOutput("table_pp_money_w_praise")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Good Money Choice Points in Workshop 4: Positive Instructions",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_money_w_instruct", height = "240"),
                                                           shiny::tableOutput("table_pp_money_w_instruct")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Good Money Choice Points in Workshop 5: Managing Stress",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_money_w_stress", height = "240"),
                                                           shiny::tableOutput("table_pp_money_w_stress")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Good Money Choice Points in Workshop 6: Family Budgets",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_money_w_money", height = "240"),
                                                           shiny::tableOutput("table_pp_money_w_money")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Good Money Choice Points in Workshop 7: Rules",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_money_w_rules", height = "240"),
                                                           shiny::tableOutput("table_pp_money_w_rules")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Good Money Choice Points in Workshop 8: Calm Consequences",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_money_w_consequence", height = "240"),
                                                           shiny::tableOutput("table_pp_money_w_consequence")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Good Money Choice Points in Workshop 9: Problem Solving",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_money_w_solve", height = "240"),
                                                           shiny::tableOutput("table_pp_money_w_solve")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Good Money Choice Points in Workshop 10: Teen Safety",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_money_w_safe", height = "240"),
                                                           shiny::tableOutput("table_pp_money_w_safe")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Good Money Choice Points in Workshop 11: Dealing with Crisis",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_money_w_crisis", height = "240"),
                                                           shiny::tableOutput("table_pp_money_w_crisis")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Good Money Choice Points in Workshop 12: Celebration and Next Steps",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_money_w_celebrate", height = "240"),
                                                           shiny::tableOutput("table_pp_money_w_celebrate")
                                                       ) #closes box
                                                     ) #closes fluid row
                                            ), #closes tab panel pp8 good money choice
                                            
                                            tabPanel("Consequence", #pp9 calm consequence
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Calm Consequence Points in Workshop 1: Welcome and Self-Care",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_consequence_w_self_care", height = "240"),
                                                           shiny::tableOutput("table_pp_consequence_w_self_care")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Calm Consequence Points in Workshop 2: One-on-One Time",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_consequence_w_1on1", height = "240"),
                                                           shiny::tableOutput("table_pp_consequence_w_1on1")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Calm Consequence Points in Workshop 3: Praise",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_consequence_w_praise", height = "240"),
                                                           shiny::tableOutput("table_pp_consequence_w_praise")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Calm Consequence Points in Workshop 4: Positive Instructions",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_consequence_w_instruct", height = "240"),
                                                           shiny::tableOutput("table_pp_consequence_w_instruct")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Calm Consequence Points in Workshop 5: Managing Stress",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_consequence_w_stress", height = "240"),
                                                           shiny::tableOutput("table_pp_consequence_w_stress")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Calm Consequence Points in Workshop 6: Family Budgets",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_consequence_w_money", height = "240"),
                                                           shiny::tableOutput("table_pp_consequence_w_money")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Calm Consequence Points in Workshop 7: Rules",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_consequence_w_rules", height = "240"),
                                                           shiny::tableOutput("table_pp_consequence_w_rules")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Calm Consequence Points in Workshop 8: Calm Consequences",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_consequence_w_consequence", height = "240"),
                                                           shiny::tableOutput("table_pp_consequence_w_consequence")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Calm Consequence Points in Workshop 9: Problem Solving",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_consequence_w_solve", height = "240"),
                                                           shiny::tableOutput("table_pp_consequence_w_solve")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Calm Consequence Points in Workshop 10: Teen Safety",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_consequence_w_safe", height = "240"),
                                                           shiny::tableOutput("table_pp_consequence_w_safe")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Calm Consequence Points in Workshop 11: Dealing with Crisis",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_consequence_w_crisis", height = "240"),
                                                           shiny::tableOutput("table_pp_consequence_w_crisis")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Calm Consequence Points in Workshop 12: Celebration and Next Steps",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_consequence_w_celebrate", height = "240"),
                                                           shiny::tableOutput("table_pp_consequence_w_celebrate")
                                                       ) #closes box
                                                     ) #closes fluid row
                                            ), #closes tab panel pp9 calm consequence
                                            
                                            tabPanel("Safe",  #pp10 safe
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Safe Points in Workshop 1: Welcome and Self-Care",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_safe_w_self_care", height = "240"),
                                                           shiny::tableOutput("table_pp_safe_w_self_care")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Safe Points in Workshop 2: One-on-One Time",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_safe_w_1on1", height = "240"),
                                                           shiny::tableOutput("table_pp_safe_w_1on1")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Safe Points in Workshop 3: Praise",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_safe_w_praise", height = "240"),
                                                           shiny::tableOutput("table_pp_safe_w_praise")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Safe Points in Workshop 4: Positive Instructions",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_safe_w_instruct", height = "240"),
                                                           shiny::tableOutput("table_pp_safe_w_instruct")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Safe Points in Workshop 5: Managing Stress",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_safe_w_stress", height = "240"),
                                                           shiny::tableOutput("table_pp_safe_w_stress")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Safe Points in Workshop 6: Family Budgets",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_safe_w_money", height = "240"),
                                                           shiny::tableOutput("table_pp_safe_w_money")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Safe Points in Workshop 7: Rules",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_safe_w_rules", height = "240"),
                                                           shiny::tableOutput("table_pp_safe_w_rules")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Safe Points in Workshop 8: Calm Consequences",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_safe_w_consequence", height = "240"),
                                                           shiny::tableOutput("table_pp_safe_w_consequence")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Safe Points in Workshop 9: Problem Solving",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_safe_w_solve", height = "240"),
                                                           shiny::tableOutput("table_pp_safe_w_solve")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Safe Points in Workshop 10: Teen Safety",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_safe_w_safe", height = "240"),
                                                           shiny::tableOutput("table_pp_safe_w_safe")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Safe Points in Workshop 11: Dealing with Crisis",
                                                           status = "warning",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pp_safe_w_crisis", height = "240"),
                                                           shiny::tableOutput("table_pp_safe_w_crisis")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Safe Points in Workshop 12: Celebration and Next Steps",
                                                           status = "warning",  
                                                           #background = "orange",
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
                                
                                fluidRow(
                                  box(width = 4,
                                      checkboxGroupInput(inputId = "CtryXE",
                                                         label = "Countries to select:",
                                                         choices = c("South Africa" = "plh_za",
                                                                     "Tanzania" = "plh_tz"),
                                                         selected = c("plh_za","plh_tz")
                                      )),
                                  box(width = 4,
                                      checkboxGroupInput(inputId = "OrgXE",
                                                         label = "Organisations to show:",
                                                         choices = c("SA: Amathuba" = "Amathuba",
                                                                     "SA: Dlalanathi" = "Dlalanathi",
                                                                     "SA: Joy" = "Joy",
                                                                     "SA: Nontobeko" = "Nontobeko", "TZ: ICS" ="ICS"),
                                                         selected = c("Amathuba","Dlalanathi",
                                                                      "Joy","Nontobeko", "ICS")
                                      )),
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
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Mean push notification clicks",
                                                           status = "success",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pushn_mean", height = "240"),
                                                           shiny::tableOutput("table_pushn_mean")
                                                       ) #closes box
                                                       ), #closes fluidrow
                                                     
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Cumulative pushnotifiation clicks",
                                                           status = "success",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_pushn_totals", height = "240"),
                                                           shiny::tableOutput("table_pushn_totals")
                                                       )#closes box
                                                     ) #closes fluid row
                                            ), #closes tabPanel Push Notifications
                                            
                                            tabPanel("Home Practice",
                                                     
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
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           checkboxGroupInput(inputId = "CtrySV1",
                                                                              label = "Countries to select:",
                                                                              choices = c("South Africa" = "plh_za",
                                                                                          "Tanzania" = "plh_tz"),
                                                                              selected = c("plh_za","plh_tz")
                                                           )),
                                                       box(width = 6,
                                                           checkboxGroupInput(inputId = "OrgSV1",
                                                                              label = "Organisations to show:",
                                                                              choices = c("SA: Amathuba" = "Amathuba",
                                                                                          "SA: Dlalanathi" = "Dlalanathi",
                                                                                          "SA: Joy" = "Joy",
                                                                                          "SA: Nontobeko" = "Nontobeko", "TZ: ICS" ="ICS"),
                                                                              selected = c("Amathuba","Dlalanathi",
                                                                                           "Joy","Nontobeko", "ICS")
                                                           ))), #closes fluidRow
                                                     
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
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
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days of attention",
                                                           status = "danger",  
                                                           h5("How many days in the past week were you able to give them your attention and do something that they enjoyed with them? [0-7]"),
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_attention", height = "240"),
                                                           shiny::tableOutput("table_sv1_attention")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days of praise",
                                                           status = "danger",
                                                           h5("How many days in the past week have you praised your teen? [0-7]"),
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_praise", height = "240"),
                                                           shiny::tableOutput("table_sv1_praise")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days of stress",
                                                           status = "danger",  
                                                           h5("How many days in the past week did you feel very stressed as a parent/caregiver? [0-7]"),
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_stress", height = "240"),
                                                           shiny::tableOutput("table_sv1_stress")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days of shouting",
                                                           status = "danger",
                                                           h5("How many days in the past week did you shout, scream or yell at your teen? [0-7]"),
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_shout", height = "240"),
                                                           shiny::tableOutput("table_sv1_shout")
                                                       ) #closes box
                                                     ),#closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days of money worries",
                                                           status = "danger",  
                                                           h5("How many days in the past week have you worried or felt anxious about money? [0-7]"),
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_money", height = "240"),
                                                           shiny::tableOutput("table_sv1_money")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days out of food money (last month)",
                                                           status = "danger",  
                                                           h5("How many days in the past month (30 days) did you run out of money to pay for food? [0-30]"),
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_food_money", height = "240"),
                                                           shiny::tableOutput("table_sv1_food_money")
                                                       ) #closes box
                                                     ),#closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days of hitting",
                                                           status = "danger", 
                                                           h5("In the past week, did you physically discipline your children by hitting, spanking, or slapping with your hand or an object like a stick or a belt? [0-7]"),
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_hitting", height = "240"),
                                                           shiny::tableOutput("table_sv1_hitting")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days with knowledge of teen activity",
                                                           status = "danger", 
                                                           h5("How many days in the past week did you know what your teen was doing and who they were with? [0-7]"),
                                                           #h7("NB next question only asked if this one is answered with '7'"),
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_week_teen_activity", height = "240"),
                                                           shiny::tableOutput("table_sv1_week_teen_activity")
                                                       ) #closes box
                                                     ),#closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Lockdown",
                                                           status = "danger",
                                                           h5("Were you in lockdown this week? [Yes/No]"),
                                                           #h7("NB next question only asked if this one is answered with 'Yes'"),
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_lockdown", height = "240"),
                                                           shiny::tableOutput("table_sv1_lockdown")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Non-lockdown knowledge of teen activity",
                                                           status = "danger",
                                                           h5("How many days in a typical non-lockdown week do you know what your teen is doing and who they are with? [0-7]"),
                                                           #h7("NB this question is only asked if the parent said they had knowledge 7/7 days and then indicated that it was a lockdown week"),
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_reg_teen_activity", height = "240"),
                                                           shiny::tableOutput("table_sv1_reg_teen_activity")
                                                       ) #closes box
                                                     ), #closes fluid row
                                                     
                                                     fluidRow(
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days of sexual safety talk (last month)",
                                                           status = "danger",  
                                                           h5("How many days in the past month (30 days) did you talk with your teen about keeping safe from sexual violence online or offline? This could be when they are going out with friends, or talking about the websites they use. [0-30, steps of 5]"),
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_sex_talk", height = "240"),
                                                           shiny::tableOutput("table_sv1_sex_talk")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days of COVID safe teenager behaviour",
                                                           status = "danger", 
                                                           h5("How many days last week did your teenager stay safe through wearing a mask, keeping a distance from people and keeping away from crowded places? [0-7]"),
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_covid_safe", height = "240"),
                                                           shiny::tableOutput("table_sv1_covid_safe")
                                                       ) #closes box
                                                     )#closes fluid row
                                            ), #closes baseline tab panel
                                            
                                            
                                            tabPanel("Endline survey",
                                                     fluidRow(
                                                       box(width = 6,
                                                           checkboxGroupInput(inputId = "CtrySV2",
                                                                              label = "Countries to select:",
                                                                              choices = c("South Africa" = "plh_za",
                                                                                          "Tanzania" = "plh_tz"),
                                                                              selected = c("plh_za","plh_tz")
                                                           )),
                                                       box(width = 6,
                                                           checkboxGroupInput(inputId = "OrgSV2",
                                                                              label = "Organisations to show:",
                                                                              choices = c("SA: Amathuba" = "Amathuba",
                                                                                          "SA: Dlalanathi" = "Dlalanathi",
                                                                                          "SA: Joy" = "Joy",
                                                                                          "SA: Nontobeko" = "Nontobeko", "TZ: ICS" ="ICS"),
                                                                              selected = c("Amathuba","Dlalanathi",
                                                                                           "Joy","Nontobeko", "ICS")
                                                           ))), #closes fluidRow
                                                     
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Endline survey completion",
                                                           status = "danger",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv2_totals", height = "240"),
                                                           shiny::tableOutput("table_sv2_totals")
                                                       )#closes box
                                                     ) # closes fluidRow
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
                                
                                fluidRow(
                                  box(width = 6,
                                      checkboxGroupInput(inputId = "CtryLB",
                                                         label = "Countries to select:",
                                                         choices = c("South Africa" = "plh_za",
                                                                     "Tanzania" = "plh_tz"),
                                                         selected = c("plh_za","plh_tz")
                                      )),
                                  box(width = 6,
                                      checkboxGroupInput(inputId = "OrgLB",
                                                         label = "Organisations to show:",
                                                         choices = c("SA: Amathuba" = "Amathuba",
                                                                     "SA: Dlalanathi" = "Dlalanathi",
                                                                     "SA: Joy" = "Joy",
                                                                     "SA: Nontobeko" = "Nontobeko", "TZ: ICS" ="ICS"),
                                                         selected = c("Amathuba","Dlalanathi",
                                                                      "Joy","Nontobeko", "ICS")
                                      ))), #closes fluidRow
                                
                                fluidRow(
                                  box(width = 12,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Parent Library average clicks per workshop week",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_library_mean", height = "240"), #generates graph
                                      shiny::tableOutput("table_library_mean")  #generates table
                                  )), #closes box and fluidRow
                                
                                fluidRow(
                                  box(width = 12,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Parent Library access",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_library_totals", height = "240"), #generates graph
                                      shiny::tableOutput("table_library_totals")  #generates table
                                  )), #closes box and fluidRow
                                
                                fluidRow(
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "My Tips (based on customisation)",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_lib_tips", height = "240"), #generates graph
                                      shiny::tableOutput("table_lib_tips")  #generates table
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Essential Tools (from each workshop week)",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_lib_tools", height = "240"),
                                      shiny::tableOutput("table_lib_tools")
                                  ) #closes box
                                ), #closes fluidRow
                                
                                fluidRow(
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Relax and Activities (fun with teen)",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_lib_activities", height = "240"), #generates graph
                                      shiny::tableOutput("table_lib_activities")  #generates table
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Customise ParentApp",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_lib_custom", height = "240"),
                                      shiny::tableOutput("table_lib_custom")
                                  ) #closes box
                                ), #closes fluidRow
                                
                                fluidRow(
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Help (quick parenting tips)",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_lib_help", height = "240"), #generates graph
                                      shiny::tableOutput("table_lib_help")  #generates table
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Technical Support",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_lib_tech", height = "240"),
                                      shiny::tableOutput("table_lib_tech")
                                  ) #closes box
                                ), #closes fluidRow
                                
                                fluidRow(
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Coping COVID",
                                      status = "primary",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_lib_covid", height = "240"), #generates graph
                                      shiny::tableOutput("table_lib_covid")  #generates table
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Coping with Grief",
                                      status = "primary",  
                                      #background = "orange",
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
  
  # df_enrolled <- summary_PT(df1,  enrolled,  enrolled, "Yes")
  # df_enrolled <- df_enrolled %>% mutate(group =  enrolled, count = enrolled_n) %>% dplyr::select(c(group, count))
  
  
  #SUMMARY STATS HEADER displays (same for all tabs)
  
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
  
  #FIRST tab DEMOGRAPHICS
#  demographics_plot_data <- reactive({
#    plhdata_org_clean_demographics <- plhdata_org_clean %>%
#      filter(Org %in% c((input$OrgDem)))
#    summary_plot_baseline <- plhdata_org_clean %>%
#      map(.x = data_baseline_survey, .f = ~summary_plot(columns_to_summarise = .x))
#    names(summary_plot_baseline) <- baseline_names_neat
#    return(summary_plot_baseline)
#  })
 
  selected_data_dem <- reactive({
    plhdata_checkgroup <- plhdata_org_clean %>% filter(Org %in% c((input$OrgDem)))
    return(plhdata_checkgroup)
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
    summary_table_baseline$` app language` %>% filter(Org %in% c((input$OrgDem)))
  }) 
  plot_language  <- reactive({
    summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field._app_language", replace = "rp.contact.field._")
  }) 
  output$table_language <- shiny::renderTable({(table_language())}, striped = TRUE)
  output$plot_language <- renderPlotly({plot_language()})
  
  #Workshop format plot and table
  table_ws_format <- reactive({
    summary_table_baseline$`Do workshops together` %>% filter(Org %in% c((input$OrgDem)))
  }) 
  plot_ws_format  <- reactive({
    summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.do_workshops_together", replace = "rp.contact.field.")
  }) 
  output$table_ws_format <- shiny::renderTable({(table_ws_format())}, striped = TRUE)
  output$plot_ws_format <- renderPlotly({plot_ws_format()})
  
  #App version
  table_app_version <- reactive({
    summary_table_baseline$`App version` %>% filter(Org %in% c((input$OrgDem)))
  }) 
  plot_app_version  <- reactive({
    plhdata_org_clean_1 <- plhdata_org_clean %>% filter(Org %in% c((input$OrgDem)))
    ggplot(plhdata_org_clean_1, aes(x = app_version, fill = Org)) +
      geom_bar(position = "dodge") +
      viridis::scale_fill_viridis(discrete = TRUE) +
      labs(x = "App version")
    #summary_plot(plhdata_org_clean, app_version)
  }) 
  output$table_app_version <- shiny::renderTable({(table_app_version())}, striped = TRUE)
  output$plot_app_version <- renderPlotly({plot_app_version()})
  
  #Parent gender plot and tabled
  table_parent_gender <- reactive({
    summary_table_baseline$`User gender` %>% filter(Org %in% c((input$OrgDem)))
  }) 
  plot_parent_gender  <- reactive({
    summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.user_gender", replace = "rp.contact.field.", plot_type = "histogram")
  }) 
  output$table_parent_gender <- shiny::renderTable({(table_parent_gender())}, striped = TRUE)
  output$plot_parent_gender <- renderPlotly({plot_parent_gender()})
  
  #Parent age plot and table
  table_parent_age <- reactive({
    plhdata_org_clean %>%
      summary_table(columns_to_summarise = rp.contact.field.user_age, summaries = "mean") %>% filter(Org %in% c((input$OrgDem)))}) 
  ##previously, but with new contact field summarise(`mean age`=(mean(rp.contact.field.user_gender, na.rm = TRUE)))
  plot_parent_age <- reactive({
    summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.user_age", replace = "rp.contact.field.", plot_type = "histogram")
  })
  output$table_parent_age <- shiny::renderTable({(table_parent_age())}, striped = TRUE)
  output$plot_parent_age <- renderPlotly({plot_parent_age()})
  
  #Adults in household plot and table
  table_household_adults <- reactive({
    summary_table_baseline$`Household adults` %>% filter(Org %in% c((input$OrgDem)))
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
    summary_table_baseline$`Household teens` %>% filter(Org %in% c((input$OrgDem)))
  }) 
  plot_household_teens  <- reactive({
    summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.household_teens", replace = "rp.contact.field.", plot_type = "histogram")
  })
  output$table_household_teens <- shiny::renderTable({( table_household_teens())}, striped = TRUE)
  output$plot_household_teens <- renderPlotly({plot_household_teens()})
  
  #Children in household plot and table
  table_household_children <- reactive({
    summary_table_baseline$`Household children` %>% filter(Org %in% c((input$OrgDem)))
  }) 
  plot_household_children  <- reactive({
    summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.household_children", replace = "rp.contact.field.", plot_type = "histogram")
  })
  output$table_household_children <- shiny::renderTable({( table_household_children())}, striped = TRUE)
  output$plot_household_children <- renderPlotly({plot_household_children()})
  
  #Babies in household plot and table
  table_household_babies <- reactive({
    summary_table_baseline$`Household babies` %>% filter(Org %in% c((input$OrgDem)))
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
  
  #Table of averages
  table_ws_totals <- reactive({
    summary_mean_completion_level %>% filter(Org %in% c((input$OrgWS))) }) 
  plot_ws_totals  <- reactive({
    summary_mean_completion_level_long <- pivot_longer(summary_mean_completion_level, cols = !Org, names_to = "Workshop", values_to = "Value")
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
  #1
  table_w_self_care <- reactive({
    summary_table_completion_level$`Self Care` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_self_care <- reactive({ summary_plot(plhdata_org_clean, "rp.contact.field.w_self_care_completion_level", replace = "rp.contact.field.w_") }) 
  output$table_w_self_care <- shiny::renderTable({(table_w_self_care())}, striped = TRUE)
  output$plot_w_self_care <- renderPlotly({plot_w_self_care()})
  
  #2
  table_w_1on1 <- reactive({
    summary_table_completion_level$`One-on-one Time` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_1on1<- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.w_1on1_completion_level", replace = "rp.contact.field.w_")
  }) 
  output$table_w_1on1 <- shiny::renderTable({(table_w_1on1())}, striped = TRUE)
  output$plot_w_1on1 <- renderPlotly({plot_w_1on1()})
  #3
  table_w_praise <- reactive({
    summary_table_completion_level$Praise %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_praise<- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.w_praise_completion_level", replace = "rp.contact.field.w_")
  }) 
  output$table_w_praise <- shiny::renderTable({(table_w_praise())}, striped = TRUE)
  output$plot_w_praise <- renderPlotly({plot_w_praise()})
  #4
  table_w_instruct <- reactive({
    summary_table_completion_level$`Positive Instructions` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_instruct<- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.w_instruct_completion_level", replace = "rp.contact.field.w_")
  }) 
  output$table_w_instruct <- shiny::renderTable({(table_w_instruct())}, striped = TRUE)
  output$plot_w_instruct <- renderPlotly({plot_w_instruct()})
  #5
  table_w_stress <- reactive({
    summary_table_completion_level$`Managing Stress` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_stress<- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.w_stress_completion_level", replace = "rp.contact.field.w_")
  }) 
  output$table_w_stress <- shiny::renderTable({(table_w_stress())}, striped = TRUE)
  output$plot_w_stress <- renderPlotly({plot_w_stress()})
  #6
  table_w_money <- reactive({
    summary_table_completion_level$`Family Budgets` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_money<- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.w_money_completion_level", replace = "rp.contact.field.w_")
  }) 
  output$table_w_money <- shiny::renderTable({(table_w_money())}, striped = TRUE)
  output$plot_w_money <- renderPlotly({plot_w_money()})
  #7
  table_w_rules <- reactive({
    summary_table_completion_level$Rules %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_rules<- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.w_rules_completion_level", replace = "rp.contact.field.w_")
  }) 
  output$table_w_rules <- shiny::renderTable({(table_w_rules())}, striped = TRUE)
  output$plot_w_rules <- renderPlotly({plot_w_rules()})
  #8
  table_w_consequence <- reactive({
    summary_table_completion_level$`Calm Consequences` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_consequence <- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.w_consequence_completion_level", replace = "rp.contact.field.w_")
  }) 
  output$table_w_consequence <- shiny::renderTable({(table_w_consequence())}, striped = TRUE)
  output$plot_w_consequence <- renderPlotly({plot_w_consequence()})
  #9
  table_w_solve <- reactive({
    summary_table_completion_level$`Problem Solving` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_solve<- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.w_solve_completion_level", replace = "rp.contact.field.w_")
  }) 
  output$table_w_solve <- shiny::renderTable({(table_w_solve())}, striped = TRUE)
  output$plot_w_solve <- renderPlotly({plot_w_solve()})
  #10
  table_w_safe <- reactive({
    summary_table_completion_level$`Teen Safety` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_safe<- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.w_safe_completion_level", replace = "rp.contact.field.w_")
  }) 
  output$table_w_safe <- shiny::renderTable({(table_w_safe())}, striped = TRUE)
  output$plot_w_safe <- renderPlotly({plot_w_safe()})
  #11
  table_w_crisis <- reactive({
    summary_table_completion_level$`Dealing with Crisis` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_crisis<- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.w_crisis_completion_level", replace = "rp.contact.field.w_")
  }) 
  output$table_w_crisis <- shiny::renderTable({(table_w_crisis())}, striped = TRUE)
  output$plot_w_crisis <- renderPlotly({plot_w_crisis()})
  #12
  table_w_celebrate <- reactive({
    summary_table_completion_level$`Celebration & Next Steps` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_celebrate<- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.w_celebrate_completion_level", replace = "rp.contact.field.w_")
  }) 
  output$table_w_celebrate <- shiny::renderTable({(table_w_celebrate())}, striped = TRUE)
  output$plot_w_celebrate <- renderPlotly({plot_w_celebrate()})
  
  #Workshop engagement sub tab: additional insights
  table_ws_started <- reactive({table_ws_started %>% filter(Org %in% c((input$OrgWS))) })
  plot_ws_started <- reactive({}) 
  output$table_ws_started <- shiny::renderTable({(table_ws_started())}, striped = TRUE)
  output$plot_ws_started <- renderPlotly({plot_ws_started()})
  
  table_ws_rel_completed <- reactive({table_perc_completed %>% filter(Org %in% c((input$OrgWS))) })
  plot_ws_rel_completed <- reactive({}) 
  output$table_ws_rel_completed <- shiny::renderTable({(table_ws_rel_completed())}, striped = TRUE)
  output$plot_ws_rel_completed <- renderPlotly({plot_ws_rel_completed()})
  
  #THIRD Tab Parent Points
  # applying org filter in parent point tab
  
  selected_data <- reactive({
    plhdata_checkgroup <- plhdata_org_clean %>% filter(Org %in% c((input$OrgPP)))
    return(plhdata_checkgroup)
  })
  
  #Table of averages
  table_pp_totals <- reactive({
    summary_mean_habits %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_totals  <- reactive({
    table_to_plot <- plhdata_org_clean %>%
      group_by(Org)  %>%
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
    summary_table_habits_all$`Relax` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_relax  <- reactive({
    # old method of just calling a static plot from analysis file - abandoned because does not allow filtering out sub-sets of data
    # summary_plot_habits_all$`Relax`
      summary_plot(data = selected_data(), columns_to_summarise = "rp.contact.field.parent_point_count_relax", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")
  }) 
  output$table_pp_relax <- shiny::renderTable({(table_pp_relax())}, striped = TRUE)
  output$plot_pp_relax <- renderPlotly({plot_pp_relax()})
  
  # pp_treat_yourself
  table_pp_treat_yourself <- reactive({
    summary_table_habits_all$`Treat yourself` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_treat_yourself  <- reactive({
    summary_plot(data = selected_data(), columns_to_summarise = "rp.contact.field.parent_point_count_treat_yourself", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")
  }) 
  output$table_pp_treat_yourself <- shiny::renderTable({(table_pp_treat_yourself())}, striped = TRUE)
  output$plot_pp_treat_yourself <- renderPlotly({plot_pp_treat_yourself()})
  
  # pp_praise_yourself
  table_pp_praise_yourself <- reactive({
    summary_table_habits_all$`Praise yourself` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_praise_yourself  <- reactive({
    summary_plot(data = selected_data(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_yourself", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")
    }) 
  output$table_pp_praise_yourself <- shiny::renderTable({(table_pp_praise_yourself())}, striped = TRUE)
  output$plot_pp_praise_yourself <- renderPlotly({plot_pp_praise_yourself()})
  
  # pp_spend_time
  table_pp_spend_time <- reactive({
    summary_table_habits_all$`Spend time` %>% filter(Org %in% c((input$OrgPP))) 
  }) 
  plot_pp_spend_time  <- reactive({
    summary_plot(data = selected_data(), columns_to_summarise = "rp.contact.field.parent_point_count_spend_time", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")
    }) 
  output$table_pp_spend_time <- shiny::renderTable({(table_pp_spend_time())}, striped = TRUE)
  output$plot_pp_spend_time <- renderPlotly({plot_pp_spend_time()})
  
  # pp_praise_teen
  table_pp_praise_teen <- reactive({
    summary_table_habits_all$`Praise teen` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_praise_teen  <- reactive({
    summary_plot(data = selected_data(), columns_to_summarise = "rp.contact.field.parent_point_count_praise_teen", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")
    }) 
  output$table_pp_praise_teen <- shiny::renderTable({(table_pp_praise_teen())}, striped = TRUE)
  output$plot_pp_praise_teen <- renderPlotly({plot_pp_praise_teen()})
  
  # pp_instruct_positively
  table_pp_instruct_positively <- reactive({
    summary_table_habits_all$`Instruct positively` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_instruct_positively  <- reactive({
    summary_plot(data = selected_data(), columns_to_summarise = "rp.contact.field.parent_point_count_instruct_positively", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")
  }) 
  output$table_pp_instruct_positively <- shiny::renderTable({(table_pp_instruct_positively())}, striped = TRUE)
  output$plot_pp_instruct_positively <- renderPlotly({plot_pp_instruct_positively()})
  
  # pp_breathe
  table_pp_breathe <- reactive({
    summary_table_habits_all$`Breathe` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_breathe  <- reactive({
    summary_plot(data = selected_data(), columns_to_summarise = "rp.contact.field.parent_point_count_breathe", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")
  }) 
  output$table_pp_breathe <- shiny::renderTable({(table_pp_breathe())}, striped = TRUE)
  output$plot_pp_breathe <- renderPlotly({plot_pp_breathe()})
  
  # pp_money
  table_pp_money <- reactive({
    summary_table_habits_all$`Money` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_money  <- reactive({
    summary_plot(data = selected_data(), columns_to_summarise = "rp.contact.field.parent_point_count_money", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")
  }) 
  output$table_pp_money <- shiny::renderTable({(table_pp_money())}, striped = TRUE)
  output$plot_pp_money <- renderPlotly({plot_pp_money()})
  
  # pp_consequence
  table_pp_consequence <- reactive({
    summary_table_habits_all$`Consequence` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_consequence  <- reactive({
    summary_plot(data = selected_data(), columns_to_summarise = "rp.contact.field.parent_point_count_consequence", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")
  }) 
  output$table_pp_consequence <- shiny::renderTable({(table_pp_consequence())}, striped = TRUE)
  output$plot_pp_consequence <- renderPlotly({plot_pp_consequence()})
  
  # pp_safe
  table_pp_safe <- reactive({
    summary_table_habits_all$`Safe` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_safe  <- reactive({
    summary_plot(data = selected_data(), columns_to_summarise = "rp.contact.field.parent_point_count_safe", replace = "rp.contact.field.parent_point_count_", plot_type = "boxplot")
  }) 
  output$table_pp_safe <- shiny::renderTable({(table_pp_safe())}, striped = TRUE)
  output$plot_pp_safe <- renderPlotly({plot_pp_safe()})
  
  
  #Parent Point sub tab Relax points pp1
  table_pp_relax_ws_totals <- reactive({
    summary_relax_workshop %>% filter(Org %in% c(input$OrgPP))
  })
  output$table_pp_relax_ws_totals <- shiny::renderTable({table_pp_relax_ws_totals()})
  plot_pp_relax_ws_totals <- reactive({summary_relax_workshop_long <- summary_relax_workshop_long %>% filter(Org %in% c(input$OrgPP)) 
    ggplot(summary_relax_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
      geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      geom_line() + labs(x = "Workshop week", y = "Number of points")
  })
  output$plot_pp_relax_ws_totals <- renderPlotly({plot_pp_relax_ws_totals()})
  
  table_pp_relax_w_self_care <- reactive({summary_table_habits_self_care$`Relax` %>% filter(Org %in% c((input$OrgPP)))})
  plot_pp_relax_w_self_care <- reactive({})
  output$table_pp_relax_w_self_care <- shiny::renderTable({(table_pp_relax_w_self_care())}, striped = TRUE)
  output$plot_pp_relax_w_self_care <- renderPlotly({plot_pp_relax_w_self_care()})
  
  table_pp_relax_w_1on1 <- reactive({summary_table_habits_1on1$`Relax` %>% filter(Org %in% c((input$OrgPP)))})
  plot_pp_relax_w_1on1 <- reactive({})
  output$table_pp_relax_w_1on1 <- shiny::renderTable({(table_pp_relax_w_1on1())}, striped = TRUE)
  output$plot_pp_relax_w_1on1 <- renderPlotly({plot_pp_relax_w_1on1()})
  
  table_pp_relax_w_praise <- reactive({summary_table_habits_praise$`Relax` %>% filter(Org %in% c((input$OrgPP)))})
  plot_pp_relax_w_praise <- reactive({})
  output$table_pp_relax_w_praise <- shiny::renderTable({(table_pp_relax_w_praise())}, striped = TRUE)
  output$plot_pp_relax_w_praise <- renderPlotly({plot_pp_relax_w_praise()})
  
  table_pp_relax_w_instruct <- reactive({summary_table_habits_instruct$`Relax` %>% filter(Org %in% c((input$OrgPP)))})
  plot_pp_relax_w_instruct <- reactive({})
  output$table_pp_relax_w_instruct <- shiny::renderTable({(table_pp_relax_w_instruct())}, striped = TRUE)
  output$plot_pp_relax_w_instruct <- renderPlotly({plot_pp_relax_w_instruct()})
  
  table_pp_relax_w_stress <- reactive({summary_table_habits_stress$`Relax`%>% filter(Org %in% c((input$OrgPP)))})
  plot_pp_relax_w_stress <- reactive({})
  output$table_pp_relax_w_stress <- shiny::renderTable({(table_pp_relax_w_stress())}, striped = TRUE)
  output$plot_pp_relax_w_stress <- renderPlotly({plot_pp_relax_w_stress()})
  
  table_pp_relax_w_money <- reactive({summary_table_habits_money$`Relax` %>% filter(Org %in% c((input$OrgPP)))})
  plot_pp_relax_w_money <- reactive({})
  output$table_pp_relax_w_money <- shiny::renderTable({(table_pp_relax_w_money())}, striped = TRUE)
  output$plot_pp_relax_w_money <- renderPlotly({plot_pp_relax_w_money()})
  
  table_pp_relax_w_rules <- reactive({summary_table_habits_rules$`Relax` %>% filter(Org %in% c((input$OrgPP)))})
  plot_pp_relax_w_rules <- reactive({})
  output$table_pp_relax_w_rules <- shiny::renderTable({(table_pp_relax_w_rules())}, striped = TRUE)
  output$plot_pp_relax_w_rules <- renderPlotly({plot_pp_relax_w_rules()})
  
  table_pp_relax_w_consequence <- reactive({summary_table_habits_consequence$`Relax` %>% filter(Org %in% c((input$OrgPP)))})
  plot_pp_relax_w_consequence <- reactive({})
  output$table_pp_relax_w_consequence <- shiny::renderTable({(table_pp_relax_w_consequence())}, striped = TRUE)
  output$plot_pp_relax_w_consequence <- renderPlotly({plot_pp_relax_w_consequence()})
  
  table_pp_relax_w_solve <- reactive({summary_table_habits_solve$`Relax` %>% filter(Org %in% c((input$OrgPP)))})
  plot_pp_relax_w_solve <- reactive({})
  output$table_pp_relax_w_solve <- shiny::renderTable({(table_pp_relax_w_solve())}, striped = TRUE)
  output$plot_pp_relax_w_solve <- renderPlotly({plot_pp_relax_w_solve()})
  
  table_pp_relax_w_safe <- reactive({summary_table_habits_safe$`Relax` %>% filter(Org %in% c((input$OrgPP)))})
  plot_pp_relax_w_safe <- reactive({})
  output$table_pp_relax_w_safe <- shiny::renderTable({(table_pp_relax_w_safe())}, striped = TRUE)
  output$plot_pp_relax_w_safe <- renderPlotly({plot_pp_relax_w_safe()})
  
  table_pp_relax_w_crisis <- reactive({summary_table_habits_crisis$`Relax` %>% filter(Org %in% c((input$OrgPP)))})
  plot_pp_relax_w_crisis <- reactive({})
  output$table_pp_relax_w_crisis <- shiny::renderTable({(table_pp_relax_w_crisis())}, striped = TRUE)
  output$table_pp_relax_w_crisis <- renderPlotly({table_pp_relax_w_crisis()})
  
  table_pp_relax_w_celebrate <- reactive({summary_table_habits_celebrate$`Relax` %>% filter(Org %in% c((input$OrgPP)))})
  plot_pp_relax_w_celebrate <- reactive({})
  output$table_pp_relax_w_celebrate <- shiny::renderTable({(table_pp_relax_w_celebrate())}, striped = TRUE)
  output$table_pp_relax_w_celebrate <- renderPlotly({table_pp_relax_w_celebrate()})
  
  #Parent Point sub tab Treat Yourself points pp2
 
   table_pp_treat_yourself_ws_totals <- reactive({
    summary_treat_yourself_workshop %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_treat_yourself_ws_totals  <- reactive({summary_treat_yourself_workshop_long <- summary_treat_yourself_workshop_long %>% filter(Org %in% c(input$OrgPP)) 
  ggplot(summary_treat_yourself_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
    geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_line() + labs(x = "Workshop week", y = "Number of points")
  }) 
  output$table_pp_treat_yourself_ws_totals <- shiny::renderTable({(table_pp_treat_yourself_ws_totals())}, striped = TRUE)
  output$plot_pp_treat_yourself_ws_totals <- renderPlotly({plot_pp_treat_yourself_ws_totals()})
  
table_pp_treat_yourself_w_self_care <- reactive({summary_table_habits_self_care$`Treat yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_treat_yourself_w_self_care <- reactive({})
output$table_pp_treat_yourself_w_self_care <- shiny::renderTable({(table_pp_treat_yourself_w_self_care())}, striped = TRUE)
output$plot_pp_treat_yourself_w_self_care <- renderPlotly({plot_pp_treat_yourself_w_self_care()})

table_pp_treat_yourself_w_1on1 <- reactive({summary_table_habits_1on1$`Treat yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_treat_yourself_w_1on1 <- reactive({})
output$table_pp_treat_yourself_w_1on1 <- shiny::renderTable({(table_pp_treat_yourself_w_1on1())}, striped = TRUE)
output$plot_pp_treat_yourself_w_1on1 <- renderPlotly({plot_pp_treat_yourself_w_1on1()})

table_pp_treat_yourself_w_praise <- reactive({summary_table_habits_praise$`Treat yourself ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_treat_yourself_w_praise <- reactive({})
output$table_pp_treat_yourself_w_praise <- shiny::renderTable({(table_pp_treat_yourself_w_praise())}, striped = TRUE)
output$plot_pp_treat_yourself_w_praise <- renderPlotly({plot_pp_treat_yourself_w_praise()})

table_pp_treat_yourself_w_instruct <- reactive({summary_table_habits_instruct$`Treat yourself ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_treat_yourself_w_instruct <- reactive({})
output$table_pp_treat_yourself_w_instruct <- shiny::renderTable({(table_pp_treat_yourself_w_instruct())}, striped = TRUE)
output$plot_pp_treat_yourself_w_instruct <- renderPlotly({plot_pp_treat_yourself_w_instruct()})

table_pp_treat_yourself_w_stress <- reactive({summary_table_habits_stress$`Treat yourself ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_treat_yourself_w_stress <- reactive({})
output$table_pp_treat_yourself_w_stress <- shiny::renderTable({(table_pp_treat_yourself_w_stress())}, striped = TRUE)
output$plot_pp_treat_yourself_w_stress <- renderPlotly({plot_pp_treat_yourself_w_stress()})

table_pp_treat_yourself_w_money <- reactive({summary_table_habits_money$`Treat yourself ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_treat_yourself_w_money <- reactive({})
output$table_pp_treat_yourself_w_money <- shiny::renderTable({(table_pp_treat_yourself_w_money())}, striped = TRUE)
output$plot_pp_treat_yourself_w_money <- renderPlotly({plot_pp_treat_yourself_w_money()})

#ERROR
table_pp_treat_yourself_w_rules <- reactive({summary_table_habits_rules$`Treat yourself ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_treat_yourself_w_rules <- reactive({})
output$table_pp_treat_yourself_w_rules <- shiny::renderTable({(table_pp_treat_yourself_w_rules())}, striped = TRUE)
output$plot_pp_treat_yourself_w_rules <- renderPlotly({plot_pp_treat_yourself_w_rules()})

table_pp_treat_yourself_w_consequence <- reactive({summary_table_habits_consequence$`Treat yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_treat_yourself_w_consequence <- reactive({})
output$table_pp_treat_yourself_w_consequence <- shiny::renderTable({(table_pp_treat_yourself_w_consequence())}, striped = TRUE)
output$plot_pp_treat_yourself_w_consequence <- renderPlotly({plot_pp_treat_yourself_w_consequence()})

table_pp_treat_yourself_w_solve <- reactive({summary_table_habits_solve$`Treat yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_treat_yourself_w_solve <- reactive({})
output$table_pp_treat_yourself_w_solve <- shiny::renderTable({(table_pp_treat_yourself_w_solve())}, striped = TRUE)
output$plot_pp_treat_yourself_w_solve <- renderPlotly({plot_pp_treat_yourself_w_solve()})

table_pp_treat_yourself_w_safe <- reactive({summary_table_habits_safe$`Treat yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_treat_yourself_w_safe <- reactive({})
output$table_pp_treat_yourself_w_safe <- shiny::renderTable({(table_pp_treat_yourself_w_safe())}, striped = TRUE)
output$plot_pp_treat_yourself_w_safe <- renderPlotly({plot_pp_treat_yourself_w_safe()})

table_pp_treat_yourself_w_crisis <- reactive({summary_table_habits_crisis$`Treat yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_treat_yourself_w_crisis <- reactive({})
output$table_pp_treat_yourself_w_crisis <- shiny::renderTable({(table_pp_treat_yourself_w_crisis())}, striped = TRUE)
output$table_pp_treat_yourself_w_crisis <- renderPlotly({table_pp_treat_yourself_w_crisis()})

table_pp_treat_yourself_w_celebrate <- reactive({summary_table_habits_celebrate$`Treat yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_treat_yourself_w_celebrate <- reactive({})
output$table_pp_treat_yourself_w_celebrate <- shiny::renderTable({(table_pp_treat_yourself_w_celebrate())}, striped = TRUE)
output$plot_pp_treat_yourself_w_celebrate <- renderPlotly({plot_pp_treat_yourself_w_celebrate()})

# Parent Point sub tab Praise Yourself points pp3
table_pp_praise_yourself_ws_totals <- reactive({ summary_praise_yourself_workshop %>% filter(Org %in% c(input$OrgPP))})
plot_pp_praise_yourself_ws_totals <- reactive({summary_praise_yourself_workshop_long <- summary_praise_yourself_workshop_long %>% filter(Org %in% c(input$OrgPP)) 
ggplot(summary_praise_yourself_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line() + labs(x = "Workshop week", y = "Number of points")
})
output$table_pp_praise_yourself_ws_totals <- shiny::renderTable({table_pp_praise_yourself_ws_totals()})
output$plot_pp_praise_yourself_ws_totals <- renderPlotly({plot_pp_praise_yourself_ws_totals()})

table_pp_praise_yourself_w_self_care <- reactive({summary_table_habits_self_care$`Praise yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_yourself_w_self_care <- reactive({})
output$table_pp_praise_yourself_w_self_care <- shiny::renderTable({(table_pp_praise_yourself_w_self_care())}, striped = TRUE)
output$plot_pp_praise_yourself_w_self_care <- renderPlotly({plot_pp_praise_yourself_w_self_care()})

table_pp_praise_yourself_w_1on1 <- reactive({summary_table_habits_1on1$`Praise yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_yourself_w_1on1 <- reactive({})
output$table_pp_praise_yourself_w_1on1 <- shiny::renderTable({(table_pp_praise_yourself_w_1on1())}, striped = TRUE)
output$plot_pp_praise_yourself_w_1on1 <- renderPlotly({plot_pp_praise_yourself_w_1on1()})

table_pp_praise_yourself_w_praise <- reactive({summary_table_habits_praise$`Praise yourself ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_yourself_w_praise <- reactive({})
output$table_pp_praise_yourself_w_praise <- shiny::renderTable({(table_pp_praise_yourself_w_praise())}, striped = TRUE)
output$plot_pp_praise_yourself_w_praise <- renderPlotly({plot_pp_praise_yourself_w_praise()})

table_pp_praise_yourself_w_instruct <- reactive({summary_table_habits_instruct$`Praise yourself ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_yourself_w_instruct <- reactive({})
output$table_pp_praise_yourself_w_instruct <- shiny::renderTable({(table_pp_praise_yourself_w_instruct())}, striped = TRUE)
output$plot_pp_praise_yourself_w_instruct <- renderPlotly({plot_pp_praise_yourself_w_instruct()})

table_pp_praise_yourself_w_stress <- reactive({summary_table_habits_stress$`Praise yourself ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_yourself_w_stress <- reactive({})
output$table_pp_praise_yourself_w_stress <- shiny::renderTable({(table_pp_praise_yourself_w_stress())}, striped = TRUE)
output$plot_pp_praise_yourself_w_stress <- renderPlotly({plot_pp_praise_yourself_w_stress()})

table_pp_praise_yourself_w_money <- reactive({summary_table_habits_money$`Praise yourself ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_yourself_w_money <- reactive({})
output$table_pp_praise_yourself_w_money <- shiny::renderTable({(table_pp_praise_yourself_w_money())}, striped = TRUE)
output$plot_pp_praise_yourself_w_money <- renderPlotly({plot_pp_praise_yourself_w_money()})

table_pp_praise_yourself_w_rules <- reactive({summary_table_habits_rules$`Praise yourself ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_yourself_w_rules <- reactive({})
output$table_pp_praise_yourself_w_rules <- shiny::renderTable({(table_pp_praise_yourself_w_rules())}, striped = TRUE)
output$plot_pp_praise_yourself_w_rules <- renderPlotly({plot_pp_praise_yourself_w_rules()})

table_pp_praise_yourself_w_consequence <- reactive({summary_table_habits_consequence$`Praise yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_yourself_w_consequence <- reactive({})
output$table_pp_praise_yourself_w_consequence <- shiny::renderTable({(table_pp_praise_yourself_w_consequence())}, striped = TRUE)
output$plot_pp_praise_yourself_w_consequence <- renderPlotly({plot_pp_praise_yourself_w_consequence()})

table_pp_praise_yourself_w_solve <- reactive({summary_table_habits_solve$`Praise yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_yourself_w_solve <- reactive({})
output$table_pp_praise_yourself_w_solve <- shiny::renderTable({(table_pp_praise_yourself_w_solve())}, striped = TRUE)
output$plot_pp_praise_yourself_w_solve <- renderPlotly({plot_pp_praise_yourself_w_solve()})

table_pp_praise_yourself_w_safe <- reactive({summary_table_habits_safe$`Praise yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_yourself_w_safe <- reactive({})
output$table_pp_praise_yourself_w_safe <- shiny::renderTable({(table_pp_praise_yourself_w_safe())}, striped = TRUE)
output$plot_pp_praise_yourself_w_safe <- renderPlotly({plot_pp_praise_yourself_w_safe()})

table_pp_praise_yourself_w_crisis <- reactive({summary_table_habits_crisis$`Praise yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_yourself_w_crisis <- reactive({})
output$table_pp_praise_yourself_w_crisis <- shiny::renderTable({(table_pp_praise_yourself_w_crisis())}, striped = TRUE)
output$table_pp_praise_yourself_w_crisis <- renderPlotly({table_pp_praise_yourself_w_crisis()})

table_pp_praise_yourself_w_celebrate <- reactive({summary_table_habits_celebrate$`Praise yourself` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_yourself_w_celebrate <- reactive({})
output$table_pp_praise_yourself_w_celebrate <- shiny::renderTable({(table_pp_praise_yourself_w_celebrate())}, striped = TRUE)
output$table_pp_praise_yourself_w_celebrate <- renderPlotly({table_pp_praise_yourself_w_celebrate()})

# Parent Point sub tab Spend Time points pp4
table_pp_spend_time_ws_totals <- reactive({ summary_spend_time_workshop %>% filter(Org %in% c(input$OrgPP))})
plot_pp_spend_time_ws_totals <- reactive({summary_spend_time_workshop_long <- summary_spend_time_workshop_long %>% filter(Org %in% c(input$OrgPP)) 
ggplot(summary_spend_time_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line() + labs(x = "Workshop week", y = "Number of points")})
output$table_pp_spend_time_ws_totals <- shiny::renderTable({table_pp_spend_time_ws_totals()})
output$plot_pp_spend_time_ws_totals <- renderPlotly({plot_pp_spend_time_ws_totals()})

table_pp_spend_time_w_self_care <- reactive({summary_table_habits_self_care$`Spend time` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_spend_time_w_self_care <- reactive({})
output$table_pp_spend_time_w_self_care <- shiny::renderTable({(table_pp_spend_time_w_self_care())}, striped = TRUE)
output$plot_pp_spend_time_w_self_care <- renderPlotly({plot_pp_spend_time_w_self_care()})

table_pp_spend_time_w_1on1 <- reactive({summary_table_habits_1on1$`Spend time` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_spend_time_w_1on1 <- reactive({})
output$table_pp_spend_time_w_1on1 <- shiny::renderTable({(table_pp_spend_time_w_1on1())}, striped = TRUE)
output$plot_pp_spend_time_w_1on1 <- renderPlotly({plot_pp_spend_time_w_1on1()})

table_pp_spend_time_w_praise <- reactive({summary_table_habits_praise$`Spend time ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_spend_time_w_praise <- reactive({})
output$table_pp_spend_time_w_praise <- shiny::renderTable({(table_pp_spend_time_w_praise())}, striped = TRUE)
output$plot_pp_spend_time_w_praise <- renderPlotly({plot_pp_spend_time_w_praise()})

table_pp_spend_time_w_instruct <- reactive({summary_table_habits_instruct$`Spend time ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_spend_time_w_instruct <- reactive({})
output$table_pp_spend_time_w_instruct <- shiny::renderTable({(table_pp_spend_time_w_instruct())}, striped = TRUE)
output$plot_pp_spend_time_w_instruct <- renderPlotly({plot_pp_spend_time_w_instruct()})

table_pp_spend_time_w_stress <- reactive({summary_table_habits_stress$`Spend time ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_spend_time_w_stress <- reactive({})
output$table_pp_spend_time_w_stress <- shiny::renderTable({(table_pp_spend_time_w_stress())}, striped = TRUE)
output$plot_pp_spend_time_w_stress <- renderPlotly({plot_pp_spend_time_w_stress()})

table_pp_spend_time_w_money <- reactive({summary_table_habits_money$`Spend time ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_spend_time_w_money <- reactive({})
output$table_pp_spend_time_w_money <- shiny::renderTable({(table_pp_spend_time_w_money())}, striped = TRUE)
output$plot_pp_spend_time_w_money <- renderPlotly({plot_pp_spend_time_w_money()})

table_pp_spend_time_w_rules <- reactive({summary_table_habits_rules$`Spend time ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_spend_time_w_rules <- reactive({})
output$table_pp_spend_time_w_rules <- shiny::renderTable({(table_pp_spend_time_w_rules())}, striped = TRUE)
output$plot_pp_spend_time_w_rules <- renderPlotly({plot_pp_spend_time_w_rules()})

table_pp_spend_time_w_consequence <- reactive({summary_table_habits_consequence$`Spend time` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_spend_time_w_consequence <- reactive({})
output$table_pp_spend_time_w_consequence <- shiny::renderTable({(table_pp_spend_time_w_consequence())}, striped = TRUE)
output$plot_pp_spend_time_w_consequence <- renderPlotly({plot_pp_spend_time_w_consequence()})

table_pp_spend_time_w_solve <- reactive({summary_table_habits_solve$`Spend time` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_spend_time_w_solve <- reactive({})
output$table_pp_spend_time_w_solve <- shiny::renderTable({(table_pp_spend_time_w_solve())}, striped = TRUE)
output$plot_pp_spend_time_w_solve <- renderPlotly({plot_pp_spend_time_w_solve()})

table_pp_spend_time_w_safe <- reactive({summary_table_habits_safe$`Spend time` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_spend_time_w_safe <- reactive({})
output$table_pp_spend_time_w_safe <- shiny::renderTable({(table_pp_spend_time_w_safe())}, striped = TRUE)
output$plot_pp_spend_time_w_safe <- renderPlotly({plot_pp_spend_time_w_safe()})

table_pp_spend_time_w_crisis <- reactive({summary_table_habits_crisis$`Spend time` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_spend_time_w_crisis <- reactive({})
output$table_pp_spend_time_w_crisis <- shiny::renderTable({(table_pp_spend_time_w_crisis())}, striped = TRUE)
output$table_pp_spend_time_w_crisis <- renderPlotly({table_pp_spend_time_w_crisis()})

table_pp_spend_time_w_celebrate <- reactive({summary_table_habits_celebrate$`Spend time` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_spend_time_w_celebrate <- reactive({})
output$table_pp_spend_time_w_celebrate <- shiny::renderTable({(table_pp_spend_time_w_celebrate())}, striped = TRUE)
output$table_pp_spend_time_w_celebrate <- renderPlotly({table_pp_spend_time_w_celebrate()})

# Parent Point sub tab Praise Teen points pp5
table_pp_praise_teen_ws_totals <- reactive({ summary_praise_teen_workshop %>% filter(Org %in% c(input$OrgPP))})
plot_pp_praise_teen_ws_totals <- reactive({summary_praise_teen_workshop_long <- summary_praise_teen_workshop_long %>% filter(Org %in% c(input$OrgPP)) 
ggplot(summary_praise_teen_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line() + labs(x = "Workshop week", y = "Number of points")})
output$table_pp_praise_teen_ws_totals <- shiny::renderTable({table_pp_praise_teen_ws_totals()})
output$plot_pp_praise_teen_ws_totals <- renderPlotly({plot_pp_praise_teen_ws_totals()})

table_pp_praise_teen_w_self_care <- reactive({summary_table_habits_self_care$`Praise teen` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_teen_w_self_care <- reactive({})
output$table_pp_praise_teen_w_self_care <- shiny::renderTable({(table_pp_praise_teen_w_self_care())}, striped = TRUE)
output$plot_pp_praise_teen_w_self_care <- renderPlotly({plot_pp_praise_teen_w_self_care()})

table_pp_praise_teen_w_1on1 <- reactive({summary_table_habits_1on1$`Praise teen` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_teen_w_1on1 <- reactive({})
output$table_pp_praise_teen_w_1on1 <- shiny::renderTable({(table_pp_praise_teen_w_1on1())}, striped = TRUE)
output$plot_pp_praise_teen_w_1on1 <- renderPlotly({plot_pp_praise_teen_w_1on1()})

table_pp_praise_teen_w_praise <- reactive({summary_table_habits_praise$`Praise teen ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_teen_w_praise <- reactive({})
output$table_pp_praise_teen_w_praise <- shiny::renderTable({(table_pp_praise_teen_w_praise())}, striped = TRUE)
output$plot_pp_praise_teen_w_praise <- renderPlotly({plot_pp_praise_teen_w_praise()})

table_pp_praise_teen_w_instruct <- reactive({summary_table_habits_instruct$`Praise teen ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_teen_w_instruct <- reactive({})
output$table_pp_praise_teen_w_instruct <- shiny::renderTable({(table_pp_praise_teen_w_instruct())}, striped = TRUE)
output$plot_pp_praise_teen_w_instruct <- renderPlotly({plot_pp_praise_teen_w_instruct()})

table_pp_praise_teen_w_stress <- reactive({summary_table_habits_stress$`Praise teen ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_teen_w_stress <- reactive({})
output$table_pp_praise_teen_w_stress <- shiny::renderTable({(table_pp_praise_teen_w_stress())}, striped = TRUE)
output$plot_pp_praise_teen_w_stress <- renderPlotly({plot_pp_praise_teen_w_stress()})

table_pp_praise_teen_w_money <- reactive({summary_table_habits_money$`Praise teen ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_teen_w_money <- reactive({})
output$table_pp_praise_teen_w_money <- shiny::renderTable({(table_pp_praise_teen_w_money())}, striped = TRUE)
output$plot_pp_praise_teen_w_money <- renderPlotly({plot_pp_praise_teen_w_money()})

table_pp_praise_teen_w_rules <- reactive({summary_table_habits_rules$`Praise teen ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_teen_w_rules <- reactive({})
output$table_pp_praise_teen_w_rules <- shiny::renderTable({(table_pp_praise_teen_w_rules())}, striped = TRUE)
output$plot_pp_praise_teen_w_rules <- renderPlotly({plot_pp_praise_teen_w_rules()})

table_pp_praise_teen_w_consequence <- reactive({summary_table_habits_consequence$`Praise teen` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_teen_w_consequence <- reactive({})
output$table_pp_praise_teen_w_consequence <- shiny::renderTable({(table_pp_praise_teen_w_consequence())}, striped = TRUE)
output$plot_pp_praise_teen_w_consequence <- renderPlotly({plot_pp_praise_teen_w_consequence()})

table_pp_praise_teen_w_solve <- reactive({summary_table_habits_solve$`Praise teen` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_teen_w_solve <- reactive({})
output$table_pp_praise_teen_w_solve <- shiny::renderTable({(table_pp_praise_teen_w_solve())}, striped = TRUE)
output$plot_pp_praise_teen_w_solve <- renderPlotly({plot_pp_praise_teen_w_solve()})

table_pp_praise_teen_w_safe <- reactive({summary_table_habits_safe$`Praise teen` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_teen_w_safe <- reactive({})
output$table_pp_praise_teen_w_safe <- shiny::renderTable({(table_pp_praise_teen_w_safe())}, striped = TRUE)
output$plot_pp_praise_teen_w_safe <- renderPlotly({plot_pp_praise_teen_w_safe()})

table_pp_praise_teen_w_crisis <- reactive({summary_table_habits_crisis$`Praise teen` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_teen_w_crisis <- reactive({})
output$table_pp_praise_teen_w_crisis <- shiny::renderTable({(table_pp_praise_teen_w_crisis())}, striped = TRUE)
output$table_pp_praise_teen_w_crisis <- renderPlotly({table_pp_praise_teen_w_crisis()})

able_pp_praise_teen_w_celebrate <- reactive({summary_table_habits_celebrate$`Praise teen` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_praise_teen_w_celebrate <- reactive({})
output$table_pp_praise_teen_w_celebrate <- shiny::renderTable({(table_pp_praise_teen_w_celebrate())}, striped = TRUE)
output$table_pp_praise_teen_w_celebrate <- renderPlotly({table_pp_praise_teen_w_celebrate()})

# Parent Point sub tab Instruct Positively points pp6
table_pp_instruct_positively_ws_totals <- reactive({ summary_instruct_positively_workshop %>% filter(Org %in% c(input$OrgPP))})
plot_pp_instruct_positively_ws_totals <- reactive({summary_instruct_positively_workshop_long <- summary_instruct_positively_workshop_long %>% filter(Org %in% c(input$OrgPP)) 
ggplot(summary_instruct_positively_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line() + labs(x = "Workshop week", y = "Number of points")})
output$table_pp_instruct_positively_ws_totals <- shiny::renderTable({table_pp_instruct_positively_ws_totals()})
output$plot_pp_instruct_positively_ws_totals <- renderPlotly({plot_pp_instruct_positively_ws_totals()})

table_pp_instruct_positively_w_self_care <- reactive({summary_table_habits_self_care$`Instruct positively` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_instruct_positively_w_self_care <- reactive({})
output$table_pp_instruct_positively_w_self_care <- shiny::renderTable({(table_pp_instruct_positively_w_self_care())}, striped = TRUE)
output$plot_pp_instruct_positively_w_self_care <- renderPlotly({plot_pp_instruct_positively_w_self_care()})

table_pp_instruct_positively_w_1on1 <- reactive({summary_table_habits_1on1$`Instruct positively` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_instruct_positively_w_1on1 <- reactive({})
output$table_pp_instruct_positively_w_1on1 <- shiny::renderTable({(table_pp_instruct_positively_w_1on1())}, striped = TRUE)
output$plot_pp_instruct_positively_w_1on1 <- renderPlotly({plot_pp_instruct_positively_w_1on1()})

table_pp_instruct_positively_w_praise <- reactive({summary_table_habits_praise$`Instruct positively ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_instruct_positively_w_praise <- reactive({})
output$table_pp_instruct_positively_w_praise <- shiny::renderTable({(table_pp_instruct_positively_w_praise())}, striped = TRUE)
output$plot_pp_instruct_positively_w_praise <- renderPlotly({plot_pp_instruct_positively_w_praise()})

table_pp_instruct_positively_w_instruct <- reactive({summary_table_habits_instruct$`Instruct positively ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_instruct_positively_w_instruct <- reactive({})
output$table_pp_instruct_positively_w_instruct <- shiny::renderTable({(table_pp_instruct_positively_w_instruct())}, striped = TRUE)
output$plot_pp_instruct_positively_w_instruct <- renderPlotly({plot_pp_instruct_positively_w_instruct()})

table_pp_instruct_positively_w_stress <- reactive({summary_table_habits_stress$`Instruct positively ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_instruct_positively_w_stress <- reactive({})
output$table_pp_instruct_positively_w_stress <- shiny::renderTable({(table_pp_instruct_positively_w_stress())}, striped = TRUE)
output$plot_pp_instruct_positively_w_stress <- renderPlotly({plot_pp_instruct_positively_w_stress()})

table_pp_instruct_positively_w_money <- reactive({summary_table_habits_money$`Instruct positively` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_instruct_positively_w_money <- reactive({})
output$table_pp_instruct_positively_w_money <- shiny::renderTable({(table_pp_instruct_positively_w_money())}, striped = TRUE)
output$plot_pp_instruct_positively_w_money <- renderPlotly({plot_pp_instruct_positively_w_money()})

table_pp_instruct_positively_w_rules <- reactive({summary_table_habits_rules$`Instruct positively` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_instruct_positively_w_rules <- reactive({})
output$table_pp_instruct_positively_w_rules <- shiny::renderTable({(table_pp_instruct_positively_w_rules())}, striped = TRUE)
output$plot_pp_instruct_positively_w_rules <- renderPlotly({plot_pp_instruct_positively_w_rules()})

table_pp_instruct_positively_w_consequence <- reactive({summary_table_habits_consequence$`Instruct positively` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_instruct_positively_w_consequence <- reactive({})
output$table_pp_instruct_positively_w_consequence <- shiny::renderTable({(table_pp_instruct_positively_w_consequence())}, striped = TRUE)
output$plot_pp_instruct_positively_w_consequence <- renderPlotly({plot_pp_instruct_positively_w_consequence()})

table_pp_instruct_positively_w_solve <- reactive({summary_table_habits_solve$`Instruct positively` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_instruct_positively_w_solve <- reactive({})
output$table_pp_instruct_positively_w_solve <- shiny::renderTable({(table_pp_instruct_positively_w_solve())}, striped = TRUE)
output$plot_pp_instruct_positively_w_solve <- renderPlotly({plot_pp_instruct_positively_w_solve()})

table_pp_instruct_positively_w_safe <- reactive({summary_table_habits_safe$`Instruct positively` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_instruct_positively_w_safe <- reactive({})
output$table_pp_instruct_positively_w_safe <- shiny::renderTable({(table_pp_instruct_positively_w_safe())}, striped = TRUE)
output$plot_pp_instruct_positively_w_safe <- renderPlotly({plot_pp_instruct_positively_w_safe()})

table_pp_instruct_positively_w_crisis <- reactive({summary_table_habits_crisis$`Instruct positively` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_instruct_positively_w_crisis <- reactive({})
output$table_pp_instruct_positively_w_crisis <- shiny::renderTable({(table_pp_instruct_positively_w_crisis())}, striped = TRUE)
output$table_pp_instruct_positively_w_crisis <- renderPlotly({table_pp_instruct_positively_w_crisis()})

table_pp_instruct_positively_w_celebrate <- reactive({summary_table_habits_celebrate$`Instruct positively` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_instruct_positively_w_celebrate <- reactive({})
output$table_pp_instruct_positively_w_celebrate <- shiny::renderTable({(table_pp_instruct_positively_w_celebrate())}, striped = TRUE)
output$table_pp_instruct_positively_w_celebrate <- renderPlotly({table_pp_instruct_positively_w_celebrate()})

# Parent Point sub tab Breathe points pp7
table_pp_breathe_ws_totals <- reactive({ summary_breathe_workshop %>% filter(Org %in% c(input$OrgPP))})
plot_pp_breathe_ws_totals <- reactive({summary_breathe_workshop_long <- summary_breathe_workshop_long %>% filter(Org %in% c(input$OrgPP)) 
ggplot(summary_breathe_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line() + labs(x = "Workshop week", y = "Number of points")})
output$table_pp_breathe_ws_totals <- shiny::renderTable({table_pp_breathe_ws_totals()})
output$plot_pp_breathe_ws_totals <- renderPlotly({plot_pp_breathe_ws_totals()})

table_pp_breathe_w_self_care <- reactive({summary_table_habits_self_care$`Breathe` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_breathe_w_self_care <- reactive({})
output$table_pp_breathe_w_self_care <- shiny::renderTable({(table_pp_breathe_w_self_care())}, striped = TRUE)
output$plot_pp_breathe_w_self_care <- renderPlotly({plot_pp_breathe_w_self_care()})

table_pp_breathe_w_1on1 <- reactive({summary_table_habits_1on1$`Breathe ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_breathe_w_1on1 <- reactive({})
output$table_pp_breathe_w_1on1 <- shiny::renderTable({(table_pp_breathe_w_1on1())}, striped = TRUE)
output$plot_pp_breathe_w_1on1 <- renderPlotly({plot_pp_breathe_w_1on1()})

table_pp_breathe_w_praise <- reactive({summary_table_habits_praise$`Breathe ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_breathe_w_praise <- reactive({})
output$table_pp_breathe_w_praise <- shiny::renderTable({(table_pp_breathe_w_praise())}, striped = TRUE)
output$plot_pp_breathe_w_praise <- renderPlotly({plot_pp_breathe_w_praise()})

table_pp_breathe_w_instruct <- reactive({summary_table_habits_instruct$`Breathe ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_breathe_w_instruct <- reactive({})
output$table_pp_breathe_w_instruct <- shiny::renderTable({(table_pp_breathe_w_instruct())}, striped = TRUE)
output$plot_pp_breathe_w_instruct <- renderPlotly({plot_pp_breathe_w_instruct()})

table_pp_breathe_w_stress <- reactive({summary_table_habits_stress$`Breathe ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_breathe_w_stress <- reactive({})
output$table_pp_breathe_w_stress <- shiny::renderTable({(table_pp_breathe_w_stress())}, striped = TRUE)
output$plot_pp_breathe_w_stress <- renderPlotly({plot_pp_breathe_w_stress()})

table_pp_breathe_w_money <- reactive({summary_table_habits_money$`Breathe ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_breathe_w_money <- reactive({})
output$table_pp_breathe_w_money <- shiny::renderTable({(table_pp_breathe_w_money())}, striped = TRUE)
output$plot_pp_breathe_w_money <- renderPlotly({plot_pp_breathe_w_money()})

table_pp_breathe_w_rules <- reactive({summary_table_habits_rules$`Breathe ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_breathe_w_rules <- reactive({})
output$table_pp_breathe_w_rules <- shiny::renderTable({(table_pp_breathe_w_rules())}, striped = TRUE)
output$plot_pp_breathe_w_rules <- renderPlotly({plot_pp_breathe_w_rules()})

table_pp_breathe_w_consequence <- reactive({summary_table_habits_consequence$`Breathe` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_breathe_w_consequence <- reactive({})
output$table_pp_breathe_w_consequence <- shiny::renderTable({(table_pp_breathe_w_consequence())}, striped = TRUE)
output$plot_pp_breathe_w_consequence <- renderPlotly({plot_pp_breathe_w_consequence()})

table_pp_breathe_w_solve <- reactive({summary_table_habits_solve$`Breathe` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_breathe_w_solve <- reactive({})
output$table_pp_breathe_w_solve <- shiny::renderTable({(table_pp_breathe_w_solve())}, striped = TRUE)
output$plot_pp_breathe_w_solve <- renderPlotly({plot_pp_breathe_w_solve()})

table_pp_breathe_w_safe <- reactive({summary_table_habits_safe$`Breathe` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_breathe_w_safe <- reactive({})
output$table_pp_breathe_w_safe <- shiny::renderTable({(table_pp_breathe_w_safe())}, striped = TRUE)
output$plot_pp_breathe_w_safe <- renderPlotly({plot_pp_breathe_w_safe()})

table_pp_breathe_w_crisis <- reactive({summary_table_habits_crisis$`Breathe` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_breathe_w_crisis <- reactive({})
output$table_pp_breathe_w_crisis <- shiny::renderTable({(table_pp_breathe_w_crisis())}, striped = TRUE)
output$table_pp_breathe_w_crisis <- renderPlotly({table_pp_breathe_w_crisis()})

table_pp_breathe_w_celebrate <- reactive({summary_table_habits_celebrate$`Breathe` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_breathe_w_celebrate <- reactive({})
output$table_pp_breathe_w_celebrate <- shiny::renderTable({(table_pp_breathe_w_celebrate())}, striped = TRUE)
output$table_pp_breathe_w_celebrate <- renderPlotly({table_pp_breathe_w_celebrate()})

# Parent Point sub tab Money points pp8
table_pp_money_ws_totals <- reactive({ summary_money_workshop %>% filter(Org %in% c(input$OrgPP))})
plot_pp_money_ws_totals <- reactive({summary_money_workshop_long <- summary_money_workshop_long %>% filter(Org %in% c(input$OrgPP)) 
ggplot(summary_money_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line() + labs(x = "Workshop week", y = "Number of points")})
output$table_pp_money_ws_totals <- shiny::renderTable({table_pp_money_ws_totals()})
output$plot_pp_money_ws_totals <- renderPlotly({plot_pp_money_ws_totals()})

table_pp_money_w_self_care <- reactive({summary_table_habits_self_care$`Money` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_money_w_self_care <- reactive({})
output$table_pp_money_w_self_care <- shiny::renderTable({(table_pp_money_w_self_care())}, striped = TRUE)
output$plot_pp_money_w_self_care <- renderPlotly({plot_pp_money_w_self_care()})

table_pp_money_w_1on1 <- reactive({summary_table_habits_1on1$`Money ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_money_w_1on1 <- reactive({})
output$table_pp_money_w_1on1 <- shiny::renderTable({(table_pp_money_w_1on1())}, striped = TRUE)
output$plot_pp_money_w_1on1 <- renderPlotly({plot_pp_money_w_1on1()})

table_pp_money_w_praise <- reactive({summary_table_habits_praise$`Money ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_money_w_praise <- reactive({})
output$table_pp_money_w_praise <- shiny::renderTable({(table_pp_money_w_praise())}, striped = TRUE)
output$plot_pp_money_w_praise <- renderPlotly({plot_pp_money_w_praise()})

table_pp_money_w_instruct <- reactive({summary_table_habits_instruct$`Money ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_money_w_instruct <- reactive({})
output$table_pp_money_w_instruct <- shiny::renderTable({(table_pp_money_w_instruct())}, striped = TRUE)
output$plot_pp_money_w_instruct <- renderPlotly({plot_pp_money_w_instruct()})

table_pp_money_w_stress <- reactive({summary_table_habits_stress$`Money ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_money_w_stress <- reactive({})
output$table_pp_money_w_stress <- shiny::renderTable({(table_pp_money_w_stress())}, striped = TRUE)
output$plot_pp_money_w_stress <- renderPlotly({plot_pp_money_w_stress()})

table_pp_money_w_money <- reactive({summary_table_habits_money$`Money ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_money_w_money <- reactive({})
output$table_pp_money_w_money <- shiny::renderTable({(table_pp_money_w_money())}, striped = TRUE)
output$plot_pp_money_w_money <- renderPlotly({plot_pp_money_w_money()})

table_pp_money_w_rules <- reactive({summary_table_habits_rules$`Money ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_money_w_rules <- reactive({})
output$table_pp_money_w_rules <- shiny::renderTable({(table_pp_money_w_rules())}, striped = TRUE)
output$plot_pp_money_w_rules <- renderPlotly({plot_pp_money_w_rules()})

table_pp_money_w_consequence <- reactive({summary_table_habits_consequence$`Money` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_money_w_consequence <- reactive({})
output$table_pp_money_w_consequence <- shiny::renderTable({(table_pp_money_w_consequence())}, striped = TRUE)
output$plot_pp_money_w_consequence <- renderPlotly({plot_pp_money_w_consequence()})

table_pp_money_w_solve <- reactive({summary_table_habits_solve$`Money` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_money_w_solve <- reactive({})
output$table_pp_money_w_solve <- shiny::renderTable({(table_pp_money_w_solve())}, striped = TRUE)
output$plot_pp_money_w_solve <- renderPlotly({plot_pp_money_w_solve()})

table_pp_money_w_safe <- reactive({summary_table_habits_safe$`Money` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_money_w_safe <- reactive({})
output$table_pp_money_w_safe <- shiny::renderTable({(table_pp_money_w_safe())}, striped = TRUE)
output$plot_pp_money_w_safe <- renderPlotly({plot_pp_money_w_safe()})

table_pp_money_w_crisis <- reactive({summary_table_habits_crisis$`Money` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_money_w_crisis <- reactive({})
output$table_pp_money_w_crisis <- shiny::renderTable({(table_pp_money_w_crisis())}, striped = TRUE)
output$table_pp_money_w_crisis <- renderPlotly({table_pp_money_w_crisis()})

table_pp_money_w_celebrate <- reactive({summary_table_habits_celebrate$`Money` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_money_w_celebrate <- reactive({})
output$table_pp_money_w_celebrate <- shiny::renderTable({(table_pp_money_w_celebrate())}, striped = TRUE)
output$table_pp_money_w_celebrate <- renderPlotly({table_pp_money_w_celebrate()})

# Parent Point sub tab Consequence points pp9
table_pp_consequence_ws_totals <- reactive({ summary_consequence_workshop %>% filter(Org %in% c(input$OrgPP))})
plot_pp_consequence_ws_totals <- reactive({summary_consequence_workshop_long <- summary_consequence_workshop_long %>% filter(Org %in% c(input$OrgPP)) 
ggplot(summary_consequence_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line() + labs(x = "Workshop week", y = "Number of points")})
output$table_pp_consequence_ws_totals <- shiny::renderTable({table_pp_consequence_ws_totals()})
output$plot_pp_consequence_ws_totals <- renderPlotly({plot_pp_consequence_ws_totals()})

table_pp_consequence_w_self_care <- reactive({summary_table_habits_self_care$`Consequence` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_consequence_w_self_care <- reactive({})
output$table_pp_consequence_w_self_care <- shiny::renderTable({(table_pp_consequence_w_self_care())}, striped = TRUE)
output$plot_pp_consequence_w_self_care <- renderPlotly({plot_pp_consequence_w_self_care()})

table_pp_consequence_w_1on1 <- reactive({summary_table_habits_1on1$`Consequence ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_consequence_w_1on1 <- reactive({})
output$table_pp_consequence_w_1on1 <- shiny::renderTable({(table_pp_consequence_w_1on1())}, striped = TRUE)
output$plot_pp_consequence_w_1on1 <- renderPlotly({plot_pp_consequence_w_1on1()})

table_pp_consequence_w_praise <- reactive({summary_table_habits_praise$`Consequence ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_consequence_w_praise <- reactive({})
output$table_pp_consequence_w_praise <- shiny::renderTable({(table_pp_consequence_w_praise())}, striped = TRUE)
output$plot_pp_consequence_w_praise <- renderPlotly({plot_pp_consequence_w_praise()})

table_pp_consequence_w_instruct <- reactive({summary_table_habits_instruct$`Consequence ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_consequence_w_instruct <- reactive({})
output$table_pp_consequence_w_instruct <- shiny::renderTable({(table_pp_consequence_w_instruct())}, striped = TRUE)
output$plot_pp_consequence_w_instruct <- renderPlotly({plot_pp_consequence_w_instruct()})

table_pp_consequence_w_stress <- reactive({summary_table_habits_stress$`Consequence ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_consequence_w_stress <- reactive({})
output$table_pp_consequence_w_stress <- shiny::renderTable({(table_pp_consequence_w_stress())}, striped = TRUE)
output$plot_pp_consequence_w_stress <- renderPlotly({plot_pp_consequence_w_stress()})

table_pp_consequence_w_money <- reactive({summary_table_habits_money$`Consequence ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_consequence_w_money <- reactive({})
output$table_pp_consequence_w_money <- shiny::renderTable({(table_pp_consequence_w_money())}, striped = TRUE)
output$plot_pp_consequence_w_money <- renderPlotly({plot_pp_consequence_w_money()})

table_pp_consequence_w_rules <- reactive({summary_table_habits_rules$`Consequence ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_consequence_w_rules <- reactive({})
output$table_pp_consequence_w_rules <- shiny::renderTable({(table_pp_consequence_w_rules())}, striped = TRUE)
output$plot_pp_consequence_w_rules <- renderPlotly({plot_pp_consequence_w_rules()})

table_pp_consequence_w_consequence <- reactive({summary_table_habits_consequence$`Consequence` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_consequence_w_consequence <- reactive({})
output$table_pp_consequence_w_consequence <- shiny::renderTable({(table_pp_consequence_w_consequence())}, striped = TRUE)
output$plot_pp_consequence_w_consequence <- renderPlotly({plot_pp_consequence_w_consequence()})

table_pp_consequence_w_solve <- reactive({summary_table_habits_solve$`Consequence` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_consequence_w_solve <- reactive({})
output$table_pp_consequence_w_solve <- shiny::renderTable({(table_pp_consequence_w_solve())}, striped = TRUE)
output$plot_pp_money_w_solve <- renderPlotly({plot_pp_consequence_w_solve()})

table_pp_consequence_w_safe <- reactive({summary_table_habits_safe$`Consequence` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_consequence_w_safe <- reactive({})
output$table_pp_consequence_w_safe <- shiny::renderTable({(table_pp_consequence_w_safe())}, striped = TRUE)
output$plot_pp_consequence_w_safe <- renderPlotly({plot_pp_consequence_w_safe()})

table_pp_consequence_w_crisis <- reactive({summary_table_habits_crisis$`Consequence` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_consequence_w_crisis <- reactive({})
output$table_pp_consequence_w_crisis <- shiny::renderTable({(table_pp_consequence_w_crisis())}, striped = TRUE)
output$table_pp_consequence_w_crisis <- renderPlotly({table_pp_consequence_w_crisis()})

table_pp_consequence_w_celebrate <- reactive({summary_table_habits_celebrate$`Consequence` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_consequence_w_celebrate <- reactive({})
output$table_pp_consequence_w_celebrate <- shiny::renderTable({(table_pp_consequence_w_celebrate())}, striped = TRUE)
output$table_pp_consequence_w_celebrate <- renderPlotly({table_pp_consequence_w_celebrate()})

# Parent Point sub tab Safe points pp10
table_pp_safe_ws_totals <- reactive({ summary_safe_workshop %>% filter(Org %in% c(input$OrgPP))})
plot_pp_safe_ws_totals <- reactive({summary_safe_workshop_long <- summary_safe_workshop_long %>% filter(Org %in% c(input$OrgPP)) 
ggplot(summary_safe_workshop_long, aes(x = name, y = value, colour = Org, shape = Org, group = Org)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line() + labs(x = "Workshop week", y = "Number of points")})
output$table_pp_safe_ws_totals <- shiny::renderTable({table_pp_safe_ws_totals()})
output$plot_pp_safe_ws_totals <- renderPlotly({plot_pp_safe_ws_totals()})

table_pp_safe_w_self_care <- reactive({summary_table_habits_self_care$`Safe` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_safe_w_self_care <- reactive({})
output$table_pp_safe_w_self_care <- shiny::renderTable({(table_pp_safe_w_self_care())}, striped = TRUE)
output$plot_pp_safe_w_self_care <- renderPlotly({plot_pp_safe_w_self_care()})

table_pp_safe_w_1on1 <- reactive({summary_table_habits_1on1$`Safe ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_safe_w_1on1 <- reactive({})
output$table_pp_safe_w_1on1 <- shiny::renderTable({(table_pp_safe_w_1on1())}, striped = TRUE)
output$plot_pp_safe_w_1on1 <- renderPlotly({plot_pp_safe_w_1on1()})

table_pp_safe_w_praise <- reactive({summary_table_habits_praise$`Safe ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_safe_w_praise <- reactive({})
output$table_pp_safe_w_praise <- shiny::renderTable({(table_pp_safe_w_praise())}, striped = TRUE)
output$plot_pp_safe_w_praise <- renderPlotly({plot_pp_safe_w_praise()})

table_pp_safe_w_instruct <- reactive({summary_table_habits_instruct$`Safe ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_safe_w_instruct <- reactive({})
output$table_pp_safe_w_instruct <- shiny::renderTable({(table_pp_safe_w_instruct())}, striped = TRUE)
output$plot_pp_safe_w_instruct <- renderPlotly({plot_pp_safe_w_instruct()})

table_pp_safe_w_stress <- reactive({summary_table_habits_stress$`Safe ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_safe_w_stress <- reactive({})
output$table_pp_safe_w_stress <- shiny::renderTable({(table_pp_safe_w_stress())}, striped = TRUE)
output$plot_pp_safe_w_stress <- renderPlotly({plot_pp_safe_w_stress()})

table_pp_safe_w_money <- reactive({summary_table_habits_money$`Safe ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_safe_w_money <- reactive({})
output$table_pp_safe_w_money <- shiny::renderTable({(table_pp_safe_w_money())}, striped = TRUE)
output$plot_pp_safe_w_money <- renderPlotly({plot_pp_safe_w_money()})

table_pp_safe_w_rules <- reactive({summary_table_habits_rules$`Safe ` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_safe_w_rules <- reactive({})
output$table_pp_safe_w_rules <- shiny::renderTable({(table_pp_safe_w_rules())}, striped = TRUE)
output$plot_pp_safe_w_rules <- renderPlotly({plot_pp_safe_w_rules()})

table_pp_safe_w_consequence <- reactive({summary_table_habits_consequence$`Safe` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_safe_w_consequence <- reactive({})
output$table_pp_safe_w_consequence <- shiny::renderTable({(table_pp_safe_w_consequence())}, striped = TRUE)
output$plot_pp_safe_w_consequence <- renderPlotly({plot_pp_safe_w_consequence()})

table_pp_safe_w_solve <- reactive({summary_table_habits_solve$`Safe` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_safe_w_solve <- reactive({})
output$table_pp_safe_w_solve <- shiny::renderTable({(table_pp_safe_w_solve())}, striped = TRUE)
output$plot_pp_money_w_solve <- renderPlotly({plot_pp_safe_w_solve()})

table_pp_safe_w_safe <- reactive({summary_table_habits_safe$`Safe` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_safe_w_safe <- reactive({})
output$table_pp_safe_w_safe <- shiny::renderTable({(table_pp_safe_w_safe())}, striped = TRUE)
output$plot_pp_safe_w_safe <- renderPlotly({plot_pp_safe_w_safe()})

table_pp_safe_w_crisis <- reactive({summary_table_habits_crisis$`Safe` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_safe_w_crisis <- reactive({})
output$table_pp_safe_w_crisis <- shiny::renderTable({(table_pp_safe_w_crisis())}, striped = TRUE)
output$table_pp_safe_w_crisis <- renderPlotly({table_pp_safe_w_crisis()})

table_pp_safe_w_celebrate <- reactive({summary_table_habits_celebrate$`Safe` %>% filter(Org %in% c((input$OrgPP)))})
plot_pp_safe_w_celebrate <- reactive({})
output$table_pp_safe_w_celebrate <- shiny::renderTable({(table_pp_safe_w_celebrate())}, striped = TRUE)
output$table_pp_safe_w_celebrate <- renderPlotly({table_pp_safe_w_celebrate()})

  
#FOURTH Tab In-week Engagement

selected_data_xe <- reactive({
  plhdata_checkgroup <- plhdata_org_clean %>% filter(Org %in% c((input$OrgXE)))
  return(plhdata_checkgroup)
})
  
#App Opens tab 4.1

  table_appopen_totals <- reactive({
    tables_app_opens$`Total` %>% filter(Org %in% c((input$OrgXE)))}) 
  plot_appopen_totals <- reactive({summary_plot(data = selected_data_xe(), columns_to_summarise = "rp.contact.field.app_launch_count", replace = "rp.contact.field.")}) 
  output$table_appopen_totals <- shiny::renderTable({(table_appopen_totals())}, striped = TRUE)
  output$plot_appopen_totals <- renderPlotly({plot_appopen_totals()})

  table_appopen_mean_week <- reactive({ summary_mean_appopens %>% filter(Org %in% c((input$OrgXE)))
  }) 
  plot_appopen_mean_week <- reactive({
    summary_mean_appopens_long <- summary_mean_appopens %>% filter(Org %in% c((input$OrgXE))) %>%
      pivot_longer(cols = !Org, names_to = "Workshop Week", values_to = "Value") %>% mutate(`Workshop Week` = fct_relevel(`Workshop Week`, data_app_opens_neat))
    ggplot(summary_mean_appopens_long, aes(x = `Workshop Week`, y = Value, fill = Org)) + 
          geom_bar(stat = "identity", position = "dodge") +
          scale_x_discrete(guide = guide_axis(angle = 90), limits = data_app_opens_neat) +
          viridis::scale_fill_viridis(discrete = TRUE)
    }) 
  output$table_appopen_mean_week <- shiny::renderTable({(table_appopen_mean_week())}, striped = TRUE)
  output$plot_appopen_mean_week <- renderPlotly({plot_appopen_mean_week()})
  
# Push notifications tab 4.2
  
  table_pushn_mean <- reactive({
  }) 
  plot_pushn_mean <- reactive({
  }) 
  output$table_pushn_mean <- shiny::renderTable({(table_pushn_mean())}, striped = TRUE)
  output$plot_pushn_mean <- renderPlotly({plot_pushn_mean()})
  
  table_pushn_totals <- reactive({pn_summary_count %>% filter(Org %in% c((input$OrgXE)))}) 
  plot_pushn_totals <- reactive({
  }) 
  output$table_pushn_totals <- shiny::renderTable({(table_pushn_totals())}, striped = TRUE)
  output$plot_pushn_totals <- renderPlotly({plot_pushn_totals()})
  

  #FIFTH Tab Surveys
  
  selected_data_sv1 <- reactive({
    plhdata_checkgroup <- plhdata_org_clean %>% filter(Org %in% c((input$OrgSV1)))
    return(plhdata_checkgroup)
  })
  
  # Baseline survey completion levels
  table_sv1_totals <- reactive({
    summary_table_survey_completion %>% filter(Org %in% c((input$OrgSV1))) })
  plot_sv1_totals <- reactive({ summary_plot(data = selected_data_sv1(), columns_to_summarise = "rp.contact.field.survey_welcome_and_setup_completion_level", replace = "rp.contact.field.") }) 
  output$table_sv1_totals <- shiny::renderTable({(table_sv1_totals())}, striped = TRUE)
  output$plot_sv1_totals <- renderPlotly({plot_sv1_totals()})
  
  # sv1_attention a_1
  table_sv1_attention <- reactive({
    summary_table_survey_past_week$Attention %>% filter(Org %in% c((input$OrgSV1))) }) 
  plot_sv1_attention  <- reactive({
    summary_plot(data = selected_data_sv1(), columns_to_summarise = "rp.contact.field.survey_welcome_a_1_final", replace = "rp.contact.field.")}) 
  output$table_sv1_attention <- shiny::renderTable({(table_sv1_attention())}, striped = TRUE,
  caption = "no_value = selected 'choose not to answer'; <br> null = skipped using the navigation buttons; <br> NA = survey not accessed or data not synced")
  output$plot_sv1_attention <- renderPlotly({plot_sv1_attention()})
  
  # sv1_praise a_2
  table_sv1_praise <- reactive({
    summary_table_survey_past_week$Praise %>% filter(Org %in% c((input$OrgSV1))) }) 
  plot_sv1_praise  <- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.survey_welcome_a_2_final", replace = "rp.contact.field.")})
  output$table_sv1_praise <- shiny::renderTable({(table_sv1_praise())}, striped = TRUE,
                                                caption = "no_value = selected 'choose not to answer'; <br> null = skipped using the navigation buttons; <br> NA = survey not accessed or data not synced")
  output$plot_sv1_praise <- renderPlotly({plot_sv1_praise()})
  
  # sv1_stress a_3
  table_sv1_stress <- reactive({
    summary_table_survey_past_week$Stress %>% filter(Org %in% c((input$OrgSV1))) }) 
  plot_sv1_stress  <- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.survey_welcome_a_3_final", replace = "rp.contact.field.")})
  output$table_sv1_stress <- shiny::renderTable({(table_sv1_stress())}, striped = TRUE,
                                                caption = "no_value = selected 'choose not to answer'; <br> null = skipped using the navigation buttons; <br> NA = survey not accessed or data not synced")
  output$plot_sv1_stress <- renderPlotly({plot_sv1_stress()})
  
  # sv1_shout a_4
  table_sv1_shout <- reactive({
    summary_table_survey_past_week$Shouting %>% filter(Org %in% c((input$OrgSV1))) }) 
  plot_sv1_shout  <- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.survey_welcome_a_4_final", replace = "rp.contact.field.")})
  output$table_sv1_shout <- shiny::renderTable({(table_sv1_shout())}, striped = TRUE,
                                               caption = "no_value = selected 'choose not to answer'; <br> null = skipped using the navigation buttons; <br> NA = survey not accessed or data not synced")
  output$plot_sv1_shout <- renderPlotly({plot_sv1_shout()})
  
  # sv1_money a_5 p1
  table_sv1_money <- reactive({
    summary_table_survey_past_week$`Money worries` %>% filter(Org %in% c((input$OrgSV1))) }) 
  plot_sv1_money  <- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.survey_welcome_a_5_part_1_final", replace = "rp.contact.field.")})
  output$table_sv1_money <- shiny::renderTable({(table_sv1_money())}, striped = TRUE,
                                               caption = "no_value = selected 'choose not to answer'; <br> null = skipped using the navigation buttons; <br> NA = survey not accessed or data not synced")
  output$plot_sv1_money <- renderPlotly({plot_sv1_money()})
  
  # sv1_food_money a_5 
  table_sv1_food_money <- reactive({
    summary_table_survey_past_week$Summary %>% filter(Org %in% c((input$OrgSV1))) }) 
  plot_sv1_food_money  <- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.survey_welcome_a_5_part_2_final", replace = "rp.contact.field.")}) 
  output$table_sv1_food_money <- shiny::renderTable({(table_sv1_food_money())}, striped = TRUE,
                                                    caption = "no_value = selected 'choose not to answer'; <br> null = skipped using the navigation buttons; <br> NA = survey not accessed or data not synced")
  output$plot_sv1_food_money <- renderPlotly({plot_sv1_food_money()})
  
  # sv1_hitting a_6
  table_sv1_hitting <- reactive({
    summary_table_survey_past_week$Hitting %>% filter(Org %in% c((input$OrgSV1))) }) 
  plot_sv1_hitting  <- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.survey_welcome_a_6_final", replace = "rp.contact.field.")}) 
  output$table_sv1_hitting <- shiny::renderTable({(table_sv1_hitting())}, striped = TRUE,
                                                 caption = "no_value = selected 'choose not to answer'; <br> null = skipped using the navigation buttons; <br> NA = survey not accessed or data not synced")
  output$plot_sv1_hitting <- renderPlotly({plot_sv1_hitting()})
  
  # sv1_week_teen_activity a_7 p1
  table_sv1_week_teen_activity <- reactive({
    summary_table_survey_past_week$`Teen activity` %>% filter(Org %in% c((input$OrgSV1))) }) 
  plot_sv1_week_teen_activity  <- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.survey_welcome_a_7_part_1_final", replace = "rp.contact.field.")})
  output$table_sv1_week_teen_activity <- shiny::renderTable({(table_sv1_week_teen_activity())}, striped = TRUE,
                                                            caption = "no_value = selected 'choose not to answer'; <br> null = skipped using the navigation buttons; <br> NA = survey not accessed or data not synced")
  output$plot_sv1_week_teen_activity <- renderPlotly({plot_sv1_week_teen_activity()})
  
  # sv1_lockdown a_7 p2
  table_sv1_lockdown <- reactive({
    summary_table_survey_past_week$`Lockdown?` %>% filter(Org %in% c((input$OrgSV1))) }) 
  plot_sv1_lockdown  <- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.survey_welcome_a_7_part_2_final", replace = "rp.contact.field.")})
  output$table_sv1_lockdown <- shiny::renderTable({(table_sv1_lockdown())}, striped = TRUE,
                                                  caption = "no_value = selected 'choose not to answer'; <br> null = skipped using the navigation buttons; <br> NA = survey not accessed or data not synced")
  output$plot_sv1_lockdown <- renderPlotly({plot_sv1_lockdown()})
  
  # sv1_reg_teen_activity a_7 p3
  table_sv1_reg_teen_activity <- reactive({
    summary_table_survey_past_week$`Knowledge of teen activity in non-lockdown week` %>% filter(Org %in% c((input$OrgSV1))) }) 
  plot_sv1_reg_teen_activity  <- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.survey_welcome_a_7_part_3_final", replace = "rp.contact.field.")}) 
  output$table_sv1_reg_teen_activity <- shiny::renderTable({(table_sv1_reg_teen_activity())}, striped = TRUE,
                                                           caption = "no_value = selected 'choose not to answer'; <br> null = skipped using the navigation buttons; <br> NA = survey not accessed or data not synced")
  output$plot_sv1_reg_teen_activity <- renderPlotly({plot_sv1_reg_teen_activity()})
  
  # sv1_sex_talk a_8
  table_sv1_sex_talk <- reactive({
    summary_table_survey_past_week$`Sexual safety talk` %>% filter(Org %in% c((input$OrgSV1))) }) 
  plot_sv1_sex_talk  <- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.survey_welcome_a_8_final", replace = "rp.contact.field.")})
  output$table_sv1_sex_talk <- shiny::renderTable({(table_sv1_sex_talk())}, striped = TRUE,
                                                  caption = "no_value = selected 'choose not to answer'; <br> null = skipped using the navigation buttons; <br> NA = survey not accessed or data not synced")
  output$plot_sv1_sex_talk <- renderPlotly({plot_sv1_sex_talk()})
  
  # sv1_covid_safe a_9
  table_sv1_covid_safe <- reactive({
    summary_table_survey_past_week$`Teen COVID safe` %>% filter(Org %in% c((input$OrgSV1))) }) 
  plot_sv1_covid_safe  <- reactive({
    summary_plot(plhdata_org_clean, "rp.contact.field.survey_welcome_a_9_final", replace = "rp.contact.field.")})
  output$table_sv1_covid_safe <- shiny::renderTable({(table_sv1_covid_safe())}, striped = TRUE,
                                                    caption = "no_value = selected 'choose not to answer'; <br> null = skipped using the navigation buttons; <br> NA = survey not accessed or data not synced")
  output$plot_sv1_covid_safe <- renderPlotly({plot_sv1_covid_safe()})
  
  
  #SIXTH Tab Parent Library

  selected_data_lb <- reactive({
    plhdata_checkgroup <- plhdata_org_clean %>% filter(Org %in% c((input$OrgLB)))
    return(plhdata_checkgroup)})
  
  #average clicks on parent library (mean per org)
  table_library_mean <- reactive({summary_library_mean %>% filter(Org %in% c((input$OrgLB))) })
  plot_library_mean  <- reactive({summary_library_mean_long <- pivot_longer(summary_library_mean, cols = !Org, names_to = "Library", values_to = "Clicks")
  ggplot(summary_library_mean_long, aes(x = Library , y = Clicks, fill = Org)) + 
    geom_bar(stat = "identity", position = "dodge") +
    # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_discrete(guide = guide_axis(angle = 90), limits = week_order) +
    viridis::scale_fill_viridis(discrete = TRUE) 
    
  })
  output$table_library_mean <- shiny::renderTable({(table_library_mean())}, striped = TRUE)
  output$plot_library_mean <- renderPlotly({plot_library_mean()})
  
  #total clicks on parent library from homescreen
  table_library_totals <- reactive({summary_table_library$` parent centre ` %>% filter(Org %in% c((input$OrgLB))) })
  plot_library_totals  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_hs_parent_centre_count", replace = "rp.contact.field.click_hs_")})
  output$table_library_totals <- shiny::renderTable({(table_library_totals())}, striped = TRUE)
  output$plot_library_totals <- renderPlotly({plot_library_totals()})

  #my tips
  table_lib_tips <- reactive({summary_table_library$`My tips ` %>% filter(Org %in% c((input$OrgLB))) }) 
  plot_lib_tips  <- reactive({summary_plot(data= selected_data_lb(), "rp.contact.field.click_pc_my_tips_count", replace = "rp.contact.field.click_pc_")})
  output$table_lib_tips <- shiny::renderTable({(table_lib_tips())}, striped = TRUE)
  output$plot_lib_tips <- renderPlotly({plot_lib_tips()})
  
  #essential tools
  table_lib_tools <- reactive({summary_table_library$`Essential tools ` %>% filter(Org %in% c((input$OrgLB))) }) 
  plot_lib_tools  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_essential_tools_count", replace = "rp.contact.field.click_pc_")})
  output$table_lib_tools <- shiny::renderTable({(table_lib_tools())}, striped = TRUE)
  output$plot_lib_tools <- renderPlotly({plot_lib_tools()})
  
  #relax and activities
  table_lib_activities <- reactive({summary_table_library$`Relax and activities ` %>% filter(Org %in% c((input$OrgLB))) }) 
  plot_lib_activities  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_relax_and_activities_count", replace = "rp.contact.field.click_pc_")})
  output$table_lib_activities <- shiny::renderTable({(table_lib_activities())}, striped = TRUE)
  output$plot_lib_activities <- renderPlotly({plot_lib_activities()})
  
  #customisse ParentApp
  table_lib_custom <- reactive({summary_table_library$`Customisation ` %>% filter(Org %in% c((input$OrgLB))) }) 
  plot_lib_custom  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_customisation_count", replace = "rp.contact.field.click_pc_")})
  output$table_lib_custom <- shiny::renderTable({(table_lib_custom())}, striped = TRUE)
  output$plot_lib_custom <- renderPlotly({plot_lib_custom()})
  
  #help - quick parenting tips
  table_lib_help <- reactive({summary_table_library$`Help ` %>% filter(Org %in% c((input$OrgLB))) }) 
  plot_lib_help  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_help_count", replace = "rp.contact.field.click_pc_")})
  output$table_lib_help <- shiny::renderTable({(table_lib_help())}, striped = TRUE)
  output$plot_lib_help <- renderPlotly({plot_lib_help()})
  
  #technical support
  table_lib_tech <- reactive({summary_table_library$`Technical support ` %>% filter(Org %in% c((input$OrgLB))) }) 
  plot_lib_tech  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_technical_support_count", replace = "rp.contact.field.click_pc_")})
  output$table_lib_tech <- shiny::renderTable({(table_lib_tech())}, striped = TRUE)
  output$plot_lib_tech <- renderPlotly({plot_lib_tech()})
  
  #coping with COVID 
  table_lib_covid <- reactive({summary_table_library$`Covid ` %>% filter(Org %in% c((input$OrgLB))) }) 
  plot_lib_covid  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_covid_count", replace = "rp.contact.field.click_pc_")})
  output$table_lib_covid <- shiny::renderTable({(table_lib_covid())}, striped = TRUE)
  output$plot_lib_covid <- renderPlotly({plot_lib_covid()})
  
  #coping with grief / bereavement
  table_lib_grief <- reactive({summary_table_library$`Bereavement ` %>% filter(Org %in% c((input$OrgLB))) }) 
  plot_lib_grief  <- reactive({summary_plot(plhdata_org_clean, "rp.contact.field.click_pc_bereavement_count", replace = "rp.contact.field.click_pc_")})
  output$table_lib_grief <- shiny::renderTable({(table_lib_grief())}, striped = TRUE)
  output$plot_lib_grief <- renderPlotly({plot_lib_grief()})
  
} #close server


# 5. Create Shiny App -----------------------------------------------------------------------------
shinyApp(ui, server)
