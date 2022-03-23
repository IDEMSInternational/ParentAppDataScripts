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
                        # FIRST tab content
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
                                      checkboxGroupInput(inputId = "OrgDem",
                                                         label = "Organisations to show:",
                                                         choices = c("Amathuba" = "Amathuba",
                                                                     "Dlalanathi" = "Dlalanathi",
                                                                     "Joy" = "Joy",
                                                                     "Nontobeko" = "Nontobeko"),
                                                         selected = c("Amathuba","Dlalanathi",
                                                                      "Joy","Nontobeko")
                                      ))), #closes fluidRow
                                
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
                        
                        # SECOND tab content
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
                                  box(width = 12,
                                      checkboxGroupInput(inputId = "OrgWS",
                                                         label = "Organisations to show:",
                                                         choices = c("Amathuba" = "Amathuba",
                                                                     "Dlalanathi" = "Dlalanathi",
                                                                     "Joy" = "Joy",
                                                                     "Nontobeko" = "Nontobeko"),
                                                         selected = c("Amathuba","Dlalanathi",
                                                                      "Joy","Nontobeko")
                                      ))), #closes fluidRow
                                
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
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_self_care", height = "240"),
                                      shiny::tableOutput("table_w_self_care")
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 2: One-on-One Time",
                                      status = "info",  
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
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_praise", height = "240"),
                                      shiny::tableOutput("table_w_praise")
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 4: Positive Instructions",
                                      status = "info",  
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
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_stress", height = "240"),
                                      shiny::tableOutput("table_w_stress")
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 6: Family Budgets",
                                      status = "info",  
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
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_rules", height = "240"),
                                      shiny::tableOutput("table_w_rules")
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 8: Calm Consequences",
                                      status = "info",  
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
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_solve", height = "240"),
                                      shiny::tableOutput("table_w_solve")
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 10: Teen Safety",
                                      status = "info",  
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
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_crisis", height = "240"),
                                      shiny::tableOutput("table_w_crisis")
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Workshop 12: Celebration and Next Steps",
                                      status = "info",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_w_celebrate", height = "240"),
                                      shiny::tableOutput("table_w_celebrate")
                                  ) #closes box
                                ) #closes fluid row
                        ), #closes tabItem
                        
                        # THIRD tab content
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
                                  box(width = 6,
                                      checkboxGroupInput(inputId = "OrgPP",
                                                         label = "Organisations to show:",
                                                         choices = c("Amathuba" = "Amathuba",
                                                                     "Dlalanathi" = "Dlalanathi",
                                                                     "Joy" = "Joy",
                                                                     "Nontobeko" = "Nontobeko"),
                                                         selected = c("Amathuba","Dlalanathi",
                                                                      "Joy","Nontobeko")
                                      )),
                                  box(width = 6,
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
                                                         selected = c("relax"),
                                                         inline = TRUE
                                      ))),
                                
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
                                  box(width = 6,
                                      checkboxGroupInput(inputId = "OrgXE",
                                                         label = "Organisations to show:",
                                                         choices = c("Amathuba" = "Amathuba",
                                                                     "Dlalanathi" = "Dlalanathi",
                                                                     "Joy" = "Joy",
                                                                     "Nontobeko" = "Nontobeko"),
                                                         selected = c("Amathuba","Dlalanathi",
                                                                      "Joy","Nontobeko")
                                      )), #closes box
                                  box(width = 6,
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
                                                                     "10	Teen Safety" = "w_crisis",
                                                                     "11 Dealing with Crisis" = "w_praise",
                                                                     "12 Celebration and Next Steps" = "w_celebrate"),
                                                         selected = c("w_self_care", "w_1on1", "w_praise", "w_instruct","w_stress","w_money", "w_rules", "w_consequence", "w_solve", "safe", "w_praise", "w_celebrate"),
                                                         inline = TRUE)
                                  )), #closes box and fluid row
                                
                                fluidRow(
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Cumulative app opens",
                                      status = "success",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_appopen_totals", height = "240"),
                                      shiny::tableOutput("table_appopen_totals")
                                  ), #closes box
                                  
                                  box(width = 6,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Cumulative push notification clicks",
                                      status = "success",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_pp_1on1", height = "240"),
                                      shiny::tableOutput("table_pp_1on1")
                                  ) #closes box
                                ), #closes fluid row
                                
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
                                ), #closes fluid row
                                
                                fluidRow(
                                  box(width = 12,
                                      collapsible = FALSE,
                                      solidHeader = TRUE,
                                      title = "Mean pushnotifiation clicks per workshop week",
                                      status = "success",  
                                      #background = "orange",
                                      plotlyOutput(outputId = "plot_pn_mean_week", height = "240"),
                                      shiny::tableOutput("table_pn_mean_week")
                                  )#closes box
                                ) #closes fluid row
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
                                                       box(width = 12,
                                                           checkboxGroupInput(inputId = "OrgSV1",
                                                                              label = "Organisations to show:",
                                                                              choices = c("Amathuba" = "Amathuba",
                                                                                          "Dlalanathi" = "Dlalanathi",
                                                                                          "Joy" = "Joy",
                                                                                          "Nontobeko" = "Nontobeko"),
                                                                              selected = c("Amathuba","Dlalanathi",
                                                                                           "Joy","Nontobeko")
                                                           ))),#closes group, box and fluidRow
                                                     
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
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_attention", height = "240"),
                                                           shiny::tableOutput("table_sv1_attention")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days of praise",
                                                           status = "danger",  
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
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_stress", height = "240"),
                                                           shiny::tableOutput("table_sv1_stress")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days of shouting",
                                                           status = "danger",  
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
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_money", height = "240"),
                                                           shiny::tableOutput("table_sv1_money")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days out of food money (last month)",
                                                           status = "danger",  
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
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_hitting", height = "240"),
                                                           shiny::tableOutput("table_sv1_hitting")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days with knowledge of teen activity",
                                                           status = "danger",  
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
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_lockdown", height = "240"),
                                                           shiny::tableOutput("table_sv1_lockdown")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Non-lockdown knowledge of teen activity",
                                                           status = "danger",  
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
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_sex_talk", height = "240"),
                                                           shiny::tableOutput("table_sv1_sex_talk")
                                                       ), #closes box
                                                       
                                                       box(width = 6,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Days of COVID safe teenager behaviour",
                                                           status = "danger",  
                                                           #background = "orange",
                                                           plotlyOutput(outputId = "plot_sv1_covid_safe", height = "240"),
                                                           shiny::tableOutput("table_sv1_covid_safe")
                                                       ) #closes box
                                                     )#closes fluid row
                                            ), #closes baseline tab panel
                                            
                                            
                                            tabPanel("Midline survey",
                                                     fluidRow(
                                                       box(width = 12,
                                                           checkboxGroupInput(inputId = "OrgSV2",
                                                                              label = "Organisations to show:",
                                                                              choices = c("Amathuba" = "Amathuba",
                                                                                          "Dlalanathi" = "Dlalanathi",
                                                                                          "Joy" = "Joy",
                                                                                          "Nontobeko" = "Nontobeko"),
                                                                              selected = c("Amathuba","Dlalanathi",
                                                                                           "Joy","Nontobeko")
                                                           )) #closes box
                                                     ), #closes fluidRow
                                                     
                                                     fluidRow(
                                                       box(width = 12,
                                                           collapsible = FALSE,
                                                           solidHeader = TRUE,
                                                           title = "Midline survey completion",
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
                                  box(width = 12,
                                      checkboxGroupInput(inputId = "OrgSV2",
                                                         label = "Organisations to show:",
                                                         choices = c("Amathuba" = "Amathuba",
                                                                     "Dlalanathi" = "Dlalanathi",
                                                                     "Joy" = "Joy",
                                                                     "Nontobeko" = "Nontobeko"),
                                                         selected = c("Amathuba","Dlalanathi",
                                                                      "Joy","Nontobeko")
                                      ))) #closes fluidRow
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
                              color = "yellow")})
  output$myvaluebox3 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox( nrow(plhdata_org_clean %>% filter(Org == "Dlalanathi")), subtitle = "Dlalanathi", icon = icon("user"),
                              color = "red")})
  output$myvaluebox4 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox( nrow(plhdata_org_clean %>% filter(Org == "Joy")), subtitle = "Joy", icon = icon("user"),
                              color = "purple")})
  output$myvaluebox5 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox( nrow(plhdata_org_clean %>% filter(Org == "Nontobeko")), subtitle = "Nontobeko", icon = icon("user"),
                              color = "teal")})
  
  
  #FIRST tab DEMOGRAPHICS
  
  #Languages plot and table
  table_language <- reactive({
    summary_table_baseline$` app language` %>% filter(Org %in% c((input$OrgDem)))
  }) 
  plot_language  <- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field._app_language) }) 
  
  output$table_language <- shiny::renderTable({(table_language())}, striped = TRUE)
  output$plot_language <- renderPlotly({plot_language()})
  
  #Workshop format plot and table
  table_ws_format <- reactive({
    summary_table_baseline$`Do workshops together` %>% filter(Org %in% c((input$OrgDem)))
  }) 
  plot_ws_format  <- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.do_workshops_together) }) 
  
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
  
  
  #Parent gender plot and table
  table_parent_gender <- reactive({
    summary_table_baseline$`User gender` %>% filter(Org %in% c((input$OrgDem)))
  }) 
  plot_parent_gender  <- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.user_gender) }) 
  
  output$table_parent_gender <- shiny::renderTable({(table_parent_gender())}, striped = TRUE)
  output$plot_parent_gender <- renderPlotly({plot_parent_gender()})
  
  
  #Parent age plot and table
  table_parent_age <- reactive({
    plhdata_org_clean %>%
      summary_table(columns_to_summarise = rp.contact.field.user_age, summaries = "mean")}) 
  ##previously, but with new contact field summarise(`mean age`=(mean(rp.contact.field.user_gender, na.rm = TRUE)))
  
  plot_parent_age  <- reactive({summary_plot(plhdata_org_clean, rp.contact.field.user_age) })
  
  output$table_parent_age <- shiny::renderTable({(table_parent_age())}, striped = TRUE)
  output$plot_parent_age <- renderPlotly({plot_parent_age()})
  
  #Adults in household plot and table
  table_household_adults <- reactive({
    summary_table_baseline$`Household adults`
    }) 
  plot_household_adults  <- reactive({
    plhdata_org_clean_1 <- plhdata_org_clean %>% filter(Org %in% c((input$OrgDem)))
    
    summary_plot(plhdata_org_clean_1, rp.contact.field.household_adults)
    })
  
  output$table_household_adults <- shiny::renderTable({( table_household_adults())}, striped = TRUE)
  output$plot_household_adults <- renderPlotly({plot_household_adults()})
  
  #Teens in household plot and table
  table_household_teens <- reactive({
    summary_table_baseline$`Household teens`
  }) 
  plot_household_teens  <- reactive({summary_plot(plhdata_org_clean, rp.contact.field.household_teens) })
  
  output$table_household_teens <- shiny::renderTable({( table_household_teens())}, striped = TRUE)
  output$plot_household_teens <- renderPlotly({plot_household_teens()})
  
  #Children in household plot and table
  table_household_children <- reactive({
    summary_table_baseline$`Household children`
  }) 
  plot_household_children  <- reactive({summary_plot(plhdata_org_clean, rp.contact.field.household_children) })
  
  output$table_household_children <- shiny::renderTable({( table_household_children())}, striped = TRUE)
  output$plot_household_children <- renderPlotly({plot_household_children()})
  
  #Babies in household plot and table
  table_household_babies <- reactive({
    summary_table_baseline$`Household babies`
  }) 
  plot_household_babies  <- reactive({summary_plot(plhdata_org_clean, rp.contact.field.household_babies) })
  
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
  
  
  #SECOND Tab Workshop Data
  
  #Table of averages
  table_ws_totals <- reactive({
    summary_mean_completion_level %>% filter(Org %in% c((input$OrgWS))) 
  }) 
  plot_ws_totals  <- reactive({
    summary_mean_completion_level_long <- pivot_longer(summary_mean_completion_level, cols = !Org, names_to = "Workshop", values_to = "Value")
    ggplot(summary_mean_completion_level_long, aes(x = Workshop, y = Value, fill = Org)) +
      geom_bar(stat = "identity", position = "dodge") +
      viridis::scale_fill_viridis(discrete = TRUE)
  }) 
  output$table_ws_totals <- shiny::renderTable({(table_ws_totals())}, striped = TRUE)
  output$plot_ws_totals <- renderPlotly({plot_ws_totals()})
  
  #Workshop tables with filter by Org
  #1
  table_w_self_care <- reactive({
    summary_table_completion_level$`Self Care` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_self_care <- reactive({ summary_plot(plhdata_org_clean, rp.contact.field.w_self_care_completion_level, replace = "rp.contact.field.w_") }) 
  output$table_w_self_care <- shiny::renderTable({(table_w_self_care())}, striped = TRUE)
  output$plot_w_self_care <- renderPlotly({plot_w_self_care()})
  
  #2
  table_w_1on1 <- reactive({
    summary_table_completion_level$`One-on-one Time` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_1on1<- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.w_1on1_completion_level, replace = "rp.contact.field.w_", plot_type = "boxplot")
  }) 
  output$table_w_1on1 <- shiny::renderTable({(table_w_1on1())}, striped = TRUE)
  output$plot_w_1on1 <- renderPlotly({plot_w_1on1()})
  #3
  table_w_praise <- reactive({
    summary_table_completion_level$Praise %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_praise<- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.w_praise_completion_level, replace = "rp.contact.field.w_", plot_type = "boxplot")
  }) 
  output$table_w_praise <- shiny::renderTable({(table_w_praise())}, striped = TRUE)
  output$plot_w_praise <- renderPlotly({plot_w_praise()})
  #4
  table_w_instruct <- reactive({
    summary_table_completion_level$`Positive Instructions` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_instruct<- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.w_instruct_completion_level, replace = "rp.contact.field.w_")
  }) 
  output$table_w_instruct <- shiny::renderTable({(table_w_instruct())}, striped = TRUE)
  output$plot_w_instruct <- renderPlotly({plot_w_instruct()})
  #5
  table_w_stress <- reactive({
    summary_table_completion_level$`Managing Stress` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_stress<- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.w_stress_completion_level, replace = "rp.contact.field.w_")
  }) 
  output$table_w_stress <- shiny::renderTable({(table_w_stress())}, striped = TRUE)
  output$plot_w_stress <- renderPlotly({plot_w_stress()})
  #6
  table_w_money <- reactive({
    summary_table_completion_level$`Family Budgets` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_money<- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.w_money_completion_level, replace = "rp.contact.field.w_")
  }) 
  output$table_w_money <- shiny::renderTable({(table_w_money())}, striped = TRUE)
  output$plot_w_money <- renderPlotly({plot_w_money()})
  #7
  table_w_rules <- reactive({
    summary_table_completion_level$Rules %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_rules<- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.w_rules_completion_level, replace = "rp.contact.field.w_")
  }) 
  output$table_w_rules <- shiny::renderTable({(table_w_rules())}, striped = TRUE)
  output$plot_w_rules <- renderPlotly({plot_w_rules()})
  #8
  table_w_consequence <- reactive({
    summary_table_completion_level$`Calm Consequences` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_consequence <- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.w_consequence_completion_level, replace = "rp.contact.field.w_")
  }) 
  output$table_w_consequence <- shiny::renderTable({(table_w_consequence())}, striped = TRUE)
  output$plot_w_consequence <- renderPlotly({plot_w_consequence()})
  #9
  table_w_solve <- reactive({
    summary_table_completion_level$`Problem Solving` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_solve<- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.w_solve_completion_level, replace = "rp.contact.field.w_")
  }) 
  output$table_w_solve <- shiny::renderTable({(table_w_solve())}, striped = TRUE)
  output$plot_w_solve <- renderPlotly({plot_w_solve()})
  #10
  table_w_safe <- reactive({
    summary_table_completion_level$`Teen Safety` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_safe<- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.w_safe_completion_level, replace = "rp.contact.field.w_")
  }) 
  output$table_w_safe <- shiny::renderTable({(table_w_safe())}, striped = TRUE)
  output$plot_w_safe <- renderPlotly({plot_w_safe()})
  #11
  table_w_crisis <- reactive({
    summary_table_completion_level$`Dealing with Crisis` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_crisis<- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.w_crisis_completion_level, replace = "rp.contact.field.w_")
  }) 
  output$table_w_crisis <- shiny::renderTable({(table_w_crisis())}, striped = TRUE)
  output$plot_w_crisis <- renderPlotly({plot_w_crisis()})
  #12
  table_w_celebrate <- reactive({
    summary_table_completion_level$`Celebration & Next Steps` %>% filter(Org %in% c((input$OrgWS))) })
  plot_w_celebrate<- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.w_celebrate_completion_level, replace = "rp.contact.field.w_")
  }) 
  output$table_w_celebrate <- shiny::renderTable({(table_w_celebrate())}, striped = TRUE)
  output$plot_w_celebrate <- renderPlotly({plot_w_celebrate()})
  
  
  #THIRD Tab Parent Points
  
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
    #summary_plot()
    # table_long <- pivot_longer(summary_table_habits_all$`Relax`, cols = !Org, names_to = "Relax Points", values_to = "Value") %>%
    #   filter(`Relax Points` != "Total")
    # ggplot(table_long, aes(x = Org, y = Value, fill = `Relax Points`)) +
    #   geom_bar(stat = "identity", position = "dodge") +
    #   viridis::scale_fill_viridis(discrete = TRUE)
  }) 
  output$table_pp_relax <- shiny::renderTable({(table_pp_relax())}, striped = TRUE)
  output$plot_pp_relax <- renderPlotly({plot_pp_relax})
  
  # pp_treat_yourself
  table_pp_treat_yourself <- reactive({
    summary_table_habits_all$`Treat yourself` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_treat_yourself  <- reactive({
  }) 
  output$table_pp_treat_yourself <- shiny::renderTable({(table_pp_treat_yourself())}, striped = TRUE)
  output$plot_pp_treat_yourself <- renderPlotly({plot_pp_treat_yourself})
  
  # pp_praise_yourself
  table_pp_praise_yourself <- reactive({
    summary_table_habits_all$`Praise yourself` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_praise_yourself  <- reactive({
  }) 
  output$table_pp_praise_yourself <- shiny::renderTable({(table_pp_praise_yourself())}, striped = TRUE)
  output$plot_pp_praise_yourself <- renderPlotly({plot_pp_praise_yourself})
  
  # pp_spend_time
  table_pp_spend_time <- reactive({
    summary_table_habits_all$`Spend time` %>% filter(Org %in% c((input$OrgPP))) 
  }) 
  plot_pp_spend_time  <- reactive({
  }) 
  output$table_pp_spend_time <- shiny::renderTable({(table_pp_spend_time())}, striped = TRUE)
  output$plot_pp_spend_time <- renderPlotly({plot_pp_spend_time})
  
  # pp_praise_teen
  table_pp_praise_teen <- reactive({
    summary_table_habits_all$`Praise teen` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_praise_teen  <- reactive({
  }) 
  output$table_pp_praise_teen <- shiny::renderTable({(table_pp_praise_teen())}, striped = TRUE)
  output$plot_pp_praise_teen <- renderPlotly({plot_pp_praise_teen})
  
  # pp_instruct_positively
  table_pp_instruct_positively <- reactive({
    summary_table_habits_all$`Instruct positively` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_instruct_positively  <- reactive({
  }) 
  output$table_pp_instruct_positively <- shiny::renderTable({(table_pp_instruct_positively())}, striped = TRUE)
  output$plot_pp_instruct_positively <- renderPlotly({plot_pp_instruct_positively})
  
  # pp_breathe
  table_pp_breathe <- reactive({
    summary_table_habits_all$`Breathe` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_breathe  <- reactive({
  }) 
  output$table_pp_breathe <- shiny::renderTable({(table_pp_breathe())}, striped = TRUE)
  output$plot_pp_breathe <- renderPlotly({plot_pp_breathe})
  
  # pp_money
  table_pp_money <- reactive({
    summary_table_habits_all$`Money` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_money  <- reactive({
  }) 
  output$table_pp_money <- shiny::renderTable({(table_pp_money())}, striped = TRUE)
  output$plot_pp_money <- renderPlotly({plot_pp_money})
  
  # pp_consequence
  table_pp_consequence <- reactive({
    summary_table_habits_all$`Consequence` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_consequence  <- reactive({
  }) 
  output$table_pp_consequence <- shiny::renderTable({(table_pp_consequence())}, striped = TRUE)
  output$plot_pp_consequence <- renderPlotly({plot_pp_consequence})
  
  # pp_safe
  table_pp_safe <- reactive({
    summary_table_habits_all$`Safe` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_pp_safe  <- reactive({
  }) 
  output$table_pp_safe <- shiny::renderTable({(table_pp_safe())}, striped = TRUE)
  output$plot_pp_safe <- renderPlotly({plot_pp_safe})
  
  
  #FOURTH Tab In-week Engagement
  table_pp_safe <- reactive({
    summary_table(columns_to_summarise = rp.contact.field.app_launch_count, replace = "rp.contact.field.") %>%
      filter(Org %in% c((input$OrgXE))) }) 
  plot_pp_safe  <- reactive({
  }) 
  output$table_pp_safe <- shiny::renderTable({(table_pp_safe())}, striped = TRUE)
  output$plot_pp_safe <- renderPlotly({plot_pp_safe})
  
  
  
  #FIFTH Tab Surveys
  
  # Baseline survey completion levels
  # table_sv1_totals <- reactive({
  #   NO TABLE ASSIGNED %>% filter(Org %in% c((input$OrgPP))) }) 
  # plot_sv1_totals  <- reactive({
  # }) 
  # output$table_sv1_totals <- shiny::renderTable({(table_sv1_totals())}, striped = TRUE)
  # output$plot_sv1_totals <- renderPlotly({plot_sv1_totals})
  
  # sv1_attention
  table_sv1_attention <- reactive({
    summary_table_survey_past_week$Attention %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_sv1_attention  <- reactive({
  }) 
  output$table_sv1_attention <- shiny::renderTable({(table_sv1_attention())}, striped = TRUE)
  output$plot_sv1_attention <- renderPlotly({plot_sv1_attention})
  
  # sv1_praise
  table_sv1_praise <- reactive({
    summary_table_survey_past_week$Praise %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_sv1_praise  <- reactive({
  }) 
  output$table_sv1_praise <- shiny::renderTable({(table_sv1_praise())}, striped = TRUE)
  output$plot_sv1_praise <- renderPlotly({plot_sv1_praise})
  
  # sv1_stress
  table_sv1_stress <- reactive({
    summary_table_survey_past_week$Stress %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_sv1_stress  <- reactive({
  }) 
  output$table_sv1_stress <- shiny::renderTable({(table_sv1_stress())}, striped = TRUE)
  output$plot_sv1_stress <- renderPlotly({plot_sv1_stress})
  
  # sv1_shout
  table_sv1_shout <- reactive({
    summary_table_survey_past_week$Shouting %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_sv1_shout  <- reactive({
  }) 
  output$table_sv1_shout <- shiny::renderTable({(table_sv1_shout())}, striped = TRUE)
  output$plot_sv1_shout <- renderPlotly({plot_sv1_shout})
  
  # sv1_money
  table_sv1_money <- reactive({
    summary_table_survey_past_week$`Money worries` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_sv1_money  <- reactive({
  }) 
  output$table_sv1_money <- shiny::renderTable({(table_sv1_money())}, striped = TRUE)
  output$plot_sv1_money <- renderPlotly({plot_sv1_money})
  
  # sv1_food_money
  table_sv1_food_money <- reactive({
    summary_table_survey_past_week$Summary %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_sv1_food_money  <- reactive({
  }) 
  output$table_sv1_food_money <- shiny::renderTable({(table_sv1_food_money())}, striped = TRUE)
  output$plot_sv1_food_money <- renderPlotly({plot_sv1_food_money})
  
  # sv1_hitting
  table_sv1_hitting <- reactive({
    summary_table_survey_past_week$Hitting %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_sv1_hitting  <- reactive({
  }) 
  output$table_sv1_hitting <- shiny::renderTable({(table_sv1_hitting())}, striped = TRUE)
  output$plot_sv1_hitting <- renderPlotly({plot_sv1_hitting})
  
  # sv1_week_teen_activity
  table_sv1_week_teen_activity <- reactive({
    summary_table_survey_past_week$`Teen activity` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_sv1_week_teen_activity  <- reactive({
  }) 
  output$table_sv1_week_teen_activity <- shiny::renderTable({(table_sv1_week_teen_activity())}, striped = TRUE)
  output$plot_sv1_week_teen_activity <- renderPlotly({plot_sv1_week_teen_activity})
  
  # sv1_lockdown
  table_sv1_lockdown <- reactive({
    summary_table_survey_past_week$`Lockdown?` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_sv1_lockdown  <- reactive({
  }) 
  output$table_sv1_lockdown <- shiny::renderTable({(table_sv1_lockdown())}, striped = TRUE)
  output$plot_sv1_lockdown <- renderPlotly({plot_sv1_lockdown})
  
  # sv1_reg_teen_activity
  table_sv1_reg_teen_activity <- reactive({
    summary_table_survey_past_week$`Knowledge of teen activity in non-lockdown week` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_sv1_reg_teen_activity  <- reactive({
  }) 
  output$table_sv1_reg_teen_activity <- shiny::renderTable({(table_sv1_reg_teen_activity())}, striped = TRUE)
  output$plot_sv1_reg_teen_activity <- renderPlotly({plot_sv1_reg_teen_activity})
  
  # sv1_sex_talk
  table_sv1_sex_talk <- reactive({
    summary_table_survey_past_week$`Sexual safety talk` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_sv1_sex_talk  <- reactive({
  }) 
  output$table_sv1_sex_talk <- shiny::renderTable({(table_sv1_sex_talk())}, striped = TRUE)
  output$plot_sv1_sex_talk <- renderPlotly({plot_sv1_sex_talk})
  
  # sv1_covid_safe
  table_sv1_covid_safe <- reactive({
    summary_table_survey_past_week$`Teen COVID safe` %>% filter(Org %in% c((input$OrgPP))) }) 
  plot_sv1_covid_safe  <- reactive({
  }) 
  output$table_sv1_covid_safe <- shiny::renderTable({(table_sv1_covid_safe())}, striped = TRUE)
  output$plot_sv1_covid_safe <- renderPlotly({plot_sv1_covid_safe})
  
  
  #SIXTH Tab Parent Library
  
} #close server


# 5. Create Shiny App -----------------------------------------------------------------------------
shinyApp(ui, server)
