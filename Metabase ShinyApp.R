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
      menuItem("Parent Points", tabName = "parentpoints", icon = icon("star"))
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
          # First tab content
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
                               background = "black",
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
                        status = "primary", # primary, success, info, warning, danger
                        #background = "orange",
                        plotlyOutput(outputId = "plot_language", height = "240"), #generates graph
                        shiny::tableOutput("table_language")  #generates table
                    ), #closes box
                    
                    box(width = 4,
                        collapsible = FALSE,
                        solidHeader = TRUE,
                        title = "Workshop format",
                        status = "primary", # primary, success, info, warning, danger
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
                            status = "primary", # primary, success, info, warning, danger
                            #background = "orange",
                            plotlyOutput(outputId = "plot_parent_gender", height = "240"), #generates graph
                            shiny::tableOutput("table_parent_gender")  #generates table
                        ), #closes box
                        
                        box(width = 4,
                            collapsible = FALSE,
                            solidHeader = TRUE,
                            title = "Parent age",
                            status = "primary", # primary, success, info, warning, danger
                            #background = "orange",
                            plotlyOutput(outputId = "plot_parent_age", height = "240"),
                            shiny::tableOutput("table_parent_age")
                        ), #closes box
                        
                        box(width = 4,
                            collapsible = FALSE,
                            solidHeader = TRUE,
                            title = "Adults in household",
                            status = "primary", # primary, success, info, warning, danger
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
                            status = "danger", # primary, success, info, warning, danger
                            #background = "orange",
                            plotlyOutput(outputId = "plot_household_teens", height = "240"),
                            shiny::tableOutput("table_household_teens")
                        ), #closes box
                        
                        box(width = 4,
                            collapsible = FALSE,
                            solidHeader = TRUE,
                            title = "Children in household",
                            status = "danger", # primary, success, info, warning, danger
                            #background = "orange",
                            plotlyOutput(outputId = "plot_household_children", height = "240"),
                            shiny::tableOutput("table_household_children")
                        ), #closes box
                        
                        box(width = 4,
                            collapsible = FALSE,
                            solidHeader = TRUE,
                            title = "Babies in household",
                            status = "danger", # primary, success, info, warning, danger
                            #background = "orange",
                            plotlyOutput(outputId = "plot_household_babies", height = "240"),
                            shiny::tableOutput("table_household_babies")
                        ) #closes box
                ) #closes fluid row
                  
        ),#closes tabItem
        
        # Second tab content
        tabItem(tabName = "workshops",
                
                fluidRow(
                  column(12, align = "centre",
                         # splitLayout gets two boxes side by side.
                         # in this case, it is just the header (h2), and an icon
                         # we want 80% of the width to be the header (h2) and 20% the icon (hence cellWidths = ...)
                         box(splitLayout(h2("Workshop engagement"), icon("lightbulb", "fa-6x"),
                                         cellArgs = list(style = "vertical-align: top"),
                                         cellWidths = c("80%", "20%")),
                             width = 15,
                             title = NULL,
                             collapsible = FALSE,
                             solidHeader = TRUE,
                             background = "black",
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
                      status = "info", # primary, success, info, warning, danger
                      #background = "orange",
                      plotlyOutput(outputId = "plot_ws_totals", height = "240"),
                      shiny::tableOutput("table_ws_totals")
                  )#closes box
                ), #closes fluid row
                
                fluidRow(
                  box(width = 6,
                      collapsible = FALSE,
                      solidHeader = TRUE,
                      title = "Workshop data",
                      status = "warning", # primary, success, info, warning, danger
                      #background = "orange",
                      plotlyOutput(outputId = "plot_consent6", height = "240"),
                      shiny::tableOutput("consent_summary6")
                  ), #closes box
                  
                  box(width = 6,
                      collapsible = FALSE,
                      solidHeader = TRUE,
                      title = "Workshop data",
                      status = "warning", # primary, success, info, warning, danger
                      #background = "orange",
                      plotlyOutput(outputId = "plot_consent7", height = "240"),
                      shiny::tableOutput("consent_summary7")
                  ) #closes box
                ) #closes fluid row
        ), #closes tabItem
        
        # Third tab content
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
                             background = "black",
                             height = "95px")
                  ) #closes box
                ), #closes fluid row
                
                fluidRow(
                  box(width = 12,
                    checkboxGroupInput(inputId = "OrgPP",
                                       label = "Organisations to show:",
                                       choices = c("Amathuba" = "Amathuba",
                                                   "Dlalanathi" = "Dlalanathi",
                                                   "Joy" = "Joy",
                                                   "Nontobeko" = "Nontobeko"),
                                       selected = c("Amathuba","Dlalanathi",
                                                    "Joy","Nontobeko")
                    )),
                
                  fluidRow(
                    box(width = 12,
                        collapsible = FALSE,
                        solidHeader = TRUE,
                        title = "Average parent points",
                        status = "info", # primary, success, info, warning, danger
                        #background = "orange",
                        plotlyOutput(outputId = "plot_pp_totals", height = "240"),
                        shiny::tableOutput("table_pp_totals")
                    )#closes box
                  ), #closes fluid row
                  
                  fluidRow(
                  box(width = 6,
                      collapsible = FALSE,
                      solidHeader = TRUE,
                      title = "Parent points: Self Care",
                      status = "info", # primary, success, info, warning, danger
                      #background = "orange",
                      plotlyOutput(outputId = "plot_pp_selfcare", height = "240"),
                      shiny::tableOutput("table_pp_selfcare")
                  ), #closes box
                  
                  box(width = 6,
                      collapsible = FALSE,
                      solidHeader = TRUE,
                      title = "Parent points: One-on-one Time",
                      status = "info", # primary, success, info, warning, danger
                      #background = "orange",
                      plotlyOutput(outputId = "plot_pp_1on1", height = "240"),
                      shiny::tableOutput("table_pp_1on1")
                  ) #closes box
                ) #closes fluid row
        ) # closes third tabItem
        
      ) # closes tabItems
  ) # closes dashboardBody
  ))# closes dashboardPage
  
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
  
  
  #First tab DEMOGRAPHICS
  
  #Languages plot and table
  table_language <- reactive({
    summary_table_baseline$` app language` %>% filter(Org %in% c((input$OrgDem))) %>%
      pivot_wider(names_from = `User gender`, values_from = N)
  }) 
  plot_language  <- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field._app_language) }) 
  
  output$table_language <- shiny::renderTable({(table_language())}, striped = TRUE)
  output$plot_language <- renderPlotly({plot_language()})
  
  #Workshop format plot and table
  table_ws_format <- reactive({
    summary_table_baseline$`Do workshops together` %>% filter(Org %in% c((input$OrgDem))) %>%
      pivot_wider(names_from = `Do workshops together`, values_from = N)
  }) 
  plot_ws_format  <- reactive({
    summary_plot(plhdata_org_clean, rp.contact.field.do_workshops_together) }) 
  
  output$table_ws_format <- shiny::renderTable({(table_ws_format())}, striped = TRUE)
  output$plot_ws_format <- renderPlotly({plot_ws_format()})
  
  #App version
  table_app_version <- reactive({
    summary_table_baseline$`App version` %>% filter(Org %in% c((input$OrgDem))) %>%
      pivot_wider(names_from = `App version`, values_from = N)
  }) 
  plot_app_version  <- reactive({
    summary_plot(plhdata_org_clean, app_version) }) 
  
  output$table_app_version <- shiny::renderTable({(table_app_version())}, striped = TRUE)
  output$plot_app_version <- renderPlotly({plot_app_version()})
  
  
  #Parent gender plot and table
  table_parent_gender <- reactive({
    summary_table_baseline$`User gender` %>% filter(Org %in% c((input$OrgDem))) %>%
                                            pivot_wider(names_from = `User gender`, values_from = N)
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
    plhdata_org_clean %>%
      summary_table(columns_to_summarise = rp.contact.field.household_adults, summaries = "mean")}) 
  
  plot_household_adults  <- reactive({summary_plot(plhdata_org_clean, rp.contact.field.household_adults) })
  
  output$ table_household_adults <- shiny::renderTable({( table_household_adults())}, striped = TRUE)
  output$plot_parent_age <- renderPlotly({plot_parent_age()})

  plot_household_adults
  
  
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
  
  
  #Second Tab Workshop Data
  
  #Table of averages
  table_ws_totals <- reactive({
    summary_mean_completion_level %>% filter(Org %in% c((input$OrgWS))) 
  }) 
  plot_pp_totals  <- reactive({
  }) 
  
  output$table_ws_totals <- shiny::renderTable({(table_ws_totals())}, striped = TRUE)
  output$plot_ws_totals <- renderPlotly({})
  

  #Third Tab Parent Points
  
  #Table of averages
  table_pp_totals <- reactive({
    summary_mean_habits %>% filter(Org %in% c((input$OrgPP))) 
  }) 
  plot_pp_totals  <- reactive({
     }) 
  
  output$table_pp_totals <- shiny::renderTable({(table_pp_totals())}, striped = TRUE)
  output$plot_pp_totals <- renderPlotly({})
  
  
} #close server


# 5. Create Shiny App -----------------------------------------------------------------------------
shinyApp(ui, server)
