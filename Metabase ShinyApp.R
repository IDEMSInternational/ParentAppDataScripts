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
          shinydashboard::valueBoxOutput("myvaluebox1", width=2), #boxes along top for ParentText
          shinydashboard::valueBoxOutput("myvaluebox2", width=2),
        ), #closes fluidRow
        
          
        tabItems(
          # First tab content
          tabItem(tabName = "demographics",
                  
                  fluidRow(
                    column(12, align = "centre",
                           # splitLayout gets two boxes side by side.
                           # in this case, it is just the header (h2), and an icon
                           # we want 80% of the width to be the header (h2) and 20% the icon (hence cellWidths = ...)
                           box(splitLayout(h2("TEST with Malasian data"), icon("users", "fa-6x"),
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
                        title = "Langauge",
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
    shinydashboard::valueBox( nrow(testdata %>% filter(consent == "Yes")), subtitle = "Consented", icon = icon("user"),
                              color = "yellow")})
  
  
  #1st tab DEMOGRAPHICS
  
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
    testdata %>% 
      group_by(parenting_goals) %>% summarise(n())
  }) #closes parent goals table
  
  plot_parent_goals  <- reactive({
    ggplot(testdata, aes(x = parenting_goals)) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = "parent goal", y = "Count") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }) #closes parent goals plot
  
  output$table_parent_goals <- shiny::renderTable({(table_parent_goals())}, striped = TRUE)
  output$plot_parent_goals <- renderPlotly({plot_parent_goals()})
  
  table_child_gender <- reactive({
    testdata %>% 
      group_by(child_gender) %>% summarise(n())
  }) #closes child gender table
  
  plot_child_gender  <- reactive({
    ggplot(testdata, aes(x = child_gender)) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = "child gender", y = "Count") +
      theme_classic()
  }) #closes parent goals plot
   
  table_child_age <- reactive({
    testdata %>% 
      group_by(child_age_group) %>% summarise(n())
  }) #closes child age table
  
  plot_child_age  <- reactive({
    ggplot(testdata, aes(x = child_age_group)) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = "child age group", y = "Count") +
      theme_classic()
  }) #closes child age plot
  
  table_child_type <- reactive({
    testdata %>% 
      group_by(challenging_type) %>% summarise(n())
  }) #closes child behaviour table
  
  plot_child_type  <- reactive({
    ggplot(testdata, aes(x = challenging_type)) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = "child behaviour", y = "Count") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }) #closes child behaviour plot
  
  
  
 
  
  #CHILD outputs first tab

  output$table_child_gender <- shiny::renderTable({(table_child_gender())}, striped = TRUE)
  output$plot_child_gender <- renderPlotly({plot_child_gender()})
  
  output$table_child_age <- shiny::renderTable({(table_child_age())}, striped = TRUE)
  output$plot_child_age <- renderPlotly({plot_child_age()})
  
  output$table_child_type <- shiny::renderTable({(table_child_type())}, striped = TRUE)
  output$plot_child_type <- renderPlotly({plot_child_type()})
  
  
  #3rd Tab Parent Points
  
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
