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
                    box(
                      checkboxInput("somevalue1", "Include women", FALSE, width=5)),
                    verbatimTextOutput("value1"),
                    
                    box(
                      checkboxInput("somevalue2", "Include teenagers", FALSE, width=5)),
                    verbatimTextOutput("value2")
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
                            title = "Parent goals",
                            status = "primary", # primary, success, info, warning, danger
                            #background = "orange",
                            plotlyOutput(outputId = "plot_parent_goals", height = "240"),
                            shiny::tableOutput("table_parent_goals")
                        ) #closes box
                ), #closes fluidRow
                
                fluidRow(
                        box(width = 4,
                            collapsible = FALSE,
                            solidHeader = TRUE,
                            title = "Child gender",
                            status = "danger", # primary, success, info, warning, danger
                            #background = "orange",
                            plotlyOutput(outputId = "plot_child_gender", height = "240"),
                            shiny::tableOutput("table_child_gender")
                        ), #closes box
                        
                        box(width = 4,
                            collapsible = FALSE,
                            solidHeader = TRUE,
                            title = "Child age",
                            status = "danger", # primary, success, info, warning, danger
                            #background = "orange",
                            plotlyOutput(outputId = "plot_child_age", height = "240"),
                            shiny::tableOutput("table_child_age")
                        ), #closes box
                        
                        box(width = 4,
                            collapsible = FALSE,
                            solidHeader = TRUE,
                            title = "Challenging type",
                            status = "danger", # primary, success, info, warning, danger
                            #background = "orange",
                            plotlyOutput(outputId = "plot_child_type", height = "240"),
                            shiny::tableOutput("table_child_type")
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
                  box(width = 6,
                      collapsible = FALSE,
                      solidHeader = TRUE,
                      title = "Parentpoint data",
                      status = "info", # primary, success, info, warning, danger
                      #background = "orange",
                      plotlyOutput(outputId = "plot_consent8", height = "240"),
                      shiny::tableOutput("consent_summary8")
                  ), #closes box
                  
                  box(width = 6,
                      collapsible = FALSE,
                      solidHeader = TRUE,
                      title = "Parentpoint data",
                      status = "info", # primary, success, info, warning, danger
                      #background = "orange",
                      plotlyOutput(outputId = "plot_consent9", height = "240"),
                      shiny::tableOutput("consent_summary9")
                  ) #closes box
                ) #closes fluid row
        ) # closes third tabItem
        
      ) # closes tabItems
  ) # closes dashboardBody
  )# closes dashboardPage
  
# 4. Define Server -----------------------------------------------------------------------------
server <- function(input, output) {
  
  # df_enrolled <- summary_PT(df1,  enrolled,  enrolled, "Yes")
  # df_enrolled <- df_enrolled %>% mutate(group =  enrolled, count = enrolled_n) %>% dplyr::select(c(group, count))
  
  table_parent_gender <- reactive({
    summary_table_baseline$`User gender`
  }) #closes parent gender table
  
  plot_parent_gender  <- reactive({
    ggplot(testdata, aes(x = parent_gender)) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = "parent gender", y = "Count") +
      theme_classic()
  }) #closes parent gender plot
  
  table_parent_age <- reactive({
    testdata %>%
       summarise(`mean age`=(mean(parent_age, na.rm = TRUE)))
  }) #closes parent age table
  
  plot_parent_age  <- reactive({
    ggplot(testdata, aes(x = parent_age)) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = "parent age", y = "Count") +
      theme_classic()
  }) #closes parent age plot
  
  table_parent_goals <- reactive({
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
  
  #SUMMARY stats first tab
  
  output$myvaluebox1 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(nrow(testdata), subtitle = "Enrolled", icon = icon("user"),
                             color = "aqua")
  })
  
  output$myvaluebox2 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox( nrow(testdata %>% filter(consent == "Yes")), subtitle = "Consented", icon = icon("user"),
                             color = "yellow")
  })
  
  
  #CHECKBOXES first tab
  
  output$value <- renderText({ input$somevalue1 })
  
  #PARENT outputs first tab
  
  output$table_parent_gender <- shiny::renderTable({(table_parent_gender())}, striped = TRUE)
  output$plot_parent_gender <- renderPlotly({plot_parent_gender()})
  
  output$table_parent_age <- shiny::renderTable({(table_parent_age())}, striped = TRUE)
  output$plot_parent_age <- renderPlotly({plot_parent_age()})
  
  output$table_parent_goals <- shiny::renderTable({(table_parent_goals())}, striped = TRUE)
  output$plot_parent_goals <- renderPlotly({plot_parent_goals()})
  
  #CHILD outputs first tab

  output$table_child_gender <- shiny::renderTable({(table_child_gender())}, striped = TRUE)
  output$plot_child_gender <- renderPlotly({plot_child_gender()})
  
  output$table_child_age <- shiny::renderTable({(table_child_age())}, striped = TRUE)
  output$plot_child_age <- renderPlotly({plot_child_age()})
  
  output$table_child_type <- shiny::renderTable({(table_child_type())}, striped = TRUE)
  output$plot_child_type <- renderPlotly({plot_child_type()})
} #close server


# 5. Create Shiny App -----------------------------------------------------------------------------
shinyApp(ui, server)
