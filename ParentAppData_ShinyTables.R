
##NB this is not yet ready to run - Margherita has added comments as "MPH"
    # search for "remove" to find bits that probably aren't relevant
    # some functions from Lily's function script seem to be required for Shiny and could be inserted here (e.g. set_rapidpro_key)

# install.packages(c("shiny", "shinythemes", "shinyjs", "plotly", "shinydashboard", "httr", "jsonlite", "tidyverse"))

library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly)
library(shinydashboard)
library(httr)
library(jsonlite)
#library(tidyverse)     ***MPH remove as already installed?
#library(here)          **MPH necessary bc done in other file?


##
source("Metabase Functions.R")

#install_github("lilyclements/Rapidreadr")   **MPH was commented out, not sure if needs to be replaced by anything
#library(Rapidreadr)

# 1. Calling the data --------------------------------------------------------------
                                            #**MPH REMOVE as I think this is done in our personal setup file 
source('config/Personal Setup.R')

#pkg_env <- new.env(parent = emptyenv())
#assign("rapidpro_key", NULL, envir = pkg_env)
#assign("rapidpro_site", NULL, envir = pkg_env)
#key <- "..."
#set_rapidpro_key(key = key)
#set_rapidpro_site(site = "https://app.rapidpro.io/api/v2/")
#set_rapidpro_uuid_names()

data<-read.csv('~/Documents/GitHub/ParentAppDataScripts/data/malaysia_data_20220111.csv')


# 2. Data Cleaning -----------------------------------------------------------------

#source("Metabase Analysis")
                                            
  

# 3. Define UI -----------------------------------------------------------------------------

ui <- dashboardPage(
  header = dashboardHeader(title = "ParentApp Dashboard"),
  dashboardPage(skin = "green"),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Overview and Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Workshops Engagement", tabName = "workshops", icon = icon("lightbulb")),
      menuItem("Parent Points", tabName = "parentpoints", icon = icon("star"))
    )),
  dashboardBody(
    fluidRow(
      shinydashboard::valueBoxOutput("myvaluebox1", width=2),
      shinydashboard::valueBoxOutput("myvaluebox2", width=2),
      shinydashboard::valueBoxOutput("myvaluebox3", width=2),
      shinydashboard::valueBoxOutput("myvaluebox4", width=2),
      shinydashboard::valueBoxOutput("myvaluebox5", width=2)),
    tabItems(
      # First tab content
      
      tabItem(tabName = "demographics",
              fluidRow(
                column(10, align = "centre",
                       # splitLayout gets two boxes side by side.
                       # in this case, it is just the header (h2), and an icon
                       # we want 80% of the width to be the header (h2) and 20% the icon (hence cellWidths = ...)
                       box(splitLayout(h2("Demographics"), icon("users", "fa-6x"),
                                       cellArgs = list(style = "vertical-align: top"),
                                       cellWidths = c("80%", "20%")),
                           width = 10,
                           title = NULL,
                           collapsible = FALSE,
                           solidHeader = TRUE,
                           background = "light-blue",
                           height = "95px"))), #closes fluid row
              
              # On the first page we want two tabs: Overall, and ByGroup.
              tabsetPanel(type = "tabs",
                          tabPanel("Overall",
                                   fluidRow(column(12,
                                                   box(width = NULL,
                                                       collapsible = FALSE,
                                                       solidHeader = TRUE,
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                
                                                                # splitLayout gets two boxes side by side.
                                                                # Here we want consent frequency and enrolled frequency.
                                                                  box( width=NULL,
                                                                       title = "Consent Frequency",
                                                                       status = "primary", # primary, success, info, warning, danger
                                                                       solidHeader = TRUE,
                                                                       plotlyOutput(outputId = "plot_consent", height = "240"),
                                                                       shiny::tableOutput("consent_summary"))
                                                                ),
                                                         width = 10),
                                                       
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                
                                                                # splitLayout gets two boxes side by side.
                                                                # Here we want parent and child demographics
                                                                splitLayout(
                                                                  box(width=NULL,
                                                                      collapsible = FALSE,
                                                                      title = "Parent Demographics",
                                                                      status = "primary",
                                                                      solidHeader = TRUE,
                                                                      
                                                                      # allll the tables to output (the r-code for this is in server)
                                                                      shiny::tableOutput("parent_gender_summary"),
                                                                      shiny::tableOutput("parent_age_summary"),
                                                                      shiny::tableOutput("parent_child_relationship_summary"),
                                                                      shiny::tableOutput("parent_relationship_status_summary"),
                                                                  ),
                                                                  box(width=NULL,
                                                                      collapsible = FALSE,
                                                                      title = "Child Demographics",
                                                                      status = "primary",
                                                                      solidHeader = TRUE,
                                                                      
                                                                      # allll the tables to output (the r-code for this should be in server, but has been removed.)
                                                                      shiny::tableOutput("child_gender_summary"),
                                                                      shiny::tableOutput("child_age_summary"),
                                                                      shiny::tableOutput("child_living_with_disabilities_summary"),
                                                                      
                                                                  ), # close child box
                                                                  cellWidths = c("50%", "50%"),
                                                                  cellArgs = list(style = "vertical-align: top"))),
                                                         width = 10) # fluid row close
                                                   )))), # close box, col, fluid row, tab panel

                          tabPanel("By group",
                                   fluidRow(column(12,
                                                   box(width = NULL,
                                                       collapsible = FALSE,
                                                       solidHeader = TRUE,
                                                       fluidRow(
                                                         column(
                                                           width = 12,
                                                           #align = "center",
                                                           fluidRow(
                                                             column(6, uiOutput("groups")),
                                                           ) #fluid row closure
                                                         ) #Outer column closure
                                                       ),
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                
                                                                # splitLayout gets two boxes side by side.
                                                                # Here we want consent frequency and enrolled frequency.
                                                                box( width=NULL,
                                                                     title = "Consent Frequency",
                                                                     status = "primary", # primary, success, info, warning, danger
                                                                     solidHeader = TRUE,
                                                                     plotlyOutput(outputId = "plot_consent_group", height = "240"),
                                                                     shiny::tableOutput("consent_summary_group"))
                                                         ),
                                                         width = 10),
                                                       
                                                       fluidRow(
                                                         column(10, align = "center",
                                                                splitLayout(
                                                                  box(width=NULL,
                                                                      collapsible = FALSE,
                                                                      title = "Parent Demographics",
                                                                      status = "primary",
                                                                      solidHeader = TRUE,
                                                                      shiny::tableOutput("parent_gender_group_summary"),
                                                                      shiny::tableOutput("parent_age_group_summary"),
                                                                      shiny::tableOutput("parent_child_relationship_group_summary"),
                                                                      shiny::tableOutput("parent_relationship_status_group_summary"),
                                                                  ),
                                                                  
                                                                  box(width=NULL,
                                                                      collapsible = FALSE,
                                                                      title = "Child Demographics",
                                                                      status = "primary",
                                                                      solidHeader = TRUE,
                                                                      shiny::tableOutput("child_gender_group_summary"),
                                                                      shiny::tableOutput("child_age_group_summary"),
                                                                      shiny::tableOutput("child_living_with_disabilities_group_summary"),
                                                                      
                                                                  ), # close child box
                                                                  cellWidths = c("50%", "50%"),
                                                                  cellArgs = list(style = "vertical-align: top")) # split layout for parent to child demographics close
                                                         )
                                                       )
                                                   )))) # close box, col, fluid row, tab panel
              ) # close tabset panel
      ), # close tab
      
      # Second tab content
      tabItem(tabName = "workshops",
      ), # close engagement tab
      
      tabItem(tabName = "parentpoints",
      ) # close behaviour tab
    ) # close items
  ) # close body
) # close function

# 3. Define Server -----------------------------------------------------------------------------
                      #**MPH play with basic setup of UI and add labels relevant for ParentApp
  
server <- function(input, output) {
  
  # Subset data
  selected_data <- reactive({
    
  })
  
  output$groups <- renderUI({
    #df_all <- df
    #selectInput(
      #inputId = "grouper",
      #label = "Group variable",
      #choices = c("Parent Gender" = "parent_gender",
                  #"Child Gender" = "child_gender",
                  #"Child Age Group" = "child_age_group"),
      #selected = "parent_gender"
   # )
  })
  
  output$plot_consent <- renderPlotly({
    # ggplot(df_all, aes(x = consent)) +
    #   geom_histogram(stat = "count") +
    #   viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
    #   labs(x = "Consent", y = "Count") +
    #   theme_classic()
  })
  
  output$plot_consent_group <- renderPlotly({
    # req(input$grouper)
    # ggplot(df_all, aes(x = consent, fill = (!!!rlang::syms(input$grouper)))) +
    #   geom_histogram(stat = "count") +
    #   viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
    #   labs(x = "Consented", y = "Count") +
    #   theme_classic()
  })
  
  consent_summary <- reactive({
      plhdata_org_clean %>% group_by(Org,rp.contact.field.click_pc_essential_tools_count) %>% summarise(n())
    })
  
  consent_summary_group <- reactive({
    # req(input$grouper)
    # summary_PT(df_all, c(consent, !!!rlang::syms(input$grouper)), enrolled, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_gender_summary <- reactive({
    # summary_PT(df_all, parent_gender, consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_gender_group_summary <- reactive({
    # summary_PT(df_all, c(parent_gender, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_child_relationship_summary <- reactive({
    # summary_PT(df_all, parent_child_relationship, consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_child_relationship_group_summary <- reactive({
    # req(input$grouper)
    # summary_PT(df_all, c(parent_child_relationship, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_relationship_status_summary <-  reactive({
    # summary_PT(df_all, parent_relationship_status, consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_relationship_status_group_summary <-  reactive({
    # summary_PT(df_all, c(parent_relationship_status, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  active_users_24hr_group_summary <- reactive({
    # req(input$grouper_engagement)
    # summary_PT(df_all, c(active_users_24hr, !!!rlang::syms(input$grouper_engagement)), program, together = TRUE, naming_convention = TRUE)
  })
  
  active_users_24hr_summary <- reactive({
    # summary_PT(df_all, active_users_24hr, program, together = TRUE, naming_convention = TRUE)
  })
  
  active_users_7d_summary <- reactive({
    # summary_PT(df_all, active_users_7d, program, together = TRUE, naming_convention = TRUE)
  })
  
  active_users_7d_group_summary <- reactive({
    # req(input$grouper_engagement)
    # summary_PT(df_all, c(active_users_7d, !!!rlang::syms(input$grouper_engagement)), program, together = TRUE, naming_convention = TRUE)
  })
  
  parent_age_summary <- reactive({
    # req(input$grouper)
    # parent_age_df <- df_all %>% 
    #   filter(consent == "Yes") %>%
    #   summarise(parent_age_mean = round(mean(parent_age, na.rm = TRUE), 2),
    #             parent_age_sd = round(sd(parent_age, na.rm = TRUE), 2))
    # colnames(parent_age_df) <- naming_conventions(colnames(parent_age_df))
    # parent_age_df
  })
  
  parent_age_group_summary <- reactive({
    # req(input$grouper)
    # parent_age_df <- df_all %>% group_by(!!!rlang::syms(input$grouper)) %>%
    #   filter(consent == "Yes") %>%
    #   summarise(parent_age_mean = round(mean(parent_age, na.rm = TRUE), 2),
    #             parent_age_sd = round(sd(parent_age, na.rm = TRUE), 2))
    # colnames(parent_age_df) <- naming_conventions(colnames(parent_age_df))
    # parent_age_df
  })
  
  # Output render
  # df_enrolled <- summary_PT(df_all,  enrolled,  enrolled, "Yes")
  # df_enrolled <- df_enrolled %>% mutate(group =  enrolled, count = enrolled_n) %>% dplyr::select(c(group, count))
  # 
  # df_consented <- summary_PT(df_all,  consent,  consent, "Yes")
  # df_consented <- df_consented %>% mutate(group =  consent, count = consent_n) %>% dplyr::select(c(group, count))
  # 
  # df_program <- summary_PT(df_all,  program,  program, "Yes")
  # df_program <- df_program %>% mutate(group =  program, count = program_n) %>% dplyr::select(c(group, count))
  # 
  # df_active_24 <- (summary_PT(df_all, active_users_24hr, program) %>% filter(active_users_24hr == "Yes"))$active_users_24hr_n
  # df_active_7d <- (summary_PT(df_all, active_users_7d, program) %>% filter(active_users_7d == "Yes"))$active_users_7d_n
  
  output$myvaluebox1 <- shinydashboard::renderValueBox({
    # shinydashboard::valueBox(df_enrolled$count[1], subtitle = "Enrolled", icon = icon("user"),
    #                          color = "aqua"
    # )
  })
  output$myvaluebox2 <- shinydashboard::renderValueBox({
    # shinydashboard::valueBox(df_consented$count[1],subtitle = "Consented",icon = icon("check"),
    #                          color = "green"
    # )
  })
  output$myvaluebox3 <- shinydashboard::renderValueBox({
    # shinydashboard::valueBox(df_program$count[1],subtitle = "In Program",icon = icon("clipboard"),
    #                          color = "yellow"
    # )
  })
  output$myvaluebox4 <- shinydashboard::renderValueBox({
    # shinydashboard::valueBox(df_active_24,subtitle = "Active in last 24 hours",icon = icon("clock"),
    #                          color = "purple"
    # )
  })
  output$myvaluebox5 <- shinydashboard::renderValueBox({
    # shinydashboard::valueBox(df_active_7d, subtitle = "Active in last 7 days", icon = icon("signal"),
    #                          color = "fuchsia"
    # )
  })
  
  output$consent_summary <- shiny::renderTable({(consent_summary())}, striped = TRUE)
  output$consent_summary_group <- shiny::renderTable({(consent_summary_group())}, striped = TRUE)
  output$parent_gender_summary <- shiny::renderTable({(parent_gender_summary())}, striped = TRUE)
  output$parent_gender_group_summary <- shiny::renderTable({(parent_gender_group_summary())}, striped = TRUE)
  output$parent_age_summary <- shiny::renderTable({(parent_age_summary())}, striped = TRUE)
  output$parent_age_group_summary <- shiny::renderTable({(parent_age_group_summary())}, striped = TRUE)
  output$parent_child_relationship_summary <- shiny::renderTable({(parent_child_relationship_summary())}, caption = "Relationship between the parent and child", striped = TRUE)
  output$parent_child_relationship_group_summary <- shiny::renderTable({(parent_child_relationship_group_summary())}, caption = "Relationship between the parent and child", striped = TRUE)
  output$parent_relationship_status_group_summary <- shiny::renderTable({(parent_relationship_status_group_summary())}, caption = "Relationship status of the parent", striped = TRUE)
  output$parent_relationship_status_summary <- shiny::renderTable({(parent_relationship_status_summary())}, caption = "Relationship status of the parent", striped = TRUE)
}

# 5. Create Shiny App -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

