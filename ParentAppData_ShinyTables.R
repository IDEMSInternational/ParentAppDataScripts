
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
#source("Functions.R")  ***MPH adapt to actual name source(here("ParentAppData_setupclean.R")) or source(here("ParentAppData_CleaningAndAnalysis.R"))

#install_github("lilyclements/Rapidreadr")   **MPH was commented out, not sure if needs to be replaced by anything
#library(Rapidreadr)

# 1. Calling the data --------------------------------------------------------------
                                            #**MPH REMOVE as I think this is done in our personal setup file 
pkg_env <- new.env(parent = emptyenv())
assign("rapidpro_key", NULL, envir = pkg_env)
assign("rapidpro_site", NULL, envir = pkg_env)
#key <- "..."
set_rapidpro_key(key = key)
set_rapidpro_site(site = "https://app.rapidpro.io/api/v2/")
set_rapidpro_uuid_names()

# 2. Data Cleaning -----------------------------------------------------------------
                                            #**MPH REMOVE as I think this is done in our setup and clean file
                                            
  contacts_unflat <- get_user_data(flatten = FALSE)
  contacts_unflat <- contacts_unflat %>% filter(as.POSIXct("2021-10-14", format="%Y-%m-%d", tzone = "UTC") < as.POSIXct(contacts_unflat$created_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  ID <- contacts_unflat$uuid
  
  # enrolled, consented, program data
  enrolled <- NULL
  consent <- NULL
  program <- NULL
  for (i in 1:length(contacts_unflat$groups)){
    contact_name <- contacts_unflat$groups[[i]]
    if (length(contact_name)==0) {
      enrolled[i] <- NA
      consent[i] <- NA
      program[i] <- NA
    } else{
      enrolled[i] <- ifelse(any(contact_name$name %in% "joined"), "Yes", "No")
      consent[i] <- ifelse(any(contact_name$name %in% "consent"), "Yes", "No")
      program[i] <- ifelse(any(contact_name$name %in% "in program"), "Yes", "No")
    }
  }
  
  enrolled <- factor(enrolled)
  consent <- factor(consent)
  program <- factor(program)
  enrolled <- forcats::fct_expand(enrolled, c("Yes", "No"))
  consent <- forcats::fct_expand(consent, c("Yes", "No"))
  program <- forcats::fct_expand(program, c("Yes", "No"))
  enrolled <- forcats::fct_relevel(enrolled, c("Yes", "No"))
  consent <- forcats::fct_relevel(consent, c("Yes", "No"))
  program <- forcats::fct_relevel(program, c("Yes", "No"))
  
  # parent data
  parent_gender <- contacts_unflat$fields$gender
  parent_gender <- factor(ifelse(parent_gender %in% c("female", "f", "woman", "Woman"), "Woman",
                                 ifelse(parent_gender %in% c("male", "m", "man", "Man"), "Man",
                                        ifelse(parent_gender %in% "no", NA, parent_gender))))
  parent_gender <- fct_expand(parent_gender, "Woman", "Man")
  parent_gender <- forcats::fct_relevel(parent_gender, c("Woman", "Man"))
  
  parent_child_relationship <- factor(contacts_unflat$fields$survey_behave_relationship)
  parent_child_relationship <- fct_expand(parent_child_relationship, "Parent", "Grandparent", "Aunt/Uncle", "Foster Parent", "Other", "Prefer not to say")
  parent_child_relationship <- forcats::fct_recode(parent_child_relationship,
                                                   Parent = "parent",
                                                   Grandparent = "grandparent",
                                                   `Aunt/Uncle`= "uncle",
                                                   `Foster Parent` = "foster",
                                                   Other = "other",
                                                   `Prefer not to say`  = "no")
  parent_child_relationship <- forcats::fct_relevel(parent_child_relationship,
                                                    c("Parent", "Grandparent", "Aunt/Uncle", "Foster Parent", "Other", "Prefer not to say"))
  
  parent_relationship_status <- factor(contacts_unflat$fields$marital_status)
  parent_relationship_status <- forcats::fct_recode(parent_relationship_status,
                                                    `Prefer not to say`  = "no")
  parent_relationship_status <- fct_expand(parent_relationship_status, "Single", "Married", "Partnered", "Divorced", "Widowed", "Prefer not to say")
  parent_relationship_status <- forcats::fct_relevel(parent_relationship_status, c("Single", "Married", "Partnered", "Divorced", "Widowed", "Prefer not to say"))
  
  parent_age <- as.numeric(as.character(contacts_unflat$fields$age))
  
  # Calculations
  # active users # N = contacts for which the time difference between the current time and the datetime variable "last_seen_on" is less than 24 h 
  active_users_24hr <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours") <= 24
  active_users_24hr <- factor(active_users_24hr)
  if (length(levels(active_users_24hr)) == 1){
    if (levels(active_users_24hr) == "FALSE"){
      levels(active_users_24hr) <- c(levels(active_users_24hr),"TRUE")
    } else if (levels(active_users_24hr) == "TRUE"){
      levels(active_users_24hr) <- c(levels(active_users_24hr),"FALSE")
    }
  }
  active_users_24hr <- forcats::fct_expand(active_users_24hr, c("Yes", "No"))
  active_users_24hr <- forcats::fct_recode(active_users_24hr,
                                           "No" = "FALSE",
                                           "Yes" = "TRUE")
  active_users_24hr <- forcats::fct_relevel(active_users_24hr, c("Yes", "No"))
  
  active_users_7d <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(contacts_unflat$last_seen_on, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours") <= 7*24
  active_users_7d <- factor(active_users_7d)
  if (length(levels(active_users_7d)) == 1){
    if (levels(active_users_7d) == "FALSE"){
      levels(active_users_7d) <- c(levels(active_users_7d),"TRUE")
    } else if (levels(active_users_7d) == "TRUE"){
      levels(active_users_7d) <- c(levels(active_users_7d),"FALSE")
    }
  }
  active_users_7d <- forcats::fct_expand(active_users_7d, c("Yes", "No"))
  active_users_7d <- forcats::fct_recode(active_users_7d,
                                         "No" = "FALSE",
                                         "Yes" = "TRUE")
  active_users_7d <- forcats::fct_relevel(active_users_7d, c("Yes", "No"))
  
  df_all <- data.frame(ID, enrolled, consent, program, parent_gender, parent_child_relationship, 
                   parent_relationship_status,
                   active_users_24hr, active_users_7d, parent_age)
  
  

# 3. Define UI -----------------------------------------------------------------------------
                                          #**MPH play with basic setup of UI and add labels relevant for ParentApp
ui <- dashboardPage(
  header = dashboardHeader(title = "ParentText Dashboard"),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Engagement", tabName = "engagement", icon = icon("clipboard")),
      menuItem("Behaviours", tabName = "behaviours", icon = icon("brain"))
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
                           height = "95px"))),
              
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
      tabItem(tabName = "engagement",
      ), # close engagement tab
      
      tabItem(tabName = "behaviours",
      ) # close behaviour tab
    ) # close items
  ) # close body
) # close function

# 3. Define Server -----------------------------------------------------------------------------
                      #**MPH play with basic setup of UI and add labels relevant for ParentApp
  
server <- function(input, output) {
  
  # Subset data
  selected_data <- reactive({
    df
  })
  
  output$groups <- renderUI({
    df_all <- df
    selectInput(
      inputId = "grouper",
      label = "Group variable",
      choices = c("Parent Gender" = "parent_gender",
                  "Child Gender" = "child_gender",
                  "Child Age Group" = "child_age_group"),
      selected = "parent_gender"
    )
  })
  
  output$plot_consent <- renderPlotly({
    ggplot(df_all, aes(x = consent)) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = "Consent", y = "Count") +
      theme_classic()
  })
  
  output$plot_consent_group <- renderPlotly({
    req(input$grouper)
    ggplot(df_all, aes(x = consent, fill = (!!!rlang::syms(input$grouper)))) +
      geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      labs(x = "Consented", y = "Count") +
      theme_classic()
  })
  
  consent_summary <- reactive({
    summary_PT(df_all, "consent", enrolled, "Yes", TRUE, naming_convention = TRUE)
  })
  
  consent_summary_group <- reactive({
    req(input$grouper)
    summary_PT(df_all, c(consent, !!!rlang::syms(input$grouper)), enrolled, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_gender_summary <- reactive({
    summary_PT(df_all, parent_gender, consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_gender_group_summary <- reactive({
    summary_PT(df_all, c(parent_gender, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_child_relationship_summary <- reactive({
    summary_PT(df_all, parent_child_relationship, consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_child_relationship_group_summary <- reactive({
    req(input$grouper)
    summary_PT(df_all, c(parent_child_relationship, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_relationship_status_summary <-  reactive({
    summary_PT(df_all, parent_relationship_status, consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  parent_relationship_status_group_summary <-  reactive({
    summary_PT(df_all, c(parent_relationship_status, !!!rlang::syms(input$grouper)), consent, "Yes", TRUE, naming_convention = TRUE)
  })
  
  active_users_24hr_group_summary <- reactive({
    req(input$grouper_engagement)
    summary_PT(df_all, c(active_users_24hr, !!!rlang::syms(input$grouper_engagement)), program, together = TRUE, naming_convention = TRUE)
  })
  
  active_users_24hr_summary <- reactive({
    summary_PT(df_all, active_users_24hr, program, together = TRUE, naming_convention = TRUE)
  })
  
  active_users_7d_summary <- reactive({
    summary_PT(df_all, active_users_7d, program, together = TRUE, naming_convention = TRUE)
  })
  
  active_users_7d_group_summary <- reactive({
    req(input$grouper_engagement)
    summary_PT(df_all, c(active_users_7d, !!!rlang::syms(input$grouper_engagement)), program, together = TRUE, naming_convention = TRUE)
  })
  
  parent_age_summary <- reactive({
    req(input$grouper)
    parent_age_df <- df_all %>% 
      filter(consent == "Yes") %>%
      summarise(parent_age_mean = round(mean(parent_age, na.rm = TRUE), 2),
                parent_age_sd = round(sd(parent_age, na.rm = TRUE), 2))
    colnames(parent_age_df) <- naming_conventions(colnames(parent_age_df))
    parent_age_df
  })
  
  parent_age_group_summary <- reactive({
    req(input$grouper)
    parent_age_df <- df_all %>% group_by(!!!rlang::syms(input$grouper)) %>%
      filter(consent == "Yes") %>%
      summarise(parent_age_mean = round(mean(parent_age, na.rm = TRUE), 2),
                parent_age_sd = round(sd(parent_age, na.rm = TRUE), 2))
    colnames(parent_age_df) <- naming_conventions(colnames(parent_age_df))
    parent_age_df
  })
  
  # Output render
  df_enrolled <- summary_PT(df_all,  enrolled,  enrolled, "Yes")
  df_enrolled <- df_enrolled %>% mutate(group =  enrolled, count = enrolled_n) %>% dplyr::select(c(group, count))
  
  df_consented <- summary_PT(df_all,  consent,  consent, "Yes")
  df_consented <- df_consented %>% mutate(group =  consent, count = consent_n) %>% dplyr::select(c(group, count))
  
  df_program <- summary_PT(df_all,  program,  program, "Yes")
  df_program <- df_program %>% mutate(group =  program, count = program_n) %>% dplyr::select(c(group, count))
  
  df_active_24 <- (summary_PT(df_all, active_users_24hr, program) %>% filter(active_users_24hr == "Yes"))$active_users_24hr_n
  df_active_7d <- (summary_PT(df_all, active_users_7d, program) %>% filter(active_users_7d == "Yes"))$active_users_7d_n
  
  output$myvaluebox1 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_enrolled$count[1], subtitle = "Enrolled", icon = icon("user"),
                             color = "aqua"
    )
  })
  output$myvaluebox2 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_consented$count[1],subtitle = "Consented",icon = icon("check"),
                             color = "green"
    )
  })
  output$myvaluebox3 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_program$count[1],subtitle = "In Program",icon = icon("clipboard"),
                             color = "yellow"
    )
  })
  output$myvaluebox4 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_active_24,subtitle = "Active in last 24 hours",icon = icon("clock"),
                             color = "purple"
    )
  })
  output$myvaluebox5 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(df_active_7d, subtitle = "Active in last 7 days", icon = icon("signal"),
                             color = "fuchsia"
    )
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
