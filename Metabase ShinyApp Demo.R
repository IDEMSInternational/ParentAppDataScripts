
# 1. Functions -------------------------------------------------------------------------
# 3. Define UI -----------------------------------------------------------------------------
parentapp_shiny <- function(country, study){
  # Define UI
  ui <- dashboardPage(
    header = dashboardHeader(title = paste(country, study, "ParentApp Dashboard")),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem("Overview and Demographics", tabName = "demographics", icon = icon("users"))
      )), #closes sidebarMenu and dashboardSidebar
    
    dashboardBody(# Boxes need to be put in a row (or column)
      #top_boxes(country = country), #closes fluidRow
      fluidRow(
        shinydashboard::valueBoxOutput("myvaluebox1", width=3), 
        shinydashboard::valueBoxOutput("myvaluebox2", width=3),
        shinydashboard::valueBoxOutput("myvaluebox3", width=3),
        shinydashboard::valueBoxOutput("myvaluebox4", width=3)
      ),
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
                
                fluid_row_box(variable1 = "language", variable2 = "ws_format",
                              title1 = "Language", title2 = "Workshop format"),
                
                fluid_row_box(variable1 = "app_version", variable2 = "parent_gender",
                              title1 = "App version", title2 = "Parent gender"),
                
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
        )#closes first tabItem
      ) # closes tabItems
    ) # closes dashboardBody
  )# closes dashboardPage
  
  # 4. Define Server -----------------------------------------------------------------------------
  server <- function(input, output, session) {
    
    observe({
      source(here("Metabase Analysis Setup.R")) # approx 17 secs # so what's the rest of the time? # How long does it take overall, 
    })

    selected_data_dem <- reactive({
        plhdata_org_clean
    })
    
    last_sync <- reactive({
      if (country == "Tanzania"){
        time_diff <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(selected_data_dem()$updatedAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours")
        return(time_diff)
      }
    })
    
    #SUMMARY STATS HEADER displays (same for all tabs)
    #if (country == "Tanzania"){
    output$myvaluebox1 <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(length(last_sync()[last_sync() > 7*24]), subtitle = "not synced in last 7 days", icon = icon("user"),
                               color = "green")})
    output$myvaluebox2 <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(length(last_sync()[last_sync() > 14*24]), subtitle = "not synced in last 14 days", icon = icon("user"),
                               color = "yellow")})
    output$myvaluebox3 <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(length(last_sync()[last_sync() > 30*24]), subtitle = "not synced in last 30 days", icon = icon("user"),
                               color = "purple")})
    output$myvaluebox4 <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(length(last_sync()[last_sync() > 60*24]), subtitle = "not synced in last 60 days", icon = icon("user"),
                               color = "orange")})
    
    opt_factors <- reactive({
      if (country == "Tanzania"){
        if (study == "Pilot"){
          opt_factors <- c("PilotSite")
        } else if (study == "Optimisation"){
            opt_factors <- c("Support", "Skin", "Digital Literacy")
        } else if (study == "RCT"){
          opt_factors = "ClusterName"
        }
      }
      return(opt_factors)
    })
    
    # Demographics ---------------------------------------------------
    summary_table_baseline <- reactive({
      summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(), columns_to_summarise = data_baseline_survey$metabase_ID)
      summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
    })
    
    table_app_launch <- reactive({}) 
    plot_app_launch  <- reactive({ # last sync
      ggplot(data = selected_data_dem(), aes(x = as.POSIXct(updatedAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))) +
        geom_freqpoly(bins = 30) +
        labs(x = "Last sync (updatedAt)", y = "Count")
    }) 
    output$table_app_launch <- shiny::renderTable({(table_app_launch())}, striped = TRUE)
    output$plot_app_launch <- renderPlotly({plot_app_launch()})
    
    #Overview and Demographics plot and table
    display_sheet_table <- function(n = "language", j = 1){
      return(output[[paste0("table_", n)]] <-  shiny::renderTable({(summary_table_baseline()[[j]])}, striped = TRUE))
    }
    display_sheet_plot <- function(n = "language", j = 1){
      return(output[[paste0("plot_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(), columns_to_summarise = j, replace = "rp.contact.field.")))
    } # plottype = histogram
    
    # run our table_baselines and plot_baselines # TODO: in PLHr function, replace for loop with map like this.
    map2(data_baseline_survey$display_name, data_baseline_survey$object_name, .f = ~ display_sheet_table(n = .y, j = .x))
    map2(data_baseline_survey$metabase_ID, data_baseline_survey$object_name, .f = ~ display_sheet_plot(n = .y, j = .x))
    
    # bit different for age
    output$table_parent_age <- shiny::renderTable({(selected_data_dem() %>% summary_table(columns_to_summarise = rp.contact.field.user_age, factors = opt_factors(), summaries = "mmm"))}, striped = TRUE)
    output$plot_parent_age <- renderPlotly({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.user_age", replace = "rp.contact.field.", plot_type = "histogram")})
    
    #App version
    plot_app_version  <- reactive({
      plhdata_org_clean_1 <- selected_data_dem()
      plhdata_org_clean_1 <- plhdata_org_clean_1 %>%
        tidyr::unite(col = "Org", opt_factors())
      ggplot(plhdata_org_clean_1, aes(x = app_version, fill = Org)) +
        geom_bar(position = "dodge") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        labs(x = "App version")
      #summary_plot(plhdata_org_clean, app_version)
    })
    output$plot_app_version <- renderPlotly({plot_app_version()})

  } #close server
  shinyApp(ui = ui, server = server)
} # close function
