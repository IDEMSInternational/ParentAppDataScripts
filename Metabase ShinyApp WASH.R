
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
        shinydashboard::valueBoxOutput("total_n", width=6), 
        shinydashboard::valueBoxOutput("total_users", width=6), 
        shinydashboard::valueBoxOutput("myvaluebox1", width=3), 
        shinydashboard::valueBoxOutput("myvaluebox2", width=3),
        shinydashboard::valueBoxOutput("myvaluebox3", width=3),
        shinydashboard::valueBoxOutput("myvaluebox4", width=3)
      ),
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
                      title = "User gender",
                      status = "primary",  
                      style='width:100%;overflow-x: scroll;',
                      plotlyOutput(outputId = "plot_parent_gender", height = "240"), #generates graph
                      shiny::tableOutput("table_parent_gender")  #generates table
                  ) #closes box
                ), #closes fluidRow
                
                demographics_fluid_row(study = study)
        )#closes first tabItem
      ) # closes tabItems
    ) # closes dashboardBody
  )# closes dashboardPage
  
  # 4. Define Server -----------------------------------------------------------------------------
  server <- function(input, output, session) {
    
    observe({
      source(here("Metabase Analysis Setup.R")) # approx 17 secs # so what's the rest of the time? # How long does it take overall, 
    })
    
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
    
    # If Checkbox  
    if (country == "Tanzania" & study %in% c("RCT", "WASH", "Optimisation")){
      observe({
        if(input$select_cluster){
          shinyjs::disable("opt_cluster")
        } else {
          shinyjs::enable("opt_cluster")
        }
      })
    }
    
    print("0")
    
    if (country == "Tanzania" & study == "Optimisation"){
      selected_data_dem <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
        print("1")
        if(input$select_cluster){
          opt_cluster_vals <- 1:16
        } else {
          opt_cluster_vals <- extract(input$opt_cluster)
        }
        plhdata_checkgroup <- plhdata_org_clean %>%
          dplyr::filter(Cluster %in% c(opt_cluster_vals))
        if (!is.null(input$opt_support)) {
          plhdata_checkgroup <- plhdata_checkgroup %>%
            dplyr::filter(Support %in% c(input$opt_support))
        }
        if (!is.null(input$opt_skin)) {
          plhdata_checkgroup <- plhdata_checkgroup %>%
            dplyr::filter(Skin %in% c(input$opt_skin))
        }
        if (!is.null(input$opt_diglit)) {
          plhdata_checkgroup <- plhdata_checkgroup %>%
            dplyr::filter(`Digital Literacy` %in% c(input$opt_diglit))
        }
        return(plhdata_checkgroup)
      })
    } else {
      selected_data_dem <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
        if (country == "Tanzania"){
          if (study == "Pilot"){
            plhdata_checkgroup <- plhdata_org_clean %>%
              dplyr::filter(PilotSite %in% c(input$OrgDem))
          } else if (study %in% c("RCT", "WASH")) {
            if(input$select_cluster){
              opt_cluster_vals <- unique(UIC_Tracker_Use$ClusterName)
            } else {
              opt_cluster_vals <- input$opt_cluster
            }
            plhdata_checkgroup <- plhdata_org_clean %>%
              dplyr::filter(ClusterName %in% c(opt_cluster_vals))
            print(opt_cluster_vals)
          } else {
            plhdata_checkgroup <- plhdata_org_clean
          }
        } else {
          plhdata_checkgroup <- plhdata_org_clean %>% dplyr::filter(Org %in% c((input$OrgDem)))
        }
        return(plhdata_checkgroup)
      })
    }
    
    last_sync <- reactive({
      if (country == "Tanzania"){
        time_diff <- difftime(lubridate::now(tzone = "UTC"), as.POSIXct(selected_data_dem()$updatedAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"), units = "hours")
        return(time_diff)
      }
    })
    
    last_sync_cat <- reactive({
      if (country == "Tanzania"){
        time_diff <- last_sync()
        last_sync_cat <- ifelse(time_diff > 60*24, "4", ifelse(time_diff > 30*24, "3", ifelse(time_diff > 14*24, "2", ifelse(time_diff > 7*24, "1", "0"))))
      }
    })
    
    #SUMMARY STATS HEADER displays (same for all tabs)
    #if (country == "Tanzania"){
    output$total_n <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(nrow(selected_data_dem()), subtitle = "total users", icon = icon("people"),
                               color = "aqua")})
    output$total_users <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(nrow(selected_data_dem() %>% filter(createdAt > as.Date(lubridate::now(tzone = "UTC")) - 7)), subtitle = "trial users joined in last 7 days", icon = icon("clock"),
                               color = "yellow")})
    output$myvaluebox1 <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(length(last_sync_cat()[last_sync_cat() == "1"]), subtitle = "not synced in more than 7 days", icon = icon("user"),
                               color = "green")})
    output$myvaluebox2 <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(length(last_sync_cat()[last_sync_cat() == "2"]), subtitle = "not synced in more than 14 days", icon = icon("user"),
                               color = "fuchsia")})
    output$myvaluebox3 <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(length(last_sync_cat()[last_sync_cat() == "3"]), subtitle = "not synced in more than 30 days", icon = icon("user"),
                               color = "purple")})
    output$myvaluebox4 <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(length(last_sync_cat()[last_sync_cat() == "4"]), subtitle = "not synced in more than 60 days", icon = icon("user"),
                               color = "orange")})
    
    opt_factors <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      print("2")
      if (country == "Tanzania"){
        if (study == "Pilot"){
          opt_factors <- c("PilotSite")
        } else if (study == "Optimisation"){
          opt_factors <- c()
          if (!is.null(input$opt_support)){
            opt_factors <- c(opt_factors, "Support")
          }
          if (!is.null(input$opt_skin)){
            opt_factors <- c(opt_factors, "Skin")
          }
          if (!is.null(input$opt_diglit)){
            opt_factors <- c(opt_factors, "Digital Literacy")
          }
          if (length(opt_factors) == 0){
            opt_factors <- c("Org")
          }
        } else if (study %in% c("RCT", "WASH")){
          opt_factors <- c("ClusterName")
        } else {
          opt_factors <- c("Org")
        }
      } else {
        opt_factors <- c("Org")
      }
      return(opt_factors)
    })
    
    # summary_table_filter <- function(summary_workshop, add_totals = TRUE){
    #   print("3")
    #   if (country == "Tanzania"){
    #     if (study == "Pilot"){
    #       summary_workshop <- summary_workshop %>%
    #         dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
    #         mutate(PilotSite = as.character(PilotSite))
    #       if (add_totals){
    #         summary_workshop <- summary_workshop %>%
    #         janitor::adorn_totals(c("row", "col"))
    #       }
    #     } else if (study == "Optimisation"){
    #       if (!is.null(input$opt_support)){
    #         summary_workshop <- summary_workshop %>%
    #           dplyr::filter(Support %in% c(selected_data_dem()$Support)) %>%
    #           mutate(Support = as.character(Support))
    #       }
    #       if (!is.null(input$opt_skin)){
    #         summary_workshop <- summary_workshop %>%
    #           dplyr::filter(Skin %in% c(selected_data_dem()$Skin)) %>%
    #           mutate(Skin = as.character(Skin))
    #       }
    #       if (!is.null(input$opt_diglit)){
    #         summary_workshop <- summary_workshop %>%
    #           dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`)) %>%
    #           mutate(`Digital Literacy` = as.character(`Digital Literacy`))
    #       }
    #       if (add_totals){
    #         summary_workshop <- summary_workshop %>%
    #           janitor::adorn_totals(c("row", "col"))
    #       } else {
    #         summary_workshop <- summary_workshop
    #       }
    #     } else {
    #       summary_workshop <- summary_workshop %>%
    #         dplyr::filter(Org %in% unique(selected_data_dem()$Org))
    #     }
    #   } else {
    #     summary_workshop <- summary_workshop %>%
    #       dplyr::filter(Org %in% unique(selected_data_dem()$Org)) #%>%
    #     #janitor::adorn_totals(c("row", "col")))
    #   }
    #   return(summary_workshop)
    # }
    
    mult_summary_table_filter <- function(summary_table = summary_table_baseline_build){
      print("4")
      # if (country == "Tanzania"){
      #   if (study == "Pilot"){
      #     summary_table <- summary_table %>% 
      #       purrr::map(.f =~.x %>%
      #                    dplyr::filter(PilotSite %in% c(selected_data_dem()$PilotSite)) %>%
      #                    mutate(PilotSite = as.character(PilotSite)) %>%
      #                    janitor::adorn_totals(c("row", "col")))
      #   } else if (study == "Optimisation"){
      # if (!is.null(input$opt_support)){
      #   summary_table <- summary_table %>% 
      #     purrr::map(.f =~.x %>%
      #                  dplyr::filter(Support %in% c(selected_data_dem()$Support))%>%
      #                  mutate(Support = as.character(Support)))
      # }
      # if (!is.null(input$opt_skin)){
      #   summary_table <- summary_table %>% 
      #     purrr::map(.f =~.x %>%
      #                  dplyr::filter(Skin %in% c(selected_data_dem()$Skin))%>%
      #                  mutate(Skin = as.character(Skin)))
      # }
      # if (!is.null(input$opt_diglit)){
      #   summary_table <- summary_table %>% 
      #     purrr::map(.f =~.x %>%
      #                  dplyr::filter(`Digital Literacy` %in% c(selected_data_dem()$`Digital Literacy`))%>%
      #                  mutate(`Digital Literacy` = as.character(`Digital Literacy`)))
      # }
      summary_table <- summary_table %>% 
        purrr::map(.f =~.x %>%
                     janitor::adorn_totals(c("row", "col")))
      #   } else {
      #     summary_table <- summary_table %>% 
      #       purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org)))
      #   }
      # } else {
      #   summary_table <- summary_table %>% 
      #     purrr::map(.f =~.x %>% dplyr::filter(Org %in% unique(selected_data_dem()$Org))) #%>%
      #   #janitor::adorn_totals(c("row", "col")))
      # }
      return(summary_table) 
    }
    
    # Demographics ---------------------------------------------------
    summary_table_baseline <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
      if (study %in% c("RCT", "WASH")){
        summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(),
                                 columns_to_summarise = data_baseline_survey$metabase_ID,
                                 include_perc = TRUE) %>%
          purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
      } else {      
        summary_table_baseline_build <- summary_table_base_build(opt_factors = opt_factors(), data = selected_data_dem(), columns_to_summarise = data_baseline_survey$metabase_ID)
        summary_table_baseline_build %>%
          purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0))) %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))
      }
    })
    
    plot_app_downloaded  <- reactive({ # last sync
      ggplot(data = selected_data_dem(), aes(x = as.POSIXct(createdAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))) +
        geom_freqpoly(bins = 30) +
        labs(x = "Created Profile (createdAt)", y = "Count")
    }) 
    output$plot_app_downloaded <- renderPlotly({plot_app_downloaded()})
    
    #table_app_launch <- reactive({}) 
    plot_app_launch  <- reactive({ # last sync
      ggplot(data = selected_data_dem(), aes(x = as.POSIXct(updatedAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))) +
        geom_freqpoly(bins = 30) +
        labs(x = "Last sync (updatedAt)", y = "Count")
    }) 
    output$plot_app_launch <- renderPlotly({plot_app_launch()})
    
    #Overview and Demographics plot and table
    display_sheet_table <- function(n = "language", j = 1){
      return(output[[paste0("table_", n)]] <- shiny::renderTable({(summary_table_baseline()[[j]])}, striped = TRUE))
    }
    display_sheet_plot <- function(n = "language", j = 1){
      return(output[[paste0("plot_", n)]] <-  renderPlotly(summary_plot(data = selected_data_dem(), columns_to_summarise = j, replace = "rp.contact.field.")))
    } # plottype = histogram
    
    # run our table_baselines and plot_baselines # TODO: in PLHr function, replace for loop with map like this.
    map2(data_baseline_survey$display_name, data_baseline_survey$object_name, .f = ~ display_sheet_table(n = .y, j = .x))
    map2(data_baseline_survey$metabase_ID, data_baseline_survey$object_name, .f = ~ display_sheet_plot(n = .y, j = .x))
    
    
    map2(data_baseline_survey$display_name, data_baseline_survey$object_name, .f = ~ display_sheet_table(n = .y, j = .x))
    
    
    # bit different for age
    # RCT TODO HERE - not called age i nRCT
    #output$table_parent_age <- shiny::renderTable({(selected_data_dem() %>% summary_table(columns_to_summarise = rp.contact.field.user_age, factors = opt_factors(), summaries = "mmm"))}, striped = TRUE)
    #output$plot_parent_age <- renderPlotly({summary_plot(data = selected_data_dem(), columns_to_summarise = "rp.contact.field.user_age", replace = "rp.contact.field.", plot_type = "histogram")})
    
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
