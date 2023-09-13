#### Defining the UI ####
ui <- dashboardPage(
  dashboardHeader(title = "WASH App Data Viz"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Modules Completion Rate", tabName = "page1"),
      menuItem("Sections Completion Rate", tabName = "page2"),
      menuItem("Toggle Usage", tabName = "page3")
    )
  ),
  dashboardBody(
    tabItems(
      # First Page ----
      tabItem(tabName = "page1",
              fluidRow(
                column(2,
                       selectInput("module_id_page1", label = h4("Select Module ID"), 
                                   choices = c(1:12), selected = 1)
                ),
                column(10,
                       fluidRow(
                         column(6,
                                box(plotOutput("plot_module_completion_rate_pie"), width = 12, title = "Module Completion Rate")),
                         column(6,
                                box(plotOutput("plot_module_click_rate_pie"), width=12, title = "Module Card Click Rate"))
                       ),
                       box(plotlyOutput("plot_module_completion_rate"), width = 12, title = "Modules Completion Rate Comparison"),
                       box(plotlyOutput("plot_module_click_rate"), width = 12, title = "Modules Card Click Rate")
                )
              )
      ),
      tabItem(
        # Second Page ----
        tabName = "page2",
        fluidRow(
          column(2,
                 selectInput("module_id_page2", label = h4("Select Module ID"), 
                             choices = c(1:12), selected = 1),
                 selectInput("section_id_page2", label = h4("Select Section ID"), 
                             choices = c(), selected = NULL)
          ),
          column(10,
                 fluidRow(
                   column(6,
                          box(plotOutput("plot_section_completion_rate_pie"), width = 12, title = "Section Completion Rate")),
                   column(6,
                          box(plotOutput("plot_section_click_rate_pie"), width=12, title = "Section Card Click Rate"))
                 ),
                 box(plotlyOutput("plot_section_completion_rate"), width = 12, title = "Sections Completion Rate Comparison"),
                 box(plotlyOutput("plot_section_click_rate"), width = 12, title = "Sections Card Click Rate")
          )
        )
      ),
      tabItem(
        # Third Page ----
        tabName = "page3",
        fluidRow(
          column(2,
                 selectInput("module_id_page3", label = h4("Select Module ID"), 
                             choices = c(1:12), selected = 1),
                 selectInput("toggle_page3", label = h4("Select Toggle Status"), 
                             choices = c("toggle", "not_toggle"), selected = NULL)
          ),
          column(10,
                 fluidRow(
                   column(6,
                          box(plotOutput("plot_started_rate_pie"), width = 12, title = "Started Toggle type")),
                   column(6,
                          box(plotOutput("plot_completed_rate_pie"), width = 12, title = "Completed Toggle Type"))
                 ),
                 box(plotlyOutput("plot_started_rate"), width = 12, title = "Started Toggle type"),
                 box(plotlyOutput("plot_completed_rate"), width = 12, title = "Completed Toggle Type")
          )
        )
      )
    )
  )
)



#### The Server ####
server <- function(input, output, session) {
  #### Tab 1 Output Starts here ####
  output$plot_module_completion_rate_pie <- renderPlot({
    
    ggplot(CDF[CDF$Module_ID == input$module_id_page1,],
           aes(x = "", y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = paste("Module", input$module_id_page1, "User Completion Rate"),
           x = NULL, y = NULL) +
      theme_void() +
      scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c"))
  })
  
  output$plot_module_click_rate_pie <- renderPlot({
    
    ggplot(MCP[MCP$Module_ID == input$module_id_page1,],
           aes(x = "", y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(
        title = paste("Module", input$module_id_page1, "User Click Rate"),
        x = NULL, y = NULL) +
      theme_void() +
      scale_fill_manual(values = c("#1f78b4", "#33a02c"))
    
  })
  
  output$plot_module_completion_rate <- renderPlotly({
    ggplotly(
      ggplot(CDF, aes(x = Module_Name, y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Module Completion Rate",
             x = "Modules",
             y = "Percentage") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 20),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16)) +
        scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c")),
      tooltip = "Percentage"
    )
  })
  
  output$plot_module_click_rate <- renderPlotly({
    ggplotly(
      ggplot(MCP, aes(x = Module_Name, y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Module Click Rate",
             x = "Modules",
             y = "Percentage") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 20),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16)) +
        scale_fill_manual(values = c("#1f78b4", "#33a02c")),
      tooltip = "Percentage"
    )
  })
  
  # Tab 1 Ends here
  
  #### Tab 2 Output Starts here ####
  
  observeEvent(input$module_id_page2, {
    section_choices <- unique(module_info[module_info$module_id == input$module_id_page2, "section_id"])
    updateSelectInput(session, "section_id_page2", choices = section_choices)
  })
  
  output$plot_section_completion_rate <- renderPlotly({
    ggplotly(
      ggplot(data = calculate_section_completion_percentage(
        module_id = input$module_id_page2),
        aes(x = Section_Name, y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Sections Completion Rate",
             x = "Sections", y = "Percentage") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 20),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16)) +
        scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c")),
      tooltip = "Percentage"
    )
  })
  
  output$plot_section_completion_rate_pie <- renderPlot({
    
    ggplot(data = calculate_section_completion_percentage(
      module_id = input$module_id_page2) %>%
        .[.$Section_ID == input$section_id_page2,],
      aes(x = "", y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(
        title = paste("Section", input$section_id_page2, "User Completion Rate"), #input$section_id_page2
        x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 20),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16)) +
      scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c"))
    
  })
  
  output$plot_section_click_rate <- renderPlotly({
    ggplotly(
      ggplot(
        data = calculate_section_click_percentage(
          module_id = input$module_id_page2),
        aes(x = Section_Name, y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Sections Click Rate",
             x = "Sections", y = "Percentage") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 20),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16)) +
        scale_fill_manual(values = c("#1f78b4", "#33a02c")),
      tooltip = "Percentage"
    )
  })
  
  output$plot_section_click_rate_pie <- renderPlot({
    
    ggplot(data = calculate_section_click_percentage(module_id = input$module_id_page2) %>%
             .[.$Section_ID == input$section_id_page2,],#input$section_id_page2
           aes(x = "", y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(
        title = paste("Section", input$section_id_page2, #input$section_id_page2
                      "User Click Rate"), x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 20),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16)) +
      scale_fill_manual(values = c("#1f78b4", "#33a02c"))
    
  })
  # Tab 2 ends here
  # Tab3 starts here ----
  
  output$plot_started_rate <- renderPlotly({
    ggplotly(
      ggplot(
        calculate_toggle_percentage(toggle_type = "started",
                                    toggle_status = input$toggle_page3),
        aes(x = Module_Name, y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Started with", input$toggle_page3),
             x = "Modules", y = "Percentage")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 20),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16)) +
        scale_fill_manual(values = c("#1f78b4", "#33a02c")),
      tooltip = "Percentage"
    )
  })
  
  output$plot_started_rate_pie <- renderPlot({
    
    ggplot(
      data = calculate_toggle_percentage(
        toggle_type = "started", toggle_status = input$toggle_page3) %>%
        .[.$Module_ID == input$module_id_page3,],
      aes(x = "", y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +coord_polar("y", start = 0) +
      labs(
        title = paste("Module", input$module_id_page3, input$toggle_page3, "Usage Rate"),
        x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 20),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16)) +
      scale_fill_manual(values = c("#1f78b4", "#33a02c"))
  })
  
  output$plot_completed_rate <- renderPlotly({
    ggplotly(
      ggplot(calculate_toggle_percentage(toggle_type = "completed",
                                         toggle_status = input$toggle_page3),
             aes(x = Module_Name, y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Completed with", input$toggle_page3),
             x = "Modules", y = "Percentage") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 20),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16)) +
        scale_fill_manual(values = c("#1f78b4", "#33a02c")),
      tooltip = "Percentage")
  })
  
  output$plot_completed_rate_pie <- renderPlot({
    
    
    ggplot(data = calculate_toggle_percentage(toggle_type = "completed",
                                              toggle_status = input$toggle_page3) %>%
             .[.$Module_ID == input$module_id_page3,],
           aes(x = "", y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(
        title = paste("Module", input$module_id_page3, input$toggle_page3, "Usage Rate"),
        x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 20),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16)) +
      scale_fill_manual(values = c("#1f78b4", "#33a02c"))})
}


