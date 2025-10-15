
# server.R
source("global.R")

shinyServer(function(input, output, session) {
  
  # Overview ------------------------------------------------------------
  
  output$tbl_head <- renderTable({
    house_head
  }, striped = TRUE, spacing = "s")
  
  output$tbl_types <- renderTable({
    tibble(
      column = names(house),
      type   = map_chr(house, ~ class(.x)[1])
    )
  }, striped = TRUE, spacing = "s")
  
  # Histogram -----------------------------------------------------------
  
  chosen_num <- reactive({
    req(input$x_num %in% names(house))
    house %>%
      select(all_of(input$x_num)) %>%
      drop_na() %>%
      pull()
  })
  
  output$plot_hist <- renderPlot({
    x_vals <- chosen_num()
    req(length(x_vals) > 0)
    
    df <- tibble(x = x_vals)
    
    if (input$group_cat != "None") {
      grp <- house[[input$group_cat]]
      df  <- tibble(x = x_vals, g = grp) %>% drop_na()
      gg  <- ggplot(df, aes(x = x, fill = g)) +
        geom_histogram(bins = input$bins, alpha = 0.7, position = "identity")
    } else {
      gg  <- ggplot(df, aes(x = x)) +
        geom_histogram(bins = input$bins)
    }
    
    gg <- gg +
      labs(x = input$x_num, y = "Count", fill = input$group_cat) +
      theme_minimal()
    
    if (isTRUE(input$log_x)) {
      gg <- gg + scale_x_log10()
    }
    
    gg
  })
  
  output$txt_summary <- renderPrint({
    summary(chosen_num())
  })
  
  # Scatter -------------------------------------------------------------
  
  output$plot_scatter <- renderPlot({
    req(input$scatter_x %in% names(house))
    req(input$scatter_y %in% names(house))
    
    df <- house %>%
      select(all_of(c(input$scatter_x, input$scatter_y))) %>%
      drop_na()
    
    ggplot(df, aes(x = .data[[input$scatter_x]],
                   y = .data[[input$scatter_y]])) +
      geom_point(alpha = 0.5) +
      labs(x = input$scatter_x, y = input$scatter_y) +
      theme_minimal()
  })
  
})
