
# server.R
source("global.R")

shinyServer(function(input, output, session) {
  
  # --------------------------------------------------------------------
  # Overview: image placeholder, stats, brief intro
  #   - relies on dataset_stats computed once in global.R
  # --------------------------------------------------------------------
  output$overview_image_placeholder <- renderUI({
    tags$div(
      style = paste(
        "height: 240px; border: 2px dashed #bbb;",
        "border-radius: 8px; display: flex; align-items: center;",
        "justify-content: center; background-color: #fcfcfc;"
      ),
      tags$span(
        style = "color:#888;",
        "Image placeholder — add visualization or header image here"
      )
    )
  })
  
  output$overview_stats <- renderUI({
    tags$ul(
      tags$li(
        sprintf(
          "Homes with build dates back to %s.",
          dataset_stats$min_constructed_year
        )
      ),
      tags$li(
        sprintf(
          "Data from %s countries and %s cities.",
          dataset_stats$n_countries,
          dataset_stats$n_cities
        )
      ),
      tags$li(
        sprintf(
          "%s purchase decision datapoints.",
          format(dataset_stats$n_rows, big.mark = ",")
        )
      )
    )
  })
  
  output$overview_intro <- renderUI({
    tags$p(
      "Use this application to explore housing purchase patterns across major global cities. ",
      "Start with the Histogram tab to examine the distribution of prices or other numeric features, ",
      "optionally grouped by a categorical field. Then use the Scatter tab for quick two-variable ",
      "relationships (e.g., property size vs. price)."
    )
  })
  
  # --------------------------------------------------------------------
  # Histogram
  # --------------------------------------------------------------------
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
  
  # --------------------------------------------------------------------
  # Scatter
  # --------------------------------------------------------------------
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
