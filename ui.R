
# ui.R
source("global.R")

shinyUI(
  fluidPage(
    titlePanel("GLOBAL HOUSING ANALYSIS — scaffold"),
    
    tabsetPanel(
      # ----------------------------------------------------------------
      # Overview (no sidebar)
      # ----------------------------------------------------------------
      tabPanel(
        "Overview",
        br(),
        uiOutput("overview_image_placeholder"),
        br(),
        tags$div(
          style = "padding: 10px; background-color: #f7f7f7; border-radius: 8px;",
          tags$strong("Global Major Cities Housing Purchase Analysis"),
          tags$p("This app focuses on housing purchase data across major global cities.")
        ),
        br(),
        tags$h4("Quick Stats"),
        uiOutput("overview_stats"),
        br(),
        tags$h4("What this site does"),
        uiOutput("overview_intro")
      ),
      
      # ----------------------------------------------------------------
      # Descriptive Market Insights w/ Interactive Map
      # ----------------------------------------------------------------
      tabPanel(
        "Descriptive Market Insights w/ Interactive Map",
        
        # a) Currency basis selector
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId  = "currency_basis",
              label    = "Currency basis",
              choices  = c("Local currency", "USD (FX, nominal)"),
              selected = "Local currency"
            )
          ),
          column(
            width = 6,
            uiOutput("fx_note", inline = TRUE)
          )
        ),
        br(),
        
        # b) Avg prices by country / city / property type
        h4("Average house prices"),
        fluidRow(
          column(
            width = 4,
            h5("By Country"),
            tableOutput("tbl_avg_country")
          ),
          column(
            width = 4,
            h5("By City"),
            tableOutput("tbl_avg_city")
          ),
          column(
            width = 4,
            h5("By Property Type"),
            tableOutput("tbl_avg_property")
          )
        ),
        br(),
        
        # c) Size vs Price line chart
        h4("Trends: Property size vs Price"),
        plotOutput("plot_size_price", height = "300px"),
        br(),
        
        # d) Amenities in high-value homes
        h4("Amenities most common in high-value homes"),
        plotOutput("plot_amenities_hv", height = "280px"),
        br(),
        
        # e) Age impact
        h4("Age of house impact on value"),
        plotOutput("plot_age_impact", height = "280px"),
        br(),
        
        # f) Interactive map
        h4("Interactive Map of Average Prices"),
        p("Hover on a country to see the average price. Click a country to drill into its cities."),
        # Wrap the map in a positioned container and overlay a reset button
        div(
          style = "position: relative;",
          leafletOutput("map_prices", height = 500),
          tags$div(
            style = "position: absolute; top: 15px; right: 15px; z-index: 1000;",
            actionButton("btn_reset_map", "Back to Global View", class = "btn-primary")
          )
        )
      )
    )
  )
)
