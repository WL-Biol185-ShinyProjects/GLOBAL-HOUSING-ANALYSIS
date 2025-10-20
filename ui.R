
# ui.R
source("global.R")

dashboardPage(
  dashboardHeader(title = "Global Housing Analysis"),
  
  # Global nav only; collapsed so it doesn't feel like a sidebar on every tab
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      id = "mainmenu",
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Descriptive Market Insights", tabName = "insights", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # ----------------------------------------------------------------
      # OVERVIEW
      # ----------------------------------------------------------------
      tabItem(
        tabName = "overview",
        
        fluidRow(
          box(
            title = "Header Image", width = 12, solidHeader = TRUE, status = "primary",
            uiOutput("overview_image_placeholder"), height = 280
          )
        ),
        
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE, title = "Scope",
            tags$strong("Global Major Cities Housing Purchase Analysis"),
            p("This app focuses on housing purchase data across major global cities.")
          )
        ),
        
        fluidRow(
          valueBoxOutput("vb_min_year", width = 3),
          valueBoxOutput("vb_countries", width = 3),
          valueBoxOutput("vb_cities", width = 3),
          valueBoxOutput("vb_rows", width = 3)
        ),
        
        fluidRow(
          box(
            title = "What this site does", width = 12, status = "warning", solidHeader = TRUE,
            uiOutput("overview_intro")
          )
        )
      ),
      
      # ----------------------------------------------------------------
      # DESCRIPTIVE MARKET INSIGHTS
      # ----------------------------------------------------------------
      tabItem(
        tabName = "insights",
        
        # Currency selector row (controls live in-body, not in the global sidebar)
        fluidRow(
          box(
            title = "Currency", width = 12, status = "primary", solidHeader = TRUE,
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
                uiOutput("fx_note")
              )
            )
          )
        ),
        
        # a) Average prices by country/city/property type
        fluidRow(
          box(title = "Average Prices by Country", width = 4, status = "info", solidHeader = TRUE,
              tableOutput("tbl_avg_country")),
          box(title = "Average Prices by City", width = 4, status = "info", solidHeader = TRUE,
              tableOutput("tbl_avg_city")),
          box(title = "Average Prices by Property Type", width = 4, status = "info", solidHeader = TRUE,
              tableOutput("tbl_avg_property"))
        ),
        
        # b) Size vs Price
        fluidRow(
          box(title = "Property Size vs Price (binned)", width = 12, status = "success", solidHeader = TRUE,
              plotOutput("plot_size_price", height = "320px"))
        ),
        
        # c) Amenities in top-quartile homes
        fluidRow(
          box(title = "Amenities in High-Value Homes", width = 12, status = "success", solidHeader = TRUE,
              plotOutput("plot_amenities_hv", height = "300px"))
        ),
        
        # d) Age impact
        fluidRow(
          box(title = "Age of House Impact on Value", width = 12, status = "success", solidHeader = TRUE,
              plotOutput("plot_age_impact", height = "300px"))
        ),
        
        # e) Interactive Map with overlay reset button
        fluidRow(
          box(title = "Interactive Map of Average Prices", width = 12, status = "primary", solidHeader = TRUE,
              p("Hover on a country to see the average price. Click a country to drill into its cities."),
              div(
                style = "position: relative;",
                leafletOutput("map_prices", height = 520),
                tags$div(
                  style = "position: absolute; top: 15px; right: 15px; z-index: 1000;",
                  actionButton("btn_reset_map", "Back to Global View", class = "btn-primary")
                )
              )
          )
        )
      )
    )
  )
)
