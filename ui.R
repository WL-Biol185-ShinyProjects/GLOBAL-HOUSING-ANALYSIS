
# ui.R
source("global.R")

dashboardPage(
  dashboardHeader(title = "Global Housing Analysis"),
  
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
        
        # Currency selector row
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
              column(width = 6, uiOutput("fx_note"))
            )
          )
        ),
        
        # Shared filters (Country + Property type)
        fluidRow(
          box(
            title = "Filters (apply to charts below)", width = 12, status = "info", solidHeader = TRUE,
            fluidRow(
              column(
                width = 6,
                selectInput("flt_country", "Country", choices = c("All", sort(unique(house$country))), selected = "All")
              ),
              column(
                width = 6,
                selectInput("flt_property", "Property type", choices = c("All", sort(unique(house$property_type))), selected = "All")
              )
            )
          )
        ),
        
        # a) Size vs Price (Plotly) + selected bin summary
        fluidRow(
          box(title = "Property Size vs Price (binned)", width = 8, status = "success", solidHeader = TRUE,
              plotlyOutput("plot_size_price", height = "360px")),
          box(title = "Selected Bin Summary", width = 4, status = "success", solidHeader = TRUE,
              uiOutput("bin_summary"))
        ),
        
        # b) Amenities pie + Furnishing pie (Plotly)
        fluidRow(
          box(title = "Amenities in Selected Price Quartile", width = 6, status = "warning", solidHeader = TRUE,
              selectInput("amen_quartile", "Price quartile (computed within current filters)",
                          choices = c("Q1 (lowest)" = "Q1", "Q2" = "Q2", "Q3" = "Q3", "Q4 (highest)" = "Q4"),
                          selected = "Q4"),
              plotlyOutput("pie_amenities", height = "320px")),
          box(title = "Furnishing Status in Selected Price Quartile", width = 6, status = "warning", solidHeader = TRUE,
              plotlyOutput("pie_furnish", height = "320px"))
        ),
        
        # c) Age impact (Plotly with box-select to ZOOM + reset)
        fluidRow(
          box(
            title = "Age of House Impact on Value (drag to zoom a year range)",
            width = 12, status = "success", solidHeader = TRUE,
            # ↓ Add the reset button just above the plot
            actionButton("btn_reset_age", "Reset view",
                         class = "btn-primary", style = "float:right;"),
            plotlyOutput("plot_age_impact", height = "360px")
          )
        ),
        
        # d) Interactive Map with overlay reset button + size legend
        fluidRow(
          box(title = "Interactive Map of Average Prices", width = 12, status = "primary", solidHeader = TRUE,
              p("Hover on a country to see the average price. Click a country to drill into its cities. Hover over a city for details."),
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
