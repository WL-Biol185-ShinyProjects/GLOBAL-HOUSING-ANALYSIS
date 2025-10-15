
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
        # a) Space for an image (we’ll wire the real image later)
        uiOutput("overview_image_placeholder"),
        br(),
        
        # b) Clear message about scope (major cities)
        tags$div(
          style = "padding: 10px; background-color: #f7f7f7; border-radius: 8px;",
          tags$strong("Global Major Cities Housing Purchase Analysis"),
          tags$p(
            "This app focuses on housing purchase data across major global cities."
          )
        ),
        br(),
        
        # c) Stats about the dataset
        tags$h4("Quick Stats"),
        uiOutput("overview_stats"),
        br(),
        
        # d) Brief introduction
        tags$h4("What this site does"),
        uiOutput("overview_intro")
      ),
      
      # ----------------------------------------------------------------
      # Histogram (sidebar present ONLY here)
      # ----------------------------------------------------------------
      tabPanel(
        "Histogram",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId   = "x_num",
              label     = "Numeric column for histogram:",
              choices   = num_cols,
              selected  = "price"
            ),
            sliderInput(
              inputId   = "bins",
              label     = "Bins:",
              min       = 10,
              max       = 100,
              value     = 30,
              step      = 5
            ),
            selectInput(
              inputId   = "group_cat",
              label     = "Optional group (color) by category:",
              choices   = c("None", cat_cols),
              selected  = "None"
            ),
            checkboxInput(
              inputId   = "log_x",
              label     = "Log scale (x-axis)",
              value     = FALSE
            )
          ),
          mainPanel(
            plotOutput("plot_hist"),
            br(),
            verbatimTextOutput("txt_summary")
          )
        )
      ),
      
      # ----------------------------------------------------------------
      # Scatter (no sidebar)
      # ----------------------------------------------------------------
      tabPanel(
        "Scatter (quick look)",
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId   = "scatter_x",
              label     = "X (numeric):",
              choices   = num_cols,
              selected  = "property_size_sqft"
            )
          ),
          column(
            width = 6,
            selectInput(
              inputId   = "scatter_y",
              label     = "Y (numeric):",
              choices   = num_cols,
              selected  = "price"
            )
          )
        ),
        plotOutput("plot_scatter")
      )
    )
  )
)
