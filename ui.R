
# ui.R
source("global.R")

shinyUI(
  fluidPage(
    titlePanel("GLOBAL HOUSING ANALYSIS — scaffold"),
    
    sidebarLayout(
      sidebarPanel(
        # Simple controls we can fill in later
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
        tabsetPanel(
          tabPanel(
            "Overview",
            h4("Data snapshot"),
            tableOutput("tbl_head"),
            br(),
            h4("Column types"),
            tableOutput("tbl_types")
          ),
          tabPanel(
            "Histogram",
            plotOutput("plot_hist"),
            br(),
            verbatimTextOutput("txt_summary")
          ),
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
  )
)
