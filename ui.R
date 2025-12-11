
# ui.R
# Enhanced UI with improved visual styling

ui <- dashboardPage(
  skin = "blue",
  
  # -------------------------------------------------------------------------
  # Header
  # -------------------------------------------------------------------------
  dashboardHeader(
    title = tags$a(
      href = "#",
      onclick = "Shiny.setInputValue('go_home', Math.random())",
      style = "color: white; text-decoration: none; display: flex; align-items: center;",
      icon("home", style = "margin-right: 8px;"),
      "Redfin Housing Explorer"
    ),
    titleWidth = 320
  ),
  
  # -------------------------------------------------------------------------
  # Sidebar
  # -------------------------------------------------------------------------
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      id = "tabs",
      
      # Sidebar header
      tags$div(
        style = "padding: 15px 15px 10px 15px; color: #95A5A6; font-size: 11px; text-transform: uppercase; letter-spacing: 1px;",
        "Navigation"
      ),
      
      menuItem(
        text     = "Home",
        tabName  = "home",
        icon     = icon("house"),
        selected = TRUE
      ),
      
      menuItem(
        text    = "Market Overview",
        tabName = "overview",
        icon    = icon("chart-line")
      ),
      
      menuItem(
        text    = "Region Explorer",
        tabName = "region_explorer",
        icon    = icon("search-location")
      ),
      
      menuItem(
        text    = "Market Heat",
        tabName = "market_heat",
        icon    = icon("fire")
      ),
      
      menuItem(
        text    = "Cross-Section Snapshot",
        tabName = "snapshot",
        icon    = icon("camera")
      ),
      
      menuItem(
        text    = "Pricing Guidance",
        tabName = "pricing",
        icon    = icon("dollar-sign")
      ),
      
      menuItem(
        text    = "Methods & Limitations",
        tabName = "methods",
        icon    = icon("book")
      ),
      
      # Sidebar footer with data info
      tags$div(
        style = "position: absolute; bottom: 0; width: 100%; padding: 15px; color: #7F8C8D; font-size: 11px; border-top: 1px solid #4a5568;",
        tags$div(
          icon("database", style = "margin-right: 5px;"),
          "Redfin Monthly Data"
        ),
        tags$div(
          style = "margin-top: 5px;",
          icon("calendar", style = "margin-right: 5px;"),
          paste("Jan 2012 –", format(latest_month, "%b %Y"))
        )
      )
    )
  ),
  
  # -------------------------------------------------------------------------
  # Body
  # -------------------------------------------------------------------------
  dashboardBody(
    # Inject custom CSS
    tags$head(
      tags$style(HTML(app_css)),
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #ECF0F1;
        }
      ")),
      # JavaScript to fix slider date format
      tags$script(HTML("
        $(document).ready(function() {
          // Function to format date as 'Mon YYYY' using UTC to avoid timezone issues
          function formatSliderDate(timestamp) {
            var date = new Date(timestamp);
            var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                          'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
            return months[date.getUTCMonth()] + ' ' + date.getUTCFullYear();
          }
          
          // Function to apply formatting to a slider
          function applyDateFormat(sliderId) {
            var slider = $('#' + sliderId);
            var ionSlider = slider.data('ionRangeSlider');
            if (ionSlider) {
              ionSlider.update({
                prettify: function(num) {
                  return formatSliderDate(num);
                }
              });
            }
          }
          
          // List of all date slider IDs
          var dateSliders = ['ov_date_range', 're_date_range', 'mh_date_range', 'ss_month', 'pg_month'];
          
          // Apply formatting when sliders are initialized
          // Use MutationObserver to catch when sliders are added to DOM
          var observer = new MutationObserver(function(mutations) {
            dateSliders.forEach(function(id) {
              var slider = $('#' + id);
              if (slider.length && slider.data('ionRangeSlider') && !slider.data('formatted')) {
                applyDateFormat(id);
                slider.data('formatted', true);
              }
            });
          });
          
          observer.observe(document.body, { childList: true, subtree: true });
          
          // Also try to format on initial load after a delay
          setTimeout(function() {
            dateSliders.forEach(function(id) {
              applyDateFormat(id);
            });
          }, 500);
          
          // Handler for programmatic updates
          Shiny.addCustomMessageHandler('updateSliderFormat', function(message) {
            setTimeout(function() {
              applyDateFormat(message.id);
            }, 100);
          });
        });
      "))
    ),
    
    tabItems(
      
      # =====================================================================
      # TAB 0: HOME
      # =====================================================================
      tabItem(
        tabName = "home",
        
        # Hero section with image background
        fluidRow(
          column(
            width = 12,
            
            tags$div(
              style = "position: relative; border-radius: 12px; overflow: hidden; margin-bottom: 30px; box-shadow: 0 8px 30px rgba(0,0,0,0.15);",
              
              # Background image with overlay
              tags$div(
                style = "
                  background-image: url('Old_World_estate_in_Southlake.jpg');
                  background-size: cover;
                  background-position: center top;
                  height: 400px;
                  position: relative;
                "
              ),
              
              # Dark overlay for text readability
              tags$div(
                style = "
                  position: absolute;
                  top: 0; left: 0; right: 0; bottom: 0;
                  background: linear-gradient(135deg, rgba(44, 62, 80, 0.85) 0%, rgba(52, 152, 219, 0.7) 100%);
                  display: flex;
                  flex-direction: column;
                  justify-content: center;
                  align-items: center;
                  padding: 40px;
                ",
                
                # Main title
                tags$h1(
                  style = "
                    color: white;
                    font-size: 3em;
                    font-weight: 700;
                    margin-bottom: 15px;
                    text-shadow: 2px 2px 8px rgba(0,0,0,0.3);
                    text-align: center;
                  ",
                  "Redfin Housing Market Explorer"
                ),
                
                # Subtitle
                tags$p(
                  style = "
                    color: #ECF0F1;
                    font-size: 1.3em;
                    max-width: 700px;
                    text-align: center;
                    margin-bottom: 25px;
                    text-shadow: 1px 1px 4px rgba(0,0,0,0.2);
                  ",
                  "An interactive dashboard for exploring U.S. housing market trends across major metropolitan areas"
                ),
                
                # CTA Button
                tags$a(
                  href    = "#",
                  onclick = "Shiny.setInputValue('go_overview', Math.random())",
                  style   = "
                    background-color: #E74C3C;
                    color: white;
                    padding: 12px 30px;
                    border-radius: 30px;
                    text-decoration: none;
                    font-weight: 600;
                    font-size: 1.1em;
                    transition: all 0.3s ease;
                    box-shadow: 0 4px 15px rgba(231, 76, 60, 0.4);
                  ",
                  class = "btn-explore",
                  icon("chart-line", style = "margin-right: 8px;"),
                  "Explore the Data"
                )
              )
            )
          )
        ),
        
        # Quick stats row
        fluidRow(
          column(
            width = 3,
            tags$div(
              style = "
                background: linear-gradient(135deg, #3498DB 0%, #2980B9 100%);
                color: white;
                padding: 25px;
                border-radius: 10px;
                text-align: center;
                box-shadow: 0 4px 15px rgba(52, 152, 219, 0.3);
              ",
              icon("globe-americas", style = "font-size: 2em; margin-bottom: 10px;"),
              tags$div(style = "font-size: 2em; font-weight: 700;", length(unique(redfin$region))),
              tags$div(style = "font-size: 0.9em; opacity: 0.9;", "Regions Covered")
            )
          ),
          column(
            width = 3,
            tags$div(
              style = "
                background: linear-gradient(135deg, #27AE60 0%, #1E8449 100%);
                color: white;
                padding: 25px;
                border-radius: 10px;
                text-align: center;
                box-shadow: 0 4px 15px rgba(39, 174, 96, 0.3);
              ",
              icon("database", style = "font-size: 2em; margin-bottom: 10px;"),
              tags$div(style = "font-size: 2em; font-weight: 700;", scales::comma(nrow(redfin))),
              tags$div(style = "font-size: 0.9em; opacity: 0.9;", "Data Points")
            )
          ),
          column(
            width = 3,
            tags$div(
              style = "
                background: linear-gradient(135deg, #9B59B6 0%, #7D3C98 100%);
                color: white;
                padding: 25px;
                border-radius: 10px;
                text-align: center;
                box-shadow: 0 4px 15px rgba(155, 89, 182, 0.3);
              ",
              icon("calendar-alt", style = "font-size: 2em; margin-bottom: 10px;"),
              tags$div(style = "font-size: 2em; font-weight: 700;", format(earliest_month, "%b %Y")),
              tags$div(style = "font-size: 0.9em; opacity: 0.9;", "Data Starts")
            )
          ),
          column(
            width = 3,
            tags$div(
              style = "
                background: linear-gradient(135deg, #E67E22 0%, #CA6F1E 100%);
                color: white;
                padding: 25px;
                border-radius: 10px;
                text-align: center;
                box-shadow: 0 4px 15px rgba(230, 126, 34, 0.3);
              ",
              icon("calendar-check", style = "font-size: 2em; margin-bottom: 10px;"),
              tags$div(style = "font-size: 2em; font-weight: 700;", format(latest_month, "%b %Y")),
              tags$div(style = "font-size: 0.9em; opacity: 0.9;", "Latest Data")
            )
          )
        ),
        
        # Spacer
        tags$div(style = "height: 30px;"),
        
        # About and Regions row
        fluidRow(
          # About the Data
          column(
            width = 6,
            box(
              title       = tagList(icon("info-circle"), " About This Dashboard"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$p(
                style = "font-size: 14px; line-height: 1.8; color: #2C3E50;",
                "This interactive dashboard provides comprehensive insights into U.S. housing market trends using data provided by ",
                tags$a(
                  href   = "https://www.redfin.com/news/data-center/",
                  target = "_blank",
                  style  = "color: #3498DB; font-weight: 600;",
                  "Redfin"
                ),
                ", a national real estate brokerage."
              ),
              
              tags$p(
                style = "font-size: 14px; line-height: 1.8; color: #2C3E50; margin-top: 15px;",
                "Explore trends in median sale prices, inventory levels, days on market, and more across major U.S. metropolitan areas. Compare regions, identify market regimes, and gain context for pricing decisions."
              ),
              
              tags$hr(style = "margin: 20px 0;"),
              
              tags$h5(
                style = "color: #2C3E50; font-weight: 600; margin-bottom: 12px;",
                icon("compass", style = "margin-right: 8px;"),
                "Dashboard Features"
              ),
              
              tags$ul(
                style = "list-style: none; padding: 0; margin: 0;",
                tags$li(
                  style = "padding: 8px 0; border-bottom: 1px solid #eee;",
                  icon("chart-line", style = "color: #3498DB; margin-right: 10px; width: 20px;"),
                  tags$strong("Market Overview"), " – High-level trends across regions"
                ),
                tags$li(
                  style = "padding: 8px 0; border-bottom: 1px solid #eee;",
                  icon("search-location", style = "color: #27AE60; margin-right: 10px; width: 20px;"),
                  tags$strong("Region Explorer"), " – Deep dive into individual markets"
                ),
                tags$li(
                  style = "padding: 8px 0; border-bottom: 1px solid #eee;",
                  icon("fire", style = "color: #E74C3C; margin-right: 10px; width: 20px;"),
                  tags$strong("Market Heat"), " – Buyer vs. seller market classification"
                ),
                tags$li(
                  style = "padding: 8px 0; border-bottom: 1px solid #eee;",
                  icon("camera", style = "color: #9B59B6; margin-right: 10px; width: 20px;"),
                  tags$strong("Cross-Section Snapshot"), " – Point-in-time comparisons"
                ),
                tags$li(
                  style = "padding: 8px 0;",
                  icon("dollar-sign", style = "color: #F39C12; margin-right: 10px; width: 20px;"),
                  tags$strong("Pricing Guidance"), " – Context-based pricing ranges"
                )
              )
            )
          ),
          
          # Regions and Metrics
          column(
            width = 6,
            
            # Regions box
            box(
              title       = tagList(icon("map-marker-alt"), " Regions Covered"),
              status      = "info",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                style = "display: flex; flex-wrap: wrap; gap: 10px;",
                lapply(levels(redfin$region), function(reg) {
                  color <- region_colors[[reg]]
                  if (is.null(color)) color <- "#666666"
                  tags$span(
                    style = paste0(
                      "background-color: ", color, "; color: white; ",
                      "padding: 8px 16px; border-radius: 25px; font-size: 13px; ",
                      "font-weight: 500; box-shadow: 0 2px 8px rgba(0,0,0,0.15);"
                    ),
                    reg
                  )
                })
              )
            ),
            
            # Metrics box
            box(
              title       = tagList(icon("chart-bar"), " Available Metrics"),
              status      = "success",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 10px;",
                
                tags$div(
                  style = "padding: 12px; background: linear-gradient(135deg, #f8f9fa 0%, #fff 100%); border-radius: 8px; border-left: 4px solid #3498DB;",
                  icon("dollar-sign", style = "color: #3498DB; margin-right: 8px;"),
                  "Median Sale Price"
                ),
                tags$div(
                  style = "padding: 12px; background: linear-gradient(135deg, #f8f9fa 0%, #fff 100%); border-radius: 8px; border-left: 4px solid #27AE60;",
                  icon("home", style = "color: #27AE60; margin-right: 8px;"),
                  "Homes Sold"
                ),
                tags$div(
                  style = "padding: 12px; background: linear-gradient(135deg, #f8f9fa 0%, #fff 100%); border-radius: 8px; border-left: 4px solid #E74C3C;",
                  icon("list", style = "color: #E74C3C; margin-right: 8px;"),
                  "New Listings"
                ),
                tags$div(
                  style = "padding: 12px; background: linear-gradient(135deg, #f8f9fa 0%, #fff 100%); border-radius: 8px; border-left: 4px solid #F39C12;",
                  icon("warehouse", style = "color: #F39C12; margin-right: 8px;"),
                  "Inventory"
                ),
                tags$div(
                  style = "padding: 12px; background: linear-gradient(135deg, #f8f9fa 0%, #fff 100%); border-radius: 8px; border-left: 4px solid #9B59B6;",
                  icon("clock", style = "color: #9B59B6; margin-right: 8px;"),
                  "Days on Market"
                ),
                tags$div(
                  style = "padding: 12px; background: linear-gradient(135deg, #f8f9fa 0%, #fff 100%); border-radius: 8px; border-left: 4px solid #1ABC9C;",
                  icon("balance-scale", style = "color: #1ABC9C; margin-right: 8px;"),
                  "Sale-to-List Ratio"
                )
              ),
              
              tags$p(
                style = "margin-top: 15px; font-size: 12px; color: #7F8C8D; font-style: italic;",
                icon("info-circle", style = "margin-right: 5px;"),
                "All metrics include month-over-month and year-over-year changes"
              )
            )
          )
        ),
        
        # Acknowledgments footer
        fluidRow(
          column(
            width = 12,
            tags$div(
              style = "
                text-align: center;
                padding: 25px;
                margin-top: 30px;
                background: #f8f9fa;
                border-radius: 10px;
                border: 1px solid #eee;
              ",
              tags$p(
                style = "margin-bottom: 8px; color: #7F8C8D; font-size: 12px;",
                "Data provided by ",
                tags$a(
                  href   = "https://www.redfin.com",
                  target = "_blank",
                  style  = "color: #E74C3C; font-weight: 500;",
                  "Redfin"
                ),
                ", a national real estate brokerage."
              ),
              tags$p(
                style = "margin: 0; color: #95A5A6; font-size: 11px;",
                "Dashboard developed with assistance from ",
                tags$span(style = "font-weight: 500;", "ChatGPT"),
                " and ",
                tags$span(style = "font-weight: 500;", "Claude"),
                " AI assistants."
              )
            )
          )
        )
      ),
      
      # =====================================================================
      # TAB 1: OVERVIEW
      # =====================================================================
      tabItem(
        tabName = "overview",
        
        # Page title
        fluidRow(
          column(
            width = 12,
            tags$h2(
              style = "margin-bottom: 5px; color: #2C3E50; font-weight: 600;",
              icon("chart-line", style = "margin-right: 10px;"),
              "Market Overview"
            ),
            tags$p(
              style = "color: #7F8C8D; margin-bottom: 20px;",
              "High-level summary of housing market conditions across regions and time"
            )
          )
        ),
        
        fluidRow(
          # -------------------------------------------------------------------
          # Filters Panel (Left)
          # -------------------------------------------------------------------
          column(
            width = 3,
            
            box(
              title       = tagList(icon("filter"), " Filters"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              collapsible = TRUE,
              
              # Region selector
              pickerInput(
                inputId  = "ov_regions",
                label    = tags$span(
                  style = "font-weight: 500;",
                  "Select Regions"
                ),
                choices  = region_choices,
                selected = "National",
                multiple = TRUE,
                options  = list(
                  `actions-box`       = TRUE,
                  `live-search`       = TRUE,
                  `selected-text-format` = "count > 2",
                  `count-selected-text`  = "{0} regions selected",
                  style               = "btn-outline-primary"
                )
              ),
              
              tags$hr(style = "margin: 15px 0;"),
              
              # Date range slider
              sliderInput(
                inputId    = "ov_date_range",
                label      = tags$span(
                  style = "font-weight: 500;",
                  "Date Range"
                ),
                min        = earliest_month,
                max        = latest_month,
                value      = c(earliest_month, latest_month),
                timeFormat = "%b %Y",
                width      = "100%"
              ),
              
              tags$hr(style = "margin: 15px 0;"),
              
              # Quick date range buttons
              tags$label(
                style = "font-weight: 500; font-size: 13px;",
                "Quick Select"
              ),
              tags$div(
                style = "margin-top: 8px;",
                actionButton(
                  "ov_last_year",
                  "Last Year",
                  class = "btn-sm btn-outline-secondary",
                  style = "margin-right: 5px; margin-bottom: 5px;"
                ),
                actionButton(
                  "ov_last_3years",
                  "Last 3 Years",
                  class = "btn-sm btn-outline-secondary",
                  style = "margin-right: 5px; margin-bottom: 5px;"
                ),
                actionButton(
                  "ov_all_time",
                  "All Time",
                  class = "btn-sm btn-outline-secondary",
                  style = "margin-bottom: 5px;"
                )
              )
            ),
            
            # Legend box
            box(
              title       = tagList(icon("palette"), " Legend"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              collapsible = TRUE,
              
              uiOutput("ov_legend_panel")
            ),
            
            # Key metrics box
            box(
              title       = tagList(icon("tachometer-alt"), " Key Metrics"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              uiOutput("ov_metrics_panel")
            )
          ),
          
          # -------------------------------------------------------------------
          # Main Content (Right)
          # -------------------------------------------------------------------
          column(
            width = 9,
            
            # Main price chart (interactive with plotly)
            box(
              title       = tagList(icon("dollar-sign"), " Median Sale Price"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              plotlyOutput("ov_price_plot", height = "350px"),
              
              tags$div(
                style = "text-align: right; font-size: 11px; color: #95A5A6; margin-top: 5px;",
                "Source: Redfin Monthly Housing Data"
              )
            ),
            
            # Secondary metrics row
            fluidRow(
              column(
                width = 4,
                box(
                  title       = tagList(icon("warehouse"), " Inventory"),
                  status      = "info",
                  solidHeader = FALSE,
                  width       = NULL,
                  plotOutput("ov_inventory_plot", height = "200px")
                )
              ),
              column(
                width = 4,
                box(
                  title       = tagList(icon("clock"), " Days on Market"),
                  status      = "info",
                  solidHeader = FALSE,
                  width       = NULL,
                  plotOutput("ov_dom_plot", height = "200px")
                )
              ),
              column(
                width = 4,
                box(
                  title       = tagList(icon("balance-scale"), " Sale-to-List Ratio"),
                  status      = "info",
                  solidHeader = FALSE,
                  width       = NULL,
                  plotOutput("ov_saletolist_plot", height = "200px")
                )
              )
            ),
            
            # Summary box
            box(
              title       = tagList(icon("lightbulb"), " Market Summary"),
              status      = "success",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                class = "summary-text",
                textOutput("ov_summary")
              )
            )
          )
        )
      ),
      
      # =====================================================================
      # TAB 2: REGION EXPLORER
      # =====================================================================
      tabItem(
        tabName = "region_explorer",
        
        # Page title
        fluidRow(
          column(
            width = 12,
            tags$h2(
              style = "margin-bottom: 5px; color: #2C3E50; font-weight: 600;",
              icon("search-location", style = "margin-right: 10px;"),
              "Region Explorer"
            ),
            tags$p(
              style = "color: #7F8C8D; margin-bottom: 20px;",
              "Deep dive into individual regions across all available metrics"
            )
          )
        ),
        
        fluidRow(
          # -------------------------------------------------------------------
          # Filters Panel (Left)
          # -------------------------------------------------------------------
          column(
            width = 3,
            
            # Region selector
            box(
              title       = tagList(icon("filter"), " Filters"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              # Primary region selector
              pickerInput(
                inputId  = "re_regions",
                label    = tags$span(
                  style = "font-weight: 500;",
                  "Select Regions (max 3)"
                ),
                choices  = setdiff(region_choices, "(All)"),
                selected = "National",
                multiple = TRUE,
                options  = list(
                  `max-options`       = 3,
                  `live-search`       = TRUE,
                  `selected-text-format` = "count > 2",
                  style               = "btn-outline-primary"
                )
              ),
              
              tags$hr(style = "margin: 15px 0;"),
              
              # Metric selector
              selectInput(
                inputId  = "re_metric",
                label    = tags$span(
                  style = "font-weight: 500;",
                  "Primary Metric"
                ),
                choices  = list(
                  "Prices" = c(
                    "Median Sale Price"     = "median_sale_price",
                    "Price MoM Change (%)"  = "median_sale_price_mom",
                    "Price YoY Change (%)"  = "median_sale_price_yoy"
                  ),
                  "Volume" = c(
                    "Homes Sold"            = "homes_sold",
                    "Homes Sold MoM (%)"    = "homes_sold_mom",
                    "Homes Sold YoY (%)"    = "homes_sold_yoy"
                  ),
                  "Supply" = c(
                    "New Listings"          = "new_listings",
                    "New Listings MoM (%)"  = "new_listings_mom",
                    "New Listings YoY (%)"  = "new_listings_yoy",
                    "Inventory"             = "inventory",
                    "Inventory MoM (%)"     = "inventory_mom",
                    "Inventory YoY (%)"     = "inventory_yoy"
                  ),
                  "Liquidity" = c(
                    "Days on Market"        = "days_on_market",
                    "DOM MoM Change"        = "days_on_market_mom",
                    "DOM YoY Change"        = "days_on_market_yoy",
                    "Sale-to-List Ratio"    = "average_sale_to_list",
                    "Sale-to-List MoM"      = "average_sale_to_list_mom",
                    "Sale-to-List YoY"      = "average_sale_to_list_yoy"
                  )
                ),
                selected = "median_sale_price"
              ),
              
              tags$hr(style = "margin: 15px 0;"),
              
              # Date range slider
              sliderInput(
                inputId    = "re_date_range",
                label      = tags$span(
                  style = "font-weight: 500;",
                  "Date Range"
                ),
                min        = earliest_month,
                max        = latest_month,
                value      = c(earliest_month, latest_month),
                timeFormat = "%b %Y",
                width      = "100%"
              ),
              
              tags$hr(style = "margin: 15px 0;"),
              
              # Quick date range buttons
              tags$label(
                style = "font-weight: 500; font-size: 13px;",
                "Quick Select"
              ),
              tags$div(
                style = "margin-top: 8px;",
                actionButton(
                  "re_last_year",
                  "1Y",
                  class = "btn-sm btn-outline-secondary",
                  style = "margin-right: 5px; margin-bottom: 5px;"
                ),
                actionButton(
                  "re_last_3years",
                  "3Y",
                  class = "btn-sm btn-outline-secondary",
                  style = "margin-right: 5px; margin-bottom: 5px;"
                ),
                actionButton(
                  "re_last_5years",
                  "5Y",
                  class = "btn-sm btn-outline-secondary",
                  style = "margin-right: 5px; margin-bottom: 5px;"
                ),
                actionButton(
                  "re_all_time",
                  "All",
                  class = "btn-sm btn-outline-secondary",
                  style = "margin-bottom: 5px;"
                )
              )
            ),
            
            # Legend box
            box(
              title       = tagList(icon("palette"), " Legend"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              collapsible = TRUE,
              
              uiOutput("re_legend_panel")
            )
          ),
          
          # -------------------------------------------------------------------
          # Main Content (Right)
          # -------------------------------------------------------------------
          column(
            width = 9,
            
            # Summary metric cards
            fluidRow(
              column(width = 3, uiOutput("re_card_price")),
              column(width = 3, uiOutput("re_card_dom")),
              column(width = 3, uiOutput("re_card_stl")),
              column(width = 3, uiOutput("re_card_volume"))
            ),
            
            # Spacer
            tags$div(style = "height: 20px;"),
            
            # Main metric chart (interactive)
            box(
              title       = uiOutput("re_main_chart_title"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              plotlyOutput("re_main_plot", height = "350px")
            ),
            
            # Secondary aligned plots
            fluidRow(
              column(
                width = 4,
                box(
                  title       = tagList(icon("dollar-sign"), " Median Sale Price"),
                  status      = "info",
                  solidHeader = FALSE,
                  width       = NULL,
                  plotOutput("re_price_plot", height = "180px")
                )
              ),
              column(
                width = 4,
                box(
                  title       = tagList(icon("clock"), " Days on Market"),
                  status      = "info",
                  solidHeader = FALSE,
                  width       = NULL,
                  plotOutput("re_dom_plot", height = "180px")
                )
              ),
              column(
                width = 4,
                box(
                  title       = tagList(icon("warehouse"), " Inventory"),
                  status      = "info",
                  solidHeader = FALSE,
                  width       = NULL,
                  plotOutput("re_inventory_plot", height = "180px")
                )
              )
            ),
            
            # Summary box
            box(
              title       = tagList(icon("lightbulb"), " Region Summary"),
              status      = "success",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                class = "summary-text",
                uiOutput("re_summary")
              )
            )
          )
        )
      ),
      
      # =====================================================================
      # TAB 3: MARKET HEAT
      # =====================================================================
      tabItem(
        tabName = "market_heat",
        
        # Page title
        fluidRow(
          column(
            width = 12,
            tags$h2(
              style = "margin-bottom: 5px; color: #2C3E50; font-weight: 600;",
              icon("fire", style = "margin-right: 10px;"),
              "Market Heat / Regimes"
            ),
            tags$p(
              style = "color: #7F8C8D; margin-bottom: 20px;",
              "Classify markets into buyer, neutral, or seller regimes over time"
            )
          )
        ),
        
        fluidRow(
          # -------------------------------------------------------------------
          # Filters Panel (Left)
          # -------------------------------------------------------------------
          column(
            width = 3,
            
            box(
              title       = tagList(icon("filter"), " Filters"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              # View type selector
              radioButtons(
                inputId  = "mh_view_type",
                label    = tags$span(style = "font-weight: 500;", "View Type"),
                choices  = c(
                  "Market Regime (Categorical)" = "regime",
                  "Heat Index (Continuous)"     = "heat_index"
                ),
                selected = "regime"
              ),
              
              tags$hr(style = "margin: 15px 0;"),
              
              # Region filter
              pickerInput(
                inputId  = "mh_regions",
                label    = tags$span(style = "font-weight: 500;", "Select Regions"),
                choices  = setdiff(region_choices, "(All)"),
                selected = setdiff(region_choices, "(All)"),
                multiple = TRUE,
                options  = list(
                  `actions-box` = TRUE,
                  `live-search` = TRUE,
                  `selected-text-format` = "count > 3",
                  `count-selected-text`  = "{0} regions selected"
                )
              ),
              
              tags$hr(style = "margin: 15px 0;"),
              
              # Date range slider
              sliderInput(
                inputId    = "mh_date_range",
                label      = tags$span(style = "font-weight: 500;", "Date Range"),
                min        = earliest_month,
                max        = latest_month,
                value      = c(
                  as.Date(latest_month) - years(5),
                  as.Date(latest_month)
                ),
                timeFormat = "%b %Y",
                width      = "100%"
              ),
              
              tags$hr(style = "margin: 15px 0;"),
              
              # Quick select buttons
              tags$label(style = "font-weight: 500; font-size: 13px;", "Quick Select"),
              tags$div(
                style = "margin-top: 8px;",
                actionButton("mh_last_3years", "3Y", class = "btn-sm btn-outline-secondary", style = "margin-right: 5px;"),
                actionButton("mh_last_5years", "5Y", class = "btn-sm btn-outline-secondary", style = "margin-right: 5px;"),
                actionButton("mh_all_time", "All", class = "btn-sm btn-outline-secondary")
              )
            ),
            
            # Regime legend box
            box(
              title       = tagList(icon("info-circle"), " Regime Definitions"),
              status      = "info",
              solidHeader = TRUE,
              width       = NULL,
              collapsible = TRUE,
              collapsed   = TRUE,
              
              tags$div(
                style = "font-size: 12px; line-height: 1.6;",
                
                tags$div(
                  style = "margin-bottom: 12px; padding: 10px; background: #ffebee; border-radius: 6px; border-left: 4px solid #E74C3C;",
                  tags$strong(style = "color: #E74C3C;", icon("fire"), " Seller's Market"),
                  tags$p(style = "margin: 5px 0 0 0; color: #666;",
                         "Sale-to-list ≥ 100% AND days on market in bottom third for that region"
                  )
                ),
                
                tags$div(
                  style = "margin-bottom: 12px; padding: 10px; background: #fff8e1; border-radius: 6px; border-left: 4px solid #F39C12;",
                  tags$strong(style = "color: #F39C12;", icon("balance-scale"), " Neutral Market"),
                  tags$p(style = "margin: 5px 0 0 0; color: #666;",
                         "Conditions between seller's and buyer's markets"
                  )
                ),
                
                tags$div(
                  style = "padding: 10px; background: #e3f2fd; border-radius: 6px; border-left: 4px solid #3498DB;",
                  tags$strong(style = "color: #3498DB;", icon("snowflake"), " Buyer's Market"),
                  tags$p(style = "margin: 5px 0 0 0; color: #666;",
                         "Sale-to-list < 99% OR days on market in top third for that region"
                  )
                )
              )
            )
          ),
          
          # -------------------------------------------------------------------
          # Main Content (Right)
          # -------------------------------------------------------------------
          column(
            width = 9,
            
            # Main heatmap
            box(
              title       = tagList(icon("th"), " Market Conditions Heatmap"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              plotlyOutput("mh_heatmap", height = "400px"),
              
              tags$div(
                style = "margin-top: 10px; display: flex; justify-content: center; gap: 20px;",
                conditionalPanel(
                  condition = "input.mh_view_type == 'regime'",
                  tags$div(
                    style = "display: flex; align-items: center; gap: 20px;",
                    tags$span(
                      style = "display: flex; align-items: center;",
                      tags$span(style = "width: 20px; height: 20px; background: #E74C3C; border-radius: 4px; margin-right: 6px;"),
                      "Seller's Market"
                    ),
                    tags$span(
                      style = "display: flex; align-items: center;",
                      tags$span(style = "width: 20px; height: 20px; background: #F39C12; border-radius: 4px; margin-right: 6px;"),
                      "Neutral"
                    ),
                    tags$span(
                      style = "display: flex; align-items: center;",
                      tags$span(style = "width: 20px; height: 20px; background: #3498DB; border-radius: 4px; margin-right: 6px;"),
                      "Buyer's Market"
                    )
                  )
                )
              )
            ),
            
            # Regime distribution
            fluidRow(
              column(
                width = 6,
                box(
                  title       = tagList(icon("chart-pie"), " Regime Distribution by Region"),
                  status      = "info",
                  solidHeader = FALSE,
                  width       = NULL,
                  
                  plotOutput("mh_regime_bars", height = "280px")
                )
              ),
              column(
                width = 6,
                box(
                  title       = tagList(icon("chart-line"), " Heat Index Over Time"),
                  status      = "info",
                  solidHeader = FALSE,
                  width       = NULL,
                  
                  # Explanation of heat index
                  tags$div(
                    style = "background: #f8f9fa; border-radius: 6px; padding: 10px; margin-bottom: 10px; font-size: 11px; color: #555;",
                    tags$strong("What is the Heat Index? "),
                    "A composite score (0-100) measuring market intensity. Higher values indicate seller-favorable conditions ",
                    "(fast sales, high sale-to-list ratios, rising prices, low inventory). ",
                    "Lower values indicate buyer-favorable conditions."
                  ),
                  
                  plotOutput("mh_heat_trend", height = "240px"),
                  
                  # Legend for the background bands
                  tags$div(
                    style = "display: flex; justify-content: center; gap: 15px; margin-top: 8px; font-size: 11px;",
                    tags$span(
                      style = "display: flex; align-items: center;",
                      tags$span(style = "width: 12px; height: 12px; background: rgba(52, 152, 219, 0.3); border-radius: 2px; margin-right: 4px;"),
                      "Buyer-Friendly (0-33)"
                    ),
                    tags$span(
                      style = "display: flex; align-items: center;",
                      tags$span(style = "width: 12px; height: 12px; background: rgba(243, 156, 18, 0.3); border-radius: 2px; margin-right: 4px;"),
                      "Balanced (33-67)"
                    ),
                    tags$span(
                      style = "display: flex; align-items: center;",
                      tags$span(style = "width: 12px; height: 12px; background: rgba(231, 76, 60, 0.3); border-radius: 2px; margin-right: 4px;"),
                      "Seller-Friendly (67-100)"
                    )
                  )
                )
              )
            ),
            
            # Heat Index Components Explanation
            box(
              title       = tagList(icon("info-circle"), " Understanding the Heat Index"),
              status      = "info",
              solidHeader = FALSE,
              width       = NULL,
              collapsible = TRUE,
              collapsed   = TRUE,
              
              fluidRow(
                column(
                  width = 3,
                  tags$div(
                    style = "text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;",
                    tags$div(style = "font-size: 24px; color: #3498DB; margin-bottom: 5px;", icon("clock")),
                    tags$div(style = "font-weight: 600; color: #2C3E50; margin-bottom: 5px;", "Days on Market"),
                    tags$div(style = "font-size: 20px; font-weight: 700; color: #3498DB;", "30%"),
                    tags$div(style = "font-size: 11px; color: #7F8C8D; margin-top: 5px;", "Faster sales = hotter market")
                  )
                ),
                column(
                  width = 3,
                  tags$div(
                    style = "text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;",
                    tags$div(style = "font-size: 24px; color: #E74C3C; margin-bottom: 5px;", icon("percentage")),
                    tags$div(style = "font-weight: 600; color: #2C3E50; margin-bottom: 5px;", "Sale-to-List Ratio"),
                    tags$div(style = "font-size: 20px; font-weight: 700; color: #E74C3C;", "30%"),
                    tags$div(style = "font-size: 11px; color: #7F8C8D; margin-top: 5px;", "Above 100% = bidding wars")
                  )
                ),
                column(
                  width = 3,
                  tags$div(
                    style = "text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;",
                    tags$div(style = "font-size: 24px; color: #F39C12; margin-bottom: 5px;", icon("warehouse")),
                    tags$div(style = "font-weight: 600; color: #2C3E50; margin-bottom: 5px;", "Inventory Level"),
                    tags$div(style = "font-size: 20px; font-weight: 700; color: #F39C12;", "20%"),
                    tags$div(style = "font-size: 11px; color: #7F8C8D; margin-top: 5px;", "Low inventory = hotter")
                  )
                ),
                column(
                  width = 3,
                  tags$div(
                    style = "text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;",
                    tags$div(style = "font-size: 24px; color: #27AE60; margin-bottom: 5px;", icon("chart-line")),
                    tags$div(style = "font-weight: 600; color: #2C3E50; margin-bottom: 5px;", "Price Growth (YoY)"),
                    tags$div(style = "font-size: 20px; font-weight: 700; color: #27AE60;", "20%"),
                    tags$div(style = "font-size: 11px; color: #7F8C8D; margin-top: 5px;", "Rising prices = hotter")
                  )
                )
              ),
              
              tags$div(
                style = "margin-top: 15px; padding: 12px; background: #e8f4fd; border-radius: 6px; font-size: 12px; color: #2C3E50;",
                icon("lightbulb", style = "color: #3498DB; margin-right: 8px;"),
                "The Heat Index is calculated by ranking each month within its region's historical distribution for each component, ",
                "then combining them using the weights shown above. A score of 50 represents typical conditions for that region."
              )
            ),
            
            # Summary
            box(
              title       = tagList(icon("lightbulb"), " Market Heat Summary"),
              status      = "success",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                class = "summary-text",
                uiOutput("mh_summary")
              )
            )
          )
        )
      ),
      
      # =====================================================================
      # TAB 4: CROSS-SECTION SNAPSHOT
      # =====================================================================
      tabItem(
        tabName = "snapshot",
        
        # Page title
        fluidRow(
          column(
            width = 12,
            tags$h2(
              style = "margin-bottom: 5px; color: #2C3E50; font-weight: 600;",
              icon("camera", style = "margin-right: 10px;"),
              "Cross-Section Snapshot"
            ),
            tags$p(
              style = "color: #7F8C8D; margin-bottom: 20px;",
              "Compare all regions at a single point in time"
            )
          )
        ),
        
        fluidRow(
          # -------------------------------------------------------------------
          # Filters Panel (Left)
          # -------------------------------------------------------------------
          column(
            width = 3,
            
            box(
              title       = tagList(icon("filter"), " Filters"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              # Month selector
              sliderInput(
                inputId    = "ss_month",
                label      = tags$span(style = "font-weight: 500;", "Select Month"),
                min        = earliest_month,
                max        = latest_month,
                value      = latest_month,
                timeFormat = "%b %Y",
                width      = "100%"
              ),
              
              tags$hr(style = "margin: 15px 0;"),
              
              # Quick month buttons
              tags$label(style = "font-weight: 500; font-size: 13px;", "Quick Select"),
              tags$div(
                style = "margin-top: 8px;",
                actionButton("ss_latest", "Latest", class = "btn-sm btn-outline-secondary", style = "margin-right: 5px; margin-bottom: 5px;"),
                actionButton("ss_year_ago", "1 Year Ago", class = "btn-sm btn-outline-secondary", style = "margin-right: 5px; margin-bottom: 5px;"),
                actionButton("ss_3years_ago", "3 Years Ago", class = "btn-sm btn-outline-secondary", style = "margin-bottom: 5px;")
              ),
              
              tags$hr(style = "margin: 15px 0;"),
              
              # Region filter
              pickerInput(
                inputId  = "ss_regions",
                label    = tags$span(style = "font-weight: 500;", "Select Regions"),
                choices  = setdiff(region_choices, "(All)"),
                selected = setdiff(region_choices, "(All)"),
                multiple = TRUE,
                options  = list(
                  `actions-box` = TRUE,
                  `live-search` = TRUE,
                  `selected-text-format` = "count > 3",
                  `count-selected-text`  = "{0} regions selected"
                )
              )
            ),
            
            # Selected month info
            box(
              title       = tagList(icon("calendar-alt"), " Selected Period"),
              status      = "info",
              solidHeader = TRUE,
              width       = NULL,
              
              uiOutput("ss_period_info")
            )
          ),
          
          # -------------------------------------------------------------------
          # Main Content (Right)
          # -------------------------------------------------------------------
          column(
            width = 9,
            
            # Bar charts row 1
            fluidRow(
              column(
                width = 6,
                box(
                  title       = tagList(icon("dollar-sign"), " Median Sale Price"),
                  status      = "primary",
                  solidHeader = FALSE,
                  width       = NULL,
                  plotlyOutput("ss_price_bars", height = "280px")
                )
              ),
              column(
                width = 6,
                box(
                  title       = tagList(icon("clock"), " Days on Market"),
                  status      = "primary",
                  solidHeader = FALSE,
                  width       = NULL,
                  plotlyOutput("ss_dom_bars", height = "280px")
                )
              )
            ),
            
            # Bar charts row 2
            fluidRow(
              column(
                width = 6,
                box(
                  title       = tagList(icon("warehouse"), " Inventory"),
                  status      = "info",
                  solidHeader = FALSE,
                  width       = NULL,
                  plotlyOutput("ss_inventory_bars", height = "280px")
                )
              ),
              column(
                width = 6,
                box(
                  title       = tagList(icon("balance-scale"), " Sale-to-List Ratio"),
                  status      = "info",
                  solidHeader = FALSE,
                  width       = NULL,
                  plotlyOutput("ss_stl_bars", height = "280px")
                )
              )
            ),
            
            # Data table
            box(
              title       = tagList(icon("table"), " Snapshot Data Table"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              collapsible = TRUE,
              
              div(style = "overflow-x: auto;", tableOutput("ss_table"))
            ),
            
            # Summary
            box(
              title       = tagList(icon("lightbulb"), " Snapshot Summary"),
              status      = "success",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                class = "summary-text",
                uiOutput("ss_summary")
              )
            )
          )
        )
      ),
      
      # =====================================================================
      # TAB 5: PRICING GUIDANCE
      # =====================================================================
      tabItem(
        tabName = "pricing",
        
        # Page title
        fluidRow(
          column(
            width = 12,
            tags$h2(
              style = "margin-bottom: 5px; color: #2C3E50; font-weight: 600;",
              icon("dollar-sign", style = "margin-right: 10px;"),
              "Pricing Guidance"
            ),
            tags$p(
              style = "color: #7F8C8D; margin-bottom: 20px;",
              "Context-based pricing ranges using market conditions"
            )
          )
        ),
        
        fluidRow(
          # -------------------------------------------------------------------
          # Filters Panel (Left)
          # -------------------------------------------------------------------
          column(
            width = 3,
            
            box(
              title       = tagList(icon("filter"), " Select Market"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              # Region selector
              selectInput(
                inputId  = "pg_region",
                label    = tags$span(style = "font-weight: 500;", "Region"),
                choices  = setdiff(region_choices, "(All)"),
                selected = "National"
              ),
              
              tags$hr(style = "margin: 15px 0;"),
              
              # Month selector
              sliderInput(
                inputId    = "pg_month",
                label      = tags$span(style = "font-weight: 500;", "Month"),
                min        = earliest_month,
                max        = latest_month,
                value      = latest_month,
                timeFormat = "%b %Y",
                width      = "100%"
              ),
              
              tags$hr(style = "margin: 15px 0;"),
              
              # Quick month buttons
              tags$label(style = "font-weight: 500; font-size: 13px;", "Quick Select"),
              tags$div(
                style = "margin-top: 8px;",
                actionButton("pg_latest", "Latest", class = "btn-sm btn-outline-secondary", style = "margin-right: 5px;"),
                actionButton("pg_year_ago", "1 Year Ago", class = "btn-sm btn-outline-secondary")
              )
            ),
            
            # Methodology info
            box(
              title       = tagList(icon("info-circle"), " Methodology"),
              status      = "info",
              solidHeader = TRUE,
              width       = NULL,
              collapsible = TRUE,
              collapsed   = TRUE,
              
              tags$div(
                style = "font-size: 12px; line-height: 1.6; color: #555;",
                
                tags$p(
                  tags$strong("This is not an appraisal."),
                  " The pricing context range is a heuristic based on:"
                ),
                
                tags$ul(
                  style = "padding-left: 20px; margin: 10px 0;",
                  tags$li("Median Sale Price as the baseline"),
                  tags$li("Sale-to-List Ratio (market competition)"),
                  tags$li("Days on Market (demand speed)"),
                  tags$li("Historical percentiles for the region")
                ),
                
                tags$hr(style = "margin: 12px 0;"),
                
                tags$p(tags$strong("Market Classification:")),
                
                tags$div(
                  style = "margin: 8px 0; padding: 8px; background: #ffebee; border-radius: 4px;",
                  tags$strong(style = "color: #E74C3C;", "Hot: "),
                  "Range 100% – 105% of median"
                ),
                tags$div(
                  style = "margin: 8px 0; padding: 8px; background: #fff8e1; border-radius: 4px;",
                  tags$strong(style = "color: #F39C12;", "Neutral: "),
                  "Range 98% – 102% of median"
                ),
                tags$div(
                  style = "margin: 8px 0; padding: 8px; background: #e3f2fd; border-radius: 4px;",
                  tags$strong(style = "color: #3498DB;", "Cool: "),
                  "Range 95% – 100% of median"
                )
              )
            )
          ),
          
          # -------------------------------------------------------------------
          # Main Content (Right)
          # -------------------------------------------------------------------
          column(
            width = 9,
            
            # Price band card (main output)
            fluidRow(
              column(
                width = 12,
                uiOutput("pg_price_card")
              )
            ),
            
            # Market indicators row
            fluidRow(
              column(width = 3, uiOutput("pg_indicator_price")),
              column(width = 3, uiOutput("pg_indicator_dom")),
              column(width = 3, uiOutput("pg_indicator_stl")),
              column(width = 3, uiOutput("pg_indicator_inventory"))
            ),
            
            # Spacer
            tags$div(style = "height: 20px;"),
            
            # Context chart
            box(
              title       = tagList(icon("chart-area"), " Price History & Context"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              plotlyOutput("pg_context_chart", height = "350px")
            ),
            
            # Interpretation
            box(
              title       = tagList(icon("lightbulb"), " Interpretation"),
              status      = "success",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                class = "summary-text",
                uiOutput("pg_interpretation")
              )
            )
          )
        )
      ),
      
      # =====================================================================
      # TAB 6: METHODS & LIMITATIONS
      # =====================================================================
      tabItem(
        tabName = "methods",
        
        # Page title
        fluidRow(
          column(
            width = 12,
            tags$h2(
              style = "margin-bottom: 5px; color: #2C3E50; font-weight: 600;",
              icon("book", style = "margin-right: 10px;"),
              "Methods & Limitations"
            ),
            tags$p(
              style = "color: #7F8C8D; margin-bottom: 20px;",
              "Documentation of data sources, transformations, and analytical approaches"
            )
          )
        ),
        
        fluidRow(
          # Left column
          column(
            width = 6,
            
            # 1. Data Source and Coverage
            box(
              title       = tagList(icon("database"), " Data Source & Coverage"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                style = "line-height: 1.7;",
                
                tags$h4(style = "color: #2C3E50; margin-top: 0;", "Source"),
                tags$p(
                  "This dashboard uses ",
                  tags$a(href = "https://www.redfin.com/news/data-center/", target = "_blank", "Redfin Monthly Housing Market Data"),
                  ", a publicly available dataset compiled by Redfin, a national real estate brokerage."
                ),
                
                tags$h4(style = "color: #2C3E50;", "Geographic Coverage"),
                tags$p("The dataset includes the following regions:"),
                tags$ul(
                  style = "padding-left: 20px;",
                  tags$li(tags$strong("National"), " — Aggregate U.S. housing market"),
                  tags$li(tags$strong("Boston, MA"), " metro area"),
                  tags$li(tags$strong("Chicago, IL"), " metro area"),
                  tags$li(tags$strong("Los Angeles, CA"), " metro area"),
                  tags$li(tags$strong("Philadelphia, PA"), " metro area"),
                  tags$li(tags$strong("Seattle, WA"), " metro area"),
                  tags$li(tags$strong("Washington, DC"), " metro area")
                ),
                
                tags$h4(style = "color: #2C3E50;", "Time Coverage"),
                tags$p(
                  "The data spans from ",
                  tags$strong(format(earliest_month, "%B %Y")),
                  " to ",
                  tags$strong(format(latest_month, "%B %Y")),
                  "."
                ),
                
                tags$h4(style = "color: #2C3E50;", "Unit of Observation"),
                tags$p(
                  "Each row represents a ",
                  tags$strong("Region–Month aggregate"),
                  ", not individual properties. All metrics are aggregated statistics for all transactions ",
                  "in that region during that month."
                )
              )
            ),
            
            # 2. Key Variables
            box(
              title       = tagList(icon("list"), " Key Variables Used"),
              status      = "info",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                style = "line-height: 1.7;",
                
                tags$table(
                  style = "width: 100%; border-collapse: collapse;",
                  tags$thead(
                    tags$tr(
                      style = "background: #f8f9fa; border-bottom: 2px solid #ddd;",
                      tags$th(style = "padding: 10px; text-align: left;", "Variable"),
                      tags$th(style = "padding: 10px; text-align: left;", "Definition")
                    )
                  ),
                  tags$tbody(
                    tags$tr(
                      style = "border-bottom: 1px solid #eee;",
                      tags$td(style = "padding: 10px; font-weight: 600;", "Median Sale Price"),
                      tags$td(style = "padding: 10px;", "The median final sale price of homes sold in the region during the month.")
                    ),
                    tags$tr(
                      style = "border-bottom: 1px solid #eee;",
                      tags$td(style = "padding: 10px; font-weight: 600;", "Homes Sold"),
                      tags$td(style = "padding: 10px;", "Total number of home sales that closed during the month.")
                    ),
                    tags$tr(
                      style = "border-bottom: 1px solid #eee;",
                      tags$td(style = "padding: 10px; font-weight: 600;", "New Listings"),
                      tags$td(style = "padding: 10px;", "Number of new properties listed for sale during the month.")
                    ),
                    tags$tr(
                      style = "border-bottom: 1px solid #eee;",
                      tags$td(style = "padding: 10px; font-weight: 600;", "Inventory"),
                      tags$td(style = "padding: 10px;", "Total active listings available at month end.")
                    ),
                    tags$tr(
                      style = "border-bottom: 1px solid #eee;",
                      tags$td(style = "padding: 10px; font-weight: 600;", "Days on Market"),
                      tags$td(style = "padding: 10px;", "Median number of days from listing to contract for homes sold.")
                    ),
                    tags$tr(
                      style = "border-bottom: 1px solid #eee;",
                      tags$td(style = "padding: 10px; font-weight: 600;", "Avg. Sale-to-List"),
                      tags$td(style = "padding: 10px;", "Average ratio of final sale price to listing price (e.g., 100% = sold at asking, >100% = sold above asking).")
                    ),
                    tags$tr(
                      tags$td(style = "padding: 10px; font-weight: 600;", "MoM / YoY Changes"),
                      tags$td(style = "padding: 10px;", "Month-over-month and year-over-year percentage changes. These are pre-computed fields provided by Redfin.")
                    )
                  )
                )
              )
            ),
            
            # 3. Derived Indicators
            box(
              title       = tagList(icon("calculator"), " Derived Indicators"),
              status      = "info",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                style = "line-height: 1.7;",
                
                tags$p("The following metrics are calculated within this dashboard:"),
                
                tags$div(
                  style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  tags$h5(style = "margin-top: 0; color: #2C3E50;", "Tightness Ratio"),
                  tags$p(style = "margin-bottom: 5px;", tags$code("Homes Sold ÷ New Listings")),
                  tags$p(style = "font-size: 13px; color: #666; margin-bottom: 0;",
                         "Values > 1 indicate demand exceeds new supply; values < 1 indicate supply exceeds immediate demand."
                  )
                ),
                
                tags$div(
                  style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  tags$h5(style = "margin-top: 0; color: #2C3E50;", "Approximate Months' Supply"),
                  tags$p(style = "margin-bottom: 5px;", tags$code("Inventory ÷ Homes Sold")),
                  tags$p(style = "font-size: 13px; color: #666; margin-bottom: 0;",
                         "Estimates how many months it would take to sell all current inventory at the current sales pace. ",
                         "Lower values indicate tighter markets."
                  )
                ),
                
                tags$div(
                  style = "background: #fff3e0; padding: 15px; border-radius: 8px; border-left: 4px solid #F39C12;",
                  tags$h5(style = "margin-top: 0; color: #2C3E50;", "Heat Index (0–100)"),
                  tags$p(style = "margin-bottom: 10px;",
                         "A composite score measuring overall market intensity. Calculated by combining four components, ",
                         "each expressed as a percentile within that region's historical distribution:"
                  ),
                  tags$ul(
                    style = "padding-left: 20px; margin-bottom: 10px;",
                    tags$li(tags$strong("Days on Market (30%)"), " — Lower DOM → higher score"),
                    tags$li(tags$strong("Sale-to-List Ratio (30%)"), " — Higher ratio → higher score"),
                    tags$li(tags$strong("Inventory (20%)"), " — Lower inventory → higher score"),
                    tags$li(tags$strong("Price YoY Change (20%)"), " — Higher growth → higher score")
                  ),
                  tags$p(style = "font-size: 13px; color: #666; margin-bottom: 0;",
                         "A score of 50 represents typical conditions for that region. Scores above 67 suggest ",
                         "seller-favorable conditions; scores below 33 suggest buyer-favorable conditions."
                  )
                )
              )
            )
          ),
          
          # Right column
          column(
            width = 6,
            
            # 4. Regime Classification
            box(
              title       = tagList(icon("tags"), " Market Regime Classification"),
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                style = "line-height: 1.7;",
                
                tags$p("Markets are classified into three regimes based on the following rules:"),
                
                tags$div(
                  style = "background: #ffebee; padding: 15px; border-radius: 8px; margin-bottom: 12px; border-left: 4px solid #E74C3C;",
                  tags$h5(style = "margin-top: 0; color: #E74C3C;", icon("fire"), " Seller's Market"),
                  tags$p(style = "margin-bottom: 0;",
                         "Classified when ", tags$strong("both"), " conditions are met:",
                         tags$ul(
                           style = "margin-top: 5px; margin-bottom: 0;",
                           tags$li("Sale-to-List Ratio ≥ 100%"),
                           tags$li("Days on Market in the bottom third (fastest 33%) for that region")
                         )
                  )
                ),
                
                tags$div(
                  style = "background: #e3f2fd; padding: 15px; border-radius: 8px; margin-bottom: 12px; border-left: 4px solid #3498DB;",
                  tags$h5(style = "margin-top: 0; color: #3498DB;", icon("snowflake"), " Buyer's Market"),
                  tags$p(style = "margin-bottom: 0;",
                         "Classified when ", tags$strong("either"), " condition is met:",
                         tags$ul(
                           style = "margin-top: 5px; margin-bottom: 0;",
                           tags$li("Sale-to-List Ratio < 99%"),
                           tags$li("Days on Market in the top third (slowest 33%) for that region")
                         )
                  )
                ),
                
                tags$div(
                  style = "background: #fff8e1; padding: 15px; border-radius: 8px; border-left: 4px solid #F39C12;",
                  tags$h5(style = "margin-top: 0; color: #F39C12;", icon("balance-scale"), " Neutral Market"),
                  tags$p(style = "margin-bottom: 0;",
                         "All other conditions — neither strongly favoring buyers nor sellers."
                  )
                )
              )
            ),
            
            # 5. Pricing Context Methodology
            box(
              title       = tagList(icon("dollar-sign"), " Pricing Context Methodology"),
              status      = "success",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                style = "line-height: 1.7;",
                
                tags$p(
                  "The ", tags$strong("Pricing Guidance"), " tab provides a contextual price range based on current market conditions. ",
                  "This is ", tags$strong("not"), " a property-specific appraisal — it is a heuristic range anchored to regional medians."
                ),
                
                tags$h5(style = "color: #2C3E50;", "How the Range is Constructed"),
                
                tags$ol(
                  style = "padding-left: 20px;",
                  tags$li(
                    tags$strong("Base: "), "The Median Sale Price for the selected region and month."
                  ),
                  tags$li(
                    tags$strong("Market Assessment: "), "Using Sale-to-List Ratio and Days on Market (as percentiles within that region's history), ",
                    "the market is classified as Hot, Neutral, or Cool."
                  ),
                  tags$li(
                    tags$strong("Range Adjustment: "),
                    tags$ul(
                      style = "margin-top: 5px;",
                      tags$li(tags$span(style = "color: #E74C3C;", "Hot Market: "), "100% – 105% of median"),
                      tags$li(tags$span(style = "color: #F39C12;", "Neutral Market: "), "98% – 102% of median"),
                      tags$li(tags$span(style = "color: #3498DB;", "Cool Market: "), "95% – 100% of median")
                    )
                  )
                ),
                
                tags$div(
                  style = "background: #e8f5e9; padding: 12px; border-radius: 6px; margin-top: 15px;",
                  icon("info-circle", style = "color: #27AE60; margin-right: 8px;"),
                  tags$span(style = "font-size: 13px;",
                            "In hotter markets, the range shifts above the median (reflecting competitive conditions). ",
                            "In cooler markets, the range centers at or below the median (reflecting negotiating room)."
                  )
                )
              )
            ),
            
            # 6. Limitations
            box(
              title       = tagList(icon("exclamation-triangle"), " Limitations"),
              status      = "warning",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                style = "line-height: 1.7;",
                
                tags$div(
                  style = "background: #fff8e1; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
                  tags$h5(style = "margin-top: 0; color: #F39C12;", icon("layer-group"), " Aggregation"),
                  tags$p(style = "margin-bottom: 0; font-size: 13px;",
                         "Data is at the regional level, which masks significant variation across neighborhoods, ",
                         "property types (single-family vs. condo), property sizes, and conditions."
                  )
                ),
                
                tags$div(
                  style = "background: #fff8e1; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
                  tags$h5(style = "margin-top: 0; color: #F39C12;", icon("clock"), " Temporal Resolution"),
                  tags$p(style = "margin-bottom: 0; font-size: 13px;",
                         "Monthly aggregates may miss very short-term market swings or rapid changes ",
                         "occurring within a month."
                  )
                ),
                
                tags$div(
                  style = "background: #fff8e1; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
                  tags$h5(style = "margin-top: 0; color: #F39C12;", icon("ban"), " Factors Not Included"),
                  tags$p(style = "margin-bottom: 0; font-size: 13px;",
                         "The dataset does not include mortgage interest rates, income levels, employment data, ",
                         "local zoning changes, school quality, or other factors that significantly influence housing markets."
                  )
                ),
                
                tags$div(
                  style = "background: #fff8e1; padding: 12px; border-radius: 6px;",
                  tags$h5(style = "margin-top: 0; color: #F39C12;", icon("map-marker-alt"), " Regional Boundaries"),
                  tags$p(style = "margin-bottom: 0; font-size: 13px;",
                         "Metro area definitions are determined by Redfin and may not align exactly with ",
                         "official Census metropolitan statistical areas (MSAs)."
                  )
                )
              )
            ),
            
            # 7. How to Interpret
            box(
              title       = tagList(icon("lightbulb"), " How to Interpret This Dashboard"),
              status      = "success",
              solidHeader = TRUE,
              width       = NULL,
              
              tags$div(
                style = "line-height: 1.7;",
                
                tags$p(tags$strong("Users should treat the outputs as:")),
                
                tags$ul(
                  style = "padding-left: 20px;",
                  tags$li(
                    tags$strong("Comparative indicators"), " — useful for understanding relative market tightness ",
                    "and price trends across regions and over time."
                  ),
                  tags$li(
                    tags$strong("Contextual ranges"), " — the pricing guidance provides a starting point grounded in ",
                    "regional medians and metric-based heuristics, not exact valuations for any specific property."
                  ),
                  tags$li(
                    tags$strong("Directional signals"), " — the Heat Index and regime classifications help identify ",
                    "whether conditions favor buyers or sellers, but do not predict future changes."
                  )
                ),
                
                tags$div(
                  style = "background: #e3f2fd; padding: 15px; border-radius: 8px; margin-top: 15px;",
                  icon("user-tie", style = "color: #3498DB; margin-right: 8px;"),
                  tags$span(
                    "For property-specific guidance, consult a licensed real estate professional or appraiser ",
                    "who can evaluate the unique characteristics of the property and its micro-location."
                  )
                )
              )
            )
          )
        )
      )
      
    ) # end tabItems
  ) # end dashboardBody
)
