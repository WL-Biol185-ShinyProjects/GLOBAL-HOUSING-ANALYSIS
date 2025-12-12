# USA-METRO-HOUSING-ANALYSIS

# Redfin Housing Market Explorer

An interactive R Shiny dashboard for exploring U.S. housing market trends across major metropolitan areas. Built using Redfin's publicly available monthly housing data, it enables users to compare regional price trends, identify buyer/seller market regimes, visualize market heat through a composite index, and explore context-based pricing ranges anchored to regional medians.

## Team Members

- Buster Couhig
- Byron Newman

## Project Description

This dashboard provides comprehensive tools for analyzing housing market conditions across seven major U.S. regions from January 2012 to October 2025. Users can explore how median sale prices, inventory levels, days on market, and sale-to-list ratios have evolved over time, compare multiple regions simultaneously, and identify whether current market conditions favor buyers or sellers. The app transforms raw housing data into actionable insights through interactive visualizations and data-driven market classifications.

## Research Question(s)

1. How do housing market conditions vary across major U.S. metropolitan areas, and how have these differences evolved over time?
2. Can we systematically classify markets as favoring buyers or sellers based on observable metrics like sale-to-list ratios and days on market?
3. What combination of market indicators best captures overall market "heat" or intensity?
4. How can regional median prices and current market conditions inform contextual pricing expectations?

## Tabs and Features

| Tab | Description |
|-----|-------------|
| **Home** | Landing page with dashboard overview, quick stats, and navigation |
| **Market Overview** | Multi-region comparison of key metrics (price, inventory, DOM, sale-to-list) with interactive Plotly charts and customizable date ranges |
| **Region Explorer** | Deep-dive analysis of 1-3 regions with 18+ selectable metrics including MoM and YoY changes |
| **Market Heat** | Heatmap visualization of market regimes (Buyer/Neutral/Seller) and a composite Heat Index (0-100) tracking market intensity over time |
| **Cross-Section Snapshot** | Point-in-time comparison of all regions via sortable bar charts and data tables |
| **Pricing Guidance** | Context-based pricing ranges using current market conditions to suggest listing price bands |
| **Methods & Limitations** | Documentation of data sources, variable definitions, derived indicators, and analytical constraints |

### Key Interactive Features
- Region multi-select with "Select All" functionality
- Date range sliders with quick-select buttons (1Y, 3Y, 5Y, All Time)
- Metric dropdown with 18+ options grouped by category
- Interactive Plotly charts with hover tooltips
- Dynamic KPI cards showing current values with YoY change indicators
- Collapsible methodology explanations

## Methodology

### Data Source
Redfin Monthly Housing Market Data covering National aggregates and six metro areas (Boston, Chicago, Los Angeles, Philadelphia, Seattle, Washington DC).

### Key Variables
- **Median Sale Price**: Median final sale price of homes sold
- **Days on Market**: Median days from listing to contract
- **Sale-to-List Ratio**: Average ratio of sale price to list price
- **Inventory**: Active listings at month end
- **Homes Sold / New Listings**: Monthly transaction volumes

### Derived Indicators

**Heat Index (0-100)**: A composite score calculated by combining four percentile-ranked components:
- Days on Market (30%) — lower is hotter
- Sale-to-List Ratio (30%) — higher is hotter
- Inventory (20%) — lower is hotter
- Price YoY Change (20%) — higher is hotter

**Market Regime Classification**:
- *Seller's Market*: Sale-to-List ≥ 100% AND Days on Market in bottom third
- *Buyer's Market*: Sale-to-List < 99% OR Days on Market in top third
- *Neutral*: All other conditions

**Pricing Context Ranges**:
- Hot Market: 100% – 105% of median
- Neutral Market: 98% – 102% of median
- Cool Market: 95% – 100% of median

### R Packages Used
`shiny`, `shinydashboard`, `shinyWidgets`, `tidyverse`, `ggplot2`, `plotly`, `lubridate`, `scales`, `janitor`, `readr`

## Acknowledgments and Citations

### Data
- Redfin. (2025). *Monthly Housing Market Data*. Retrieved from [Redfin Data Center](https://www.redfin.com/news/data-center/)

### AI Assistance
This dashboard was developed with assistance from AI tools:
- **[Claude](https://www.anthropic.com/claude)** (Anthropic) — Primary development partner for code architecture, Shiny implementation, debugging, and iterative refinement
- **[ChatGPT](https://openai.com/chatgpt)** (OpenAI) — Contributed to initial planning and feature ideation

### Image
- Hero image: [Old World Estate in Southlake](https://commons.wikimedia.org/wiki/File:Old_World_estate_in_Southlake.JPG) — Used for educational/demonstration purposes

## Disclaimers

1. **Not Financial or Real Estate Advice**: This dashboard is for educational and exploratory purposes only. The pricing guidance feature provides heuristic-based contextual ranges, not property-specific appraisals or investment recommendations.

2. **Data Limitations**: All data is aggregated at the regional level, which masks significant variation across neighborhoods, property types, sizes, and conditions within each metro area.

3. **No Predictive Claims**: The Heat Index and market regime classifications describe current and historical conditions. They do not predict future market movements.

4. **Temporal Constraints**: Monthly aggregates may not capture rapid intra-month market changes. Data availability depends on Redfin's update schedule.

5. **Excluded Factors**: The analysis does not incorporate mortgage interest rates, employment data, income levels, zoning changes, school quality, or other factors that significantly influence local housing markets.

6. **Consult Professionals**: For property-specific guidance, consult a licensed real estate professional or appraiser who can evaluate unique property characteristics and local market conditions.
