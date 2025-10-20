
# global.R — shared by ui.R and server.R
# Loads data once and defines helpers used across the app

library(shiny)
library(tidyverse)
library(leaflet)
library(shinydashboard)
library(ggpubr)     # (kept; not strictly required after switching pies to Plotly)
library(plotly)

# ---------------------------------------------------------------------
# Read once, share everywhere
# ---------------------------------------------------------------------
house <- read.csv("global_house_purchase_dataset.csv",
                  stringsAsFactors = FALSE)

# ---------------------------------------------------------------------
# Helpers (column types if needed later)
# ---------------------------------------------------------------------
num_cols <- house %>%
  dplyr::select(where(is.numeric)) %>%
  names()

cat_cols <- house %>%
  dplyr::select(where(function(x) is.character(x) || is.factor(x))) %>%
  names()

# ---------------------------------------------------------------------
# Dataset stats for the Overview tab (computed once)
# ---------------------------------------------------------------------
dataset_stats <- list(
  min_constructed_year = min(house$constructed_year, na.rm = TRUE),
  n_countries          = dplyr::n_distinct(house$country),
  n_cities             = dplyr::n_distinct(house$city),
  n_rows               = nrow(house)
)

# ---------------------------------------------------------------------
# Currency mapping (by country) and optional FX table
#   - currency_symbol used for compact local formatting
# ---------------------------------------------------------------------
currency_map <- tibble::tribble(
  ~country,        ~currency, ~currency_symbol,
  "France",        "EUR",     "€",
  "Germany",       "EUR",     "€",
  "Canada",        "CAD",     "C$",
  "Brazil",        "BRL",     "R$",
  "UAE",           "AED",     "AED",
  "Australia",     "AUD",     "A$",
  "UK",            "GBP",     "£",
  "USA",           "USD",     "$",
  "China",         "CNY",     "¥",
  "Singapore",     "SGD",     "S$",
  "India",         "INR",     "₹",
  "Japan",         "JPY",     "¥",
  "South Africa",  "ZAR",     "R"
)

# Optional: static FX table in app folder
# Expect columns: currency, rate_to_usd  (e.g., EUR, 1.08)
fx_rates_path <- "fx_rates_static.csv"
fx_rates <- if (file.exists(fx_rates_path)) {
  readr::read_csv(fx_rates_path, show_col_types = FALSE) %>%
    dplyr::select(currency, rate_to_usd)
} else {
  tibble::tibble(currency = "USD", rate_to_usd = 1)
}

fx_available <- all(c("currency", "rate_to_usd") %in% names(fx_rates)) &&
  any(fx_rates$currency != "USD")

# ---------------------------------------------------------------------
# Map scaffolding: country & city coordinates (centroids)
# ---------------------------------------------------------------------
country_coords <- tibble::tribble(
  ~country,       ~lat,     ~lng,
  "France",        46.2276,   2.2137,
  "South Africa", -30.5595,  22.9375,
  "Germany",       51.1657,  10.4515,
  "Canada",        56.1304, -106.3468,
  "Brazil",       -14.2350, -51.9253,
  "UAE",           23.4241,  53.8478,
  "Australia",    -25.2744, 133.7751,
  "UK",            55.3781,  -3.4360,
  "USA",           37.0902, -95.7129,
  "China",         35.8617, 104.1954,
  "Singapore",      1.3521, 103.8198,
  "India",         20.5937,  78.9629,
  "Japan",         36.2048, 138.2529
)

city_coords <- tibble::tribble(
  ~city,            ~country,        ~lat,      ~lng,
  "Paris",          "France",         48.8566,    2.3522,
  "Lyon",           "France",         45.7640,    4.8357,
  "Marseille",      "France",         43.2965,    5.3698,
  "Berlin",         "Germany",        52.5200,   13.4050,
  "Frankfurt",      "Germany",        50.1109,    8.6821,
  "Munich",         "Germany",        48.1351,   11.5820,
  "Toronto",        "Canada",         43.6532,  -79.3832,
  "Montreal",       "Canada",         45.5017,  -73.5673,
  "Vancouver",      "Canada",         49.2827, -123.1207,
  "Rio de Janeiro", "Brazil",        -22.9068,  -43.1729,
  "São Paulo",      "Brazil",        -23.5505,  -46.6333,
  "Dubai",          "UAE",            25.2048,   55.2708,
  "Abu Dhabi",      "UAE",            24.4539,   54.3773,
  "Sydney",         "Australia",     -33.8688,  151.2093,
  "Melbourne",      "Australia",     -37.8136,  144.9631,
  "Brisbane",       "Australia",     -27.4698,  153.0251,
  "London",         "UK",             51.5074,   -0.1278,
  "Liverpool",      "UK",             53.4084,   -2.9916,
  "Manchester",     "UK",             53.4808,   -2.2426,
  "Birmingham",     "UK",             52.4862,   -1.8904,
  "New York",       "USA",            40.7128,  -74.0060,
  "Los Angeles",    "USA",            34.0522, -118.2437,
  "San Francisco",  "USA",            37.7749, -122.4194,
  "Chicago",        "USA",            41.8781,  -87.6298,
  "Houston",        "USA",            29.7604,  -95.3698,
  "Beijing",        "China",          39.9042,  116.4074,
  "Shanghai",       "China",          31.2304,  121.4737,
  "Shenzhen",       "China",          22.5431,  114.0579,
  "Singapore",      "Singapore",       1.3521,  103.8198,
  "Tokyo",          "Japan",          35.6762,  139.6503,
  "Osaka",          "Japan",          34.6937,  135.5023,
  "Delhi",          "India",          28.7041,   77.1025,
  "Mumbai",         "India",          19.0760,   72.8777,
  "Bangalore",      "India",          12.9716,   77.5946,
  "Hyderabad",      "India",          17.3850,   78.4867,
  "Pune",           "India",          18.5204,   73.8567,
  "Cape Town",      "South Africa",  -33.9249,   18.4241,
  "Johannesburg",   "South Africa",  -26.2041,   28.0473,
  "Chennai",        "India",          13.0827,   80.2707
)
