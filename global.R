
# Loads data once and exposes types for dynamic UI

# global.R — shared by ui.R and server.R
library(shiny)
library(tidyverse)
library(leaflet)

# Read once, share everywhere -------------------------------------------

# Using base R's read.csv()
# stringsAsFactors = FALSE ensures text columns stay as characters (not factors)
house <- read.csv("global_house_purchase_dataset.csv",
                  stringsAsFactors = FALSE)

# Helpers ---------------------------------------------------------------

# Columns by type (used to populate UI controls)
num_cols <- house %>%
  select(where(is.numeric)) %>%
  names()

cat_cols <- house %>%
  select(where(function(x) is.character(x) || is.factor(x))) %>%
  names()

# A tiny, safe sample preview (used in the Overview tab)
house_head <- head(house, 10)
