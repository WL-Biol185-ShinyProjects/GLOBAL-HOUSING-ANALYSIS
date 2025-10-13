
# Loads data once and exposes types for dynamic UI

# global.R — shared by ui.R and server.R
library(shiny)
library(tidyverse)

# Read once, share everywhere
house <- readr::read_csv("global_house_purchase_dataset.csv",
                         show_col_types = FALSE)

# Helpers --------------------------------------------------------------

# Columns by type (used to populate UI controls)
num_cols <- house %>% select(where(is.numeric)) %>% names()
cat_cols <- house %>% select(where(function(x) is.character(x) || is.factor(x))) %>% names()

# A tiny, safe sample preview
house_head <- head(house, 10)
