
# server.R
source("global.R")

shinyServer(function(input, output, session) {
  # ====================================================================
  # OVERVIEW TAB
  # ====================================================================
  output$overview_image_placeholder <- renderUI({
    tags$div(
      style = paste(
        "height: 240px; border: 2px dashed #bbb;",
        "border-radius: 8px; display: flex; align-items: center;",
        "justify-content: center; background-color: #fcfcfc;"
      ),
      tags$span(
        style = "color:#888;",
        "Image placeholder — add visualization or header image here"
      )
    )
  })
  
  output$overview_intro <- renderUI({
    tags$p(
      "Use this application to explore housing purchase patterns across major global cities. ",
      "The Descriptive Market Insights tab summarizes prices, size–price trends, amenities in high-value homes, ",
      "and the relationship between home age and value. An interactive map lets you drill from country to city."
    )
  })
  
  # Value boxes
  output$vb_min_year <- renderValueBox({
    valueBox(
      subtitle = "Earliest Build Year",
      value = dataset_stats$min_constructed_year,
      icon = icon("calendar"),
      color = "teal"
    )
  })
  
  output$vb_countries <- renderValueBox({
    valueBox(
      subtitle = "Countries",
      value = dataset_stats$n_countries,
      icon = icon("globe"),
      color = "aqua"
    )
  })
  
  output$vb_cities <- renderValueBox({
    valueBox(
      subtitle = "Cities",
      value = dataset_stats$n_cities,
      icon = icon("city"),
      color = "blue"
    )
  })
  
  output$vb_rows <- renderValueBox({
    valueBox(
      subtitle = "Datapoints",
      value = format(dataset_stats$n_rows, big.mark = ","),
      icon = icon("database"),
      color = "purple"
    )
  })
  
  # ====================================================================
  # CURRENCY NORMALIZATION LAYER (for Market Insights + Map)
  # ====================================================================
  
  base_df <- reactive({
    house %>%
      dplyr::left_join(currency_map, by = "country") %>%
      dplyr::left_join(fx_rates, by = "currency") %>%
      dplyr::mutate(rate_to_usd = dplyr::coalesce(rate_to_usd, 1))
  })
  
  output$fx_note <- renderUI({
    if (identical(input$currency_basis, "USD (FX, nominal)") && !fx_available) {
      tags$div(
        style = "padding:6px 10px; background:#fff3cd; border:1px solid #ffeeba; border-radius:6px;",
        tags$small("USD conversion needs an FX table (fx_rates_static.csv). Showing local currency instead.")
      )
    } else NULL
  })
  
  df_price <- reactive({
    df <- base_df()
    use_usd <- identical(input$currency_basis, "USD (FX, nominal)") && fx_available
    df %>%
      dplyr::mutate(
        price_display = if (use_usd) price * rate_to_usd else price,
        price_is_usd  = use_usd
      )
  })
  
  # --------------------------------------------------------------------
  # Robust formatters (no crashes on NA/NaN/Inf)
  # --------------------------------------------------------------------
  pretty_si <- function(x, accuracy = 0.1) {
    x[!is.finite(x)] <- NA_real_
    out <- ifelse(
      is.na(x), NA_character_,
      ifelse(
        abs(x) >= 1e9, paste0(scales::number(x / 1e9, accuracy = accuracy), "B"),
        ifelse(
          abs(x) >= 1e6, paste0(scales::number(x / 1e6, accuracy = accuracy), "M"),
          ifelse(
            abs(x) >= 1e3, paste0(scales::number(x / 1e3, accuracy = accuracy), "k"),
            scales::number(x, accuracy = accuracy)
          )
        )
      )
    )
    out
  }
  
  fmt_money <- function(x) {
    if (isTruthy(input$currency_basis) &&
        identical(input$currency_basis, "USD (FX, nominal)") &&
        fx_available) {
      scales::dollar(x)   # handles NA
    } else {
      pretty_si(x)
    }
  }
  
  # ====================================================================
  # (a) AVERAGE PRICES TABLES (country / city / property type)
  # ====================================================================
  output$tbl_avg_country <- renderTable({
    df_price() %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(avg_price = mean(price_display, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(avg_price = dplyr::if_else(is.nan(avg_price), NA_real_, avg_price)) %>%
      dplyr::filter(is.finite(avg_price) | is.na(avg_price)) %>%
      dplyr::arrange(dplyr::desc(avg_price)) %>%
      dplyr::mutate(`Average price` = fmt_money(avg_price)) %>%
      dplyr::select(country, `Average price`)
  }, striped = TRUE, spacing = "s")
  
  output$tbl_avg_city <- renderTable({
    df_price() %>%
      dplyr::group_by(country, city) %>%
      dplyr::summarise(avg_price = mean(price_display, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(avg_price = dplyr::if_else(is.nan(avg_price), NA_real_, avg_price)) %>%
      dplyr::filter(is.finite(avg_price) | is.na(avg_price)) %>%
      dplyr::arrange(country, dplyr::desc(avg_price)) %>%
      dplyr::mutate(`Average price` = fmt_money(avg_price)) %>%
      dplyr::select(country, city, `Average price`)
  }, striped = TRUE, spacing = "s")
  
  output$tbl_avg_property <- renderTable({
    df_price() %>%
      dplyr::group_by(property_type) %>%
      dplyr::summarise(avg_price = mean(price_display, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(avg_price = dplyr::if_else(is.nan(avg_price), NA_real_, avg_price)) %>%
      dplyr::filter(is.finite(avg_price) | is.na(avg_price)) %>%
      dplyr::arrange(dplyr::desc(avg_price)) %>%
      dplyr::mutate(`Average price` = fmt_money(avg_price)) %>%
      dplyr::select(property_type, `Average price`)
  }, striped = TRUE, spacing = "s")
  
  # ====================================================================
  # (b) SIZE vs PRICE CURVE (binned sqft)
  # ====================================================================
  output$plot_size_price <- renderPlot({
    df_price() %>%
      dplyr::mutate(
        size_bin = cut(
          property_size_sqft,
          breaks = quantile(property_size_sqft, probs = seq(0, 1, 0.05), na.rm = TRUE),
          include.lowest = TRUE, dig.lab = 8
        )
      ) %>%
      dplyr::group_by(size_bin) %>%
      dplyr::summarise(
        avg_price = mean(price_display, na.rm = TRUE),
        avg_sqft  = mean(property_size_sqft, na.rm = TRUE),
        n = dplyr::n(), .groups = "drop"
      ) %>%
      dplyr::mutate(
        avg_price = dplyr::if_else(is.nan(avg_price), NA_real_, avg_price),
        avg_sqft  = dplyr::if_else(is.nan(avg_sqft),  NA_real_, avg_sqft)
      ) %>%
      dplyr::filter(is.finite(avg_price), is.finite(avg_sqft)) %>%
      ggplot2::ggplot(ggplot2::aes(x = avg_sqft, y = avg_price, group = 1)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::labs(x = "Average sqft (bin center)", y = NULL, title = "Property size vs. price (binned)") +
      ggplot2::scale_y_continuous(labels = fmt_money) +
      ggplot2::theme_minimal()
  })
  
  # ====================================================================
  # (c) AMENITIES IN HIGH-VALUE HOMES (top quartile by chosen currency)
  # ====================================================================
  output$plot_amenities_hv <- renderPlot({
    df <- df_price()
    q3 <- stats::quantile(df$price_display, 0.75, na.rm = TRUE)
    df %>%
      dplyr::mutate(is_high_value = price_display >= q3) %>%
      dplyr::filter(is_high_value) %>%
      dplyr::summarise(
        garage_share = mean(garage == 1, na.rm = TRUE),
        garden_share = mean(garden == 1, na.rm = TRUE)
      ) %>%
      tidyr::pivot_longer(dplyr::everything(), names_to = "amenity", values_to = "share") %>%
      dplyr::mutate(amenity = dplyr::recode(amenity, garage_share = "Garage", garden_share = "Garden")) %>%
      ggplot2::ggplot(ggplot2::aes(x = amenity, y = share)) +
      ggplot2::geom_col() +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(x = NULL, y = "Share of high-value homes", title = "Amenities in top-quartile priced homes") +
      ggplot2::theme_minimal()
  })
  
  # ====================================================================
  # (d) AGE OF HOUSE IMPACT (constructed_year vs avg price)
  # ====================================================================
  output$plot_age_impact <- renderPlot({
    df_price() %>%
      dplyr::group_by(constructed_year) %>%
      dplyr::summarise(avg_price = mean(price_display, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(avg_price = dplyr::if_else(is.nan(avg_price), NA_real_, avg_price)) %>%
      dplyr::filter(is.finite(avg_price), is.finite(constructed_year)) %>%
      dplyr::arrange(constructed_year) %>%
      ggplot2::ggplot(ggplot2::aes(x = constructed_year, y = avg_price)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::labs(x = "Constructed year", y = NULL, title = "Home age (constructed year) vs. average price") +
      ggplot2::scale_y_continuous(labels = fmt_money) +
      ggplot2::theme_minimal()
  })
  
  # ====================================================================
  # (e) INTERACTIVE MAP (country view -> click -> city view)
  # ====================================================================
  output$map_prices <- renderLeaflet({
    df_c <- df_price() %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(avg_price = mean(price_display, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(avg_price = dplyr::if_else(is.nan(avg_price), NA_real_, avg_price)) %>%
      dplyr::filter(is.finite(avg_price)) %>%
      dplyr::inner_join(country_coords, by = "country")
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 10, lat = 20, zoom = 2) %>%
      addCircleMarkers(
        data    = df_c,
        lng     = ~lng, lat = ~lat,
        radius  = ~scales::rescale(avg_price, to = c(6, 16)),
        label   = ~paste0(
          country, ": ",
          if (identical(input$currency_basis, "USD (FX, nominal)") && fx_available)
            scales::dollar(round(avg_price, 0))
          else
            pretty_si(round(avg_price, 0))
        ),
        layerId = ~country,
        stroke  = FALSE, fillOpacity = 0.7
      )
  })
  
  observeEvent(input$map_prices_marker_click, {
    click <- input$map_prices_marker_click
    req(click$id %in% country_coords$country)
    
    df_city <- df_price() %>%
      dplyr::group_by(country, city) %>%
      dplyr::summarise(avg_price = mean(price_display, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(avg_price = dplyr::if_else(is.nan(avg_price), NA_real_, avg_price)) %>%
      dplyr::filter(country == click$id, is.finite(avg_price)) %>%
      dplyr::inner_join(city_coords, by = c("country", "city"))
    
    if (nrow(df_city) == 0) return()
    
    leafletProxy("map_prices") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data    = df_city,
        lng     = ~lng, lat = ~lat,
        radius  = ~scales::rescale(avg_price, to = c(5, 14)),
        label   = ~paste0(
          city, ": ",
          if (identical(input$currency_basis, "USD (FX, nominal)") && fx_available)
            scales::dollar(round(avg_price, 0))
          else
            pretty_si(round(avg_price, 0))
        ),
        stroke  = FALSE, fillOpacity = 0.75
      ) %>%
      fitBounds(lng1 = min(df_city$lng), lat1 = min(df_city$lat),
                lng2 = max(df_city$lng), lat2 = max(df_city$lat))
  })
  
  # Reset overlay button (in UI) -> back to global country view
  observeEvent(input$btn_reset_map, {
    df_c <- df_price() %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(avg_price = mean(price_display, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(avg_price = dplyr::if_else(is.nan(avg_price), NA_real_, avg_price)) %>%
      dplyr::filter(is.finite(avg_price)) %>%
      dplyr::inner_join(country_coords, by = "country")
    
    leafletProxy("map_prices") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data    = df_c,
        lng     = ~lng, lat = ~lat,
        radius  = ~scales::rescale(avg_price, to = c(6, 16)),
        label   = ~paste0(
          country, ": ",
          if (identical(input$currency_basis, "USD (FX, nominal)") && fx_available)
            scales::dollar(round(avg_price, 0))
          else
            pretty_si(round(avg_price, 0))
        ),
        layerId = ~country,
        stroke  = FALSE, fillOpacity = 0.7
      ) %>%
      setView(lng = 10, lat = 20, zoom = 2)
  })
})
