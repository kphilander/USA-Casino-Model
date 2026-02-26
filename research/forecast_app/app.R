# =============================================================================
# Casino Revenue Forecasting App
# =============================================================================
library(shiny)
library(leaflet)
library(sf)
library(DT)
library(scales)

# Load pre-built data bundle and forecast engine
env <- readRDS("forecast_data.rds")
source("forecast_engine.R")

# Pre-compute baseline model predictions for all markets (for existing casino display)
p <- env$model_params
.dw <- decay_power(env$dist_matrix, p$cg_beta)
.dw[env$dist_matrix > p$MAX_DIST] <- 0
.attract <- exp(p$cg_a_hotel * env$markets$has_hotel + p$cg_a_table * env$markets$has_tables)
.dw_a <- sweep(.dw, 2, .attract, "*")
.denom <- rowSums(.dw_a, na.rm = TRUE)
.denom[.denom == 0] <- 1
.probs <- sweep(.dw_a, 1, .denom, "/")
.demand_idx <- colSums(sweep(.probs, 1, env$pop_income, "*"), na.rm = TRUE)
.smear <- if (!is.null(p$cg_duan_smear)) p$cg_duan_smear else 1
.cr_delta <- if (!is.null(p$cg_cardroom_delta)) p$cg_cardroom_delta else 0
env$markets$model_pred_rev <- exp(p$cg_intercept + p$cg_gamma * log(.demand_idx) + .cr_delta * env$markets$is_cardroom) * .smear
env$markets$demand_index <- .demand_idx
rm(.dw, .attract, .dw_a, .denom, .probs, .demand_idx)

# Casino-to-market lookup
env$casinos_study$market_idx <- match(env$casinos_study$market_id, env$markets$market_id)

# Within-cluster attractiveness shares for individual casino revenue attribution
env$casinos_study$within_cluster_share <- 1  # default for single-casino markets
for (.mid in unique(env$casinos_study$market_id)) {
  .idx <- which(env$casinos_study$market_id == .mid)
  if (length(.idx) > 1) {
    .a <- exp(p$cg_a_hotel * env$casinos_study$has_hotel[.idx] +
              p$cg_a_table * env$casinos_study$has_tables[.idx])
    env$casinos_study$within_cluster_share[.idx] <- .a / sum(.a)
  }
}
rm(.mid, .idx, .a)

# Continental US center
map_center_lat <- 39.0
map_center_lon <- -98.0

# =============================================================================
# UI
# =============================================================================
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  div(class = "outer",

    # Full-screen leaflet map
    leafletOutput("map", width = "100%", height = "100%"),

    # ---- Left: Control Panel ----
    div(id = "controls",
      p(class = "app-title", "Casino Revenue Forecaster"),
      p(class = "app-subtitle", "Competitive Gravity Revenue Model"),

      hr(),

      # ZIP code search
      div(class = "input-row",
        textInput("zip_input", label = NULL, placeholder = "Enter ZIP code"),
        actionButton("btn_search", "Search", class = "btn-sm btn-primary")
      ),
      p(class = "help-text", "Or click anywhere on the map to place a casino"),

      # Location display (populated after click/search)
      conditionalPanel(
        condition = "output.has_location",
        uiOutput("location_display")
      ),

      hr(),

      # Casino attributes
      div(style = "display: flex; flex-wrap: wrap; gap: 4px 16px; margin-bottom: 12px;",
        tags$div(title = "Full-service casino with on-site hotel accommodations; increases destination appeal",
          checkboxInput("has_hotel", "Hotel/Resort", value = TRUE)
        ),
        tags$div(title = "Casino offers table games (blackjack, poker, roulette, etc.) in addition to slots",
          checkboxInput("has_tables", "Table Games", value = TRUE)
        ),
        tags$div(title = "Card room only \u2014 no slot machines (e.g., WA/CA commercial card rooms); implies table games",
          checkboxInput("is_cardroom", "Cardroom (no slots)", value = FALSE)
        )
      ),

      # Forecast button
      actionButton("btn_forecast", "Run Forecast", class = "btn-primary",
                   icon = icon("chart-line")),

      # Map view toggle (after forecast)
      conditionalPanel(
        condition = "output.has_forecast",
        div(class = "view-toggle",
          hr(),
          radioButtons("map_view", "Map View",
            choices = c("Visit Probability" = "prob", "Primary Market Area" = "primary"),
            selected = "prob", inline = TRUE
          ),
          p(style = "font-size: 10px; color: #888; margin-top: 2px;",
            HTML("Visit probability = modeled share of each ZIP's gambling demand
                  directed to the proposed casino, given all competing venues
                  (gravity model with power decay)."))
        )
      )
    ),

    # ---- Right: Results Panel ----
    conditionalPanel(
      condition = "output.has_results",
      div(id = "results_panel",
        uiOutput("results_display"),

        # Cannibalization section (only for forecasts)
        conditionalPanel(
          condition = "output.has_forecast",
          div(id = "canib_section",
            actionLink("toggle_canib", "Show cannibalization details",
                       class = "canib-toggle"),
            conditionalPanel(
              condition = "input.toggle_canib % 2 == 1",
              DT::dataTableOutput("canib_table")
            )
          )
        )
      )
    ),

    # Citation
    tags$div(id = "cite",
      HTML("Casino Revenue Forecaster &mdash; Kahlil Philander (2024) &mdash; Competitive Gravity Revenue Model")
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # ---- Reactive values ----
  rv <- reactiveValues(
    lat = NULL, lon = NULL,
    city = NULL, state_detected = NULL, zipcode = NULL,
    forecast_result = NULL,
    casino_info = NULL,          # existing casino click info
    shape_just_clicked = FALSE   # debounce flag for shape vs map click
  )

  # ---- Condition outputs for conditionalPanel ----
  output$has_location <- reactive({ !is.null(rv$lat) })
  outputOptions(output, "has_location", suspendWhenHidden = FALSE)

  output$has_forecast <- reactive({ !is.null(rv$forecast_result) })
  outputOptions(output, "has_forecast", suspendWhenHidden = FALSE)

  output$has_results <- reactive({ !is.null(rv$forecast_result) || !is.null(rv$casino_info) })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  # ---- Base map ----
  output$map <- renderLeaflet({
    # Show individual casinos (not market clusters)
    cas <- env$casinos_study
    aga_states <- env$state_revenue$state
    cas$in_aga <- cas$state %in% aga_states
    obs_cas   <- cas[cas$in_aga, ]
    unobs_cas <- cas[!cas$in_aga, ]

    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = map_center_lon, lat = map_center_lat, zoom = 4) %>%
      # Casinos in AGA-covered states — blue pin markers
      addAwesomeMarkers(
        data = obs_cas,
        lat = ~latitude, lng = ~longitude,
        icon = awesomeIcons(icon = "building", markerColor = "blue",
                            iconColor = "white", library = "fa"),
        layerId = paste0("cas_", obs_cas$casino_id),
        label = ~name,
        group = "aga_casinos",
        clusterOptions = markerClusterOptions()
      ) %>%
      # Casinos in non-AGA states (tribal / no revenue data) — gray pin markers
      addAwesomeMarkers(
        data = unobs_cas,
        lat = ~latitude, lng = ~longitude,
        icon = awesomeIcons(icon = "building", markerColor = "gray",
                            iconColor = "white", library = "fa"),
        layerId = paste0("cas_", unobs_cas$casino_id),
        label = ~name,
        group = "other_casinos",
        clusterOptions = markerClusterOptions()
      ) %>%
      addLayersControl(
        overlayGroups = c("aga_casinos", "other_casinos"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })

  # ---- Existing casino click handler ----
  observeEvent(input$map_marker_click, priority = 10, {
    click <- input$map_marker_click
    lid <- click$id
    if (is.null(lid) || !grepl("^cas_", lid)) return()

    rv$shape_just_clicked <- TRUE
    rv$forecast_result <- NULL  # clear forecast view

    # Look up the casino
    cid <- as.integer(sub("^cas_", "", lid))
    cas_row <- which(env$casinos_study$casino_id == cid)
    if (length(cas_row) == 0) return()

    cas <- env$casinos_study[cas_row[1], ]
    mkt_idx <- cas$market_idx
    mkt <- env$markets[mkt_idx, ]

    # Build casino info
    aga_state <- mkt$state %in% env$state_revenue$state
    state_total <- if (aga_state) {
      env$state_revenue$revenue[env$state_revenue$state == mkt$state]
    } else NA

    # Other casinos in same market cluster
    cluster_mates <- env$casinos_study[env$casinos_study$market_id == mkt$market_id, ]

    # Within-cluster share for individual casino attribution
    wcs <- cas$within_cluster_share

    rv$casino_info <- list(
      name           = cas$name,
      state          = cas$state,
      type           = cas$tribal,
      has_hotel      = cas$has_hotel,
      has_tables     = cas$has_tables,
      lat            = cas$latitude,
      lon            = cas$longitude,
      market_label   = mkt$label,
      n_in_cluster   = nrow(cluster_mates),
      cluster_names  = cluster_mates$name,
      within_cluster_share = wcs,
      allocated_rev  = if (mkt$observed) mkt$revenue * wcs else NA,
      cluster_rev    = if (mkt$observed) mkt$revenue else NA,
      tribal_pseudo  = if (!is.null(mkt$tribal_pseudo) && mkt$tribal_pseudo > 0) mkt$tribal_pseudo * wcs else NA,
      model_pred_rev = mkt$model_pred_rev * wcs,
      demand_index   = mkt$demand_index,
      aga_state      = aga_state,
      state_total    = state_total,
      observed       = mkt$observed,
      commercial_share = if (!is.null(mkt$commercial_share)) mkt$commercial_share else 1
    )
  })

  # ---- Map click handler ----
  observeEvent(input$map_click, priority = 1, {
    # Skip if an existing casino marker was just clicked
    if (isTRUE(rv$shape_just_clicked)) {
      rv$shape_just_clicked <- FALSE
      return()
    }

    click <- input$map_click
    rv$lat <- click$lat
    rv$lon <- click$lng
    rv$forecast_result <- NULL  # clear old forecast
    rv$casino_info <- NULL      # clear casino info

    # Auto-detect location
    loc <- detect_location(click$lat, click$lng, env$allzips_map)
    rv$city <- loc$city
    rv$state_detected <- loc$state
    rv$zipcode <- loc$zipcode

    # Place marker
    leafletProxy("map") %>%
      clearGroup("proposed") %>%
      addAwesomeMarkers(
        lng = click$lng, lat = click$lat,
        icon = awesomeIcons(icon = "star", markerColor = "red", library = "fa"),
        label = "Proposed Casino",
        group = "proposed"
      )
  })

  # ---- ZIP search handler ----
  observeEvent(input$btn_search, {
    zip_val <- trimws(input$zip_input)
    if (nchar(zip_val) == 0) return()

    result <- lookup_zip(zip_val, env$allzips_map)
    if (is.null(result)) {
      showNotification(paste("ZIP code", zip_val, "not found in study region."),
                       type = "warning", duration = 4)
      return()
    }

    rv$lat <- result$lat
    rv$lon <- result$lon
    rv$city <- result$city
    rv$state_detected <- result$state
    rv$zipcode <- result$zipcode
    rv$forecast_result <- NULL
    rv$casino_info <- NULL

    # Place marker and zoom
    leafletProxy("map") %>%
      clearGroup("proposed") %>%
      clearGroup("catchment") %>%
      addAwesomeMarkers(
        lng = result$lon, lat = result$lat,
        icon = awesomeIcons(icon = "star", markerColor = "red", library = "fa"),
        label = "Proposed Casino",
        group = "proposed"
      ) %>%
      setView(lng = result$lon, lat = result$lat, zoom = 10)
  })

  # ---- Location display ----
  output$location_display <- renderUI({
    req(rv$lat)
    state_label <- rv$state_detected
    div(class = "location-display",
      HTML(paste0(
        "<strong>", rv$city, ", ", rv$state_detected, " ", rv$zipcode, "</strong><br>",
        "<span style='font-size:11px; color:#888;'>",
        sprintf("%.4f, %.4f", rv$lat, rv$lon), "</span>"
      ))
    )
  })

  # ---- Cardroom toggle: auto-check tables (cardrooms ARE table games), uncheck hotel ----
  observeEvent(input$is_cardroom, {
    if (input$is_cardroom) {
      updateCheckboxInput(session, "has_hotel", value = FALSE)
      updateCheckboxInput(session, "has_tables", value = TRUE)
    }
  })

  # ---- Unchecking tables should uncheck cardroom (cardrooms must have tables) ----
  observeEvent(input$has_tables, {
    if (!input$has_tables && input$is_cardroom) {
      updateCheckboxInput(session, "is_cardroom", value = FALSE)
    }
  })

  # ---- Run forecast ----
  observeEvent(input$btn_forecast, {
    req(rv$lat, rv$lon)

    # Determine state
    target_state <- rv$state_detected

    # Run the forecast
    withProgress(message = "Running forecast...", value = 0.5, {
      result <- forecast_casino(
        lat = rv$lat, lon = rv$lon,
        has_hotel = as.integer(input$has_hotel),
        has_tables = as.integer(input$has_tables),
        is_cardroom = as.integer(input$is_cardroom),
        state = target_state,
        label = "Proposed Casino",
        env = env
      )
      incProgress(0.5)
    })

    rv$casino_info <- NULL  # clear existing casino view
    rv$forecast_result <- result
  })

  # ---- Results display ----
  output$results_display <- renderUI({
    # Case 1: Existing casino info
    if (!is.null(rv$casino_info)) {
      info <- rv$casino_info
      alloc_label <- if (!is.na(info$allocated_rev)) {
        paste0("$", formatC(round(info$allocated_rev / 1e6), format = "d", big.mark = ","), "M")
      } else "N/A"
      pred_label <- paste0("$", formatC(round(info$model_pred_rev / 1e6), format = "d", big.mark = ","), "M")

      calib_ratio <- if (!is.na(info$allocated_rev) && info$model_pred_rev > 0) {
        round(info$allocated_rev / info$model_pred_rev, 2)
      } else NA

      # Tribal pseudo-estimate label
      tribal_label <- if (!is.na(info$tribal_pseudo)) {
        paste0("$", formatC(round(info$tribal_pseudo / 1e6), format = "d", big.mark = ","), "M")
      } else NULL

      comm_pct <- round(info$commercial_share * 100)
      is_mixed <- info$aga_state && comm_pct > 0 && comm_pct < 100

      return(tagList(
        div(class = "revenue-headline", style = "font-size: 20px;", info$name),
        div(class = "revenue-label",
            paste0(info$state, " \u2022 ", info$type,
                   if (info$has_hotel) " \u2022 Hotel" else "",
                   if (info$has_tables) " \u2022 Table Games" else "")),

        if (info$n_in_cluster > 1) {
          cluster_total_label <- if (!is.na(info$cluster_rev)) {
            paste0(" &mdash; cluster total $",
                   formatC(round(info$cluster_rev / 1e6), format = "d", big.mark = ","), "M")
          } else ""
          div(style = "margin: 8px 0; padding: 6px 10px; background: #fef3c7; border-radius: 6px; font-size: 12px;",
            HTML(paste0("<strong>Market cluster</strong> (", info$n_in_cluster,
                        " casinos within 5 mi &mdash; ",
                        round(info$within_cluster_share * 100), "% attractiveness share",
                        cluster_total_label,
                        if (is_mixed) paste0("<br>", comm_pct, "% commercial attractiveness") else "",
                        ")"))
          )
        },

        # Revenue metrics (individual casino share of cluster)
        div(class = "metric-grid",
          div(class = "metric-card",
            div(class = "metric-value", alloc_label),
            div(class = "metric-label",
              if (!is.na(info$allocated_rev)) "Allocated Revenue" else "Allocated Revenue")
          ),
          div(class = "metric-card",
            div(class = "metric-value", pred_label),
            div(class = "metric-label", "Model Predicted")
          )
        ),

        # Methodology notes
        if (!is.na(calib_ratio)) {
          mixed_note <- if (is_mixed) {
            paste0("<br>Mixed cluster (", comm_pct, "% commercial attractiveness)",
                   " &mdash; tribal pseudo-est.: ", tribal_label)
          } else ""
          cluster_note <- if (info$n_in_cluster > 1) {
            paste0("<br>Within-cluster attribution: ",
                   round(info$within_cluster_share * 100), "% of cluster by relative attractiveness")
          } else ""
          div(class = "state-totals",
            HTML(paste0(
              "<strong>Calibration Ratio: ", calib_ratio, "</strong><br>",
              "<span style='font-size:11px; color:#666;'>",
              "Demand share of state total ($",
              formatC(round(info$state_total / 1e6), format = "d", big.mark = ","),
              "M)", mixed_note, cluster_note, "<br>",
              "Predicted: CG Model &mdash; ln(Rev) = 5.459 + 1.009 &times; ln(D) &minus; 1.851 &times; Cardroom</span>"
            ))
          )
        } else if (info$aga_state && info$type == "Tribal") {
          tribal_est <- if (!is.na(info$tribal_pseudo)) {
            paste0("<br><strong>Tribal pseudo-estimate: ", tribal_label, "</strong>",
                   " (derived from commercial calibration rate)")
          } else ""
          div(class = "state-totals",
            HTML(paste0(
              "<span style='font-size:11px; color:#666;'>",
              "Tribal venue &mdash; AGA state total covers commercial casinos only",
              tribal_est, "<br>",
              "CG Model: ln(Rev) = 5.459 + 1.009 &times; ln(D) &minus; 1.851 &times; Cardroom</span>"
            ))
          )
        } else {
          div(class = "state-totals",
            HTML(paste0(
              "<span style='font-size:11px; color:#666;'>",
              "No AGA state revenue data &mdash; showing raw model prediction only<br>",
              "CG Model: ln(Rev) = 5.459 + 1.009 &times; ln(D) &minus; 1.851 &times; Cardroom</span>"
            ))
          )
        },

        div(style = "margin-top: 8px; font-size: 11px; color: #888;",
          paste0("Demand Index: ", formatC(round(info$demand_index), format = "d", big.mark = ","))
        ),

        hr()
      ))
    }

    # Case 2: Forecast results
    req(rv$forecast_result)
    res <- rv$forecast_result

    rev_label <- paste0("$", formatC(round(res$predicted_revenue / 1e6), format = "d", big.mark = ","), "M")

    tagList(
      div(class = "revenue-headline", rev_label),
      div(class = "revenue-label", "Predicted Annual Revenue"),
      p(style = "font-size: 10px; color: #888; margin-top: 2px;",
        if (res$is_existing_state) {
          "Competitive gravity model, calibrated to state commercial GGR"
        } else {
          "Competitive gravity model (uncalibrated \u2014 no state GGR data)"
        }
      ),

      # Cluster proximity warning
      if (isTRUE(res$in_existing_cluster) && !is.null(res$nearby_markets)) {
        nm <- res$nearby_markets
        div(style = "margin: 8px 0; padding: 8px 10px; background: #fef3c7; border-left: 3px solid #f59e0b; border-radius: 4px; font-size: 12px;",
          HTML(paste0(
            "\u26A0\uFE0F <strong>Within existing market cluster</strong><br>",
            paste(sprintf("%s (%.1f mi)", nm$label, nm$distance), collapse = "<br>"),
            "<br><span style='color:#92400e;'>Direct product competition expected in this local market.</span>"
          ))
        )
      },

      div(class = "metric-grid",
        if (res$is_existing_state) {
          tagList(
            div(class = "metric-card",
              div(class = "metric-value", paste0(round(res$predicted_share * 100, 1), "%")),
              div(class = "metric-label", "State Market Share")
            ),
            div(class = "metric-card expansion",
              div(class = "metric-value", paste0("+", round(res$expansion_pct, 1), "%")),
              div(class = "metric-label", "Market Expansion")
            )
          )
        } else {
          div(class = "metric-card", style = "grid-column: span 2;",
            div(class = "metric-value", "New State Market"),
            div(class = "metric-label", "No existing observed casinos for comparison")
          )
        }
      ),

      if (res$is_existing_state) {
        div(class = "state-totals",
          HTML(paste0(
            "<strong>", res$state, " State Totals</strong><br>",
            "<div class='before-after'>",
            "<span>Before: $", formatC(round(res$old_state_total / 1e6), format = "d", big.mark = ","), "M</span>",
            "<span>After: $", formatC(round(res$new_state_total / 1e6), format = "d", big.mark = ","), "M</span>",
            "</div>",
            "<div class='before-after'>",
            "<span>Expansion: $", formatC(round(res$expansion_dollars / 1e6), format = "d", big.mark = ","), "M</span>",
            "<span>Demand Index: ", formatC(round(res$demand_index), format = "d", big.mark = ","), "</span>",
            "</div>"
          ))
        )
      },

      hr()
    )
  })

  # ---- Cannibalization table ----
  output$canib_table <- DT::renderDataTable({
    req(rv$forecast_result)
    ct <- rv$forecast_result$cannibalization_table
    if (is.null(ct)) return(NULL)

    # Format for display
    display <- data.frame(
      Market = ct$Market,
      `Before ($M)` = ct$Before_Rev_M,
      `After ($M)` = ct$After_Rev_M,
      `Change ($M)` = ct$Change_M,
      `Share Chg (pp)` = ct$Share_Change_pp,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    DT::datatable(display,
      options = list(
        pageLength = 20,
        dom = 't',
        ordering = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-right', targets = 1:4)
        )
      ),
      rownames = FALSE,
      selection = "none"
    ) %>%
      formatStyle("Change ($M)",
        color = styleInterval(0, c("#dc2626", "#16a34a")),
        fontWeight = "bold"
      ) %>%
      formatStyle("Share Chg (pp)",
        color = styleInterval(0, c("#dc2626", "#16a34a"))
      )
  })

  # ---- Catchment area map rendering ----
  observe({
    req(rv$forecast_result)
    res <- rv$forecast_result
    view_mode <- input$map_view

    # Get visit probabilities (indexed by zips_study rows)
    visit_prob <- res$zip_visit_prob

    # Build a lookup table: zipcode -> prob/primary
    # This correctly joins zips_study (model) with allzips_map (geometry)
    prob_df <- data.frame(
      zipcode = env$zips_study$zipcode,
      prob    = visit_prob,
      primary = res$zip_primary,
      stringsAsFactors = FALSE
    )

    # Filter to significant ZIPs (prob > 0.5%) for performance
    prob_df <- prob_df[prob_df$prob > 0.005, ]
    if (nrow(prob_df) == 0) return()

    # Join with map polygons by zipcode
    sig_zips <- merge(env$allzips_map, prob_df, by = "zipcode")

    proxy <- leafletProxy("map") %>%
      clearGroup("catchment")

    if (view_mode == "prob") {
      # Visit probability heatmap — use quantile-based breaks for better contrast
      probs_vec <- sig_zips$prob

      pal <- colorNumeric(
        palette = c("#ffffcc", "#fd8d3c", "#e31a1c", "#800026"),
        domain = c(0, max(probs_vec)),
        na.color = "transparent"
      )

      # Scale opacity with probability for better visual effect
      fill_opac <- pmin(0.2 + 0.6 * (probs_vec / max(probs_vec)), 0.8)

      proxy %>%
        addPolygons(
          data = sig_zips,
          fillColor = pal(probs_vec),
          weight = 0.3,
          opacity = 0.5,
          color = "white",
          fillOpacity = fill_opac,
          group = "catchment",
          label = ~paste0(zipcode, " (", city, ", ", state, "): ",
                          round(prob * 100, 1), "%"),
          highlightOptions = highlightOptions(
            weight = 2, color = "#333", fillOpacity = 0.8, bringToFront = TRUE
          )
        ) %>%
        addLegend(
          "bottomright", pal = pal, values = probs_vec,
          title = "Visit Probability",
          labFormat = labelFormat(suffix = "%", transform = function(x) round(x * 100, 1)),
          layerId = "catchment_legend",
          group = "catchment"
        )

    } else {
      # Primary market area: highlight ZIPs where new casino is #1
      is_primary <- sig_zips$primary == res$new_market_idx
      fill_colors <- ifelse(is_primary, "#e31a1c", "#d4d4d4")
      fill_opacity <- ifelse(is_primary, 0.6, 0.2)

      proxy %>%
        addPolygons(
          data = sig_zips,
          fillColor = fill_colors,
          weight = 0.3,
          opacity = 0.5,
          color = "white",
          fillOpacity = fill_opacity,
          group = "catchment",
          label = ~paste0(zipcode, " (", city, ", ", state, ")",
                          ifelse(is_primary, " - PRIMARY", "")),
          highlightOptions = highlightOptions(
            weight = 2, color = "#333", fillOpacity = 0.8, bringToFront = TRUE
          )
        ) %>%
        addLegend(
          "bottomright",
          colors = c("#e31a1c", "#d4d4d4"),
          labels = c("Primary Market (new casino #1)", "Secondary (other casino #1)"),
          title = "Market Area",
          layerId = "catchment_legend",
          group = "catchment"
        )
    }
  })
}

# =============================================================================
# Run
# =============================================================================
shinyApp(ui, server)
