# app.R

library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(scales)
library(tidyr)
library(lubridate)

# --- Build 2024 CD metrics (if not already created) ---

passes_cd_2024 <- rat_inspection_with_cd_df %>%
  mutate(
    cd_id = as.character(BoroCD),
    year  = year(inspection_date)
  ) %>%
  filter(year == 2024, result == "Passed", !is.na(cd_id)) %>%
  group_by(cd_id) %>%
  summarise(n_pass_2024 = n(), .groups = "drop")

calls_cd_2024 <- rat311_with_cd_df %>%
  mutate(
    cd_id = as.character(BoroCD),
    year  = year(created_date)
  ) %>%
  filter(year == 2024, !is.na(cd_id)) %>%
  group_by(cd_id) %>%
  summarise(calls_2024 = n(), .groups = "drop")

pass_call_cd_2024 <- passes_cd_2024 %>%
  left_join(calls_cd_2024, by = "cd_id") %>%
  mutate(calls_2024 = replace_na(calls_2024, 0L))

# --- Prep NTA sf with metrics (pass_call_2024 already exists from your NTA EDA) ---

nta_map <- nta %>%
  left_join(pass_call_2024, by = c("NTA2020" = "area_id")) %>%
  mutate(
    geo        = "NTA",
    geo_id     = NTA2020,
    geo_name   = NTAName,
    n_pass_2024 = replace_na(n_pass_2024, 0L),
    calls_2024  = replace_na(calls_2024, 0L)
  ) %>%
  select(geo, geo_id, geo_name, n_pass_2024, calls_2024, geometry)

# --- Prep CD sf with metrics using cd_keep ---

cd_map <- cd_keep %>%
  st_transform(st_crs(nta)) %>%
  mutate(
    geo      = "Community District",
    geo_id   = as.character(BoroCD),
    geo_name = paste("CD", geo_id)
  ) %>%
  left_join(pass_call_cd_2024, by = c("geo_id" = "cd_id")) %>%
  mutate(
    n_pass_2024 = replace_na(n_pass_2024, 0L),
    calls_2024  = replace_na(calls_2024, 0L)
  ) %>%
  select(geo, geo_id, geo_name, n_pass_2024, calls_2024, geometry)

map_data <- bind_rows(nta_map, cd_map)

# -------------------- UI --------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "geo_level", "Geography",
        choices  = c("NTA", "Community District"),
        selected = "NTA"
      ),
      radioButtons(
        "metric", "Color by",
        choices = c(
          "Passed inspections (2024)" = "n_pass_2024",
          "311 complaints (2024)"     = "calls_2024"
        ),
        selected = "n_pass_2024"
      ),
      checkboxInput(
        "show_mismatch",
        "Highlight top-quartile mismatch",
        value = TRUE
      )
    ),
    mainPanel(
      leafletOutput("map", height = 650)
    )
  )
)

# -------------------- SERVER --------------------

server <- function(input, output, session) {
  
  filtered_sf <- reactive({
    map_data %>% filter(geo == input$geo_level)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -73.95, lat = 40.70, zoom = 11)
  })
  
  observe({
    sf0 <- filtered_sf()
    
    # mismatch computed within selected geography
    x_cut <- quantile(sf0$n_pass_2024, 0.75, na.rm = TRUE)
    y_cut <- quantile(sf0$calls_2024, 0.75, na.rm = TRUE)
    
    sf0 <- sf0 %>%
      mutate(mismatch = (n_pass_2024 >= x_cut) & (calls_2024 >= y_cut))
    
    metric_col <- input$metric
    pal <- colorNumeric("viridis", domain = sf0[[metric_col]])
    
    popup_txt <- ~paste0(
      "<b>", geo_name, "</b> (", geo_id, ")<br/>",
      "Passed inspections (2024): ", comma(n_pass_2024), "<br/>",
      "311 complaints (2024): ", comma(calls_2024), "<br/>",
      "Mismatch: ", ifelse(mismatch, "Yes", "No")
    )
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data = sf0,
        color = "white", weight = 0.5,
        fillColor = ~pal(.data[[metric_col]]),
        fillOpacity = 0.75,
        popup = popup_txt,
        highlightOptions = highlightOptions(
          weight = 2, color = "#444", bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal, values = sf0[[metric_col]],
        title = ifelse(
          metric_col == "n_pass_2024",
          "Passed inspections (2024)",
          "311 complaints (2024)"
        ),
        position = "bottomright"
      )
    
    if (isTRUE(input$show_mismatch)) {
      leafletProxy("map") %>%
        addPolygons(
          data = sf0 %>% filter(mismatch),
          color = "black", weight = 1.2,
          fillColor = "red", fillOpacity = 0.30,
          popup = popup_txt
        )
    }
  })
}

shinyApp(ui, server)
