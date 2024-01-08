library(tidyverse)
library(maps)
library(leaflet)
library(shiny)
library(DT)
library(here)
library(sp)
library(sf)
library(rgdal)
library(lubridate)
library(shiny.telemetry)
library(leaflet.extras)
library(geojsonsf)
library(geojsonio)
library(dbscan)
library(factoextra)
library(mapboxapi)
library(RColorBrewer)
library(basemaps)
library(ggmap)
library(plotly)
library(shinyjqui)

url_month <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_month.geojson"
url_week <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_week.geojson"
url_day <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_day.geojson"
#color_list <- c("Reds", "Spectral", "Pastel1", "PuRd", "PuBuGn")
color_list = rownames(subset(brewer.pal.info, category %in% c("seq", "div")))

world <- map_data("world")
#WorldData <- map_data('world')
#wdf <- st_as_sf(WorldData, coords = c("long", "lat"), crs = 4326)
#print(st_crs(wdf))

ui <- navbarPage("USGS Earthquakes - Real Time Data", 
      tabPanel("panel 1", htmlTemplate("template.html",
      nav =  navlistPanel("USGS Earthquakes"),
      map = leafletOutput("eqMap", width = "auto", height = "auto"),
      filters = tabsetPanel(type = "tabs",
                           tabPanel("Data Filters",  selectInput("dataSelect", h4("Select GeoJSON Feed"),
                                                                 choices = c("1 Month", "1 Week", "1 Day"),
                                                       selected = "1 Month"),
                           sliderInput("slider", h4("Select the magnitude"), 2, 9, value=c(2, 9)),  
                           selectInput("color_choice", h4("Symbology"), color_list, selected = "RdBu")),
                           tabPanel("DBSCAN Parameters", numericInput("eps_input", h4("eps"), 0.45, min = 0.1, max = .99, step = .01),
                            numericInput("minpts_input", h4("minPts"), 5, min = 1, max = 100, step = 1),
                           )),
      renderdbscan = uiOutput("dbovermap"),
      toggleplot = uiOutput("toggle"),
      timeTable = dataTableOutput("timeTable"),
))
)

server <- function(input, output, session) {
  
  output$eqMap <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "DarkMatter", options = tileOptions(noWrap = FALSE)) %>% # add CARTO tiles
      addTiles(
        urlTemplate = "http://{s}.tiles.mapbox.com/v4/mapbox.satellite/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoidGZqYWNrYyIsImEiOiJjbGhhd3VsZHAwbHV1M3RudGt0bWFhNHl0In0.5qDpeYjN5r-rBh-SYA9Qgw",
        options = tileOptions(
          id = "mapbox/satellite-v9",  # Replace with your desired Mapbox style ID
          accessToken = "pk.eyJ1IjoidGZqYWNrYyIsImEiOiJjbGhhd3VsZHAwbHV1M3RudGt0bWFhNHl0In0.5qDpeYjN5r-rBh-SYA9Qgw"  # Replace with your Mapbox access token
        ),
        group = "Satellite"
      ) %>%
      setView(-18.525960, 26.846869, 3) %>%
      addLayersControl(overlayGroups = c("vectorData"), baseGroups = c("OSM", "DarkMatter", "Satellite")) %>%
      addDrawToolbar( polylineOptions = FALSE,
                      polygonOptions = FALSE,
                      rectangleOptions = FALSE,
                      circleOptions = TRUE,
                      markerOptions = FALSE,
                      circleMarkerOptions = FALSE,
                      singleFeature = TRUE,
                      editOptions = editToolbarOptions())
  })
  
  dataInput <- reactive({
    if (input$dataSelect != "1 Month" & input$dataSelect != "1 Week") {
      url_day
    } else if (input$dataSelect != "1 Month" & input$dataSelect != "1 Day") {
      url_week
    } else if (input$dataSelect != "1 Week" & input$dataSelect != "1 Day") {
      url_month
    }
  })
  
  filteredEqsf <- reactive({
    earthquakes <- read_sf(dataInput())
    eqsf <- st_as_sf(earthquakes)
    eqsf <- st_transform(eqsf, 4326)
    eqsf$time <- as.POSIXct(as.numeric(eqsf$time)/1000, origin = "1970-01-01", tz = "America/Los_Angeles")
    eqsf$time_formatted <- format(eqsf$time, "%Y-%m-%d %I:%M:%S %p %Z")
    eqsf_table <- eqsf %>%
      st_drop_geometry(eqsf) %>%
      select(mag, place, time_formatted)
    
    filteredData <- eqsf %>%
      filter(mag >= input$slider[1] & mag <= input$slider[2])
    
    pal <- colorBin(
      palette = input$color_choice,
      domain = filteredData$mag,
      reverse = TRUE,
      bins = 5
    )
    
    list(filteredData = filteredData, pal = pal, eqsf_table = eqsf_table, eqsf = eqsf)
  })
  
  
  observe({
    filteredData <- filteredEqsf()$filteredData
    pal <- filteredEqsf()$pal
    
    leafletProxy("eqMap", data = filteredData) %>%
      clearMarkers() %>%
      addCircleMarkers(
        fillColor = ~pal(mag),
        radius = ~(mag*2), 
        weight = 1,
        stroke = TRUE,
        color = "black",
        fillOpacity = 1,
        popup = paste0(
          "<strong>Title:</strong> ", filteredData$title,
          "<br><strong>Time:</strong> ", format(filteredData$time, "%Y-%m-%d %I:%M:%S %p %Z"),
          "<br><strong>Magnitude:</strong> ", filteredData$mag
        ),
        group = "vectorData"
      ) %>%
      clearControls() %>%
      addLegend("topleft", pal = pal, values = ~filteredData$mag,
                title = "Magnitude",
                opacity = 1)
  })
  
  observe({
    eqsf_table <- filteredEqsf()$eqsf_table
    output$timeTable <- DT::renderDataTable(eqsf_table, server = FALSE, options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ))
  })
  
  # features are created/edited/deleted from the map
  observeEvent(input$eqMap_draw_all_features, {
    
    runDBSCAN <- function(circle_geom, db, xmin, xmax, ymin, ymax, locs) { 
      
      eps_input <- reactive(input$eps_input)
      minpts_input <- reactive(input$minpts_input)
      
      # Calculate clustering result and cluster visualization
      db_result <- reactive({
        dbscan::dbscan(locs, eps = eps_input(), minPts = minpts_input())
      })
      
      cluster_data <- reactive({
        factoextra::fviz_cluster(db_result(), locs, stand = FALSE, ellipse = TRUE, ggtheme = theme_minimal(), geom = "point")
      })
      
      # Render dynamic UI
      output$dbovermap <- renderUI({
        jqui_draggable(jqui_resizable(plotOutput("dbscan_plot", height="480")))
      })
      
      bbox <- st_bbox(circle_geom)
      ymin <- as.numeric(bbox['ymin'])
      ymax <- as.numeric(bbox['ymax'])
      xmax <- as.numeric(bbox['xmax'])
      xmin <- as.numeric(bbox['xmin'])
      
      # Render plot
      output$dbscan_plot <- renderPlot({
        
        cluster_data_plot <- cluster_data()
        
        cluster_data_plot +
          geom_map(
            data = world, map = world,
            aes(long, lat, map_id = region),
            color = "black", fill = NA
          ) +
          coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
        
      })
      
      outputOptions(output, "dbscan_plot", suspendWhenHidden = FALSE)
      
      output$toggle <- renderUI({
        checkboxInput("toggle_plot", h4("DBSCAN Plot"), TRUE)
      })
    }
    
    eqsf <- filteredEqsf()$eqsf

    if (!is.null(input$eqMap_draw_all_features) && length(input$eqMap_draw_all_features$features) > 0) {
      
      
      numFeatures <- length(input$eqMap_draw_all_features$features)
      lng <- input$eqMap_draw_all_features$features[[numFeatures]]$geometry$coordinates[1]
      lat <- input$eqMap_draw_all_features$features[[numFeatures]]$geometry$coordinates[2]
      radius <- input$eqMap_draw_all_features$features[[numFeatures]]$properties$radius
      
      print(paste0("geom coordinates: ", lat, ", ", lng))
      
      if (!is.null(radius)) {
        print(paste("radius: ", round(radius, digits = 2), "m"))
        
        # Convert radius from meters to decimal degrees
        new_geom <- data.frame(lon = as.numeric(lng), lat = as.numeric(lat))
        new_geom <- st_as_sf(new_geom, coords = c("lon", "lat"), crs = 4326) #4979 change before deployment to 4326
        
        circle_geom <- st_buffer(new_geom, radius)
        
        circle_pts <- st_intersection(eqsf, circle_geom)
        df <- st_as_sf(circle_pts)
        df_coords <- data.frame(st_coordinates(df))
        locs <- dplyr::select(df_coords,X,Y) 
        
        runDBSCAN(circle_geom=circle_geom, locs=locs, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
      }
    }
  })
  
}


shinyApp(ui, server)