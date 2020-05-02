# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# -----------Libraries----------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

library(dplyr)
library(rio)
library(sf)
library(shiny)
library(leaflet)
library(viridis)
library(shiny)
library(devtools)
library(shinydashboard)
library(V8)
library(shinyjs)
library(RColorBrewer)
library(readr)
library(tidyr)
library(leaflet.extras)
library(htmltools)
library(shinyWidgets)
library(ggplot2)
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----Reference Data & Styles---- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

raw_data = import("v10almost_newer_natural_resources.dta")


df = raw_data %>%
  dplyr::select(locationname, country,year, resource, local_value ,log_local_val, latitude, longitude) 

df = na.omit(df)



# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(
    id = "topbar",
    fixed = TRUE,
    draggable = FALSE,
    top = "0%",
    left = "0%",
    right = "0%",
    bottom = "95%",
    
    
    fluidRow(
      column(8, offset = 1, 
             h1(strong("IPD Natural Resource Explorer"),
                h3("(",textOutput("num_matching", inline = TRUE)," locations)"))
      )
      
      
    )),
  absolutePanel(
                top = "2.5%",
                left = "85%",
                right = "2.5%",
                bottom = "85%",
                
                selectInput(inputId = "resource", label = "Resource",
                            width = '100%',
                            choices = unique(df$resource),
                            selected = c("oil"))
                )
  )



# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# --------Dashboard Server------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    df %>% 
      filter(resource %in% input$resource)
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({ 
    
              colorNumeric(
                palette = "viridis",
                domain = filteredData()$log_local_val)
  }) 
  
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data = filteredData(), 
            options = leafletOptions(
              attributionControl=FALSE)) %>%
      addTiles(
        urlTemplate = "https://tile.thunderforest.com/cycle/{z}/{x}/{y}.png?apikey=aae485d383324e008257aab3f9467916",
        attribution = 'Imagery from <a href="http://giscience.uni-hd.de/">GIScience Research Group @ University of Heidelberg</a> | Map data &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors', 
        options = tileOptions(minZoom = 2, maxZoom = 18)
      ) 
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      
      clearShapes() %>%
      
      addCircles(radius = ~log_local_val*5000, weight = 1, color = "#777777",
                 fillColor = ~pal(log_local_val), fillOpacity = 0.4, popup = ~paste0(
                   "<h4/><b>",locationname,"</b><h5/>",
                   "<h5/>","Resource: ", resource,
                   "<h5/>","Local Value of Exports: ",local_value
                 )) %>% 
      addLegend("bottomright",pal = pal, values = ~log_local_val, title = ~ paste0("Local Export Value"))
  })
  
  output$num_matching <-renderText({format(nrow(filteredData()), big.mark = ",")})
  
  
}



# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

shinyApp(ui, server)



