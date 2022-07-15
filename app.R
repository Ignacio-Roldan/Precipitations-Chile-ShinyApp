#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(plotly)
library(shinythemes)

#Load Data
precipitations <- readRDS("precipitations.RDS")

#Settings, using fixed color bins
bin_breaks = c(-6000,-3000,-1500,-750,-200,0,200,750,1500,3000,6000)
pal <- colorBin("RdBu", bins = bin_breaks)

ui <- fillPage(theme = shinytheme("cerulean"),
   
   # Application title
   titlePanel("Precipitations in Chile 1970-2019. Are they decreasing over time?"),
   
   # Sidebar with a slider input for years and output of time series plot
   sidebarLayout(
      sidebarPanel(
         sliderInput("year",
                     "Year",
                     min = 1970,
                     max = 2019,
                     value = 1970,
                     animate = animationOptions(interval = 1000, loop = FALSE)),
         p(),
         p("Click on a marker to show historical data of that station"),
         plotlyOutput("plot")
      
         ),
      
      # Panel to show map and app details
      mainPanel(
         leafletOutput("map"),
         "Adapted from Center of Climate and Resilience Research. (2021). Datos de Precipitación (Cr2 PrDaily 2019 Ghcn Zip) [Data set].",
         tags$a(href="http://www.cr2.cl/datos-de-precipitacion/", 
                "http://www.cr2.cl/datos-de-precipitacion/"),
         p(),
         p("With this application you can explore how yearly precipitations have changed in Chile over time. Take a look to what happened in 2019."),
         p(),
         p("The application shows the accumulated rainfall per year since 1970 to 2019 measured by a weather station. The accumulated precipitations are represented by the markers size, and color shows the variation from previous year."),
         p(),
         p("To see the historical data of a station, click on the marker. Note that not all stations exist since 1970, Only data from the first non-zero precipitations will be shown in the graphs"),
         p()
      )
   )
)

# Server uses leaflet to display map view and plotly to display time series data
server <- function(input, output) {
   
   v <- reactiveValues(plot = NULL)
   
   observeEvent(input$map_marker_click, {
     sel_loc <- input$map_marker_click
     plot_data <- precipitations %>% filter(  lat == sel_loc$lat , lon == sel_loc$lng, year < 2020)
     start_date = (1970 + which(plot_data$total!=0)[1L] - 1)
     plot_data <- plot_data %>% filter(year >= start_date)
     v$plot <- ggplotly(plot_data %>%
                          ggplot( aes(x = year, y = total) ) +
                          geom_line( color = "blue", alpha = 0.6) +
                          geom_point(color = "blue") +
                          geom_smooth(method='lm') +
                          labs( title= plot_data$name[1],
                                y = "Precipitations [mm]", 
                                x = "Year" ) +
                          scale_x_continuous(breaks=seq(start_date,2019,5)) +
                          theme_classic())
   })
  
   output$map <- renderLeaflet({
      
     leaflet(data = precipitations) %>% addTiles() %>%
       setView(lat = -38.18, lng = -72.33, zoom = 3) %>%
       addLegend("bottomleft", pal = pal, values = ~variation,
                 title = "Variation [mm]",
                 opacity = 1)
     

   })
   
   observe({
     leafletProxy("map", data = precipitations ) %>%
       clearMarkers() %>%
       addCircleMarkers(data = precipitations %>% filter(year == input$year),
                        lat = ~lat,
                        lng = ~lon,
                        radius = ~(total/200),
                        stroke = FALSE, fillOpacity = 0.8,
                        label = ~name,
                        color = ~pal(variation)
       )

   })
   
   output$plot <- renderPlotly({
     if (is.null(v$plot)) return()
     v$plot
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

