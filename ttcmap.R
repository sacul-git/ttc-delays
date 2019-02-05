library(shiny)
library(leaflet)
library(tidyverse)
library(viridis)
# sf package *must* be loaded in order to merge 
# route and delay data and still get sf object
library(sf)

# load map data
m <- readRDS("data/busroutes.rds")
# load delay data
d <- read_csv("data/delay_data.csv")

# get incidents per year per route
inc_year_route <- d %>%
    mutate(year = format(.$Report_Date, "%Y")) %>%
    group_by(Route, year) %>%
    summarise(n_incidents = n()) %>%
    arrange(desc(n_incidents))

Data <- inner_join(x = m, y = inc_year_route, by = c("route" = "Route"))

ui <- shiny::fluidPage(
  shiny::titlePanel("TTC Delays"),
  shiny::mainPanel(
    leaflet::leafletOutput("map", width = "100%", height = "800")),
  #shiny::absolutePanel(
   shiny::sidebarPanel(
     shiny::checkboxGroupInput(inputId = "route", label = "Routes",
                               choices = sort(intersect(m$route, d$Route)),
                               selected = c(52, 82),
                               inline = TRUE,
                               width = "100%"
    ),
    shiny::selectInput(inputId = "year", label = "Year",
                choices = c(2014:2018))
    #shiny::dateRangeInput(inputId = "daterange", label = "Dates",
                          #start = min(d$Report_Date),
                          #end = max(d$Report_Date),
                          #min = min(d$Report_Date),
                          #max = max(d$Report_Date))
    )
)

server <- function(input, output, session){
  filtered <- reactive({
    Data %>%
        filter(route %in% input$route & year == input$year)
  })

  colorpal_legend <- shiny::reactive(
    leaflet::colorNumeric(
    palette = "viridis",
    domain = Data$n_incidents))

  # Static part of map:
  output$map <- leaflet::renderLeaflet({
    leaflet(Data) %>%
      leaflet::setView(lng = -79.45, lat = 43.7, zoom = 12) %>%
      leaflet::addProviderTiles(providers$Stamen.TonerLite) %>%
      leaflet::addLegend(position = "topright", pal = colorpal_legend(),
                values = Data$n_incidents,
                title = "Number of incidents on each route")
  })
  observe({
    pal <- colorpal_legend()

    leaflet::leafletProxy("map", data = filtered()) %>%
        leaflet::clearShapes() %>%
        leaflet::addPolylines(label = ~paste(as.factor(n_incidents),
                                             " incidents reported on route ",
                                             as.factor(route),
                                             " in ", as.factor(year)),
                     color = ~pal(filtered()$n_incidents))
  })
}

shiny::shinyApp(ui, server)
