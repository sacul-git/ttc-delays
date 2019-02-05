library(shiny)
library(shinyWidgets)
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
  shiny::sidebarPanel(
    shinyWidgets::pickerInput(inputId = "route", label = "Routes",
                              choices = sort(intersect(m$route, d$Route)),
                              selected = c(52, 82),
                              multiple = TRUE,
                              options = list(
                                `actions-box` = TRUE,
                                `select-all-text` = "Select All Routes (slow render)",
                                 `deselect-all-text` = "Deselect All Routes"
                                            )
    ),
    shiny::selectInput(inputId = "year", label = "Year",
                choices = c(2014:2018))

    #shiny::plotOutput(outputId = "delayplot")

    )
)


server <- function(input, output, session){
  # Filter the data
  filtered <- reactive({
    Data %>%
      filter(route %in% input$route & year == input$year)
  })
  # Define color palette
  colorpal_legend <- shiny::reactive(
    leaflet::colorNumeric(
    palette = "viridis",
    domain = Data$n_incidents))
  # Static map:
  output$map <- leaflet::renderLeaflet({
    leaflet(Data) %>%
      leaflet::setView(lng = -79.45, lat = 43.7, zoom = 12) %>%
      leaflet::addProviderTiles(providers$Stamen.TonerLite) %>%
      leaflet::addLegend(position = "topright", pal = colorpal_legend(),
                         values = Data$n_incidents,
                         title = "Reported Delays")
  })
  # Dynamic map:
  observe({
    pal <- colorpal_legend()
    leaflet::leafletProxy("map", data = filtered()) %>%
      leaflet::clearShapes() %>%
      leaflet::addPolylines(label = ~paste("Route ",
                                           as.factor(route)),
                            labelOptions = labelOptions(textsize = "25px"),
                            highlightOptions = highlightOptions(bringToFront = TRUE,
                                                                weight = 8),
                            color = ~pal(filtered()$n_incidents), opacity = 1,
                            popup = paste("<h3>Route ", filtered()$route,
                            ":</h3><br><h4>", filtered()$n_incidents, 
                            " Delays Reported in ", filtered()$year, "</h4>")
                            )
  })

  #observeEvent(input$map_shape_click, {
  #p <- input$map_shape_click
  #print(p)
  #})

  #output$delayplot <- renderPlot({
      #d <- Data
      #ggplot(data = d, aes(x = year, y = n_incidents)) +
          #geom_bar(stat = "identity")
  #})

}

shiny::shinyApp(ui, server)

