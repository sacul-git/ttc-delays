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
Data <- inc_year_route <- d %>%
    mutate(year = format(.$Report_Date, "%Y")) %>%
    group_by(Route, year) %>%
    summarise(n_incidents = n()) %>%
    arrange(desc(n_incidents)) %>%
    inner_join(x = m, y = ., by=c("route" = "Route"))

ui <- shiny::fluidPage(
  shiny::titlePanel("TTC Delays"),
  shiny::mainPanel(
    leaflet::leafletOutput("map", width = "100%", height = "800")),
    shiny::sidebarPanel(
      shinyWidgets::pickerInput(inputId = "route", label = "Routes",
                                choices = sort(unique(Data$route)),
                                selected = c(35, 36, 52),
                                multiple = TRUE,
                                options = list(
                                  `actions-box` = TRUE,
                                  `select-all-text` = "Select All Routes (slow render)",
                                  `deselect-all-text` = "Deselect All Routes"
                                              )
    ),
      shiny::selectInput(inputId = "year", label = "Year",
                  choices = c(2014:2018)),
      shiny::plotOutput(outputId = "plot")
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
      leaflet::addLegend(position = "topright", opacity = 1, 
                         pal = colorpal_legend(),
                         values = Data$n_incidents,
                         title = "Reported Delays")
  })
  # Load selected Bus Routes:
  observe({
    pal <- colorpal_legend()
    leaflet::leafletProxy("map", data = filtered()) %>%
      leaflet::clearShapes() %>%
      leaflet::addPolylines(layerId = filtered()$route, label = ~paste("Route ",
                                           as.factor(route)),
                            labelOptions = labelOptions(textsize = "25px"),
                            highlightOptions = highlightOptions(bringToFront = TRUE,
                                                                weight = 8),
                            color = ~pal(filtered()$n_incidents), opacity = 1,
                            popup = str_glue("<h3>Route {filtered()$route}:</h3>",
                                             "<br><h4>{filtered()$n_incidents} ",
                                             "Delays Reported in {filtered()$year} </h4>")
                            )
  })
  # Bar graph of the delays on selected route per year
  observeEvent(input$map_shape_click, {
  r <- input$map_shape_click$id
  d  <- Data %>% filter(route == r) %>% distinct(year, .keep_all=TRUE)
  output$plot <- renderPlot({
    ggplot(data = d, aes(x = year, y = n_incidents)) +
        geom_bar(stat = "identity", aes(fill = n_incidents)) +
        scale_fill_viridis(limits = c(min(Data$n_incidents), max(Data$n_incidents))) +
        labs(title = str_glue("Delays per Year on Route {r}"),
             x = "Year", y = "Number of Reported Delays") +
        theme(legend.position = "none", 
              plot.title = element_text(hjust = 0.5, size = 25),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 15))
    })
  })
}

shiny::shinyApp(ui, server)
