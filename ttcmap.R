library(shiny)
library(leaflet)
library(tidyverse)
library(viridis)

# load map data
m <- readRDS("data/busroutes.rds")
# load delay data
d <- read_csv("data/delay_data.csv") 

# get incidents per year per route
inc_per_route <- d %>%
  group_by(Route) %>%
  summarise(n_incidents = n(),
            perc_incidents = (n() / nrow(d)) * 100
            ) %>%
  arrange(desc(n_incidents))

Data <- inner_join(x = m, y = inc_per_route, by = c("route" = "Route"))

sort(unique(Data$route))

ui <- bootstrapPage(
  leafletOutput("map", width = "80%", height = "800"),
  absolutePanel(top = 10, right = 10,
    selectInput(inputId = "route", label = "Route",
                choices = sort(unique(Data$route))
    ),
    selectInput(inputId = "year", label = "Year",
                choices = c(2014:2018))
    )
)

server <- function(input, output, session){
  filtered <- reactive({
    Data %>% 
        filter(route == input$route)
  })

  colorpal <- reactive(colorNumeric(
    palette = "viridis",
    domain = Data$n_incidents))

  # Static part of map:
  output$map <- renderLeaflet({
    leaflet(Data) %>%
      setView(lng = -79.367494, lat = 43.722780, zoom = 12) %>%
      addProviderTiles(providers$Stamen.Toner) %>% 
      addLegend(position = "topright", pal = colorpal(), values = Data$n_incidents, title = "Number of incidents on each route")
  })
  observe({
    pal <- colorpal()

    leafletProxy("map", data = filtered()) %>% 
        clearShapes() %>% 
        addPolylines(label = ~as.factor(route), color = ~pal(n_incidents))
  })
}

shinyApp(ui, server)
