library(tidyverse)
library(osmdata)
library(sf)
library(rmapshaper)

get_bus_data <- function(filename = "data/busroutes.osm"){
  # Gets the bus route OSM data from the toronto area,
  # saves it as a .osm file
  #
  # Args:
  #   filename: The filename under which the resulting osm object will be saved
    opq(c(-79.68, 43.6, -79.2, 43.76)) %>%
        add_osm_feature(key = "route", value = "bus") %>%
        osmdata_xml(filename = filename)
}

preprocess_osm <- function(filename = "data/busroutes.osm",
                           delete_osm = TRUE){
    # Performs basic preprocessing of osm file, saves the output as .rds
    #
    # Args:
    #  filename: the name of the file containing the osm data
    #  delete_osm: if TRUE, the osm file will be deleted after processing
    busroutes <- st_read(filename, layer = "multilinestrings") %>%
    # Filter out those that have TTC in "other_tags" column
      filter(grepl("TTC", other_tags)) %>%
      # Add in the route number
      mutate(route = as.numeric(sub("([0-9]+).*", "\\1", .$name))) %>%
      select(route) %>%
      # Shrink data at least a little bit
      ms_simplify()
    if (delete_osm){
        file.remove(filename)
    }
    saveRDS(busroutes, paste0(gsub(".osm", "", filename), ".rds"))
}

# Get and preprocess the osm data
get_bus_data()
preprocess_osm()
