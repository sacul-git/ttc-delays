library(curl)
library(tidyverse)
library(lubridate)
library(readxl)
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


get_data_ttc  <- function(year = 2018){
    # Downloads the TTC bus delay data from toronto open data catalogue
    # This function will hopefully be removed by the upcoming open data API
    #
    # Args:
    #  Year: the year for which data will be downloaded.
    #        Currently, only 2014-2018 data is available
  base_url <- "https://www.toronto.ca/ext/open_data/catalog/data_set_files/Bus"
  url <- paste0(base_url, paste0("_", year), ".xlsx")
  url2 <- URLencode(paste(base_url, paste0(year, ".xlsx")))
  file  <- paste0("data/data_", year, ".xlsx")
  tryCatch({
    curl::curl_download(url, file)
    print(paste("downloading data for year", year))
    },
    error = function(cond){
      tryCatch({
          curl::curl_download(url2, file)
        },
        error = function(cond){
          message("Can\'t Download the Data. Must be betweeen 2014 and 2018")
        },
        finally = {
            return()
        }
        )
    },
    finally = {
        return()
    }
    )
}


parse_frames <- function(filename = "data/delay_data.csv", write_file = TRUE,
                         delete_xlfiles = TRUE){
    # Parses the data obtained from get_data_ttc function
    #
    # Args:
    #   filename: the file name under which the data is to be saved
    #   write_file: if FALSE, the data is saved to memory
    #               if TRUE, the data is written to file (under filename)
    #   delete_xlfiles: if TRUE, the excel files obtained from
    #                   get_data_ttc are deleted
  dfs <- list()
  files <- list.files(path = "./data/", pattern = "^data_\\d{4}.xlsx",
                      full.names = TRUE)
  i <- 1
  for (f in files){
    d <- purrr::map_df(readxl::excel_sheets(f),
                        ~readxl::read_excel(f, sheet = .x))
    dfs[[i]] <- d
    i <- i + 1
  }
  data_out <- bind_rows(dfs) %>%
     select_all(~str_replace(., " ", "_")) %>%
     mutate(Report_Date = as_date(.$Report_Date),
            Time = format(ymd_hms(.$Time), "%H:%M:%S"))
  if (delete_xlfiles){
    file.remove(files)
  }
  if (write_file){
    write.csv(data_out, filename, row.names = FALSE)
  }
  else{
    return(data_out)
  }
}


# Get and preprocess the osm data
get_bus_data()
preprocess_osm()


# Download all available years of the delay data:
for (year in c(2014:2018)){
  get_data_ttc(year)
}

# parse the frames and save result to csv
parse_frames()

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

# merge the multilinestrings together:
multilineMerge <- function(multilinestrings){
    return(st_combine(
             st_cast(
               st_line_merge(
                 st_union(
                   st_cast(
                     multilinestrings, "MULTILINESTRING"))
                 ), "LINESTRING")
             )
    )
}

Data <- Data %>%
    group_by(route) %>%
    mutate(geometry = multilineMerge(geometry))

saveRDS(Data, "data/Data.rds")

