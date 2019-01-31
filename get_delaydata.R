library(curl)
library(dplyr)
library(readxl)

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
  if (delete_xlfiles){
    file.remove(files)
  }
  if (write_file){
    write.csv(bind_rows(dfs), filename, row.names = FALSE)
  }
  else{
    return(bind_rows(dfs))
  }
}


# To download all the available years:
for (year in c(2014:2018)){
  get_data_ttc(year)
}

# parse the frames and save them to csv
parse_frames()
