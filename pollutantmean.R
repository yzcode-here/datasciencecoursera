library(tidyverse) # needed for pipingm map_df and read_csv

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ids <- sprintf("%03d", id) # adding leading zeros to ids to match with file names
  files <- list.files(path = file.path(getwd(), directory), pattern = paste0(ids, collapse = "|"), full.names = TRUE) # creating a vector of full paths to files with requested ids

  value <- pull( # reading in files with specified ids 
    files %>%
      map_df(~read_csv(.)) %>% # combining files in one tibble
      summarise(
        mean = mean(.data[[pollutant]], na.rm = TRUE) # calculating the mean values of the requested pollutant
        )
    )

  print(value) # printing out the mean value 

}

