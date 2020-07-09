library(tidyverse)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ids <- sprintf("%03d", id)
  files <- list.files(path = file.path(getwd(), directory), pattern = paste0(ids, collapse = "|"), full.names = TRUE)

  value <- pull(
    files %>%
      map_df(~read_csv(.)) %>%
      select(pollutant) %>%
      summarise(
        mean = mean(.data[[pollutant]], na.rm = TRUE)
        )
    )

  print(value)

}

