library(tidyverse) # piping and read_csv

complete <- function(directory, id = 1:332) {
  ids <- sprintf("%03d", id) # adding leading zeros to ids to match with file names
  files <- list.files(path = file.path(getwd(), directory), pattern = paste0(ids, collapse = "|"), full.names = TRUE) # paths to files
  
  compl <- as.data.frame(files %>%
                           map_df(~read_csv(.)) %>% # reading in files
                           filter(complete.cases(.)) %>% # filtering complete cases
                           group_by(id = ID, .add = TRUE) %>% # grouping by ID and replacing "ID" with "id"
                           summarise(nobs = n()) %>% # calculating number of observation per group
                           ungroup()
  )
  
  return(compl) # printing out the value 

}


