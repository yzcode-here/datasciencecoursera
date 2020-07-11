library(tidyverse) # piping and read_csv

corr <- function(directory, threshold = 0) {
  files <- list.files(path = file.path(getwd(), directory), pattern = "*.csv", full.names = TRUE) # paths to files
  
  corr_value <- numeric() # empty numeric vector; will return 0 if the threshold condition is not met
  
  for (i in 1:length(files)) {
    
    data <- read_csv(files[i]) # read in files
    
    clean_data <- data %>% # filter complete cases
      filter(complete.cases(.))
    
    n_obs <- nrow(clean_data) # get number of observations (w/o NA)
    
    if (n_obs > threshold) { # check the threshold condition
      corr_value <- c(corr_value, cor(clean_data$nitrate, clean_data$sulfate))
    }

  }
  
  return(corr_value) # printing out the value 
  
}
