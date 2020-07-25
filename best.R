best <- function(state, outcome) {
  
  library(tidyverse)
  
  ## Read outcome data
  data <- read_csv("outcome-of-care-measures.csv", na = c("Not Available"))
  
  ## Check that state and outcome are valid
  `%notin%` <- Negate(`%in%`)
  
  if (state %notin% data$State) {
    stop("invalid state")
  } else {
    stat <- state
  }

  if (outcome %notin% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  } else {
    out <- outcome
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  if(out == "heart attack") {
    value <- data %>%
      select(2, 7, 11) %>%
      filter(.[2] == stat) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      slice(1) %>%
      .[[1]]
    
    return(value)
  } 
  
  if(out == "heart failure") {
    value <- data %>%
      select(2, 7, 17) %>%
      filter(.[2] == stat) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      slice(1) %>%
      .[[1]]
        
      return(value)
  } 
  
  if(out == "pneumonia") {
    value <- data %>%
      select(2, 7, 23) %>%
      filter(.[2] == stat) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      slice(1) %>%
      .[[1]]
        
    return(value)
  }
      
} 

  

