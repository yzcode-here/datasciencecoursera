rankhospital <- function(state, outcome, num = "best") { 
  
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
  
  num_v <- num
  ## Return hospital name in that state with given rank 30-day death rate
  if (out == "heart attack" & num_v == 'best') {
    value <- data %>%
      select(2, 7, 11) %>%
      filter(.[2] == stat, !is.na(.[3])) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      slice(1) %>%
      .[[1]]
    
    return(value)
  } 
  
  if (out == "heart attack" & num_v == 'worst') {
    value <- data %>%
      select(2, 7, 11) %>%
      filter(.[2] == stat, !is.na(.[3])) %>%
      arrange(.[1]) %>%
      arrange(desc(.[3])) %>%
      slice(1) %>%
      .[[1]]
    
    return(value)
  } 
  
  if (out == "heart attack" & is.numeric(num_v) & num_v <= length(data[2][!is.na(data[11]) & data$State == stat])) {
    value <- data %>%
      select(2, 7, 11) %>%
      filter(.[2] == stat, !is.na(.[3])) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      slice(num_v) %>%
      .[[1]]
    
    return(value)
  } else if (out == "heart attack" & is.numeric(num_v) & num_v > length(data[2][!is.na(data[11]) & data$State == stat])) {
    return(NA)
  }
  
  if (out == "heart failure" & num_v == 'best') {
    value <- data %>%
      select(2, 7, 17) %>%
      filter(.[2] == stat, !is.na(.[3])) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      slice(1) %>%
      .[[1]]
    
    return(value)
  } 
  
  if (out == "heart failure" & num_v == 'worst') {
    value <- data %>%
      select(2, 7, 17) %>%
      filter(.[2] == stat, !is.na(.[3])) %>%
      arrange(.[1]) %>%
      arrange(desc(.[3])) %>%
      slice(1) %>%
      .[[1]]
    
    return(value)
  } 
  
  if (out == "heart failure" & is.numeric(num_v) & num_v <= length(data[2][!is.na(data[17]) & data$State == stat])) {
    value <- data %>%
      select(2, 7, 17) %>%
      filter(.[2] == stat, !is.na(.[3])) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      slice(num_v) %>%
      .[[1]]
    
    return(value)
  } else if (out == "heart failure" & is.numeric(num_v) & num_v > length(data[2][!is.na(data[17]) & data$State == stat])) {
    return(NA)
  }
  
  if (out == "pneumonia" & num_v == 'best') {
    value <- data %>%
      select(2, 7, 23) %>%
      filter(.[2] == stat, !is.na(.[3])) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      slice(1) %>%
      .[[1]]
    
    return(value)
  } 
  
  if (out == "pneumonia" & num_v == 'worst') {
    value <- data %>%
      select(2, 7, 23) %>%
      filter(.[2] == stat, !is.na(.[3])) %>%
      arrange(.[1]) %>%
      arrange(desc(.[3])) %>%
      slice(1) %>%
      .[[1]]
    
    return(value)
  } 
  
  if (out == "pneumonia" & is.numeric(num_v) & num_v <= length(data[2][!is.na(data[23]) & data$State == stat])) {
    value <- data %>%
      select(2, 7, 23) %>%
      filter(.[2] == stat, !is.na(.[3])) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      slice(num_v) %>%
      .[[1]]
    
    return(value)
  } else if (out == "pneumonia" & is.numeric(num_v) & num_v > length(data[2][!is.na(data[23]) & data$State == stat])) {
    return(NA)
  }

}


