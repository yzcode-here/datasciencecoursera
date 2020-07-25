rankall <- function(outcome, num = "best") { 
  
  library(tidyverse)
  
  ## Read outcome data
  data <- read_csv("outcome-of-care-measures.csv", na = c("Not Available"))
  
  ## Check that state and outcome are valid
  `%notin%` <- Negate(`%in%`)
  
  if (outcome %notin% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  } else {
    out <- outcome
  }
  
  num_v <- num
  
  ## Return hospital name in that state with given rank 30-day death rate
  
  df_out <- data.frame()
  
  if (out == "heart attack" & num_v == 'best') {
    value <- data %>%
      select(2, 7, 11) %>%
      filter(!is.na(.[3])) %>%
      group_by(.[2]) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      select(hospital = 1, state = 2) %>%
      slice(1)
      
      return(value)
    }

  if (out == "heart attack" & num_v == 'worst') {
    value <- data %>%
      select(2, 7, 11) %>%
      filter(!is.na(.[3])) %>%
      group_by(.[2]) %>%
      arrange(.[1]) %>%
      arrange(desc(.[3])) %>%
      select(hospital = 1, state = 2) %>%
      slice(1)
    
    return(value)
  } 
  
  if (out == "heart attack" & is.numeric(num_v)) {
    value <- data %>%
      select(2, 7, 11) %>%
      filter(!is.na(.[3])) %>%
      group_by(.[2]) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      select(hospital = 1, state = 2) %>%
      slice(num_v)
    
    return(value)
  } 
  
  if (out == "heart failure" & num_v == 'best') {
    value <- data %>%
      select(2, 7, 17) %>%
      filter(!is.na(.[3])) %>%
      group_by(.[2]) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      select(hospital = 1, state = 2) %>%
      slice(1)
    
    return(value)
  } 
  
  if (out == "heart failure" & num_v == 'worst') {
    value <- data %>%
      select(2, 7, 17) %>%
      filter(!is.na(.[3])) %>%
      group_by(.[2]) %>%
      arrange(.[1]) %>%
      arrange(desc(.[3])) %>%
      select(hospital = 1, state = 2) %>%
      slice(1)
    
    return(value)
  } 
  
  if (out == "heart failure" & is.numeric(num_v)) {
    value <- data %>%
      select(2, 7, 17) %>%
      filter(!is.na(.[3])) %>%
      group_by(.[2]) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      select(hospital = 1, state = 2) %>%
      slice(num_v)
    
    return(value)
  } 
  
  if (out == "pneumonia" & num_v == 'best') {
    value <- data %>%
      select(2, 7, 23) %>%
      filter(!is.na(.[3])) %>%
      group_by(.[2]) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      select(hospital = 1, state = 2) %>%
      slice(1)
    
    return(value)
  } 
  
  if (out == "pneumonia" & num_v == 'worst') {
    value <- data %>%
      select(2, 7, 23) %>%
      filter(!is.na(.[3])) %>%
      group_by(.[2]) %>%
      arrange(.[1]) %>%
      arrange(desc(.[3])) %>%
      select(hospital = 1, state = 2) %>%
      slice(1)
    
    return(value)
  } 
  
  if (out == "pneumonia" & is.numeric(num_v)) {
    value <- data %>%
      select(2, 7, 23) %>%
      filter(!is.na(.[3])) %>%
      group_by(.[2]) %>%
      arrange(.[1]) %>%
      arrange(.[3]) %>%
      select(hospital = 1, state = 2) %>%
      slice(num_v)
    
    return(value)
  } 
  
}


