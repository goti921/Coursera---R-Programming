best <- function( state, outcome ) {
      #Read outcome data
      data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
      
      #Check the state and outcome are valid
      out <- c("heart attack", "heart failure", "pneumonia")
      st  <- data$State
      if(!(state %in% st)){
        stop("invalid state")
      }
      if(!(outcome %in% out)){
        stop("invalid outcome")
      }
      
      if(outcome == "heart attack") n <- 11
      else if(outcome == "heart failure") n <- 17
      else if(outcome == "pneumonia") n <- 23
      
      #Return Hospital name in that state with lowest 30-day death rate
      st_filter <- subset( data, data$State == state )
      st_filter <- subset( st_filter, st_filter[ , n] != "Not Available"  )
      st_filter[ , n] <- as.numeric(levels(st_filter[ , n]))[st_filter[ , n]]
      mini <- min(st_filter[ , n])
      min_death <- subset(st_filter, st_filter[ , n] == mini )
      hosp <- sort(min_death[ , 2])
      as.character(hosp)
}