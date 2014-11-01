rankhospital <- function(state, outcome, num = "best") {
  #Read outcome data
  data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv");
  
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
  cnt <- nrow(st_filter)
  if(num == "best") num <- "1"
  else if(num == "worst") num <- as.character(cnt)
  num <- as.numeric(num)
  if(num > cnt) return(NA)
  st_sort <- st_filter[order(st_filter[ , 2]), ]
  st_sort <- st_sort[order(st_sort[ , n]), ]
  #temp <- st_sort[ , n]
  #hosp <- subset(st_sort, st_sort[ , n] == temp[num])
  #hosp <- hosp[order(hosp[ , 2]), ]
  as.character(st_sort[num, 2])
}