rankall <- function(outcome, num = "best"){
  #Read outcome data
  data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv");
  
  #Check the state and outcome are valid
  out <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% out)){
    stop("invalid outcome")
  }
  
  if(outcome == "heart attack") n <- 11
  else if(outcome == "heart failure") n <- 17
  else if(outcome == "pneumonia") n <- 23
  
  #For each state, find the hospital of the given rank
  dt <- subset( data, data[ , n] != "Not Available"  )
  dt[ , n] <- as.numeric(levels(dt[ , n]))[dt[ , n]]
  state <- as.character(unique(dt[,7]))
  hospital <- NA
  df <- data.frame(hospital, state)
  temp <- num
  for(i in 1:nrow(df)){
    st_filter <- subset( dt, dt$State == df[i,2] )
    cnt <- nrow(st_filter)
    if(num == "best") temp <- "1"
    else if(num == "worst") temp <- as.character(cnt)
    temp <- as.numeric(temp)
    if(temp > cnt) next
    st_sort <- st_filter[order(st_filter[ , 2]), ]
    st_sort <- st_sort[order(st_sort[ , n]), ]
    df[i,1] <- as.character(st_sort[temp, 2])
  }
  #Return a data frame with the hospital names and the (abbreviated) state name
  df <- df[order(df[,2]), ]
  df
}