corr <- function(directory, threshold = 0){
  df <- complete(directory)
  vid <- numeric()
  for(i in 1:nrow(df)){
    if(df[i,]$nobs > threshold){
      vid <- c(vid,df[i,]$id)
    }
  }
  vcor <- numeric()
  for(i in seq_along(vid)){
    fname <- sprintf("%03d.csv",vid[i])
    loc <- paste(directory,fname,sep="/")
    csv <- read.csv(loc)
    vcor[i] <- cor(csv$sulfate, csv$nitrate, use = "complete.obs")
  }
  
  vcor
}