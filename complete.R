complete <- function(directory, id = 1:332){
  vec <- numeric()
  for(i in seq_along(id)){
#     ch <- as.character(id[i])
#     n <- nchar(ch)
#     if(n == 1){
#       ch <- paste("00", ch, sep="")
#     }
#     else if(n == 2){
#       ch <- paste("0", ch, sep="")
#     }
#    fname <- paste(ch,"csv",sep=".")
    fname <- sprintf("%03d.csv",id[i])
    loc <- paste(directory,fname,sep="/")
    csv <- read.csv(loc)
    r <- complete.cases(csv)
    vec[i] <- sum(r)
  }
  df <- data.frame(id = id, nobs = vec)
  df
}