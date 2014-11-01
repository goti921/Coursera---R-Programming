pollutantmean <- function(directory, pollutant, id = 1:332){
  filenames <- sprintf("%03d.csv",id)
  allfiles <- paste(directory, filenames, sep="/")
  rd <- lapply(allfiles, read.csv)
  #mat <- as.matrix(rd)
  vec <- numeric()
  for(i in 1:length(rd)){
    vec <- c(vec, rd[[i]][[pollutant]])
  }
  avg <- mean(vec, na.rm = TRUE)
  avg
}