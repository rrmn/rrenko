renko_remove_noise <- function(data){

  # remove noise
  data <- data[!is.na(direction)]

  return(data)
}
