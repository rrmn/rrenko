renko_add_bricks <- function(data, size){

  require(data.table)

  data[, step := 1:.N]

  data2 <- copy(data)
  data2[, base := size]
  data <- rbindlist(list(data, data2))

  return(data)
}
