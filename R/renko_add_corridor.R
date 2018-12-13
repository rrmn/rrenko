renko_add_corridor <- function(data, size){

  require(data.table)

  # add corridor
  setDT(data)[, corridor_bottom := size * floor(close / size)]
  data[, corridor_top := corridor_bottom + size]

  # add sequence group by corridor
  data <- data[, head(.SD, 1), by=.(corridor_bottom, rleid(corridor_bottom))]

  return(data)

}
