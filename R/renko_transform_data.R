renko_transform_data <- function(data, size){

  require(data.table)

  setDT(data)

  # add corridor and rleid
  data <- renko_add_corridor(data, size)

  # add directions and brick base
  data <- renko_add_direction(data, size)

  # remove noise
  data <- renko_remove_noise(data)

  # fill gaps
  data <- renko_fill_gaps(data, size)

  # add bricks
  data <- renko_add_bricks(data)

  # return result
  return(data)
}
