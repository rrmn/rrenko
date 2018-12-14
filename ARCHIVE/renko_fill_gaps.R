renko_fill_gaps <- function(data, size){

  require(data.table)

  # set key to prevent duplicated cols in filling
  setkey(data, rleid, base)

  # add base lag
  # dont break this up in lines
  ### still needs a catch for first row
  # funky stuff with on=.(date) / on=.(x) -> needs testing
  data[, base_lag := data[!.BY, on=.(rleid)][.(.SD$date - 1), on=.(date), roll=TRUE, x.base], by=rleid]

  # add direction lag
  # dont break this up in lines
  ### still needs a catch for first row
  # funky stuff with on=.(date) / on=.(x) -> needs testing
  data[, direction_lag := data[!.BY, on=.(rleid)][.(.SD$date - 1), on=.(date), roll=TRUE, x.direction], by=rleid]

  # start with first row
  result <- data[rleid == 1]

  # fill gap up->up
  if(nrow(data[direction == "up" & direction_lag == "up" & base >= base_lag + size]) > 0){
    data_up_up <- data[direction == "up" & direction_lag == "up" & base >= base_lag + size,
                       .(base = seq(base_lag + size, base, by = size)),
                       by = list(rleid, date, close, direction, base_lag, direction_lag, corridor_bottom, corridor_top)]
    setkey(data_up_up, NULL)
    result <- rbindlist(list(result, data_up_up), use.names = TRUE)
  }

  # fill gap up->down
  if(nrow(data[direction == "down" & direction_lag == "up" & base <= base_lag - size]) > 0){
    data_up_down <- data[direction == "down" & direction_lag == "up" & base <= base_lag - size,
                         .(base = seq(base_lag - size, base, by = -size)),
                         by = list(rleid, date, close, direction, base_lag, direction_lag, corridor_bottom, corridor_top)]
    setkey(data_up_down, NULL)
    result <- rbindlist(list(result, data_up_down), use.names = TRUE)
  }

  # fill gap down->up
  if(nrow(data[direction == "up" & direction_lag == "down" & base >= base_lag + size]) > 0){
    data_down_up <- data[direction == "up" & direction_lag == "down" & base >= base_lag + size,
                         .(base = seq(base_lag + size, base, by = size)),
                         by = list(rleid, date, close, direction, base_lag, direction_lag, corridor_bottom, corridor_top)]
    setkey(data_down_up, NULL)
    result <- rbindlist(list(result, data_down_up), use.names = TRUE)
  }

  # fill gap down->down
  if(nrow(data[direction == "down" & direction_lag == "down" & base <= base_lag - size]) > 0) {
    data_down_down <- data[direction == "down" & direction_lag == "down" & base <= base_lag - size,
                           .(base = seq(base_lag - size, base, by = -size)),
                           by = list(rleid, date, close, direction, base_lag, direction_lag, corridor_bottom, corridor_top)]
    setkey(data_down_down, NULL)
    result <- rbindlist(list(result, data_down_down), use.names = TRUE)
  }

  # reset key in original table
  #setkey(data, NULL)

  return(result)
}
