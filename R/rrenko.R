require(data.table)

set.seed(1702)
df <- data.frame(date = seq.Date(as.Date("2014-05-02"), as.Date("2018-05-04"), by = "week"),
                 close = abs(100 + cumsum(sample(seq(-4.9, 4.9, 0.1), 210, replace = TRUE))))

size <- 5



renko_check <- function(data, size){

  j <- 1

  for( i in 1:nrow(data) ) {
    # debug
#    print(paste("F initialized with i =", i, "and j =", j))

    # Check if it is row 1
    if ( i == 1 ) {
      # debug
      print(paste("0- i =", i, "was incremented to", i + 1, "| j was set to", i))

      # write base value
      data[i, base := corridor_bottom - size]
      data[i, direction := "up"]

    # Last brick was upwards - Check if next brick is upwards
    } else if ( data[j, direction == "up"] &
                data[i, corridor_bottom] >=  data[j, corridor_top]) {

      # debug
      print(paste("1- i =", i, "was incremented to", i + 1, "| j was set to", i))

      # write base value
      # wrong
      # data[i, base := tail(na.locf(data[j:i, base, ]), 1) + size]
      data[i, base := corridor_bottom - size]

      # write direction
      data[i, direction := "up"]

      # increment match counter
      j <- i

    # Last brick was upwards - Check if next brick is downwards
    } else if ( data[j, direction == "up"] &
                data[i, corridor_top] <=  data[j, corridor_bottom] - 2 * size) {

      # debug
      print(paste("2- i =", i, "was incremented to", i + 1, "| j was set to", i))

      # write base value
      # wrong
      # data[i, base := tail(na.locf(data[j:i, base, ]), 1) - 2 * size]
      data[i, base := corridor_top]

      # write direction
      data[i, direction := "down"]

      # increment match counter
      j <- i

    # Last brick was downwards - Check if next brick is upwards
    } else if ( data[j, direction == "down"] &
                data[i, corridor_bottom] > data[j, corridor_top] + size ) {

      # debug
      print(paste("3- i =", i, "was incremented to", i + 1, "| j was set to", i))

      # write base value
      # wrong
      # data[i, base := tail(na.locf(data[j:i, base, ]), 1) + size]
      data[i, base := corridor_bottom - size]

      # write direction
      data[i, direction := "up"]

      # increment match counter
      j <- i

    # Last brick was downwards - Check if next brick is downwards
    } else if ( data[j, direction == "down"] &
                data[i, corridor_top] <= data[j, corridor_bottom]) {

      # debug
      print(paste("4- i =", i, "was incremented to", i + 1, "| j was set to", i))

      # write base value
      # wrong
      # data[i, base := tail(na.locf(data[j:i, base, ]), 1) - 2 * size]
      data[i, base := corridor_top]


      # write direction
      data[i, direction := "down"]

      # increment match counter
      j <- i

    } else {

      # debug
      print(paste("5- Row", i, "without match with j =", j))
    }
  }
  return(data)
}





renko_transform <- function(data, y, size){

  # add corridor
  setDT(data)[, corridor_bottom := size * floor(y / size)]
  data[, corridor_top := corridor_bottom + size]

  # add sequence group by corridor
  data <- data[, head(.SD, 1), by=.(corridor_bottom, rleid(corridor_bottom))]

return(data)

}



remove_noise <- function(data){

  # remove noise
  data <- data[!is.na(direction)]

}


renko_fill <- function(data, x, y, size){

  # set key to prevent duplicated cols in filling
  setkey(data, rleid, base)

  # add base lag
  # dont break this up in lines
  ### still needs a catch for first row
  data[, base_lag := data[!.BY, on=.(rleid)][.(.SD$x - 1), on=.(x), roll=TRUE, x.base], by=rleid]

  # add direction lag
  # dont break this up in lines
  ### still needs a catch for first row
  data[, direction_lag := data[!.BY, on=.(rleid)][.(.SD$x - 1), on=.(x), roll=TRUE, x.direction], by=rleid]

  # start with first row
  result <- data[rleid == 1]

  # fill gap up->up
  if(nrow(data[direction == "up" & direction_lag == "up" & base >= base_lag + size]) > 0){
    data_up_up <- data[direction == "up" & direction_lag == "up" & base >= base_lag + size,
                      .(base = seq(base_lag + size, base, by = size)),
                      by = list(rleid, x, y, direction, base_lag, direction_lag, corridor_bottom, corridor_top)]
    setkey(data_up_up, NULL)
    result <- rbindlist(list(result, data_up_up), use.names = TRUE)
  }

  # fill gap up->down
  if(nrow(data[direction == "down" & direction_lag == "up" & base <= base_lag - size]) > 0){
    data_up_down <- data[direction == "down" & direction_lag == "up" & base <= base_lag - size,
                    .(base = seq(base_lag - size, base, by = -size)),
                    by = list(rleid, x, y, direction, base_lag, direction_lag, corridor_bottom, corridor_top)]
    setkey(data_up_down, NULL)
    result <- rbindlist(list(result, data_up_down), use.names = TRUE)
  }

  # fill gap down->up
  if(nrow(data[direction == "up" & direction_lag == "down" & base >= base_lag + size]) > 0){
    data_down_up <- data[direction == "up" & direction_lag == "down" & base >= base_lag + size,
                       .(base = seq(base_lag + size, base, by = size)),
                       by = list(rleid, x, y, direction, base_lag, direction_lag, corridor_bottom, corridor_top)]
    setkey(data_down_up, NULL)
    result <- rbindlist(list(result, data_down_up), use.names = TRUE)
  }

  # fill gap down->down
  if(nrow(data[direction == "down" & direction_lag == "down" & base <= base_lag - size]) > 0) {
      data_down_down <- data[direction == "down" & direction_lag == "down" & base <= base_lag - size,
                         .(base = seq(base_lag - size, base, by = -size)),
                         by = list(rleid, x, y, direction, base_lag, direction_lag, corridor_bottom, corridor_top)]
      setkey(data_down_down, NULL)
      result <- rbindlist(list(result, data_down_down), use.names = TRUE)
  }

  # reset key in original table
  #setkey(data, NULL)

  return(result)
}



renko_add_bricks <- function(data){

  data[, step := 1:.N]

  data2 <- copy(data)
  data2[, base := size]
  data <- rbindlist(list(data, data2))

  return(data)
}



renko_data <- function(data, x, y, size){

  # transform data
  data <- renko_transform(data, y, size)

  # start sequence
  data <- renko_check(data, size)

  # remove noise
  data <- remove_noise(data)

  # fill gaps
  data <- renko_fill(data)

  # add bricks
  data <- renko_add_bricks(data)

  # return result
  return(data)
}

renko <- function(data, x, y, size = 10, style = "modern", points = FALSE){

  # do the data stuff
  data <- renko_data(data, x, y, size)


  require(ggplot2)

  # plot the plot
  g <- ggplot(data) +
    # some bugs because of how ggplot handles the order of step / rleid should be fixed
    geom_col(aes(interaction(paste(format(rleid, digits = nchar(max(rleid))), step)),
                 base,
                 fill = paste(direction, base != size),
                 color = paste(direction, base != size)))

  if(style == "modern"){
    g <- g + scale_fill_manual(values = c("#F8766D", "transparent", "#00BFC4", "transparent")) +
      scale_color_manual(values = c("#F8766D", "transparent", "#00BFC4", "transparent"))
  } else if(style == "classic"){
    g <- g + scale_fill_manual(values = c("#000000", "transparent", "#FFFFFF", "transparent")) +
      scale_color_manual(values = c("#000000", "transparent", "#000000", "transparent"))
  } else {
    stop("Unrecognized style.")
  }

    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none") +
    scale_x_discrete(labels = c(data$x))

    geom_point(aes(x = interaction(paste(format(rleid, digits = nchar(max(rleid))), step)),
                   y = close))

  return(g)

}

