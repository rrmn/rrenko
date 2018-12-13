renko_add_direction <- function(data, size = 10){

  require(data.table)

  # initialize counter
  j <- 1

  for(i in 1:nrow(data)) {
    # debug
    #    print(paste("F initialized with i =", i, "and j =", j))

    # Check if it is row 1
    if (i == 1) {
      # debug
      #print(paste("0- i =", i, "was incremented to", i + 1, "| j was set to", i))

      # write base value
      data[i, base := corridor_bottom - size]
      data[i, direction := "up"]

      # Last brick was upwards - Check if next brick is upwards
    } else if (data[j, direction == "up"] &
               data[i, corridor_bottom] >=  data[j, corridor_top]) {

      # debug
      #print(paste("1- i =", i, "was incremented to", i + 1, "| j was set to", i))

      # write base value
      # wrong
      # data[i, base := tail(na.locf(data[j:i, base, ]), 1) + size]
      data[i, base := corridor_bottom - size]

      # write direction
      data[i, direction := "up"]

      # increment match counter
      j <- i

      # Last brick was upwards - Check if next brick is downwards
    } else if (data[j, direction == "up"] &
               data[i, corridor_top] <=  data[j, corridor_bottom] - 2 * size) {

      # debug
      #print(paste("2- i =", i, "was incremented to", i + 1, "| j was set to", i))

      # write base value
      # wrong
      # data[i, base := tail(na.locf(data[j:i, base, ]), 1) - 2 * size]
      data[i, base := corridor_top]

      # write direction
      data[i, direction := "down"]

      # increment match counter
      j <- i

      # Last brick was downwards - Check if next brick is upwards
    } else if (data[j, direction == "down"] &
               data[i, corridor_bottom] > data[j, corridor_top] + size) {

      # debug
      #print(paste("3- i =", i, "was incremented to", i + 1, "| j was set to", i))

      # write base value
      # wrong
      # data[i, base := tail(na.locf(data[j:i, base, ]), 1) + size]
      data[i, base := corridor_bottom - size]

      # write direction
      data[i, direction := "up"]

      # increment match counter
      j <- i

      # Last brick was downwards - Check if next brick is downwards
    } else if (data[j, direction == "down"] &
               data[i, corridor_top] <= data[j, corridor_bottom]) {

      # debug
      #print(paste("4- i =", i, "was incremented to", i + 1, "| j was set to", i))

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
      #print(paste("5- Row", i, "without match with j =", j))
    }
  }
  return(data)
}
