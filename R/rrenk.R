library(tidyverse)
library(data.table)

set.seed(1702)
df <- data.table(date = seq.Date(as.Date("2014-05-02"), as.Date("2018-05-04"), by = "week"),
                 close = abs(100 + cumsum(sample(seq(-4.9, 4.9, 0.1), 210, replace = TRUE))))

size <- 5


df2 <- df %>%
  mutate(corridor_bottom = size * floor(close / size),
         corridor_top = size * ceiling(close / size)) %>%
  group_by(group = {group = rle(corridor_bottom); rep(seq_along(group$lengths), group$lengths)}) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(base = case_when(row_number() == 1 ~ corridor_bottom - size,
                          TRUE ~ NA_real_),
         direction = NA_character_) 


rrenko <- function(data){
  j <- 1
  x <- NULL
  
  for(i in 1:nrow(data))(
    if ( data[i, corridor_bottom] > tail(na.locf(data[j:i, base, ]), 1) + size) {
      data[i, base := tail(na.locf(data[j:i, base, ]), 1) + size]
      data[i, direction := "up"]
      j <- i
      x <- rbindlist(list(x, data[i, ]))
    } else if ( data[i, corridor_top] < tail(na.locf(data[j:i, base, ]), 1) - size) {
      data[i, base := tail(na.locf(data[j:i, base, ]), 1) - 2 * size]
      data[i, direction := "down"]
      j <- i
      x <- rbindlist(list(x, data[i, ]))
    }
    
  )
  return(data)
}










library(tidyverse)
library(data.table)
library(zoo)

set.seed(1702)
df <- data.frame(date = seq.Date(as.Date("2014-05-02"), as.Date("2018-05-04"), by = "week"),
                 close = abs(100 + cumsum(sample(seq(-4.9, 4.9, 0.1), 210, replace = TRUE))))

size <- 5


df2 <- df %>%
  mutate(corridor_bottom = size * floor(close / size),
         corridor_top = size * ceiling(close / size)) %>%
  group_by(group = {group = rle(corridor_bottom); rep(seq_along(group$lengths), group$lengths)}) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(base = case_when(row_number() == 1 ~ corridor_bottom - size,
                          TRUE ~ NA_real_),
         direction = NA_character_) %>%
  as.data.table()


library(tidyverse)
library(data.table)

set.seed(1702)
df <- data.frame(date = seq.Date(as.Date("2014-05-02"), as.Date("2018-05-04"), by = "week"),
                 close = abs(100 + cumsum(sample(seq(-4.9, 4.9, 0.1), 210, replace = TRUE))))

size <- 5


df2 <- df %>%
  mutate(corridor_bottom = size * floor(close / size),
         corridor_top = size * ceiling(close / size)) %>%
  group_by(group = {group = rle(corridor_bottom); rep(seq_along(group$lengths), group$lengths)}) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(base = case_when(row_number() == 1 ~ corridor_bottom - size,
                          TRUE ~ NA_real_),
         direction = NA_character_) %>%
  as.data.table()






check_upwards <- function(data, i, j){
  
  # debug
  print(paste("check_upwards() initialized with i =", i, "and j =", j))
  
  while(i <= nrow(data)) {
    if ( data[i, corridor_bottom] > tail(na.locf(data[j:i, base, ]), 1) + size) {
      
      # debug
      print(paste("11- i =", i, "was incremented to", i + 1, "| j was set to", i))
      
      # write base value
      data[i, base := tail(na.locf(data[j:i, base, ]), 1) + size]
      
      # write direction
      data[i, direction := "up"]
      
      # increment match counter
      j <- i
      
      # increment row counter
      i <- i + 1
      
      # continue upwards
      # ifelse(i <= nrow(data), check_upwards(data), return(data))
      
      
    } else if ( data[i, corridor_top] < tail(na.locf(data[j:i, base, ]), 1) - size) {
      
      # debug
      print(paste("12- i =", i, "was incremented to", i + 1, "| j was set to", i))
      
      # write base value
      data[i, base := tail(na.locf(data[j:i, base, ]), 1) - 2 * size]
      
      # write direction
      data[i, direction := "down"]
      
      # increment match counter
      j <- i
      
      # increment row counter
      i <- i + 1
      
      # continue downwards
      # ifelse(i <= nrow(data), check_upwards(data), return(data))
      check_downwards(data, i, j)
      
    } else {
      
      # debug
      print(paste("13- Row", i, "without match with j =", j))
      
      # increment row counter
      i <- i + 1
      
      # continue upwards
      # check_upwards(data)
    } 
  }   
  return(data)
}




check_downwards <- function(data, i, j){
  
  # debug
  print(paste("check_downwards() initialized with i =", i, "and j =", j))
  
  while(i <= nrow(data)) {
    if ( data[i, corridor_top] > tail(na.locf(data[j:i, base, ]), 1) + size) {
      
      # debug
      print(paste("21- i =", i, "was incremented to", i + 1, "| j was set to", i))
      
      # write base value
      data[i, base := tail(na.locf(data[j:i, base, ]), 1) + size]
      
      # write direction
      data[i, direction := "up"]
      
      # increment match counter
      j <- i
      
      # increment row counter
      i <- i + 1
      
      # continue upwards
      # ifelse(i <= nrow(data), check_upwards(data), return(data))
      
      
    } else if ( data[i, corridor_bottom] < tail(na.locf(data[j:i, base, ]), 1) - size) {
      
      # debug
      print(paste("22- i =", i, "was incremented to", i + 1, "| j was set to", i))
      
      # write base value
      data[i, base := tail(na.locf(data[j:i, base, ]), 1) - 2 * size]
      
      # write direction
      data[i, direction := "down"]
      
      # increment match counter
      j <- i
      
      # increment row counter
      i <- i + 1
      
      # continue upwards
      # ifelse(i <= nrow(data), check_upwards(data), return(data))
      check_upwards(data, i, j)
      
    } else {
      
      # debug
      print(paste("23- Row", i, "without match with j =", j))
      
      # increment row counter
      i <- i + 1
      
      # continue upwards
      # check_upwards(data)
    } 
  }    
  return(data)
}








rrenko_check <- function(data){
  
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





rrenko_transform <- function(data){
  
  # add corridor 
  data <- setDT(data)[, `:=` (corridor_bottom = size * floor(close / size), 
                          # to weed out bugs when corridor_bottom == corridor_top
                          corridor_top = corridor_bottom + size)]
  
  # add sequence group by corridor
  data <- data[, head(.SD, 1), by=.(corridor_bottom, rleid(corridor_bottom))]
  
return(data)

}



remove_noise <- function(data){
  
  # remove noise
  data <- data[!is.na(direction)]

}
  

renko_fill <- function(data){
  
  # set key to prevent duplicated cols in filling
  setkey(data, rleid, base)
  
  # add base lag
  # dont break this up in lines 
  ### still needs a catch for first row 
  data[, base_lag := data[!.BY, on=.(rleid)][.(.SD$date - 1), on=.(date), roll=TRUE, x.base], by=rleid]
  
  # add direction lag
  # dont break this up in lines 
  ### still needs a catch for first row
  data[, direction_lag := data[!.BY, on=.(rleid)][.(.SD$date - 1), on=.(date), roll=TRUE, x.direction], by=rleid]
  

  # fill gap up->up
  data_up_up <- data[direction == "up" & direction_lag == "up" & base > base_lag + size,
                    .(base = seq(base_lag + size, base - size, by = size)),
                    by = list(rleid, date, close, direction, base_lag, direction_lag)]
  
  setkey(data_up_up, NULL)
#  setorder(data_up_up, rleid, base)
  
  # fill gap up->down
  data_up_down <- data[direction == "down" & direction_lag == "up" & base < base_lag - size,
                  .(base = seq(base_lag - size, base + size, by = -size)),
                  by = list(rleid, date, close, direction, base_lag, direction_lag)]
  
  setkey(data_up_down, NULL)
#  setorder(data_up_down, rleid, -base)  
  
  
  # fill gap down->up
  data_down_up <- data[direction == "up" & direction_lag == "down" & base > base_lag + size,
                     .(base = seq(base_lag + size, base - size, by = size)),
                     by = list(rleid, date, close, direction, base_lag, direction_lag)]
  
  setkey(data_down_up, NULL)
#  setorder(data_down_up, rleid, base)
  
  
  # fill gap down->down
  data_down_down <- data[direction == "down" & direction_lag == "down" & base < base_lag - size,
                       .(base = seq(base_lag - size, base + size, by = -size)),
                       by = list(rleid, date, close, direction, base_lag, direction_lag)]
  
  setkey(data_down_down, NULL)
#  setorder(data_down_down, rleid, -base)
  
  # reset key in original table
  setkey(data, NULL)
  
  # bind tables
  data <- rbindlist(list(data, data_up_up, data_up_down, data_down_up, data_down_down), use.names = TRUE, fill = TRUE)
  
  return(data)
}

  
  
renko_add_bricks <- function(data){
  
  data[, step := 1:.N]
  
  data2 <- copy(data)
  data2[, base := size]
  data <- rbindlist(list(data, data2))
  
  return(data)
}



renko_data <- function(data){
  
  # transform data
  data <- rrenko_transform(data)
  
  # start sequence
  data <- rrenko_check(data)
  
  # fill gaps
  data <- rrenko_fill(data)
  
  # remove noise
  data <- remove_noise(data)
  
  # fill gaps
  data <- renko_fill(data)
  
  # add bricks
  data <- renko_add_bricks(data)
  
  # return result
  return(data)
}

renko <- function(data){
  
  data <- renko_data(data)
  
  require(ggplot2)
  ggplot(data[date < "2014-10-01"]) + 
    # some bugs because of how ggplot handles the order of step / rleid
    geom_col(aes(x = interaction(paste(format(rleid, digits = nchar(max(rleid))), step)), 
                 y = base, 
                 fill = paste(direction, base != size))) +
    scale_fill_manual(values = c("#F8766D", "transparent", "#00BFC4", "transparent")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none")
  
}

# df5 <- df4 %>% filter(!is.na(base)) %>% mutate(base = size) %>% bind_rows(df4) %>% filter(!is.na(base)) %>% arrange(date, base)

# ggplot(df5) + 
#  geom_col(aes(as.factor(date), base, fill = paste(direction, base != size))) +
#  scale_fill_manual(values = c("#F8766D", "transparent", "#00BFC4", "transparent")) +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1),
#        legend.position = "none") +
#  geom_point(aes(as.factor(date), close))