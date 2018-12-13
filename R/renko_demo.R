renko_demo <- function(){
  set.seed(1701)
  df <- data.frame(date = seq.Date(as.Date("2014-05-02"), as.Date("2018-05-04"), by = "week"),
                   close = abs(100 + cumsum(sample(seq(-4.9, 4.9, 0.1), 210, replace = TRUE))))

  size <- 10

  renko(df)
}
