renko <- function(data, size = 10, style = "modern", points = FALSE){

  require(data.table)
  require(ggplot2)

  # transform to data.table
  data <- copy(as.data.table(data))

  # do the data stuff
  data <- renko_transform_data(data, size)

  # plot the plot
  g <- ggplot(data) +
    # some bugs because of how ggplot handles the order of step / rleid should be fixed
    geom_col(aes(interaction(paste(format(rleid, digits = nchar(max(rleid))), step)),
                 base,
                 fill = paste(direction, base != size),
                 color = paste(direction, base != size))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none") +
    scale_x_discrete(labels = c(data$date))


  if(style == "modern"){
    g <- g + scale_fill_manual(values = c("#F8766D", "transparent", "#00BFC4", "transparent")) +
      scale_color_manual(values = c("#F8766D", "transparent", "#00BFC4", "transparent"))
  } else if(style == "classic"){
    g <- g + scale_fill_manual(values = c("#000000", "transparent", "#FFFFFF", "transparent")) +
      scale_color_manual(values = c("#000000", "transparent", "#000000", "transparent"))
  } else {
    stop("Unrecognized style. Maybe try style = \"modern\" or \"classic\"?")
  }


    geom_point(aes(x = interaction(paste(format(rleid, digits = nchar(max(rleid))), step)),
                   y = close))

  return(g)

}

