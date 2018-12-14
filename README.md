# rrenko

## About
rrenko is an R package to efficiently build Renko charts with the help of ggplot2 and data.table. Renko charts are used to filter the signal from the noise in volatile stock movements. 

## Installation

    devtools::install_github("RomanAbashin/rrenko")
    library(rrenko)
    library(ggplot2)
    library(data.table)

## Variables

    renko(df, size, style)

* **df** = a data frame or data table with 2 variables: date (x-axis) and close (y-axis)
* **size** = the size of the renko bricks (default = 10)
* **style** = the visual style: modern or classic (default = "modern")

## Examples

### Data

    set.seed(1702)
    df <- data.frame(date = seq.Date(as.Date("2014-05-02"), as.Date("2018-05-04"), by = "week"),
                     close = abs(100 + cumsum(sample(seq(-4.9, 4.9, 0.1), 210, replace = TRUE))))

### Code

    devtools::install_github("RomanAbashin/rrenko")

    renko(df, 5, style = "modern") + 
        labs(title = "Renko chart with R", x = "", y = "")

![rrenko modern](/images/rrenko_modern.png)


    renko(df, 5, style = "classic") + 
        labs(title = "Renko chart with R", x = "", y = "")

![rrenko classic](/images/rrenko_classic.png)


## Changelog
### 0.1.3 - Initial Commit
2018-12-13
* Works

### 0.1.0 - Initial Commit
2018-12-13
* Still unstable
* Lots of bugs
* For experimental use only
