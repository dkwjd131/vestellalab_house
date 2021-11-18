
library(plumber)
r <- plumb("C:/Users/VEStellaLab/Documents/R/Rstudio/LiveTrafficProject/source2/app2.R")

r$run(port=8000)
