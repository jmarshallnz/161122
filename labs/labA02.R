# Our second R script

# Load up the libraries we need and data
library(tidyverse)
data(quakes)

# Let's plot where the earthquakes are again
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat))

# Other stuff goes here

