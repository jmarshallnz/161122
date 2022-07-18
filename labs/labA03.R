# Load up the libraries we need and data
library(tidyverse)
data(quakes)

# A histogram of magnitudes
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag))

# Other stuff goes here

