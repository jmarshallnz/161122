# Our first R script

# Lines starting with a # are comments. You can write anything you like there and
# it will be ignored when 'run' in R/RStudio. These are useful for adding notes
# to yourself for later.

# This first command loads up the `tidyverse` library/package which includes
# `ggplot2` for creating pretty charts.
library(tidyverse)

# Now we'll load some data on earthquakes from around Fiji
data(quakes)

# Let's plot where the earthquakes are
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat))

# You can add more code or comments below here.
# The ggplot() function creates a new plot and associates the data with it (quakes)
ggplot(data=quakes)

# The aes() function creates an 'aesthetic mapping' - it defines how `x` and `y` on the plot
# should be derived from our quakes data. In this case we're saying that `x` should be taken
# from the `long` column, and `y` taken from the `lat` column.
aes(x=long, y=lat)

# The geom_point() bit then adds a geometry layer to our plot.
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat))

# We can change other aspects of our plot:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), col='pink', size=2)

# NOTE: When setting aesthetics, make sure you put them outside the mapping=aes() bit, but
# inside the geom_*() bit. e.g. this is wrong:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat, col='pink'), size=2)

# We can also change the shape:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), col='maroon', shape = 'square', size=2)

# We can also change the transparency with alpha. 1 is completely opaque, and 0 is completely
# transparent. 0.5 is 50% transparent.
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), col='maroon', shape = 'square', size=2, alpha=0.5)

# Finding help:
?geom_point
vignette("ggplot2-specs")

# Colours:
colours()

# using a hex colour (google a colour picker, pick a colour, copy the hex value)
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), col='#5a6e47', shape="diamond open")

# Shapes:
shape_names <- c(
  "circle", paste("circle", c("open", "filled", "cross", "plus", "small")), "bullet",
  "square", paste("square", c("open", "filled", "cross", "plus", "triangle")),
  "diamond", paste("diamond", c("open", "filled", "plus")),
  "triangle", paste("triangle", c("open", "filled", "square")),
  paste("triangle down", c("open", "filled")),
  "plus", "cross", "asterisk"
)

shapes <- data.frame(
  shape_names = shape_names,
  x = c(1:7, 1:6, 1:3, 5, 1:3, 6, 2:3, 1:3),
  y = -rep(1:6, c(7, 6, 4, 4, 2, 3))
)
shapes

ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), col='black', shape="diamond filled",
             fill='yellow', stroke=0.1)

# Mapping the colour to another column in our data.
ggplot(data=quakes) +
  geom_point(aes(x=long, y=lat, col=mag))

# Colouring by station
ggplot(data=quakes) +
  geom_point(aes(x=long, y=lat, col=stations))

# Changing colours! Viridis is quite a nice palette to use:
ggplot(data=quakes) +
  geom_point(aes(x=long, y=lat, col=stations)) +
  scale_colour_viridis_c()
